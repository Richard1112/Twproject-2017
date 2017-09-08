package org.jblooming.scheduler;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.utilities.JSP;
import org.jblooming.tracer.Tracer;
import org.jblooming.operator.Operator;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.hibernate.HibernateFactory;
import org.jblooming.persistence.hibernate.PersistenceContext;

import java.util.*;
import java.util.concurrent.Future;
import java.io.Serializable;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */

public class Scheduler extends TimerTask {

  public TreeSet<OrderedJob> toBeExecuted = new TreeSet<OrderedJob>();

  public TreeMap<Serializable, FutureIsPink> inExecution = new TreeMap();

  public long tick;

  private static Scheduler scheduler;

  private Timer timer;

  public String instantiator;
  public Date instantiationTime;

  private Set<JobLogData> jobLogsToBeStored = new HashSet();

  public static final long DEFAULT_TICK = 600000;
  public static int deltaForExcludingRelaunch = 30000;


  private Scheduler() {
  }

  public static void instantiate(long tick, Operator operator) {

    instantiate(tick, operator.getDisplayName());

  }

  public static void instantiate(long tick, String operatorName) {

    if (scheduler != null && scheduler.timer != null)
      return;
    //throw new PlatformRuntimeException("Scheduler and timer already instantiated");

    if (scheduler == null) {
      scheduler = new Scheduler();
    }

    scheduler.fillFromPersistence();

    scheduler.instantiator = operatorName;
    scheduler.instantiationTime = new Date();
    scheduler.tick = tick;
    scheduler.timer = new Timer(false);
    scheduler.timer.schedule(scheduler, new Date(), tick);

  }

  public static Scheduler getInstance() {
    return scheduler;
  }

  /**
   * tick: run due and refills toBeExecuted
   */
  public void run() {

    boolean exception = true;
    PersistenceContext pc = null;

    try {

      //must be new, not get default as it is called in same thread by loader support
      pc = new PersistenceContext();

      SortedSet<OrderedJob> toBeExecutedJobs = new TreeSet(toBeExecuted.headSet(new OrderedJob(new Date().getTime() - tick + 1, -1)));

      for (OrderedJob orderedJob : new TreeSet<OrderedJob>(toBeExecutedJobs)) {

        Serializable id = orderedJob.jobId;

        Job job = (Job) PersistenceHome.findByPrimaryKey(Job.class, id, pc);

        if (job != null && job.isEnabled()) {

          //is running ?
          if (inExecution.keySet().contains(job.getId())) {


            Scheduler.FutureIsPink fip = inExecution.get(job.getId());
            Future future = fip.future;
            boolean stillRunning = !future.isDone();

            //check still running in timeout
            if (stillRunning) {
              if (System.currentTimeMillis() > fip.expireTime) {
                future.cancel(true);
                Tracer.jobLogger.error(job.getName()+": went in timeout, cancelled ");
              }
            }

            boolean failedAsJob = false;
            Iterator i = jobLogsToBeStored.iterator();
            while (i.hasNext()) {
              JobLogData jobLogData = (JobLogData) i.next();
              if (jobLogData.id.equals(job.getId()))
                failedAsJob = !jobLogData.successfull;
            }

            boolean failedAsThread = future.isCancelled();

            if (failedAsJob)
              Tracer.jobLogger.error(job.getName()+": job failed as job");
            if (failedAsThread)
              Tracer.jobLogger.error(job.getName()+": job failed as thread");

            //completed successfully as thread and as job
            if (!stillRunning && !failedAsJob) {

              inExecution.remove(job.getId());

              toBeExecuted.remove(orderedJob);

              //eventually put in relaunch queue

              Date dateAfter = job.getSchedule().getNextDateAfter(new Date(System.currentTimeMillis()));
              if (dateAfter != null) {
                toBeExecuted.add(new OrderedJob(dateAfter.getTime(), job.getId()));
              }
            }
            //failed
            if (!stillRunning && (failedAsJob || failedAsThread)) {

              if (!job.isOnErrorSuspendScheduling()) {
                if (!job.isOnErrorRetryNow()) {
                  inExecution.remove(job.getId());
                  rescheduleJob(orderedJob, job);
                } else
                  //retry now case need not be handled, as job is not removed from toBeExecuted
                  Tracer.jobLogger.error(job.getName()+": job retried");

              } else {
                //suspended
                inExecution.remove(job.getId());
                toBeExecuted.remove(orderedJob);
              }

            }

            // otherwise is still running -> do nothing

          } else {
            // this in order to avoid a second node (server) to launch exactly the same job for the same scheduled launch
            long lastExecutionTime = job.getLastExecutionTime();
            long ctm = System.currentTimeMillis();
            long delta = Math.abs(lastExecutionTime - ctm);
            // this in order to avoid a second node (server) to launch exactly the same job for the same scheduled launch
            if (delta > deltaForExcludingRelaunch) {
              job.resetLastExecutionTime();
              job.store(pc);
              pc.checkPoint();
              runAJob(job);
            } else {
              //compute next execution time
              rescheduleJob(orderedJob, job);
              Tracer.platformLogger.debug("Job " + job.getName() + " not launched as found launched by another node");
            }
          }
        }
      }

      //store the jobLogs
      while (true) {
        if (jobLogsToBeStored.size() > 0) {
          HashSet<JobLogData> datas = new HashSet<JobLogData>(jobLogsToBeStored);
          if (datas.size() > 0) {
            JobLogData jl = datas.iterator().next();
            Job job = (Job) PersistenceHome.findByPrimaryKey(Job.class, jl.id, pc);
            long howLongPiuOMenTheTick = System.currentTimeMillis() - (job.getLastExecutionTime() + tick);
            howLongPiuOMenTheTick = howLongPiuOMenTheTick < 0 ? howLongPiuOMenTheTick + tick : howLongPiuOMenTheTick;
            Tracer.jobLogger.info("job: " + job.getName() + " executed at " + jl.date + " successful:" + jl.successfull + " duration:" + howLongPiuOMenTheTick + "ms.  notes: " + jl.notes);
            jobLogsToBeStored.remove(jl);
          }
        } else
          break;
      }

      //check those in timeout
      for (Serializable jid : new HashSet<Serializable>(inExecution.keySet())) {
        FutureIsPink fip = inExecution.get(jid);
        Future future = fip.future;
        boolean stillRunning = !future.isDone();

        //check still running in timeout
        if (stillRunning) {
          if (System.currentTimeMillis() > fip.expireTime) {
            future.cancel(true);
            inExecution.remove(jid);
            Tracer.jobLogger.error("Job id:" + jid + " went in timeout, cancelled ");
          }
        }
      }

      exception = false;

      pc.commitAndClose();
    } catch (Throwable t) {
      if (pc!=null)
        pc.rollbackAndClose();
      //throw new PlatformRuntimeException(e);
      Tracer.desperatelyLog("Scheduler failed a run " + JSP.w(t.getMessage()), false, t);
      Tracer.jobLogger.error(t);
      Tracer.platformLogger.error(t);
    }
  }

  private void rescheduleJob(OrderedJob orderedJob, Job job) {
    toBeExecuted.remove(orderedJob);

    Date dateAfter = job.getSchedule().getNextDateAfter(new Date(System.currentTimeMillis()));
    if (dateAfter != null) {
      toBeExecuted.add(new OrderedJob(dateAfter.getTime(), job.getId()));
      Tracer.jobLogger.error(job.getName()+": job rescheduled");
    }
  }

  public void runAJob(Job job) {
    Future future = PlatformExecutionService.executorService.submit(job);
    FutureIsPink fip = new FutureIsPink();
    fip.future = future;
    fip.startTime = System.currentTimeMillis();
    fip.expireTime = job.getTimeoutTime() > 0 ? System.currentTimeMillis() + job.getTimeoutTime() : Long.MAX_VALUE;
    inExecution.put(job.getId(), fip);
  }

  public void addJob(Job job) {
    fillFromPersistence();
  }

  public void removeJob(Job job) {
    scheduler.toBeExecuted.remove(new OrderedJob(0, job.getId()));
  }

  public synchronized void fillFromPersistence() {

    synchronized (this) {
      PersistenceContext pc = null;
      try {
        pc = new PersistenceContext();
        String hql = "from " + Job.class.getName() + " as job where job.schedule.end >= :now and job.enabled = :istrue";
        OqlQuery q = new OqlQuery(hql, pc);
        q.getQuery().setTimestamp("now", new Date(System.currentTimeMillis()));
        q.getQuery().setBoolean("istrue", true);

        scheduler.toBeExecuted = new TreeSet<OrderedJob>();
        List<Job> persJobs = q.list();
        for (Job job : persJobs) {
          Date dateAfter = job.getSchedule().getNextDateAfter(new Date(System.currentTimeMillis()));
          if (dateAfter != null) {
            Serializable id = job.getId();
            scheduler.toBeExecuted.add(new OrderedJob(dateAfter.getTime(), id));
          }
        }
        pc.commitAndClose();
      } catch (Throwable e) {
        if (pc != null)
          pc.rollbackAndClose();
            throw new PlatformRuntimeException(e);
          }
      }
    }

  public static boolean isRunning() {

    boolean sched = scheduler != null;
    return sched && scheduler.timer != null;
  }

  public void stop() {
    if (scheduler != null) {
      scheduler.cancel();

      for (FutureIsPink fip :inExecution.values())
        fip.future.cancel(true);

      if (scheduler.timer != null)
        scheduler.timer.cancel();
      scheduler.timer = null;
      scheduler = null;
    }
  }

  public synchronized void addJobLogData(JobLogData jobLog) {
    jobLogsToBeStored.add(jobLog);
  }

  public class OrderedJob implements Comparable {

    public long exeTime;
    public Date exeTimeDate;
    public Serializable jobId;

    public OrderedJob(long exeTime, Serializable jobId) {
      this.exeTime = exeTime;
      this.exeTimeDate = new Date(exeTime);
      this.jobId = jobId;
    }

    public int compareTo(Object o) {
      int result = (new Long(exeTime).compareTo(new Long(((OrderedJob) o).exeTime)));
      if (result == 0 && !this.equals(o))
        result = -1;
      return result;
    }

    public boolean equals(Object o) {
      return jobId == ((OrderedJob) o).jobId;
    }

    public int hashCode() {
      return jobId.hashCode();
    }
  }

  public class FutureIsPink {
    public Future future;
    public long expireTime;
    public long startTime;

  }

}
