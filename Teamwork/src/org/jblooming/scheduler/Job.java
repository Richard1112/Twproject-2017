package org.jblooming.scheduler;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.objectEditor.FieldFeature;
import org.jblooming.persistence.objectEditor.ObjectEditor;
import org.jblooming.persistence.objectEditor.businessLogic.ObjectEditorAction;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.ActionUtilities;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.persistence.hibernate.HibernateFactory;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.agenda.ScheduleSupport;
import org.jblooming.waf.view.RestState;

import java.io.Serializable;
import java.util.*;
import java.lang.reflect.Field;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */

public class Job extends IdentifiableSupport implements Runnable {

  protected ScheduleSupport schedule;
  protected String executable;
  protected Map<String, String> parameters = new HashMap();
  protected String name;
  protected String description;
  protected int estimatedDuration;
  protected boolean onErrorRetryNow = true;
  protected boolean onErrorSuspendScheduling = false;
  protected boolean enabled = true;
  protected long timeoutTime = 0;
  private long lastExecutionTime = 0;

//not persistent
  private long startedOn;
  private long secondLastExecutionTime;

  //read only collections
  //private Set<JobLog> jobLogs = new TreeSet();

  public Job() {
  }

  public Job(String executable, ScheduleSupport schedule) {
    try {
      if (! (Class.forName(executable).newInstance() instanceof Executable))
        throw new PlatformRuntimeException("Executable required");
      this.executable = executable;
    } catch (InstantiationException e) {
      throw new PlatformRuntimeException(e);
    } catch (IllegalAccessException e) {
      throw new PlatformRuntimeException(e);
    } catch (ClassNotFoundException e) {
      throw new PlatformRuntimeException(e);
    }
    this.schedule = schedule;
  }


  public Map<String, String> getParameters() {
    return parameters;
  }

  public void setParameters(Map<String, String> parameters) {
    this.parameters = parameters;
  }

  public ScheduleSupport getSchedule() {
    return schedule;
  }

  public void setSchedule(ScheduleSupport schedule) {
    this.schedule = schedule;
  }

  public int getEstimatedDuration() {
    return estimatedDuration;
  }

  public void setEstimatedDuration(int estimatedDuration) {
    this.estimatedDuration = estimatedDuration;
  }

  public String getExecutable() {
    return executable;
  }

  public void setExecutable(String executable) {
    this.executable = executable;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public boolean isEnabled() {
    return enabled;
  }

  public void setEnabled(boolean enabled) {
    this.enabled = enabled;
  }

  public void run() {

   boolean exception = true;
    Exception exc = null;
    this.startedOn = System.currentTimeMillis();

    Executable executable = null;
    try {
      executable = (Executable) Class.forName(this.getExecutable()).newInstance();
    } catch (InstantiationException e) {
      throw new PlatformRuntimeException(e);
    } catch (IllegalAccessException e) {
      throw new PlatformRuntimeException(e);
    } catch (ClassNotFoundException e) {
      throw new PlatformRuntimeException(e);
    }

    //refresh persistent job
    PersistenceContext pc=null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();
      Job persJob = (Job) PersistenceHome.findByPrimaryKey(Job.class, getId());
      Map<String, String> parameters = persJob.getParameters();
      for (String fieldName : parameters.keySet()) {

        String value = parameters.get(fieldName);
        /*
        Field field = ReflectionUtilities.getField(fieldName, executable.getClass());

        field.setAccessible(true);
        try {
          field.set(executable, value);
        } catch (Exception e) {
          exc = e;
        }*/

        try {
          RestState rs = new RestState();
          rs.addClientEntry(fieldName, value);
          FieldFeature fieldFeature = new FieldFeature(fieldName, StringUtilities.deCamel(fieldName));
          ObjectEditorAction.setFieldFeatureValueToObject(executable, fieldFeature, rs);
        } catch (Exception e) {
          exc = e;
        }
      }
      pc.commitAndClose();

    } catch (PersistenceException e) {
      if (pc!=null)
        pc.rollbackAndClose();
      throw new PlatformRuntimeException(e);
    }


    JobLogData jobLogData = null;
    try {
      jobLogData = executable.runAndLog(this);
      if (exc == null)
        exception = false;
    } catch (Exception e) {
      exc = e;
    } finally {
      if (exception) {
        if (jobLogData == null) {
          jobLogData = new JobLogData();
          jobLogData.id = this.id;
          jobLogData.date = new Date();
        }
        if (exc != null)
          jobLogData.notes = jobLogData.notes + exc.getMessage();
        else
          jobLogData.notes = jobLogData.notes + "Run failed with Runtime, so no message available; see the container logs.";
        jobLogData.successfull = false;
      }
    }

    Scheduler.getInstance().addJobLogData(jobLogData);
  }

  public boolean isOnErrorRetryNow() {
    return onErrorRetryNow;
  }

  public void setOnErrorRetryNow(boolean onErrorRetryNow) {
    this.onErrorRetryNow = onErrorRetryNow;
  }

  public boolean isOnErrorSuspendScheduling() {
    return onErrorSuspendScheduling;
  }

  public void setOnErrorSuspendScheduling(boolean onErrorSuspendScheduling) {
    this.onErrorSuspendScheduling = onErrorSuspendScheduling;
  }

  public long getTimeoutTime() {
    return timeoutTime;
  }

  public void setTimeoutTime(long timeoutTime) {
    this.timeoutTime = timeoutTime;
  }


  /*public synchronized void addJobLog(JobLog jobLog) {
    jobLogs.add(jobLog);
  }

  public JobLog getLastLog() {
    TreeSet<JobLog> jl = new TreeSet(jobLogs);
    return jl.last();
  }*/


  public long getStartedOn() {
    return startedOn;
  }

  private void setStartedOn(long startedOn) {
    this.startedOn = startedOn;
  }

 /* private Set<JobLog> getJobLogs() {
    return jobLogs;
  }

  private void setJobLogs(Set<JobLog> jobLogs) {
    this.jobLogs = jobLogs;
  }*/

  public long getLastExecutionTime() {
    return lastExecutionTime;
  }

  private void setLastExecutionTime(long lastExecutionTime) {
    this.lastExecutionTime = lastExecutionTime;
  }


  public void resetLastExecutionTime(){
    secondLastExecutionTime=getLastExecutionTime();
    setLastExecutionTime(System.currentTimeMillis());
  }

  public long getSecondLastExecutionTime() {
    return secondLastExecutionTime;
  }

  public static Job load(Serializable mainObjectId) throws FindByPrimaryKeyException {
    return (Job) PersistenceHome.findByPrimaryKey(Job.class, mainObjectId);
  }


}
