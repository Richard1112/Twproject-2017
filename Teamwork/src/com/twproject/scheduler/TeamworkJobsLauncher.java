package com.twproject.scheduler;

import com.twproject.messaging.*;
import com.twproject.messaging.stickyNote.StickyMessageDispatcher;
import org.jblooming.security.License;
import com.twproject.security.LicenseUpdater;
import com.twproject.task.DataHistoryBuilder;
import com.twproject.task.IssuesEmailDownloader;
import com.twproject.worklog.ExecuteTimeCounterChecks;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.agenda.ScheduleDaily;
import org.jblooming.agenda.ScheduleMinute;
import org.jblooming.agenda.ScheduleWeekly;
import org.jblooming.messaging.DigestMessageDispatcher;
import org.jblooming.messaging.EmailMessageDispatcher;
import org.jblooming.messaging.EventListenerMatcher;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.scheduler.Job;
import org.jblooming.scheduler.Scheduler;
import org.jblooming.utilities.StringUtilities;

import java.util.Date;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class TeamworkJobsLauncher {

  public static void launch(String operatorName) {

    //in order not to have duplicates
    Scheduler instance = Scheduler.getInstance();
    if (instance != null)
      instance.stop();

    Scheduler.instantiate(5000, operatorName);

    PersistenceContext pc = null;
    try {
      pc = new PersistenceContext();

      addScheduleMinuteJob( ExecuteTimeCounterChecks.class, 2000, 5, 0, pc);

      addScheduleMinuteJob( EventListenerMatcher.class, 5000, 1, 0, pc);

      boolean activateMail = MessagingSystem.activeMedia.contains(MessagingSystem.Media.EMAIL);
      if (activateMail) {
        addScheduleMinuteJob( EmailMessageDispatcher.class, 5000, 1, 0, pc);
      }

      boolean activateSticky = MessagingSystem.activeMedia.contains(MessagingSystem.Media.STICKY);
      if (activateSticky) {
        addScheduleMinuteJob( StickyMessageDispatcher.class, 5000, 1, 0, pc);
      }

      addScheduleMinuteJob( MilestoneEventFinder.class, 50000, 720, 0, pc);

      addScheduleMinuteJob( ExpiredTaskFinder.class, 50000, 720, 0, pc);

      addScheduleMinuteJob( EmailDownloader.class, 60000, 5, 0, pc);

      if (License.assertLevel(30))
        addScheduleMinuteJob( IssuesEmailDownloader.class, 60000, 5, 0, pc);
      else
        removeJob( IssuesEmailDownloader.class,pc);

      addScheduleMinuteJob( AgendaEventFinder.class, 5000, 1, 0, pc);

      //addScheduleMinuteJob(GoogleCalendarDownloader.class, 60000, 3, 0, pc);

      //addScheduleDayJob(PersonEmailChecker.class,50000,1,0, pc);

      addScheduleDayJob(OrphanKiller.class, 0, 50000,1,0, pc);

      addScheduleDayJob(BudgetOverflowChecker.class, CompanyCalendar.MILLIS_IN_MINUTE*10, (int)CompanyCalendar.MILLIS_IN_MINUTE*5,1,0, pc);

      addScheduleDayJob(LicenseUpdater.class, CompanyCalendar.MILLIS_IN_MINUTE*((long)(Math.random()*60)), (int)CompanyCalendar.MILLIS_IN_MINUTE*5,1,0, pc);

      addScheduleWeeklyJob(DigestMessageDispatcher.class,new int[]{CompanyCalendar.MONDAY}, (int)CompanyCalendar.MILLIS_IN_3_HOUR,(int) CompanyCalendar.MILLIS_IN_HOUR,1,0,pc);

      addScheduleWeeklyJob(MissingWorklogChecker.class,new int[]{CompanyCalendar.FRIDAY}, (int)CompanyCalendar.MILLIS_IN_HOUR*16,(int) CompanyCalendar.MILLIS_IN_HOUR,1,0,pc);

      addScheduleWeeklyJob(CheckForTwprojectUpdates.class,new int[]{CompanyCalendar.THURSDAY}, (int)CompanyCalendar.MILLIS_IN_HOUR*15,(int) CompanyCalendar.MILLIS_IN_HOUR,1,0,pc);

      addScheduleDayJob(DataHistoryBuilder.class,  CompanyCalendar.MILLIS_IN_MINUTE*20, 50000,1,0,pc);

      pc.checkPoint();

    } catch (Throwable e) {
      throw new PlatformRuntimeException(e);
    } finally {
      if (pc != null)
        try {
          pc.commitAndClose();
        } catch (PersistenceException e) {
          throw new PlatformRuntimeException(e);
        }
    }
    instance = Scheduler.getInstance();
    instance.fillFromPersistence();
    instance.run();
  }

  private static void addScheduleDayJob(Class jobClass, long millisFromMidnight, int estimDur, int freq, int rep, PersistenceContext pc) throws StoreException {
    Job cmdJob = (Job) PersistenceHome.findUniqueNullIfEmpty(Job.class, "name", jobClass.getSimpleName(), pc);
    if (cmdJob == null) {
      cmdJob = new Job();
      cmdJob.setIdAsNew();
      cmdJob.setName(jobClass.getSimpleName());
      cmdJob.setDescription(StringUtilities.deCamel(jobClass.getSimpleName()));
      cmdJob.setExecutable(jobClass.getName());
      cmdJob.setEstimatedDuration(estimDur);
      cmdJob.setOnErrorRetryNow(true);
      cmdJob.setOnErrorSuspendScheduling(false);

      CompanyCalendar cc= new CompanyCalendar();
      cc.setAndGetTimeToDayEnd();

      cc.add(CompanyCalendar.MILLISECOND,(int)millisFromMidnight+2);

      ScheduleDaily sm = new ScheduleDaily(cc.getTime(), 50000, freq, rep);
      sm.store(pc);
      cmdJob.setSchedule(sm);
      cmdJob.store(pc);
    }
  }

  private static void removeJob(Class jobClass, PersistenceContext pc) throws PersistenceException {
    Job cmdJob = (Job) PersistenceHome.findUniqueNullIfEmpty(Job.class, "name", jobClass.getSimpleName(), pc);
    if (cmdJob != null) {
      cmdJob.remove(pc);
    }
  }

  private static void addScheduleMinuteJob(Class jobClass, int estimDur, int freq, int rep, PersistenceContext pc) throws StoreException {
    Job cmdJob = (Job) PersistenceHome.findUniqueNullIfEmpty(Job.class, "name", jobClass.getSimpleName(), pc);
    if (cmdJob == null) {
      cmdJob = new Job();
      cmdJob.setIdAsNew();
      cmdJob.setName(jobClass.getSimpleName());
      cmdJob.setDescription(StringUtilities.deCamel(jobClass.getSimpleName()));
      cmdJob.setExecutable(jobClass.getName());
      cmdJob.setEstimatedDuration(estimDur);
      cmdJob.setOnErrorRetryNow(false);
      cmdJob.setOnErrorSuspendScheduling(false);
      ScheduleMinute sm = new ScheduleMinute(new Date(), estimDur, freq, rep);
      sm.store(pc);
      cmdJob.setSchedule(sm);
      cmdJob.store(pc);
    }
  }

  private static void addScheduleWeeklyJob(Class jobClass, int days[], int startHourInMillis, int duration, int freq, int repetition, PersistenceContext pc) throws StoreException {
    Job cmdJob = (Job) PersistenceHome.findUniqueNullIfEmpty(Job.class, "name", jobClass.getSimpleName(), pc);
    if (cmdJob == null) {
      cmdJob = new Job();
      cmdJob.setIdAsNew();
      cmdJob.setName(jobClass.getSimpleName());
      cmdJob.setDescription(StringUtilities.deCamel(jobClass.getSimpleName()));
      cmdJob.setExecutable(jobClass.getName());
      cmdJob.setEstimatedDuration(duration);
      cmdJob.setOnErrorRetryNow(false);
      cmdJob.setOnErrorSuspendScheduling(false);

      CompanyCalendar cal= new CompanyCalendar(new Date());
      cal.setMillisFromMidnight(startHourInMillis);
      ScheduleWeekly sw= new ScheduleWeekly(days,cal.getTime(),duration,freq, repetition);

      sw.store(pc);
      cmdJob.setSchedule(sw);
      cmdJob.store(pc);
    }
  }
}
