package com.opnlb.website.forum.scheduler;

import org.jblooming.scheduler.Scheduler;
import org.jblooming.scheduler.Job;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.agenda.ScheduleMinute;
import org.jblooming.messaging.EventListenerMatcher;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.messaging.EmailMessageDispatcher;

import java.util.Date;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Feb 5, 2007
 * Time: 4:15:24 PM
 */
public class ForumJobsLauncher {

  public static void launch(String operatorName) throws StoreException {

    //in order not to have duplicates
    Scheduler instance = Scheduler.getInstance();
    if (instance!=null)
      instance.stop();

    Scheduler.instantiate(5000, operatorName);
    instance = Scheduler.getInstance();

    Job elmJob = (Job) PersistenceHome.findUniqueNullIfEmpty(Job.class, "name", "EventListenerMatcher");
    if (elmJob == null) {
      elmJob = new Job();
      elmJob.setIdAsNew();
      elmJob.setName("EventListenerMatcher");
      elmJob.setDescription("matches the content of the event list on the content of the listener list");
      elmJob.setExecutable(EventListenerMatcher.class.getName());
      elmJob.setEstimatedDuration(5000);
      elmJob.setOnErrorRetryNow(true);
      elmJob.setOnErrorSuspendScheduling(false);
      ScheduleMinute sm = new ScheduleMinute(new Date(), 5000, 1, 0);
      sm.store();
      elmJob.setSchedule(sm);
      elmJob.store();
    }
    instance.addJob(elmJob);

    boolean activateMail = MessagingSystem.activeMedia.contains(MessagingSystem.Media.EMAIL);
    if (activateMail) {
      Job emailMessageJob = (Job) PersistenceHome.findUniqueNullIfEmpty(Job.class, "name", "EmailMessageDispatcher");
      if (emailMessageJob == null) {
        emailMessageJob = new Job();
        emailMessageJob.setIdAsNew();
        emailMessageJob.setName("EmailMessageDispatcher");
        emailMessageJob.setDescription("send messages by mail");
        emailMessageJob.setExecutable(EmailMessageDispatcher.class.getName());
        emailMessageJob.setEstimatedDuration(5000);
        emailMessageJob.setOnErrorRetryNow(true);
        emailMessageJob.setOnErrorSuspendScheduling(false);
        ScheduleMinute sm = new ScheduleMinute(new Date(), 5000, 1, 0);
        sm.store();
        emailMessageJob.setSchedule(sm);
        emailMessageJob.store();
      }
      instance.addJob(emailMessageJob);
    }

    instance.run();
  }

}
