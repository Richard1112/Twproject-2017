package org.jblooming.messaging;

import org.jblooming.messaging.SomethingHappened;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.scheduler.ExecutableSupport;
import org.jblooming.scheduler.JobLogData;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.DateUtilities;

import java.util.Date;

public class ClockEventDispatcher extends ExecutableSupport {

  public JobLogData run(JobLogData jobLogData) throws Exception {

    PersistenceContext pc = null;

    try {
      pc = PersistenceContext.getDefaultPersistenceContext();

      SomethingHappened timerEvent = new SomethingHappened("NOT_AVAILABLE", "NOT_AVAILABLE", MessagingSystem.TIMER_EVENT, new Date(), null);
      timerEvent.store();
      pc.commitAndClose();

      jobLogData.notes = jobLogData.notes + "ClockEventDispatcher executed on " + DateUtilities.dateAndHourToString(new Date());
    } catch (Throwable e) {
      Tracer.platformLogger.error("ClockEventDispatcher error",e);

    } finally {
        if (pc != null) {
          pc.rollbackAndClose();
        }
        jobLogData.successfull = false;
    }

    return jobLogData;
  }
}
