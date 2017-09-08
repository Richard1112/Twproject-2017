package com.twproject.messaging;

import com.twproject.resource.Person;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.scheduler.ExecutableSupport;
import org.jblooming.scheduler.JobLogData;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.DateUtilities;

import java.util.Date;
import java.util.List;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class AreaKiller extends ExecutableSupport {

  public JobLogData run(JobLogData jobLogData) throws Exception {
    PersistenceContext pc = null;

    try {
      pc = PersistenceContext.getDefaultPersistenceContext();

      String hql="select p from "+Person.class.getName()+" as p where p.area.expiry<:yerlaltr";
      OqlQuery query = new OqlQuery(hql,pc);
      query.setParameter("yerlaltr",new Date(System.currentTimeMillis()-CompanyCalendar.MILLIS_IN_WEEK));
      List<Person>ps=query.list();
      for (Person p:ps){
        if (p.getMyself()!=null){
          p.getMyself().setEnabled(false);
          p.getMyself().store(pc);
        }

      }

      pc.commitAndClose();

      jobLogData.notes = jobLogData.notes + getClass().getName() + " executed on " + DateUtilities.dateAndHourToString(new Date());
    } catch (Throwable e) {
      Tracer.platformLogger.error(getClass().getName() + " error", e);
      jobLogData.successfull = false;

      if (pc != null) {
        pc.rollbackAndClose();
      }
    }


    return jobLogData;
  }

}
