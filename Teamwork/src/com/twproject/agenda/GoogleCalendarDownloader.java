package com.twproject.agenda;

import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Person;
import net.fortuna.ical4j.data.ParserException;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.scheduler.ExecutableSupport;
import org.jblooming.scheduler.JobLogData;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;

import java.io.IOException;
import java.net.URL;
import java.util.*;

/**
 * Created by Silvia The Great
 * (c) Open Lab ab urbe condita - today.
 */
public class GoogleCalendarDownloader extends ExecutableSupport {

  public JobLogData run(JobLogData jobLogData) throws Exception {

    PersistenceContext pc = null;
    try {

      pc = PersistenceContext.getDefaultPersistenceContext();

      //String sqlSelect = "select distinct p from " + Person.class.getName() + " as p";
      String hql = "select p from " + Person.class.getName() + " p join p.myself as op where op.options['SEND_TO_GOOGLE'] like  'yes'";
      OqlQuery query = new OqlQuery(hql, pc);
      List<Person> list = query.list();

      for (Person person : list) {
        try {
          TeamworkOperator twOp = person.getMyself();
          String urlString = "";
          String googleAccount = twOp.getOption("GOOGLE_LOGIN_USER");
          if (JSP.ex(googleAccount)) {
            if (!googleAccount.contains("@"))
              googleAccount = googleAccount + "@gmail.com";
            urlString = "http://www.google.com/calendar/ical/" + googleAccount + "/public/basic.ics";

            URL url = new URL(urlString);
            IcalUtilities.createEvents(url, person, false);
          }
        } catch (IOException e) {
        } catch (ParserException e) {
        }
      }

      pc.commitAndClose();

    } catch (Throwable e) {
      Tracer.platformLogger.error(getClass().getName() + " error");
      jobLogData.successfull = false;

      if (pc != null) {
        pc.rollbackAndClose();
      }
    }

    jobLogData.notes = jobLogData.notes + getClass().getName() + " executed on " + DateUtilities.dateAndHourToString(new Date());
    return jobLogData;
  }

}
