package com.twproject.messaging;

import com.twproject.agenda.Event;
import com.twproject.resource.Resource;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.agenda.ScheduleSupport;
import org.jblooming.messaging.Listener;
import org.jblooming.messaging.SomethingHappened;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.scheduler.ExecutableSupport;
import org.jblooming.scheduler.JobLogData;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.view.PageSeed;

import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.HashSet;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class AgendaEventFinder extends ExecutableSupport {

  public JobLogData run(JobLogData jobLogData) throws Exception {
    PersistenceContext pc = null;

    CompanyCalendar cc = new CompanyCalendar();
    Date todayAtMidn = cc.setAndGetTimeToDayEnd();

    try {

      pc = PersistenceContext.getDefaultPersistenceContext();

      String hql = "select event, listener from " + Event.class.getName() + " as event, " + Listener.class.getName() + " as listener " +
              "where " +
              //single event query
              "( (event.schedule.start > :beginYesterday) or " +
              //recurrent event
              "(event.schedule.start < :nownow and event.schedule.end > :nownow) ) and " +
              "listener.identifiableId = event.id and " +
              "listener.theClass = :eventClassName";

      OqlQuery query = new OqlQuery(hql);
      query.getQuery().setTimestamp("nownow", cc.getTime());
      cc.add(CompanyCalendar.DAY_OF_YEAR, -1);
      cc.setAndGetTimeToDayStart();
      query.getQuery().setTimestamp("beginYesterday", cc.getTime());
      query.getQuery().setString("eventClassName", Event.class.getName());
      List<Object[]> events = query.list();


      Set<Event> alreadyGenerated = new HashSet();

      //generate events
      for (Object[] evlist : events) {
        Event event = (Event) evlist[0];
        Listener listener = (Listener) evlist[1];

        long alertTimeValue = Long.parseLong(listener.getAdditionalParams().get("alertTime"));
        long lastTimeAlerted = Long.parseLong(listener.getAdditionalParams().get("lastTimeAlerted"));

        ScheduleSupport ss = event.getSchedule();
        long nta = ss.getNextFireTimeAfter(System.currentTimeMillis());
        if (System.currentTimeMillis() > (nta - alertTimeValue) && ((lastTimeAlerted < (nta - alertTimeValue)))) {
          listener.getAdditionalParams().put("lastTimeAlerted", System.currentTimeMillis() + "");
          listener.store();
          if (!alreadyGenerated.contains(event)) {
            storeEvent(event, listener, nta);
            alreadyGenerated.add(event);
          }
        }
      }
      pc.commitAndClose();

      jobLogData.notes = jobLogData.notes + "AgendaEventFinder executed on " + DateUtilities.dateAndHourToString(new Date());
    } catch (Throwable e) {
      Tracer.platformLogger.error("AgendaEventFinder error", e);

      if (pc != null) {
        pc.rollbackAndClose();
      }
      jobLogData.successfull = false;

    }

    return jobLogData;
  }

  private void storeEvent(Event event, Listener listener, long nextFireTimeAfter) throws StoreException {

    String happyParty = "";
    for (Resource r : event.getTargets()) {
      happyParty = happyParty + r.getDisplayName() + " ";
    }

    SomethingHappened change = new SomethingHappened();
    change.setIdAsNew();
    change.setIdentifiable(event);
    change.setEventType("AGENDA_EVENT_APPROACHING");
    change.setMessageTemplate("ALERT_BEFORE_X_HOURS_MESSAGE_TEMPLATE");
    change.getMessageParams().put("when",DateUtilities.dateToString(new Date(nextFireTimeAfter), "EEEE dd MMM yyyy HH:mm") +"-" +DateUtilities.dateToString(new Date(nextFireTimeAfter + event.getSchedule().getDurationInMillis()), "EEEE dd MMM yyyy HH:mm"));
    change.getMessageParams().put("what", JSP.limWr(event.getSummary(),1000));
    change.getMessageParams().put("where", JSP.w(event.getLocation()));
    change.getMessageParams().put("withWhom", happyParty);
    change.setWhoCausedTheEvent(null);
    PageSeed ps = new PageSeed(ApplicationState.serverURL + "/applications/teamwork/agenda/agendaWeekDay.jsp");
    ps.addClientEntry("FOCUS_MILLIS", nextFireTimeAfter);
    ButtonLink edit = new ButtonLink(ps);
    edit.label = event.getDescription();
    change.setLink(edit.toPlainLink());
    change.store();
  }
}

