package com.twproject.messaging;

import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Person;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.agenda.Period;
import org.jblooming.messaging.Message;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.scheduler.ExecutableSupport;
import org.jblooming.scheduler.JobLogData;
import org.jblooming.scheduler.Parameter;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.CollectionUtilities;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.constants.OperatorConstants;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageSeed;

import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.TreeMap;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class MissingWorklogChecker extends ExecutableSupport {

  @Parameter("15")
  public int maxDaysToCheck=15;
  @Parameter("3")
  public int minDaysToCheck=3;
  @Parameter("no")
  public boolean showUnavailability=false;
  @Parameter("no")
  public boolean showOverwork=false;


  public JobLogData run(JobLogData jobLogData) throws Exception {
    PersistenceContext pc = null;

    CompanyCalendar cc= new CompanyCalendar();
    long lastRun=this.secondLastExecutionTime;

    //check max
    lastRun=lastRun< (System.currentTimeMillis()-CompanyCalendar.MILLIS_IN_DAY*maxDaysToCheck)?(System.currentTimeMillis()-CompanyCalendar.MILLIS_IN_DAY*maxDaysToCheck):lastRun;


    //check min
    lastRun= lastRun> (System.currentTimeMillis()-CompanyCalendar.MILLIS_IN_DAY*minDaysToCheck)?(System.currentTimeMillis()-CompanyCalendar.MILLIS_IN_DAY*minDaysToCheck):lastRun;

    cc.setTimeInMillis(lastRun);
    Date start = cc.setAndGetTimeToDayStart();

    cc.setTimeInMillis(System.currentTimeMillis());
    Date end = cc.setAndGetTimeToDayEnd();


    Period period = new Period(start, end);

    try {
      pc = PersistenceContext.getDefaultPersistenceContext();
      String hql = "select distinct op from " + TeamworkOperator.class.getName() + " as op where op.enabled=true";
      OqlQuery query = new OqlQuery(hql);
      List<TeamworkOperator> lts = query.list();

      for (TeamworkOperator op:lts){
        Person person = op.getPerson();


        TreeMap<Integer,Long> workablePlan = person.getWorkablePlan(period); //for each day the workable millis for that day. If day is holidays -1  if is completely unaivailable millis are -2
        TreeMap<Integer,Long> workedPlan = person.getWorkedPlan(period); //for each day the worked millis for that day.

        long defaultDailyCapacity = person.getWorkDailyCapacity();

        PageSeed agendaDayForResource=new PageSeed(ApplicationState.serverURL+"/applications/teamwork/agenda/agendaWeekDay.jsp");
        PageSeed worlogDayForResource=new PageSeed(ApplicationState.serverURL+"/applications/teamwork/task/worklog/worklogWeek.jsp");

        agendaDayForResource.addClientEntry("AGENDA_TYPE", "DAY"); // day
        agendaDayForResource.addClientEntry("WG_IDS",person.getId()); // set persona giusta
        worlogDayForResource.addClientEntry("RES_ID",person.getId()); // set persona giusta

        String lang = op.getLanguage();
        String message="<table cellpadding=\"3\" cellspacing=\"3\">" +
          "<tr><th>"+I18n.getLabel("DATE",lang)+"</th><th>"+I18n.getLabel("WORKLOG_DONE",lang)+"</th><th>"+I18n.getLabel("WORKING_HOUR_TOTAL",lang)+"</th></tr>";

        boolean notify=false;
        for (int day:workablePlan.keySet()){
          Date date = DateUtilities.intToDate(day);
          long dayCapacity = workablePlan.get(day);
          Long wL = workedPlan.get(day);
          long worked=wL==null?0:wL;

          agendaDayForResource.addClientEntry("FOCUS_MILLIS",date.getTime()); // set giorno giusto
          worlogDayForResource.addClientEntry("FOCUS_MILLIS",date.getTime()); // set giorno giusto

          //salta i giorni di festa
          if (dayCapacity==-1){
            //hai lavorato?
            if (worked>0 && showOverwork){
              message+="<tr><td bgcolor=\"#FFF5E6\">"+DateUtilities.dateToFullString(date)+"</td><td bgcolor=\"#FFF5E6\">"+" <a class=\"message_link\" href=\""+worlogDayForResource.toLinkToHref()+"\">"+DateUtilities.getMillisInHoursMinutes(worked)+"</a></td><td bgcolor=\"#FFF5E6\">0:00</td></tr>";
              notify=true;
            }
            continue;
          }

          // salta giorni completi di ferie
          if (dayCapacity==-2){
            String d = "<tr><td>"+DateUtilities.dateToFullString(date) + "</td>";
            if (worked>0 && showOverwork){
              message+=d+ "<td><a class=\"message_link\" href=\""+worlogDayForResource.toLinkToHref()+"\">"+DateUtilities.getMillisInHoursMinutes(worked)+"</a></td>";
              notify=true;
              d = "";
            }

            if (showUnavailability){
              message+=d+"<td colspan=2 bgcolor=\"#e6c2df\"><a class=\"message_link\" href=\""+agendaDayForResource.toLinkToHref()+"\">"+ I18n.getLabel("PLAN_IN_VACATION_LEGENDA", lang)+"</a></td>";
              notify=true;
              d = "";
            }

            message+=(JSP.ex(d)?"":"</td></tr>");
            continue;
          }

          //hai lavorato più o meno del previsto?
          if (worked<dayCapacity || (worked>dayCapacity && showOverwork)){
            notify=true;
            message+="<tr><td>"+DateUtilities.dateToFullString(date) + "</td><td> ";

            message+=" <a class=\"message_link\" href=\""+worlogDayForResource.toLinkToHref()+"\">"+ DateUtilities.getMillisInHoursMinutes(worked)+"</a></td>";


            //ci sono ferie o permessi?
            if (dayCapacity<defaultDailyCapacity){
              message+="<td bgcolor=\"#e6c2df\"><a class=\"message_link\" href=\""+agendaDayForResource.toLinkToHref()+"\">"+DateUtilities.getMillisInHoursMinutes(dayCapacity)+"</a></td> ";
            } else {
              message+="<td>"+DateUtilities.getMillisInHoursMinutes(dayCapacity)+"</td>";
            }

            message+="</tr>";
          }

        }
        if (notify){
          message="<b>"+I18n.getLabel("MISSING_WORKLOGS", lang)+"</b><br><br>" +
            I18n.getLabel("PERIOD", lang)+": "+DateUtilities.dateToFullString(period.getStartDate())+"-"+DateUtilities.dateToFullString(period.getEndDate())+
            "<br><hr>"+message+"</table>";
          sendMessage(person, message,lang);
        }

      }

      pc.commitAndClose();
      jobLogData.notes = jobLogData.notes + getClass().getName() + " executed on " + DateUtilities.dateAndHourToString(new java.util.Date());
    } catch (Throwable e) {
      Tracer.platformLogger.error(getClass().getName() + " error", e);
      jobLogData.successfull = false;

      if (pc != null) {
        pc.rollbackAndClose();
      }
    }

    return jobLogData;
  }

  private void sendMessage(Person person, String messageStr, String lang) throws StoreException {
    TeamworkOperator teamworkOperator = person.getMyself();
    Set<String> medias =teamworkOperator.getPreferredMediaOrDefault(MessagingSystem.Media.STICKY);

    for (String media : medias) {
      Message message = new Message();
      message.setFromOperator(teamworkOperator);
      message.setToOperator(teamworkOperator);
      message.setMedia(media);
      message.setDefaultExpires();
      message.setSubject(I18n.getLabel("PLEASE_COMPLETE_TIMESHEET", lang));
      message.setMessageBody(messageStr);
      message.setReceived(new Date());
      message.store();
    }


  }
}
