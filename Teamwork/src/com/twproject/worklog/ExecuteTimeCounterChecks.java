package com.twproject.worklog;

import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Person;
import com.twproject.resource.Resource;
import com.twproject.task.Assignment;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.messaging.Message;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.operator.Operator;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.scheduler.ExecutableSupport;
import org.jblooming.scheduler.JobLogData;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.settings.Application;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageSeed;

import java.util.Date;
import java.util.List;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class ExecuteTimeCounterChecks extends ExecutableSupport {

  public JobLogData run(JobLogData jobLogData) {

    PersistenceContext pc = null;
    try {

      pc = PersistenceContext.getDefaultPersistenceContext();

      String hql = "from " + Assignment.class.getName() + " assig where assig.counted=:truth";
      OqlQuery oql = new OqlQuery(hql);
      oql.getQuery().setBoolean("truth", Boolean.TRUE);
      List countedAssig = null;

      countedAssig = oql.list();

      Application app = ApplicationState.platformConfiguration.defaultApplication;

      for (Object assigO : countedAssig) {
        Assignment assig = (Assignment) assigO;
        Resource r = (Resource) ReflectionUtilities.getUnderlyingObject(assig.getResource());

        if (r instanceof Person) {
          TeamworkOperator op = r.getMyself();
          String language = op.getLanguage();

          long millis = r.getWorkDailyCapacity();
          if ((System.currentTimeMillis() - assig.getCountingStartedAt().getTime()) > millis) {

            Worklog worklog = assig.closeCounter(null);
            worklog.store();

            Message message = new Message();
            message.setFromOperator(null);
            message.setToOperator(op);
            message.setDefaultExpires();
            message.setMedia(MessagingSystem.Media.STICKY.toString());
            String subject = I18n.getLabel("COUNTER_EXPIRED", app.getName(), language);
            message.setSubject(subject);

            PageSeed ps = new PageSeed(ApplicationState.serverURL + "/applications/teamwork/task/worklog/worklogWeek.jsp");
            ps.addClientEntry("ASS_ID", assig.getId());
            ps.command="EXPANDASS";
            ButtonLink editLink = ButtonLink.getTextualInstance(subject, ps);

            message.setLink(editLink.toPlainLink());
            message.setMessageBody(assig.getDisplayNameWithTask());
            message.setReceived(new Date());
            message.store();

            assig.store();
            Tracer.platformLogger.debug("ExecuteTimeCounterChecks counter closed for assig " + assig.getId());

          }

        }
      }

      pc.commitAndClose();

      jobLogData.notes = jobLogData.notes + "ExecuteTimeCounterChecks executed on " + DateUtilities.dateAndHourToString(new Date());

    } catch (Throwable e) {
      Tracer.platformLogger.error("ExecuteTimeCounterChecks error", e);
      if (pc != null) {
        pc.rollbackAndClose();
      }
      jobLogData.successfull = false;
    }
    return jobLogData;
  }


}
