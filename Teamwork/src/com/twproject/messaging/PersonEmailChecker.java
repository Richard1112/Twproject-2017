package com.twproject.messaging;

import org.jblooming.scheduler.ExecutableSupport;
import org.jblooming.scheduler.JobLogData;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.oql.OqlQuery;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.tracer.Tracer;
import org.jblooming.messaging.Message;
import org.jblooming.messaging.MessagingSystem;
import com.twproject.resource.Person;
import com.twproject.operator.TeamworkOperator;

import java.util.List;
import java.util.Date;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class PersonEmailChecker extends ExecutableSupport {

  public JobLogData run(JobLogData jobLogData) throws Exception {

    PersistenceContext pc = null;

    CompanyCalendar cc = new CompanyCalendar();
    cc.setAndGetTimeToDayStart();
    long now = cc.getTime().getTime();

    try {

      pc = PersistenceContext.getDefaultPersistenceContext();

      /**
       * Search for enabled operator without e-mail
       */
      String hql = "select distinct p from " + Person.class.getName() + " as p where (p.myself.loginName is not null) and (p.myself.loginName<>'') and (p.myself.enabled=:t)";
      OqlQuery query = new OqlQuery(hql);
      query.getQuery().setBoolean("t", true);
      List<Person> tasks = query.list();

      //generate events
      for (Person person : tasks) {
        if (!JSP.ex(person.getDefaultEmail())) {
          TeamworkOperator twOp = person.getMyself();
          String language = twOp.getLanguage();
          String subject = I18n.getLabel("TWSYSCHECK_NO_EMAIL_SUBJECT", language);
          String body = I18n.getLabel("TWSYSCHECK_%%_NO_EMAIL_BODY", language);

          PageSeed mySelf = new PageSeed(ApplicationState.serverURL + "/applications/teamwork/resource/resourceEditor.jsp");
          mySelf.setCommand(Commands.EDIT);
          mySelf.setMainObjectId(person.getId());
          ButtonLink link = new ButtonLink(person.getDisplayName(), mySelf);

          Message mess = new Message(subject, StringUtilities.replaceParameters(body, link.toPlainLink()), twOp, MessagingSystem.Media.STICKY + "");
          mess.store();

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
