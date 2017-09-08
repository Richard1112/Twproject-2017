package com.twproject.messaging;

import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Person;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.agenda.Period;
import org.jblooming.messaging.MailHelper;
import org.jblooming.messaging.Message;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.scheduler.ExecutableSupport;
import org.jblooming.scheduler.JobLogData;
import org.jblooming.scheduler.Parameter;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.*;
import org.jblooming.waf.constants.OperatorConstants;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageSeed;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.TreeMap;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class CheckForTwprojectUpdates extends ExecutableSupport {


  public JobLogData run(JobLogData jobLogData) throws Exception {
    try {
      String checkUrl = "https://shop.twproject.com/utils/checkUpdates.jsp";
      //si recupera la versione online
      String onlineVersion = HttpUtilities.getPageContent(checkUrl);
      //se sono diverse
      if (!ApplicationState.getApplicationVersion().trim().equalsIgnoreCase(onlineVersion.trim())) {
        //si cercano tutti gli amministrator abilitati
        String hql = "select p from " + Person.class.getName() + " as p where p.myself is not null and p.myself.enabled=true and p.myself.administrator=true";
        List<Person> admins = new OqlQuery(hql).list();
        if (JSP.ex(admins)){
          for (Person p : admins) {
            String lang = p.getMyself().getLanguage();
            String msg=I18n.getLabel("NEW_TW_VERSION_AVAILABLE",lang)+": "+onlineVersion+"<br>"+
              I18n.getLabel("SEE_TW_CHANGELOG",lang)+" <a href=\"https://twproject.com/twproject-changelog/\" target=\"_blank\">https://twproject.com/twproject-changelog/</a>";
            sendMessage(p,msg,lang);
          }
        }
      }

      jobLogData.notes = jobLogData.notes + "CheckForTwprojectUpdates executed on " + DateUtilities.dateAndHourToString(new Date()) + ". Result:" + onlineVersion;
    } catch (Exception e) {
      Tracer.platformLogger.error("CheckForTwprojectUpdates error", e);
      jobLogData.successfull = false;
    }

    return jobLogData;
  }


  private void sendMessage(Person person, String messageStr,String lang) throws StoreException {
    TeamworkOperator teamworkOperator = person.getMyself();
    Set<String> medias = teamworkOperator.getPreferredMediaOrDefault(MessagingSystem.Media.STICKY);

    for (String media : medias) {
      Message message = new Message();
      message.setFromOperator(teamworkOperator);
      message.setToOperator(teamworkOperator);
      message.setMedia(media);
      message.setDefaultExpires();
      message.setSubject(I18n.getLabel("NEW_TW_VERSION_AVAILABLE",lang));
      message.setMessageBody(messageStr);
      message.setReceived(new Date());
      message.store();
    }


  }
}
