package org.jblooming.messaging;

import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.operator.Operator;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.scheduler.ExecutableSupport;
import org.jblooming.scheduler.JobLogData;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.settings.Application;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;

import java.util.*;

public class DigestMessageDispatcher extends ExecutableSupport {

  public JobLogData run(JobLogData jobLogData) throws Exception {
    PersistenceContext pc = null;
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();
      // get all messages in DIGEST channel
      String hql = "from " + Message.class.getName() + " as mess where mess.media = :media order by mess.toOperator, mess.received";
      OqlQuery query = new OqlQuery(hql);
      query.getQuery().setString("media", MessagingSystem.Media.DIGEST.toString());

      List<Message> messages = query.list();

      // loop for all messages in order to spread them in a map<Operator,map<Date,List<Messages>>>
      Map<Operator, Map<Date, List<Message>>> digests = new HashMap<Operator, Map<Date, List<Message>>>();

      for (Message message : messages) {

        //test already have an entry
        Map<Date, List<Message>> digestsForOperator = digests.get(message.getToOperator());
        if (digestsForOperator == null) {
          digestsForOperator = new HashMap<Date, List<Message>>();
          digests.put(message.getToOperator(), digestsForOperator);
        }

        // get the day begin for the message in order to group by date
        Date dayNormalized = new CompanyCalendar(message.getReceived()).setAndGetTimeToDayStart();
        List<Message> massagesOfDay = digestsForOperator.get(dayNormalized);
        if (massagesOfDay == null) {
          massagesOfDay = new ArrayList<Message>();
          digestsForOperator.put(dayNormalized, massagesOfDay);
        }

        //add message to the day list
        massagesOfDay.add(message);

      }

      // lop for all operators in digests
      for (Operator operator : digests.keySet()) {

        String language = operator.getLanguage();

        String digested = "";

        Map<Date, List<Message>> userMessages = digests.get(operator);

        for (Date aDay : userMessages.keySet()) {
          digested += "<div style='background-color:#e9e9e9; padding:15px'>";
          digested += JSP.makeTag("div", "style=\"max-width:550px; margin: 0 auto; background-color: #2f97c6;color:#fff;padding:5px 15px 5px;margin-bottom:0\"", JSP.makeTag("p", "style=\"margin:0;padding-bottom:2px; border-bottom: 2px solid #2f97c6;\"", DateUtilities.dateToFullString(aDay)));

          //loop for messages
          for (Message message : userMessages.get(aDay)) {

            digested += "<div style='background-color:#fff; padding:15px; max-width:550px; margin: 0 auto; margin-bottom: 0'>";
            digested += JSP.makeTag("h2", "style=\"color:#333;margin:10px 0 5px\"", message.getSubject());
            digested += JSP.makeTag("p", "style=\"color:#444;margin:5px 0 10px; font-size: 120%; line-height: 140%;\"", message.getMessageBody());
            if (JSP.ex(message.getLink())) {
              digested += "<div>" + JSP.w(message.getLink()) + "</div>";
            }
            digested += "</div>";

            //remove the message
            message.remove();
          }
          digested += "</div>";
        }


        //if (Fields.TRUE.equals(operator.getOption("MESSAGE_DIGESTER_BY_EMAIL"))){
        //Creates a new message for email/rss channels depending on user options
        Message digestedMessage = new Message();
        digestedMessage.setDefaultExpires();
        digestedMessage.setFromOperator(null);
        digestedMessage.setMedia(MessagingSystem.Media.EMAIL + "");
        digestedMessage.setMessageBody(digested);
        digestedMessage.setReceived(new Date());
        //digestedMessage.setStatus();
        Application app = ApplicationState.platformConfiguration.defaultApplication;
        String subject = I18n.getLabel("DIGEST_MESSAGE_SUBJECT", app.getName(), language);

        digestedMessage.setSubject(subject);
        digestedMessage.setToOperator(operator);

        digestedMessage.store();
        //}
      }


      pc.commitAndClose();
      jobLogData.notes = jobLogData.notes + "DigestMessageDispatcher executed on " + DateUtilities.dateAndHourToString(new Date());
    } catch (Throwable e) {
      Tracer.platformLogger.error("DigestMessageDispatcher error", e);
      Tracer.emailLogger.error("DigestMessageDispatcher error", e);
      jobLogData.successfull = false;
      if (pc != null)
        pc.rollbackAndClose();
    }

    return jobLogData;
  }
}
