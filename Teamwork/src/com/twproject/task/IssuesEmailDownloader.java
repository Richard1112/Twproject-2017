package com.twproject.task;

import com.twproject.operator.TeamworkOperator;
import com.twproject.rank.Hit;
import com.twproject.resource.Person;
import com.twproject.resource.ResourceBricks;
import com.twproject.security.TeamworkPermissions;
import com.twproject.task.businessLogic.IssueAction;
import net.sf.json.JSONArray;
import net.sf.json.JSONObject;
import org.jblooming.messaging.Listener;
import org.jblooming.messaging.MailHelper;
import org.jblooming.messaging.MailMessageUtilities;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.operator.Operator;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.scheduler.ExecutableSupport;
import org.jblooming.scheduler.JobLogData;
import org.jblooming.security.License;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.HtmlSanitizer;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;

import javax.mail.*;
import javax.mail.internet.ContentType;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;
import java.io.IOException;
import java.util.Date;
import java.util.List;

public class IssuesEmailDownloader extends ExecutableSupport {

  public JobLogData run(JobLogData jobLogData) throws Exception {
    if (!License.assertLevel(30))
      return jobLogData;

    String emailTaskAssociation = ApplicationState.getApplicationSetting("EMAIL_TASK_ASSOCIATIONS");

    if (!JSP.ex(emailTaskAssociation))
      return jobLogData;
    List<JSONObject> jsa = new JSONArray();
    try {
      jsa = JSONArray.fromObject(emailTaskAssociation);
    } catch (Throwable t) {
      jobLogData.successfull = false;
      jobLogData.notes = "Invalid values in configuration \"EMAIL_TASK_ASSOCIATIONS\": " + emailTaskAssociation;
      return jobLogData;
    }


    //for each task-email association
    for (JSONObject json : jsa) {

      String host = json.getString("host");
      String user = json.getString("user");
      String taskId = json.getString("emailTask");


      //test if minimal config is available
      if (JSP.ex(host) && JSP.ex(user)) {

        boolean exception = true;
        PersistenceContext pc = null;

        Store store = null;
        Folder folder = null;

        try {
          pc = PersistenceContext.getDefaultPersistenceContext();

          Task task = Task.load(taskId, pc);

          // if task is found
          if (task != null) {

            //if task is active or ignore task status
            if ("no".equals(json.getString("active")) || task.isActive() && task.getSchedule().contains(new Date())) {


              String sport = json.getString("port");
              int port = JSP.ex(sport) ? Integer.parseInt(sport) : -1;
              //ope email server
              store = MailHelper.getPop3AndConnect(host, user, json.getString("password"), port, json.getString("protocol"));
              if (store != null) {

                boolean isPublic = "yes".equalsIgnoreCase(json.getString("public"));

                jobLogData.notes = jobLogData.notes + getClass().getName() + " executed on " + DateUtilities.dateAndHourToString(new Date());
                exception = false;

                folder = store.getFolder("INBOX");
                folder.open(Folder.READ_WRITE);
                Message[] messages = folder.getMessages();

                //loop for each message
                for (int i = 0, n = messages.length; i < n; i++) {
                  MimeMessage message = null;
                  String email = "--undefined--";

                  try {
                    message = (MimeMessage) messages[i];

                    //check if the message is auto replay
                    boolean isAutoReply = JSP.ex(message.getHeader("X-Autoreply", ",")) || JSP.ex(message.getHeader("X-Autorespond", ",")) || (message.getHeader("auto-submitted", ",") + "").toUpperCase().contains("AUTO-REPLIED");

                    if (!isAutoReply) {
                      //log e-mail
                      email = ((InternetAddress) message.getFrom()[0]).getAddress();
                      Tracer.emailLogger.debug("IssuesEmailDownloader\nfrom: " + email + "\nsubject: " + message.getSubject());

                      boolean isCalendricalMessage = calendricalMessage(message);

                      // when the content-Type is calendar ignore it
                      if (!isCalendricalMessage) {
                        handleTaskIssue(message, task, isPublic);
                      }
                    }
                    message.setFlag(Flags.Flag.DELETED, true);

                  } catch (Throwable e) {
                    try {
                      if (message != null) {
                        Tracer.emailLogger.error("IssuesEmailDownloader - E-mail could not be handled;\nfrom: " + email + "\nsubject: " + message.getSubject(), e);
                        message.setFlag(Flags.Flag.DELETED, true);
                      } else {
                        Tracer.emailLogger.error("IssuesEmailDownloader - E-mail could not be handled;\nfrom: " + email, e);
                      }
                    } catch (MessagingException e1) {
                      //"e" here is intentional: "e" is the important exception
                      Tracer.platformLogger.error(e);
                      Tracer.emailLogger.error(e);
                    }
                  }
                }


              } else {
                Tracer.jobLogger.warn("IssuesEmailDownloader.run cannot create mail session.");
              }
            }
          }
          pc.commitAndClose();
        } catch (Throwable e) {

          Tracer.platformLogger.error(getClass().getName() + " error", e);
          Tracer.emailLogger.error(getClass().getName() + " error", e);
          if (pc != null) {
            pc.rollbackAndClose();
          }

        } finally {

          if (folder != null)
            try {
              folder.close(true);
            } catch (MessagingException e) {
              Tracer.platformLogger.error(e);
              Tracer.emailLogger.error(e);
            }
          if (store != null)
            try {
              store.close();
            } catch (MessagingException e) {
              Tracer.platformLogger.error(e);
              Tracer.emailLogger.error(e);
            }

          if (exception) {
            jobLogData.successfull = false;
          }
        }
      }

    }
    return jobLogData;
  }

  private boolean calendricalMessage(Message message) throws MessagingException, IOException {
    boolean result = false;
    ContentType ct = MailMessageUtilities.getContentType(message.getContentType());
    if ("text".equalsIgnoreCase(ct.getPrimaryType()) && "calendar".equalsIgnoreCase(ct.getSubType()))
      result = true;
    else if ("multipart".equalsIgnoreCase(ct.getPrimaryType())) {

      for (Part part : MailHelper.getMessageParts(message)) {
        ContentType ctPart = MailMessageUtilities.getContentType(part.getContentType());
        if ("text".equalsIgnoreCase(ctPart.getPrimaryType()) && "calendar".equalsIgnoreCase(ctPart.getSubType())) {
          result = true;
          break;
        }
      }
    }
    return result;
  }


  private void handleTaskIssue(MimeMessage message, Task t, boolean isPublic) throws IOException, MessagingException, PersistenceException {

    String sender = ((InternetAddress) message.getFrom()[0]).getAddress();

    TeamworkOperator teamworkOperator = getOperatorAndGiveFeedback(message, isPublic);

    Person person = null;
    if (teamworkOperator != null)
      person = teamworkOperator.getPerson();

    //no permission send message back
    if (!(isPublic || (teamworkOperator != null && t.hasPermissionFor(teamworkOperator, TeamworkPermissions.issue_canCreate)))) {
      if (teamworkOperator != null) {
        // "User " + person.getDisplayName() + " does not have permission for adding issues on task " + t.getDisplayName()
        replyToMessage(message, "TWMAIL_%%_ERROR_ISSUE_NO_PERMISSION", teamworkOperator, person.getDisplayName(), t.getDisplayName());
      }
      return;
    }

    MailHelper.Email email = new MailHelper.Email(message);

    String emailContent = JSP.ex(email.html) ? email.html : email.text;
    String emailwithoutBR = StringUtilities.replaceAllNoRegex(emailContent, new String[]{"<br>", "\r"}, new String[]{"\n", " "});
    String sanitizedText = HtmlSanitizer.getText(emailwithoutBR);
    sanitizedText = sanitizedText.replaceAll("&nbsp;", " ");
    sanitizedText = sanitizedText.replaceAll("(\\n{2,}+)", "\n");


    if (JSP.ex(sanitizedText)) {

      Issue issue = new Issue();
      issue.setIdAsNew();
      issue.setTask(t);
      issue.setArea(t.getArea());
      issue.setStatusOpen();

      boolean isPrio = ((message.getHeader("X-Priority", ",") + "").toUpperCase().contains("HIGH")) ||
              ((message.getHeader("X-MSMail-Priority", ",") + "").toUpperCase().contains("HIGH")) ||
              ((message.getHeader("Importance", ",") + "").toUpperCase().contains("HIGH"));
      if (isPrio)
        issue.setGravity(Issue.GRAVITY_BLOCK);
      else
        issue.setGravity(Issue.GRAVITY_MEDIUM);

      issue.setDateSignalled(new Date());
      issue.setOwner(teamworkOperator);


      if (person != null)
        issue.setAssignedBy(person);


      String[] descrArray;
      String shortDescr = "";
      String subject = (JSP.ex(email.subject) ? JSP.encode(email.subject) + "\n" : "");

      if (JSP.ex(sanitizedText) && sanitizedText.length() > 200) {
        descrArray = sanitizedText.split(" ");
        for (int i = 0; i < descrArray.length; i++) {
          shortDescr = shortDescr + " " + descrArray[i];
          if (shortDescr.length() > 200) {
            shortDescr = shortDescr + "..." + I18n.get("ALL_MESSAGE_IN_COMMENT");
            break;
          }
        }


        subject = subject + shortDescr;
      } else {
        subject = subject + sanitizedText;
      }

      issue.setDescription(subject);

      if (teamworkOperator != null) {
        issue.setCreator(teamworkOperator.getDisplayName());
        issue.setLastModifier(teamworkOperator.getDisplayName());
      } else {
        issue.setCreator(sender);
        issue.setLastModifier(sender);
        issue.setExtRequesterEmail(sender);  // si registra l'external requester
      }


      for (PersistentFile pf : email.attachments)
        issue.addFile(pf);

      issue.store();
      if (JSP.ex(sanitizedText) && sanitizedText.length() > 200) {
        IssueHistory history = new IssueHistory();
        history.setIssue(issue);
        history.setComment(sanitizedText);
        history.store();
      }
      // questa notifica anche l'eventuale richiedente esterno
      IssueAction.createEventIssueAddedClosed(issue, true, teamworkOperator, person == null ? sender : person.getDisplayName());

      //subscribe sender if exists
      if (teamworkOperator != null) {
        Listener l = new Listener(teamworkOperator);
        l.setIdAsNew();
        l.setIdentifiable(issue);
        l.setMedia(StringUtilities.unSplit(teamworkOperator.getPreferredMediaOrDefault(MessagingSystem.Media.EMAIL), ","));
        l.setEventType(Issue.Event.ISSUE_CLOSE + "");
        l.setOneShot(true);
        //l.setValidityEnd();
        l.store();

        Hit.getInstanceAndStore(issue, teamworkOperator, .2);
      }


    }
  }


  private TeamworkOperator getOperatorAndGiveFeedback(Message message, boolean isPpublic) throws MessagingException, PersistenceException {

    //sends email replay only if is not public
    Person p = null;
    String sender = ((InternetAddress) message.getFrom()[0]).getAddress();
    List<Person> r = ResourceBricks.getPersonByEmail(sender);
    if (r.size() == 0) {
      if (!isPpublic)
        MailHelper.replyToMessage(message, I18n.get("TWMAIL_NO_USER_FOR_EMAIL") + " " + sender, true);
    } else if (r.size() > 1) {
      if (!isPpublic)
        MailHelper.replyToMessage(message, I18n.get("TWMAIL_MULTIPLE_USER_FOR_EMAIL") + " " + sender, true);
    } else {
      p = r.get(0);
    }
    TeamworkOperator ret = null;
    if (p != null) {
      ret = p.getMyself();
      if (ret == null && !isPpublic)
        MailHelper.replyToMessage(message, I18n.getLabel("TWMAIL_USER_FOR_EMAIL_%%_NO_LOGIN", sender), true);
    }
    return ret;

  }


  public static void replyToMessage(Message message, String errorMessageI18NLabel, Operator operator, String... parameters) throws MessagingException {
    String mess = I18n.getLabel(errorMessageI18NLabel, operator.getLanguage());
    mess = StringUtilities.replaceParameters(mess, parameters);
    MailHelper.replyToMessage(message, mess, true);
    Tracer.emailLogger.error("reply to message: " + mess);
  }


}