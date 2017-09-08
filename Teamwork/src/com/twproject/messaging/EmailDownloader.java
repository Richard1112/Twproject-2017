package com.twproject.messaging;

import com.twproject.agenda.IcalUtilities;
import com.twproject.document.TeamworkDocument;
import com.twproject.document.businessLogic.DocumentAction;
import com.twproject.exchange.contacts.businessLogic.ResourcesImportControllerAction;
import com.twproject.operator.TeamworkOperator;
import com.twproject.rank.Hit;
import com.twproject.resource.Person;
import com.twproject.resource.ResourceBricks;
import com.twproject.security.TeamworkPermissions;
import com.twproject.task.Issue;
import com.twproject.task.IssueHistory;
import com.twproject.task.Task;
import com.twproject.task.TaskBricks;
import com.twproject.task.businessLogic.AssignmentAction;
import com.twproject.task.businessLogic.IssueAction;
import com.twproject.task.businessLogic.TaskAction;
import net.fortuna.ical4j.data.CalendarBuilder;
import net.fortuna.ical4j.data.ParserException;
import net.fortuna.ical4j.model.Component;
import net.fortuna.ical4j.model.component.VEvent;
import net.wimpi.pim.contact.io.vcard.vCardUnmarshaller;
import net.wimpi.pim.contact.model.Communications;
import net.wimpi.pim.contact.model.Contact;
import net.wimpi.pim.contact.model.PersonalIdentity;
import net.wimpi.pim.contact.model.PhoneNumber;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.messaging.Listener;
import org.jblooming.messaging.MailHelper;
import org.jblooming.messaging.MailMessageUtilities;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.operator.Operator;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.scheduler.ExecutableSupport;
import org.jblooming.scheduler.JobLogData;
import org.jblooming.security.License;
import org.jblooming.system.SystemConstants;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.HtmlSanitizer;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.RestState;

import javax.mail.*;
import javax.mail.internet.ContentType;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;
import java.io.IOException;
import java.io.InputStream;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Apr 16, 2007
 * Time: 2:37:36 PM
 */
public class EmailDownloader extends ExecutableSupport {

  public JobLogData run(JobLogData jobLogData) throws Exception {

    String FLD_POP3_HOST = ApplicationState.getApplicationSetting(SystemConstants.FLD_POP3_HOST);
    String FLD_POP3_USER = ApplicationState.getApplicationSetting(SystemConstants.FLD_POP3_USER);

    if (FLD_POP3_HOST != null && FLD_POP3_USER != null && FLD_POP3_HOST.trim().length() > 0 && FLD_POP3_USER.trim().length() > 0) {

      boolean exception = true;
      PersistenceContext pc = null;

      Store store = null;
      Folder folder = null;

      try {

        store = MailHelper.getPop3AndConnect();
        if (store != null) {
          pc = PersistenceContext.getDefaultPersistenceContext();

          jobLogData.notes = jobLogData.notes + getClass().getName() + " executed on " + DateUtilities.dateAndHourToString(new Date());
          exception = false;

          folder = store.getFolder("INBOX");
          folder.open(Folder.READ_WRITE);
          Message[] messages = folder.getMessages();
          for (int i = 0, n = messages.length; i < n; i++) {
            MimeMessage message = (MimeMessage) messages[i];

            InternetAddress internetAddress = (InternetAddress) message.getFrom()[0];

            try {

              boolean isAutoReply = JSP.ex(message.getHeader("X-Autoreply", ","))
                      || JSP.ex(message.getHeader("X-Autorespond", ","))
                      || (message.getHeader("auto-submitted", ",") + "").toUpperCase().contains("AUTO-REPLIED");
              if (!isAutoReply) {

                //log e-mail
                String email = internetAddress.getAddress();
                Tracer.emailLogger.debug("EmailDownloader\n" + "from: " + email + "\nsubject: " + message.getSubject());

                boolean isCalendricalMessage = calendricalMessage(message);

                // when the content-Type is calendar
                if (isCalendricalMessage) {
                  handleCalendar(message);
                } else {
                  //tutto gli altri tipi di interazione
                  handleMessage(message);
                }
              }
              message.setFlag(Flags.Flag.DELETED, true);
            } catch (Throwable e) {
              try {

                Tracer.emailLogger.error("EmailDownloader - E-mail could not be handled;\n" + "from: " + internetAddress + "\nsubject: " + message.getSubject(), e);
                message.setFlag(Flags.Flag.DELETED, true);
              } catch (MessagingException e1) {
                //e here is intentional: e is the important exception
                Tracer.platformLogger.error(e);
                Tracer.emailLogger.error(e);
              }
            }
          }

          pc.commitAndClose();
        } else {
          Tracer.jobLogger.warn("EmailDownloader.run cannot create mail session");
        }
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
      //} else
      //  jobLogData.notes = jobLogData.notes + getClass().getName() + " executed on " + DateUtilities.dateAndHourToString(new Date());
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

  private void handleMessage(MimeMessage message) throws IOException, MessagingException, PersistenceException {
    if (!License.assertLevel(30))
      return;

    String subject = JSP.w(message.getSubject()).trim();

    /**
     * messages should be in this format:
     * [TASK] #[TASK_EASY_ID] | [TASK_CODE] | [TASK_NAME] | [TASK_ID]# [attachment]|[issue]
     * [T0D0]
     * [NEW TASK]|[NEW PROJECT]|[NEW]
     */

    boolean handled = false;

    TeamworkOperator teamworkOperator = getOperatorFromMessage(message);
    // caso di mail mandata da utente TW e attivo
    if (teamworkOperator != null && teamworkOperator.isEnabled()) {
      if (subject != null) {
        String subjectUC = message.getSubject().trim().toUpperCase();

        //  ------------------------------------------------------------------------------------------------- TASK & ISSUES handling
        if (subjectUC.toUpperCase().startsWith("TASK")) {
          handled = true;
          Matcher matcher = Pattern.compile("#.*#").matcher(subjectUC);

          if (matcher.find()) {
            Task t = null;
            String codeFurbOrElse = matcher.group();
            //The offset after the last character matched
            int pos = matcher.end();
            codeFurbOrElse = codeFurbOrElse.substring(1, codeFurbOrElse.length() - 1);
            String command = subjectUC.substring(pos).toUpperCase().trim();

            try { // first try to use the task id
              int filterInt = Integer.parseInt(codeFurbOrElse);
              t = (Task) PersistenceHome.findByPrimaryKey(Task.class, filterInt + "");

            } catch (NumberFormatException nfe) { // then try to use code or name
              String hql = "select t from " + Task.class.getName() + " as t where t.code=:filter or t.name=:filter ";
              OqlQuery oql = new OqlQuery(hql);
              oql.getQuery().setString("filter", codeFurbOrElse);
              try {
                t = (Task) oql.uniqueResult();
              } catch (FindException e) {
                //"Invalid task identification: " + codeFurbOrElse
                replyToMessage(message, "TWMAIL_%%_ERROR_INVALID_TASK_ID", teamworkOperator, codeFurbOrElse);
              }
            }

            if (t != null) { // got the task
              // ----------------------------------------  ADD ISSUE
              if ("ISSUE".equals(command)) {
                handleTaskIssue(message, teamworkOperator, t);

                // ----------------------------------------  ADD DOCUMENT
              } else {
                handleTaskDocument(message, teamworkOperator, t);
              }
            }
          }

          // ------------------------------------------------------------------------------------------------- T O D O  handling
        } else if (subjectUC.toUpperCase().startsWith("TODO") || subjectUC.toUpperCase().startsWith("TO-DO")) {
          handled = true;
          handleTodo(message, teamworkOperator);

          // ----------------------------------------  ADD ISSUE COMMENT - Siamo nel caso in cui un
        } else if (subject.matches(".*[Ii]#\\d*#")) {
          handleIssueComment(message, teamworkOperator);
        }
      }

      // --------------------------------------------------------------------------------------------------- VCard handling
      if (!handled) {
        handled = handleVcard(message, teamworkOperator);
      }

      // --------------------------------------------------------------------------------------------------- create task handling
      if (!handled) {
        handled = handleTaskCreationMessage(message, teamworkOperator);
      }

      // --------------------------------------------------------------------------------------------------- Resource INBOX
      if (!handled) {
        handleResourceDocument(message, teamworkOperator);
      }


      // caso di mail mandata da utente esterno
    } else {
      if (subject.matches(".*[Ii]#\\d*#")) {
        handleIssueComment(message, teamworkOperator);
      }
    }

  }


  /**
   * Puo essere un messaggo mandato da il richiedente esterno -> senza operatore o da un utente vero tw
   */
  private void handleIssueComment(MimeMessage message, TeamworkOperator sender) throws MessagingException, IOException, PersistenceException {
    //si recupera la issue
    Matcher matcher = Pattern.compile("[Ii]#(\\d*)#").matcher(message.getSubject());
    if (matcher.find()) {
      String issueId = matcher.group(1);
      Issue issue = Issue.load(issueId + "");
      if (issue != null) {
        boolean hasPermissionToComment = false;
        String senderEmail = ((InternetAddress) message.getFrom()[0]).getAddress();

        //check se il sender ha i permessi di aggiungere commenti sulla issue
        if (sender != null) {
          hasPermissionToComment = issue.hasPermissionFor(sender, TeamworkPermissions.issue_canWrite);
        } else if (JSP.ex(senderEmail)) {
          hasPermissionToComment = senderEmail.equals(issue.getExtRequesterEmail());
        }

        //se ha i permessi per commentare
        if (hasPermissionToComment) {
          //si estrae il body della mail e si ripulisce dalle schifezze. Il metodo estrae
          MailHelper.Email email = new MailHelper.Email(message);

          String messageBody = email.text;
          int posAbove = messageBody.indexOf(MailHelper.REPLY_ABOVE_TXT);

          if (posAbove > 3)
            messageBody = messageBody.substring(0, posAbove - 3);

          //IssueHistory issueHistory = issue.addComment(HtmlSanitizer.getText(substring));  //todo fa casino con gli encoding
          IssueHistory issueHistory = issue.addComment(messageBody);
          if (sender != null) {
            issueHistory.setOwner(sender);
            issueHistory.setCreator(sender.getDisplayName());
            issueHistory.setLastModifier(sender.getDisplayName());

          } else {
            issueHistory.setExtRequesterEmail(senderEmail);
            issueHistory.setCreator(senderEmail);
            issueHistory.setLastModifier(senderEmail);
          }
          issueHistory.store();


          //notify all previous commenters
          Set<TeamworkOperator> ops = new HashSet();
          for (IssueHistory comment : issue.getComments()) {
            if (comment.getOwner() != null && !comment.getOwner().equals(sender))
              ops.add((TeamworkOperator) comment.getOwner());
          }

          //si notificano tutti quelli coinvolti via log tranne chi ha mandato la mail
          for (TeamworkOperator op : ops) {
            issue.generateCommentMessage(null, op, issueHistory, MessagingSystem.Media.LOG);
          }

          //se la issue era stata generata da un esterno e non che ha inviato la mail  gli si notifica la nota
          if (JSP.ex(issue.getExtRequesterEmail()) && !issue.getExtRequesterEmail().equals(senderEmail)) {
            issue.generateCommentMessageForExternalResource(issueHistory);
          }

          //si genera un evento se si può
          issue.riseCommentAddedEvent(sender, issueHistory.getComment());

        }

      }
    }
  }


  private void handleTaskIssue(MimeMessage message, TeamworkOperator teamworkOperator, Task t) throws MessagingException, IOException, StoreException {
    Person person = teamworkOperator.getPerson();
    if (t.hasPermissionFor(teamworkOperator, TeamworkPermissions.issue_canCreate)) {
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
        issue.setArea(teamworkOperator);
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
        issue.setAssignedBy(person);

        //issue.setDescription(email.text);
        issue.setDescription(sanitizedText);

        issue.setCreator(teamworkOperator.getDisplayName());
        issue.setLastModifier(teamworkOperator.getDisplayName());

        for (PersistentFile pf : email.attachments)
          issue.addFile(pf);
        issue.store();

        IssueAction.createEventIssueAddedClosed(issue, true, teamworkOperator, teamworkOperator.getDisplayName());

        //subscribe sender
        Listener l = new Listener(teamworkOperator);
        l.setIdAsNew();
        l.setIdentifiable(issue);
        l.setMedia(StringUtilities.unSplit(teamworkOperator.getPreferredMediaOrDefault(MessagingSystem.Media.EMAIL), ","));
        l.setEventType(Issue.Event.ISSUE_CLOSE + "");
        l.setOneShot(true);
        //l.setValidityEnd();
        l.store();


        Hit.getInstanceAndStore(issue, teamworkOperator, .2);

      } else {
        replyToMessage(message, "TWMAIL_%%_ERROR_ISSUE_NO_DESCR", teamworkOperator, t.getDisplayName());
      }

    } else {
      replyToMessage(message, "TWMAIL_%%_ERROR_ISSUE_NO_PERMISSION", teamworkOperator, person.getDisplayName(), t.getDisplayName());
    }
  }

  private void handleTaskDocument(MimeMessage message, TeamworkOperator teamworkOperator, Task t) throws MessagingException, IOException, StoreException {
    Person person = teamworkOperator.getPerson();
    // ----------------------------------------- TASK ATTACHMENT
    if (t.hasPermissionFor(person.getMyself(), TeamworkPermissions.document_canCreate)) {
      MailHelper.Email email = new MailHelper.Email(message);
      String subject = email.subject;
      String sanitizedText = HtmlSanitizer.sanitize(JSP.ex(email.html) ? email.html : email.text);
      if (email.attachments.size() > 0) {
        for (PersistentFile att : email.attachments) {
          TeamworkDocument document = new TeamworkDocument();
          document.setIdAsNew();
          document.setName((JSP.ex(subject) ? subject + " - " : "") + att.getOriginalFileName());
          document.setAuthor(person.getDisplayName());
          document.setType(TeamworkDocument.IS_UPLOAD);
          document.setVersion("01");
          document.setTask(t);
          document.setFile(att);
          document.setContent(JSP.limWr(sanitizedText, 4000));
          document.store();

          DocumentAction.generateDocumentEvent(document, teamworkOperator);
        }


      } else if (JSP.ex(sanitizedText)) {

        String content = HtmlSanitizer.getText(JSP.ex(email.html) ? email.html : email.text);

        TeamworkDocument document = new TeamworkDocument();
        document.setIdAsNew();
        document.setName(JSP.limWr(content, 50));
        document.setAuthor(person.getDisplayName());
        document.setType(TeamworkDocument.IS_CONTENT);
        document.setVersion("01");
        document.setTask(t);
        document.setContent(JSP.limWr(sanitizedText, 4000));
        document.store();

        DocumentAction.generateDocumentEvent(document, teamworkOperator);
      }


    } else {
      replyToMessage(message, "TWMAIL_%%_ERROR_TASK_NO_PERMISSION", teamworkOperator, person.getDisplayName(), t.getDisplayName());
    }
  }


  private void handleResourceDocument(MimeMessage message, TeamworkOperator teamworkOperator) throws MessagingException, IOException, StoreException {
    Person person = teamworkOperator.getPerson();
    // ----------------------------------------- RESOURCE ATTACHMENT
    // una risorsa può sempre allegare doc su se stesso
    //if (person.hasPermissionFor(teamworkOperator, TeamworkPermissions.document_canCreate)) {
    MailHelper.Email email = new MailHelper.Email(message);
    String subject = email.subject;
    String sanitizedText = HtmlSanitizer.sanitize(JSP.ex(email.html) ? email.html : email.text);
    if (email.attachments.size() > 0) {
      for (PersistentFile att : email.attachments) {
        TeamworkDocument document = new TeamworkDocument();
        document.setIdAsNew();
        document.setName((JSP.ex(subject) ? subject + " - " : "") + att.getOriginalFileName());
        document.setAuthor(person.getDisplayName());
        document.setType(TeamworkDocument.IS_UPLOAD);
        document.setVersion("01");
        document.setResource(person);
        document.setFile(att);
        document.setContent(JSP.limWr(sanitizedText, 4000));
        document.setKind("inbox"); // serve per riconoscere che sono stati scaricati sull'utente
        document.store();
      }


    } else if (JSP.ex(sanitizedText)) {
      TeamworkDocument document = new TeamworkDocument();
      document.setIdAsNew();
      document.setName(JSP.w(subject));
      document.setAuthor(person.getDisplayName());
      document.setType(TeamworkDocument.IS_CONTENT);
      document.setVersion("01");
      document.setResource(person);
      document.setContent(JSP.limWr(sanitizedText, 4000));
      document.setKind("inbox"); // serve per riconoscere che sono stati scaricati sull'utente
      document.store();
    }


    //} else {
    //  replyToMessage(message, "TWMAIL_%%_ERROR_TASK_NO_PERMISSION", teamworkOperator, person.getDisplayName(), t.getDisplayName());
  }


  private void handleTodo(MimeMessage message, TeamworkOperator teamworkOperator) throws MessagingException, IOException, StoreException {
    Person person = teamworkOperator.getPerson();
    MailHelper.Email email = new MailHelper.Email(message);
    String sanitizedText = HtmlSanitizer.getText(email.text);
    if (JSP.ex(sanitizedText)) {
      Issue issue = new Issue();
      issue.setIdAsNew();
      issue.setArea(teamworkOperator);
      issue.setStatusOpen();
      //message.getFlags().contains(Flags.)

      boolean isPrio = ((message.getHeader("X-Priority", ",") + "").toUpperCase().contains("HIGH")) ||
              ((message.getHeader("X-MSMail-Priority", ",") + "").toUpperCase().contains("HIGH")) ||
              ((message.getHeader("Importance", ",") + "").toUpperCase().contains("HIGH"));
      if (isPrio)
        issue.setGravity(Issue.GRAVITY_BLOCK);
      else
        issue.setGravity(Issue.GRAVITY_MEDIUM);

      issue.setDateSignalled(new Date());
      issue.setOwner(teamworkOperator);
      issue.setAssignedBy(person);
      issue.setAssignedTo(person);
      issue.setDescription(sanitizedText);

      issue.setCreator(teamworkOperator.getDisplayName());
      issue.setLastModifier(teamworkOperator.getDisplayName());

      for (PersistentFile pf : email.attachments)
        issue.addFile(pf);

      issue.store();
    } else {
      replyToMessage(message, "TWMAIL_ERROR_TODO_NO_DESCR", teamworkOperator, null);

    }
  }

  private boolean handleVcard(MimeMessage message, TeamworkOperator teamworkOperator) throws MessagingException, IOException, PersistenceException {
    boolean ret = false;
    ContentType ct = MailMessageUtilities.getContentType(message.getContentType());
    if ("multipart".equalsIgnoreCase(ct.getPrimaryType())) {

      Multipart multipart = (Multipart) message.getContent();
      int partCount = multipart.getCount();

      for (int j = 0; j < partCount; j++) {
        BodyPart part = multipart.getBodyPart(j);
        ContentType ctPart = MailMessageUtilities.getContentType(part.getContentType());

        if ("text".equalsIgnoreCase(ctPart.getPrimaryType()) && "x-vcard".equalsIgnoreCase(ctPart.getSubType())) {
          ret = true;
          InputStream inputStream = part.getInputStream();
          vCardUnmarshaller vcu = new vCardUnmarshaller();
          Contact[] cts = vcu.unmarshallContacts(inputStream);
          for (int i = 0; i < cts.length; i++) {
            Contact contact = cts[i];
            PersonalIdentity pid = contact.getPersonalIdentity();
            String emailC = null;
            String phone = null;
            String mobile = null;
            String fax = null;
            Communications communications = contact.getCommunications();
            if (communications != null) {
              emailC = communications.getPreferredEmailAddress() == null ? null : communications.getPreferredEmailAddress().getAddress();
              PhoneNumber phoneNumber = communications.getPreferredPhoneNumber();
              if (phoneNumber != null) {
                if (phoneNumber.isCellular())
                  mobile = phoneNumber.getNumber();
                else if (phoneNumber.isFax())
                  fax = phoneNumber.getNumber();
                else if (!(phoneNumber.isPager() || phoneNumber.isPCS() || phoneNumber.isISDN() || phoneNumber.isMessaging() || phoneNumber.isMODEM()))
                  phone = phoneNumber.getNumber();
              }

              //loop al devices
              Iterator numbers = communications.getPhoneNumbers();
              while (numbers.hasNext()) {
                PhoneNumber pn = (PhoneNumber) numbers.next();
                if (mobile == null && pn.isCellular())
                  mobile = pn.getNumber();
                else if (fax == null && pn.isFax())
                  fax = pn.getNumber();

                else if (phone == null && !(pn.isPager() || pn.isPCS() || pn.isISDN() || pn.isMessaging() || pn.isMODEM()))
                  phone = pn.getNumber();

              }
            }

            String street = null;
            String city = null;
            String country = null;
            String state = null;
            String zip = null;

            if (contact.getAddressCount() > 0) {
              net.wimpi.pim.contact.model.Address address = (net.wimpi.pim.contact.model.Address) contact.getAddresses().next();
              street = address.getStreet();
              city = address.getCity();
              country = address.getCountry();
              state = address.getRegion();
              zip = address.getPostalCode();
            }

            if (pid != null) {
              try {
                ResourcesImportControllerAction.createPerson(pid.getFirstname(), pid.getLastname(), emailC, phone, mobile, street, city, state, zip, country, null, null, teamworkOperator);
              } catch (org.jblooming.security.SecurityException e) {
                replyToMessage(message, "TWMAIL_%%_ERROR_RESOURCE_NO_PERMISSION", teamworkOperator, teamworkOperator.getPerson().getDisplayName());
              }
            }
          }
        }
      }
    }
    return ret;
  }


  private TeamworkOperator getOperatorFromMessage(Message message) throws MessagingException, PersistenceException {
    Person p = null;
    TeamworkOperator ret = null;

    String sender = ((InternetAddress) message.getFrom()[0]).getAddress();

    //if the message comes from teamwork's configured pop3 email do not reply in order to avoid infinite loop of e-mails
    if (JSP.ex(sender) && !sender.equals(ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_FROM))) {
      List<Person> r = ResourceBricks.getPersonByEmail(sender);
      if (r.size() > 0) {
        p = r.get(0);
      }

      if (p != null) {
        ret = p.getMyself();
      }
    } else {
      Tracer.emailLogger.error("Error: sender is Twproject itself: " + sender + ". Message ignored: " + message.getSubject());
    }
    return ret;
  }


  private static void replyToMessage(Message message, String errorMessageI18NLabel, Operator operator, String... parameters) throws MessagingException {
    String mess = I18n.getLabel(errorMessageI18NLabel, operator.getLanguage());
    mess = StringUtilities.replaceParameters(mess, parameters);
    MailHelper.replyToMessage(message, mess);
    Tracer.emailLogger.error("reply to message: " + mess);
  }


  private String extractValue(String row, String label) {
    String value = row.substring(label.length() + 1);
    value = value.replace("^[^a-zA-A0-9]*", "");
    return value;
  }


  private void handleCalendar(Message message) throws IOException, ParserException, MessagingException, PersistenceException, ActionException {

    ContentType ct = MailMessageUtilities.getContentType(message.getContentType());

    if ("text".equalsIgnoreCase(ct.getPrimaryType()) && "calendar".equalsIgnoreCase(ct.getSubType())) {
      InputStream inputStream = (InputStream) message.getContent();
      handleCalendarMessage(inputStream, message);
    } else if ("multipart".equalsIgnoreCase(ct.getPrimaryType())) {

      for (Part part : MailHelper.getMessageParts(message)) {
        ContentType ctPart = MailMessageUtilities.getContentType(part.getContentType());
        if ("text".equalsIgnoreCase(ctPart.getPrimaryType()) && "calendar".equalsIgnoreCase(ctPart.getSubType())) {
          InputStream inputStream = (InputStream) part.getContent();
          handleCalendarMessage(inputStream, message);
        }
      }
    }
  }

  private void handleCalendarMessage(InputStream inputStream, Message message) throws IOException, ParserException, MessagingException, PersistenceException, ActionException {
    CalendarBuilder cb = new CalendarBuilder();
    net.fortuna.ical4j.model.Calendar iCalendar = cb.build(inputStream);

    Tracer.emailLogger.debug("\ncontent: " + iCalendar.toString());

    String email = ((InternetAddress) message.getFrom()[0]).getAddress();
    List<Person> r = ResourceBricks.getPersonByEmail(email);
    Person sender = null;
    if (r.size() == 1) {
      sender = r.get(0);
    }
    for (Iterator it = iCalendar.getComponents().iterator(); it.hasNext(); ) {
      Component component = (Component) it.next();
      if (component instanceof VEvent) {
        VEvent ve = (VEvent) component;
        IcalUtilities.manageIncomingEvent(iCalendar, ve, sender, message);
      }
    }
  }


  /**
   * Email subject  [NEW TASK | NEW PROJECT | NEW_TASK | NEW_PROJECT | NEW] project_name
   * <p/>
   * enmail body could have multiple lines staring with:
   * <p/>
   * CODE task_code
   * START a_date_in_the_user_format
   * END a_date_in_the_user_format
   * DURATION duration_in_days
   * <p/>
   * n attachments: will be laoded as document
   * large desciption: first 2000 chars in project description, if larger also as textual document
   */
  private boolean handleTaskCreationMessage(MimeMessage message, TeamworkOperator teamworkOperator) throws IOException, MessagingException, PersistenceException {
    boolean ret = false;
    Person person = teamworkOperator.getPerson();

    String[] ss = new String[]{"NEW TASK", "NEW PROJECT", "NEW_TASK", "NEW_PROJECT", "NEW"};

    String sub = message.getSubject();
    String subUp = sub.toUpperCase();
    for (String s : ss) {
      if (subUp.startsWith(s)) {

        if (teamworkOperator.hasPermissionFor(teamworkOperator, TeamworkPermissions.project_canCreate)) {


          RestState restState = new RestState(teamworkOperator);
          try {
            String taskName = extractValue(sub, s);

            TaskAction ta = new TaskAction(restState);
            ta.cmdAdd();
            restState.addClientEntry("TASK_NAME", taskName);

            MailHelper.Email email = new MailHelper.Email(message);
            String sanitizedText = HtmlSanitizer.getText(email.text);
            if (JSP.ex(sanitizedText)) {

              //si splitta il testo per righe
              String[] rows = StringUtilities.splitToArray(sanitizedText, "\n");

              //si controlla se le righe iniziano con qualcosa di sensato
              for (String row : rows) {
                String ROW = row.toUpperCase().trim();

                //code
                if (ROW.startsWith("CODE")) {
                  String v = extractValue(row, "CODE");
                  if (JSP.ex(v))
                    restState.addClientEntry("TASK_CODE", v);

                  //start
                } else if (ROW.startsWith("START")) {
                  String v = extractValue(row, "START");
                  if (JSP.ex(v))
                    restState.addClientEntry("START", v);

                  //end
                } else if (ROW.startsWith("END")) {
                  String v = extractValue(row, "END");
                  if (JSP.ex(v))
                    restState.addClientEntry("END", v);

                  //duration
                } else if (ROW.startsWith("DURATION")) {
                  String v = extractValue(row, "DURATION");
                  if (JSP.ex(v))
                    restState.addClientEntry("TASK_DURATION", v);
                } else if (ROW.startsWith("DAYS")) {
                  String v = extractValue(row, "DAYS");
                  if (JSP.ex(v))
                    restState.addClientEntry("TASK_DURATION", v);

                }
              }

              //in ogni caso si mette il messaggio
              restState.addClientEntry("DESCRIPTION", JSP.limWr(sanitizedText, 2000));

            }

            //salva
            ta.cmdSave();

            //ho il task
            Task task = (Task) restState.getMainObject();

            //creo assegnazione a chi ha inviato la mail
            AssignmentAction aa = new AssignmentAction(restState);
            restState.addClientEntry("TASK_ID", task.getId());
            restState.addClientEntry("ASSIGNEE", person.getId());
            restState.addClientEntry("ASSIG_ROLE", TaskBricks.getProjectManagerRole(task.getArea()).getId());
            aa.cmdAdd();
            aa.cmdSave();

            //se il messaggio è grande si allega come doc
            if (JSP.ex(sanitizedText) && sanitizedText.length() > 1980) {
              TeamworkDocument document = new TeamworkDocument();
              document.setIdAsNew();
              document.setName(JSP.limWr(sanitizedText, 50));
              document.setAuthor(person.getDisplayName());
              document.setType(TeamworkDocument.IS_CONTENT);
              document.setVersion("01");
              document.setTask(task);
              document.setContent(JSP.limWr(sanitizedText, 4000));
              document.store();

              DocumentAction.generateDocumentEvent(document, teamworkOperator);
            }

            //gestione allegati
            for (PersistentFile att : email.attachments) {
              TeamworkDocument document = new TeamworkDocument();
              document.setIdAsNew();
              document.setName(att.getName());
              document.setAuthor(person.getDisplayName());
              document.setType(TeamworkDocument.IS_UPLOAD);
              document.setVersion("01");
              document.setTask(task);
              document.setFile(att);
              document.setContent(JSP.limWr(sanitizedText, 4000));
              document.store();

              DocumentAction.generateDocumentEvent(document, teamworkOperator);
            }

            ret = true;
            break;

          } catch (Throwable t) {
            String cee = restState.getCEErrors("<br>"); //si prendono gli eventuali errori sulle ce
            replyToMessage(message, "TWMAIL_ERROR_CREATING_TASK_%%", teamworkOperator, t.getMessage() + "<br>" + cee);
            throw new PlatformRuntimeException(t);
          }

        } else {
          replyToMessage(message, "TWMAIL_ERROR_%%_NO_PERMISSION_CREATE_TASK", teamworkOperator, person.getDisplayName());
        }
      }
    }

    return ret;
  }

}


