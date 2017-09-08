package org.jblooming.messaging;

import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.scheduler.PlatformExecutionService;
import org.jblooming.system.SystemConstants;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.utilities.file.FileUtilities;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.settings.ApplicationState;

import javax.mail.Message;
import javax.mail.*;
import javax.mail.internet.*;
import java.io.*;
import java.util.*;
import java.util.concurrent.Future;

public class MailHelper {

  static MailDateFormat mailDateFormat = new MailDateFormat();

  protected static final String RFC_2822_DATE = "Date";

  public static final String REPLY_ABOVE_TXT = "----TWPROJECT NOTIFICATION----";
  public static final String REPLY_ABOVE_HTML = "<span style='color:#FFF;font-size:5px'>"+REPLY_ABOVE_TXT+"</span>";

  //------------------------------------------------  PLAIN TEXT EMAIL -----------------------------------------------------------------------


  public static Future sendMailWithHeader(final Set<InternetAddress> tos, final String subject, final String header, final String body) { //final Map<String,String> nameAndMail

    final InternetAddress from = MailHelper.getSystemInternetAddress();

    return PlatformExecutionService.executorService.submit(
      new Thread() {
        public void run() {
          try {
            Session session = getSessionForSending();
            if (session!=null) {
              MimeMessage message;
              message = new MimeMessage(session);

              // first set the body as text/plain
              message.setText(body, "UTF-8");
              // then force the content-type
              message.addHeader("Content-Type", header);
              message.setHeader("Precedence", "bulk");

              message.setFrom(from);

              for (InternetAddress to : tos) {
                message.addRecipient(MimeMessage.RecipientType.TO, to);
              }

              message.setSubject(subject, "UTF-8");

              String date = mailHeaderDate();
              if (message.getHeader(RFC_2822_DATE) == null) {
                message.addHeader(RFC_2822_DATE, date);
              }
              send(message, session);
            } else {
              Tracer.platformLogger.warn("MailHelper.sendMailWithHeader cannot create session");
            }
          } catch (Exception e) {
            //Tracer.platformLogger.error(e);
            throw new PlatformRuntimeException(e);
          }
        }
      });
  }


  //------------------------------------------------  HTML EMAIL -----------------------------------------------------------------------


  public static void sendHtmlMail(String to, String subject, String body) throws ApplicationException {
    if (JSP.ex(to)) {
      sendHtmlMail(null, mailTos(to), subject, body);
    }
  }


  public static Future sendHtmlMailInSeparateThread(Set<String> to, String subject, String body) {
    return sendHtmlMailInSeparateThread(null, to, subject, body);
  }

  public static Future sendHtmlMailInSeparateThread(String from,String to, String subject, String body) {
    return sendHtmlMailInSeparateThread(from,MailHelper.mailTos(to),subject,body);
  }

  public static Future sendHtmlMailInSeparateThread(final String fromMail, final Set<String> to, final String subject, final String body) {
    return PlatformExecutionService.executorService.submit(
            new Thread() {
              public void run() {
                try {
                  sendHtmlMail(fromMail, to, subject, body);
                } catch (ApplicationException e) {
                  Tracer.platformLogger.error("MailHelper.sendHtmlMailInSeparateThread", e);
                }
              }
            });
  }


  public static void sendHtmlMail(String fromName, Set<String> to, String subject, String body) throws ApplicationException {
    Session mailSession = getSessionForSending();
    if (mailSession != null) {

      try {

        MimeMessage message = new MimeMessage(mailSession);

        message.setHeader("Precedence", "bulk");

        InternetAddress systemMail = getSystemInternetAddress();
        if (JSP.ex(fromName))
          systemMail.setPersonal(fromName, "UTF-8");


        message.setFrom(systemMail);
        for (String rec : to) {
          message.addRecipient(MimeMessage.RecipientType.TO, new InternetAddress(rec));
        }
        message.setSubject(subject, "UTF-8");
        message.setContent(wrapBody(body), "text/html; charset=\"UTF-8\"");
        String date = mailHeaderDate();
        if (message.getHeader(RFC_2822_DATE) == null) {
          message.addHeader(RFC_2822_DATE, date);
        }
        message.addHeader("X-Mailer", "JBlooming Platform");

        send(message, mailSession);

      } catch (Exception e) {
        throw new ApplicationException(e);
      }
    } else {
      Tracer.platformLogger.warn("MailHelper.sendHtmlMail cannot create session");
    }
  }


  //------------------------------------------------  TOOLS  -----------------------------------------------------------------------


  /**
   * NON CAMBIARE NIENTE SENZA CHIEDERE A ROBICCH. ---TWPROJECT NOTIFICATION--- NON si deve vedere
   */
  private static String wrapBody(String body){
    return REPLY_ABOVE_HTML +
      "<div style='background-color:#e9e9e9; padding:30px 30px; margin-bottom:10px'>" +
      "<div style='background-color:#fff; color:#333;padding:20px; font-size: 120%; line-height: 140%; border-top: 6px solid #2F97C6'>" +
      JSP.w(body) +
      "</div></div>";

  }

  public static String mailHeaderDate() {
    return mailDateFormat.format(new Date());
  }

  public static Session getSessionForSending() {

    Properties mailProps = new Properties();

    boolean useAuth = Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_USE_AUTHENTICATED));
    String mailHost = ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_SMTP);

    if (!JSP.ex(mailHost)) {  // se non c'è il server si ritorn un null
      Tracer.platformLogger.error("Tried to send mail without mail host configured in settings:" + SystemConstants.FLD_MAIL_SMTP);
      return null;
    }

    String protocol = "smtp";
    String protocolParam = ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_PROTOCOL);
    if (JSP.ex(protocolParam))
      protocol = protocolParam;

      mailProps.put("mail." + protocol + ".host", mailHost);

      int port = 25;
      String portS = ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_SMTP_PORT);
      try {
        port = Integer.parseInt(portS);
      } catch (NumberFormatException e) {
      }

      //Pietro: incredible, unbelievable bug!!!!
      //http://forums.sun.com/thread.jspa?threadID=778568
      //
      // this was: mailProps.put("mail." + protocol + ".port", port);
      //
      //  "The problem with my code was that smtpPort was int, and autoboxing made it Integer and added to the props without any compile time problems.
      //   But props.getProperty("mail.smtp.port") returns null unless the value is String. That caused JavaMail to use the default port."
      //  this was mailProps.put("mail." + protocol + ".port", port); which for mailProps.getProperty("mail." + protocol + ".port") return NULL!!!!!!!!!!
      mailProps.setProperty("mail." + protocol + ".port", port + "");

      boolean useStartTLS="yes".equalsIgnoreCase(ApplicationState.getApplicationSetting("MAIL_SMTP_USE_TLS"));
      if (useStartTLS) {
        mailProps.setProperty("mail." + protocol + ".starttls.enable", "true");
      }

      String heloHost = ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_HELO_HOST);
      if (!JSP.ex(heloHost))
        heloHost = mailHost;
      mailProps.setProperty("mail." + protocol + ".localhost", heloHost);
      if (useAuth) {
        mailProps.setProperty("mail." + protocol + ".auth", "true");
      }

    String useAuthenticatedMail = ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_USE_AUTHENTICATED);
    MailAutenticator autenticator = null;
    if (Fields.TRUE.equalsIgnoreCase(useAuthenticatedMail)) {
      String smtpUser = ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_USER);
      String smtpPwd = ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_PWD);
      autenticator = new MailAutenticator(smtpUser, smtpPwd);
    }

    Session mailSession= Session.getInstance(mailProps, autenticator);
    if (ApplicationState.platformConfiguration.development){
      mailSession.setDebug(true);
      mailSession.setDebugOut(System.err);
    }

    return mailSession;
  }

  public static Session getSessionForReceiving(Properties mailProps) {
    Session mailSession = Session.getInstance(mailProps);
    if (ApplicationState.platformConfiguration.development){
      mailSession.setDebug(true);
      mailSession.setDebugOut(System.err);
    }
    return mailSession;
  }


  public static void _testSend(Message message, Session session) throws MessagingException {
    send(message, session);
  }


  private static void send(Message message, Session session) throws MessagingException {

    String protocol = "smtp";
    String protocolParam = ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_PROTOCOL);
    if (JSP.ex(protocolParam))
      protocol = protocolParam;

    if (session!=null) {
      Transport t = session.getTransport(protocol);

      boolean useAuth = Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_USE_AUTHENTICATED));
      String mailHost = ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_SMTP);

      if (useAuth) {
        String smtpUser = ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_USER);
        String smtpPwd = ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_PWD);
        t.connect(mailHost, smtpUser, smtpPwd);
        t.sendMessage(message, message.getAllRecipients());
      } else
        t.send(message);

      t.close();
    } else {
      Tracer.platformLogger.warn("MailHelper.send cannot create session");
    }

  }


  public static Store getPop3AndConnect() throws MessagingException {
    String port = ApplicationState.getApplicationSetting(SystemConstants.FLD_POP3_PORT);
    int FLD_POP3_PORT = JSP.ex(port) ? Integer.parseInt(port) : -1;

    String protocol = "pop3";
    String protocolParam = ApplicationState.getApplicationSetting(SystemConstants.FLD_EMAIL_DOWNLOAD_PROTOCOL);
    if (JSP.ex(protocolParam))
      protocol = protocolParam;

    return getPop3AndConnect(ApplicationState.getApplicationSetting(SystemConstants.FLD_POP3_HOST), ApplicationState.getApplicationSetting(SystemConstants.FLD_POP3_USER), ApplicationState.getApplicationSetting(SystemConstants.FLD_POP3_PSW), FLD_POP3_PORT, protocol );
  }


  public static Store getPop3AndConnect(String host, String user, String password, int port, String protocol) throws MessagingException {
    Properties mailProps = new Properties();

    boolean useStartTLS="yes".equalsIgnoreCase(ApplicationState.getApplicationSetting("MAIL_POP3_USE_TLS"));
    if (useStartTLS) {
      mailProps.setProperty("mail." + protocol + ".starttls.enable", "true");
    }

    Session session = getSessionForReceiving(mailProps);
    Store store=null;
    if (JSP.ex(host, user)) {
      store = session.getStore(protocol);
      store.connect(host, port, user, password);
    } else {
      Tracer.platformLogger.error("MailHelper.getPop3AndConnect. Invalid parameters host:"+host+"  user:+"+user);
    }
    return store;
  }


  public static InternetAddress getSystemInternetAddress() {
    InternetAddress fromIA = null;
    try {
      String twEmail = ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_FROM);
      if (!JSP.ex(twEmail)){
        twEmail = "configure_your_server_email_address@example.com";
        Tracer.platformLogger.warn(SystemConstants.FLD_MAIL_FROM + " is not configured in global settings");
      }

      fromIA = new InternetAddress(twEmail);
      if (!JSP.ex(fromIA.getPersonal()))
        fromIA.setPersonal("Twproject Mail Service", "UTF-8");

    } catch (Exception e) {
      throw new PlatformRuntimeException(e);
    }


    return fromIA;
  }

  public static List<String> mailTosAsList(String manyButNotAllWaysSeparatedText) {

    List<String> tos = new ArrayList<String>();
    manyButNotAllWaysSeparatedText = normalizeEmailSep(manyButNotAllWaysSeparatedText);

    List<String> tosTmp = StringUtilities.splitToList(manyButNotAllWaysSeparatedText, ",");
    if (JSP.ex(tosTmp))
      for (String to : tosTmp) {
        if (JSP.ex(to) && to.indexOf("@") > -1)
          tos.add(to);
      }
    return tos;
  }


  public static Set<String> mailTos(String manyButNotAllWaysSeparatedText) {

    Set<String> tos = new HashSet<String>();
    manyButNotAllWaysSeparatedText = normalizeEmailSep(manyButNotAllWaysSeparatedText);

    Set<String> tosTmp = StringUtilities.splitToSet(manyButNotAllWaysSeparatedText, ",");
    if (JSP.ex(tosTmp))
      for (String to : tosTmp) {
        if (JSP.ex(to) && to.indexOf("@") > -1)
          tos.add(to);
      }
    return tos;
  }

  private static String normalizeEmailSep(String manyButNotAllWaysSeparatedText) {
    manyButNotAllWaysSeparatedText = StringUtilities.replaceAllNoRegex(manyButNotAllWaysSeparatedText, ";", ",");
    manyButNotAllWaysSeparatedText = StringUtilities.replaceAllNoRegex(manyButNotAllWaysSeparatedText, ":", ",");
    manyButNotAllWaysSeparatedText = StringUtilities.replaceAllNoRegex(manyButNotAllWaysSeparatedText, "\r", "");
    manyButNotAllWaysSeparatedText = StringUtilities.replaceAllNoRegex(manyButNotAllWaysSeparatedText, "\n", ",");
    manyButNotAllWaysSeparatedText = StringUtilities.replaceAllNoRegex(manyButNotAllWaysSeparatedText, "\t", ",");
    manyButNotAllWaysSeparatedText = StringUtilities.replaceAllNoRegex(manyButNotAllWaysSeparatedText, "<br>", ",");
    return manyButNotAllWaysSeparatedText;
  }


  public static void replyToMessage(Message message, String newMessageText) throws MessagingException {
    replyToMessage(message, newMessageText, false);
  }

  public static void replyToMessage(Message message, String newMessageText, boolean includeOriginalText) throws MessagingException {
    Message reply = message.reply(false);

    String replyText = "";
    if (includeOriginalText) {
      try {
        replyText = "\n\n----------------------------------------------\n\n" +
                (JSP.ex(message.getSubject()) ? ">" + JSP.w(message.getSubject()) + "\n" : "") +
                (message.getContent() + "").replaceAll("(?m)^", "> ");
      } catch (IOException io) {
      }
    }
    reply.setText(newMessageText + replyText);
    reply.setFrom(getSystemInternetAddress());

    if (!Fields.TRUE.equals(ApplicationState.applicationParameters.get("TEAMWORK_ASP_INSTANCE")))  // in case of demo don not notity in order to avoid infinite e-mail loops
      send(reply, getSessionForSending());
  }


  public static String mailToUrl(String fromEmail, String toEmail, String subject, String body) {
    String mailTo = "";

    if (JSP.ex(fromEmail, toEmail)) {

      toEmail = JSP.urlEncode(toEmail);
      subject = JSP.urlEncode(subject);
      body = JSP.urlEncode(body);
      fromEmail = fromEmail.toLowerCase().trim();

      if (fromEmail.endsWith("@gmail.com")) {
        mailTo = "http://mail.google.com/mail/?view=cm&fs=1&to=" + toEmail + (JSP.ex(subject) ? "&su=" + subject : "") + (JSP.ex(body) ? "&body=" + body : "") + "&ui=1";
      } else if (fromEmail.endsWith("@hotmail.com")) {
        mailTo = "http://www.hotmail.msn.com/secure/start?action=compose&to=" + toEmail + (JSP.ex(subject) ? "&subject=" + subject : "") + (JSP.ex(body) ? "&body=" + body : "");
      } else if (fromEmail.endsWith("@yahoo.com")) {
        mailTo = "http://compose.mail.yahoo.com/?To=" + toEmail + (JSP.ex(subject) ? "&Subject=" + subject : "") + (JSP.ex(body) ? "&body=" + body : "");
      } else if (fromEmail.endsWith("@aol.com")) {
        mailTo = "http://webmail.aol.com/25045/aol/en-us/Mail/compose-message.aspx?to=" + toEmail + (JSP.ex(subject) ? "&Subject=" + subject : "") + (JSP.ex(body) ? "&body=" + body : "");
      } else {
        mailTo = "mailto:" + toEmail + (JSP.ex(subject) || JSP.ex(body) ? "?" : "") + (JSP.ex(subject) ? "subject=" + subject + (JSP.ex(body) ? "&" : "") : "") + (JSP.ex(body) ? "body=" + body : "");
      }
    }
    return mailTo;
  }

  public static List<Part> getMessageParts(Message message) throws MessagingException, IOException {
    List<Part> ret = new ArrayList();

    if (message.isMimeType("multipart/*")) {

      Multipart multipart = (Multipart) message.getContent();

      buildPartInfoList(ret, multipart);
    }
    return ret;
  }

  private static void buildPartInfoList(List<Part> partlist, Multipart mp) throws MessagingException, IOException {

    for (int i = 0; i < mp.getCount(); i++) {
      //Get part
      Part apart = mp.getBodyPart(i);
      //handle single & multiparts
      if (apart.isMimeType("multipart/*")) {
        //recurse
        buildPartInfoList(partlist, (Multipart) apart.getContent());
      } else {
        //append the part
        partlist.add(apart);
      }
    }
  }


  public static PersistentFile extractPeristentFileFromEmailPart(Part part) throws MessagingException, StoreException, IOException {
    String fileNameEncoded = JSP.ex(part.getFileName()) ? MimeUtility.decodeText(part.getFileName()) : "attachment";
    InputStream inputStream = part.getInputStream();
    PersistentFile persistentFile=PersistentFile.createPersistentFileFromStream(PersistentFile.DEFAULT_STORAGE_TYPE, inputStream, fileNameEncoded, "", null);
    return persistentFile;
  }


  public static class Email {

    public String subject;
    public String text;
    public String html;
    public List<PersistentFile> attachments = new ArrayList();


    public Email(Message message) throws MessagingException, IOException, StoreException {
      ContentType ct = MailMessageUtilities.getContentType(message.getContentType());
      subject = message.getSubject();
      StringBuffer content = new StringBuffer();
      StringBuffer htmlContent = new StringBuffer();

      if ("multipart".equalsIgnoreCase(ct.getPrimaryType())) {

        for (Part part : MailHelper.getMessageParts(message)) {

          ContentType ctPart = MailMessageUtilities.getContentType(part.getContentType());

          String disposition = part.getDisposition();
          if (Part.ATTACHMENT.equals(disposition) || Part.INLINE.equals(disposition) && "image".equalsIgnoreCase(ctPart.getPrimaryType())) {

            PersistentFile attachedFile = MailHelper.extractPeristentFileFromEmailPart(part);
            attachments.add(attachedFile);

          } else if ("text".equalsIgnoreCase(ctPart.getPrimaryType()) && "plain".equalsIgnoreCase(ctPart.getSubType())) {
            MailMessageUtilities.decodeTextPlain(content, part);
          } else if ("text".equalsIgnoreCase(ctPart.getPrimaryType()) && "html".equalsIgnoreCase(ctPart.getSubType())) {
            MailMessageUtilities.decodeTextPlain(htmlContent, part);
          } else if ("multipart".equalsIgnoreCase(ctPart.getPrimaryType()) && "alternative".equalsIgnoreCase(ctPart.getSubType())) {
            MailMessageUtilities.decodeContent(part, content, null, null);
          }
        }
      } else {
        if (message.getContent() instanceof String) {
          content.append(message.getContent());
          Tracer.emailLogger.debug("\ncontent: " + text);
        } else {
          InputStream inputStream = (InputStream) message.getContent();
          FileUtilities.readInputStream(inputStream, content);
        }
      }

      text = content.toString();
      html = htmlContent.toString();
    }
  }


}