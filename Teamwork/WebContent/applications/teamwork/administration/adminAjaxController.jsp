<%@ page import="com.twproject.agenda.Event, com.twproject.operator.TeamworkOperator, com.twproject.resource.Person, com.twproject.resource.Resource, com.twproject.security.TeamworkPermissions, com.twproject.task.*, com.twproject.task.businessLogic.IssueAction, net.sf.json.JSONArray, net.sf.json.JSONObject, org.jblooming.agenda.Period, org.jblooming.messaging.MessagingSystem, org.jblooming.messaging.SomethingHappened, org.jblooming.ontology.Pair, org.jblooming.ontology.PersistentFile, org.jblooming.ontology.businessLogic.DeleteHelper, org.jblooming.oql.OqlQuery, org.jblooming.persistence.PersistenceHome, org.jblooming.persistence.exceptions.PersistenceException, org.jblooming.persistence.hibernate.PersistenceContext, org.jblooming.tracer.Tracer, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.utilities.StringUtilities, org.jblooming.waf.JSONHelper, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.exceptions.ActionException, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.input.Uploader, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.*, java.util.*, org.jblooming.waf.settings.businessLogic.I18nAction, org.jblooming.waf.settings.I18nEntryPersistent, org.jblooming.scheduler.JobLogData, com.twproject.security.LicenseUpdater, org.jblooming.security.License, org.jblooming.system.SystemConstants, org.jblooming.messaging.MailHelper, java.io.ByteArrayOutputStream, java.io.PrintStream, javax.mail.*, org.jblooming.utilities.HashTable, javax.mail.internet.MimeMessage, javax.mail.internet.InternetAddress" %>
<%
  JSONHelper jsonHelper = new JSONHelper();
  JSONObject json = jsonHelper.json;
  PageState pageState = PageState.getCurrentPageState(request);
  try {
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    logged.testIsAdministrator();

    // ------------------------------------  SAVE CUSTOMIZATIONE field, excel, etc ---------------------------------------------
    if ("SVCST".equals(pageState.command)) {
      pageState.initializeEntries("table");
      String cfs_s = pageState.getEntry("cfs").stringValueNullIfEmpty();
      if (JSP.ex(cfs_s)) {

        JSONObject cfs = JSONObject.fromObject(cfs_s);
        for (Object o : cfs.keySet()) {
          String code = o + "";
          String label = cfs.getString(code);
          //di default se sono vuote si settano a "no"
          label = JSP.ex(label) ? label : "no";

          String lang = "EN";
          String appl = "Teamwork";

          if (JSP.ex(code)) {
            I18nAction i18Act = new I18nAction();
            i18Act.saveEntryInMemory(code, appl, lang, label);

            //se non siamo in sviluppo sono sul db e vanno rimosse
            if (!ApplicationState.platformConfiguration.development) {
              // remove all code
              OqlQuery oql = new OqlQuery("delete from " + I18nEntryPersistent.class.getName() + " where code like :code");
              oql.getQuery().setString("code", code);
              oql.getQuery().executeUpdate();

              if (JSP.ex(label)) {
                I18nEntryPersistent ent = new I18nEntryPersistent();
                ent.setCode(code);
                ent.setApplication(appl);
                ent.setLanguage(lang);
                ent.setValue(label);
                ent.store();
              } else {
                I18n.removeEntry(code, appl, lang);
              }
            }

          }

        }

      }

      // save new license online
    } else if ("SVNEWLIC".equals(pageState.command)) {
      String nl = pageState.getEntry("nl").stringValueNullIfEmpty();
      //decrypt
      String nld = StringUtilities.decryptBase64(nl, LicenseUpdater.key);
      License license = License.fromString(nld);
      if (license.loadOk) {
        license.storeLicense();
      } else {
        throw new Exception("Invalid license data.");
      }
      pageState.initializeEntries("div");

      // ----------------------------------- TEST POP3 -----------------------------------------------
    } else if ("TSTPOP3".equals(pageState.command)) {

      Store store = null;
      Folder folder = null;

      String pop3Host = pageState.getEntry(SystemConstants.FLD_POP3_HOST).stringValue();
      String pop3User = pageState.getEntry(SystemConstants.FLD_POP3_USER).stringValue();
      String pop3Psw = pageState.getEntry(SystemConstants.FLD_POP3_PSW).stringValue();
      int pop3Port = pageState.getEntry(SystemConstants.FLD_POP3_PORT).intValueNoErrorCodeNoExc();
      pop3Port = pop3Port == 0 ? -1 : pop3Port;
      String protocol = pageState.getEntry(SystemConstants.FLD_EMAIL_DOWNLOAD_PROTOCOL).stringValueNullIfEmpty();

      Properties mailProps = new Properties();
      boolean useStartTLS = pageState.getEntry("MAIL_POP3_USE_TLS").checkFieldValue();
      if (useStartTLS)
        mailProps.setProperty("mail." + protocol + ".starttls.enable", "true");

      ByteArrayOutputStream baos = new ByteArrayOutputStream();
      PrintStream ps = new PrintStream(baos);

      try {
        Session sessionM = MailHelper.getSessionForReceiving(mailProps);
        sessionM.setDebug(true);
        sessionM.setDebugOut(ps);

        store = sessionM.getStore(protocol);
        store.connect(pop3Host, pop3Port, pop3User, pop3Psw);
        folder = store.getFolder("INBOX");
        folder.open(Folder.READ_WRITE);
        Message[] messages = folder.getMessages();

        if (messages.length > 0) {
          pageState.addMessageError("Folder contains " + messages.length + " messages. Pop3/IMAP for Teamwok should use a NEW account, otherwise all messages left on server of the account will be resent and then DELETED.", "Error testing pop3.");
          json.element("ok",false);
        } else {
          pageState.addMessageOK("POP3 server successfully tested.");
        }

      } catch (MessagingException me){
        String content = new String(baos.toByteArray());
        pageState.addMessageError(JSP.convertLineFeedToBR(content),"Error testing pop3.");
        json.element("ok",false);

      } finally {
        ps.close();
        baos.close();
        if (folder!=null)
          folder.close(true);
        if (store !=null)
          store.close();
      }


      // ----------------------------------------------- TEST SMTP -----------------------------------------------
    } else if ("TSTSMTP".equals(pageState.command)) {

      //se il logged non ha una mail salvata non si può andare avanti
      if (!JSP.ex(logged.getDefaultEmail())){
        throw new Exception("In order to test SMTP/SMTPS you must have an email set on your user "+logged.getDisplayName());
      }

      //è troppo complicato riscrivere tutti i parametri

      //si salva la vecchia configurazione
      Map<String, String>  old = new HashTable();
      old.putAll(ApplicationState.applicationSettings);

      ByteArrayOutputStream baos = new ByteArrayOutputStream();
      PrintStream ps = new PrintStream(baos);
      try {

        //si sovrascruvono i parametri ricevuti
        ApplicationState.applicationSettings.put(SystemConstants.FLD_MAIL_SMTP, pageState.getEntry(SystemConstants.FLD_MAIL_SMTP).stringValue());
        ApplicationState.applicationSettings.put(SystemConstants.FLD_MAIL_SMTP_PORT, pageState.getEntry(SystemConstants.FLD_MAIL_SMTP_PORT).stringValue());
        ApplicationState.applicationSettings.put(SystemConstants.FLD_MAIL_USE_AUTHENTICATED, pageState.getEntry(SystemConstants.FLD_MAIL_USE_AUTHENTICATED).stringValue());
        ApplicationState.applicationSettings.put("MAIL_SMTP_USE_TLS", pageState.getEntry("MAIL_SMTP_USE_TLS").stringValue());
        ApplicationState.applicationSettings.put(SystemConstants.FLD_MAIL_USER, pageState.getEntry(SystemConstants.FLD_MAIL_USER).stringValue());
        ApplicationState.applicationSettings.put(SystemConstants.FLD_MAIL_PWD, pageState.getEntry(SystemConstants.FLD_MAIL_PWD).stringValue());
        String protocol = pageState.getEntry(SystemConstants.FLD_MAIL_PROTOCOL).stringValue();
        ApplicationState.applicationSettings.put(SystemConstants.FLD_MAIL_PROTOCOL, protocol);

        //si manda una mail all'utente loggato
        Session sessionForSending = MailHelper.getSessionForSending();
        sessionForSending.setDebug(true);
        sessionForSending.setDebugOut(ps);

        MimeMessage message = new MimeMessage(sessionForSending);

        InternetAddress systemMail = MailHelper.getSystemInternetAddress();
        systemMail.setPersonal(logged.getDisplayName(), "UTF-8");
        message.setFrom(systemMail);

        message.addRecipient(MimeMessage.RecipientType.TO, new InternetAddress(logged.getDefaultEmail()));
        message.setSubject("Test "+protocol+": "+ DateUtilities.dateToHourMinutes(new Date()), "UTF-8");
        message.setContent("If you read this message, everything works fine!<br>You can save Twproject mail configuration.", "text/html; charset=\"UTF-8\"");
        String date = MailHelper.mailHeaderDate();
        if (message.getHeader("Date") == null) {
          message.addHeader("Date", date);
        }
        message.addHeader("X-Mailer", "JBlooming Platform");

        MailHelper._testSend(message,sessionForSending);

        pageState.addMessageInfo("SMTP server connection testes successfully.<br>Check your inbox "+logged.getDefaultEmail()+"<br>Once you receive the message save mail configuration.");

      } catch (Exception e){
        String content = new String(baos.toByteArray());
        pageState.addMessageError(JSP.convertLineFeedToBR(content),"Error testing pop3.");
        json.element("ok",false);
        throw e;

      } finally {
        //si rimette tutto com'era
        ApplicationState.applicationSettings=old;

        ps.close();
        baos.close();

      }

    }


  } catch (Throwable t) {
    jsonHelper.error(t);
  }

  jsonHelper.close(pageContext);


%>