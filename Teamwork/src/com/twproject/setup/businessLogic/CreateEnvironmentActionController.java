package com.twproject.setup.businessLogic;

import com.opnlb.website.waf.WebSiteConstants;
import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Person;
import com.twproject.security.RoleTeamwork;
import com.twproject.security.TeamworkArea;
import com.twproject.setup.WizardSupport;
import org.jblooming.ApplicationException;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.anagraphicalData.AnagraphicalData;
import org.jblooming.messaging.MailHelper;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.ThreadLocalPersistenceContextCarrier;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.remoteFile.Document;
import org.jblooming.remoteFile.FileStorage;
import org.jblooming.security.businessLogic.LoginAction;
import org.jblooming.system.SystemConstants;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.HttpUtilities;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.ActionSupport;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.constants.OperatorConstants;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;


public class CreateEnvironmentActionController extends ActionSupport implements ActionController {
  public CreateEnvironmentActionController(PageState pageState) {
    super(pageState);
  }

  public PageState perform(HttpServletRequest request, HttpServletResponse response) throws PersistenceException, ApplicationException {
    PageState currentPageState = PageState.getCurrentPageState(request);
    String command = restState.getCommand();
    if (Commands.SAVE.equals(command)) {
      cmdSave(request, response);
    } else if ("FORGOT_PWD".equals(restState.command)) {
      try {
        cmdForgot();
      } catch (ActionException a) {
      }
    } else if ("resetPassword".equals(restState.command)) {
      cmdReset();
    }
    return currentPageState;
  }

  private void cmdReset() throws ApplicationException {
    int opId = restState.getEntry("OPID").intValueNoErrorCodeNoExc();
    String oldPassword = restState.getEntry("OP").stringValueNullIfEmpty();
    String newPassword = restState.getEntry("NP").stringValueNullIfEmpty();
    TeamworkOperator op = null;
    try {
      op = (TeamworkOperator) PersistenceHome.findByPrimaryKey(TeamworkOperator.class, opId);
      if (op != null && op.getPassword().equals(oldPassword)) {
        op.changePassword( newPassword);
        op.store();
      } else {
        ClientEntry loginNameEntry = new ClientEntry(OperatorConstants.FLD_LOGIN_NAME, "");
        loginNameEntry.errorCode = "Inconsistent data in URL.";
        restState.addClientEntry(loginNameEntry);
      }
    } catch (PersistenceException e) {
      throw new ApplicationException(e);
    }
  }


  private void cmdForgot() throws ActionException, ApplicationException {
    ClientEntry loginNameEntry = restState.getEntryAndSetRequired(OperatorConstants.FLD_LOGIN_NAME);
    String emailAndLoginName = loginNameEntry.stringValue();

    // String password = pageState.getEntryAndSetRequired("pwd").stringValue();
    String newPassword = StringUtilities.generatePassword(6);

    // check if user with this e-mail exists
    {
      try {
        TeamworkOperator op = (TeamworkOperator) PersistenceHome.findUnique(TeamworkOperator.class, "loginName", emailAndLoginName);
        if (op == null) {
          loginNameEntry.errorCode = "No operator defined for this account.";
          throw new ActionException("user not exists");
        }

        // reset password link
        PageSeed resetLink = new PageSeed(ApplicationState.serverURL + "/applications/teamwork/forgotPassword.jsp");
        resetLink.command = "resetPassword";
        resetLink.addClientEntry("NP", newPassword);
        resetLink.addClientEntry("OP", op.getPassword());
        resetLink.addClientEntry("OPID", op.getId());

        Person pe = op.getPerson();

        // Send password to user
        String mailMessage = "Hi " + pe.getDisplayName() + ", <br><br>" +
            "&nbsp;&nbsp;this e-mail has been sent to " + pe.getDefaultEmail() + " in order to reset your password. <br><br>" +
            "If you did not ask for resetting the password, ignore this message and do nothing. <br>" +
            "In case you really need to reset you password just click on the link below and you password will be changed.<br><hr>" +
            "New password active after reset: " + newPassword + "<br>" +
            "You can anytime change your login name and you password.<hr><br>" +
            "Click here to reset you password: <a href=\"" + resetLink.toLinkToHref() + "\">" + resetLink.toLinkToHref() + "</a><br><br>" +
            "Best regards,<br>" +
            "Twproject Staff";


        sendMessageToOperatorAndInfo(pe, "Twproject password recovery", mailMessage);


      } catch (PersistenceException fe) {
        loginNameEntry.errorCode = "no account for this e-mail.";
        throw new ActionException("user not exists");
      }


    }
  }

  private void cmdSave(HttpServletRequest request, HttpServletResponse response) throws ApplicationException, PersistenceException {
    try {

      ClientEntry loginNameEntry = restState.getEntryAndSetRequired(OperatorConstants.FLD_LOGIN_NAME);
      String emailAndLoginName = loginNameEntry.stringValue();

      // get the administrator
      TeamworkOperator administrator = (TeamworkOperator) PersistenceHome.findUniqueNullIfEmpty(TeamworkOperator.class, "loginName", "administrator");

      ClientEntry pwdEntry = restState.getEntryAndSetRequired(OperatorConstants.FLD_PWD);
      String password = pwdEntry.stringValue();

      //test same pwd
      if (!password.equals(restState.getEntryAndSetRequired(OperatorConstants.FLD_PWD_RETYPE).stringValue())) {
          pwdEntry.errorCode = "Retyped password not matching";
          throw new ActionException("Retyped password not matching");
        }


      //String password = StringUtilities.generatePassword(6);

      // check if user already exists
      {
        TeamworkOperator op = (TeamworkOperator) PersistenceHome.findUniqueNullIfEmpty(TeamworkOperator.class, "loginName", emailAndLoginName);
        if (op != null) {
          loginNameEntry.errorCode = "user already exists. Choose another name.";
          throw new ActionException("user already exists");
        }
      }

    //  try {
/*        boolean isResponseCorrect = false;
        String remoteAddr = request.getRemoteAddr();
        ReCaptchaImpl reCaptcha = new ReCaptchaImpl();
        reCaptcha.setPrivateKey("6Lc0hQwAAAAAAGxnyfgUo8o5-4NHdiOJ7H5TEGY-");

        String challenge = request.getParameter("recaptcha_challenge_field");
        String uresponse = request.getParameter("recaptcha_response_field");
        if (JSP.ex(challenge, uresponse)) {
          ReCaptchaResponse reCaptchaResponse = reCaptcha.checkAnswer(remoteAddr, challenge, uresponse);

          isResponseCorrect = reCaptchaResponse.isValid();
        }
        if (!isResponseCorrect) {
          pageState.getEntry("recaptcha_challenge_field").errorCode = "letters do not correspond to image";
          throw new ActionException("invalid letters from image");
        }
//          } catch (Throwable e) {
//        pageState.getEntry("recaptcha_challenge_field").errorCode = "session expired - retry";
//       throw new ActionException("session expired - retry");
//      }
            */
      // create the person
      Person person = new Person();
      person.setIdAsNew();
      person.setPersonName("Test");
      person.setPersonSurname("User");
      person.store();

      // create the operator
      TeamworkOperator operator = new TeamworkOperator();
      operator.setIdAsNew();
      operator.setName("Test");
      operator.setSurname("User");
      operator.setLoginName(emailAndLoginName);
      operator.changePassword(password);
      operator.putOption(OperatorConstants.MEDIA_PREFERRED_CHANNEL, MessagingSystem.Media.LOG + "");
      operator.putOption(WebSiteConstants.HOME_PAGE, "personFirstStart.jsp");
      operator.store();

      ThreadLocalPersistenceContextCarrier threadLocalPersistenceContextCarrier = PersistenceContext.threadLocalPersistenceContextCarrier.get();
      threadLocalPersistenceContextCarrier.setOperator(operator);

      person.setMyself(operator);
      person.store();

      // create the area
      TeamworkArea area;
      area = new TeamworkArea();
      area.setIdAsNew();
      area.setName("Production");
      area.setOwner(administrator);
      area.setFreeAccount(Fields.TRUE);
      area.setEnabledOperators(30);
      area.setCreator(operator.getId() + "");
      area.setExpiry(new Date(System.currentTimeMillis()+ 15*CompanyCalendar.MILLIS_IN_DAY)); // si setta la scadenza della demo

      area.store();
      person.setArea(area);

      // create the AnagraphicalData
      AnagraphicalData data = new AnagraphicalData();
      data.setIdAsNew();
      data.setOrderFactor(1);
      data.setLocationDescription("default");
      data.setEmail(emailAndLoginName);
      data.store();
      person.getAnagraphicalDatas().add(data);

      // ---------------------------------- Area Manager ----------------------------------
      RoleTeamwork am = WizardSupport.getRole("AM", "Area manager", area, operator);
      WizardSupport.setAreaManagerPermissions(am);
      am.store();

      //  ---------------------------------- Project manager ----------------------------------
      RoleTeamwork pm = WizardSupport.getRole("PM", "Project manager", area, operator);
      WizardSupport.setProjectManagerPermissions(pm);
      pm.store();

      // ---------------------------------- Customer ----------------------------------
      RoleTeamwork so = WizardSupport.getRole("SH", "Customer", area, operator);
      WizardSupport.setStakeholderPermissions(so);
      so.store();

      // ---------------------------------- Worker ----------------------------------
      RoleTeamwork wo = WizardSupport.getRole("WK", "Worker", area, operator);
      WizardSupport.setWorkerPermissions(wo);
      wo.store();

      // ---------------------------------- Supervisor ----------------------------------
      RoleTeamwork reader = WizardSupport.getRole("SU", "Supervisor", area, operator);
      WizardSupport.setSupervisorPermissions(reader);
      reader.store();

      // ---------------------------------- Operational ----------------------------------
      RoleTeamwork stdOperatorRole = WizardSupport.getRole("OP", "Operational", area, operator);
      WizardSupport.setOperatorPermissions(stdOperatorRole);
      stdOperatorRole.setLocalToAssignment(false);
      stdOperatorRole.store();

      // set operator as AreaManager
      operator.addRoleAndPersist(am);

      // set operator options
      //operator.putOption("HOME_PAGE", "firstStart.page");
      operator.putOption(OperatorConstants.FLD_SELECT_LANG, restState.getEntry(OperatorConstants.FLD_SELECT_LANG).stringValue());
      operator.store();


      // creates a file storage
      String decentFSName = StringUtilities.replaceAllNoRegex(emailAndLoginName,"@","__");
      decentFSName = StringUtilities.replaceAllNoRegex(decentFSName,".","__");
      FileStorage fs = new FileStorage();
      fs.setIdAsNew();
      fs.setType(Document.IS_FILE_STORAGE);
      fs.setName("File_Storage_" + decentFSName);
      fs.setArea(area);
      fs.setAuthor(decentFSName);
      fs.setCode("FS_" + decentFSName);
      fs.setConnType(Document.ConnectionType.FS);
      new File(ApplicationState.getApplicationSetting("SHARED_FILE_STORAGE") + File.separator + decentFSName).mkdirs();
      fs.setContent(ApplicationState.getApplicationSetting("SHARED_FILE_STORAGE") + File.separator + decentFSName);
      fs.setOwner(administrator);
      fs.store();


      // create area specific lookup
      WizardSupport.createFixedTypesForArea(area);

      // Send password to user
      String mailMessage = "Hi " + operator.getLoginName() + ", " +
          "&nbsp;&nbsp;thank you for signing up to Teamwork's demo. " +
          "You can now use the free demo version; " +
          "you can at any time buy a teamwork hosting (get it at <a href=\"http://twproject.com/prices/#tab-id-2\" target=_blank>http://twproject.com/prices/#tab-id-2</a>), with unlimited projects. " +
          "Or you can install Twproject locally (get it at <a href=\"http://twproject.com/on-your-server\" target=_blank>http://twproject.com/on-your-server/</a>), " +
          "where data will be available locally for integrations. " +
          "<br><hr>" +
          "Login name: " + operator.getLoginName() + "<br> " +
          "Password:   " + password + "<br>" +
          "Website:    <a href=\"http://" + ApplicationState.getApplicationSetting(SystemConstants.PUBLIC_SERVER_NAME) + "?FLD_LOGIN_NAME=" + operator.getLoginName() + "&FLD_PWD=" + password + "\">" +
          ApplicationState.getApplicationSetting(SystemConstants.PUBLIC_SERVER_NAME) + "</a>" +
          "You can anytime change your login name and you password. With your account you can create other users who can log in.<hr><br>" +

          "In order to synchronize Outlook/GMail/iCal clients with Twproject, demo accounts can \"invite\" Twproject " +
          "(and hence have the appointment on Teamwork's calendar) by using this e-mail: " + ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_FROM) + "<br>" +
          "If you want to receive in your Outlook/GMail/iCal client Twproject appointments, just activate the checkbox in your options. " +
          "All details on usage can be found in our support site: " +
          "<a href=\"http://twproject.com/support\" target=_blank>http://twproject.com/support/</a>" +
          "" +

          "Start working now at <a href=\"http://" + ApplicationState.getApplicationSetting(SystemConstants.PUBLIC_SERVER_NAME) + "\">" + ApplicationState.getApplicationSetting(SystemConstants.PUBLIC_SERVER_NAME) + "</a>" +
          "Best regards,<br>" +
          "Twproject Staff";


      sendMessageToOperatorAndInfo(person, "Twproject account created", mailMessage);

      //manda dati online
      try {
        HttpUtilities.getPageContent("https://shop.twproject.com/utils/onlineDemoReport.jsp?email="+emailAndLoginName);
      } catch (Throwable e) {
      }



      PageState currentPageState = PageState.getCurrentPageState(request);
      LoginAction.doLog(operator, currentPageState.sessionState);

    } catch (ActionException e) {
    }
  }


  private void sendMessageToOperatorAndInfo(Person person, String subject, String mailMessage) {

    MailHelper.sendHtmlMailInSeparateThread(null, person.getDefaultEmail(), subject, mailMessage);

    try {
      Thread.sleep(2000);
    } catch (InterruptedException e) {
      Tracer.platformLogger.error(e);
    }


    if (Fields.TRUE.equals(ApplicationState.applicationParameters.get("TEAMWORK_ASP_INSTANCE"))) {
      MailHelper.sendHtmlMailInSeparateThread(null, "info@twproject.com", "Mail in CC copy: " + subject + " " + person.getDefaultEmail(), mailMessage);
    }
  }
}
