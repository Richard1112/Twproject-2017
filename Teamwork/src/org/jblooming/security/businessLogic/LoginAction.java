package org.jblooming.security.businessLogic;

import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.ldap.LdapUtilities;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.scheduler.Scheduler;
import org.jblooming.tracer.Tracer;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.operator.Operator;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.FindException;
import org.jblooming.system.SystemConstants;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.RecaptchaV2;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.OperatorConstants;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.state.ScreenElementStatus;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.PlatformConfiguration;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.ClientEntries;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.security.NoSuchAlgorithmException;
import java.util.Iterator;
import java.util.Date;
import java.util.List;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */
public class LoginAction {


  //overridden by TeamworkSettings
  public static String cookieName="LOGINCOOKIE";
  public static String cookiePath="/commons/security";
  public static String cookieSaltPepper= "[vh34ult[j'x12[]srtuvf";

  public void login(PageState pageState, HttpServletRequest request, HttpServletResponse response) throws PersistenceException, ApplicationException {

    if (pageState.getCommand() != null && pageState.getCommand().equals(Commands.LOGOUT)) {
      pageState.setClientEntries(new ClientEntries());
      return;
    }

    Integer loginAttemptFailed = (Integer) pageState.sessionState.getAttribute("loginAttemptFailed");

    int maxLoginAttemptFailed = Integer.parseInt(ApplicationState.getApplicationSetting(OperatorConstants.FLD_MAX_LOGIN_FAILED, "0"));

    boolean isResponseCorrect = false;
    boolean checkCaptcha = JSP.ex(loginAttemptFailed) && loginAttemptFailed > maxLoginAttemptFailed && maxLoginAttemptFailed>0;

    if (checkCaptcha){
      RecaptchaV2 recaptcha = new RecaptchaV2("6LeGqCITAAAAANwJVFJhjMxha7m_OqJXs7U-Cbiz", "6LeGqCITAAAAAKfLkKiLYsvcEOwBGpGe1ciygt1j");
      isResponseCorrect = recaptcha.isValid(pageState,true);
    }

    if (isResponseCorrect || !checkCaptcha) {

    Operator user = null;

    String password = null;

    String auth_type = ApplicationState.getApplicationSetting(SystemConstants.AUTHENTICATION_TYPE);



    // ----------------------------------------------------- COOKIES LOGIN (if enabled) ----------------------------------------------------------------------------
    if(ApplicationState.platformConfiguration.defaultApplication.isLoginCookieEnabled())
      user = cookiesAuthentication(cookieName,request, pageState);


    if (user==null){ // if not authenticated by cookies test standard

      // ----------------------------------------------------- LDAP_AUTHENTICATION ----------------------------------------------------------------------------
      if (SystemConstants.ENABLE_AUTHENTICATION_TYPE.ENABLE_LDAP_AUTHENTICATION.toString().equals(auth_type)) {
        user = ldapAuthentication(pageState);

        // ----------------------------------------------------- LDAP_AUTHENTICATION_WITH_STANDARD_FALLBACK ----------------------------------------------------------------------------
      } else if (SystemConstants.ENABLE_AUTHENTICATION_TYPE.ENABLE_LDAP_AUTHENTICATION_WITH_FALLBACK_ON_STANDARD.toString().equals(auth_type)) {
        user = ldapAuthentication(pageState);
        if (user == null){
          pageState.getEntry(OperatorConstants.FLD_LOGIN_NAME).errorCode =null;
          pageState.getEntry(OperatorConstants.FLD_LOGIN_NEW_PWD).errorCode =null;
          user = standardAuthentication(pageState);

          // se l'utente è importato/aggiornato da LDAP deve autenticarsi via LDAP  se l'opzione è accesa
          if (I18n.isActive("CUSTOM_FEATURE_FORCE_LDAP_AUTHENTICATION_FOR_IMPORTED_USERS") && user!=null && "LDAP".equals(user.getAuthentication())){
            pageState.getEntry(OperatorConstants.FLD_LOGIN_NAME).errorCode = pageState.getI18n("ERR_INVALID_LOGIN") + " (LDAP)";
            user=null;
          }
        }

        // ----------------------------------------------------- HTTP_AUTHENTICATION ----------------------------------------------------------------------------
      } else if (SystemConstants.ENABLE_AUTHENTICATION_TYPE.ENABLE_HTTP_AUTHENTICATION.toString().equals(auth_type)) {
        user = httpAuthentication(pageState, request);

        // ----------------------------------------------------- STANDARD_AUTHENTICATION ----------------------------------------------------------------------------
      } else {
        user = standardAuthentication(pageState);
      }
    }

    if (user != null && pageState.validEntries()) {

      SessionState sm = pageState.getSessionState();
      Operator op = (Operator) PersistenceHome.findByPrimaryKey(PlatformConfiguration.defaultOperatorSubclass, Integer.parseInt(user.getId().toString()));

      doLog(op, sm);


      // ----------------------------------------------------- SET LOGIN COOKIE ----------------------------------------------------------------------------
      if(ApplicationState.platformConfiguration.defaultApplication.isLoginCookieEnabled())
        setLoginCookie(op,response);

      pageState.sessionState.setAttribute("loginAttemptFailed", 0);
      pageState.setCommand(Commands.FIND);


    }else{
      if(loginAttemptFailed != null){
        pageState.sessionState.setAttribute("loginAttemptFailed", new Integer(loginAttemptFailed.intValue() + 1));
      }else{
        loginAttemptFailed = new Integer(1);
        pageState.sessionState.setAttribute("loginAttemptFailed", loginAttemptFailed);
      }
    }
  }
  }

  private Operator standardAuthentication(PageState pageState) throws PersistenceException, ApplicationException {
    String password;
    Operator user = null;

    try {
      ClientEntry ceName = pageState.getEntryAndSetRequired(OperatorConstants.FLD_LOGIN_NAME);
      ClientEntry cePassword = pageState.getEntry(OperatorConstants.FLD_PWD);

      password = cePassword.stringValue();
      String username = ceName.stringValue();

      String newPassword = null;

      try {
        user = Operator.authenticateUser(password, username);
      } catch (org.jblooming.security.SecurityException e) {
        //same error to avoid user disclosure
        ceName.errorCode = e.getMessage();
        //ceName.errorCode = "ERR_INVALID_LOGIN";
        Tracer.platformLogger.warn("Invalid login attempted with loginname: "+username+ " ("+e.getMessage()+")");

      }

      if (pageState.validEntries()) {

        final String pass_exp = ApplicationState.getApplicationSetting(SystemConstants.FLD_PASSWORD_EXPIRY);
        int maxDaysPassed = 0;

        try {
          maxDaysPassed = pass_exp != null ? Integer.parseInt(pass_exp) : 0;
        } catch (Throwable e) {
          Tracer.platformLogger.error("Invalid password expiry value in global settings:" + SystemConstants.FLD_PASSWORD_EXPIRY + "=" + pass_exp, e);
        }

        if ((maxDaysPassed > 0 &&
                user.getLastPasswordChangeDate() != null &&
                ((System.currentTimeMillis() - user.getLastPasswordChangeDate().getTime()) / (CompanyCalendar.MILLIS_IN_HOUR * 24)) > maxDaysPassed)) {

          try {
            ClientEntry ceNewPassword = pageState.getEntryAndSetRequired(OperatorConstants.FLD_LOGIN_NEW_PWD);
            ClientEntry ceNewPasswordConfirm = pageState.getEntryAndSetRequired(OperatorConstants.FLD_LOGIN_NEW_PWD_RETYPE);

            if (!ceNewPassword.stringValue().equals(ceNewPasswordConfirm.stringValue())) {
              ceNewPasswordConfirm.errorCode = "ERR_PASSWORD_MUST_BE_IDENTICAL";
              throw new ActionException();
            }
            final Iterator lastPasswordIterator = user.getLastPasswordIterator();
            while (lastPasswordIterator.hasNext()) {
              String s = (String) lastPasswordIterator.next();
              try {
                if (s.equals(user.computePassword(ceNewPassword.stringValue()))) {
                  ceNewPassword.errorCode = "ERR_PASSWORD_ALREADY_USED";
                  throw new ActionException();
                }
              } catch (NoSuchAlgorithmException e) {
                throw new ApplicationException(e);
              }
            }

            //passed all obstacles
            newPassword = ceNewPassword.stringValue();

            if (newPassword != null) {
              user.changePassword(newPassword);
            }


          } catch (ActionException e) {
          }
        }
      }

    } catch (ActionException e) {
    }
    return user;
  }

  private Operator httpAuthentication(PageState pageState, HttpServletRequest request) throws PersistenceException {
    Operator user = null;
    if (request.getRemoteUser() != null) {
      user = Operator.findByLoginName(request.getRemoteUser());
    } else {
      pageState.getEntry(OperatorConstants.FLD_LOGIN_NAME).errorCode = SystemConstants.ENABLE_AUTHENTICATION_TYPE.ENABLE_HTTP_AUTHENTICATION + "=yes on " +
              PlatformConfiguration.globalSettingsFileName + " but no user (request.getRemoteUser()) is provided by the web app context ";
    }
    return user;
  }


  private Operator ldapAuthentication(PageState pageState) {
    String password;
    String username = pageState.getEntryAndSetRequired(OperatorConstants.FLD_LOGIN_NAME).stringValueNullIfEmpty();
    password = pageState.getEntry(OperatorConstants.FLD_PWD).stringValueNullIfEmpty();
    Operator user = null;

    String domain = ApplicationState.getApplicationSetting(LdapUtilities.DOMAIN_NAME);
    String provider = ApplicationState.getApplicationSetting(LdapUtilities.PROVIDER_URL);
    String secAuth = ApplicationState.getApplicationSetting(LdapUtilities.SECURITY_AUTHENTICATION);

    if (username != null && password != null) {
      String msgError = LdapUtilities.checkUser(provider, domain, username, secAuth, password);
      if (msgError != null) {
        pageState.getEntry(OperatorConstants.FLD_LOGIN_NAME).errorCode = pageState.getI18n("ERR_INVALID_LOGIN") + " (LDAP)";

      } else {

        //got authorized; now search user
        try {
          user = Operator.findByLoginName(username);
        } catch (PersistenceException e) {
          Tracer.platformLogger.debug(e);
        }
        if (user == null) {
          boolean create = Fields.TRUE.equals(ApplicationState.getApplicationSetting(LdapUtilities.CREATE_USERS_ON_LOGIN));
          if (create) {
            user = createPlatformUserFromLDAP(username, pageState);
          } else {
            pageState.getEntry(OperatorConstants.FLD_LOGIN_NAME).errorCode = pageState.getI18n("ERR_INVALID_LOGIN") + " (LDAP)";
          }
        }
      }
    } else {
      pageState.getEntry(OperatorConstants.FLD_LOGIN_NAME).errorCode = pageState.getI18n("ERR_INVALID_LOGIN") + " (LDAP)";
    }
    return user;
  }

  protected Operator createPlatformUserFromLDAP(String username, PageState pageState) {
    throw new PlatformRuntimeException("LoginAction:createPlatformUserFromLDAP you must provide your implementation");
  }

  public static void doLog(Operator op, SessionState sessionState) throws PersistenceException, ApplicationException {
    op.setLastLoggedOn(new Date());
    op.store();
    sessionState.setLoggedOperator(op);
    sessionState.screenElementsStatus = ScreenElementStatus.getInstanceFromOptions(op);

    if (ApplicationState.platformConfiguration.schedulerRunsByDefault && !Scheduler.isRunning()) {
      ApplicationState.applicationSettings.put(SystemConstants.ADMIN_MESSAGE, "Scheduler is NOT running. Start it in admin -> monitoring -> scheduler monitor");
    }

    //reset locale on session
    sessionState.resetLocaleAndTimeZone();
    SessionState.getLocale();
  }


  public void logout(PageState pageState, HttpServletRequest request, HttpServletResponse response) {
    request.getSession().removeAttribute(Fields.SESSION);

    //session.invalidate DOES NOT WORK IN TOMCAT!

    //this is meant to bind SessionState to a NEW http session;
    // but with this, SessionState is bound to the old, dying http session
    // and after 1 minute the user is logged out (by http session)!!!
    //request.getSession().setMaxInactiveInterval(1);

    //so we keep the SAME http session, and we hope for the best
    pageState.sessionState = null;
    pageState.sessionState = SessionState._getSessionState(request);
    request.getSession().setAttribute("CMD_LOG_OUT_PARAM_SESSION_KEY", "y"); // add graziella - per evitare che il ping si repeta anche dopo il log out

    //kill cookies if enabled
    if(ApplicationState.platformConfiguration.defaultApplication.isLoginCookieEnabled())
      killCookie(response);


  }

  protected Operator cookiesAuthentication(String cookieName,HttpServletRequest request,PageState pageState ) throws FindByPrimaryKeyException {
    Operator user = null;
    Cookie loginCookie = null;
    //check if exists login cookie
    if (request.getCookies() != null) {
      for (Cookie coo : request.getCookies()) {
        if (cookieName.equals(coo.getName()) && JSP.ex(coo.getValue())) {
          loginCookie = coo;
          //generate a fake login token
          pageState.tokenCreate("login", pageState);
          break;
        }
      }
    }
    if (loginCookie != null) {
      // increment a counter for failed login in the session
      Integer count = (Integer) pageState.sessionState.getAttribute("invalidLoginCount");
      if (count == null)
        count = 0;
      pageState.sessionState.setAttribute("invalidLoginCount", new Integer(count + 1));
      // if invalid login count is greater than x return or  do something worst like block ip, suspend the thread, offend parents....
      if (count > 10)
        return null;
      //try with cookie
      user = getFromCookie(loginCookie);

    }
    return user;
  }


  public static Operator getFromCookie(Cookie loginCookie) throws FindByPrimaryKeyException {
    try {
      if (loginCookie != null) {
        return getFromCookie(loginCookie.getValue());
      }
    } catch (NoSuchAlgorithmException e) {
      Tracer.platformLogger.info("Problem recovering operator from cookies");
    }
    return null;
  }


  public static Operator getFromCookie(String loginCookie) throws FindByPrimaryKeyException, NoSuchAlgorithmException {
    Operator operator = null;

    if (JSP.ex(loginCookie)) {
      List<String> vars = StringUtilities.splitToList(loginCookie.replace(';', 'x'), "x");
      if (vars.size() >= 2) {
        Operator ope = Operator.load(vars.get(0));
        if (ope != null) {
          if (getApiKey(ope).equalsIgnoreCase(loginCookie)) {
            operator = ope;
          }
        }
      }
    }

    return operator;
  }


  public static String getApiKey(Operator op) throws NoSuchAlgorithmException {
    return op.getId() + "x" + StringUtilities.md5Encode(op.getLoginName() + op.getPassword() +cookieSaltPepper);
  }


  protected void setLoginCookie(Operator loggedOperator,HttpServletResponse response) {
    try {
      // set the cookie
      Cookie coo = null;
      coo = new Cookie(LoginAction.cookieName, getApiKey(loggedOperator));
      coo.setMaxAge(60 * 60 * 24 * 7);

      coo.setPath(ApplicationState.contextPath +cookiePath);
      response.addCookie(coo);
    } catch (Throwable e) {
      Tracer.platformLogger.error("Error saving cookies for operator: " + loggedOperator.getId() + " - " + loggedOperator.getLoginName() + e);
    }
  }

  protected void killCookie(HttpServletResponse response) {
    Cookie killMyCookie = new Cookie(cookieName, null);
    killMyCookie.setMaxAge(0);
    killMyCookie.setPath(ApplicationState.contextPath + cookiePath);
    response.addCookie(killMyCookie);
  }


}
