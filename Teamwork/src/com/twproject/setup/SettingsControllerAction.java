package com.twproject.setup;

import com.opnlb.fulltext.IndexingConstants;
import com.opnlb.fulltext.IndexingHelper;
import org.jblooming.security.License;
import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.ldap.LdapUtilities;
import org.jblooming.oql.QueryHelper;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.system.SystemConstants;
import org.jblooming.utilities.HttpUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.utilities.file.FileUtilities;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.constants.OperatorConstants;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.PlatformConfiguration;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.IOException;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.NumberFormat;
import java.util.Properties;
import java.util.Locale;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Sep 26, 2008
 * Time: 5:46:45 PM
 */
public class SettingsControllerAction implements ActionController {

  public static boolean ldapAuthHasJustBeenSet(PageState pageState) throws ActionException {
    boolean ldapAuthHasJustBeenSet = false;
    String authNewValue = pageState.getEntry(SystemConstants.AUTHENTICATION_TYPE).stringValue();
    if (
      (!Fields.TRUE.equals(pageState.getEntry("CONFIRM_LDAP_AUTH").stringValueNullIfEmpty())) &&
        JSP.ex(authNewValue) &&
        (!authNewValue.equals(ApplicationState.getApplicationSetting(SystemConstants.AUTHENTICATION_TYPE))) &&
        (SystemConstants.ENABLE_AUTHENTICATION_TYPE.ENABLE_LDAP_AUTHENTICATION.toString().equals(authNewValue) || SystemConstants.ENABLE_AUTHENTICATION_TYPE.ENABLE_LDAP_AUTHENTICATION_WITH_FALLBACK_ON_STANDARD.toString().equals(authNewValue))
      ) {
      ldapAuthHasJustBeenSet = true;
    }
    return ldapAuthHasJustBeenSet;
  }


  public PageState perform(HttpServletRequest request, HttpServletResponse response)
    throws ApplicationException, IOException, PersistenceException, ActionException, org.jblooming.security.SecurityException {

    PageState pageState = PageState.getCurrentPageState(request);
    final String command = pageState.getCommand();

    if (Commands.SAVE.equals(command)) {

      String globalPath = HttpUtilities.getFileSystemRootPathForRequest(request) + File.separator + "commons" + File.separator + "settings" + File.separator + PlatformConfiguration.globalSettingsFileName;

      File global = new File(globalPath);
      if (!global.exists())
        throw new PlatformRuntimeException("Global Settings File Name points to a non existing file: " + globalPath);

      Properties properties = FileUtilities.getProperties(globalPath);

      if (!ldapAuthHasJustBeenSet(pageState)) {
        putProperty(SystemConstants.AUTHENTICATION_TYPE, pageState.getEntry(SystemConstants.AUTHENTICATION_TYPE).stringValue(), properties);
      } else {
        pageState.initializeEntries("table");
      }


      try {
        //mail
        putProperty(SystemConstants.FLD_MAIL_FROM, pageState.getEntry(SystemConstants.FLD_MAIL_FROM).stringValue(), properties);

        putProperty(SystemConstants.FLD_MAIL_SMTP, pageState.getEntry(SystemConstants.FLD_MAIL_SMTP).stringValue(), properties);
        putProperty(SystemConstants.FLD_MAIL_SMTP_PORT, pageState.getEntry(SystemConstants.FLD_MAIL_SMTP_PORT).stringValue(), properties);
        putProperty(SystemConstants.FLD_MAIL_USE_AUTHENTICATED, pageState.getEntry(SystemConstants.FLD_MAIL_USE_AUTHENTICATED).stringValue(), properties);
        putProperty("MAIL_SMTP_USE_TLS", pageState.getEntry("MAIL_SMTP_USE_TLS").stringValue(), properties);
        putProperty(SystemConstants.FLD_MAIL_USER, pageState.getEntry(SystemConstants.FLD_MAIL_USER).stringValue(), properties);
        putProperty(SystemConstants.FLD_MAIL_PWD, pageState.getEntry(SystemConstants.FLD_MAIL_PWD).stringValue(), properties);
        putProperty(SystemConstants.FLD_MAIL_PROTOCOL, pageState.getEntry(SystemConstants.FLD_MAIL_PROTOCOL).stringValue(), properties);
        putProperty(SystemConstants.FLD_MAIL_SUBJECT, pageState.getEntry(SystemConstants.FLD_MAIL_SUBJECT).stringValue(), properties);

        String pop3HostNew = pageState.getEntry(SystemConstants.FLD_POP3_HOST).stringValue();
        String pop3UserNew = pageState.getEntry(SystemConstants.FLD_POP3_USER).stringValue();
        putProperty(SystemConstants.FLD_POP3_HOST, pop3HostNew, properties);
        putProperty(SystemConstants.FLD_POP3_USER, pop3UserNew, properties);
        putProperty(SystemConstants.FLD_POP3_PSW, pageState.getEntry(SystemConstants.FLD_POP3_PSW).stringValue(), properties);
        int port = pageState.getEntry(SystemConstants.FLD_POP3_PORT).intValueNoErrorCodeNoExc();
        putProperty(SystemConstants.FLD_POP3_PORT, (port == 0 ? -1 : port) + "", properties);
        putProperty("MAIL_POP3_USE_TLS", pageState.getEntry("MAIL_POP3_USE_TLS").stringValue(), properties);
        putProperty(SystemConstants.FLD_EMAIL_DOWNLOAD_PROTOCOL, pageState.getEntry(SystemConstants.FLD_EMAIL_DOWNLOAD_PROTOCOL).stringValue(), properties);

        //projects

        putProperty("MILESTONE_ALERT_DELTA", pageState.getEntry("MILESTONE_ALERT_DELTA").stringValue(), properties);
        putProperty("DEFAULT_PROJECT_MANAGER_ROLE_NAME", pageState.getEntry("DEFAULT_PROJECT_MANAGER_ROLE_NAME").stringValue(), properties);
        putProperty("DEFAULT_WORKER_ROLE_NAME", pageState.getEntry("DEFAULT_WORKER_ROLE_NAME").stringValue(), properties);
        putProperty("DEFAULT_CUSTOMER_ROLE_NAME", pageState.getEntry("DEFAULT_CUSTOMER_ROLE_NAME").stringValue(), properties);


        //on global.properties number are always in locale.US
        NumberFormat numberInstance = NumberFormat.getNumberInstance(Locale.US);
        putProperty("ASSIG_COST", numberInstance.format(pageState.getEntry("ASSIG_COST").doubleValueNoErrorNoCatchedExc()), properties);

        putProperty("ROLE_SCRUM_MASTER_NAME", pageState.getEntry("ROLE_SCRUM_MASTER_NAME").stringValue(), properties);
        putProperty("ROLE_SCRUM_TEAM_NAME", pageState.getEntry("ROLE_SCRUM_TEAM_NAME").stringValue(), properties);
        putProperty("MPXJ_LOCALE", pageState.getEntry("MPXJ_LOCALE").stringValue(), properties);
        putProperty("SET100ONCLOSE", pageState.getEntry("SET100ONCLOSE").stringValue(), properties);
        putProperty("USETYPEONISSUES", pageState.getEntry("USETYPEONISSUES").stringValue(), properties);
        putProperty("USECODEONISSUES", pageState.getEntry("USECODEONISSUES").stringValue(), properties);
        putProperty("USEIMPACTONISSUES", pageState.getEntry("USEIMPACTONISSUES").stringValue(), properties);
        putProperty("USEUNIQUECODES", pageState.getEntry("USEUNIQUECODES").stringValue(), properties);

        putProperty("EMAIL_TASK_ASSOCIATIONS", pageState.getEntry("EMAIL_TASK_ASSOCIATIONS").stringValue(), properties);

        putProperty("GENTASKCODES", pageState.getEntry("GENTASKCODES").stringValue(), properties);

        putProperty("DISABLEWORKFLOW",License.assertLevel(30)?pageState.getEntry("DISABLEWORKFLOW").stringValue():"yes", properties);
        putProperty(OperatorConstants.FLD_WORKING_HOUR_TOTAL, pageState.getEntry(OperatorConstants.FLD_WORKING_HOUR_TOTAL).stringValue(), properties);

        //commons
        putProperty("DEFAULT_STORAGE_TYPE", pageState.getEntry("DEFAULT_STORAGE_TYPE").stringValue(), properties);
        putProperty(SystemConstants.FLD_REPOSITORY_URL, pageState.getEntry(SystemConstants.FLD_REPOSITORY_URL).stringValue(), properties);

        int UPLOAD_MAX_SIZE = 20;
        try {
          UPLOAD_MAX_SIZE = pageState.getEntry(SystemConstants.UPLOAD_MAX_SIZE).intValue();
        } catch (ParseException e) {
          UPLOAD_MAX_SIZE = 20;
        }
        putProperty(SystemConstants.UPLOAD_MAX_SIZE, UPLOAD_MAX_SIZE + "", properties);
        putProperty(SystemConstants.STORAGE_PATH_ALLOWED, pageState.getEntry(SystemConstants.STORAGE_PATH_ALLOWED).stringValue(), properties);
        String psn = pageState.getEntry(SystemConstants.PUBLIC_SERVER_NAME).stringValue();
        if (JSP.ex(psn) && psn.contains(":")) {
          pageState.getEntry(SystemConstants.PUBLIC_SERVER_NAME).errorCode = "Don't put server port in server name: " + psn;
        }
        putProperty(SystemConstants.PUBLIC_SERVER_NAME, psn, properties);

        String psp = pageState.getEntry(SystemConstants.PUBLIC_SERVER_PORT).stringValue();
        if (JSP.ex(psp) && psp.contains(":")) {
          pageState.getEntry(SystemConstants.PUBLIC_SERVER_PORT).errorCode = "Don't put ':' in server port: " + psp;
        }
        putProperty(SystemConstants.PUBLIC_SERVER_PORT, psp, properties);

        String confProt = pageState.getEntry(SystemConstants.HTTP_PROTOCOL).stringValue();
        putProperty(SystemConstants.HTTP_PROTOCOL, confProt, properties);

        //op
        putProperty(OperatorConstants.FLD_CURRENT_SKIN, pageState.getEntry(OperatorConstants.FLD_CURRENT_SKIN).stringValue(), properties);
        putProperty(OperatorConstants.OP_PAGE_SIZE, pageState.getEntry(OperatorConstants.OP_PAGE_SIZE).stringValue(), properties);

        putProperty(OperatorConstants.FLD_WORKING_HOUR_BEGIN, pageState.getEntry(OperatorConstants.FLD_WORKING_HOUR_BEGIN).stringValue(), properties);
        putProperty(OperatorConstants.FLD_WORKING_HOUR_END, pageState.getEntry(OperatorConstants.FLD_WORKING_HOUR_END).stringValue(), properties);
        putProperty("SHOW_USER_SCORES", pageState.getEntry("SHOW_USER_SCORES").stringValue(), properties);

        //i18n
        putProperty(OperatorConstants.FLD_SELECT_LANG, pageState.getEntry(OperatorConstants.FLD_SELECT_LANG).stringValue(), properties);
        putProperty(SystemConstants.FRIDAY_IS_WORKING_DAY, pageState.getEntry(SystemConstants.FRIDAY_IS_WORKING_DAY).stringValue(), properties);
        putProperty(SystemConstants.SATURDAY_IS_WORKING_DAY, pageState.getEntry(SystemConstants.SATURDAY_IS_WORKING_DAY).stringValue(), properties);
        putProperty(SystemConstants.SUNDAY_IS_WORKING_DAY, pageState.getEntry(SystemConstants.SUNDAY_IS_WORKING_DAY).stringValue(), properties);


        String maybeCurr = pageState.getEntry(SystemConstants.CURRENCY_FORMAT).stringValue();
        boolean schiants = false;
        try {
          DecimalFormat format = new DecimalFormat(maybeCurr);
          format.format(123);
        } catch (Throwable t) {
          schiants = true;
        }
        if (!schiants)
          putProperty(SystemConstants.CURRENCY_FORMAT, maybeCurr, properties);
        else
          pageState.getEntry(SystemConstants.CURRENCY_FORMAT).errorCode = "This format is not legal in Java currency";

        String printLogo = pageState.getEntry(SystemConstants.PRINT_LOGO).stringValueNullIfEmpty();
        printLogo = JSP.ex(printLogo) ? printLogo : "printLogo.gif";
        putProperty(SystemConstants.PRINT_LOGO, printLogo, properties);
        String tzv = pageState.getEntry("SERVER_TIME_ZONE").stringValue();
        if (JSP.ex(tzv)) {
          putProperty("SERVER_TIME_ZONE", tzv, properties);
        }

        //var
        putProperty(QueryHelper.QBE_CONVERT_TO_UPPER, pageState.getEntry(QueryHelper.QBE_CONVERT_TO_UPPER).stringValue(), properties);
        putProperty(SystemConstants.SETUP_DB_UPDATE_DONE, pageState.getEntry(SystemConstants.SETUP_DB_UPDATE_DONE).stringValue(), properties);
        putProperty(SystemConstants.SETUP_NOTIFIED_ADMIN_WIZARDS, pageState.getEntry(SystemConstants.SETUP_NOTIFIED_ADMIN_WIZARDS).stringValue(), properties);
        putProperty(SystemConstants.AUDIT, pageState.getEntry(SystemConstants.AUDIT).stringValue(), properties);
        putProperty("COMBO_ROWS_TO_FETCH", pageState.getEntry("COMBO_ROWS_TO_FETCH").stringValue(), properties);

        //secur
        putProperty(SystemConstants.ENABLE_REDIR_AFTER_LOGIN, pageState.getEntry(SystemConstants.ENABLE_REDIR_AFTER_LOGIN).stringValue(), properties);
        putProperty(SystemConstants.ALLOW_EMPTY_STRING_PSW, pageState.getEntry(SystemConstants.ALLOW_EMPTY_STRING_PSW).stringValue(), properties);
        putProperty(SystemConstants.DISABLE_COOKIE_LOGIN, pageState.getEntry(SystemConstants.DISABLE_COOKIE_LOGIN).stringValue(), properties);
        putProperty(SystemConstants.FLD_PASSWORD_EXPIRY, pageState.getEntry(SystemConstants.FLD_PASSWORD_EXPIRY).stringValue(), properties);
        putProperty(SystemConstants.FLD_PASSWORD_MIN_LEN, pageState.getEntry(SystemConstants.FLD_PASSWORD_MIN_LEN).stringValue(), properties);
        putProperty(OperatorConstants.FLD_LOGOUT_TIME, pageState.getEntry(OperatorConstants.FLD_LOGOUT_TIME).stringValue(), properties);
        putProperty(OperatorConstants.FLD_MAX_LOGIN_FAILED, pageState.getEntry(OperatorConstants.FLD_MAX_LOGIN_FAILED).stringValue(), properties);

        //ldap
        if (License.assertLevel(30)) {
          putProperty(LdapUtilities.DOMAIN_NAME, pageState.getEntry(LdapUtilities.DOMAIN_NAME).stringValue(), properties);
          putProperty(LdapUtilities.BASE_DN, pageState.getEntry(LdapUtilities.BASE_DN).stringValue(), properties);
          String ldap_url = pageState.getEntry(LdapUtilities.PROVIDER_URL).stringValue();
          if (ldap_url.toLowerCase().indexOf("ldap://") == -1 && ldap_url.toLowerCase().indexOf("ldaps://") == -1)
            ldap_url = "ldap://" + ldap_url;
          putProperty(LdapUtilities.PROVIDER_URL, ldap_url, properties);
          putProperty(LdapUtilities.SECURITY_PRINCIPAL, pageState.getEntry(LdapUtilities.SECURITY_PRINCIPAL).stringValue(), properties);
          // test for retyped security credentials  identical
          ClientEntry epsw = pageState.getEntry(LdapUtilities.SECURITY_CREDENTIALS);
          ClientEntry repsw = pageState.getEntry("RETYPE_" + LdapUtilities.SECURITY_CREDENTIALS);
          if (!epsw.stringValue().equals(repsw.stringValue())) {
            epsw.errorCode = "ERR_PASSWORD_MUST_BE_IDENTICAL";
            repsw.setValue("");
            throw new ActionException();
          } else {
            putProperty(LdapUtilities.SECURITY_CREDENTIALS, StringUtilities.encrypt(epsw.stringValue()), properties);
          }
          putProperty(LdapUtilities.SECURITY_AUTHENTICATION, pageState.getEntry(LdapUtilities.SECURITY_AUTHENTICATION).stringValue(), properties);

          properties.put(LdapUtilities.CREATE_USERS_ON_LOGIN, pageState.getEntry(LdapUtilities.CREATE_USERS_ON_LOGIN).checkFieldValue() ? Fields.TRUE : Fields.FALSE);
          properties.put(LdapUtilities.CREATE_USERS_IN_AREA, JSP.w(pageState.getEntry(LdapUtilities.CREATE_USERS_IN_AREA).stringValueNullIfEmpty()));

          properties.put(LdapUtilities.ENABLE_LDAP_SSL, pageState.getEntry(LdapUtilities.ENABLE_LDAP_SSL).checkFieldValue() ? Fields.TRUE : Fields.FALSE);

          String ldapFileName = pageState.getEntry(LdapUtilities.LDAP_CONFIG_FILE).stringValue();
          putProperty(LdapUtilities.LDAP_CONFIG_FILE, ldapFileName, properties);
          ApplicationState.applicationSettings.put(LdapUtilities.LDAP_CONFIG_FILE, ldapFileName);
          LdapUtilities.loadLdapMappingFromFile();
        }

        //indexing
        putProperty(IndexingConstants.INDEX_PATH, pageState.getEntry(IndexingConstants.INDEX_PATH).stringValue(), properties);
        String analyzLang = pageState.getEntry(IndexingConstants.ANALYZER_LANGUAGE).stringValue();
        putProperty(IndexingConstants.ANALYZER_LANGUAGE, analyzLang, properties);

        if (pageState.validEntries()) {
          ApplicationState.getApplicationSettings().putAll(properties);
          ApplicationState.refreshGlobalSettings(properties, request);

          IndexingHelper.refreshIndexSettings(analyzLang, pageState);

          ApplicationState.dumpApplicationSettings();
        }


      } catch (ActionException e) {
      }


    }
    return pageState;

  }

  private static void putProperty(String name, String value, Properties properties) {
    if (value == null)
      properties.put(name, "");
    else
      properties.put(name, value);
  }

}
