package org.jblooming.waf.settings;

import org.jblooming.operator.Operator;

import java.util.*;

/**
 * Holds a list of applications, and with the setup method lauches their config in the correct order.
 *
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */
public class PlatformConfiguration {

  /**
   * used for checking launch of admin features with no operator logged
   */
  public static String psw = "dom";
  public static String globalSettingsFileName = "global.properties";
  public static String logSettingsFileName = "logSetting.properties";
  public static Class defaultOperatorSubclass = Operator.class;
  public Map<String, Application> applications = new TreeMap();


  public static boolean schedulerRunsByDefault = false;
  public static boolean logOnConsole;
  public static boolean logOnFile;
  public static String logFilesRoot ;
  public static String logPattern;

  // Log file names constants
  public static String platformLogFileName    = "platform.log";
  public static String hibernateLogFileName   = "hibernate.log";
  public static String jobLoggerLogFileName   = "jobLogger.log";
  public static String i18nLogFileName        = "i18n.log";
  public static String emailLogFileName       = "email.log";

  //public static boolean oracle10gRelease2caseInsensitiveSupport = false;


  /**
   * @see "http://www.hibernate.org/293.html"
   *      Sample config for case-sensitive dbs (Oracle):
   *      stringPrimitiveFieldsConversion = PlatformConfiguration.StringConversionType.UPPER;
   *      searchStringParametersConversion = PlatformConfiguration.StringConversionType.UPPER
   *      <p/>
   *      Sample config for case-insensitive dbs (SqlServer, MySQL):
   *      leave them all to none
   */
  public static enum StringConversionType {UPPER,LOWER,NONE}


  /**
   * distinguish between development and production environment
   * example: allows to use a different login only in deployment
   */
  public boolean development = false;

  public String defaultIndex = "/index.jsp";

  public Application defaultApplication;

  public boolean catchPersistenceErrors = false;

  public boolean loadedWithPageContextSettings = false;

  public void addApplication(Application application) {
    applications.put(application.getName(), application);
  }

  public Application getDefaultApplication() {
    return defaultApplication;
  }

  public Application getApplication(String appName) {

    //in case of Platform, fall to default
    Application app = null;
    if (appName != null && !"Platform".equalsIgnoreCase(appName))
      app = applications.get(appName);
    if (app != null)
      return app;
    else
      return getDefaultApplication();
  }




}
