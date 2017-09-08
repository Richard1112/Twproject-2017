package org.jblooming.waf.configuration;

import com.opnlb.fulltext.IndexingMachine;
import org.apache.log4j.*;
import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.logging.Sniffer;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.operator.Operator;
import org.jblooming.oql.OqlQuery;
import org.jblooming.oql.QueryHelper;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.hibernate.HibernateFactory;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.scheduler.PlatformExecutionService;
import org.jblooming.scheduler.Scheduler;
import org.jblooming.security.PermissionCache;
import org.jblooming.system.SystemConstants;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.*;
import org.jblooming.utilities.file.FileUtilities;
import org.jblooming.waf.PlatformSettings;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.constants.OperatorConstants;
import org.jblooming.waf.settings.*;
import org.jblooming.waf.settings.businessLogic.I18nAction;
import org.jblooming.waf.view.ClientEntries;
import org.jblooming.waf.view.ClientEntry;

import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.jsp.PageContext;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.sql.Connection;
import java.util.*;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Jun 27, 2007
 * Time: 2:55:54 PM
 */
public abstract class LoaderSupport implements ServletContextListener {

  public static LoaderSupport myself;

  public static String configFiles;
  public static Class<? extends LoaderSupport> implementor;


  public void contextInitialized(ServletContextEvent event) {

    myself = this;
    implementor = this.getClass();
    configFiles = event.getServletContext().getInitParameter("configFile");
    start(event.getServletContext());
  }

  public void start(ServletContext servletContext) {

    //sample for audit
    Sniffer.addInauditedProperty("lastModified");

    if (!JSP.ex(configFiles))
      configFiles = "config.properties";

    List<String> configs = StringUtilities.splitToList(configFiles, ",");

    boolean isFirstThatWins = true;

    ApplicationState.contextPath = servletContext.getContextPath();


    String root = servletContext.getRealPath("/");
    if (root.endsWith(File.separator))
      root = root.substring(0, root.length() - 1);
    ApplicationState.webAppFileSystemRootPath = root;

    for (String configFile : configs) {
      String path = root + File.separator + "WEB-INF" + File.separator + configFile;

      if (!new File(path).exists()) {
        String notEx = "open lab platform - global catastrophe - the " + configFile + " not found in " + root + "WEB-INF" + File.separator;
        Tracer.desperatelyLog(notEx, true, null);
      }

      Properties p = new Properties();
      try {
        p.load(new FileInputStream(path));
      } catch (IOException e) {
        p = null;
      }

      if (p == null)
        Tracer.desperatelyLog("open lab platform - global catastrophe - properties unreadable from " + path, true, null);

      if (p.keySet().size() == 0)
        Tracer.desperatelyLog("open lab platform - global catastrophe - no properties in " + path, true, null);

      if (isFirstThatWins) {
        String psw = getSafelyTrimmedPropertyValue("adminPassword", p);
        if (JSP.ex(psw))
          PlatformConfiguration.psw = psw;

        //system time zone overwrite
        if (JSP.ex(getSafelyTrimmedPropertyValue("SERVER_TIME_ZONE", p))){
          ApplicationState.SYSTEM_TIME_ZONE=TimeZone.getTimeZone(getSafelyTrimmedPropertyValue("SERVER_TIME_ZONE", p));
          TimeZone.setDefault(ApplicationState.SYSTEM_TIME_ZONE);
        }

        String logPropetiesPath = root + File.separator + "WEB-INF" + File.separator + PlatformConfiguration.logSettingsFileName;

        if (new File(logPropetiesPath).exists()) {
          // se esiste il file o la propetie logProperties salva su file e usa il file
          // se non fa come prima
          Properties pL = new Properties();
          try {
            pL.load(new FileInputStream(logPropetiesPath));
            configLog4J(pL);
          } catch (IOException e) {
            configLog4J(p);
          }
        } else
          configLog4J(p);

        // get global properties name. Last one is the correct one
        if (JSP.ex(getSafelyTrimmedPropertyValue("globalProperties", p)))
          PlatformConfiguration.globalSettingsFileName = getSafelyTrimmedPropertyValue("globalProperties", p);

        isFirstThatWins = false;
      }

      ApplicationState.platformConfiguration.development = Fields.TRUE.equalsIgnoreCase(getSafelyTrimmedPropertyValue("development", p));

      String connectionName = configFile.substring(0, configFile.lastIndexOf('.'));

      // creates PersistenceConfiguration
      PersistenceConfiguration.getInstance(connectionName, p);

    }

    // add Platform as mandatory application
    ApplicationState.platformConfiguration.addApplication(new PlatformSettings());

    configApplications();

    configGlobalSettings();

    //configSkins();

    //verify correct configuration of platform base app
    Map<String, Application> applications = ApplicationState.platformConfiguration.applications;

    Application platform = applications.get(Application.PLATFORM_APP_NAME);
    if (platform == null)
      throw new PlatformRuntimeException("Fatal error in " + this.getClass().getSimpleName() + ": platform base application not found in PlatformConfiguration.applications");

    //first launch base classes
    launch(platform, null);

    //verify correct configuration of at least one web app
    if (applications.keySet().size() < 2)
      throw new PlatformRuntimeException("Fatal error in " + this.getClass().getSimpleName() + ": no application found in PlatformConfiguration.applications other than platform base");

    //launch single web apps
    for (Iterator iterator = new HashSet(applications.keySet()).iterator(); iterator.hasNext();) {
      final String key = (String) iterator.next();
      if (!Application.PLATFORM_APP_NAME.equals(key)) {
        Application application = applications.get(key);
        launch(application, key);
      }
    }

    // applications loaded

    //load Hibernate Session Factories
    for (PersistenceConfiguration pconf : PersistenceConfiguration.persistenceConfigurations.values()) {
      pconf.configAndBuildHibSessionFactory();
    }

    // some database do not supports something
    String dialectClassName = PersistenceConfiguration.getDefaultPersistenceConfiguration().dialect.getName();
    if(dialectClassName.contains("SQLServer")){
      QueryHelper.CLOB_SUPPORTS_UPPER=false;
    }

    afterPersistenceSetup();

    PersistenceContext pc = null;
    try {
      //create a default session for this thread
      pc = PersistenceContext.get(Operator.class);

      createDefaultOperators();
      Tracer.platformLogger.info("open lab platform - persistence settings loaded ok");


      // load I18n for all apps eventually from db!!!!
      new I18nAction().cmdReload();

      //needs i18n loaded
      applySystemSettings();

      if (ApplicationState.platformConfiguration.development)
        Tracer.platformLogger.warn("open lab platform - LOADED SETTINGS FOR DEVELOPMENT ENVIRONMENT");

      if (ApplicationState.platformConfiguration.defaultApplication == null)
        Tracer.platformLogger.warn("open lab platform\nCould not find find a default application: add\n  ApplicationState.platformConfiguration.defaultApplication = ...\nin your settings.jsp.");
      else
        ApplicationState.loaded = true;

      pc.commitAndClose();

    } catch (Throwable e) {

      if (pc != null)
        pc.rollbackAndClose();

      Tracer.platformLogger.fatal("open lab platform - global catastrophe - init of loader servlet failed: " + e.getMessage(), e);
    }
  }


  //public void destroy() {

  public void contextDestroyed(ServletContextEvent event) {


    Tracer.platformLogger.info("---------------------------DESTROY START NEW----------------------------");
    try {

      // stop all jobs
      if (Scheduler.isRunning()) {
        Scheduler.getInstance().stop();
      }

      // stop indexing
      if (IndexingMachine.isRunning())
        IndexingMachine.stop();


      //launch single web apps
      for (Application application : ApplicationState.platformConfiguration.applications.values()) {
        application.applicationDestroy();
      }


      if (PlatformExecutionService.executorService != null)
        try {
          PlatformExecutionService.executorService.shutdown();
        } catch (Throwable e) {
          Tracer.platformLogger.error(e);
        }

      // test hsql
      String dialectClassName = PersistenceConfiguration.getDefaultPersistenceConfiguration().dialect.getName();
      if (dialectClassName.startsWith("org.hibernate.dialect.HSQL")) {
        PersistenceContext pc = null;
        try {
          pc = new PersistenceContext();
          Connection c = pc.session.connection();
          c.createStatement().execute("SHUTDOWN");
          Tracer.platformLogger.debug("HSQLDB SHUTDOWN.");
          pc.commitAndClose(); //chissaseriesciràmaiafarlogirare
        } catch (Throwable e) {
          Tracer.platformLogger.error(e);
          if (pc != null) {
            pc.rollbackAndClose();
          }
        }
      }

      Tracer.platformLogger.info("---------------------------DESTROY END NEW----------------------------");
    } catch (Throwable e) {
      Tracer.platformLogger.error(e);
    }

    //super.destroy();
  }


  public void afterPersistenceSetup() {

    Application platform = ApplicationState.platformConfiguration.applications.get(Application.PLATFORM_APP_NAME);
    afterPersistenceSetupForApp(platform);
    for (Application application : ApplicationState.platformConfiguration.applications.values()) {
      if (!Application.PLATFORM_APP_NAME.equals(application.getName()))
        afterPersistenceSetupForApp(application);
    }
  }

  private void afterPersistenceSetupForApp(Application application) {
    try {
      application.configureNeedingPersistence(ApplicationState.platformConfiguration);
      Tracer.platformLogger.info("open lab platform - " + application.getName().toLowerCase() + " settings after persistence loaded ok");
    } catch (Exception e) {
      Tracer.platformLogger.error("application " + application.getName() + " removed as it failed post persistence setup");
      e.printStackTrace();
    }
  }

  protected abstract void configApplications();


  protected Level getLogLevel(String propName, Properties p, Level defaultLevel) {
    String logLevelStr = getSafelyTrimmedPropertyValue(propName, p);
    Level logLevel = defaultLevel;
    if (JSP.ex(logLevelStr)) {
      if ("FATAL".equalsIgnoreCase(logLevelStr)) {
        logLevel = Level.FATAL;
      } else if ("ERROR".equalsIgnoreCase(logLevelStr)) {
        logLevel = Level.ERROR;
      } else if ("INFO".equalsIgnoreCase(logLevelStr)) {
        logLevel = Level.INFO;
      } else if ("WARN".equalsIgnoreCase(logLevelStr)) {
        logLevel = Level.WARN;
      } else if ("DEBUG".equalsIgnoreCase(logLevelStr)) {
        logLevel = Level.DEBUG;
      }
    }
    return logLevel;
  }

  protected void configLog4J(Properties p) {

    PlatformConfiguration.logOnConsole = Fields.TRUE.equalsIgnoreCase(getSafelyTrimmedPropertyValue("logOnConsole", p));
    PlatformConfiguration.logOnFile = !Fields.FALSE.equalsIgnoreCase(getSafelyTrimmedPropertyValue("logOnFile", p));
    String logFilesRoot = getSafelyTrimmedPropertyValue("logFilesRoot", p);
    PlatformConfiguration.logFilesRoot = JSP.ex(logFilesRoot) ? logFilesRoot + (logFilesRoot.endsWith(File.separator) ? "" : File.separator) : ApplicationState.webAppFileSystemRootPath + File.separator + "WEB-INF/log/";
    //PlatformConfiguration.logPattern = "%d{yyyy MMM dd HH:mm:ss} %5p %c{1}:%L - %m%n";
    PlatformConfiguration.logPattern= "%d{yyyy-MM-dd HH:mm:ss-SSS} %-7c %-p %X{loginName}@%X{clientIp} %C{1}:%L  %t %n%m%n";
    String unNamedLogPattern= "%d{yyyy-MM-dd HH:mm:ss-SSS} %-7c %-p %C{1}:%L  %t %n%m%n";


    Tracer.hibernateLogger.setLevel(getLogLevel("hibernateLogLevel", p, Level.ERROR));
    Tracer.platformLogger.setLevel(getLogLevel("platformLogLevel", p, Level.WARN));
    Tracer.jobLogger.setLevel(getLogLevel("jobLogLevel", p, Level.ERROR));
    Tracer.i18nLogger.setLevel(getLogLevel("i18nLogLevel", p, Level.ERROR));
    Tracer.emailLogger.setLevel(getLogLevel("emailLogLevel", p, Level.ERROR));

    createLogger(PlatformConfiguration.logOnConsole, PlatformConfiguration.logOnFile, PlatformConfiguration.logPattern, PlatformConfiguration.logFilesRoot, Tracer.hibernateLogger, "hibConsoleAppender", PlatformConfiguration.hibernateLogFileName, "hibFileAppender");
    createLogger(PlatformConfiguration.logOnConsole, PlatformConfiguration.logOnFile, PlatformConfiguration.logPattern, PlatformConfiguration.logFilesRoot, Tracer.platformLogger, "platformConsoleAppender", PlatformConfiguration.platformLogFileName, "plaFileAppender");
    createLogger(PlatformConfiguration.logOnConsole, PlatformConfiguration.logOnFile, unNamedLogPattern, PlatformConfiguration.logFilesRoot, Tracer.jobLogger, "jobLoggerConsoleAppender", PlatformConfiguration.jobLoggerLogFileName, "jobLoggerFileAppender");
    createLogger(PlatformConfiguration.logOnConsole, PlatformConfiguration.logOnFile, unNamedLogPattern, PlatformConfiguration.logFilesRoot, Tracer.i18nLogger, "i18nConsoleAppender", PlatformConfiguration.i18nLogFileName, "i18nFileAppender");
    createLogger(PlatformConfiguration.logOnConsole, PlatformConfiguration.logOnFile,unNamedLogPattern, PlatformConfiguration.logFilesRoot, Tracer.emailLogger, "emailConsoleAppender", PlatformConfiguration.emailLogFileName, "emailFileAppender");

    //avoid duplications
    if (Logger.getRootLogger() != null){
      Logger.getRootLogger().setLevel(Level.OFF);
      Logger.getRootLogger().removeAllAppenders();
    }
  }

  private String getSafelyTrimmedPropertyValue(String name, Properties p) {
    String s = p.getProperty(name);
    return s != null ? s.trim() : null;
  }

  private void launch(Application application, final String key) {

    try {
      application.configureFreeAccess(ApplicationState.platformConfiguration);
      application.configurePersistence(ApplicationState.platformConfiguration);
      //I18n.loadI18n(application); moved to LoaderSupport.start
      ApplicationState.getPermissions().addAll(application.getPermissions());
      Tracer.platformLogger.info("open lab platform - " + application.getName().toLowerCase() + " settings before persistence loaded ok");

    } catch (Exception e) {
      if (key != null)
        ApplicationState.platformConfiguration.applications.remove(key);

      Tracer.platformLogger.error("open lab platform - " + "application " + application.getName().toLowerCase() + " removed as it failed persistence setup");
      e.printStackTrace();
    }
  }


  public void createDefaultOperator(PersistenceContext pc) throws PersistenceException, ApplicationException {

    OqlQuery q = new OqlQuery("from " + Operator.class.getName() + " as op where op.administrator = :admin ", pc);
    q.getQuery().setBoolean("admin", true);
    q.getQuery().setMaxResults(1);

    List opL = q.list();

    if (opL == null || opL.size() == 0) {
      Operator administrator = null;
      try {
        administrator = (Operator) PlatformConfiguration.defaultOperatorSubclass.newInstance();
      } catch (Exception e) {
        throw new PlatformRuntimeException(e);
      }

      administrator.setAdministrator(true);
      administrator.setEnabled(true);
      administrator.setLoginName("administrator");
      administrator.changePassword("");
      administrator.setName("System");
      administrator.setSurname("Manager");
      administrator.store(pc);
    }
  }

  public void createSystemOperator(PersistenceContext pc) throws PersistenceException, ApplicationException {
    OqlQuery q = new OqlQuery("from " + Operator.class.getName() + " as op where op.loginName = :system ", pc);
    q.getQuery().setString("system", "system");
    q.getQuery().setMaxResults(1);


    List opL = q.list();


    if (opL == null || opL.size() == 0) {
      Operator system = null;
      try {
        system = (Operator) PlatformConfiguration.defaultOperatorSubclass.newInstance();
      } catch (Exception e) {
        throw new PlatformRuntimeException(e);
      }
      system.setAdministrator(true);
      system.setLoginName("system");

      /*STANISLAO MOULINSKY
      Acerrimo nemico di Carter, finto barone spagnolo di origine russa,
      è l'asso dei travestimenti: quando si traveste il suo corpo cambia totalmente.
      Si è travestito praticamente da tutto: Contessa spagnola, Uomo delle fogne, armadio,
      Ricetrasmittente da guerra, Angelo viola, maggiordomo, cadavere di banchiere...
      Frase celebre:
      "Ebbene sì, maledetto Carter, hai vinto anche stavolta!"*/
      system.changePassword("stanislaomoulinsky");
      system.setName("System");
      system.setSurname("Operator");
      system.setEnabled(false);

      system.store(pc);
      Tracer.platformLogger.info("open lab platform - created default system operator");
    }
  }


  public static void createLogger(boolean logOnConsole, boolean logOnFile, String pattern, String logFilesRoot, Logger logger, String consoleAppenderName, String logFileName, String fileAppenderName) {
    logger.removeAllAppenders();

    PatternLayout pl = new PatternLayout();
    pl.setConversionPattern(pattern);

    if (logOnConsole) {

      ConsoleAppender consoleAppender = new ConsoleAppender(pl);
      consoleAppender.setName(consoleAppenderName);
      logger.addAppender(consoleAppender);
    }

    if (logOnFile) {
      File log = new File(logFilesRoot + logFileName);
      if (!log.exists())
        log.getParentFile().mkdirs();
      RollingFileAppender fileAppender = null;
      try {
        fileAppender = new RollingFileAppender(pl, log.getPath());
        fileAppender.setMaxFileSize("1MB");
        fileAppender.setMaxBackupIndex(5);

      } catch (IOException e) {
        throw new RuntimeException(e);
      }
      fileAppender.setName(fileAppenderName);
      logger.addAppender(fileAppender);
    }
  }


  public void createDefaultOperators() {
    PersistenceContext pc = null;

    try {

      pc = HibernateFactory.newFreeSession();
      createDefaultOperator(pc);
      createSystemOperator(pc);
      Tracer.platformLogger.info("open lab platform - default operator management loaded ok");

    } catch (Exception e) {
      Tracer.platformLogger.error("LoaderSupport.createDefaultOperators failed: platform setup exception", e);
      Tracer.hibernateLogger.fatal("Database access exception", e);
      //ApplicationState.hibernateLoaded = false;
    } finally {
      try {
        pc.commitAndClose();
      } catch (PersistenceException pe) {
        Tracer.platformLogger.error("LoaderSupport.createDefaultOperators failure closing session: platform setup exception", pe);
      }
    }
  }


  public void configGlobalSettings() {
    String globalPath = ApplicationState.webAppFileSystemRootPath + File.separator + "commons" + File.separator + "settings" + File.separator + PlatformConfiguration.globalSettingsFileName;
    File global = new File(globalPath);
    Properties p = null;
    if (!global.exists()) {
      p = fillApplication(globalPath);
    } else {
      p = FileUtilities.getProperties(globalPath);
    }
    for (Object name : p.keySet()) {
      ApplicationState.applicationSettings.put((String) name, (String) p.get(name));
    }


    if (!JSP.ex(ApplicationState.serverURL)) {
      HttpUtilities.serverURL(null); // this uses PUBLIC_SERVER_NAME if exists
    }


  }

  public Properties fillApplication(String global) {
    Properties prop = new Properties();

    /**
     *      layout
     */

    prop.put(OperatorConstants.FLD_CURRENT_SKIN, "navy");

    /**
     * general
     */

    prop.put(SystemConstants.FLD_PASSWORD_EXPIRY, "0");
    prop.put(SystemConstants.FLD_PASSWORD_MIN_LEN, "0");
    prop.put(SystemConstants.FLD_SERVER_TIME_ZONE, ApplicationState.SYSTEM_TIME_ZONE.getID());
    prop.put(SystemConstants.AUDIT, Fields.FALSE);
    prop.put(SystemConstants.FLD_INDEX_EVERY_HOUR, "0");
    prop.put(SystemConstants.FLD_MAX_FILE_SIZE, "500");
    prop.put(SystemConstants.STORAGE_PATH_ALLOWED, "");
    File rep = new File(ApplicationState.webAppFileSystemRootPath + File.separator + "/WEB-INF/data");
    rep.mkdirs();
    String repUrl = rep.getAbsolutePath();
    prop.put(SystemConstants.FLD_REPOSITORY_URL, repUrl);

    /**
     *      behaviour
     */

    prop.put(OperatorConstants.FLD_SELECT_LANG, ApplicationState.SYSTEM_LOCALE.toString()); //qui si deve passare il codice a quattro cifre
    prop.put(OperatorConstants.FLD_LOGIN_COOKIES, Fields.FALSE);
    prop.put(OperatorConstants.FLD_WORKING_HOUR_TOTAL, "08:00");
    prop.put(OperatorConstants.FLD_WORKING_HOUR_BEGIN, "09:00");
    prop.put(OperatorConstants.FLD_WORKING_HOUR_END, "18:00");

    FileUtilities.savePropertiesInUTF8(prop, global);

    Tracer.platformLogger.info("open lab platform - created default global settings file: " + global);
    return prop;
  }


  public static void withPageContextSetup(PageContext pageContext) {

    for (Application application : ApplicationState.platformConfiguration.applications.values()) {
      try {
        application.configureNeedingPageContext(pageContext);
        Tracer.platformLogger.info("open lab platform - " + application.getName().toLowerCase() + " settings with PageContext loaded ok");
      } catch (Exception e) {
        Tracer.platformLogger.error("Application " + application.getName() + " removed as it failed settings with PageContext setup");
        e.printStackTrace();
      }
    }
    ApplicationState.platformConfiguration.loadedWithPageContextSettings = true;
  }

  public static void applySystemSettings() {

    ApplicationState.setLocale();
    CompanyCalendar.setup();

    String fld_wbd = ApplicationState.getApplicationSetting(OperatorConstants.FLD_WORKING_HOUR_BEGIN);

    if (fld_wbd != null) {
      int hour_begin = (int) DateUtilities.millisFromHourMinuteSmart(ApplicationState.getApplicationSetting(OperatorConstants.FLD_WORKING_HOUR_BEGIN));
      if (hour_begin > 0) {
        CompanyCalendar.WORKING_HOUR_BEGIN = hour_begin;
      }
    } else
      Tracer.platformLogger.info(OperatorConstants.FLD_WORKING_HOUR_BEGIN + " not found in global properties");

    // set default currency format
    String currFormat = ApplicationState.getApplicationSetting("CURRENCY_FORMAT");
    if (JSP.ex(currFormat))
      NumberUtilities.DEFAULT_CURRENCY_FORMAT = currFormat;


    String fld_wed = ApplicationState.getApplicationSetting(OperatorConstants.FLD_WORKING_HOUR_END);
    if (fld_wed != null) {
      int hour_end = (int) DateUtilities.millisFromHourMinuteSmart(ApplicationState.getApplicationSetting(OperatorConstants.FLD_WORKING_HOUR_END));
      if (hour_end > 0) {
        CompanyCalendar.WORKING_HOUR_END = hour_end;
      }
    } else
      Tracer.platformLogger.info(OperatorConstants.FLD_WORKING_HOUR_END + " not found in global properties");

    DateUtilities.DAY_SHORT_CODE = I18n.get("DAY_SHORT_CODE");

    //set TimeZone
    if (JSP.ex(ApplicationState.getApplicationSetting("SERVER_TIME_ZONE"))){
      ApplicationState.SYSTEM_TIME_ZONE=TimeZone.getTimeZone(ApplicationState.getApplicationSetting("SERVER_TIME_ZONE"));
      TimeZone.setDefault(ApplicationState.SYSTEM_TIME_ZONE);
    }


    //default file storage type
    String fsType = ApplicationState.getApplicationSetting("DEFAULT_STORAGE_TYPE");
    if (JSP.ex(fsType)) {
      PersistentFile.DEFAULT_STORAGE_TYPE=fsType;
    }


    //permission cache control
    PermissionCache.MAX_AGE= new ClientEntry("d",ApplicationState.getApplicationSetting("PERMISSION_CACHE_MAXAGE", "30000")).longValueNoErrorNoCatchedExc();
    PermissionCache.MAX_AGE=PermissionCache.MAX_AGE==0?30000:PermissionCache.MAX_AGE;
    PermissionCache.USECACHE=Fields.TRUE.equals(ApplicationState.getApplicationSetting("PERMISSION_CACHE_ENABLED", Fields.TRUE));


  }
}
