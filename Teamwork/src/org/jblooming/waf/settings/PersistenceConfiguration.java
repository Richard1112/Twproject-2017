package org.jblooming.waf.settings;

import org.hibernate.boot.registry.StandardServiceRegistryBuilder;
import org.hibernate.c3p0.internal.C3P0ConnectionProvider;
import org.hibernate.cfg.NamingStrategy;
import org.hibernate.cfg.Environment;
import org.hibernate.dialect.Dialect;
import org.hibernate.ConnectionReleaseMode;
import org.hibernate.SessionFactory;
import org.hibernate.cache.ehcache.EhCacheRegionFactory;
import org.jblooming.persistence.hibernate.PlatformAnnotationConfiguration;
import org.jblooming.persistence.hibernate.OLNamingStrategy;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.persistence.ThreadLocalPersistenceContextCarrier;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.tracer.Tracer;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.configuration.LoaderSupport;
import org.apache.log4j.Logger;

import java.util.Properties;
import java.util.LinkedHashMap;


/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 8-lug-2008 : 12.54.02
 */
public class PersistenceConfiguration {

  public String name;

  public String driver_class;
  public String driver_url;
  public Class dialect;
  public String db_user_name;
  public String db_user_psw;
  public String dataSource;
  public boolean usePool = false;
  public boolean useEHCache = false;
  public boolean useHibStats = false;
  public String poolAlias;
  public PlatformConfiguration.StringConversionType stringPrimitiveFieldsConversion = PlatformConfiguration.StringConversionType.NONE;
  public PlatformConfiguration.StringConversionType searchStringParametersConversion = PlatformConfiguration.StringConversionType.NONE;

  Properties hibInfo = new Properties();

  public boolean loaded = false;
  public boolean hibernateLoaded;


  /**
   * This should be specified as tablespace name in oracle when the user has sufficient rights to access different schemas
   */
  public String schemaName;

  public String namingStrategy;

  private PlatformAnnotationConfiguration hibernateConfiguration;
  private SessionFactory sessionFactory;


  public static LinkedHashMap<String, PersistenceConfiguration> persistenceConfigurations = new LinkedHashMap();


  public static String POOL_ACQUIRE_INCREMENT = "2";
  public static String POOL_IDLE_TEST_PERIOD = "100";
  public static String POOL_MAX_STATEMENTS = "0";
  public static String POOL_MIN_SIZE = "1";

  public static String POOL_MAX_SIZE = "30";  //poolMaxSize on config
  public static String POOL_TIMEOUT = "800"; //poolTimeout in seconds on config

  public PersistenceConfiguration(String name) {
    this.name = name;
    hibernateConfiguration = new PlatformAnnotationConfiguration(this);
    poolAlias = "pool." + name;
  }


  public static PersistenceConfiguration getInstance(String persistenceConfigurationName, Properties p) {

    String oscvt = getSafelyTrimmedPropertyValue("onSaveConvertValuesTo", p);
    PersistenceConfiguration persistenceConfiguration = new PersistenceConfiguration(persistenceConfigurationName);
    if (PlatformConfiguration.StringConversionType.LOWER.toString().equalsIgnoreCase(oscvt)) {
      persistenceConfiguration.stringPrimitiveFieldsConversion = PlatformConfiguration.StringConversionType.LOWER;
    } else if (PlatformConfiguration.StringConversionType.UPPER.toString().equalsIgnoreCase(oscvt)) {
      persistenceConfiguration.stringPrimitiveFieldsConversion = PlatformConfiguration.StringConversionType.UPPER;
    } else
      persistenceConfiguration.stringPrimitiveFieldsConversion = PlatformConfiguration.StringConversionType.NONE;

    String osearchcvt = getSafelyTrimmedPropertyValue("onSearchConvertValuesTo", p);
    if (PlatformConfiguration.StringConversionType.LOWER.toString().equalsIgnoreCase(osearchcvt)) {
      persistenceConfiguration.searchStringParametersConversion = PlatformConfiguration.StringConversionType.LOWER;
    } else if (PlatformConfiguration.StringConversionType.UPPER.toString().equalsIgnoreCase(osearchcvt)) {
      persistenceConfiguration.searchStringParametersConversion = PlatformConfiguration.StringConversionType.UPPER;
    } else
      persistenceConfiguration.searchStringParametersConversion = PlatformConfiguration.StringConversionType.NONE;

    boolean encriptDbAccount = Fields.TRUE.equalsIgnoreCase(getSafelyTrimmedPropertyValue("encriptDbAccount", p));

    String datasource = getSafelyTrimmedPropertyValue("dataSource", p);
    if (JSP.ex(datasource))
      persistenceConfiguration.dataSource = (!datasource.startsWith("java:comp/env/") ? "java:/comp/env/" : "") + datasource;

    persistenceConfiguration.driver_class = getSafelyTrimmedPropertyValue("driver", p);
    persistenceConfiguration.driver_url = getSafelyTrimmedPropertyValue("url", p);
    try {
      persistenceConfiguration.dialect = Class.forName(getSafelyTrimmedPropertyValue("dialect", p));
    } catch (ClassNotFoundException e) {
      throw new PlatformRuntimeException(e);
    }

    String user = getSafelyTrimmedPropertyValue("user", p);
    if (encriptDbAccount)
      user = StringUtilities.decrypt(user);
    persistenceConfiguration.db_user_name = user;

    String password = getSafelyTrimmedPropertyValue("password", p);
    if (encriptDbAccount)
      password = StringUtilities.decrypt(password);
    persistenceConfiguration.db_user_psw = password;

    String schemaName = getSafelyTrimmedPropertyValue("schemaName", p);
    if (JSP.ex(schemaName))
      persistenceConfiguration.schemaName = schemaName;

    persistenceConfiguration.namingStrategy = getSafelyTrimmedPropertyValue("namingStrategy", p);

    String value = getSafelyTrimmedPropertyValue("poolMaxSize", p);
    if (JSP.ex(value))
      PersistenceConfiguration.POOL_MAX_SIZE = value;

    String c3p0timeout = getSafelyTrimmedPropertyValue("poolTimeout", p);
    if (JSP.ex(c3p0timeout))
      PersistenceConfiguration.POOL_TIMEOUT = c3p0timeout;


    persistenceConfiguration.usePool = Fields.TRUE.equalsIgnoreCase(getSafelyTrimmedPropertyValue("usePool", p)) || Fields.TRUE.equalsIgnoreCase(getSafelyTrimmedPropertyValue("useProxool", p)) ||  Fields.TRUE.equalsIgnoreCase(getSafelyTrimmedPropertyValue("useC3P0", p));

    persistenceConfiguration.useEHCache = Fields.TRUE.equalsIgnoreCase(getSafelyTrimmedPropertyValue("useEHCache", p));
    persistenceConfiguration.useHibStats = Fields.TRUE.equalsIgnoreCase(getSafelyTrimmedPropertyValue("useHibStats", p));

    NamingStrategy ns = new OLNamingStrategy();
    if (JSP.ex(persistenceConfiguration.namingStrategy)) {
      try {
        ns = (NamingStrategy) (Class.forName(persistenceConfiguration.namingStrategy.trim()).newInstance());
      } catch (Exception e) {
        throw new PlatformRuntimeException(e);
      }
    }

    persistenceConfiguration.hibernateConfiguration.setNamingStrategy(ns);

    persistenceConfiguration.configPoolAndHibernateProperties();

    PersistenceConfiguration.persistenceConfigurations.put(persistenceConfigurationName, persistenceConfiguration);

    return persistenceConfiguration;

  }

  private void configPoolAndHibernateProperties() {

    if (usePool) {
      configPool();
    } else {
      configNoPool();
    }

    configHibernateProperties();
  }


  private void configNoPool() {
    if (JSP.ex(dataSource)) {
      hibInfo.setProperty(Environment.DATASOURCE, dataSource);
    } else {
      hibInfo.setProperty(Environment.DRIVER, driver_class);
      hibInfo.setProperty(Environment.URL, driver_url);
      hibInfo.setProperty(Environment.USER, db_user_name);
      hibInfo.setProperty(Environment.PASS, db_user_psw);
    }

    //Nonostante sia altamente sconsigliato in caso di spegnimento di c3p0
    // entra in gioco il pooling di hibernate che con questi valori dovrebbe funzionare & good luck!
    hibInfo.setProperty(Environment.POOL_SIZE, "0"); //
    hibInfo.setProperty("com.sun.jndi.ldap.connect.pool.timeout", "800");
    Tracer.platformLogger.info("open lab platform - not using connection pooling");
  }


  private void configPool() {

      if (JSP.ex(dataSource)) {
        hibInfo.setProperty(Environment.DATASOURCE, dataSource);

      } else {
        //com.mchange.v2.log.MLog
        hibInfo.setProperty(Environment.DRIVER, driver_class);
        hibInfo.setProperty(Environment.URL, driver_url);
        hibInfo.setProperty(Environment.USER, db_user_name);
        hibInfo.setProperty(Environment.PASS, db_user_psw);
      }

      hibInfo.setProperty(Environment.C3P0_ACQUIRE_INCREMENT, POOL_ACQUIRE_INCREMENT);
      hibInfo.setProperty(Environment.C3P0_IDLE_TEST_PERIOD, POOL_IDLE_TEST_PERIOD);
      hibInfo.setProperty(Environment.C3P0_MAX_STATEMENTS, POOL_MAX_STATEMENTS);
      hibInfo.setProperty(Environment.C3P0_MIN_SIZE, POOL_MIN_SIZE);

      hibInfo.setProperty(Environment.C3P0_MAX_SIZE, POOL_MAX_SIZE);
      hibInfo.setProperty(Environment.C3P0_TIMEOUT, POOL_TIMEOUT);
     
      Logger c3p0Logger = Logger.getLogger("com.mchange.v2");
      c3p0Logger.setLevel(Tracer.hibernateLogger.getLevel());
      LoaderSupport.createLogger(PlatformConfiguration.logOnConsole, PlatformConfiguration.logOnFile, PlatformConfiguration.logPattern, PlatformConfiguration.logFilesRoot, c3p0Logger, "c3p0ConsoleAppender", "c3p0.log", "c3p0FileAppender");
      Tracer.platformLogger.info("open lab platform - using C3P0 connection pooling");

  }

  public void configHibernateProperties() {

    try {
      if (!(dialect.newInstance() instanceof Dialect))
        throw new PlatformRuntimeException("Must configure dialect with a class extending org.hibernate.dialect.Dialect");
    } catch (InstantiationException e) {
      throw new RuntimeException(e);
    } catch (IllegalAccessException e) {
      throw new RuntimeException(e);
    }

    //hibernate
    // hibInfo.setProperty(Environment.QUERY_SUBSTITUTIONS, "true 'Y', false 'N'"); // this is dialect sensitive
    hibInfo.setProperty(Environment.QUERY_SUBSTITUTIONS, "true 1, false 0"); // this is dialect sensitive

    hibInfo.setProperty(Environment.DIALECT, dialect.getName());
   // hibInfo.setProperty(Environment.SHOW_SQL, "true");
    hibInfo.setProperty(Environment.MAX_FETCH_DEPTH, "2");
    hibInfo.setProperty(Environment.USE_STREAMS_FOR_BINARY, "true");
    hibInfo.setProperty(Environment.STATEMENT_BATCH_SIZE, "15");

    //statistics
    if (useHibStats)
      hibInfo.setProperty(Environment.GENERATE_STATISTICS, "true");


    if (usePool) {
      hibInfo.setProperty(Environment.CONNECTION_PROVIDER, C3P0ConnectionProvider.class.getName());
      hibInfo.setProperty(Environment.RELEASE_CONNECTIONS, ConnectionReleaseMode.ON_CLOSE.toString());
    }

    if (useEHCache) {
      hibInfo.setProperty(Environment.CACHE_REGION_FACTORY, EhCacheRegionFactory.class.getName());
      hibInfo.setProperty(Environment.USE_QUERY_CACHE, "true");
      Tracer.platformLogger.info("open lab platform - using EHCache");
    } else {
      hibInfo.setProperty(Environment.USE_SECOND_LEVEL_CACHE, "false");
      hibInfo.setProperty(Environment.USE_QUERY_CACHE, "false");
    }

    hibInfo.setProperty(Environment.USE_REFLECTION_OPTIMIZER, (ApplicationState.platformConfiguration.development ? "false" : "true"));

    hibInfo.setProperty(Environment.USE_SCROLLABLE_RESULTSET, "true");

    //no connection isolation with hsqldb
    if (dialect.getName().indexOf("HSQL") == -1)
      hibInfo.setProperty(Environment.ISOLATION, "2");

    hibernateConfiguration.addProperties(hibInfo);

  }

  public void configAndBuildHibSessionFactory() {
    if (!loaded) {
      try {
        sessionFactory = hibernateConfiguration.buildSessionFactory(new StandardServiceRegistryBuilder().applySettings(hibInfo).build());
      } catch (Throwable throwable) {
        throwable.printStackTrace();
        throw new RuntimeException(throwable);
      }
      //restart case
    } else
      sessionFactory.close();

    hibernateLoaded = true;
    Tracer.platformLogger.info("open lab platform - config:" + name + " hibernate factory ok");
  }


  private static String getSafelyTrimmedPropertyValue(String name, Properties p) {
    String s = p.getProperty(name);
    return s != null ? s.trim() : null;
  }

  /**
   * @return first loaded PersistenceConfiguration
   */
  public static PersistenceConfiguration getFirstPersistenceConfiguration() {
    return PersistenceConfiguration.persistenceConfigurations.values().iterator().next();
  }

  /**
   * @return PersistenceConfiguration in threadLocal if existing else first one
   */
  public static PersistenceConfiguration getDefaultPersistenceConfiguration() {
    ThreadLocalPersistenceContextCarrier carrier = PersistenceContext.threadLocalPersistenceContextCarrier.get();
    if (carrier != null && carrier.currentPC != null)
      return carrier.currentPC.persistenceConfiguration;
    else
      return getFirstPersistenceConfiguration();
  }

  public PlatformAnnotationConfiguration getHibernateConfiguration() {
    return hibernateConfiguration;
  }

  public SessionFactory getSessionFactory() {
    return sessionFactory;
  }


  public static PersistenceConfiguration getInstance(Class<? extends IdentifiableSupport> persistentClass) {
    PersistenceConfiguration conf = null;
    for (String confName : PersistenceConfiguration.persistenceConfigurations.keySet()) {
      PersistenceConfiguration pcf = PersistenceConfiguration.persistenceConfigurations.get(confName);
      if (pcf.getHibernateConfiguration().getClassMapping(persistentClass.getName()) != null) {
        conf = pcf;
        break;
      }
    }
    return conf;
  }

}