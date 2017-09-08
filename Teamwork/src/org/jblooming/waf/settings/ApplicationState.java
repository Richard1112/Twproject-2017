package org.jblooming.waf.settings;

import com.opnlb.fulltext.IndexingConstants;
import org.jblooming.security.Permission;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.*;
import org.jblooming.utilities.file.FileUtilities;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.DefaultCommandController;
import org.jblooming.waf.EntityViewerBricks;
import org.jblooming.waf.configuration.LoaderSupport;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.constants.OperatorConstants;
import org.jblooming.waf.html.core.UrlComposer;
import org.jblooming.waf.view.ClientEntry;

import javax.servlet.http.HttpServletRequest;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.text.DateFormat;
import java.util.*;

public class ApplicationState {

  public static I18n i18n = new I18n();

  private static Map configuredUrls = new FinalKeyMap();

  public static PlatformConfiguration platformConfiguration = new PlatformConfiguration();

  public static boolean loaded;

  private static String version;
  private static String build;


  public static Map<String, String> applicationSettings = new HashTable();
  public static Map<String, Object> applicationParameters = new HashTable<String, Object>();

  private static Set<Permission> permissions = new HashSet<Permission>();

  public static Locale SYSTEM_LOCALE = Locale.US;
  public static TimeZone SYSTEM_TIME_ZONE = DateFormat.getTimeInstance().getTimeZone();
  private static String[] localizedDateFormats=null;

  public static boolean isHostMode=false;

  
  /**
   * this is the controller used by the command.jsp to communicate client->server.
   * Can be overriden by other application's specific controller
   */
  public static Class<? extends ActionController> commandController = DefaultCommandController.class;
  /**
   * http(s)/server name:server port/context path
   */
  public static String serverURL;
  /**
   * does not end with file separator
   */
  public static String webAppFileSystemRootPath;
  public static String contextPath = "";

  public static Map<String, EntityViewerBricks> entityViewers = new HashTable();

  private ApplicationState() {
  }

  public static String getApplicationSetting(String key) {
    if (ApplicationState.getApplicationSettings() == null)
      return null;

    Object o = ApplicationState.getApplicationSettings().get(key);
    if (o == null)
      return null;

    return (String) o;
  }

  public static String getApplicationSetting(String key, String defaultValue) {
    String value = getApplicationSetting(key);
    if (!JSP.ex(value))
      value = defaultValue;
    return value;
  }

  public static Map getApplicationSettings() {
    return applicationSettings;
  }

  public static Map getConfiguredUrls() {
    return configuredUrls;
  }



  public static String getApplicationVersion() {
    return getVersion()+"."+getBuild();
  }


  public static String getVersion()  {
    if (!JSP.ex(ApplicationState.version)) {
      try {
        getBuildProps();
      } catch (IOException e) {
        Tracer.platformLogger.error(e);
        build = "ERROR";
      }
    }
    return ApplicationState.version ;
  }


  public static String getBuild() {
    if (!JSP.ex(build)) {
      try {
        getBuildProps();
      } catch (IOException e) {
        Tracer.platformLogger.error(e);
        build = "ERROR";
      }
    }
    return build;
  }


  private static void getBuildProps() throws IOException {
    Properties props = new Properties();
    FileInputStream is =  new FileInputStream(ApplicationState.webAppFileSystemRootPath + File.separator + "commons" + File.separator + "settings" + File.separator + "Platform.number");

    if (is != null) {
      props.load(is);
      version = props.getProperty("version").trim();
      build = props.getProperty("build").trim();
    }
  }


  public static void dumpApplicationSettings() {

    String globalPath = ApplicationState.webAppFileSystemRootPath + File.separator +
            "commons" + File.separator + "settings" + File.separator + PlatformConfiguration.globalSettingsFileName;

    Properties properties = new Properties();
    TreeSet<String> sett = new TreeSet(ApplicationState.applicationSettings.keySet());
    for (String key : sett) {
      properties.put(key, ApplicationState.applicationSettings.get(key));
    }
    FileUtilities.savePropertiesInUTF8(properties, globalPath);
  }

  public static Set<Permission> getPermissions() {
    synchronized (permissions) {
      return permissions;
    }
  }

  public static void refreshGlobalSettings(Properties properties, HttpServletRequest request) {
    ApplicationState.getApplicationSettings().putAll(properties);

    UrlComposer.DISABLE_VIEW_ID = Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("DISABLE_VIEW_ID"));

    HttpUtilities.serverURL(request); // this uses PUBLIC_SERVER_NAME if exists
    String indexPath = ApplicationState.getApplicationSetting(IndexingConstants.INDEX_PATH);
    if (JSP.ex(indexPath))
      PersistenceConfiguration.getFirstPersistenceConfiguration().getHibernateConfiguration().getProperties().put("hibernate.search.default.indexBase", indexPath);
    LoaderSupport.applySystemSettings();
  }

  public static void setLocale() {
    //reset date formats
    localizedDateFormats=null;
    
    ApplicationState.SYSTEM_LOCALE = I18n.getLocale(ApplicationState.getApplicationSetting(OperatorConstants.FLD_SELECT_LANG));
    //very important! so the JDK is aware of the application needs
    Locale.setDefault(ApplicationState.SYSTEM_LOCALE);
  }


  public static String getSystemLocalizedDateFormat(int field){
    if (localizedDateFormats==null){
      localizedDateFormats =DateUtilities.getLocalizedDateFormats(SYSTEM_LOCALE);
    }
    return localizedDateFormats[field];
  }

  public static boolean isActive(String parameterName) {
    return new ClientEntry("d",applicationSettings.get(parameterName)+"").checkFieldValue();
  }
}