package org.jblooming.waf.settings;

import org.jblooming.security.Permission;

import javax.servlet.jsp.PageContext;
import javax.servlet.http.HttpServletRequest;
import java.util.Map;
import java.util.Set;


/**
 * A platform application configuration.
 * 
 * @see org.jblooming.waf.settings.PlatformConfiguration
 */
public interface Application {

  public final static String PLATFORM_APP_NAME = "Platform";

  String getName();

  String getRootFolder();

  boolean isLoginCookieEnabled();

  void configurePersistence(PlatformConfiguration pc) throws Exception;

  void configureFreeAccess(PlatformConfiguration pc);

  void configureNeedingPersistence(PlatformConfiguration pc);

  void configureNeedingPageContext(PageContext pageContext);

  Set<Permission> getPermissions();

  Class getDefaultScreenClass();

  void configBeforePerform(HttpServletRequest request);

  void applicationDestroy();

}
