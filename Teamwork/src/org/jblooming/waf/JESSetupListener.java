package org.jblooming.waf;

import com.ericdaugherty.mail.server.Mail;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.log4j.Appender;
import org.apache.log4j.Logger;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.scheduler.PlatformExecutionService;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.settings.ApplicationState;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.http.HttpSessionAttributeListener;
import javax.servlet.http.HttpSessionBindingEvent;
import javax.servlet.http.HttpSessionEvent;
import javax.servlet.http.HttpSessionListener;
import java.io.File;
import java.util.Enumeration;
import java.util.concurrent.Future;

public class JESSetupListener implements ServletContextListener, HttpSessionListener, HttpSessionAttributeListener {

/*
  Per attivarlo bisogna mettere questo su web.xml

  <listener>
    <listener-class>org.jblooming.waf.JESSetupListener</listener-class>
  </listener>


  e JES_ENABLED=yes su global.properties

*/

  public static Future<?> mailFuture = null;

  public JESSetupListener() {
  }


  /*

chiamata originale del mail.bat

"%JAVA_EXEC%" -client -Xmx512m
-Ddns.server=
-Ddns.search=
-Dsun.net.spi.nameservice.provider.1=dns,dnsjavaJES
-Djava.security.policy=%JES_HOME%\jes.policy
-Dorg.apache.commons.logging.Log=org.apache.commons.logging.impl.Log4JLogger
-Dlog4j.configuration=file:/%JES_HOME%/conf/log4j.properties
-cp %JES_HOME%\jes.jar;%JES_HOME%\lib\log4j-1.2.17.jar
com.ericdaugherty.mail.server.Mail %JES_HOME% %1

   */
  /* This method is called when the servlet context is
     initialized(when the Web application is deployed).
     You can initialize servlet context related data here.
  */
  public void contextInitialized(ServletContextEvent sce) {

    //redirect JES Logger su email.log


    //Log jesLogger = LogFactory.getLog("JESLogger");

    try {

      if ("yes".equalsIgnoreCase(ApplicationState.getApplicationSettings().get("JES_ENABLED") + "")) {

        Logger logger = Logger.getLogger("JESLogger");
        logger.removeAllAppenders();
        Enumeration<Appender> allAppenders = Tracer.emailLogger.getAllAppenders();
        logger.setLevel(Tracer.emailLogger.getLevel()); // copia il livello da email

        while (allAppenders.hasMoreElements())
          logger.addAppender(allAppenders.nextElement());


        Tracer.emailLogger.info("JESSetupListener started");


        startJES();


      }


    } catch (Throwable t) {
      Tracer.emailLogger.error("Something wrong loading JES", t);
      throw new PlatformRuntimeException(t);
    }

  }

  /* This method is invoked when the Servlet Context
     (the Web application) is undeployed or
     Application Server shuts down.
  */
  public void contextDestroyed(ServletContextEvent sce) {
    try {
      Mail instance = Mail.getInstance();
      if (instance != null && !instance.isShuttingDown()) {
        instance.shutdown();
        if (mailFuture != null)
          mailFuture.cancel(true);
        Tracer.emailLogger.info("JESSetupListener: JES shutting down");
      }
    } catch (Throwable t) {
      Tracer.emailLogger.info("JESSetupListener: error shutting down JES", t);
    }
  }

  // -------------------------------------------------------
  // HttpSessionListener implementation
  // -------------------------------------------------------
  public void sessionCreated(HttpSessionEvent se) {
    /* Session is created. */
  }

  public void sessionDestroyed(HttpSessionEvent se) {
    /* Session is destroyed. */
  }

  // -------------------------------------------------------
  // HttpSessionAttributeListener implementation
  // -------------------------------------------------------

  public void attributeAdded(HttpSessionBindingEvent sbe) {
    /* This method is called when an attribute 
       is added to a session.
    */
  }

  public void attributeRemoved(HttpSessionBindingEvent sbe) {
    /* This method is called when an attribute
       is removed from a session.
    */
  }

  public void attributeReplaced(HttpSessionBindingEvent sbe) {
    /* This method is invoked when an attibute
       is replaced in a session.
    */
  }

  public static void stopJES() {
    Mail instance = Mail.getInstance();
    if (instance != null && !instance.isShuttingDown() && !instance.isRestarting())
      instance.shutdown();
    if (JESSetupListener.mailFuture != null)
      JESSetupListener.mailFuture.cancel(true);
  }


  public static void startJES() {
    String jh = (String) ApplicationState.getApplicationSettings().get("JES_HOME");
    final String JES_HOME = JSP.ex(jh) ? jh : (ApplicationState.webAppFileSystemRootPath + File.separator + "WEB-INF" + File.separator + "jes");

    System.setProperty("dns.server", "");
    System.setProperty("dns.search", "");
    System.setProperty("sun.net.spi.nameservice.provider.1", "dns");
    System.setProperty("java.security.policy", JES_HOME + File.separator + "jes.policy");

    if (Mail.getInstance() == null) {

      //lanciato su thread separato altrimenti pianta tutto
      mailFuture = PlatformExecutionService.executorService.submit(
        new Thread() {
          public void run() {
            try {
              Mail.main(new String[]{JES_HOME});
            } catch (Throwable e) {
              Tracer.emailLogger.error("JESSetupListener JES starting error", e);
            }
          }
        });
    } else {
      Tracer.emailLogger.error("Cannot instantiate JES. Is already running.");
    }
  }


}
