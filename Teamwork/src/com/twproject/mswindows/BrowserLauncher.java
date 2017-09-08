package com.twproject.mswindows;

import org.jblooming.tracer.Tracer;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import java.io.File;

/**
 * @author Pietro Polsinelli : ppolsinelli@open-lab.com
 */
public class BrowserLauncher implements ServletContextListener {
  public void contextInitialized(ServletContextEvent event) {
    try {
      if (System.getProperty("os.name").toLowerCase().indexOf("windows") != -1) {
        String path = event.getServletContext().getRealPath("WEB-INF/classes/setupped.txt");
        if (!new File(path).exists()) {
          Runtime.getRuntime().exec("explorer http://localhost");
        }
      }
    } catch (Throwable e) {
      // it ain't dramatic
      Tracer.platformLogger.info("Automated browser launch failed:" + e.getMessage());
    }

  }

  public void contextDestroyed(ServletContextEvent event) {
  }

}
