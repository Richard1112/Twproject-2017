package org.jblooming.flowork;

import org.jblooming.persistence.hibernate.HibernateFactory;
import org.jblooming.waf.view.PageState;
import org.jblooming.tracer.Tracer;
import org.jblooming.waf.view.RestState;
import org.jbpm.JbpmConfiguration;
import org.jbpm.JbpmContext;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class PlatformJbpmSessionFactory {

  private static JbpmConfiguration jbpmConfiguration;

  public static JbpmConfiguration getJbpmConfiguration() {

    if (jbpmConfiguration ==null) {
      jbpmConfiguration = JbpmConfiguration.getInstance();
      jbpmConfiguration.startJobExecutor();      
    }
    return jbpmConfiguration;
  }

  public static JbpmContext getJbpmContext(RestState pageState) {

    JbpmContext jbpmContext = (JbpmContext) pageState.getAttribute(JbpmContext.DEFAULT_JBPM_CONTEXT_NAME);
    if (jbpmContext==null) {
      jbpmContext = getJbpmConfiguration().createJbpmContext();
      pageState.setAttribute(JbpmContext.DEFAULT_JBPM_CONTEXT_NAME,jbpmContext);
      jbpmContext.setSession(HibernateFactory.getSession());
    }
    return jbpmContext;
  }

  public static void destroy(){
    if(jbpmConfiguration!=null){
      Tracer.platformLogger.debug("Destroying jbpm schedulers.");
      jbpmConfiguration.close();
    }
  }
}
