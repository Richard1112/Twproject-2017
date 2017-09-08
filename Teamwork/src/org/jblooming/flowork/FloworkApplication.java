package org.jblooming.flowork;

import org.jbpm.graph.def.ProcessDefinition;
import org.jbpm.jpdl.exe.MilestoneInstance;
import org.jbpm.JbpmContext;
import org.jbpm.db.GraphSession;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.hibernate.HibernateFactory;
import org.jblooming.waf.settings.ApplicationSupport;
import org.jblooming.waf.view.PageState;
import org.jblooming.oql.OqlQuery;
import org.jblooming.security.Permissions;
import org.jblooming.operator.Operator;
import org.hibernate.Session;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Aug 24, 2007
 * Time: 2:52:29 PM
 */
public abstract class FloworkApplication extends ApplicationSupport {

  protected FloworkApplication(Permissions permissionsImpl) {
    this(Operator.class,permissionsImpl);
  }

  protected FloworkApplication(Class defaultOperatoSubClass, Permissions permissionsImpl) {
    super(defaultOperatoSubClass,permissionsImpl);
  }

  protected FloworkApplication(Class defaultOperatoSubClass, Permissions[] permissionsImpl) {
     super(defaultOperatoSubClass,permissionsImpl);
  }

  protected abstract void removeLocalDependencies(ProcessDefinition def) throws PersistenceException;


  public void removeDefinition(ProcessDefinition def, PageState pageState) throws PersistenceException {

    removeLocalDependencies(def);

    String hql = "select msi from " + MilestoneInstance.class.getName() + " as msi where msi.token.processInstance.processDefinition=:pd";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setEntity("pd", def);
    List<MilestoneInstance> misL = oql.list();
    Session session = HibernateFactory.getSession();

    for (MilestoneInstance milestoneInstance : misL) {
      milestoneInstance.setToken(null);
      session.save(milestoneInstance);
    }

    //HibernateFactory.getSession().delete(pd);*/
    JbpmContext jbpmSession = PlatformJbpmSessionFactory.getJbpmContext(pageState);
    GraphSession gs = jbpmSession.getGraphSession();
    gs.deleteProcessDefinition(def);

    for (MilestoneInstance milestoneInstance : misL) {
      session.delete(milestoneInstance);
    }

  }
  
  public void configBeforePerform(HttpServletRequest request) {
    JbpmContext jbpmSession = PlatformJbpmSessionFactory.getJbpmContext(PageState.getCurrentPageState(request));
    PageState pageState = PageState.getCurrentPageState(request);
    if (pageState.getLoggedOperator()!=null)
    jbpmSession.setActorId(pageState.getLoggedOperator().getId().toString());
  }



  public void applicationDestroy() {
    //destroy JBPM -> stops jbpm schedulers
    PlatformJbpmSessionFactory.destroy();
  }


}
