package org.jblooming.flowork;

import org.jblooming.waf.view.RestState;
import org.jbpm.taskmgmt.exe.*;
import org.jbpm.db.*;
import org.jbpm.graph.def.*;
import org.jbpm.JbpmContext;
import org.jblooming.oql.*;
import org.jblooming.persistence.exceptions.*;
import org.jblooming.persistence.hibernate.*;
import org.jblooming.waf.view.PageState;

import java.util.*;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class JBPMQueryHelper {

  public static List<TaskInstance> getTasksForSwimlane(String swimlaneName) throws FindException {

    String hql = "from " + TaskInstance.class.getName() + " as ti where ti.swimlaneInstance.swimlane.name = :name and ti.end is null and ti.token.end is null";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setString("name", swimlaneName);
    return oql.list();
  }

  public static Node doStep(long taskInstanceId, long transitionId, RestState pageState) {

    JbpmContext jbpmSession = PlatformJbpmSessionFactory.getJbpmContext(pageState);
    
    TaskMgmtSession tms = jbpmSession.getTaskMgmtSession();
    TaskInstance ti = tms.loadTaskInstance(taskInstanceId);
    Transition transition = (Transition) HibernateFactory.getSession().load(Transition.class, transitionId);
    ti.end(transition);
    
    jbpmSession.save(ti);

    return transition.getFrom();
  }


}
