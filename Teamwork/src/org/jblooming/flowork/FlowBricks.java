package org.jblooming.flowork;

import org.jblooming.waf.Bricks;
import org.jblooming.waf.html.input.Combo;
import org.jblooming.waf.view.PageState;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.utilities.CodeValueList;
import org.jblooming.utilities.HashTable;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.exceptions.FindException;
import org.jbpm.graph.node.TaskNode;
import org.jbpm.graph.exe.Token;
import org.jbpm.graph.exe.ProcessInstance;
import org.jbpm.graph.def.Node;
import org.jbpm.graph.def.ProcessDefinition;
import org.jbpm.graph.def.Transition;
import org.jbpm.taskmgmt.exe.TaskInstance;
import org.jbpm.JbpmContext;
import org.jbpm.db.GraphSession;

import java.util.*;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 21-apr-2006 : 14.56.07
 */
public class FlowBricks extends Bricks {

  /*
    public static void getActiveTokens(Token t, List<Token> at) {

      if (t.getChildren()!=null) {
        for (Object tc : t.getChildren().values()) {
          Token tk = (Token)tc;
          TaskNode tn = null;
          if (tk.getNode() instanceof TaskNode) {
            tn = (TaskNode)tk.getNode();
          }

          if (tk.getEnd()==null && tn!=null)
            at.add(tk);
          getActiveTokens(tk,at);
        }
      }
    }

  */
  public static void getActiveTokens(Token t, List<Token> at) {

    TaskNode tn = null;
    Object tnO = ReflectionUtilities.getUnderlyingObjectAsObject(t.getNode());
    if (tnO instanceof TaskNode) {
      tn = (TaskNode) tnO;
    }
    if (t.getEnd() == null && tn != null)
      at.add(t);

    if (t.getChildren() != null) {
      for (Object tc : t.getChildren().values()) {
        Token tk = (Token) tc;
        getActiveTokens(tk, at);
      }
    }
  }


  public static TaskInstance getTaskInstance(ProcessInstance pi, TaskNode n) {

    String hql = "from " + TaskInstance.class.getName() + " as ti where ti.processInstance=:pi and ti.task.taskNode=:tn";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setEntity("pi", pi);
    oql.getQuery().setEntity("tn", n);
    return (TaskInstance) oql.uniqueResultNullIfEmpty();
  }


  public static List<ProcessDefinition> findLatestProcessDefinitions(JbpmContext jbpmSession) {

    GraphSession gs = jbpmSession.getGraphSession();

    List<ProcessDefinition> updDefs = gs.findLatestProcessDefinitions();

    Collections.sort(updDefs,
            new Comparator() {
              public int compare(Object b, Object a) {
                return ((ProcessDefinition) b).getName().compareToIgnoreCase((((ProcessDefinition) a).getName()));
              }
            });

    return updDefs;
  }

  public static Combo getLatestProcessDefinitionsCombo(String fieldName, PageState pageState) {

    JbpmContext jbpmSession = PlatformJbpmSessionFactory.getJbpmContext(pageState);
    CodeValueList cvl = new CodeValueList();
    List<ProcessDefinition> pds = findLatestProcessDefinitions(jbpmSession);
    for (ProcessDefinition pd : pds) {
      cvl.add(pd.getId()+"",pd.getName());
    }
    cvl.addChoose(pageState);
    Combo c = new Combo(fieldName,"</td><td>",null,50,cvl,null);
    return c;
  }

}
