package org.jblooming.flowork.businessLogic;

import org.jbpm.graph.def.Transition;
import org.jbpm.graph.def.Node;
import org.jbpm.graph.def.ProcessDefinition;
import org.jbpm.graph.node.TaskNode;
import org.jbpm.graph.node.Fork;
import org.jbpm.graph.node.Join;
import org.jbpm.graph.node.MilestoneNode;
import org.jbpm.taskmgmt.def.Task;
import org.jbpm.taskmgmt.def.Swimlane;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.HashTable;

import java.util.*;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 15-dic-2005 : 11.10.37
 */
public class JbpmGraphSupport {

  public static Set<Node> getDestinations(Node node) {
    Set<Node> ret = new HashSet<Node>();
    node = (Node) ReflectionUtilities.getUnderlyingObjectAsObject(node);
    if (node instanceof Fork || node instanceof Join) {     // se la destinazione è un fork/join si crea un link per ogni leaving
      Collection lec = node.getLeavingTransitions();
      for (Iterator ite = lec.iterator(); ite.hasNext();) {
        Transition transition = (Transition) ite.next();
        Node dest = transition.getTo();
        ret.addAll(getDestinations(dest));
      }
    } else {
      ret.add(node);
    }
    return ret;
  }

  public static Set<Node> getOrigins(Node node) {
    Set<Node> ret = new HashSet<Node>();
    node = (Node) ReflectionUtilities.getUnderlyingObjectAsObject(node);
    if (node instanceof Fork || node instanceof Join) {     // se la destinazione è un fork/join si crea un link per ogni incoming
      Collection lec = node.getArrivingTransitions();
      for (Iterator ite = lec.iterator(); ite.hasNext();) {
        Transition transition = (Transition) ite.next();
        Node dest = transition.getFrom();
        ret.addAll(getDestinations(dest));
      }
    } else {
      ret.add(node);
    }
    return ret;
  }


  public static String getSwimlane(Node node) {
    String swim = "";
    if (node instanceof TaskNode) {
      TaskNode tn = (TaskNode) node;
      Map<String, Task> tmap = tn.getTasksMap();
      if (tmap.size() > 0) {
        Swimlane swimlane = tmap.values().iterator().next().getSwimlane();        
        swim = swimlane==null?"none":swimlane.getName();
      }
    } else if (node instanceof MilestoneNode || node instanceof Join) {
      List leavingTransitions = node.getLeavingTransitions();
      for (Object transitionO : leavingTransitions) {
        Transition transition = (Transition) transitionO;
        node = transition.getTo();
        swim = getSwimlane(node);
        if (swim.trim().length() > 0)
          return swim;
      }
    } else if (node instanceof Fork) {
      Set<Transition> arrivingTransitions = node.getArrivingTransitions();
      if (arrivingTransitions.size() > 0)
        swim = getSwimlane(arrivingTransitions.iterator().next().getFrom());

    }
    return swim;
  }

  public static List<List<Node>> getNodesByLevel(ProcessDefinition definition) {
    Set<Node> visited = new HashSet();
    LinkedList<List<Node>> result = new LinkedList();

    int level = 0;
    flattenIt(definition.getStartState(), result, visited, new HashTable<Transition, Integer>(), level);

    return result;
  }

  public static void flattenIt(Node node, LinkedList<List<Node>> nodes, Set<Node> visited, Map<Transition, Integer> visitedTransition, int level) {

    boolean allIncomingVisited = true;
    int maxLevel = level;
    if (JSP.ex(node.getArrivingTransitions()) && (node instanceof Join)) {

      for (Transition trans : (Set<Transition>) node.getArrivingTransitions()) {
        if (!visitedTransition.containsKey(trans)) {
          allIncomingVisited = false;
          break;
        } else {
          maxLevel = Math.max(maxLevel, visitedTransition.get(trans));
        }
      }
    }

    // exist the level?
    if (maxLevel >= nodes.size()) {
      nodes.add(new ArrayList());
    }

    level = maxLevel;

    // if not already visited
    if (allIncomingVisited) {
      visited.add(node);

      // get the list and add the node
      nodes.get(level).add(node);

      //increment the level
      level++;

      // loop transitions


      for (Object t : node.getLeavingTransitions()) {
        Transition transition = (Transition) t;
        visitedTransition.put(transition, level);
        if (!visited.contains(transition.getTo())) {
          flattenIt(transition.getTo(), nodes, visited, visitedTransition, level);
        }
      }
    }

  }

  public static int getNodelevel(Node node, List<List<Node>> nodesByLevel) {
    int indoglie = -1;
    for (List<Node> level : nodesByLevel) {
      indoglie++;
      if (level.contains(node))
        break;
    }
    return indoglie;
  }
}
