package org.jblooming.flowork.samples;

import org.jbpm.graph.def.*;
import org.jbpm.graph.exe.*;
import org.jbpm.graph.node.*;
import org.jbpm.taskmgmt.def.*;

import java.util.*;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class SendMailToSwimlaneAction implements ActionHandler {

  // Before each test (in the setUp), the isExecuted member
  // will be set to false.
  public static boolean isExecuted = false;

  // The action will set the isExecuted to true so the
  // unit test will be able to show when the action
  // is being executed.
  public void execute(ExecutionContext executionContext) {

    TaskNode tn = (TaskNode) executionContext.getNode();
    Map<String, Task> tmap = tn.getTasksMap();
    String swimlanesDescs = "";
    for (String tname : tmap.keySet()) {
      Task task = tmap.get(tname);
      Swimlane s = task.getSwimlane();
      swimlanesDescs = swimlanesDescs + s != null ? s.getName() + " " : "no swiml.for " + tname + "&nbsp;";
    }

    isExecuted = true;
  }
}
