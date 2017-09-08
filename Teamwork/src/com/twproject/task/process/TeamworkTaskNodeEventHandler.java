package com.twproject.task.process;

import com.twproject.task.Task;
import org.jblooming.ontology.PerformantNodeSupport;
import org.jbpm.graph.exe.ExecutionContext;
import org.jbpm.graph.node.TaskNode;


public abstract class TeamworkTaskNodeEventHandler extends DefaultActionHandler {

  TaskNode taskNode;
  Task taskOnStep;

  public void execute(ExecutionContext executionContext)  throws Exception{
    super.execute(executionContext);

    taskNode = (TaskNode) executionContext.getNode();

    // recover the taskOnStep by using the externalCode
    for (PerformantNodeSupport t : taskProcessRoot.getChildren()) {
      Task task = (Task) t;
      if ((taskNode.getId() + "").equalsIgnoreCase(task.getExternalCode())) {
        taskOnStep = task;
        break;
      }
    }

    doTheRealAction(executionContext);

  }

  abstract void doTheRealAction(ExecutionContext executionContext);

}
