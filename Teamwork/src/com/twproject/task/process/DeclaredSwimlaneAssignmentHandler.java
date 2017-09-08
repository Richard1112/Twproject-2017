package com.twproject.task.process;

import com.twproject.task.Assignment;
import com.twproject.task.Task;
import com.twproject.resource.Resource;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.utilities.StringUtilities;
import org.jbpm.graph.exe.ExecutionContext;
import org.jbpm.graph.node.TaskNode;
import org.jbpm.taskmgmt.def.Swimlane;
import org.jbpm.taskmgmt.exe.Assignable;
import org.jbpm.taskmgmt.exe.SwimlaneInstance;

import java.util.Set;

public class DeclaredSwimlaneAssignmentHandler extends DinamicAssignmentHandler {

  String resource;

  public Assignment getTeamworkAssignment(Assignable assignable, ExecutionContext executionContext) {
    Assignment assig = null;

    try {

      //if "resource" param is specified
      if (JSP.ex(resource)) {

        Set<String> ress = StringUtilities.splitToSet(resource, ",");  //supports multiple resources comma delimited

        for (String res : ress) {
          //recover resource
          Resource twResource = Resource.loadByRef(res);
          if (twResource != null) {
            //create first assig or retrieve it
            assig = ProcessUtilities.createAssignment(taskOnStep, twResource,role,estimated,  executionContext.getTaskInstance().getTask(), JSP.w(node.getDescription()),loggedOperator, true);

            //recover all task for this swimlane
            SwimlaneInstance swli = (SwimlaneInstance) ReflectionUtilities.getUnderlyingObjectAsObject(assignable);
            Swimlane swimlane = swli.getSwimlane();
            Set<org.jbpm.taskmgmt.def.Task> jbpmTasks = swimlane.getTasks();

            //match twTask with jbpmTasks
            for (Task twTask : taskProcessRoot.getDescendants()) {
              if (taskOnStep.equals(twTask)) //skip this one
                continue;

              //loop for for jbTaskNodes to find
              for (org.jbpm.taskmgmt.def.Task jbTask : jbpmTasks) {
                if ((jbTask.getTaskNode().getId() + "").equals(twTask.getExternalCode())) {
                  //create the assignement for task found
                  ProcessUtilities.createAssignment(twTask, assig.getResource(),role,estimated, jbTask, JSP.w(jbTask.getTaskNode().getDescription()),loggedOperator, true);
                  break;
                }
              }
            }

          } else {
            throw new PlatformRuntimeException("Resource not found: " + resource + " in process " + executionContext.getProcessDefinition().getName() + " instance " + executionContext.getProcessInstance().getId());
          }
        }
      }

    } catch (Exception e) {
      throw new PlatformRuntimeException("Exception  on " + executionContext.getProcessDefinition().getName() + " instance " + executionContext.getProcessInstance().getId(), e);
    }

    return assig;
  }


}