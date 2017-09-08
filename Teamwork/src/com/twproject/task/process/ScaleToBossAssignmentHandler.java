package com.twproject.task.process;

import org.jbpm.graph.exe.ExecutionContext;
import org.jbpm.graph.node.TaskNode;
import org.jbpm.taskmgmt.exe.Assignable;
import org.jbpm.taskmgmt.def.Task;
import org.jblooming.utilities.JSP;
import org.jblooming.PlatformRuntimeException;
import com.twproject.resource.Person;
import com.twproject.task.Assignment;

public class ScaleToBossAssignmentHandler extends DinamicAssignmentHandler {

  public Assignment getTeamworkAssignment(Assignable assignable, ExecutionContext executionContext) {

    TaskNode taskNode = (TaskNode) node;

    Task jbpmTask = executionContext.getTaskInstance().getTask();

    Assignment ass = null;

    try {

      if (!JSP.ex(role))
        role = "Worker";

      //recover boss of logged
      Person person = loggedOperator.getPerson();
      Person boss = person.getMyManager();
      if (boss == null)
        boss = person;

      if (boss != null) {
        ass = ProcessUtilities.createAssignment(taskOnStep, boss,role, estimated, jbpmTask,JSP.w(taskNode.getDescription()),loggedOperator, false);
      } else {
        throw new PlatformRuntimeException("Boss not found: in process " + executionContext.getProcessDefinition().getName() + " instance " + executionContext.getProcessInstance().getId());
      }


    } catch (Exception e) {
      throw new PlatformRuntimeException("Exception  on "+ executionContext.getProcessDefinition().getName() + " instance " + executionContext.getProcessInstance().getId(),e);
    }

    return ass;
  }

}