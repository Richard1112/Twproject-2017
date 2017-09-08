package com.twproject.task.process;

import com.twproject.operator.TeamworkOperator;
import com.twproject.task.Assignment;
import org.jblooming.PlatformRuntimeException;
import org.jbpm.graph.exe.ExecutionContext;
import org.jbpm.taskmgmt.exe.Assignable;

public abstract class DinamicAssignmentHandler extends DefaultAssignmentHandler {

  String role;
  String estimated;

  public void assign(Assignable assignable, ExecutionContext executionContext) throws Exception {
    super.assign(assignable, executionContext);

    Assignment ass = getTeamworkAssignment(assignable, executionContext);
    if (ass != null) {
      TeamworkOperator op = ass.getResource().getMyself();

      if (op == null)
        op = loggedOperator;
      assignable.setActorId(op.getId() + "");
    } else {
      throw new PlatformRuntimeException("Assignment not created in process " + executionContext.getProcessDefinition().getName() + " instance " + executionContext.getProcessInstance().getId());
    }
  }


  abstract Assignment getTeamworkAssignment(Assignable assignable, ExecutionContext executionContext);


}