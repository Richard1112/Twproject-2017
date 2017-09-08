package com.twproject.task.process;

import com.twproject.operator.TeamworkOperator;
import com.twproject.task.Task;
import org.jblooming.ontology.PerformantNodeSupport;
import org.jblooming.operator.Operator;
import org.jblooming.persistence.PersistenceHome;
import org.jbpm.graph.def.Node;
import org.jbpm.graph.exe.ExecutionContext;
import org.jbpm.graph.exe.ProcessInstance;
import org.jbpm.security.SecurityHelper;
import org.jbpm.taskmgmt.def.AssignmentHandler;
import org.jbpm.taskmgmt.exe.Assignable;

public class DefaultAssignmentHandler implements AssignmentHandler {

  ProcessInstance processInstance;
  Node node;
  TaskProcess taskProcess;
  Task taskProcessRoot;
  Task taskOnStep;
  TeamworkOperator loggedOperator;


  public void assign(Assignable assignable, ExecutionContext executionContext) throws Exception {
    processInstance = executionContext.getProcessInstance();

    node = executionContext.getNode();

    taskProcess = (TaskProcess) PersistenceHome.findUnique(TaskProcess.class, "processInstance", processInstance);
    taskProcessRoot = taskProcess.getTask();

    String loggedOperatorId = executionContext.getJbpmContext().getActorId();
    loggedOperator = (TeamworkOperator) PersistenceHome.findByPrimaryKey(Operator.class, loggedOperatorId);


    // recover the taskOnStep by using the externalCode
    for (PerformantNodeSupport t : taskProcessRoot.getChildren()) {
      Task task = (Task) t;
      if ((node.getId() + "").equalsIgnoreCase(task.getExternalCode())) {
        taskOnStep = task;
        break;
      }
    }

  }

}