package com.twproject.task.process;

import com.twproject.operator.TeamworkOperator;
import com.twproject.task.Task;
import org.jblooming.persistence.PersistenceHome;
import org.jbpm.graph.def.ActionHandler;
import org.jbpm.graph.def.Node;
import org.jbpm.graph.exe.ExecutionContext;
import org.jbpm.graph.exe.ProcessInstance;


public abstract class DefaultActionHandler implements ActionHandler {

  ProcessInstance processInstance;
  Node node;
  TaskProcess taskProcess;
  Task taskProcessRoot;
  TeamworkOperator loggedOperator;

  public void execute(ExecutionContext executionContext)  throws Exception{
    processInstance = executionContext.getProcessInstance();

    node = executionContext.getNode();

    taskProcess = (TaskProcess) PersistenceHome.findUnique(TaskProcess.class, "processInstance", processInstance);
    if (taskProcess!=null)
      taskProcessRoot = taskProcess.getTask();

    String loggedOperatorId = executionContext.getJbpmContext().getActorId();
    loggedOperator = TeamworkOperator.load(loggedOperatorId);
  }

}