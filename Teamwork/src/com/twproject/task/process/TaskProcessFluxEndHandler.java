package com.twproject.task.process;

import com.twproject.task.TaskStatus;
import com.twproject.task.Task;
import org.jblooming.tracer.Tracer;
import org.jblooming.operator.Operator;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.ontology.Pair;
import org.jbpm.graph.exe.ExecutionContext;
import org.jbpm.graph.exe.ProcessInstance;
import org.jbpm.graph.def.ActionHandler;

import java.util.HashSet;
import java.util.ArrayList;


public class TaskProcessFluxEndHandler implements ActionHandler {




  public void execute(ExecutionContext executionContext) throws PersistenceException {

    try {
      ProcessInstance processInstance = executionContext.getProcessInstance();

      TaskProcess taskProcess = (TaskProcess) PersistenceHome.findUnique(TaskProcess.class, "processInstance", processInstance);
      Task taskProcessRoot = taskProcess.getTask();

      String loggedOperatorId = executionContext.getJbpmContext().getActorId();
      Operator loggedOperator = (Operator) PersistenceHome.findByPrimaryKey(Operator.class, loggedOperatorId);

      taskProcessRoot.changeStatusPersistAndPropagate(TaskStatus.STATUS_DONE, "",   new HashSet<Task>(), loggedOperator, new ArrayList<Pair<String, String[]>>());

    } catch (Throwable t) {
      Tracer.platformLogger.error(t);
    }

  }



}