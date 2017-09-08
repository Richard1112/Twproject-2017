package com.twproject.task.process;

import com.twproject.task.TaskStatus;
import com.twproject.task.Task;
import com.twproject.rank.Hit;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.ontology.Pair;
import org.jbpm.graph.exe.ExecutionContext;

import java.util.HashSet;
import java.util.ArrayList;


public class TaskProcessTaskNodeLeaveHandler extends TeamworkTaskNodeEventHandler {


  public void doTheRealAction(ExecutionContext executionContext) {
    try {

      // set task status to DONE
      if (!TaskStatus.STATUS_DONE.equals(taskOnStep.getStatus())) {
        taskOnStep.changeStatusPersistAndPropagate(TaskStatus.STATUS_DONE, "", new HashSet<Task>(), loggedOperator, new ArrayList<Pair<String, String[]>>());
      }


      Hit.getInstanceAndStore(taskOnStep, loggedOperator, .2);


    } catch (Throwable t) {
      throw new PlatformRuntimeException(t);
    }

  }
}