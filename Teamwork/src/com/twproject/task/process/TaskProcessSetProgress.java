package com.twproject.task.process;

import com.twproject.rank.Hit;
import org.jblooming.PlatformRuntimeException;
import org.jbpm.graph.exe.ExecutionContext;


public class TaskProcessSetProgress extends TeamworkTaskNodeEventHandler {

  Double progress;

  public void doTheRealAction(ExecutionContext executionContext) {
    try {

      taskOnStep.setProgress(progress);
      taskOnStep.store();
      Hit.getInstanceAndStore(taskOnStep, loggedOperator, .2);

    } catch (Throwable t) {
      throw new PlatformRuntimeException(t);
    }

  }
}