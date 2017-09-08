package com.twproject.task.process;

import org.jbpm.graph.exe.ExecutionContext;


public class SetProgressToRoot extends DefaultActionHandler {

  Double progress;

  public void execute(ExecutionContext executionContext)  throws Exception{

    super.execute(executionContext);
    if (taskProcessRoot!=null){  //may happen when deleting a task
      taskProcessRoot.setProgress(progress);
      taskProcessRoot.store();
    }

  }
}