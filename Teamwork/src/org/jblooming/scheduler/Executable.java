package org.jblooming.scheduler;

import java.io.Serializable;

public interface Executable {

  public JobLogData run(JobLogData jobLog) throws Exception;
  public JobLogData runAndLog(Job job);

}
