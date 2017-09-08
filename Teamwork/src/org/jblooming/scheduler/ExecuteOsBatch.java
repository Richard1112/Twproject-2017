package org.jblooming.scheduler;

import org.jblooming.utilities.DateUtilities;

import java.io.IOException;
import java.util.Date;

public class ExecuteOsBatch extends ExecutableSupport {
  @Parameter("[insert here complete path to executable/script]")
          public String commandToExecute;

  public JobLogData run(JobLogData jobLogData) throws IOException {

    Runtime.getRuntime().exec(commandToExecute);

    jobLogData.notes = jobLogData.notes + "Executed on " + DateUtilities.dateAndHourToString(new Date());

    return jobLogData;
  }
}
