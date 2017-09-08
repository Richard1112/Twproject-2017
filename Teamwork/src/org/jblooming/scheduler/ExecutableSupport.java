package org.jblooming.scheduler;

import org.jblooming.tracer.Tracer;

import java.util.Date;
import java.io.Serializable;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public abstract class ExecutableSupport implements Executable {

public long secondLastExecutionTime=0;
  public String name;


  public ExecutableSupport(){
    this("");
    name=super.toString();
  }
  public ExecutableSupport(String name){
    this.name=name;
  }

  public JobLogData runAndLog(Job job)  {
    secondLastExecutionTime = job.getSecondLastExecutionTime();
    JobLogData jl = new JobLogData();
    jl.id = job.getId();
    jl.date = new Date();
    try {
      run(jl);
      jl.successfull=true;
    } catch (Throwable e) {
      jl.successfull=false;
      jl.notes = jl.notes+" "+e.getMessage();
      Tracer.platformLogger.error("JobLogData runAndLog "+e.getMessage(),e);
    }

    return jl;
  }

  public String toString(){
    return name;
  }

}
