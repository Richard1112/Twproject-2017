package org.jblooming.scheduler;

import org.jblooming.utilities.StringUtilities;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.tracer.Tracer;

import java.lang.reflect.Method;
import java.util.Date;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class ExecuteJavaMain extends ExecutableSupport{

  @Parameter("[insert here complete classpath of class]")
  public String classToLaunch;


  @Parameter("[insert here parameters separated by spaces]")
  public String spaceSeparatedParameters;

  public JobLogData run(JobLogData jobLogData) {

    try {

      Class clazz = Class.forName(classToLaunch);     
      Method main = clazz.getDeclaredMethod("main",String[].class);
      main.invoke(null, new Object[]{StringUtilities.splitToArray(spaceSeparatedParameters," ")});

      jobLogData.notes = jobLogData.notes + "Executed on "+DateUtilities.dateAndHourToString(new Date());

    } catch (Throwable e) {
      Tracer.platformLogger.error("ExecuteJavaMain error",e);
      jobLogData.successfull = false;
    }
    return jobLogData;
  }

}
