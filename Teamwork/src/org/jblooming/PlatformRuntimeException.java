package org.jblooming;

import org.jblooming.operator.Operator;
import org.jblooming.persistence.ThreadLocalPersistenceContextCarrier;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.waf.settings.ApplicationState;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Date;

/**
 * @author Pietro Polsinelli : ppolsinelli@open-lab.com
 */
public class PlatformRuntimeException extends RuntimeException {

  public PlatformRuntimeException() {
    this("");
  }

  public PlatformRuntimeException(Throwable cause) {
    this(cause.getMessage(),cause);
  }

  public PlatformRuntimeException(String message) {
    super(message);
    //Tracer.platformLogger.fatal(message, this);
  }

  public PlatformRuntimeException(String message, Throwable cause) {
    //super(getExtendedMessage(message)+"\n", cause);
    super(message, cause);
    //Tracer.platformLogger.fatal(message, cause);
  }

  private static String getExtendedMessage(String message) {

    //believe it or not, we put it here! HAHAHAHAHA (with a Satanic ring)!
    try {
      ThreadLocalPersistenceContextCarrier threadLocalPersistenceContextCarrier = PersistenceContext.threadLocalPersistenceContextCarrier.get();
      if (threadLocalPersistenceContextCarrier !=null && threadLocalPersistenceContextCarrier.getOperator() !=null) {
        Operator operator = threadLocalPersistenceContextCarrier.getOperator();
        message = message + " logged operator: ("+operator.getId()+") "+ operator.getDisplayName()+"\n";
      }
    } catch (Exception e) {
    }

    return message;
  }

  public static String getStackTrace(Throwable aThrowable) {
    if (aThrowable!=null) {
    final Writer result = new StringWriter();
    final PrintWriter printWriter = new PrintWriter(result);
    aThrowable.printStackTrace(printWriter);
    return result.toString();
    } else {
      return "JBlooming: No throwable object available";
    }
  }

  protected static String getTime() {
    return "Server time: " + DateUtilities.dateToString(new Date(), "yyyy MM dd HH:mm:ss") + ". ";
  }
}
