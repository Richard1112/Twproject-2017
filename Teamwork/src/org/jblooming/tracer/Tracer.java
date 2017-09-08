package org.jblooming.tracer;

import net.sf.json.JSONObject;
import org.apache.log4j.Logger;
import org.hibernate.stat.Statistics;
import org.jblooming.persistence.hibernate.HibernateFactory;
import org.jblooming.utilities.HashTable;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.utilities.StringUtilities;

import javax.servlet.http.HttpServletRequest;
import java.util.*;

public class Tracer {

  private static final MemoryCounter mc = new MemoryCounter();

  public static Logger platformLogger = Logger.getLogger("platformLogger");
  public static Logger i18nLogger = Logger.getLogger("i18nLogger");
  public static Logger hibernateLogger = Logger.getLogger("org.hibernate");
  public static Logger jobLogger = Logger.getLogger("jobLogger");
  public static Logger emailLogger = Logger.getLogger("emailLogger");


  public static String traceRequest(HttpServletRequest request) {
    return traceRequest(request, true);
  }

  public static String traceRequest(HttpServletRequest request, boolean useBR) {
    StringBuffer rd = new StringBuffer();

    String br = useBR ? "<br>" : "\n";
    String hr = useBR ? "<hr>" : "\n----------------------------------------------------------------------\n";

    //authentication
    if (request.getUserPrincipal() != null)
      rd.append("request.getUserPrincipal().getName() " + request.getUserPrincipal().getName() + br);
    rd.append("request.getAuthType()" + request.getAuthType() + br);
    rd.append("request.getRemoteUser() " + request.getRemoteUser() + br);
    rd.append("request.isUserInRole(\"user\") " + request.isUserInRole("user") + br);
    rd.append("request.isSecure() " + request.isSecure() + br + br);

    rd.append("request.getRequestedSessionId() " + request.getRequestedSessionId() + br);
    rd.append("request.getCharacterEncoding() " + request.getCharacterEncoding() + br);
    rd.append("request.getMethod() " + request.getMethod() + "<br><br>");

    //params
    Enumeration parameters = request.getParameterNames();
    rd.append(System.currentTimeMillis() + br);

    rd.append("header:" + br + br);
    Enumeration en = request.getHeaderNames();
    while (en.hasMoreElements()) {
      String s = (String) en.nextElement();
      rd.append(s + ": " + request.getHeader(s) + br);
    }
    rd.append(hr);

    rd.append("attributes:<br><br>");
    Enumeration attr = request.getAttributeNames();
    while (attr.hasMoreElements()) {
      String requestString = (String) attr.nextElement();
      rd.append(requestString + ": " + request.getAttribute(requestString) + br);
    }
    rd.append(hr);

    rd.append("parameters:" + br + br);
    while (parameters.hasMoreElements()) {
      String requestString = (String) parameters.nextElement();
      rd.append(requestString + ": " + request.getParameter(requestString) + br);
    }
    rd.append(hr);

    rd.append("request.getContextPath():" + request.getContextPath() + br);
    rd.append("request.getContentType():" + request.getContentType() + br);
    rd.append("request.getRemoteAddr():" + request.getRemoteAddr() + br);
    rd.append("request.getRemoteHost():" + request.getRemoteHost() + br);
    rd.append("request.getRequestURI():" + request.getRequestURI() + br);
    rd.append("request.getRequestURL():" + request.getRequestURL() + br);

    return rd.toString();

  }

  public static String measureSize(Object o) {
    return measureSize(o, false);
  }


  public static String measureSize(Object o, boolean includeStatic) {
    long mem = mc.estimate(o, includeStatic);
    String s = o.getClass().getSimpleName() + " memory usage size: " + objectSize(mem);
    return s;
  }

  public static String objectSize(long size) {
    int divisor = 1;
    String unit = "bytes";
    if (size >= 1024 * 1024) {
      divisor = 1024 * 1024;
      unit = "MB";
    } else if (size >= 1024) {
      divisor = 1024;
      unit = "KB";
    }
    if (divisor == 1) return size / divisor + " " + unit;
    String aftercomma = "" + 100 * (size % divisor) / divisor;
    if (aftercomma.length() == 1) aftercomma = '0' + aftercomma;
    return size / divisor + "." + aftercomma + ' ' + unit;
  }


  public static String getCallTrace(boolean showFullStack) {

    StringBuffer sb = new StringBuffer();
    Throwable stea = new Throwable();

    StackTraceElement[] trace = stea.getStackTrace();
    //jump the first element
    for (int i = 1; i < trace.length; i++) {
      StackTraceElement element = trace[i];
      try {
        String className = element.getClassName();
        if (showFullStack || (className.indexOf("com.caucho") == -1 && className.indexOf("org.apache") == -1))
          sb.append("\n at " + element);
      } catch (Exception e) {
        throw new PlatformRuntimeException(e);
      }
    }

    /*final Writer result = new StringWriter();
    final PrintWriter printWriter = new PrintWriter(result);
    stea.printStackTrace(printWriter);
    return result.toString();*/
    return sb.toString();
  }

  public static void logExceptionOnPlatformOrOther(Throwable throwable) {
    if (platformLogger != null && platformLogger.getAllAppenders().hasMoreElements()) {
      platformLogger.error(throwable.getMessage(), throwable);
    } else
      desperatelyLog(throwable.getMessage(), false, throwable);
  }

  public static void desperatelyLog(String message, boolean throwPlatformRuntimeException, Throwable e) {

    if (platformLogger != null && platformLogger.getAllAppenders().hasMoreElements()) {
      platformLogger.fatal(e.getMessage(), e);
      return;
    }

    try {
      if (Logger.getRootLogger() != null)
        Logger.getRootLogger().fatal(message);
    } catch (Exception ex) {
    }

    System.out.println(message);

    if (throwPlatformRuntimeException) {
      if (e != null)
        throw new PlatformRuntimeException(message, e);
      else
        throw new PlatformRuntimeException(message);
    }
  }

  public static void traceHibernateStart() {
    HibernateFactory.getSessionFactory().getStatistics().clear();
  }

  public static String traceHibernateEnd() {
    String qq = "";
    Statistics statistics = HibernateFactory.getSessionFactory().getStatistics();

    for (String q : statistics.getQueries()) {
      qq = qq + q + "\n";
    }
    qq = qq + "\n" + "Total HQL queries: " + statistics.getQueries().length + "\n";

    qq = qq + "----------------------------\n" + "Total queries executed to database: " + statistics.getQueryExecutionCount() + "\n";

    qq = qq + "----------------------------\n" + "Slowest query: " + statistics.getQueryExecutionMaxTimeQueryString() + " TOOK " + statistics.getQueryExecutionMaxTime() + "\n";

    qq = qq + "----------------------------\n" + StringUtilities.replaceAllNoRegex(statistics + "", ",", "\n");

    return qq;
  }


  private static Map<String, Profiler> __profiler = new HashTable<String, Profiler>();

  public static void resetProfilers() {
    __profiler = new HashTable<String, Profiler>();
  }

  public static String printProfilers() {
    String ret = "\n--------------------------------------\n";
    for (Profiler p : __profiler.values())
      ret = ret + "\n" + p;
    return ret;
  }

  public static Profiler getProfiler(String name) {
    Profiler p = __profiler.get(name);
    if (p == null) {
      p = new Profiler(name);
      __profiler.put(name, p);
    }
    p.startMillis = System.currentTimeMillis();
    return p;
  }

  public static JSONObject jsonifyRequest(HttpServletRequest request) {
    JSONObject json = new JSONObject();

    JSONObject jReq = new JSONObject();

    //authentication
    if (request.getUserPrincipal() != null)
      jReq.element("userPrincipal.Name", request.getUserPrincipal().getName() );
    jReq.element("authType", request.getAuthType() );
    jReq.element("remoteUser" , request.getRemoteUser() );
    jReq.element("isSecure" , request.isSecure()  );

    jReq.element("sessionId" , request.getRequestedSessionId() );
    jReq.element("characterEncoding" , request.getCharacterEncoding() );
    jReq.element("method" , request.getMethod());
    jReq.element("timeInMillis",System.currentTimeMillis());

    jReq.element("contextPath" , request.getContextPath() );
    jReq.element("contentType" , request.getContentType() );
    jReq.element("remoteAddr" , request.getRemoteAddr() );
    jReq.element("remoteHost" , request.getRemoteHost() );
    jReq.element("requestURI" , request.getRequestURI() );
    jReq.element("requestURL" , request.getRequestURL() );

    json.element("request",jReq);


    //header
    JSONObject jHead= new JSONObject();
    Enumeration en = request.getHeaderNames();
    while (en.hasMoreElements()) {
      String s = (String) en.nextElement();
      jHead.element(s,request.getHeader(s) );
    }
    if (jHead.size()>0)
      json.element("headers",jHead);


    //attributes
    JSONObject jAttr= new JSONObject();
    Enumeration attr = request.getAttributeNames();
    while (attr.hasMoreElements()) {
      String requestString = (String) attr.nextElement();
      jAttr.element(requestString ,request.getAttribute(requestString) );
    }
    if (jAttr.size()>0)
      json.element("attributes",jAttr);

    //params
    JSONObject jParam = new JSONObject();
    Enumeration parameters = request.getParameterNames();
    while (parameters.hasMoreElements()) {
      String requestString = (String) parameters.nextElement();
      jParam.element(requestString ,request.getParameter(requestString) );
    }
    if (jParam.size()>0)
      json.element("parameters",jParam);


    return json;
  }


  public static class Profiler {
    String name;
    int count = 0;
    long startMillis;
    long duration = 0;


    private Profiler(String name) {
      this.name = name;
    }


    public void stop() {
      count++;
      duration += System.currentTimeMillis() - startMillis;
    }

    public void reset() {
      count = 0;
      startMillis = System.currentTimeMillis();
      duration = 0;
    }

    public String toString() {
      return name + " n.:" + count + " dur.:" + duration;
    }

  }

  public static Throwable getRootCause(Throwable t){
    int hashCode = t.hashCode();
    Throwable cause = t.getCause();
    if (cause!=null && !cause.equals(t))
      return getRootCause(cause);
    else
      return t;
  }
}

