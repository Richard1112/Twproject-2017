<%@ page import="com.twproject.operator.TeamworkOperator, org.jblooming.PlatformExceptionCarrier, org.jblooming.PlatformRuntimeException, org.jblooming.system.ServerInfo, org.jblooming.utilities.HttpUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.SessionState, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.PersistenceConfiguration, org.jblooming.waf.view.PageState, java.io.*, java.text.SimpleDateFormat, java.util.Date, java.util.Properties, org.jblooming.security.License, org.jblooming.utilities.DateUtilities"%>
<%@ page pageEncoding="utf-8" %><%@page isErrorPage="true" %><!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN"><%

  try {

  PlatformExceptionCarrier exceptionCarrier = (PlatformExceptionCarrier)session.getAttribute("PLAT_EXCEPTION");
  Throwable __platformError = (exceptionCarrier==null?null:exceptionCarrier.exception);

  ServerInfo si = new ServerInfo();
  si.lineBreak = "\n";

  PageState pageState = null;
  Throwable theException = null;
  String errorCode = "500"; // by default is a 500
  String errorMessage = "";

  String stktrc = "No stacktrace available";
  String appName = "Teamwork";

  String lang = "en";
  String email = null;
  TeamworkOperator logged = null;

  String clientIp = request.getLocalAddr();
  String lic = "";

  String twversion = "";
  String driver_urls = "";

  String driver_classes = "";
  String dialects = "";

  String username = "";
  String requestUrl = JSP.w(request.getAttribute("javax.servlet.error.request_uri"));

  // this is the standard behaviour catching from "web.xml"
   if (exception != null) {
     theException=exception;
     theException = ((ServletException) theException).getRootCause();

     errorCode = JSP.w(request.getAttribute("javax.servlet.error.status_code"));
     errorMessage = theException.getClass().getName()+": "+ JSP.w(request.getAttribute("javax.servlet.error.message"))+"\n"+
             "servlet path:" + JSP.w(request.getAttribute("javax.servlet.forward.servlet_path")) + "\n" +
             "servlet: " + JSP.w(request.getAttribute("javax.servlet.error.servlet_name"))+"\n"+
             "exception type: "+ JSP.w(request.getAttribute("javax.servlet.error.exception_type"));

     requestUrl = JSP.w(request.getAttribute("javax.servlet.error.request_uri"));


   // this is in strage cases of 404, 500 etc. but usually doesn't works. The real exeption is wrapped
   } else if (request.getAttribute("javax.servlet.error.status_code") != null) {
     theException = (Throwable) request.getAttribute("javax.servlet.error.exception");
     if (theException==null)
       theException=new Exception(JSP.w(request.getAttribute("javax.servlet.error.message")));

     //theException = ((ServletException) theException).getRootCause();

     errorCode = JSP.w(request.getAttribute("javax.servlet.error.status_code"));
     errorMessage = JSP.w(request.getAttribute("javax.servlet.error.message"))+"\n"+
             "servlet path:" + JSP.w(request.getAttribute("javax.servlet.forward.servlet_path")) + "\n" +
             "servlet: " + JSP.w(request.getAttribute("javax.servlet.error.servlet_name"))+"\n"+
             "exception type: "+ JSP.w(request.getAttribute("javax.servlet.error.exception_type"));

     requestUrl = JSP.w(request.getAttribute("javax.servlet.error.request_uri"));

   // exceptions catched from FrontCotrollerFilter are on session wrapped on a PlatformRuntime
   } else if (exceptionCarrier != null) {
     theException=exceptionCarrier.exception;
     requestUrl=exceptionCarrier.requestURI;
     errorMessage=theException.getClass().getName()+": "+JSP.w(theException.getMessage())+"\n"+
             "command: "+JSP.w(exceptionCarrier.command)+"\n"+
             "object id: "+JSP.w(exceptionCarrier.objectID)+"\n"+
             "query string: "+JSP.w(exceptionCarrier.queryString);
   }

   if (theException != null) {
     //get stacktrace as string
     final Writer result = new StringWriter();
     final PrintWriter printWriter = new PrintWriter(result);
     theException.printStackTrace(printWriter);
     stktrc = result.toString();
   }


  try {
     //not using constructor in order to not generate too much
     //pageState = (PageState) request.getAttribute(Fields.VIEW);
     pageState = PageState.getCurrentPageState(request);
     logged = (TeamworkOperator) pageState.getLoggedOperator();
     lang = SessionState.getLocale().getLanguage();
     email = logged.getPerson().getDefaultEmail();
     username = logged.getDisplayName();
   } catch (Throwable t) {
   }


  try {
    License license = License.fromFile();

    lic = lic + "customer=" + license.customerCode;
    lic = lic + "&isEnt=" + (license.enterprise?"yes":"no");
    lic = lic + "&licenseOnPropertyFile=" + license.licenseKey.substring(13)+"...";
    lic = lic + "&expires=" + DateUtilities.dateToString(license.expires, "dd/MM/yyyy") ;
    lic = lic + "&clients=" + license.licenses;

    //try to recover e-mail from license
    if (email == null && license.customerCode.indexOf("@") > 0) {
      email = license.customerCode;
    }
   } catch (Throwable e) {
  }

  try {
    twversion = ApplicationState.getApplicationVersion();
  } catch (Exception e) {
  }

  for (PersistenceConfiguration pc : PersistenceConfiguration.persistenceConfigurations.values()) {
    driver_urls = driver_urls + pc.driver_url + ", ";
    driver_classes = driver_classes + pc.driver_class + ", ";
    dialects = dialects + pc.dialect + ", ";
  }
  //get server time
  SimpleDateFormat sdf = new SimpleDateFormat("yyyy MM dd HH:mm:ss");
  String serverTime = sdf.format(new Date());
  String backTo = ApplicationState.serverURL;
  stktrc = PlatformRuntimeException.getStackTrace(theException) + "";
%>
<script language="javascript">
     if (window != top) top.location.href = location.href;
</script><!-- COPY THIS CODE IN YOUR SERVER ERROR PAGE and fill properties-->
<form action="https://bugsvoice.teamworkhost.com/applications/bugsVoice/site/bv.jsp" id="bugsVoiceSendError" method="post" style="display:none">
  <!-- DO NOT CHANGE START -->
  <input type="text" name="key" value="d937aed0075bcd1b">
  <!-- DO NOT CHANGE END -->
  <textarea name="applicationLicense"><%=lic%></textarea>
  <textarea name="applicationName"><%=appName%></textarea>
  <textarea name="applicationVersion"><%=twversion%></textarea>
  <textarea name="errorCode"><%=errorCode%></textarea>
  <textarea name="backTo"><%=backTo%></textarea>
  <textarea name="clientIp"><%=clientIp%></textarea>
  <textarea name="clientLanguage"><%=lang%></textarea>
   <textarea name="requestUrl"><%=requestUrl%></textarea>
  <textarea name="dbConnection"><%=driver_urls + driver_classes%></textarea>
  <textarea name="dbDriver"><%=dialects%></textarea>
  <textarea name="loggedUserEmail"><%=email%></textarea>
  <textarea name="serverDate"><%=serverTime%></textarea>
  <textarea name="loggedUserName"><%=username%></textarea>
  <textarea name="serverMemory"><%=si.systemState()%></textarea>
  <textarea name="serverIp"><%=request.getRemoteAddr()%></textarea>
  <textarea name="serverOS"><%=si.systemProps(false)%>
  </textarea>
  <textarea name="stackTrace"><%=JSP.htmlEncodeApexes(stktrc)%></textarea>
</form>
<small>Redirecting to collect your feedback...</small>
<script type="text/javascript">
  /* Don't be scared by this JavaScript remote call: it loads a JavaScript file that fills the form with the host and the other properties detectable by JavaScript. */
  document.write(unescape("%3Cscript src='https://bugsvoice.teamworkhost.com/applications/bugsVoice/site/cd.js' type='text/javascript'%3E%3C/script%3E"));
</script><%
  } catch (Throwable t) {
   t.printStackTrace();
  }
%>
