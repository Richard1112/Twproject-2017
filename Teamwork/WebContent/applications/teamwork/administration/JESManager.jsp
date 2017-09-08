<%@ page import="com.ericdaugherty.mail.server.Mail, com.twproject.waf.TeamworkPopUpScreen, org.apache.commons.logging.Log, org.apache.commons.logging.LogFactory, org.jblooming.waf.JESSetupListener, org.jblooming.waf.ScreenArea, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, com.ericdaugherty.mail.server.configuration.MailServicesControl, com.ericdaugherty.mail.server.configuration.ConfigurationManager, java.io.File" %>
<%


  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {
    pageState.getLoggedOperator().testIsAdministrator();

    if ("STOP".equals(pageState.command)) {
      %><h1>JES is stopping. Wait.................</h1>
<script>
  $(function(){
    $("body").oneTime(5000,"jesrefresh",function(){
      window.location.href = window.location.pathname; //si ricarica senza parametri
    });
  });
</script>
<%
      JESSetupListener.stopJES();


    } else if ("START".equals(pageState.command)) {
      %><h1>JES is starting. Wait.................</h1>
      <script>
        $(function(){
          $("body").oneTime(5000,"jesrefresh",function(){
            window.location.href = window.location.pathname; //si ricarica senza parametri
          });
        });
      </script>
<%
      JESSetupListener.startJES();

    } else {

      Mail instance = Mail.getInstance();

%><h1>JES instance state:</h1>
  <%
    if (instance == null) {
  %>No istance found<%
} else {
  ConfigurationManager configurationManager = ConfigurationManager.getInstance();

  Log jesLogger = LogFactory.getLog("JESLogger");

  String smtpDirectory = configurationManager.getSMTPDirectory();
  File smtpDir = new File(smtpDirectory);
  int smtpQueueSize=smtpDir.exists()?smtpDir.listFiles().length:0;


%>
  <h3>Platform JES Thread</h3>
  thread is done:  <%=JESSetupListener.mailFuture != null && JESSetupListener.mailFuture.isDone()%><br>
  thread is cancelled:  <%=JESSetupListener.mailFuture != null && JESSetupListener.mailFuture.isCancelled()%><br>


  <br>
  <h3>JES Mail service</h3>
  is running: true<br>
  is restart: <%=instance.isRestart()%><br>
  is restarting: <%=instance.isRestarting()%><br>
  is shutting down: <%=instance.isShuttingDown()%><br>
  <br>
  smtp folder: <%=smtpDirectory%><br>
  smtp queue: <%=smtpQueueSize%><br>

  JES logger: debug=<%=jesLogger.isDebugEnabled()%> info=<%=jesLogger.isInfoEnabled()%> warn=<%=jesLogger.isWarnEnabled()%>  error=<%=jesLogger.isErrorEnabled()%> fatal=<%=jesLogger.isFatalEnabled()%>
  <%

    }

  %>

<hr>
<%

    PageSeed pageSeed = pageState.thisPage(request);

    new ButtonLink("Refresh JES", pageSeed).toHtml(pageContext);


    pageSeed.command = "STOP";
    new ButtonLink("Stop JES", pageSeed).toHtml(pageContext);

    pageSeed.command = "START";
    new ButtonLink("Start JES", pageSeed).toHtml(pageContext);

  }
  }
%>