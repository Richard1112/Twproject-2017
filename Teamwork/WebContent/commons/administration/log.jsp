<%@ page import="com.twproject.waf.TeamworkPopUpScreen,
                 org.apache.log4j.Level,
                 org.jblooming.system.SystemConstants,
                 org.jblooming.tracer.Tracer,
                 org.jblooming.utilities.file.fileStorage.FileStorageUtilities,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.SessionState,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.constants.Fields,
                 org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.container.ButtonBar,
                 org.jblooming.waf.html.container.Container,
                 org.jblooming.waf.html.input.RadioButton,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.settings.PlatformConfiguration,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState,
                 java.io.File"%><%

  PageState pageState = PageState.getCurrentPageState(request);

  pageState.getLoggedOperator().testIsAdministrator();
  
  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);

 } else {


  if (Commands.SAVE.equals(pageState.getCommand())) {
    Tracer.platformLogger.setLevel(Level.toLevel(pageState.getEntry(SystemConstants.FLD_LOG_PLATFORM_LEVEL).intValue()));
    Tracer.hibernateLogger.setLevel(Level.toLevel(pageState.getEntry(SystemConstants.FLD_LOG_HIB_LEVEL).intValue()));
    Tracer.i18nLogger.setLevel(Level.toLevel(pageState.getEntry(SystemConstants.FLD_LOG_I18N_LEVEL).intValue()));
    Tracer.jobLogger.setLevel(Level.toLevel(pageState.getEntry(SystemConstants.FLD_LOG_JOB_LEVEL).intValue()));
    Tracer.emailLogger.setLevel(Level.toLevel(pageState.getEntry(SystemConstants.FLD_LOG_EMAIL_LEVEL).intValue()));


  } else {
    pageState.addClientEntry(SystemConstants.FLD_LOG_PLATFORM_LEVEL, Tracer.platformLogger.getLevel().toInt() + "");
    pageState.addClientEntry(SystemConstants.FLD_LOG_HIB_LEVEL, Tracer.hibernateLogger.getLevel().toInt() + "");
    pageState.addClientEntry(SystemConstants.FLD_LOG_I18N_LEVEL, Tracer.i18nLogger.getLevel().toInt() + "");
    pageState.addClientEntry(SystemConstants.FLD_LOG_JOB_LEVEL, Tracer.jobLogger.getLevel().toInt() + "");
    pageState.addClientEntry(SystemConstants.FLD_LOG_EMAIL_LEVEL, Tracer.emailLogger.getLevel().toInt() + "");
  }

  PageSeed self = pageState.thisPage(request);
  self.setCommand(Commands.SAVE);
  if (pageState.getEntry(Fields.APPLICATION_NAME).stringValueNullIfEmpty() != null)
    self.addClientEntry(Fields.APPLICATION_NAME, pageState.getEntry(Fields.APPLICATION_NAME).stringValueNullIfEmpty());

  Form f = new Form(self);
  pageState.setForm(f);
  f.start(pageContext);


  String root = request.getSession().getServletContext().getRealPath("/").replaceAll("\\\\", "/");
  String logPath = root + "WEB-INF/log/";

  %><script>$("#ADMINISTRATION_ROOT_MENU").addClass('selected');</script>
<h1><%=I18n.get("LOG_CONFIGURATION")%></h1>
<div class="container level_2">
<table cellpadding="5" cellspacing="0"><tr><td><%

  Level levels [] = {Level.ALL,Level.DEBUG, Level.INFO,Level.WARN,Level.ERROR,Level.FATAL};
  %><tr><td>"platform" <%=I18n.get("LOG_LEVEL")%>: </td><td><%
  for (int i = 0; i < levels.length; i++) {
    RadioButton rbs = new RadioButton(levels[i].toString().toLowerCase(), SystemConstants.FLD_LOG_PLATFORM_LEVEL, levels[i].toInt()+"",
                  "", "", false, "");
    rbs.translateLabel = false;
    rbs.toHtml(pageContext);
  }

  %></td></tr><tr><td>"persistence" <%=I18n.get("LOG_LEVEL")%>:</td><td><%
  for (int i = 0; i < levels.length; i++) {
    RadioButton rbs = new RadioButton(levels[i].toString().toLowerCase(), SystemConstants.FLD_LOG_HIB_LEVEL, levels[i].toInt()+"",
                  "", "", false, "");
    rbs.translateLabel = false;
    rbs.toHtml(pageContext);
  }

  %></td></tr>

<tr><td>"i18n" <%=I18n.get("LOG_LEVEL")%>: </td><td><%
  for (int i = 0; i < levels.length; i++) {
    RadioButton rbs = new RadioButton(levels[i].toString().toLowerCase(), SystemConstants.FLD_LOG_I18N_LEVEL, levels[i].toInt()+"",
                  "", "", false, "");
    rbs.translateLabel = false;
    rbs.toHtml(pageContext);
  }
  %></td></tr>

<tr><td>"job" <%=I18n.get("LOG_LEVEL")%>: </td><td><%
  for (int i = 0; i < levels.length; i++) {
    RadioButton rbs = new RadioButton(levels[i].toString().toLowerCase(), SystemConstants.FLD_LOG_JOB_LEVEL, levels[i].toInt()+"",
                  "", "", false, "");
    rbs.translateLabel = false;
    rbs.toHtml(pageContext);
  }
  %></td></tr>

  <tr><td>"email" <%=I18n.get("LOG_LEVEL")%>: </td><td><%
  for (int i = 0; i < levels.length; i++) {
    RadioButton rbs = new RadioButton(levels[i].toString().toLowerCase(), SystemConstants.FLD_LOG_EMAIL_LEVEL, levels[i].toInt()+"",
                  "", "", false, "");
    rbs.translateLabel = false;
    rbs.toHtml(pageContext);
  }
  %></td></tr>
 
  <tr><td>&nbsp;</td></tr>
  <tr><td><%=I18n.get("LOG_OUT_MODALITY")%>: <%=I18n.get(PlatformConfiguration.logOnFile ? "LOG_ON_FILE" : "LOG_ON_CONSOLE")%></td><%

  %></tr>
  </table><%

  if (PlatformConfiguration.logOnFile) {
    Container logFiles = new Container();
    logFiles.title = I18n.get("LOG_FILES");
    logFiles.setCssPostfix("thin");
    logFiles.start(pageContext);
    %> <%=I18n.get("LOG_FILES_IN")%> <%=logPath%>:<%

    File logs = new File(logPath);
    File[] logFls = logs.listFiles();
    if (logFls!=null && logFls.length>0) {
      for (int i = 0; i < logFls.length; i++) {
        File logFl = logFls[i];
        %><%=logFl.getName()%>( <%=FileStorageUtilities.convertFileSize(logFl.length())%>)<br><%
      }
    } else {
      %><%=I18n.get("NO_LOG_FILES")%><%
    }
    logFiles.end(pageContext);
    %><br><%
  }


  
  ButtonBar bb2= new ButtonBar();

  ButtonSubmit save = new ButtonSubmit(f);
  save.label =I18n.get("SAVE");
  bb2.addButton(save);
  bb2.toHtml(pageContext);

  %></div><%
  f.end(pageContext);
}
%>
