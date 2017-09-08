<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.waf.TeamworkPopUpScreen, org.jblooming.PlatformRuntimeException, org.jblooming.agenda.CompanyCalendar,org.jblooming.operator.Operator,
org.jblooming.oql.OqlQuery,org.jblooming.persistence.hibernate.PersistenceContext,org.jblooming.scheduler.Scheduler, org.jblooming.system.ServerInfo, org.jblooming.tracer.Tracer,
org.jblooming.utilities.HttpUtilities, org.jblooming.utilities.StringUtilities, org.jblooming.utilities.file.fileStorage.FileStorageUtilities,
org.jblooming.waf.ScreenArea, org.jblooming.waf.SessionState, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink,
org.jblooming.waf.html.container.Container, org.jblooming.waf.html.container.TabSet, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n,
 org.jblooming.waf.settings.PersistenceConfiguration, org.jblooming.waf.settings.PlatformConfiguration, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState,
 java.io.File, java.io.FileInputStream, java.sql.Statement, java.text.ParseException, java.text.SimpleDateFormat, java.util.*, org.jblooming.waf.html.button.ButtonJS, org.jblooming.security.License, org.jblooming.utilities.JSP" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    //put controller !
    final ScreenArea body = new ScreenArea(request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);

    if (pageState.getLoggedOperator() == null || !pageState.getLoggedOperator().hasPermissionAsAdmin() || ApplicationState.isHostMode) {
      throw new SecurityException("Access denied");
    }


    if ("VERSION".equals(pageState.command)) {
      %><%=ApplicationState.getApplicationVersion()%><%
      return;
    } else if ("CHECKPOINT".equals(pageState.command)) {
      PersistenceContext pc = null;
      try {

        pc = PersistenceContext.getDefaultPersistenceContext();
        Statement s = pc.session.connection().createStatement();
        String delSQL = " checkpoint;";
        s.execute(delSQL);
        pc.checkPoint();
      } catch (Throwable e) {
        if (pc != null) {
          //pc.rollbackAndClose();
        }
      }

    }

    pageState.toHtml(pageContext);

} else {

%><h1>Twproject system check</h1>
<div class="container level_2"><h2>Application</h2>
<table><tr><td><%=I18n.get("APP_VERSION")%>:&nbsp;<%=ApplicationState.getApplicationVersion()%></td><%

    //get current version from online
    PageSeed cu = new PageSeed("https://shop.twproject.com/utils/checkUpdates.jsp");
    cu.addClientEntry("TW_CURRENT_VERSION",ApplicationState.getApplicationVersion());
    cu.command = "CHECK_UPDATES";
    ButtonLink scbl = new ButtonLink("Check for updates on the web",cu);
    %><td><%scbl.toHtml(pageContext);%></td></tr><%

    //warn if scheduler is stopped
    if (!Scheduler.isRunning()) {
      PageSeed sched = pageState.pageFromCommonsRoot("/scheduler/scheduleManager.jsp");
      ButtonLink schBL = new ButtonLink(I18n.get("MONITOR"),sched);
      schBL.target="_blank";
      %><tr><td><font class="warning">Scheduler is not running</font></td><td><%schBL.toHtml(pageContext);%></td></tr><%
    }


    //administrators without passwords
    String hql = "select operator from "+ TeamworkOperator.class.getName()+" as operator where operator.administrator=:truth and operator.enabled=:truth";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setBoolean("truth",Boolean.TRUE);
    List<TeamworkOperator> all = oql.list();
    PageSeed edPers = pageState.pageFromRoot("resource/resourceEditor.jsp");
    edPers.command = Commands.EDIT;
    TabSet.pointToTab("resourceTabSet","securityTab",edPers);
    ButtonLink edResBL = new ButtonLink("",edPers);
    edResBL.target ="_blank";

    for (TeamworkOperator operator : all) {
      String prefixedPassword = operator.getLoginName() + "";
      prefixedPassword = StringUtilities.md5Encode(prefixedPassword);
      if (operator.getPassword().equals(prefixedPassword) && operator.isEnabled()) {
        try {
        edResBL.pageSeed.mainObjectId = operator.getPerson().getId();
        edResBL.label = operator.getPerson().getDisplayName();
        %><tr><td><%edResBL.toHtmlInTextOnlyModality(pageContext);%>:</td><td><span class="warning"> has empty password!</span></td></tr><%
      } catch (Exception e) {
        Tracer.platformLogger.error("operator "+operator.getId() +" has no corresponding resource");
      }
    }
  }

%></table><%

  // application root
  %><hr><b><%=I18n.get("APPLICATION_ROOT")%></b>:&nbsp;<%=ApplicationState.webAppFileSystemRootPath%><%


%></div><br>
<div class="container level_2"><h2>License data</h2><%


    String error = (String) ApplicationState.applicationParameters.get("FILTER_ERROR");
    if (error!=null && error.trim().length()>0) {
      %><span class="warning"><%=error%></span>&nbsp;&nbsp;&nbsp;<a href="http://twproject.com" target="_blank">get a license</a><br><%
    }

    License license = License.fromFile();
    %>Number of users: <%=license.licenses%>.&nbsp;<%
    //warn if close to limit
    hql = "select count(op) from "+ Operator.class.getName()+" as op where op.enabled = :truth";
    oql = new OqlQuery(hql);
    oql.getQuery().setBoolean("truth",Boolean.TRUE);
    long totOp = (Long)oql.uniqueResult();
    long remainingUsers = license.licenses- totOp;
    if (Math.abs(remainingUsers)<3) {
      %><span class="warning">You have only <%=remainingUsers%> users available:</span><%

    PageSeed licLink = pageState.pageFromRoot("buyTwprojectLicense.jsp");
    ButtonLink licLinkB = new ButtonLink("Twproject shop",licLink);
      licLinkB.target="_top";
      %><%licLinkB.toHtml(pageContext);%><%

    } else {
      %>There are <%=totOp%> users created.<%
    }

    %><br>License expires: <%=JSP.w(license.expires)%>.&nbsp;<%
    long daysRemaining = (license.expires.getTime() - System.currentTimeMillis()) / CompanyCalendar.MILLIS_IN_DAY;
    if (daysRemaining < 20 && remainingUsers>=3) {
      %><span class="warning">You have only <%=daysRemaining%> usage days remaining:</span><%

      PageSeed licLink = pageState.pageFromRoot("buyTwprojectLicense.jsp");
      ButtonLink licLinkB = new ButtonLink("Twproject shop",licLink);
      licLinkB.target="_blank";
      %><%licLinkB.toHtml(pageContext);%><%
    } else {
      %><br><%
    }

    //get current version from online
    PageSeed reg = pageState.pageInThisFolder("register.jsp",request);
    ButtonLink regB = new ButtonLink("Insert new Twproject license",reg);
    %><br><%regB.toHtml(pageContext);%><br><%

%></div><br>
<div class="container level_2"><h2>Server information</h2><%


    ServerInfo si = new ServerInfo();
    %><b>Databases</b><br><%

   for (PersistenceConfiguration pc : PersistenceConfiguration.persistenceConfigurations.values()){

    %>Connected to: <%=ApplicationState.platformConfiguration!=null ? pc.driver_url : "no connection"%><br>
    with user: <%=ApplicationState.platformConfiguration!=null ? pc.db_user_name : "no connection"%><br> <%
    if (pc.dialect.getName().toUpperCase().indexOf("HSQL")>-1) {
      %><br><br><span class="warning">Using HSQLDB as database: remember to frequently backup the folder: <b><%=ApplicationState.platformConfiguration!=null ? pc.driver_url.substring(12) : "no connection"%></b></span><br><%
      PageSeed c = pageState.thisPage(request);
      c.command = "CHECKPOINT";
      ButtonJS cb = new ButtonJS("Checkpoint","window.location.href='"+c.toLinkToHref()+"'");
      cb.confirmQuestion = "This operation will clear the content of your HsqlDb log file transfeirng it in the script file.<br>Perform this operation only if you know which the consequences are.Do you want to proceed?";
      cb.confirmRequire = true;
      cb.toHtml(pageContext);
       %>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<i>Clear the HsqlDb log file transfering its content in the script file</i>
  <br><%
    }
  }
%>

<br>
    <b>System properties</b><br>
    <%=si.systemProps(false)%><br>
    <b>System state</b><br>
    <%=si.systemState()%><br><%

  //warn about low allowed memory
  if ( (si.getMaxMemory()/1024)<70E3 ) {
    %><span class="warning">Max memory setting is too low: allow at least 128MB.</span><br><%
  }

  //warn about low memory in use
  if ( (si.getFreeMemory()/1024)<10E3 ) {
    %><span class="warning">Free memory is too low: increase max memory setting.</span><br><%
  }

  Container allProps = new Container();
  allProps.title = "All properties";
  allProps.level=1;
  allProps.collapsable=true;
  allProps.status=Container.COLLAPSED;
  allProps.start(pageContext);
  //... Add property list data to text area.
  Properties pr = System.getProperties();
  TreeSet propKeys = new TreeSet(pr.keySet());  // TreeSet sorts keys
  for (Iterator it = propKeys.iterator(); it.hasNext(); ) {
    String key = (String)it.next();
    %><%=key%>=<%=pr.get(key)%><br><%
  }
  allProps.end(pageContext);

  %></div><%

  String root = request.getSession().getServletContext().getRealPath("/").replaceAll("\\\\", "/");
  String logPath = root + "WEB-INF/log/";

  if (PlatformConfiguration.logOnFile) {
    %><div class="container level_2"><h2><%=I18n.get("LOG_FILES")%></h2>
    <%=I18n.get("LOG_FILES_IN")%> <%=logPath%>:<%

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
%></div><%  }



  }
%>
