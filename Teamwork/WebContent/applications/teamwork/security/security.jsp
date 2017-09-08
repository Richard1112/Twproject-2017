<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.security.SecurityBricks, com.twproject.security.TeamworkArea, com.twproject.setup.WizardSupport,
com.twproject.waf.TeamworkHBFScreen, org.jblooming.oql.OqlQuery, org.jblooming.security.Area, org.jblooming.security.PlatformPermissions,
org.jblooming.security.Role, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands,
org.jblooming.waf.constants.Fields, org.jblooming.waf.exceptions.ActionException, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.Container,
org.jblooming.waf.html.container.HeadBar, org.jblooming.waf.html.input.Combo, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState" %><%
   PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;

    final ScreenArea body = new ScreenArea(request);
    body.areaHtmlClass="lreq30 lreqPage";
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    logged.testIsAdministrator();


    //controller
    StringBuffer feedback = new StringBuffer();
    pageState.setAttribute("feedback", feedback);

    if ("SAVE_SECUR_STD".equals(pageState.command)) {
      pageState.initializeEntries("table");

      String areaName = null;
      try {
        areaName = pageState.getEntryAndSetRequired("WIZ_CREATE_DEFAULT_AREA_NAME").stringValue();
      } catch (ActionException e) {
      }
      if (JSP.ex(areaName)) {
        WizardSupport.createAreaAndStandardRoles(areaName, logged, feedback);
        //empty client entries
        pageState.addClientEntry("WIZ_CREATE_DEFAULT_SECURITY", Fields.FALSE);
        pageState.addClientEntry("WIZ_CREATE_DEFAULT_AREA_NAME", "");
      }

      
    } else if ("ADD_SCRUM_ROLES".equals(pageState.command)) {
      pageState.initializeEntries("table");
      TeamworkArea area= TeamworkArea.load(pageState.getEntry("ADD_ROLES_AREA").intValueNoErrorCodeNoExc());
        if (area !=null){
          WizardSupport.addScrumRolesOnArea(area, logged, feedback);
        }

    } else if ("SAVE_SECUR_SCRUM".equals(pageState.command)) {
      pageState.initializeEntries("table");

      String areaName = null;
      try {
        areaName = pageState.getEntryAndSetRequired("WIZ_CREATE_DEFAULT_AREA_NAME_SCRUM").stringValue();
      } catch (ActionException e) {
      }
       if (JSP.ex(areaName)) {

        WizardSupport.createAreaAndScrumRoles(areaName, logged, feedback);

        //empty client entries
        pageState.addClientEntry("WIZ_CREATE_DEFAULT_SECURITY", Fields.FALSE);
        pageState.addClientEntry("WIZ_CREATE_DEFAULT_AREA_NAME", "");

      }

    }    pageState.toHtml(pageContext);

  } else {

    final TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    logged.testIsAdministrator();

    PageSeed self = pageState.thisPage(request);
    Form f = new Form(self);
    pageState.setForm(f);
    f.start(pageContext);


/*    HeadBar hb = new HeadBar();

    hb.addButton(new ButtonLink(I18n.get("AREA_MANAGEMENT"), pageState.pageInThisFolder("area.jsp", request)));
    hb.addButton(new ButtonLink(I18n.get("ROLE_MANAGEMENT"), pageState.pageInThisFolder("roleList.jsp", request)));
    hb.addButton(new ButtonLink(I18n.get("SECURITY_TEST"), pageState.pageFromRoot("administration/securityTest.jsp")));

    PageSeed ps = pageState.pageFromRoot("resource/resourceSecurity.jsp");
    ps.mainObjectId = logged.getPerson().getId();
    ps.command = Commands.EDIT;
    ButtonLink bl = new ButtonLink(I18n.get("MY_SECURITY_SETTINGS"),ps);
    bl.iconChar="&rsquo;";
    hb.addButton(bl);

    hb.toHtml(pageContext);*/


    %><script>$("#TOOLS_MENU").addClass('selected');</script>

<h1><%=I18n.get("SECURITY_INTRO")%></h1>

<table class="table"><%

   %><tr><td><h3><%=I18n.get("TEAMWORK_ROLE_BASED_CONCEPT")%></h3></td></tr><%

    String hql = "select count(a.id) from "+ Area.class.getName()+" as a";
    OqlQuery oql = new OqlQuery(hql);
    long totArea = (Long)oql.uniqueResult();
    hql = "select count(a.id) from "+ Role.class.getName()+" as a";
    oql = new OqlQuery(hql);
    long totRol = (Long)oql.uniqueResult();

    %>

  <tr><td colspan="2"><h3><%=I18n.get("TOTAL_AREAS_%%_TOTAL_ROLES_%%",totArea+"",totRol+"")%></h3></td></tr><%

%></table>

<div class="inlineContainerWrapper">
  <div class="container" style="width: 33%">
    <h2><%=I18n.get("CREATE_AREA")%></h2>
    <table border="0"><%

      //enable checkbox if no areas
      hql = "select count(area) from "+ Area.class.getName()+" as area";
      long totAreas = (Long)new OqlQuery(hql).uniqueResult();
      if (totAreas==0 && pageState.command==null)
        pageState.addClientEntry("WIZ_CREATE_DEFAULT_SECURITY",Fields.TRUE);

    %><tr><td colspan="2"><%=I18n.get("WIZ_CREATE_DEFAULT_SECURITY_DESCR")%></td></tr><%
    %><tr><td><%
      TextField areaName = new TextField("","WIZ_CREATE_DEFAULT_AREA_NAME", "", 45);
      areaName.innerLabel = I18n.get("WIZ_CREATE_DEFAULT_AREA_NAME");
      areaName.label = "";
      areaName.fieldClass = "formElements formElementsBig";
      areaName.toHtml(pageContext);
    %></td><td><%
      ButtonSubmit standardRoles = ButtonSubmit.getSaveInstance(f, I18n.get("CREATE"));
      standardRoles.variationsFromForm.command = "SAVE_SECUR_STD";
      standardRoles.additionalCssClass = "first big";
      standardRoles.toHtml(pageContext);
    %></td></tr></table>
  </div>

<div class="container" style="width: 33%">
  <h2><%=I18n.get("CREATE_AREA_SCRUM")%></h2>
  <table border="0"><%
  %><tr><td colspan="2"><%=I18n.get("WIZ_CREATE_DEFAULT_SECURITY_SCRUM_DESCR")%></td></tr><%
  %><tr><td><%
    areaName = new TextField("","WIZ_CREATE_DEFAULT_AREA_NAME_SCRUM", "", 45);
    areaName.innerLabel = I18n.get("WIZ_CREATE_DEFAULT_AREA_NAME");
    areaName.label = "";
    areaName.fieldClass = "formElements formElementsBig";
    areaName.toHtml(pageContext);
  %></td><td><%
    ButtonSubmit SCRUMRoles = ButtonSubmit.getSaveInstance(f, I18n.get("CREATE"));
    SCRUMRoles.variationsFromForm.command = "SAVE_SECUR_SCRUM";
    SCRUMRoles.additionalCssClass = "first big";
    SCRUMRoles.toHtml(pageContext);
  %></tr>
  </table>
</div>


<div class="container" style="width: 33%">
  <h2><%=I18n.get("WIZ_ADD_DEFAULT_SECURITY_SCRUM_DESCR")%></h2>
  <table border="0">
    <tr><td width="3%" nowrap><%
      Combo cbb = SecurityBricks.getAreaCombo("ADD_ROLES_AREA", PlatformPermissions.role_canRead, pageState);
      cbb.separator="";
      cbb.cvl.addChoose(pageState);
      cbb.fieldClass = "formElements formElementsBig";
      cbb.label="";
      cbb.toHtml(pageContext);

    %></td><td><%

      ButtonSubmit addSCRUMRoles = ButtonSubmit.getSaveInstance(f, I18n.get("ADD"));
      addSCRUMRoles.variationsFromForm.command = "ADD_SCRUM_ROLES";
      addSCRUMRoles.additionalCssClass = "first big";
      addSCRUMRoles.toHtml(pageContext);
    %></td></tr></table>
</div>

</div>

<%

  f.end(pageContext);

   StringBuffer feedback = (StringBuffer) pageState.getAttribute("feedback");

  if (feedback!=null && feedback.length()>0) {
    %><p align="center"><%
    Container fb = new Container();
  fb.title = "Wizard feedback";
  fb.centeredOnScreen = true;
  fb.closeable = true;

  fb.draggable = true;
  fb.width = "400px";
  fb.height = "500px";
  fb.overflow="auto";
  fb.start(pageContext);
      %><%=feedback.toString()%><%
    fb.end(pageContext);
      %></p><%

  }


   }
%>
