<%@ page
        import="com.twproject.operator.TeamworkOperator, com.twproject.resource.Person, com.twproject.resource.ResourceBricks,
        com.twproject.security.TeamworkPermissions, com.twproject.task.Task, com.twproject.task.TaskBricks,
        com.twproject.waf.TeamworkHBFScreen, org.jblooming.persistence.PersistenceHome, org.jblooming.security.Permission,
        org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.html.button.ButtonLink,
        org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.SmartCombo, org.jblooming.waf.html.state.Form,
        org.jblooming.waf.settings.Application, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n,
        org.jblooming.waf.view.PageState, java.util.TreeSet" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    if (!logged.hasPermissionAsAdmin())
      throw new SecurityException("Not admin");

    final ScreenArea body = new ScreenArea(request);
    body.areaHtmlClass="lreq30 lreqPage";
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);

  } else {



    Form f = new Form(pageState.thisPage(request));
    pageState.setForm(f);
    f.start(pageContext);

    %><script>$("#ADMINISTRATION_ROOT_MENU").addClass('selected');</script>
<%
  ButtonLink adminLink = new ButtonLink(I18n.get("ADMINISTRATION_ROOT_MENU") + " /",pageState.pageFromRoot("administration/administrationIntro.jsp"));
%>
<%adminLink.toHtmlInTextOnlyModality(pageContext);%>
<h1>Security test page</h1>
<div class="optionsBar clearfix">
<div class="filterElement"><%
          SmartCombo sc = ResourceBricks.getPersonCombo("PERSON",true,null,pageState);
          sc.label="Pick user";
          sc.separator="<br>";
          sc.toHtml(pageContext);
        %></div>
  <div class="filterElement"><%
          SmartCombo task = TaskBricks.getTaskCombo("TASK",false, TeamworkPermissions.task_canRead,pageState);
          task.label="Pick task";
          task.separator="<br>";
          task.toHtml(pageContext);
        %></div>
    <div class="filterElement centered"><%
          CheckField ONLY_TRUE = new CheckField("ONLY_TRUE","&nbsp;",false);
          ONLY_TRUE.label="Show only enabled permissions";
          ONLY_TRUE.toHtml(pageContext);
        %></div>
  <div class="filterElement centered"><%
    ButtonSubmit bs = ButtonSubmit.getSearchInstance(f,pageState);
    bs.additionalCssClass="small";
    bs.toHtml(pageContext);
  %></div>
</div>
    <%


    String pId = pageState.getEntry("PERSON").stringValueNullIfEmpty();
    String tId = pageState.getEntry("TASK").stringValueNullIfEmpty();

    boolean wantsOnlyTrue = pageState.getEntry("ONLY_TRUE").checkFieldValue();

      if (JSP.ex(pId,tId)) {
        Person p = (Person) PersistenceHome.findByPrimaryKey(Person.class,pId);
        Task t = (Task) PersistenceHome.findByPrimaryKey(Task.class,tId);
    %><table class="table">
      <tr class="tableSection"><td colspan="2" class="childNode"><h2><%=p.getDisplayName()%> (of area <%=p.getArea().getName()%>) on <b><%=t.getDisplayName()%></b> (of area <%=t.getArea().getName()%>):</h2></td></tr>
      <tr><th class="tableHead">permission</th><th class="tableHead">result</th></tr><%

      TreeSet<Permission> permissions = new TreeSet<Permission>(ApplicationState.platformConfiguration.defaultApplication.getPermissions());



      %><tr><th colspan="2" class="tableHead secondRow">Twproject and Website permissions</th></tr><%
      for (Permission perm: permissions) {
        boolean has = t.hasPermissionFor(p.getMyself(), perm);
        if (!wantsOnlyTrue || has) {
          %><tr class="alternate"><td width="30%" nowrap><%=I18n.get(perm.getName())%> (<%=perm.getName()%>)</td><td><%=has%></td></tr><%
        }
      }

      %><tr><th colspan="2" class="tableHead secondRow">Platform permissions</th></tr><%
      permissions = new TreeSet(ApplicationState.platformConfiguration.applications.get(Application.PLATFORM_APP_NAME).getPermissions());

      for (Permission perm: permissions) {
        boolean has = t.hasPermissionFor(p.getMyself(), perm);
        if (!wantsOnlyTrue || has) {
          %><tr><td width="30%" nowrap><%=I18n.get(perm.getName())%> (<%=perm.getName()%>)</td><td><%=has%></td></tr><%
        }
      }

  }

      %></table><%



    f.end(pageContext);

  }
%>