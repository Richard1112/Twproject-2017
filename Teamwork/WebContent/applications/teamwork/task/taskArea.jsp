<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.security.SecurityBricks, com.twproject.security.TeamworkPermissions, com.twproject.task.Task,com.twproject.task.businessLogic.TaskController, com.twproject.waf.TeamworkHBFScreen,
com.twproject.waf.html.TaskHeaderBar, org.jblooming.operator.Operator, org.jblooming.utilities.StringUtilities, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Fields,org.jblooming.waf.html.button.ButtonLink,org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.Combo, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jblooming.waf.constants.Commands, org.jblooming.system.SystemConstants, com.twproject.waf.TeamworkPopUpScreen" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new TaskController(), request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    //this is set by action
    Task task = Task.load(pageState.mainObjectId);
    pageState.setMainObject(task);

    PageSeed ps = pageState.thisPage(request);
    ps.command = "";
    ps.mainObjectId = task.getId();
    Form form = new Form(ps);
    form.alertOnChange = true;
    form.start(pageContext);
    pageState.setForm(form);


%>
<div class="mainColumn">
<%
  if (task.hasPermissionFor(logged,TeamworkPermissions.task_canWrite)) {
%>

<h2><%=I18n.get("ADVANCED_SECURITY_SETTINGS")%></h2>

<table class="table" border="0" cellpadding="5">
  <tr>
    <td><%
      boolean thereIsAreaToMoveTo = false;
      if (task.hasPermissionFor(logged,TeamworkPermissions.task_canWrite)) {

        Combo cb = SecurityBricks.getAreaCombo("TASK_AREA", TeamworkPermissions.project_canCreate, pageState);
        cb.separator="<br>";
        if (!SecurityBricks.isSingleArea() && cb.cvl.size() > 0) {
          cb.toHtmlI18n(pageContext);
          CheckField propag = new CheckField("TASK_AREA_PROPAGATE", "", false);
          propag.toHtmlI18n(pageContext);
          thereIsAreaToMoveTo = true;
        }
      }

    %></td></tr><tr><td><%

      if (!thereIsAreaToMoveTo && task.getArea() != null) {    //just show it
        TextField.hiddenInstanceToHtml("TASK_AREA", pageContext);
    %><label><%=I18n.get("TASK_AREA")%></label>
    <br><b><%=task.getArea().getName()%>
    </b><%
      }



      if (task.getArea() == null) {
    %>
    <b><span class="warning"><%=I18n.get("TASK_AREA_IS_NULL")%></span></b><%
      }


    %></td>
  </tr>
  <tr height="30">
    <td><%
      TextField.hiddenInstanceToHtml("TASK_OWNER", pageContext);
      Operator owner = task.getOwner();%><label><%=I18n.get("TASK_OWNER")%></label>
    <br>
      <table cellpadding="0" cellspacing="0">
        <tr>
          <td>
            <b><%=owner != null ? "(" + owner.getId() + ")&nbsp;" + owner.getDisplayName() : I18n.get("OWNER_UNDEFINED")%>
              &nbsp;&nbsp;&nbsp;&nbsp;</b></td>
          <%
            if (owner == null || !owner.equals(logged)) {
          %>
          <td valign="top"><%
            ButtonSubmit takeOwn = new ButtonSubmit(pageState.getForm());
            takeOwn.variationsFromForm.setCommand("TAKE_OWNERSHIP");
            takeOwn.label = I18n.get("TAKE_OWNERSHIP");
            takeOwn.enabled = logged.hasPermissionAsAdmin();

            CheckField propag = new CheckField("TAKE_OWNERSHIP_PROPAGATE", "&nbsp;", false);
            propag.disabled = !logged.hasPermissionAsAdmin();

          %><%takeOwn.toHtmlI18n(pageContext);%></td>
          <td><%propag.toHtmlI18n(pageContext);%></td>
          <%
            }
          %></tr>
      </table>
    </td>
  </tr>
  <tr>
    <td colspan="2"><%
      PageSeed auditPs = pageState.pageFromCommonsRoot("administration/audits.jsp");
      auditPs.addClientEntry("entityId",task.getId());
      auditPs.addClientEntry("entityClass", Task.class.getName());
      auditPs.command= Commands.FIND;
      ButtonLink audit = new ButtonLink(I18n.get("AUDIT"), auditPs);
      audit.enabled = logged.hasPermissionAsAdmin() && Fields.TRUE.equals(ApplicationState.getApplicationSettings().get(SystemConstants.AUDIT));
      audit.toHtmlInTextOnlyModality(pageContext);
    %></td>
  </tr>
  <tr>
    <td colspan="2" align="right">
      <%
        ButtonBar buttonBar = new ButtonBar();
        ButtonSubmit save = ButtonSubmit.getSaveInstance(pageState);
        save.variationsFromForm.setCommand("TASK_SAVE_SECURITY");
        save.additionalCssClass="first";
        buttonBar.addButton(save);
        buttonBar.toHtml(pageContext);
      %>
    </td>
  </tr>
</table>
<%
} else {
%>
<table width="100%">
  <tr>
    <td height="200px;" valign="top"><%
      if (pageState.getEntry("MASTER_PUBLIC_TASK").checkFieldValue()) {
        pageState.addClientEntry("PUBLIC_TASK_URL", ApplicationState.serverURL + "/project/" + task.getId());
        TextField link = new TextField("PUBLIC_TASK_URL", "&nbsp;&nbsp;");
        link.label = I18n.get("PUBLIC_PAGE_ADDRESS");
        link.fieldSize = 50;
        link.readOnly = true;
        link.toHtml(pageContext);
        %>&nbsp;&nbsp;&nbsp;<a target="_blank" href="<%=ApplicationState.serverURL + "/project/" + task.getId()%>"><%=I18n.get("LINK")%> </a> <%
    } else {
      %><%=I18n.get("NO_PUBLIC_PAGE_FOR_THIS_TASK")%><%
        }
      %></td>
  </tr>
</table>
<%
  }
%></div>


<%
form.end(pageContext);

}

%>
