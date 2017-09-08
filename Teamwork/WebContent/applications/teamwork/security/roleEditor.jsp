<%@ page
  import="com.opnlb.website.security.WebSitePermissions, com.twproject.operator.TeamworkOperator, com.twproject.security.RoleTeamwork,
  com.twproject.security.SecurityBricks, com.twproject.security.TeamworkPermissions, com.twproject.security.businessLogic.TeamworkRoleController,
  com.twproject.task.Task, com.twproject.waf.TeamworkHBFScreen, com.twproject.waf.settings.ReportBricks,
  org.jblooming.messaging.MessagingSystem, org.jblooming.security.Permission, org.jblooming.security.PlatformPermissions, org.jblooming.utilities.CodeValue,
  org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.constants.RoleConstants,
  org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.display.DeletePreviewer,
  org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.Combo, org.jblooming.waf.html.input.TextArea,
  org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n,
  org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.ArrayList, java.util.List" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new TeamworkRoleController(), request);
    body.areaHtmlClass="lreq30 lreqPage";
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);

    pageState.toHtml(pageContext);

  } else {

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    RoleTeamwork role = (RoleTeamwork) pageState.getMainObject();

    boolean isResourceRole=role.equals(RoleTeamwork.getMyselfRole()) || role.equals(RoleTeamwork.getManagerRole());
    boolean isSystemRole=role.getArea()==null&&!isResourceRole;
    boolean isLocalToAssignment=role.isLocalToAssignment();
    boolean isGlobalRole=!isLocalToAssignment && !isSystemRole && !isResourceRole;



    boolean canWriteRole = role.isNew() || role.hasPermissionFor(logged, PlatformPermissions.role_canWrite) ;
    boolean canCreateRole = role.isNew() || role.hasPermissionFor(logged, PlatformPermissions.role_canCreate);



    PageSeed self = pageState.thisPage(request);
    self.setCommand(Commands.EDIT);
    self.setMainObjectId(role.getId());
    self.addClientEntry(pageState.getEntry("LOCAL_TO_ASS"));
    Form f = new Form(self);
    f.alertOnChange = true;

    pageState.setForm(f);
    f.start(pageContext);




    TextField code = new TextField(I18n.get(RoleConstants.FLD_ROLE_CODE), RoleConstants.FLD_ROLE_CODE, "<br>", 10, false);
    code.readOnly = !canWriteRole || isResourceRole;
    code.required = true;

    TextField name = new TextField(I18n.get(RoleConstants.FLD_ROLE_NAME), RoleConstants.FLD_ROLE_NAME, "<br>", 30, false);
    name.readOnly = !canWriteRole;
    name.required = true;

    pageState.setFocusedObjectDomId(name.id);


%>
<h1><a href="roleList.jsp"><%=I18n.get("ROLES")%></a> / <%= (role.getName() != null ? role.getName() : I18n.get("NEW"))%></h1>
<table border="0" cellpadding="5">
  <tr>
    <td><% code.toHtml(pageContext);%></td>
    <td><% name.toHtml(pageContext);%></td>
    <%
      Combo areas = SecurityBricks.getAreaCombo("AREA", canWriteRole ? PlatformPermissions.role_canWrite : PlatformPermissions.role_canRead,pageState);
      for (CodeValue cv : areas.cvl.codeValues) {
        cv.value = I18n.get("AREA")+": "+cv.value;
      }
      areas.cvl.addChoose(pageState);
      if (logged.hasPermissionAsAdmin() && !role.isLocalToAssignment()) {
        areas.cvl.add("SYSTEM_ROLE",I18n.get("SYSTEM_ROLE"));
      }
      areas.required = isGlobalRole||isLocalToAssignment;
      areas.separator = "<br>";
      areas.script = " onChange=\"if (this.value=='') {obj('ck_LOCAL_TO_ASS').disabled=true;obj('ck_LOCAL_TO_ASS').checked=false;obj('LOCAL_TO_ASS').value='no'} else {obj('ck_LOCAL_TO_ASS').disabled=false;}\"";
      areas.disabled = !canWriteRole  || isResourceRole;
    %>
    <td class="<%=SecurityBricks.isSingleArea()?"displayNone":""%>"><% areas.toHtmlI18n(pageContext);%></td>
  </tr>
  <%
    TextArea description = new TextArea(I18n.get(RoleConstants.FLD_ROLE_DESCRIPTION), RoleConstants.FLD_ROLE_DESCRIPTION, "<br>", 80,6, null);
    description.readOnly = !canWriteRole;
    description.maxlength=2000;
  %><tr><td valign="top" colspan="3"><% description.toHtml(pageContext);%></td></tr>
  </table>

<%

  // ---------------------------- global permissions -----------------------------------------------
  List<Permission> globalPermissions= new ArrayList<Permission>();
  globalPermissions.add(PlatformPermissions.area_canManage);
  globalPermissions.add(PlatformPermissions.role_canCreate);
  globalPermissions.add(PlatformPermissions.role_canRead);
  globalPermissions.add(PlatformPermissions.role_canWrite);
  globalPermissions.add(TeamworkPermissions.resource_canRead);
  globalPermissions.add(TeamworkPermissions.resource_canWrite);
  globalPermissions.add(TeamworkPermissions.resource_canCreate);
  globalPermissions.add(TeamworkPermissions.resource_canDelete);
  globalPermissions.add(TeamworkPermissions.resource_cost_canRead);
  globalPermissions.add(TeamworkPermissions.resource_manage);
  globalPermissions.add(TeamworkPermissions.assignment_canCRW);
  globalPermissions.add(TeamworkPermissions.worklog_manage);
  globalPermissions.add(TeamworkPermissions.expense_manage);
  globalPermissions.add(TeamworkPermissions.project_canCreate);
  globalPermissions.add(TeamworkPermissions.task_canRead);
  globalPermissions.add(TeamworkPermissions.task_canCreate);
  globalPermissions.add(TeamworkPermissions.task_canDelete);
  globalPermissions.add(TeamworkPermissions.task_canWrite);
  globalPermissions.add(TeamworkPermissions.task_canChangeStatus);
  globalPermissions.add(TeamworkPermissions.task_cost_canRead);
  globalPermissions.add(TeamworkPermissions.task_cost_canCreate);
  globalPermissions.add(TeamworkPermissions.task_cost_canWrite);
  globalPermissions.add(TeamworkPermissions.issue_canRead);
  globalPermissions.add(TeamworkPermissions.issue_canWrite);
  globalPermissions.add(TeamworkPermissions.issue_canCreate);
  globalPermissions.add(TeamworkPermissions.issue_canDelete);
  globalPermissions.add(TeamworkPermissions.issue_canChangeStatus);
  globalPermissions.add(TeamworkPermissions.document_canRead);
  globalPermissions.add(TeamworkPermissions.document_canWrite);
  globalPermissions.add(TeamworkPermissions.document_canCreate);
  globalPermissions.add(TeamworkPermissions.document_canDelete);
  globalPermissions.add(TeamworkPermissions.board_canRead);
  globalPermissions.add(TeamworkPermissions.board_canWrite);
  globalPermissions.add(TeamworkPermissions.board_canCreate);
  globalPermissions.add(TeamworkPermissions.fileStorage_canRead);

  if (! Fields.TRUE.equals(ApplicationState.applicationParameters.get("TEAMWORK_ASP_INSTANCE"))) {
    globalPermissions.add(TeamworkPermissions.fileStorage_canWrite);
    globalPermissions.add(TeamworkPermissions.fileStorage_canCreate);
  }

  globalPermissions.add(TeamworkPermissions.fileStorage_explorer_canRead);
  globalPermissions.add(TeamworkPermissions.fileStorage_explorer_canWrite);
  globalPermissions.add(TeamworkPermissions.fileStorage_explorer_canCreate);
  globalPermissions.add(TeamworkPermissions.classificationTree_canManage);



  // ---------------------------- project permissions -----------------------------------------------
  List<Permission> projectPermissions= new ArrayList<Permission>();
  projectPermissions.add(TeamworkPermissions.resource_canRead);
  projectPermissions.add(TeamworkPermissions.resource_manage);
  projectPermissions.add(TeamworkPermissions.task_canCreate);
  projectPermissions.add(TeamworkPermissions.task_canDelete);
  projectPermissions.add(TeamworkPermissions.task_canRead);
  projectPermissions.add(TeamworkPermissions.task_canWrite);
  projectPermissions.add(TeamworkPermissions.task_canChangeStatus);
  projectPermissions.add(TeamworkPermissions.task_cost_canRead);
  projectPermissions.add(TeamworkPermissions.task_cost_canCreate);
  projectPermissions.add(TeamworkPermissions.task_cost_canWrite);
  projectPermissions.add(TeamworkPermissions.assignment_canCRW);
  projectPermissions.add(TeamworkPermissions.worklog_manage);
  projectPermissions.add(TeamworkPermissions.expense_manage);
  projectPermissions.add(TeamworkPermissions.issue_canRead);
  projectPermissions.add(TeamworkPermissions.issue_canWrite);
  projectPermissions.add(TeamworkPermissions.issue_canCreate);
  projectPermissions.add(TeamworkPermissions.issue_canDelete);
  projectPermissions.add(TeamworkPermissions.issue_canChangeStatus);
  projectPermissions.add(TeamworkPermissions.document_canRead);
  projectPermissions.add(TeamworkPermissions.document_canWrite);
  projectPermissions.add(TeamworkPermissions.document_canCreate);
  projectPermissions.add(TeamworkPermissions.document_canDelete);
  
  projectPermissions.add(TeamworkPermissions.report_canRead);
  projectPermissions.add(TeamworkPermissions.report_canWrite);
  projectPermissions.add(TeamworkPermissions.report_canCreate);
  projectPermissions.add(TeamworkPermissions.report_canDelete);
  
  projectPermissions.add(TeamworkPermissions.fileStorage_explorer_canRead);
  projectPermissions.add(TeamworkPermissions.fileStorage_explorer_canWrite);
  projectPermissions.add(TeamworkPermissions.fileStorage_explorer_canCreate);
  
  projectPermissions.add(TeamworkPermissions.task_audit_canCreate);
  projectPermissions.add(TeamworkPermissions.task_audit_canAudit);
  projectPermissions.add(TeamworkPermissions.task_audit_canRCreate);



  // ---------------------------- system permissions -----------------------------------------------
  List<Permission> systemPermissions= new ArrayList<Permission>();
  systemPermissions.add(PlatformPermissions.i18n_manage);
  systemPermissions.add(PlatformPermissions.schedule_manage);
  systemPermissions.add(TeamworkPermissions.classificationTree_canManage);
  systemPermissions.add(TeamworkPermissions.system_canManageCalendar);
  systemPermissions.add(WebSitePermissions.news_canManage);
  systemPermissions.add(WebSitePermissions.news_canWrite);


  // ---------------------------- resource - permissions -----------------------------------------------
  List<Permission> resourcePermissions= new ArrayList<Permission>();
  resourcePermissions.add(TeamworkPermissions.resource_canRead);
  resourcePermissions.add(TeamworkPermissions.resource_canWrite);
  resourcePermissions.add(TeamworkPermissions.resource_manage);
  resourcePermissions.add(TeamworkPermissions.resource_cost_canRead);

  resourcePermissions.add(TeamworkPermissions.task_canRead);

  resourcePermissions.add(TeamworkPermissions.issue_canRead);
  resourcePermissions.add(TeamworkPermissions.issue_canChangeStatus);


  // ---------------------------- custom & report permissions -----------------------------------------------
  List<Permission> customPermissions= new ArrayList<Permission>();

  //inject all other permissions System Custom_
  for (Permission perm:ApplicationState.getPermissions()){
    //check if already used
    if (perm.name.toLowerCase().startsWith("custom_") ){
      customPermissions.add(perm);
    } else if (!projectPermissions.contains(perm) && ! globalPermissions.contains(perm)) {
      systemPermissions.add(perm);
    }
  }


  String title="";
  List<Permission>permissions=null;
  if (isResourceRole){
    title=I18n.get("RESOURCE_PERMISSIONS");
    permissions=resourcePermissions;
  } else if (isSystemRole){
    title=I18n.get("SYSTEM_PERMISSIONS");
    permissions=systemPermissions;
  } else if (isLocalToAssignment){
    title=I18n.get("PROJECT_LOCAL_PERMISSIONS");
    permissions=projectPermissions;
  }  else { //isGlobalRole
    title=I18n.get("AREA_LOCAL_PERMISSIONS");
    permissions=globalPermissions;

  }

%>
<h2><%=title%></h2>
  <table class="table dataTable">
  <tr>
    <thead class="dataTableHead">
    <th class="tableHead"><%=I18n.get("NAME")%></th>
    <th class="tableHead">&nbsp;</th>
    <th class="tableHead"><%=I18n.get("DESCRIPTION")%></th>
    </thead>
  </tr>
    <%
      for (Permission perm:permissions){
        %><tr class="alternate" ><td><%new CheckField(perm.name,"</td><td align=\"center\">",true).toHtmlI18n(pageContext);%></td> <td><%=I18n.get(perm.name + "_DESCRIPTION")%></td></tr><%
      }


      // -----------------  CUSTOM PERMISSIONS su ruoli area a progetto -------------------------
      if (customPermissions.size()>0 && (isLocalToAssignment || isGlobalRole )){
        %><thead class="dataTableHead"><tr class="alternate" ><th colspan="4" class="tableHead"><%=I18n.get("CUSTOM_PERMISSIONS")%></th></tr></thead><%
        for (Permission perm:customPermissions){
            %><tr class="alternate" ><td><%
            CheckField cf = new CheckField(perm.name, "</td><td align=\"center\">", true);
            String permName = perm.name.substring(7);
            cf.label=permName;
            cf.toHtmlI18n(pageContext);

            String permDescription = I18n.get("DEFAULT_CUSTOM_PERM_DESCR_%%",permName);
            if (I18n.isActive(perm.name+"_DESCRIPTION"))
              permDescription=I18n.get(perm.name+"_DESCRIPTION");
            %></td> <td><%=permDescription%></td></tr><%
         }
       }


    // -----------------  REPORTS PERMISSIONS su ruoli area a progetto -------------------------
    if (ReportBricks.getAllReports().size()>0  && (isLocalToAssignment || isGlobalRole )){
      %><thead class="dataTableHead"><tr class="alternate" ><th colspan="4" class="tableHead"><%=I18n.get("REPORT_PERMISSIONS")%></th></tr></thead><%
      for (ReportBricks.Report report:ReportBricks.getAllReports()){
        %><tr class="alternate" ><td><%
        CheckField cf = new CheckField(report.requiredPermission.name, "</td><td align=\"center\">", true);
        cf.label="report "+ report.name; //sono nella forma: "Report_tasklist"
        cf.toHtmlI18n(pageContext);%></td> <td><%=I18n.get("REPORT_PERMISSION_%",report.name)%></td></tr><%
        }
      }

    %></table> <%


  // -----------------  SUBSCRIPTIONS su ruoli di progetto -------------------------
  if (isLocalToAssignment){
    %>
    <span style="float: right"><%
      CheckField propDesc = new CheckField("TASK_NOTIFY_DESC", "&nbsp;", false);
      propDesc.label = "<small>" + I18n.get("TASK_NOTIFY_DESC") + "</small>";
      propDesc.disabled = !canWriteRole;
      propDesc.toHtml(pageContext);
    %></span>
    <h2><%=I18n.get("SELECT_DEFAULT_SUBSCRIPTIONS")%></h2>
    <table class="table">
      <thead class="dataTableHead">
      <tr>
        <th class="tableHead"><%=I18n.get("EVENT_TYPE")%></th>
        <%for (MessagingSystem.Media media : MessagingSystem.activeMedia) {%>
        <th class="tableHead"><%=I18n.get(media.toString().toUpperCase())%> <%
          List<String> cks = new ArrayList<String>();
          for (Task.Event event : Task.Event.values()) {
            cks.add(event + "_" + media.toString().toUpperCase());
          }
          cks.add("ASSIGNEE_NOTIFY_" + media.toString().toUpperCase());
          CheckField cf = CheckField.getMasterCheckField("SELECT_ALL", cks);

          cf.disabled = !canWriteRole;
          cf.preserveOldValue = false;
          cf.label = "";
          cf.toHtml(pageContext);
        %>
        </th>
        <%}%>
      </tr>
      </thead>
      <%for (Task.Event event : Task.Event.values()) {%>
      <tr class="alternate">
        <td><%=I18n.get(event.toString())%></td>
        <%for (MessagingSystem.Media media : MessagingSystem.activeMedia) {%>
        <td align="center">
          <%
            String subscrField = event + "_" + media.toString().toUpperCase();

            CheckField cf = new CheckField(subscrField, "", true);
            cf.label = "";
            cf.disabled = !canWriteRole;
            cf.toHtml(pageContext);
          %>
        </td>
        <%}%>
      </tr>
      <%}%>
    </table>
    <%
  }


    ButtonBar bb2 = new ButtonBar();

    if (!role.isNew())
      bb2.loggableIdentifiableSupport = role;

    ButtonSubmit save = ButtonSubmit.getSaveInstance(f, I18n.get("SAVE"));
    save.enabled = canWriteRole;
    save.additionalCssClass = "first big";
    bb2.addButton(save);

    if (!role.isNew() && !isResourceRole) {
      DeletePreviewer deletePreviewer = new DeletePreviewer("ROLE_DEL",TeamworkRoleController.class, pageState);
      ButtonJS delB = deletePreviewer.getDeleteButton(I18n.get("DELETE"), role.getId());
      delB.enabled = canCreateRole && !isResourceRole;
      delB.additionalCssClass="big delete";
      bb2.addButton(delB);

      bb2.addSeparator(20);

      ButtonSubmit clone = ButtonSubmit.getSaveInstance(f, I18n.get("CLONE"));
      clone.variationsFromForm.command = "CLONE";
      clone.enabled = canCreateRole && !isResourceRole;
      clone.additionalCssClass = "big";
      bb2.addButton(clone);

      if (isGlobalRole) {
        ButtonSubmit attributeMeRole = new ButtonSubmit(f);
        attributeMeRole.variationsFromForm.setCommand("CROWN_ME_WITH_ROLE");
        attributeMeRole.label = I18n.get("CROWN_ME_WITH_ROLE");
        attributeMeRole.additionalCssClass = "big";
        bb2.addButton(attributeMeRole);
        bb2.addSeparator(10);
      }
    }
    bb2.toHtml(pageContext);

    f.end(pageContext);

  }
%>