<%@ page import="com.twproject.operator.TeamworkOperator" %>
<%@ page import="com.twproject.security.businessLogic.TeamworkRoleController" %>
<%@ page import="com.twproject.waf.TeamworkHBFScreen" %>
<%@ page import="org.jblooming.page.Page, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.state.Form, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List, com.twproject.security.RoleTeamwork, org.jblooming.oql.OqlQuery, org.jblooming.security.OperatorRole, org.jblooming.operator.Operator, com.twproject.task.Assignment, com.twproject.resource.Resource, java.util.ArrayList, org.jblooming.waf.html.button.ButtonSubmit, com.twproject.setup.WizardSupport, java.util.Set, org.jblooming.waf.settings.ApplicationState, com.twproject.security.TeamworkPermissions, org.jblooming.security.Permission, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.settings.I18n, org.jblooming.utilities.JSP, org.jblooming.waf.view.ClientEntry, java.util.Map" %>
<%!
  private RoleTeamwork getRoleTeamwork(String roleName ) {
    RoleTeamwork vacca = null;

    if ("Area manager".equalsIgnoreCase(roleName)) {
      vacca = new RoleTeamwork();
      WizardSupport.setAreaManagerPermissions(vacca);

    } else if ("Supervisor".equalsIgnoreCase(roleName)) {
      vacca = new RoleTeamwork();
      WizardSupport.setSupervisorPermissions(vacca);

    } else if ("Project launcher".equalsIgnoreCase(roleName)) {
      vacca = new RoleTeamwork();
      WizardSupport.setProjectLauncherPermissions(vacca);

    } else if ("Operational".equalsIgnoreCase(roleName)) {
      vacca = new RoleTeamwork();
      WizardSupport.setOperatorPermissions(vacca);

    } else if ((ApplicationState.getApplicationSetting("DEFAULT_PROJECT_MANAGER_ROLE_NAME", "Project manager")).equalsIgnoreCase(roleName)) {
      vacca = new RoleTeamwork();
      WizardSupport.setProjectManagerPermissions(vacca);

    } else if ("Stakeholder".equalsIgnoreCase(roleName)) {
      vacca = new RoleTeamwork();
      WizardSupport.setStakeholderPermissions(vacca);

    } else if ((ApplicationState.getApplicationSetting("DEFAULT_CUSTOMER_ROLE_NAME", "Customer")).equalsIgnoreCase(roleName)) {
      vacca = new RoleTeamwork();
      WizardSupport.setStakeholderPermissions(vacca);

    } else if ((ApplicationState.getApplicationSetting("DEFAULT_WORKER_ROLE_NAME", "Worker")).equalsIgnoreCase(roleName)) {
      vacca = new RoleTeamwork();
      WizardSupport.setWorkerPermissions(vacca);

    }
    return vacca;
  }

%>


<%
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
  logged.testIsAdministrator();

  if (!pageState.screenRunning) {

    pageState.screenRunning = true;

    final ScreenArea body = new ScreenArea(request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);


    // ------------------------ RESET DEFAULT ROLES ------------------------------------------
    if ("RESET_DEF_ROLES".equals(pageState.command)) {
      List<RoleTeamwork> roles = new OqlQuery("select r from " + RoleTeamwork.class.getName() + " as r").list();

      for (RoleTeamwork r : roles) {

        //si controlla se il ruolo è stato sporcato dal nostro update
        if (!r.hasPermissionFor(TeamworkPermissions.resource_manage) && !r.hasPermissionFor(TeamworkPermissions.assignment_canCRW) && !r.hasPermissionFor(TeamworkPermissions.expense_manage))
          continue;

        //si cerca un ruolo con quel nome
        RoleTeamwork vacca = getRoleTeamwork(r.getName());

        //se aveva un nome di quelli buoni
        if (vacca != null) {
          //si tolgono quelle incriminate
          r.removePermission(TeamworkPermissions.resource_manage);
          r.removePermission(TeamworkPermissions.assignment_canCRW);
          r.removePermission(TeamworkPermissions.expense_manage);

          // si rimettono tutte quelle di default
          for (Permission p: vacca.getPermissions())
            r.addPermission(p);
          r.store();
        }

      }



    // ------------------------ SAVE ROLE CHANGES ------------------------------------------
    } else if (Commands.SAVE.equals(pageState.command)){
      Map<String, ClientEntry> ces = pageState.getClientEntries().getEntriesStartingWithStripped("CKR_");
      for (String permId:ces.keySet()){
        String [] ss=permId.split(":");
        RoleTeamwork r=RoleTeamwork.load(ss[1]);
        if (ces.get(permId).checkFieldValue())
          r.addPermission(new Permission(ss[0]));
        else
          r.removePermission(new Permission(ss[0]));

        r.store();
      }


    }

    pageState.toHtml(pageContext);

  } else {


    PageSeed self = pageState.thisPage(request);
    self.setCommand(Commands.FIND);
    Form f = new Form(self);
    pageState.setForm(f);

    f.start(pageContext);

%><h1>Fix Role Permissions 6.0.600015</h1>



<h2>Following roles requires your attention:</h2>

<%

  List<RoleTeamwork> roles = new OqlQuery("select r from " + RoleTeamwork.class.getName() + " as r").list();
  for (RoleTeamwork r : roles) {

    //si controlla se il ruolo è stato sporcato dal nostro update
    if (!r.hasPermissionFor(TeamworkPermissions.resource_manage) && !r.hasPermissionFor(TeamworkPermissions.assignment_canCRW) && !r.hasPermissionFor(TeamworkPermissions.expense_manage))
      continue;


    //cerchiamo un ruolo di default con quel nome
    RoleTeamwork vacca = getRoleTeamwork(r.getName());

    //il ruolo di default ha gli stessi permessi incriminati?
    if (vacca!=null &&
      vacca.hasPermissionFor(TeamworkPermissions.resource_manage)==r.hasPermissionFor(TeamworkPermissions.resource_manage )&&
      vacca.hasPermissionFor(TeamworkPermissions.assignment_canCRW)==r.hasPermissionFor(TeamworkPermissions.assignment_canCRW )&&
      vacca.hasPermissionFor(TeamworkPermissions.expense_manage)==r.hasPermissionFor(TeamworkPermissions.expense_manage )){
      continue;
    }


    String name = r.getDisplayName();
    //3 ruoli incriminati


    //chi usa questo ruolo?
    List<Resource> localResources = new ArrayList<Resource>();
    List<Operator> globalOperators = new ArrayList<Operator>();
    boolean isLocalRole = r.isLocalToAssignment();
    if (isLocalRole) {

      OqlQuery oql = new OqlQuery("select distinct ass.resource from " + Assignment.class.getName() + " as ass where ass.role=:role");
      oql.getQuery().setEntity("role", r);
      oql.getQuery().setMaxResults(21);
      localResources = oql.list();

    } else {

      OqlQuery oql = new OqlQuery("select opr.operator from " + OperatorRole.class.getName() + " as opr where opr.role=:role");
      oql.getQuery().setEntity("role", r);
      oql.getQuery().setMaxResults(21);
      globalOperators = oql.list();

    }



%><br>
<table style="border:2px dashed #e0e0e0" width="100%" cellpadding="10px">
  <tr>
    <td><h2><%=name%></h2>
  </tr>
  <tr>
    <td><%
      if (isLocalRole && JSP.ex((localResources.size()))) {
        %>Assigned to: <%
        int i=0;
        for (Resource rs : localResources) {
          %><%=rs.getDisplayName()%>&nbsp;&nbsp;<%
          i++;
          if (i>=20)
            break;
        }
        if (localResources.size()>20){
          %>...<%
        }
      } else if(JSP.ex(globalOperators)) {
        %>Assigned to: <%

        int i=0;
        for (Operator rs : globalOperators) {
          %><%=rs.getDisplayName()%>&nbsp;&nbsp;<%
          i++;
          if (i>=20)
            break;
        }
        if (globalOperators.size()>20){
          %>...<%
        }
      }

      if (globalOperators.size()==0 && localResources.size()==0){
        %>Role not yet used.<%
      }

    %>
    </td>
    <td align="right">
      <%
        //TeamworkPermissions.resource_manage) || !r.hasPermissionFor(TeamworkPermissions.assignment_canCRW) || !r.hasPermissionFor(TeamworkPermissions.expense_manage

        String ceName = "CKR_" + TeamworkPermissions.resource_manage + ":" + r.getId();
        pageState.addClientEntry(ceName,r.hasPermissionFor(TeamworkPermissions.resource_manage));
        new CheckField(I18n.get(TeamworkPermissions.resource_manage + ""),ceName,"",true).toHtmlI18n(pageContext);

        ceName = "CKR_" + TeamworkPermissions.assignment_canCRW + ":" + r.getId();
        pageState.addClientEntry(ceName,r.hasPermissionFor(TeamworkPermissions.assignment_canCRW));
        new CheckField(I18n.get(TeamworkPermissions.assignment_canCRW + ""),ceName,"",true).toHtmlI18n(pageContext);

        ceName = "CKR_" + TeamworkPermissions.expense_manage + ":" + r.getId();
        pageState.addClientEntry(ceName,r.hasPermissionFor(TeamworkPermissions.expense_manage));
        new CheckField(I18n.get(TeamworkPermissions.expense_manage + ""),ceName,"",true).toHtmlI18n(pageContext);
      %>
    </td>
  </tr>
</table>
<%
    }

    ButtonSubmit save = ButtonSubmit.getSaveInstance(f, "save");
    save.additionalCssClass = "first big";
    save.label = "Save";
    save.confirmRequire=true;

    ButtonSubmit reset = new ButtonSubmit("reset", "RESET_DEF_ROLES", f);
    reset.label = "Reset Default Permissions";
    reset.confirmRequire=true;
    reset.confirmQuestion="Da you want to reset standard roles to factory defaults?";


    %>
    <p align="right">
    <br> Use checkboxes in order to change permissions, then  <%save.toHtml(pageContext);%>
    </p>

    <br><br><br><hr>
    <p align="right">WARNING: this button will reset ALL "dirty" standard roles to factory defaults. <%reset.toHtml(pageContext);%>
    </p>






<%

    f.end(pageContext);

  }
%>




