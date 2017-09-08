<%@ page import="com.twproject.operator.TeamworkOperator,
                 com.twproject.security.RoleTeamwork,
                 com.twproject.security.TeamworkPermissions,
                 com.twproject.task.Task,
                 com.twproject.task.businessLogic.ScrumControllerAction,
                 com.twproject.waf.TeamworkHBFScreen,
                 org.jblooming.oql.OqlQuery,
                 org.jblooming.security.Area,
                 org.jblooming.security.Role,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.container.ButtonBar,
                 org.jblooming.waf.html.container.Container,
                 org.jblooming.waf.html.input.*,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState, java.util.List, java.util.Set" %>
<%


  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;

    final ScreenArea body = new ScreenArea(new ScrumControllerAction(pageState),request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);

  } else {

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    if (logged.hasPermissionFor(TeamworkPermissions.project_canCreate)) {

      // first of all MUST CHECK if SCRUM roles exists
      boolean existsScrumMaster = false;
      boolean existsProductOwner = false;
      boolean existsStakeholder = false;
      boolean existsScrumTeam = false;
      Set<Area> areas = logged.getAreasForPermission(TeamworkPermissions.project_canCreate);
      String hql = "from " + RoleTeamwork.class.getName() + " as role where role.area in (:areas)";
      OqlQuery oql = new OqlQuery(hql);
      oql.getQuery().setParameterList("areas", areas);
      List<RoleTeamwork> roles = oql.list();
      for (Role role : roles) {
        if (role.getName().equalsIgnoreCase("Scrum Master"))
          existsScrumMaster = true;
        if (role.getName().equalsIgnoreCase("Scrum Stakeholder"))
          existsStakeholder = true;
        if (role.getName().equalsIgnoreCase("Scrum Team"))
          existsScrumTeam = true;
        if (role.getName().equalsIgnoreCase("Product Owner"))
          existsProductOwner = true;
      }

      boolean canCreate = existsScrumMaster && existsProductOwner && existsStakeholder && existsScrumTeam;



  Task savedTask = (Task) pageState.getMainObject();

  boolean justSaved = (savedTask != null);


  PageSeed pageSeed = pageState.thisPage(request);

  Form form = new Form(pageSeed);
  form.start(pageContext);





%><script>$("#TASK_MENU").addClass('selected');</script>

<h1><%=I18n.get("SCRUM_PROJECT_CREATION")%></h1>
<div class="container level_2">
<table class="table" cellpadding="5" border="0">
  <tr style="background-color: #eeeeee">
  <td valign="top" width="15%"><%

  TextField code = new TextField("SCRUM_TASK_CODE", "<br>");
  code.fieldSize = 12;
  code.readOnly = justSaved;
  code.toHtmlI18n(pageContext);

%></td>
  <td valign="top" colspan="2"><%

    TextField tf = new TextField("SCRUM_TASK_NAME", "<br>");
    tf.required = true;
    tf.fieldSize = 50;
    tf.readOnly = justSaved;
    tf.toHtmlI18n(pageContext);

  %></td>
  <td  valign="top" rowspan="2" width="70%"><%

    TextArea ta = new TextArea("TASK_DESCRIPTION","<br>",50,4,null);
    ta.readOnly = justSaved;
    ta.maxlength=2000;
    ta.script="style=width:100%;";
    ta.toHtmlI18n(pageContext);

  %></td>
</tr>
  <tr style="background-color: #eeeeee"><td nowrap><%


  DateDurationInput ddi = new DateDurationInput("SCRUM_START", "STARTISMILESTONE", "SCRUM_END", "ENDISMILESTONE", "SCRUM_DURATION", pageState);

    %><label><%=I18n.get("START")%></label>&nbsp;<%
    CheckField startMilestoneField = ddi.startMilestoneField;
    startMilestoneField.label = "<i>" + I18n.get("ISMILESTONE") + "</i>";
    startMilestoneField.separator = "";
    startMilestoneField.toHtml(pageContext);
  %><%
  DateField df = ddi.startField;
  df.readOnly = justSaved;
  df.labelstr = "";
  df.separator = "<br>";
  df.required = true;
  df.preserveOldValue = true;
  ddi.drawStart(pageContext);
  %></td><td width="15%" nowrap><label><%=I18n.get("END")%></label>&nbsp;<%
    CheckField endMilestoneField = ddi.endMilestoneField;
    endMilestoneField.label = "<i>" + I18n.get("ISMILESTONE") + "</i>";
    endMilestoneField.separator = "";
    endMilestoneField.toHtml(pageContext);

    df = ddi.endField;
    df.readOnly = justSaved;
    df.required = true;
    df.labelstr = "";
    df.separator = "<br>";
    df.preserveOldValue = true;
    ddi.drawEnd(pageContext);
  %></td>
    <td nowrap=""><%

    TextField td = ddi.durationField;
    td.separator = "<br>";
    td.readOnly = justSaved;
    td.fieldSize = 6;
    td.label = I18n.get("DAYS");
    td.toolTip = I18n.get("HELP_DURATION");
    ddi.drawDuration(pageContext);

  %></td></tr>
<%

  Task task = new Task();
  Area myarea = logged.getDefaultAreaForPermission(TeamworkPermissions.project_canCreate);
  task.setArea(myarea);


%><tr><td colspan="4">&nbsp;</td></tr>
  <tr><td colspan="4" class="tableHead"><h2 class="sectionTitle"><%=I18n.get("SCRUM_DEFINE_PRODUCT_OWNER")%></h2></td></tr><%

    %><tr class="data"><td><%
  SmartCombo sc = task.bricks.getAssignableResourceCombo("PRODUCT_OWNER", false, pageState);
  sc.separator = "</td><td colspan='3'>";
  sc.fieldSize = 40;
  sc.label = I18n.get("SCRUM_SELECT");
  sc.readOnly = justSaved;
  sc.required = true;
  sc.toHtml(pageContext);
%></td></tr>
  <tr><td colspan="4">&nbsp;</td></tr>

  <tr><td colspan="4" class="tableHead"><h2 class="sectionTitle"><%=I18n.get("SCRUM_DEFINE_SCRUM_MASTER")%></h2></td></tr>
  <tr class="data"><td><%
  sc = task.bricks.getAssignableResourceCombo("SCRUM_MASTER", false, pageState);
  sc.separator = "</td><td colspan='3'>";
  sc.fieldSize = 40;
  sc.label = I18n.get("SCRUM_SELECT");
  sc.readOnly = justSaved;
  sc.required = true;  
  sc.toHtml(pageContext);
%></td></tr>
  <tr><td colspan="4">&nbsp;</td></tr>
  <tr><td colspan="4" class="tableHead"><h2 class="sectionTitle"><%=I18n.get("SCRUM_DEFINE_STAKEHOLDERS")%></h2></td></tr><%

    for (int i=0;i<3;i++){
      %><tr class="data"><td><%
  sc = task.bricks.getAssignableResourceCombo("SCRUM_STAKEHOLDER_" + i, false, pageState);
  sc.separator = "</td><td colspan='3'>";
  sc.fieldSize = 40;
  sc.label = i == 0 ? I18n.get("SCRUM_SELECT") : "&nbsp;";
  sc.readOnly = justSaved;
  sc.toHtml(pageContext);
%></td></tr><%
    }

  %><tr><td colspan="4">&nbsp;</td></tr>
  <tr><td colspan="4" class="tableHead"><h2 class="sectionTitle"><%=I18n.get("SCRUM_DEFINE_TEAM")%></h2></td></tr><%

    for (int i=0;i<7;i++){
      %><tr class="data"><td><%
  sc = task.bricks.getAssignableResourceCombo("SCRUM_TEAM_" + i, false, pageState);
  sc.separator = "</td><td colspan='3'>";
  sc.fieldSize = 40;
  sc.label = i == 0 ? I18n.get("SCRUM_SELECT") : "&nbsp;";
  sc.readOnly = justSaved;
  sc.toHtml(pageContext);
%></td></tr><%
  }

%></table>
<%

        if (!justSaved) {
          ButtonBar bb = new ButtonBar();
          ButtonSubmit saveInstance = ButtonSubmit.getSaveInstance(form, I18n.get("SCRUM_CREATE_TASK"));
          saveInstance.variationsFromForm.command="SAVE_ROOT_SCRUM_WIZ";
          saveInstance.enabled = canCreate;
          saveInstance.additionalCssClass="big first";
          bb.addButton(saveInstance);
          bb.toHtml(pageContext);

           if (!canCreate) {
        Container box = new Container();
        box.title = I18n.get("SCRUM_MISSING_ROLES");
        box.width = "30%";
        box.centeredOnScreen = true;
        box.setCssPostfix("warn");
        box.start(pageContext);

        if (!existsProductOwner) {
          %><%=I18n.get("SCRUM_ROLE_%%_IS_MISSING", "Product Owner")%><br><%
        }
        if (!existsScrumMaster) {
          %><%=I18n.get("SCRUM_ROLE_%%_IS_MISSING", "Scrum Master")%><br><%
        }
        if (!existsStakeholder) {
          %><%=I18n.get("SCRUM_ROLE_%%_IS_MISSING", "Scrum Stakeholder")%><br><%
        }
        if (!existsScrumTeam) {
          %><%=I18n.get("SCRUM_ROLE_%%_IS_MISSING", "Scrum Team")%><br><%
        }

        if (logged.hasPermissionAsAdmin()) {
          PageSeed wiz = pageState.pageFromRoot("/security/security.jsp");
          ButtonLink bl = new ButtonLink("SCRUM_GO_TO_WIZARD_PAGE", wiz);
          bl.toHtmlI18n(pageContext);
        } else {
          %><%=I18n.get("SCRUM_CONTACT_ADMIN_TO_CREATE_ROLES")%><%
        }
      box.end(pageContext);
    }


        }
%></div><%
        form.end(pageContext);


        if (justSaved) {

          Container box = new Container();
          box.title = I18n.get("SCRUM_PROJECT_CREATED");
          box.width = "40%";
          box.draggable = true;
          box.collapsable = false;
          box.centeredOnScreen = true;
          box.closeable = false;
          box.start(pageContext);

          PageSeed issues = pageState.pageFromRoot("issue/issueList.jsp");
          issues.addClientEntry("FLT_ISSUE_TASK", savedTask.getId());
          issues.command = Commands.FIND;          
          ButtonLink addBl = new ButtonLink(I18n.get("ADD_ISSUE"),issues);
          addBl.enabled = logged.hasPermissionFor(TeamworkPermissions.issue_canCreate);

          %><h3><%=I18n.get("SCRUM_PROJECT_%%_CORRECTLY_CREATED",savedTask.getDisplayName())%></h3><%


          %><p><%=I18n.get("SCRUM_PROJECT_CREATE_BACKLOG")%>: <%addBl.toHtmlInTextOnlyModality(pageContext);%>
          </p><%

          PageSeed edit = pageState.pageFromRoot("task/taskOverview.jsp");
          edit.command = Commands.EDIT;
          edit.mainObjectId = savedTask.getId();
          ButtonLink le = new ButtonLink(edit);
          le.label = I18n.get("INSPECT_CREATED_TASK") + ":&nbsp;<b>" + savedTask.getName() + "</b>";

          %><%le.toHtml(pageContext);%><%

          box.end(pageContext);
        }




    }
  }
%>
