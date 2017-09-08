<%@ page
        import="com.twproject.operator.TeamworkOperator, com.twproject.security.TeamworkPermissions, com.twproject.task.Assignment, com.twproject.task.Issue,
        com.twproject.task.Task, com.twproject.task.TaskBricks, com.twproject.task.businessLogic.ScrumControllerAction,
        com.twproject.waf.TeamworkHBFScreen, org.jblooming.oql.OqlQuery, org.jblooming.persistence.PersistenceHome,
        org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.SessionState, org.jblooming.waf.constants.Commands,
        org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar,
        org.jblooming.waf.html.container.Container, org.jblooming.waf.html.display.Img, org.jblooming.waf.html.display.PathToObject,
        org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.DateDurationInput, org.jblooming.waf.html.input.TextArea, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.html.table.ListHeader, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List, com.twproject.utilities.TeamworkComparators" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;

    final ScreenArea body = new ScreenArea(new ScrumControllerAction(pageState), request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);

    pageState.toHtml(pageContext);

  } else {
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  String scrumRootId = pageState.getEntry(Fields.PARENT_ID).stringValue();
  Task scrumRoot = (Task) PersistenceHome.findByPrimaryKey(Task.class, scrumRootId);
  //scrumRoot.testPermission(logged, TeamworkPermissions.assignment_manage);
  scrumRoot.testPermission(logged, TeamworkPermissions.task_canCreate);
  scrumRoot.testPermission(logged, TeamworkPermissions.assignment_canCRW);

  // first of all MUST CHECK if SCRUM roles exists
    boolean existsScrumMaster = false;
    boolean existsScrumTeam = false;
    String ROLE_SCRUM_MASTER_NAME = ApplicationState.getApplicationSetting("ROLE_SCRUM_MASTER_NAME","Scrum Master");
    String ROLE_SCRUM_TEAM_NAME = ApplicationState.getApplicationSetting("ROLE_SCRUM_TEAM_NAME","Scrum Team");
    for (Assignment ass : scrumRoot.getAssignments()) {

      if (ass.getRole().getName().equalsIgnoreCase(ROLE_SCRUM_TEAM_NAME))
        existsScrumTeam=true;
      else if  (ass.getRole().getName().equalsIgnoreCase(ROLE_SCRUM_MASTER_NAME))
        existsScrumMaster=true;
    }

    boolean canCreate = existsScrumMaster && existsScrumTeam;

    Task sprintTask = (Task) pageState.getMainObject();
    boolean justSaved = !sprintTask.isNew();

    %><script>$("#TASK_MENU").addClass('selected');</script><%

    // ------------  path to object
  PathToObject pto = new PathToObject(sprintTask);
  pto.comparator = new TeamworkComparators.TaskManualOrderComparator();
  pto.canClick = TeamworkPermissions.task_canRead;

  PageSeed back = new PageSeed(ApplicationState.contextPath+"/applications/teamwork/task/taskList.jsp");
  //back.setCommand(Commands.FIND);
  ButtonLink taskList = new ButtonLink(I18n.get("TASK_LIST")+" /", back);
  pto.rootDestination = taskList;

  pto.destination = pageState.pageFromRoot("task/taskOverview.jsp");
  pto.destination.setCommand(Commands.EDIT);
  pto.displayNameForNewObject="";
  pto.toHtml(pageContext);



    PageSeed pageSeed = pageState.thisPage(request);
    pageSeed.mainObjectId = sprintTask.getId();
    pageSeed.addClientEntry(Fields.PARENT_ID,scrumRootId);
    pageSeed.command = "GENERATE_SPRINT";

    Form form = new Form(pageSeed);
    form.start(pageContext);


  

   %>
<h1><%=I18n.get("SCRUM_SPRINT_WIZARD")%></h1>
<table class="table" cellpadding="5"><tr>
    <td valign="top">
      <%
        TextField code = new TextField("TASK_CODE", "<br>");
        code.fieldSize = 15;
        code.readOnly = justSaved;
        code.script = "style=width:100%";
        code.toHtmlI18n(pageContext);
      %>
    </td>
    <td valign="top" colspan="2"><%

  TextField tf = new TextField("TASK_NAME", "<br>");
  tf.label="SCRUM_SPRINT_NAME";
  tf.required = true;
  tf.fieldSize = 50;
  tf.script = "style=width:100%";
  tf.readOnly = justSaved;
  pageState.setFocusedObjectDomId(tf.id);
  tf.toHtmlI18n(pageContext);

%></td><td valign="top" rowspan="2"><%

  TextArea ta = new TextArea("DESCRIPTION","<br>",50,5,null);
  ta.label = I18n.get("TASK_DESCRIPTION");
  ta.readOnly = justSaved;
  ta.maxlength=2000;
  ta.script = "style=width:100%";
  ta.toHtmlI18n(pageContext);

%></td>
</tr>
  <tr class="dateRow"><td>


   <%
     DateDurationInput ddi = new DateDurationInput("START","STARTISMILESTONE","END","ENDISMILESTONE","TASK_DURATION",pageState);
   %>

    <label><%=I18n.get("START")%></label><%
    CheckField startMilestoneField = ddi.startMilestoneField;
    startMilestoneField.label = "<i>" + I18n.get("ISMILESTONE") + "</i>";
    startMilestoneField.toHtml(pageContext);


    ddi.startField.separator="<br>";
    ddi.startField.readOnly =justSaved;
    ddi.startField.labelstr="";
    ddi.drawStart(pageContext);
    %></td>

  <td><label><%=I18n.get("END")%></label><%
    CheckField endMilestoneField = ddi.endMilestoneField;
    endMilestoneField.label = "<i>" + I18n.get("ISMILESTONE") + "</i>";
    endMilestoneField.toHtml(pageContext);

    ddi.endField.separator="<br>";
    ddi.endField.readOnly=justSaved;
    ddi.endField.labelstr="";
    ddi.drawEnd(pageContext);
    %></td>
    <td ><%
      ddi.durationField.separator="<br>";
      ddi.durationField.readOnly=justSaved;
      ddi.durationField.label=I18n.get("TASK_DURATION");
      ddi.drawDuration(pageContext);
    %></td>
  </tr></table>
<br>
<div class="isMeeting">
<h2 class="sectionTitle"><%=I18n.get("STAND_UP_MEETING_SETUP")%></h2>
<table class="table">
<tr class="data"><td><%
  TextField time = TextField.getTimeInstance("SCRUM_STAND_UP_MEETING_TIME");
  time.separator= "<br>";
  time.fieldSize = 5;
  time.script = "style=width:100%";
  time.readOnly = justSaved;
  time.toHtmlI18n(pageContext);
  %></td><td><%
  TextField desc = new TextField("SCRUM_STAND_UP_MEETING_DESC", "<br>");
  desc.fieldSize = 30;
  desc.readOnly = justSaved;
  desc.script = "style=width:100%";
  desc.toHtmlI18n(pageContext);
  %></td><td><%
  TextField loc = new TextField("SCRUM_STAND_UP_MEETING_LOCATION", "<br>");
  loc.fieldSize = 30;
  loc.script = "style=width:100%";
  loc.readOnly = justSaved;
  loc.toHtmlI18n(pageContext);
  %></td></tr><%

  %></table></div>

<h2 class="sectionTitle"><%=I18n.get("ORDER_LOG_AND_SELECT_ISSUES")%></h2>
  <table class="table" border="0" id="orderArea">
  <%

  String hql = "from "+ Issue.class.getName()+" as issue where issue.task = :root and issue.status.behavesAsOpen = true order by issue.gravity desc, issue.orderFactor";
  OqlQuery oql = new OqlQuery(hql);
  if (justSaved)
    oql.getQuery().setEntity("root",sprintTask);  
  else
    oql.getQuery().setEntity("root",scrumRoot);
  List<Issue> all = oql.list();

  ListHeader lh = new ListHeader("ISSLH",form);
    lh.addHeaderFitAndCentered(I18n.get("ISSUE_SELECT_ISSUE_FOR_SPRINT"));
    lh.addHeader(I18n.get("ISSUE_DESCRIPTION"));
    lh.addHeaderFitAndCentered(I18n.get("ISSUE_TYPE_SHORT"));
    lh.addHeaderFitAndCentered(I18n.get("ISSUE_GRAVITY_SHORT"));
    lh.addHeaderFitAndCentered(I18n.get("ISSUE_WORKLOG_ESTIMATED_TIME"));
  lh.toHtml(pageContext);

  for (Issue issue : all) {
    %><tr id="tr_<%=issue.getId()%>" sortable="true" oldclass="" class="data"><%

      CheckField cf = new CheckField("issue_"+issue.getId(),"",true);
      cf.disabled = justSaved;
      cf.label = "";

      %><td nowrap align="center"><%cf.toHtml(pageContext);%></td>
    <td><%=JSP.encode(issue.getDescription())%></td>
    <td align="left"><%=issue.getType()!=null ? JSP.w(issue.getType().getDescription()) : "&nbsp;"%></td>
    <td align="center">
    <div style="height:12px;width:12px; background-color: <%=issue.bricks.getGravityColor()%>" title="<%=I18n.get(issue.getGravity()+"")%>"></div>
    </td>

    <td align="center"><%=DateUtilities.getMillisInHoursMinutes(issue.getEstimatedDuration())%>&nbsp;</td>


   </tr><%
  }

  %></table><%

    ButtonBar bb = new ButtonBar();
    ButtonSubmit go = ButtonSubmit.getSaveInstance(form,I18n.get("GENERATE_SPRINT"));  
    go.variationsFromForm.command="GENERATE_SPRINT";
    go.additionalCssClass = "first big";
    go.enabled = !justSaved && canCreate;
    bb.addButton(go);

    %><br><%bb.toHtml(pageContext);%><%

    if (!canCreate) {
        Container box = new Container();
        box.title = I18n.get("SCRUM_MISSING_ROLES");
        box.width = "350px";
        box.centeredOnScreen = true;
        box.draggable=true;
        
        box.setCssPostfix("warn");
        box.start(pageContext);

        if (!existsScrumMaster) {
          %><%=I18n.get("SCRUM_ROLE_%%_IS_MISSING", "Scrum Master")%><br><%
        }
        if (!existsScrumTeam) {
          %><%=I18n.get("SCRUM_ROLE_%%_IS_MISSING", "Scrum Team")%><br><%
        }

      box.end(pageContext);
    }  


    form.end(pageContext);


  }
%>
