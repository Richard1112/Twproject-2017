<%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8" %><%@ page import="com.twproject.agenda.Event,
                 com.twproject.meeting.DiscussionPoint, com.twproject.meeting.Meeting, com.twproject.operator.TeamworkOperator, com.twproject.resource.Person, com.twproject.resource.Resource, com.twproject.security.TeamworkPermissions,
                 com.twproject.task.Assignment, com.twproject.task.Task, com.twproject.task.TaskBricks, com.twproject.task.TaskStatus, com.twproject.task.businessLogic.TaskController, com.twproject.waf.TeamworkHBFScreen,
                 com.twproject.waf.html.StatusIcon, com.twproject.waf.html.TaskHeaderBar, net.sf.json.JSONObject, org.hibernate.Query, org.jblooming.designer.DesignerField, org.jblooming.oql.OqlQuery,
                 org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.utilities.ReflectionUtilities, org.jblooming.utilities.StringUtilities,
                 org.jblooming.waf.PluginBricks, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonJS,
                 org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.DivOnMouseover, org.jblooming.waf.html.core.JspHelper,
                 org.jblooming.waf.html.display.PercentileDisplay, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jbpm.graph.def.Node, org.jbpm.graph.def.ProcessDefinition, org.jbpm.graph.def.Transition, org.jbpm.graph.exe.ProcessInstance, org.jbpm.graph.node.TaskNode, org.jbpm.taskmgmt.exe.TaskInstance, org.jbpm.taskmgmt.exe.TaskMgmtInstance, java.util.Collection, java.util.Date, java.util.List, java.util.Map"%><%

  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new TaskController(), request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {

    Task task = (Task) pageState.getMainObject();

    // Form -----------------------------------------------------------------------------------------------
    PageSeed self = pageState.thisPage(request);
    self.mainObjectId = task.getId();
    self.setCommand(Commands.EDIT);
    String parId = pageState.getEntry(Fields.PARENT_ID).stringValueNullIfEmpty();
    self.addClientEntry(Fields.PARENT_ID,JSP.w(parId));

    Form f = new Form(self);
    f.encType=Form.MULTIPART_FORM_DATA;
    f.alertOnChange = true;
    pageState.setForm(f);
    f.start(pageContext);

    boolean isProcessDriven=task.isProcessDriven();
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    Person loggedPerson=logged.getPerson();
%>
<div class="mainColumn clearfix" style="position: relative"><%

  //-----------------HEAD BAR START
  pageState.addClientEntry("TASK_TABSET","TASK_GENERAL_TAB");
  TaskHeaderBar head = new TaskHeaderBar(task);
  //head.pathToObject.mainClass = Task.class;
  head.toHtml(pageContext);
  //-----------------HEAD BAR END


%>
<div class="inlineContainerWrapper">

<div style="width: 30%; max-width: 450px; padding: 10px 10px 10px 0" class="container   <%=(task.isNew() ? "new" : "" )%>">

<%

  ButtonJS addOpener = new ButtonJS("");
  addOpener.iconChar = "P";
  addOpener.toolTip = I18n.get("ADD");
  addOpener.label = addOpener.toolTip;
  addOpener.additionalCssClass = "first";
  DivOnMouseover add = new DivOnMouseover(addOpener);


  // task editor
  if (!task.isNew() && task.hasPermissionFor(logged, TeamworkPermissions.task_canCreate) ) {
    if (JSP.w(task.getTags()).indexOf("SCRUM") >= 0) {
      PageSeed pageSeed = pageState.pageFromRoot("task/scrum/sprintWizard.jsp");
      pageSeed.addClientEntry(Fields.PARENT_ID, task.getId());
      pageSeed.command = "NEW_SPRINT";
      ButtonLink buttonLink = new ButtonLink("+ "+I18n.get("NEW_SPRINT"), pageSeed);
      add.addButton(buttonLink);

    }
    {
      PageSeed newChildTask = new PageSeed(request.getContextPath() + "/applications/teamwork/task/taskNew.jsp");
      newChildTask.setCommand(Commands.ADD);
      newChildTask.addClientEntry(Fields.PARENT_ID, task.getId());
      newChildTask.addClientEntry("ADD_TYPE", "ADD_CHILD_TASK");
      ButtonSupport buttonLink = ButtonLink.getBlackInstance("+ "+I18n.get("ADD_CHILD_TASK"), 720,800,newChildTask);

      buttonLink.id = "btnAddSubtask";
      add.addButton(buttonLink);
    }
  }

  // add brother
  if (task.getParent() != null && task.getParent().hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.task_canCreate)) {
    PageSeed newBrotherTask = new PageSeed(request.getContextPath() + "/applications/teamwork/task/taskNew.jsp");
    newBrotherTask.setCommand(Commands.ADD);
    newBrotherTask.addClientEntry(Fields.PARENT_ID, task.getParent().getId());
    newBrotherTask.addClientEntry("ADD_TYPE", "ADD_BROTHER_TASK");
    ButtonSupport buttonLink = ButtonLink.getBlackInstance("+ "+I18n.get("ADD_BROTHER_TASK"), 720,800,newBrotherTask);
    add.addButton(buttonLink);
  }

  //add assignment if in taskAssignList
  if(!task.isNew()  &&  task.hasPermissionFor(logged,TeamworkPermissions.assignment_canCRW) ) {
    PageSeed psAddAssignment = pageState.pageFromRoot("task/taskAssignmentNew.jsp");
    psAddAssignment.addClientEntry("TASK_ID",task.getId());
    psAddAssignment.command = Commands.ADD;


    ButtonSupport buttonLink = ButtonLink.getBlackInstance(I18n.get("ADD_ASSIGNMENT"),600,800, psAddAssignment);
    buttonLink.enabled = task.hasPermissionFor(logged,TeamworkPermissions.assignment_canCRW);
    buttonLink.label = "+ "+I18n.get("ADD_ASSIGNMENTS");
    add.addButton(buttonLink);
  }

  ButtonSupport addIssue;
  PageSeed issues = pageState.pageFromRoot("task/taskIssueList.jsp");
  issues.mainObjectId = task.getId();
  issues.command = "ADDISSUE";
  addIssue = new ButtonLink("", issues);
  addIssue.enabled = task.hasPermissionFor(logged,TeamworkPermissions.issue_canCreate);
  addIssue.label="+ "+ I18n.get("ADD_ISSUE");
  add.addButton(addIssue);



  // document list
  if(!task.isNew() && task.hasPermissionFor(logged,TeamworkPermissions.document_canCreate)){
    PageSeed ps = pageState.pageFromRoot("task/taskDocumentEditor.jsp");
    ps.setCommand(Commands.ADD  );
    ps.addClientEntry("TASK_ID",task.getId());
    ButtonLink addDoc = new ButtonLink(ps);
    addDoc.label = "+ "+I18n.get("ADD_DOCUMENT");
    add.addButton(addDoc);

  }

  //add discussion
  PageSeed newTh= pageState.pageFromRoot("task/taskForumEditor.jsp");
  newTh.addClientEntry("TASK_ID", task.getId());
  newTh.command=Commands.ADD;
  ButtonLink addPost = new ButtonLink("+ "+I18n.get("POST_NEW_THREAD"),newTh);
  add.addButton(addPost);


  //inject wizards
  List<ButtonSupport> bss2 = PluginBricks.createPageQuarkItemsButtons("WIZARDS", pageState);
  for (ButtonSupport bs:bss2 ){
    bs.additionalCssClass="special";
    add.addButton(bs);
  }



//add.toHtml(pageContext);
  String taskColor = task.getTaskColor();

%>
  <div style="float: right;width: 30%"><%

  ButtonLink bEdit = ButtonLink.getEditInstanceForList("taskEditor.jsp", task, request);
  //bEdit.outputModality=ButtonSupport.GRAPHICAL;
  //bEdit.additionalCssClass="small";
  bEdit.label=I18n.get("EDIT_DATA");

  bEdit.toHtml(pageContext);


%></div>
<%--<div style="border-top:3px solid #959595;margin:10px 0; "></div>--%>
<%--<h2><%=task.getDisplayName()%></h2>--%>

<div style="max-width: 450px; cursor:pointer; " onclick="location.href='<%=JSP.javascriptEncode(bEdit.pageSeed.toLinkToHref())%>'">


<% //---------------------------------------------------------------------------------  START PROCESS MANAGEMENT ------------------------------------------------------------------------------------
if (isProcessDriven) {

  %><div class="box lreq30 lregLabel"><%
  ProcessInstance processInstance = task.getProcess();
  TaskMgmtInstance tmi = processInstance.getTaskMgmtInstance();
  Collection<TaskInstance> instances = tmi.getTaskInstances();
  ProcessDefinition def = processInstance.getProcessDefinition();

  // add parameter to form
  pageState.getForm().url.addClientEntry("TRANSITIONID","");
  pageState.getForm().url.addClientEntry("TASKINSTANCE","");
  %><span class="teamworkIcon">&pound;</span> <b><%=I18n.get("PROCESS_DRIVEN_TASK")+"</b>:&nbsp;"%><%=def.getName()%>&nbsp;</b><%--small>(ver. <%=def.getVersion()%> id:"<%=def.getId()%>)</small--%><br><%

  PageSeed seePlan=pageState.pageInThisFolder("taskProcess.jsp",request);
  seePlan.command= Commands.EDIT;
  seePlan.mainObjectId=task.getId();

  // check for "touched" tasks
  for (TaskInstance taskInstance : instances) {

    // loop on task assigs
    for (Assignment ass : task.getAssignments()) {

      //  is this TWtask related to the jbpmTask?
      if ((taskInstance.getTask().getId() + "").equals(ass.getExternalCode())) {
        // if it is still open
        if (taskInstance.isOpen()) {
          // is assigned to the logged
          if (ass.getResource().equals(loggedPerson)) {
            // do something goat!
            %><label><%=I18n.get("STEP_YOUR_ACTION_IS_REQUIRED")%>:
            <%--// print task name is !=--%>
            <%=taskInstance.getName().equalsIgnoreCase(task.getName())?"":" \""+taskInstance.getName()+"\""%>
            </label><br><%
            List<Transition> trans=taskInstance.getAvailableTransitions();

            ButtonSubmit step = new ButtonSubmit(pageState.getForm());

            if (JSP.ex(trans)) {
              //Designer designer = new Designer("/commons/flowork/stepDraw.jsp", "ALL_DATA", ProcessInstance.class, processInstance.getId());
              //boolean existModule = designer.designerFields.size() > 0;
              boolean moduleFullfilled = true;
              //if (existModule) {
              //  moduleFullfilled = designer.isFulfilled();
              //}

              boolean morThanOneTrans = trans.size() > 1;
              for (Transition transition : trans) {
                %><div style="display:inline-block;"><%
                String stepName = I18n.get("STEP_PROCEED");
                if (morThanOneTrans) {
                  stepName = transition.getName();
                } else {
                  Node transTo = (Node) ReflectionUtilities.getUnderlyingObjectAsObject(transition.getTo());
                  if (transTo instanceof TaskNode) {
                    stepName = I18n.get("STEP_PROCEED_TO")+" '" + transition.getTo().getName() + "'";
                  }
                }
                step.label = stepName;
                step.variationsFromForm.addClientEntry("TRANSITIONID", transition.getId());
                step.variationsFromForm.addClientEntry("TASKINSTANCE", taskInstance.getId());
                step.variationsFromForm.command="DO_STEP";
                step.enabled = moduleFullfilled && !taskInstance.hasEnded();
                step.confirmRequire=true;
                step.toHtml(pageContext);

                //description
                if (JSP.ex(transition.getDescription())){
                  %><br><small><%=JSP.encode(transition.getDescription())%></small><%
                }
                %></div><%
              }
            }
            // if it is not assign to you show timing
          } else {
            %><%=I18n.get("TASK_IS_WAITING_FOR_%%_FROM_%%",ass.getResource().getDisplayName(), DateUtilities.dateAndHourToString(taskInstance.getCreate()))%><%
          }
        } else {
          // the action has been closed on
          Resource resource = ass.getResource();
          %><b><%=I18n.get("TASK_STEP_COMPLETED_ON_%%_BY_%%",DateUtilities.dateAndHourToString(taskInstance.getEnd()), resource.getDisplayName())%></b><%
        }
        %><br><%
      }
    }
  }
  %></div><%
}
  //---------------------------------------------------------------------------------  END PROCESS MANAGEMENT ------------------------------------------------------------------------------------

  StatusIcon statusIcon = task.bricks.getStatusIcon(15, pageState);

  PercentileDisplay progressBar = task.bricks.getProgressBar(false);
  progressBar.width="100px";
  progressBar.height="15px";


  Date startDate = task.getSchedule().getStartDate();
  Date endDate = task.getSchedule().getEndDate();
  Date now=new Date();

  boolean startInError=false;
  String startErrorMessage="";
  if (TaskStatus.STATUS_SUSPENDED.equals(task.getStatus()) && task.getPreviousesSize()>0 && startDate.before(now) ){
    startInError=true;
    startErrorMessage+=I18n.get("TASK_SHOULD_BE_ALREADY_OPEN");
  }

  boolean endInError=false;
  String endErrorMessage="";
  if (TaskStatus.STATUS_ACTIVE.equals(task.getStatus()) &&  endDate.before(now) ){
    endInError=true;
    endErrorMessage+=I18n.get("TASK_SHOULD_BE_ALREADY_CLOSED");
  }


%>
<table class="taskOverview table" >
  <tr>
    <td><label><%=I18n.get("START")%></label></td><td><b class="<%=startInError?"warning":""%>" title="<%=startErrorMessage%>"><%=JSP.w(startDate)%></b> <%=(task.isStartIsMilestone()?" <span class='teamworkIcon' title='"+I18n.get("MILESTONE")+"'>^</span> ":"")%></td>
  </tr>
  <tr>
    <td><label><%=I18n.get("END")%></label></td><td> <b class="<%=endInError?"warning":""%>" title="<%=endErrorMessage%>"><%=JSP.w(task.getSchedule().getEndDate())%></b> <%=(task.isEndIsMilestone()?" <span class='teamworkIcon' title='"+I18n.get("MILESTONE")+"'>^</span> ":"")%></td>
  </tr>
  <tr>
    <td style="border-color: #cccccc"><label><%=I18n.get("DAYS")%></label></td><td style="border-color: #cccccc"><b><%=task.getDuration()%></b></td>
  </tr>
  <tr>
    <td colspan="2" style="border: none;font-size: 1px;">&nbsp;</td>
  </tr>
  <%

  if (task.getType() != null) {
%><tr><td><label><%=I18n.get("TYPE")%></label></td><td><%=JSP.w(task.getType() != null ? task.getType().getDescription() : "")%></td></tr><%
  }
%>
  <tr><td><label><%=I18n.get("RELEVANCE")%></label></td><td><%=JSP.w(task.getRelevance())%>%</td></tr>
  <tr><td valign="top" style="min-width: 120px"><label><%=I18n.get("PROGRESS")%></label></td><td valign="top"><%progressBar.toHtml(pageContext);%>
    <%
      Map<String,Double> doubleMap = task.fullCheckup();

      double max=0;
      String message ="";

      for (String s : doubleMap.keySet()){
        message += (message.length()>0?", ":"")+I18n.get(s);
        max=max>doubleMap.get(s)?max:doubleMap.get(s);
      }

      if (max<=0)
        message=I18n.get("CK_ALL_OK");
    %><span class="textSmall"><span class="teamworkIcon"  style="margin-left:-18px" >!</span> <%=message%></span>
  </td></tr>
  <tr>
    <td valign="top" nowrap><%
      //from assig
      PageSeed editA = pageState.pageInThisFolder("taskAssignmentList.jsp",request);
      editA.addClientEntry("TASK_ID",task.getId());
      ButtonLink editAssig = new ButtonLink(editA);
      editAssig.toolTip = I18n.get("HELP_ASSIGNMENT_WORKLOG_ESTIMATION")+" - "+I18n.get("UNIT_DAY_HOUR");
      editAssig.label = DateUtilities.getMillisInDaysWorkHoursMinutes( task.getTotalWorklogEstimated());
      editAssig.enabled=!task.isNew();


    %><label title="<%=I18n.get("HELP_ASSIGNMENT_WORKLOG_ESTIMATION")%>">
      <%=I18n.get("FROM_ASSIG_SHORT")%>:</label></td><td><%editAssig.toHtmlInTextOnlyModality(pageContext);%>
  </td>
  </tr>
  <tr><td nowrap>
    <%
      editAssig.label = DateUtilities.getMillisInDaysWorkHoursMinutes(task.getTotalWorklogDone());
    %><label title="<%=I18n.get("HELP_ASSIGNMENT_WORKLOG_ESTIMATION")+" - "+I18n.get("UNIT_DAY_HOUR")%>"><%=I18n.get("WORKLOG_DONE")%>:&nbsp;</label></td><td><%editAssig.toHtmlInTextOnlyModality(pageContext);%>
  </td>
  </tr><%
  if (JSP.ex(task.getDescription())) {
%><tr>
  <td colspan="2" style="padding-top: 10px"><label><%=I18n.get("DESCRIPTION")%></label>
    <div class="textSmall linkEnabled"style="padding-bottom: 10px"><%=JSP.encode(task.getDescription())%></div>
  </td>
</tr><%
  }

    if (JSP.ex(task.getTags())) {
      PageSeed search = pageState.pageInThisFolder("taskList.jsp",request);
      search.command=Commands.FIND;

      List<String> tags = StringUtilities.splitToList(task.getTags(), ",");
  %><tr><td colspan="2"><%
    for (String tag:tags){
      search.addClientEntry("TASK_TAGS",tag);
      ButtonLink bl= new ButtonLink(tag,search);
      bl.additionalCssClass="tag";
      bl.toHtmlInTextOnlyModality(pageContext);
    }
  %></td></tr><%

    }

  if (JSP.ex(task.getNotes())) {
%><tr><td colspan="2"><label><%=I18n.get("NOTES_DELIVERABLES")%></label><br><span class="textSmall"style="padding-bottom: 10px"><%=JSP.encode(task.getNotes())%></span></td></tr><%
  }



  if (JSP.ex(task.getMilestones())) {
%>
  <tr><td><label><%=I18n.get("MILESTONES")%></label></td><td>
    <%
      // ----------------------------- MILESTONES START --------------------------------

      List<Task.TaskMilestone> tml = task.getMilestones();

      if (tml.size() > 0) {

    %><div id="milestones"><%

    PageSeed edChTsk = pageState.thisPage(request);
    edChTsk.setCommand(Commands.EDIT);
    //ButtonLink bl = ButtonLink.getTextualInstance("", edChTsk);
    for (Task.TaskMilestone tm : tml) {
      edChTsk.mainObjectId = tm.task.getId();
      %><span title="<%=tm.task.getDisplayName()%>"><%=JSP.w(tm.when)%><small title="<%=I18n.get("RELEVANCE")%>"><%=tm.task.getRelevance()>0?"("+tm.task.getRelevance()+ ")":""%></small>&nbsp;</span><%
    }
    %></div><%
    }
  %>
  </td></tr><%
  }

  JSONObject jsonData=task.getJsonData();
  if (jsonData.has("publicPage") ){
%>
  <tr>
    <td><label><%=I18n.get("PROJECT_PUBLIC_PAGE")%></label></td><td><%
    //show public link if active

    JSONObject options = jsonData.getJSONObject("publicPage");
    if (Fields.TRUE.equals(options.get("MASTER_PUBLIC_TASK"))){
  %><a target="_blank" href="<%=ApplicationState.serverURL + "/project/" + task.getId()%>"><%=ApplicationState.serverURL + "/project/" + task.getId()%></a><%
    }

  %></td></tr><%
  }

  //try to know if there is custom forms or custom field
  if (TaskBricks.hasCustomField()){
    //---------------------------------------------------- CUSTOM FIELDS ------------------------------------------------------------------------
    for (int i=1; i<7; i++) {
      if (!JSP.ex((String)ReflectionUtilities.getFieldValue("customField"+i,task)))
        continue;
      DesignerField dfStr = DesignerField.getCustomFieldInstance( "TASK_CUSTOM_FIELD_",i, task,!task.hasPermissionFor(logged,TeamworkPermissions.task_canWrite), false, false, pageState);
      if (dfStr!=null){
        dfStr.exportable=true;
        dfStr.separator="</td><td>";
%><tr><td><%dfStr.toHtml(pageContext);%></td></tr><%
      }
    }

  }

%>
  <%
    //link to meetings
    {

      String hql= "select distinct dip.meeting.id from "+ DiscussionPoint.class.getName()+" as dip where dip.task=:tsk order by dip.meeting.id desc";// order by dip.meeting.event.schedule.start desc";
      Query query = new OqlQuery(hql).getQuery();
      query.setEntity("tsk",task);
      List<Integer> meetingList = (List<Integer>) query.list();
      if (JSP.ex(meetingList)){
        PageSeed meetPs = pageState.pageFromRoot("agenda/agendaEditor.jsp");
        meetPs.command =Commands.EDIT;

  %><tr><td valign="top"><label><%=I18n.get("TASK_MEETING")%></label><td><%
  for (Integer mId: meetingList){
    Meeting m = Meeting.load(mId);
    Event event = m.getEvent();
    if (event ==null)
      continue;

    meetPs.mainObjectId=event.getId();
    ButtonLink bl= new ButtonLink(DateUtilities.dateAndHourToString(event.getSchedule().getStartDate())+" - "+event.getDisplayName() ,meetPs);
    bl.toHtmlInTextOnlyModality(pageContext);

%><br><%
  }
%></td></tr><%
    }
  }
%>
</table>
</div>


</div>

<div  style="width: 50%" class="container " >
  <%
    //---------------------------------------------------- STATISTICS START ------------------------------------------------------------------------

    if (!task.isNew()) {
  %>
  <jsp:include page="partTaskStatistics.jsp"/>
  <hr>
  <jsp:include page="partTaskStatGraph.jsp"/>

  <%
  } else {
  %><%=I18n.get("TASK_STATS_UNAVAILABLE_FOR_UNSAVED_TASK")%><br>&nbsp;<%
  }

%>
</div>

<%
  //---------------------------------------------------- WORKGROUP /DOCS START ------------------------------------------------------------------------
%>
<jsp:include page="part/partTaskColumnRight.jsp"/>
</div>
</div>

<%---------------------------------------------- MAIN COLUMN END ---------------------------------------------------------%>

<%---------------------------------------------- RIGHT COLUMN START ---------------------------------------------------------%><%
  if (!task.isNew()) {
    JspHelper side = new JspHelper("part/partTaskSideBar.jsp");
    side.parameters.put("TASK", task);
    side.toHtml(pageContext);
  }
%>

<%---------------------------------------------- RIGHT COLUMN END ---------------------------------------------------------%>

<jsp:include page="milestoneBar.jsp"/>
<%
    f.end(pageContext);

  }
%>
