<%@ page
        import="com.twproject.task.IssueStatus, com.twproject.task.Task, com.twproject.waf.html.StatusIcon, com.twproject.waf.html.TaskHeaderBar, org.jblooming.utilities.JSP, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.Tab, org.jblooming.waf.html.container.TabSet, org.jblooming.waf.html.core.HtmlIncluder, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List, com.twproject.task.IssueBricks, com.twproject.task.TaskStatus, java.util.Date, org.jblooming.waf.PluginBricks, org.jblooming.waf.html.container.DivOnMouseover, com.twproject.security.TeamworkPermissions, org.jblooming.operator.Operator, org.jblooming.waf.html.layout.HtmlColors" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  TaskHeaderBar taskHeaderBar = (TaskHeaderBar) JspIncluderSupport.getCurrentInstance(request);
  Task task = taskHeaderBar.task;

  boolean isNew = task.isNew();


  if (taskHeaderBar.pathToObject.destination==null){
    taskHeaderBar.pathToObject.destination = pageState.thisPage(request);
    taskHeaderBar.pathToObject.destination.setCommand(Commands.EDIT);
  }

  StatusIcon statusIcon = task.bricks.getStatusIcon(22, pageState);

  PageSeed taskEd = pageState.pageFromRoot("task/taskOverview.jsp");
  taskEd.command=Commands.EDIT;
  taskHeaderBar.pathToObject.selfDestination= taskEd;
  taskHeaderBar.pathToObject.separator="/ ";
  taskHeaderBar.pathToObject.currentNodeLeftElement=statusIcon;
  taskHeaderBar.pathToObject.toHtml(pageContext);


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
<div class="pathCodeWrapper">
  <%
    // se non hai permesso di vedere la root, ma è attiva la custom feature mostra un path
    Operator logged = pageState.getLoggedOperator();
    if (I18n.isActive("CUSTOM_FEATURE_ALWAYS_SHOW_TASK_PATH")&&!task.getRoot().hasPermissionFor(logged,TeamworkPermissions.task_canRead)){
      %><div class="pathSmall" style="max-width:1024px;"><%=task.getPath(" / ", true)%></div><%
    }
  %>
    <span class='pathCode' title='<%=I18n.get("REFERENCE_CODE")%>'><%=(task.isNew() ? "" : "T#" + task.getMnemonicCode())%>#</span>
  &nbsp;
  <span class='pathCode' >
   <%=I18n.get("START")%> <b class="<%=startInError?"warning":""%>" title="<%=startErrorMessage%>"><%=JSP.w(startDate)%></b>&nbsp;&nbsp;&nbsp;
    <%=I18n.get("END")%> <b class="<%=endInError?"warning":""%>" title="<%=endErrorMessage%>"><%=JSP.w(endDate)%></b>&nbsp;&nbsp;&nbsp;
    (<%=I18n.get("DAYS")%>: <b><%=task.getDuration()%></b>)
  </span>

</div>
<script>
  $(function(){
    $("#TASK_MENU").addClass('selected');
  });
</script>
<%
String color = task.getTaskColor();
if (I18n.isActive("CUSTOM_FEATURE_USE_PROJECT_COLOR") && JSP.ex(color) ) {
  %><style>.tabTaskColoured{
    background-color: <%=color%> !important;
  }
  .tabSelected.tab.tabTaskColoured {
    border-bottom-color: <%=color%>;
  }
  .tabTaskColoured a{
    color: <%=HtmlColors.contrastColor(color)%> !important;
  }</style><%

}



TabSet tabset = new TabSet("TASK_TABSET",pageState);
  {
    //-----------------TASK GENERAL
    PageSeed ps = pageState.pageFromRoot("task/taskOverview.jsp");
    ps.command = Commands.EDIT;
    ps.mainObjectId = task.getId();
    ButtonLink bl = new ButtonLink(I18n.get(task.getParent()==null?"PROJECT_GENERAL_TAB":"TASK_GENERAL_TAB"), ps);
    bl.id="btn_taskGeneralData";
    bl.enabled = !isNew;
    bl.hasFocus = pageState.thisPage(request).href.indexOf(ps.href) >= 0;
    Tab tab = new Tab("TASK_GENERAL_TAB", bl);
    tab.additionalCssClass="tabTaskColoured";
    tabset.addTab(tab);
  }

  if (task.isProcessDriven()) {
    //-----------------GRAPH
    PageSeed ps = pageState.pageFromRoot("task/taskProcess.jsp");
    ps.command = "SHOW_GRAPH";
    ps.mainObjectId = task.getId();
    ButtonSupport bl = ButtonLink.getBlackInstance(I18n.get("TASK_PROCESS_TAB"), ps);
    bl.enabled = !isNew;
    bl.hasFocus = pageState.thisPage(request).href.indexOf(ps.href) >= 0;
    Tab tab = new Tab("TASK_PROCESS_TAB", bl);
    tabset.addTab(tab);
  } else {
    //-----------------GANTT
    PageSeed ps = pageState.pageFromRoot("task/gantt/gantt.jsp");
    ps.command = "GANTT";
    ps.mainObjectId = task.getId();

    ButtonLink bl =new ButtonLink(I18n.get("TASK_GANTT_TAB"),ps);
    bl.enabled = !isNew;
    bl.hasFocus = pageState.thisPage(request).href.indexOf(ps.href) >= 0;
    bl.id="btnGantt";
    bl.additionalCssClass="lreq10 lreqLabel lreqActive";
    Tab tab = new Tab("TASK_GANTT_TAB", bl);
    tabset.addTab(tab);
  }

  //-----------------ASSIGNMENTS

  PageSeed psAddAssignment = pageState.pageFromRoot("task/taskAssignmentEditor.jsp");
  psAddAssignment.addClientEntry("TASK_ID",task.getId());
  psAddAssignment.command = Commands.ADD;

  {
    PageSeed ps;
    if ( task.getAssignementsSize() == 0 && task.getChildrenSize()==0 &&( task.hasPermissionFor(logged,TeamworkPermissions.assignment_canCRW) || task.hasPermissionFor(logged,TeamworkPermissions.resource_manage))) {
      ps = psAddAssignment;
    } else {
      ps = pageState.pageFromRoot("task/taskAssignmentList.jsp");
      ps.command = Commands.FIND;
      ps.addClientEntry("TASK_ID", task.getId());
    }
    ButtonLink bl = new ButtonLink(I18n.get("TASK_ASSIGNMENT_TAB") + (!task.isNew() ? " <font style='font-size:13px;line-height:10px'>(" + task.getAssignementsSize() + ")</font>" : ""), ps);
    bl.enabled = !isNew;
    bl.hasFocus = pageState.getHref().indexOf("Assignment") >= 0;
    Tab tab = new Tab("TASK_ASSIGNMENT_TAB", bl);
    tabset.addTab(tab);
  }
  {
    //-----------------COSTS
    PageSeed ps = pageState.pageFromRoot("task/financial/taskCostList.jsp");
    ps.command = "";
    ps.mainObjectId=task.getId();
    ButtonLink bl = new ButtonLink(I18n.get("TASK_COST_TAB"), ps);
    bl.id="btnCosts";
    bl.enabled = !isNew &&  task.hasPermissionFor(logged,TeamworkPermissions.task_cost_canRead);
    bl.hasFocus = pageState.thisPage(request).href.indexOf(ps.href) >= 0;
    Tab tab = new Tab("TASK_COST_TAB", bl);
    tabset.addTab(tab);
  }

  //------------------- ISSUE

  if (task.hasPermissionFor(logged,TeamworkPermissions.issue_canRead)) {
    PageSeed issues = pageState.pageFromRoot("task/taskIssueList.jsp");
    issues.mainObjectId=task.getId();
    ButtonLink is = new ButtonLink("", issues);
    is.enabled = !isNew;

    //issues or backlog
    is.label = I18n.get(JSP.w(task.getTags()).indexOf("SCRUM") >= 0?"ISSUES_BACKLOG":"ISSUES") + (!task.isNew() ? "  <font style='font-size:13px;'>(" + task.getTotalIssuesOpen() +"/"+task.getTotalIssues() + ")</font>" : "");

    Tab tab = new Tab("TASK_ISSUES_TAB", is);
    tabset.addTab(tab);

  }
  {
  //-----------------DOCUMENTS
  PageSeed psDocumentList = pageState.pageFromRoot("task/taskDocumentList.jsp");
  psDocumentList.setCommand("LIST_DOCS");
  psDocumentList.addClientEntry("TASK_ID",task.getId());
  ButtonLink blDocumentList = new ButtonLink(I18n.get("DOCUMENTS") + (!task.isNew() ? " <font style='font-size:13px;line-height:10px'>(" + task.getDocuments().size() + ")</font>" : ""), psDocumentList);
  blDocumentList.enabled = !isNew && task.hasPermissionFor(logged,TeamworkPermissions.document_canRead);
  blDocumentList.hasFocus = pageState.getHref().indexOf("Document") >= 0;

  Tab tab = new Tab("TASK_DOCS_TAB", blDocumentList);
  tabset.addTab(tab);
  }
  {
	  //-----------------DOCUMENTS
	  PageSeed psReportList = pageState.pageFromRoot("task/taskReportList.jsp");
	  psReportList.setCommand("LIST_REPORTS");
	  psReportList.addClientEntry("TASK_ID",task.getId());
	  ButtonLink blReportList = new ButtonLink(I18n.get("REPORTS") + (!task.isNew() ? " <font style='font-size:13px;line-height:10px'>(" + task.getReportSet().size() + ")</font>" : ""), psReportList);
	  blReportList.enabled = !isNew && task.hasPermissionFor(logged,TeamworkPermissions.document_canRead);
	  blReportList.hasFocus = pageState.getHref().indexOf("Report") >= 0;

	  Tab tab = new Tab("TASK_REPORT_TAB", blReportList);
	  tabset.addTab(tab);
	  }
  {
    //-----------------DISCUSSION
    PageSeed ps=pageState.pageFromRoot("task/taskForumList.jsp");
    ps.addClientEntry("TASK_ID",task.getId());
    ps.command="";
    ButtonLink forum= new ButtonLink(ps);
    int quantSon=task.getForumEntry()==null?0:task.getForumEntry().getChildrenSize();
    forum.label = I18n.get("TASK_TLOG_TAB", (!task.isNew() ? "<font style='font-size:13px;'>(" + quantSon + ")</font>" : ""));
    forum.enabled = !isNew;
    forum.hasFocus = pageState.getHref().indexOf("Forum") >= 0;
    forum.additionalCssClass="lreq10 lreqLabel lreqActive";
    Tab tab = new Tab("TASK_TLOG_TAB", forum);
    tabset.addTab(tab);
  }

  tabset.drawBar(pageContext);

  tabset.end(pageContext);
%>

