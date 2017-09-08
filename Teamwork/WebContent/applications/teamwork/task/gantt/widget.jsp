<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.task.Task, com.twproject.task.businessLogic.TaskAction, com.twproject.waf.TeamworkPopUpScreen,  net.sf.json.JSONObject, org.jblooming.utilities.DateUtilities, org.jblooming.waf.ScreenArea, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, org.jblooming.waf.view.RestState, com.twproject.task.businessLogic.TaskGanttAction, com.twproject.resource.Person" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);


  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    pageState.setPopup(true);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);
  } else {


    String key = pageState.getEntry("key").stringValueNullIfEmpty() + "";
    Task project = Task.loadFromKey(key);

    if (project == null) {
      %><h1>No project, no party!</h1><%
    } else {


    Person person= new Person();
    person.setId("-3");

    TeamworkOperator fop = new TeamworkOperator(person);
    fop.setId(-3);
    person.setMyself(fop);


    RestState restState = new RestState(fop);

    JSONObject json = new JSONObject();
    new TaskGanttAction(restState).cmdLoadProject(project.getId() + "", restState, json, true);

    //si mette la restState sul pageState per farla usare dalla ganttTemplates
    pageState.setAttribute("restState",restState);

    String jsPrj = json.getJSONObject("project").toString();

%>
<link rel=stylesheet href="<%=request.getContextPath()%>/applications/teamwork/task/gantt/gantt.css" type="text/css">
<link rel=stylesheet href="<%=request.getContextPath()%>/applications/teamwork/task/gantt/ganttPrint.css" type="text/css" media="print">
<script type="text/javascript" src="<%=request.getContextPath()%>/commons/js/jquery/svg/jquery.svg.min.js"></script>
<script type="text/javascript" src="<%=request.getContextPath()%>/commons/js/jquery/svg/jquery.svgdom.1.8.js"></script>

<script type="text/javascript" src="<%=request.getContextPath()%>/applications/teamwork/task/gantt/ganttUtilities.js"></script>
<script type="text/javascript" src="<%=request.getContextPath()%>/applications/teamwork/task/gantt/ganttTask.js"></script>
<script type="text/javascript" src="<%=request.getContextPath()%>/applications/teamwork/task/gantt/ganttDrawerSVG.js"></script>
<script type="text/javascript" src="<%=request.getContextPath()%>/applications/teamwork/task/gantt/ganttGridEditor.js"></script>
<script type="text/javascript" src="<%=request.getContextPath()%>/applications/teamwork/task/gantt/ganttMaster.js"></script>

<span class='lastMod' style="z-index: 30"> last change <%=DateUtilities.dateToRelative(project.getLastModified())%> by <%=project.getLastModifier()%></span>


<div id="workSpace"></div>

<script type="text/javascript">
  var ge;
  $(function () {
    ge = new GanttMaster();

    ge.permissions.canSeeFullEdit= false;
    ge.permissions.canSeePopEdit= false;
    ge.permissions.canWriteOnParent= false;
    ge.permissions.canWrite= false;
    ge.permissions.canAdd= false;

    ge.fillWithEmptyLines=false;


    var workSpace = $("#workSpace");

    workSpace.css({width:$(window).width(), height:$(window).height()});

    ge.init(workSpace);


    loadI18n(); //overwrite with localized ones

    //in order to force compute the best-fitting zoom level
    delete ge.gantt.zoom;

    $(".ganttButtonBar .requireCanWrite").prop("disabled", true).remove();
    $(".ganttButtonBar #fullscrbtn").remove();

    var jsPrj =<%=jsPrj%>;
    jsPrj.canWrite =false;
    ge.loadProject(jsPrj);

    ge.checkpoint(); //empty the undo stack

  });

   function loadI18n() {
    GanttMaster.messages = {
      "CANNOT_WRITE":                         "<%=I18n.get("NO_RIGHTS_FOR_CHANGING_FOLLOWING_TASK")%>",
      "CHANGE_OUT_OF_SCOPE":                  "<%=I18n.get("NO_RIGHTS_FOR_UPDATE_PARENTS_OUT_OF_EDITOR_SCOPE")%>",
      "START_IS_MILESTONE":                   "<%=I18n.get("START_IS_MILESTONE")%>",
      "END_IS_MILESTONE":                     "<%=I18n.get("END_IS_MILESTONE")%>",
      "TASK_HAS_CONSTRAINTS":                 "<%=I18n.get("TASK_HAS_CONSTRAINTS")%>",
      "GANTT_ERROR_DEPENDS_ON_OPEN_TASK":     "<%=I18n.get("GANTT_ERROR_DEPENDS_ON_OPEN_TASK")%>",
      "GANTT_ERROR_DESCENDANT_OF_CLOSED_TASK":"<%=I18n.get("GANTT_ERROR_DESCENDANT_OF_CLOSED_TASK")%>",
      "TASK_HAS_EXTERNAL_DEPS":               "<%=I18n.get("TASK_HAS_EXTERNAL_DEPS")%>",
      "GANNT_ERROR_LOADING_DATA_TASK_REMOVED":"<%=I18n.get("GANNT_ERROR_LOADING_DATA_TASK_REMOVED")%>",
      "CIRCULAR_REFERENCE":                   "<%=I18n.get("CIRCULAR_REFERENCE")%>",
      "CANNOT_DEPENDS_ON_ANCESTORS":          "<%=I18n.get("CANNOT_DEPENDS_ON_ANCESTORS")%>",
      "INVALID_DATE_FORMAT":                  "<%=I18n.get("INVALID_DATA")%>",
      "GANTT_ERROR_LOADING_DATA_TASK_REMOVED":"<%=I18n.get("GANTT_ERROR_LOADING_DATA_TASK_REMOVED")%>",
      "CANNOT_CLOSE_TASK_IF_OPEN_ISSUE":      "<%=I18n.get("CANNOT_CLOSE_TASK_IF_OPEN_ISSUE")%>",
      "TASK_MOVE_INCONSISTENT_LEVEL":         "<%=I18n.get("TASK_MOVE_INCONSISTENT_LEVEL")%>",
      "GANTT_QUARTER_SHORT":                  "<%=I18n.get("GANTT_QUARTER_SHORT")%>",
      "GANTT_SEMESTER_SHORT":                 "<%=I18n.get("GANTT_SEMESTER_SHORT")%>"
    };
  }

</script>
<style type="text/css">
  #twInnerContainerPopup {
    padding: 0;
  }
</style>

<jsp:include page="ganttTemplates.jsp"/>

<% }

}%>
