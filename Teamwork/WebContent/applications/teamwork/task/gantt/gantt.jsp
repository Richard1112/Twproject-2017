<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.security.TeamworkPermissions, com.twproject.task.Task, com.twproject.task.businessLogic.TaskController, com.twproject.waf.TeamworkHBFScreen, com.twproject.waf.html.TaskHeaderBar, org.jblooming.waf.ScreenArea, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jblooming.waf.constants.Fields, org.jblooming.utilities.JSP, org.jblooming.waf.html.input.TextArea" %>
<%
    PageState pageState = PageState.getCurrentPageState(request);
    if (!pageState.screenRunning) {
        pageState.screenRunning = true;
        final ScreenArea body = new ScreenArea(new TaskController(),request);
        TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
        lw.register(pageState);
        pageState.perform(request, response).toHtml(pageContext);
    } else {

        TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
        Task focusTask= (Task) pageState.getMainObject();
        if (focusTask!=null)
            focusTask.testPermission(logged,TeamworkPermissions.task_canRead );
        else
            logged.testPermission(TeamworkPermissions.project_canCreate );

        PageSeed thisPage = pageState.thisPage(request);
        thisPage.mainObjectId=focusTask.getId();
        Form f = new Form(thisPage);
        f.alertOnChange=true;
        pageState.setForm(f);
        f.start(pageContext);

        //usato per accorgersi se ci sono modifiche non salvate
        TextField dummy=new TextField("Gantt","");
        dummy.type="hidden";
        dummy.toHtml(pageContext);

        f.end(pageContext);


        Task topTask= focusTask;
        if (I18n.isActive("CUSTOM_FEATURE_NAVIGATE_TREE_TO_TOP")) {
            while (topTask.getParent() != null) {
                if (topTask.getParent().hasPermissionFor(logged, TeamworkPermissions.task_canRead))
                    topTask = topTask.getParent();
                else
                    break;
            }
        }

%><div class="mainColumn"><%

    pageState.addClientEntry("TASK_TABSET","TASK_GANTT_TAB");

    TaskHeaderBar head = new TaskHeaderBar(focusTask);
    head.toHtml(pageContext);
    String nocache=ApplicationState.getBuild();

%>

    <link rel=stylesheet href="gantt.css?<%=nocache%>" type="text/css" >
    <link rel=stylesheet href="ganttPrint.css?<%=nocache%>" type="text/css" media="print">

    <script type="text/javascript" src="<%=request.getContextPath()%>/commons/js/jquery/svg/jquery.svg.min.js"></script>
    <script type="text/javascript" src="<%=request.getContextPath()%>/commons/js/jquery/svg/jquery.svgdom.1.8.js"></script>

    <script type="text/javascript" src="ganttUtilities.js?<%=nocache%>"></script>
    <script type="text/javascript" src="ganttTask.js?<%=nocache%>"></script>
    <script type="text/javascript" src="ganttDrawerSVG.js?<%=nocache%>"></script>
    <script type="text/javascript" src="ganttGridEditor.js?<%=nocache%>"></script>
    <script type="text/javascript" src="ganttMaster.js?<%=nocache%>"></script>

  <% if (ApplicationState.platformConfiguration.development && JSP.ex(pageState.getEntry("executeTest"))){%>
    <script type="text/javascript" src="ganttTestSuite.js?<%=nocache%>"></script>
  <%}%>

  <div class="lreq10 lreqPage">
  <div id="workSpace"></div>
  </div>

  <script type="text/javascript">

var ge;
$(function () {

  //fix old collapsed task
  fixOldCollapsedTaskString();

  ge = new GanttMaster();
  ge.resourceUrl=contextPath+"/applications/teamwork/task/gantt/res/";

  ge.set100OnClose=<%=Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("SET100ONCLOSE"))%>;

  ge.init( $("#workSpace"));
  loadI18n(); //overwrite with localized ones

  //in order to force compute the best-fitting zoom level
  delete ge.gantt.zoom;
  <% if (topTask.isNew()){ %>
  newProjectFromServer();
  <%} else {%>

  loadGanttFromServer("<%=topTask.getId()%>", function (response) {
    $("body").oneTime(100, "clickrow", function () { // delayed as gant is refreshed after 50ms
      ge.getTask("<%=focusTask.getId()%>").rowElement.click();
    });
  });
  <%}%>

});

function newProjectFromServer() {
  //console.debug("newProjectFromServer");
  var data = {CM: "NEWPROJECT"};
  $.getJSON("ganttAjaxController.jsp", data, function (response) {
    jsonResponseHandling(response);
    if (response.ok) {
      //console.debug("about to load ",response.project);
      ge.loadProject(response.project);
      ge.checkpoint(); //empty the undo stack

      ge.editor.element.oneTime(100, "cl", function () {$(this).find("tr.emptyRow:first").click()});
    }
  });
}

function loadGanttFromServer(taskId, callback) {
  //console.debug("loadGanttFromServer",taskId);
  var data = {CM: "LOADPROJECT", taskId: taskId};
  $.getJSON("ganttAjaxController.jsp", data, function (response) {
    jsonResponseHandling(response);
    if (response.ok) {
      //console.debug("about to load ",response.project);
      ge.loadProject(response.project);
      ge.checkpoint(); //empty the undo stack
      if (typeof(callback) == "function")
        callback(response);
    }
  });

}


function saveGanttOnServer(el) {
  if (el.is(".disabled"))
    return;
  var prj = ge.saveGantt();

  if (ge.deletedTaskIds.length > 0) {
    if (!confirm("<%=I18n.get("TASK_THAT_WILL_BE_REMOVED")%>\n" + ge.deletedTaskIds.length))
      return;
  }

  //var time = new Date().getTime();
  //console.debug(time)
  showSavingMessage();
  $.ajax("ganttAjaxController.jsp", {
    dataType: "json",
    data:     {CM: "SVPROJECT", prj: JSON.stringify(prj)},
    type:     "POST",

    success: function (response) {
      hideSavingMessage();
      jsonResponseHandling(response);
      if (response.ok) {
        //console.debug(new Date().getTime()-time)
        if (response.project) {
          ge.loadProject(response.project); //must reload as "tmp_" ids are now the good ones
          ge.checkpoint(); //empty the undo stack
        } else {
          ge.reset();
        }
      }
    }
  });
}


function fixOldCollapsedTaskString(){
  if (localStorage && localStorage.getItem("TWPGanttCollTasks") && !localStorage.getItem("TWPGanttCollTasks").startsWith("[")) {
    localStorage.setObject("TWPGanttCollTasks",[]);
  }

}

<%-------------------------------- WORKLOAD MANAGEMENT ------------------------------------------%>
var lastPeriodChecked;

function getCurrentTaskPeriod() {
  //console.debug("getCurrentTaskPeriod",$(".ganttTaskEditor [name=start]")) ;
  var ret = {startMillis: Date.parseString($(".ganttTaskEditor [name=start]").val()).setHours(0, 0, 0), endMillis: Date.parseString($(".ganttTaskEditor [name=end]").val()).setHours(23, 59, 59)};
  return ret;
}

function updateResourceLoad(el) {
  //console.debug("updateResourceLoad")
  var row = el.closest("tr");
  computeResourceLoad(el, getCurrentTaskPeriod(), row.find("[name=resourceId]").val(), millisFromString(row.find("[name=effort]").val(),true), row.attr("assId"));
}

function workloadDatesChanged() {
  //console.debug("workloadDatesChanged");
  var ret = getCurrentTaskPeriod();
  if (!lastPeriodChecked || ret.startMillis != lastPeriodChecked.startMillis || ret.endMillis != lastPeriodChecked.endMillis) {
    lastPeriodChecked = ret;
    $("tr.assigEditRow [name=resourceId]").each(function () {
      var inp = $(this);
      if (parseInt(inp.val())) {
        updateResourceLoad(inp);
      }
    })
  }
}


</script>
    <jsp:include page="../plan/workloadUtilities.js.jsp"/>
    <jsp:include page="ganttTemplates.jsp"/>
</div>
<%---------------------------------------------- RIGHT COLUMN START ---------------------------------------------------------%><%
    JspHelper side = new JspHelper("/applications/teamwork/task/part/partTaskSideBar.jsp");
    side.parameters.put("TASK", focusTask);
    side.toHtml(pageContext);
//---------------------------------------------- RIGHT COLUMN END ---------------------------------------------------------

 }%>
