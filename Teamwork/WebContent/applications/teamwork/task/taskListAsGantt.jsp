<%@ page import="com.twproject.task.businessLogic.TaskController, com.twproject.waf.TeamworkHBFScreen, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.SessionState,
                 org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.Tab, org.jblooming.waf.html.container.TabSet,
                 org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.DataTable, org.jblooming.waf.html.input.LoadSaveFilter, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jblooming.waf.html.container.ButtonBar" %> <%


    PageState pageState = PageState.getCurrentPageState(request);

    if (!pageState.screenRunning) {
      pageState.screenRunning = true;
      final ScreenArea body = new ScreenArea(new TaskController(), request);
      body.areaHtmlClass="lreq10 lreqPage";
      TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
      lw.register(pageState);
      pageState.perform(request, response);
      pageState.toHtml(pageContext);
    } else {

      String nocache= ApplicationState.getBuild();

      String savedFilterName = pageState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty();
%>

<link rel=stylesheet href="gantt/gantt.css?<%=nocache%>" type="text/css" >
<link rel=stylesheet href="gantt/ganttPrint.css?<%=nocache%>" type="text/css" media="print">

<script type="text/javascript" src="<%=request.getContextPath()%>/commons/js/jquery/svg/jquery.svg.min.js"></script>
<script type="text/javascript" src="<%=request.getContextPath()%>/commons/js/jquery/svg/jquery.svgdom.1.8.js"></script>

<script type="text/javascript" src="gantt/ganttUtilities.js?<%=nocache%>"></script>
<script type="text/javascript" src="gantt/ganttTask.js?<%=nocache%>"></script>
<script type="text/javascript" src="gantt/ganttDrawerSVG.js?<%=nocache%>"></script>
<script type="text/javascript" src="gantt/ganttGridEditor.js?<%=nocache%>"></script>
<script type="text/javascript" src="gantt/ganttMaster.js?<%=nocache%>"></script>

<%---------------------------------------------- MAIN COLUMN START ---------------------------------------------------------%>


<div class="mainColumn">
<%---------------------------------------------- HEADER START ---------------------------------------------------------%>

<h1 class="filterTitle" defaultTitle="<%=I18n.get("VIEW_AS_GANTT")%>">
  <%=JSP.ex(savedFilterName)?I18n.get(savedFilterName):I18n.get("VIEW_AS_GANTT")%>
</h1>

<%---------------------------------------------- HEADER END ---------------------------------------------------------%>


<script>
    $("#TASK_MENU").addClass('selected');

    var _tasks=[];
    var _resources={};
    var _roles={};

</script>
<style>
  .table.dataTable th, .tableHead {border-left: none;}
  #workSpace{margin-top: -10px;}
</style>

<%
  PageSeed self = pageState.thisPage(request);
  self.setCommand(Commands.FIND);
  Form f = new Form(self);
  f.id = "TASK_LIST";
  f.alertOnChange = true;

  pageState.setForm(f);
  f.start(pageContext);

  //usato per accorgersi se ci sono modifiche non salvate
  TextField dummy=new TextField("Gantt","");
  dummy.type="hidden";
  dummy.toHtml(pageContext);


  DataTable dataTable= new DataTable("TSKLST",f, new JspHelper("/applications/teamwork/task/rowTaskListAsGantt.jsp"), TaskController.class,pageState );

  dataTable.addHeader("","task.status");
  dataTable.addHeader(I18n.get("TASK_CODE"),"task.code");
  dataTable.addHeader(I18n.get("TASK_NAME"), "task.name");
  dataTable.addHeader(I18n.get("TYPE"), "task.type");
  dataTable.addHeader(I18n.get("START"),"task.schedule.start");
  dataTable.addHeader(I18n.get("END"),"task.schedule.end");
  dataTable.addHeader(I18n.get("ISSUES"),"task.totalIssuesOpen");
  dataTable.addHeader(I18n.get("WORKLOG_DONE_SHORT"),"task.totalWorklogDone");
  dataTable.addHeader("%","task.progress");
  dataTable.addHeader("");
  dataTable.tableClass = "table";



  LoadSaveFilter lsfb = new LoadSaveFilter(I18n.get("MY_SAVED_FILTERS"), "TASK", dataTable.form);
  lsfb.drawButtons = false;
  lsfb.drawEditor = true;

  //----------------------------------------------------------------------------  START FILTER ----------------------------------------------------------------------------

  JspHelper filter = new JspHelper("partTaskListFilter.jsp");
  filter.parameters.put("searchButton", dataTable.getSearchButton());
  filter.parameters.put("loadSaveFilter",lsfb);
  filter.toHtml(pageContext);



  //----------------------------------------------------------------------------  END FILTER ----------------------------------------------------------------------------

    ButtonSubmit toList=new ButtonSubmit(I18n.get("VIEW_AS_LIST"),"",f);
    toList.variationsFromForm.href="taskList.jsp";
    toList.hasFocus=true;

    ButtonSubmit toGantt=new ButtonSubmit(I18n.get("VIEW_AS_GANTT"),"",f);
    toGantt.variationsFromForm.href="taskListAsGantt.jsp";

%>
<div style="position: relative;">
  <%dataTable.drawPaginator(pageContext);%>
</div>
  <%

  //---------------------------------  INIZIO TABELLA ----------------------------------------------
  dataTable.drawTable(pageContext);

  f.end(pageContext);
  %>


  <style>
    .ganttButtonBar h1{
      display:none;
    }

  </style>
<div id="workSpace" style="border-top:2px solid #bbb "></div>

<script type="text/javascript">
var ge;
$(function() {
  ge = new GanttMaster();

  ge.isMultiRoot=true;
  ge.permissions.canAdd=false;
  ge.permissions.canMoveUpDown=false;
  ge.permissions.canInOutdent=false;
  ge.permissions.canSeeCriticalPath=false;
  ge.permissions.canSeeDep=false;
  ge.permissions.canAdd=false;
  ge.permissions.canAddIssue=true;
  ge.permissions.canSeeDep= false;

  var workSpace = $("#workSpace");
  workSpace.addClass("isGanttList");// enable display rows alternate

  ge.init(workSpace);

  loadI18n(); //overwrite with localized ones

  //in order to force compute the best-fitting zoom level
  delete ge.gantt.zoom;


  //tarocchi sulla table head: si nasconde la tableHead
  $(".dataTableHead tr:first").hide();
  synchOrderButtons();


  //si tolgono gli header non usati per ordinare
  $(".dataTableHead .tableHead:not(:last)").hide();

//  $(window).resize(function(){
////          workSpace.css({width:"100%",height:$(window).height() - (workSpace.position().top) });
//    workSpace.css({width:"100%",height:$(window).height() - (workSpace.offset().top+20) });
//    workSpace.trigger("resize.gantt");
//  }).oneTime(5,"resize",function(){$(this).trigger("resize")});

});



<%-- questa funzione è chiamata dalla dataTable quando ha finito di disegnare le righe
In questo caso ogni riga inietta uno <script> che aggiunge valori agli array  _tasks[],_roles{} e _resources{} (n.b.: sono array o mappe)
Questa funzione copia quest valori ricostruendo un oggetto che viene caricato dal gantt. Dopo di che le mappe vengono svuotate per l'eventuale pagina successiva.--%>

function dataTableCallback(num){
  //console.debug("dataTableCallback",num)

  if (num>0) {
    $("#btn_ttlp,#btn_gnt_print,#btn_print").removeAttr("disabled").show();
    $("#no_task_add_box").hide();
  }else {
    $("#btn_ttlp,#btn_gnt_print,#btn_print").attr("disabled", true).hide();
    $("#no_task_add_box").show();
  }


  synchOrderButtons(); // dato che l'ordinamento è cambiato si ricopiano dalla bottoniera vera

  delete ge.gantt.zoom;

  var tasks=[].concat(_tasks);
  var roles=[];
  for (var id in _roles){
    roles.push(_roles[id]);
  }
  var resources=[];
  for (var id in _resources){
    resources.push(_resources[id]);
  }


  //console.debug("dataTableCallback zoom",ge.gantt.zoom)
  ge.loadProject({
    tasks:tasks,
    roles:roles,
    resources:resources,
    serverTimeOffset:<%=SessionState.getTimeZone().getOffset(System.currentTimeMillis())%>,
    canWriteOnParent:false,
    canWrite:true
  });
  ge.checkpoint();
  ge.resourceUrl = "gantt/res/"; // URL to resources (images etc.)


  $(window).trigger("resize");

  //si svuotano i valori settati dalla rowTaskListAsGantt
  _tasks=[];
  _roles={};
  _resources={};

}

function saveGanttOnServer(){
  var prj = ge.saveGantt();

  if (ge.deletedTaskIds.length>0){
    if (!confirm("<%=I18n.get("TASK_THAT_WILL_BE_REMOVED")%>\n"+ge.deletedTaskIds.length))
      return;
  }

  showSavingMessage();
  $.ajax("gantt/ganttAjaxController.jsp", {
    dataType:"json",
    data: {CM:"SVTASKLIST",taskList:JSON.stringify(prj)},
    type:"POST",

    success: function(response) {
      hideSavingMessage();
      //console.debug(response);
      jsonResponseHandling(response);
      if (response.ok) {
        //prof.stop();
        if (response.project){
          ge.loadProject(response.project); //must reload as "tmp_" ids are now the good ones
          ge.checkpoint();
          $(window).trigger("resize");
        } else {
          ge.reset();
        }
      }
    }

  });
}


function synchOrderButtons(){
  //si copiano gli header ordinabili dalla dataTable al gridEditor ATTENZIONE AL NUMERO DELLE COLONNE
  $(".gdfTable .gdfColHeader").eq(1).empty().append($(".dataTableHead .tableHead:eq(0) span").clone()); // status
  $(".gdfTable .gdfColHeader").eq(2).empty().append($(".dataTableHead .tableHead:eq(1) span").clone()); // code
  $(".gdfTable .gdfColHeader").eq(3).empty().append($(".dataTableHead .tableHead:eq(2) span").clone()); // name
  $(".gdfTable .gdfColHeader").eq(5).empty().append($(".dataTableHead .tableHead:eq(4) span").clone()); // start
  $(".gdfTable .gdfColHeader").eq(7).empty().append($(".dataTableHead .tableHead:eq(5) span").clone()); // end
  $(".gdfTable .gdfColHeader").eq(9).empty().append($(".dataTableHead .tableHead:eq(8) span").clone()); // progress
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

<jsp:include page="plan/workloadUtilities.js.jsp"/>

<jsp:include page="gantt/ganttTemplates.jsp"/>




  <%
  //---------------------------------  FINE TABELLA ----------------------------------------------


  new JspHelper("taskListNothingFound.jsp").toHtml(pageContext);
%>

</div>
<%---------------------------------------------- MAIN COLUMN END ---------------------------------------------------------%>

<%---------------------------------------------- RIGHT COLUMN START ---------------------------------------------------------%>
<% JspHelper rightColumn = new JspHelper("partTaskListRightColumn.jsp");
  rightColumn.parameters.put("loadSaveFilter",lsfb);
  rightColumn.toHtml(pageContext);%>

<%------------------------------------------------ RIGHT COLUMN END ---------------------------------------------------------%>

<%



    }

%>
