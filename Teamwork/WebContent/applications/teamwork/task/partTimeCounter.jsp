<%@ page import="com.twproject.resource.Person, com.twproject.task.Assignment, com.twproject.task.Task, com.twproject.waf.html.TimeCounterDrawer, org.jblooming.agenda.Period, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.OperatorConstants, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.input.ComboBox, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.Date, java.util.List, java.util.Collections, com.twproject.utilities.TeamworkComparators, com.twproject.operator.TeamworkOperator"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
  Person resource = logged.getPerson();
  List<Assignment>assigs = resource.getActiveAssignments(Period.getDayPeriodInstance(new Date()),true);
  Collections.sort(assigs, new TeamworkComparators.AssignmentByPriority(new Date()));

%><h1 style="display: inline;"><%=I18n.get("TIMECOUNTER_MENU")%></h1> <span id="tcSearchPlace"></span><%
  if (assigs.size()>0) {
%>
<h2 class="sectionTitle"><%=I18n.get("ACTIVITY_ALL_IN_ONE")%></h2>
<table class="table fixHead timeCounterWidget" cellpadding="4" cellspacing="0" border="0" id="tcPrj">
  <thead class="dataTableHead" style="">
  <tr>

    <th class="tableHead" colspan="3" nowrap="" id="tcPrjFC"><%=I18n.get("ASSIGNMENT")%></th>
    <th class="tableHead" nowrap=""><%=I18n.get("PRIORITY")%></th>
    <th class="tableHead" nowrap="" colspan="2"></th>
  </thead>
  <%

    for (Assignment ass : assigs) {
      Task task = ass.getTask();
      if (task.isActive() && ass.isEnabled() && Assignment.ACTIVITY_ALL_IN_ONE.equals(ass.getActivity())) {
        new TimeCounterDrawer(ass).toHtml(pageContext);
      }
    }
  %></table>

<br>
<h2 class="sectionTitle"><%=I18n.get("ACTIVITY_REPEATED_IN_TIME")%></h2>
<table class="table timeCounterWidget" cellpadding="4" cellspacing="0" border="0" id="tcRoutine">
  <thead class="dataTableHead" style="">
  <tr>
    <th class="tableHead" colspan="3" nowrap="" id="tcRoutineFC"><%=I18n.get("ASSIGNMENT")%></th>
    <th class="tableHead" nowrap=""><%=I18n.get("PRIORITY")%></th>
    <th class="tableHead" nowrap=""></th>
  </thead><tbody><%

  for (Assignment ass : assigs) {
    Task task = ass.getTask();
    if (task.isActive() && ass.isEnabled() && Assignment.ACTIVITY_REPEATED_IN_TIME.equals(ass.getActivity())) {
      new TimeCounterDrawer(ass).toHtml(pageContext);
    }
  }

  Assignment assignment = Assignment.getCountedAssignment(logged);


  // questa e il path local a questa pagina che può essere sovrascritto con una custom label
  String descriptionFormUrl=pageState.pageFromRoot("task/worklog/worklogDescriptionForm.jsp").toLinkToHref();
  if (I18n.isActive("CUSTOM_FEATURE_WORKLOG_FORM")){
    descriptionFormUrl=I18n.get("CUSTOM_FEATURE_WORKLOG_FORM");
  }

%></tbody></table>


<script>

  //inject the table search
  createTableFilterElement($("#tcSearchPlace"),"[assId]",".columnTaskName,.columnTaskCode").css("float","none");

  $(function () {
    var assig =
    <%=assignment==null?"undefined":assignment.jsonify()%>
    if (assig)
      $(".timeCounterWidget tr[assId=" + assig.id + "]").find(".timeCounter").data("time", assig.countingStartedMillis).activateTimeCounter().show();
  });

  function pTCStartStop(el) {
    var row = el.closest("tr[assId]");
    var assId = row.attr("assId");
    showSavingMessage();

    if (el.is(".stop")) {//   ---------------------------------  STOP
      var request = {CM: "STOPCOUNTER", assId: assId};
      $.getJSON(contextPath + "/applications/teamwork/task/worklog/worklogAjaxController.jsp", request, function (response) {
        //console.debug("stop: " + row.attr("assId"));
        jsonResponseHandling(response);
        if (response.ok) {
          row.find(".timeCounter").stopTimeCounter().hide();
          el.removeClass("stop").addClass("play").find(".teamworkIcon").html("a");
          top.$("body").trigger("timeCounterEvent", [
            {type: "stop", response: response}
          ]);

          pTCAskForAction(el,response);
        }
        hideSavingMessage();
      });

    } else {//   ---------------------------------  START

      //se ce n'è uno aperto lo stoppo
      $(".timeCounterWidget .controls.stop").click();

      var request = {CM: "STARTCOUNTER", assId: assId};
      $.getJSON(contextPath + "/applications/teamwork/task/worklog/worklogAjaxController.jsp", request, function (response) {
        //console.debug("start: " + row.attr("assId"));
        jsonResponseHandling(response);
        if (response.ok) {
          row.find(".timeCounter").data("time", new Date().getTime()).show().activateTimeCounter();
          el.removeClass("play").addClass("stop").find(".teamworkIcon").html("s");
          pTCAskForAction(el,response);
          top.$("body").trigger("timeCounterEvent", [
            {type: "start", response: response}
          ]);
        }
        hideSavingMessage();
      });

    }
  }


  function pTCAskForAction(el,response) {
    //console.debug("pTCAskForAction",el,response)
    if (!response.worklog)
      return;

    openWorklogEditorCloseToElement(el,{assId:response.worklog.assId, title: response.worklog.taskCode+" - "+ response.worklog.taskName ,millis: new Date().getTime()},function(wlBox){
      wlBox.find("#WORKLOG_DURATION").val(getMillisInHoursMinutes(response.worklog.duration));
      wlBox.find("#WORKLOG_ACTION").focus();
    });


  }

</script>




<%
} else {
%><h2 class="hint" style="margin:10px 0"><%=I18n.get("NO_ASSIGNMENTS_HERE_FORYOU")%></h2><%
}
%>
