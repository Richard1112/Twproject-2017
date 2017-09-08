<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.resource.Person,
com.twproject.task.Assignment, com.twproject.task.TaskBricks,
org.jblooming.agenda.Period, org.jblooming.utilities.JSP,
org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.input.SmartCombo, org.jblooming.waf.settings.I18n,
org.jblooming.waf.view.PageState, java.util.Date, java.util.List"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);


  TeamworkOperator logged= (TeamworkOperator) pageState.getLoggedOperator();

  Person resource = logged.getPerson();
  List<Assignment>assigs = resource.getActiveAssignments(Period.getDayPeriodInstance(new Date()),true);

  %><div id="wp_tc_slim" class="portletBox tcSlim lreq10 lreqLabel"><h1><%=I18n.get("TIMECOUNTER_MENU")%></h1><%

  if (JSP.ex(assigs)){

  Assignment assignment = Assignment.getCountedAssignment(logged);
  if (assignment!=null) {
    pageState.addClientEntry("wpTCSAssignment",assignment);
  }

  // questa e il path local a questa pagina che può essere sovrascritto con una custom label
  String descriptionFormUrl=pageState.pageFromRoot("task/worklog/worklogDescriptionForm.jsp").toLinkToHref();
  if (I18n.isActive("CUSTOM_FEATURE_WORKLOG_FORM")){
    descriptionFormUrl=I18n.get("CUSTOM_FEATURE_WORKLOG_FORM");
  }

  pageState.getEntryOrDefault("wpTCSAssignment");

  SmartCombo comboAssigs = TaskBricks.getAllAssignmentsOfCombo("wpTCSAssignment", resource,Period.getDayPeriodInstance(new Date()), true, false,false,false,null);
  comboAssigs.label = "";
  comboAssigs.separator = "";
  comboAssigs.fieldSize = 45;
  comboAssigs.script = "style=width:100%";
  comboAssigs.required=true;
  comboAssigs.disabled=assignment!=null;
  comboAssigs.innerLabel=I18n.get("START_WORKING_ON_TASK");

%><table class="table" cellpadding="4">

  <tr>
    <td><%comboAssigs.toHtmlI18n(pageContext);%></td>
    <td style="position: relative"  width="1%">
      <div class="timeCounter" style="white-space: nowrap">00<span class="counterLabel">h</span> 00<span class="counterLabel">m</span><span class="sec"> 00<span class="counterLabel">s</span></span></div>
    </td>
    <td align="center" width="1%"><%
      ButtonJS bs=new ButtonJS("wpTCSStartStop($(this));");
      bs.iconChar = assignment==null ? "a" : "s";
      bs.additionalCssClass = assignment==null ? "controls play" : "controls stop";
      bs.label = "";
      bs.toHtmlInTextOnlyModality(pageContext);
  %></td></tr>
  </table>

<script>
  $(function () {
    var assig = <%=assignment==null?"undefined":assignment.jsonify()%>;

    if (assig)
      $("#wp_tc_slim").find(".timeCounter").data("time", assig.countingStartedMillis).activateTimeCounter();
  });

  function wpTCSStartStop(el) {
    var assCombo = $("#wpTCSAssignment");
    var wp = $("#wp_tc_slim");
    if (assCombo.isFullfilled()) {
      var assId = assCombo.val();
      showSavingMessage();

      if (el.is(".stop")) {
        var request = {CM: "STOPCOUNTER", assId: assId};
        $.getJSON(contextPath + "/applications/teamwork/task/worklog/worklogAjaxController.jsp", request, function (response) {
          jsonResponseHandling(response);
          if (response.ok) {
            $("#wp_tc_slim .timeCounter").stopTimeCounter();
            el.removeClass("stop").addClass("play").find(".teamworkIcon").html("a");
            wp.find(":input").prop("disabled", false);
            top.$("body").trigger("timeCounterEvent", [
              {type: "stop", response: response}
            ]);
            wpTCSAskForAction(el,response);
          }
          hideSavingMessage();
        });


      } else {   //START
        var request = {CM: "STARTCOUNTER", assId: assId};
        $.getJSON(contextPath + "/applications/teamwork/task/worklog/worklogAjaxController.jsp", request, function (response) {
          jsonResponseHandling(response);
          if (response.ok) {
            $("#wp_tc_slim .timeCounter").data("time", new Date().getTime()).activateTimeCounter();
            el.removeClass("play").addClass("stop").find(".teamworkIcon").html("s");
            wp.find(":input").prop("disabled", true);
            wpTCSAskForAction(el,response);
            top.$("body").trigger("timeCounterEvent", [
              {type: "start", response: response}
            ]);

          }
          hideSavingMessage();
        });

      }

    }
  }


  function wpTCSAskForAction(el,response) {
    //console.debug("wpTCSAskForAction",el,response)
    if (!response.worklog)
      return;
    openWorklogEditorCloseToElement(el, {assId:response.worklog.assId, title: response.worklog.taskCode+" - "+ response.worklog.taskName , millis:new Date().getTime()},function(wlBox){
      wlBox.find("#WORKLOG_DURATION").val(getMillisInHoursMinutes(response.worklog.duration));
      wlBox.find("#WORKLOG_ACTION").focus();
    });
  }

</script>


<%
  } else {
      %><%=I18n.get("NO_ASSIG")%><%
  }

%></div>
