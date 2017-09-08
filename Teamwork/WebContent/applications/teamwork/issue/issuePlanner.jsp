<%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8" %><%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.resource.Resource, com.twproject.resource.ResourceBricks, com.twproject.security.TeamworkPermissions, com.twproject.task.Issue, com.twproject.task.IssueBricks, com.twproject.waf.TeamworkPopUpScreen, net.sf.json.JSONObject, org.jblooming.agenda.CompanyCalendar, org.jblooming.waf.ScreenArea, org.jblooming.waf.SessionState, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.core.JST, org.jblooming.waf.html.input.SmartCombo, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.ClientEntry, org.jblooming.waf.view.PageState, java.util.Date, org.jblooming.waf.html.layout.HtmlColors, com.twproject.waf.TeamworkHBFScreen, org.jblooming.waf.html.state.Form, org.jblooming.waf.view.PageSeed, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.input.LoadSaveFilter, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.display.DataTable, com.twproject.task.businessLogic.IssueController, org.jblooming.agenda.Period, org.jblooming.utilities.JSP" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;

    final ScreenArea body = new ScreenArea(new IssueController(), request);
    body.areaHtmlClass="lreq20 lreqPage";

    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();


    PageSeed self = pageState.thisPage(request);
    self.command = Commands.FIND;
    Form form = new Form(self);
    form.alertOnChange = true;
    form.id = "issueMultiEditor";
    pageState.setForm(form);
    form.start(pageContext);

    long focusedMillis = pageState.getEntry("FOCUS_MILLIS").longValueNoErrorNoCatchedExc();
    focusedMillis = focusedMillis == 0 ? System.currentTimeMillis() : focusedMillis;
    pageState.addClientEntry("FOCUS_MILLIS", focusedMillis);

    CompanyCalendar cc = new CompanyCalendar(new Date(focusedMillis), SessionState.getLocale());


    String savedFilterName = pageState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty();

%>

<div class="mainColumn">
  <h1 class="filterTitle" defaultTitle="<%=I18n.get("ISSUE_PLANNER")%>">
    <%=JSP.ex(savedFilterName)?I18n.get("ISSUE_PLANNER")+": "+I18n.get(savedFilterName):I18n.get("ISSUE_PLANNER")%>
  </h1>


<%----------------------------------------------------------------------------  START FILTER ----------------------------------------------------------------------------%>
  <div class="filterBar withButtons clearfix">
    <%
      new JspHelper("/applications/teamwork/issue/partIssueFilter.jsp").toHtml(pageContext);

      LoadSaveFilter lsfb = new LoadSaveFilter("ISSUEFILTER", pageState.getForm());
      lsfb.label = I18n.get("WANT_TO_SAVE_FILTER");
      lsfb.drawEditor = true;
      lsfb.drawButtons = false;
      lsfb.id="issueLSF";

    %>

  <div class="filterButtons">
    <div class="filterButtonsElement filterAdd">
      <span class="button" id="filterSelectorOpener" title="<%=I18n.get("ADD_FILTER")%>" onclick="bjs_showMenuDiv('filterSelectorBox', 'filterSelectorOpener');"><span class="teamworkIcon">f</span></span>
    </div>
    <div class="filterButtonsElement filterSearch"><%
      ButtonSubmit.getSearchInstance(form, pageState).toHtml(pageContext);%></div>

    <div class="filterActions">
      <div class="filterButtonsElement filterSave"><%lsfb.toHtml(pageContext);%></div>
      <div class="filterButtonsElement filterHelp"><%
        DataTable.getQBEHelpButton(pageState).toHtmlInTextOnlyModality(pageContext);%></div>
    </div>

  </div>
</div>
<script src="<%=request.getContextPath()%>/commons/js/filterEngine.js"></script>
<%----------------------------------------------------------------------------  END FILTER ----------------------------------------------------------------------------%>

<%--  -------------------------------------------------------------------- CSS --------------------------------------------------------------------%>
<style type="text/css">


  [gravity="<%=Issue.GRAVITY_BLOCK%>"] {
    background-color: <%=IssueBricks.getGravityColor(Issue.GRAVITY_BLOCK)%>;
    color: <%=HtmlColors.contrastColor(IssueBricks.getGravityColor(Issue.GRAVITY_BLOCK))%>
  }

  [gravity="<%=Issue.GRAVITY_CRITICAL%>"] {
    background-color: <%=IssueBricks.getGravityColor(Issue.GRAVITY_CRITICAL)%>;
    color: <%=HtmlColors.contrastColor(IssueBricks.getGravityColor(Issue.GRAVITY_CRITICAL))%>
  }

  [gravity="<%=Issue.GRAVITY_HIGH%>"] {
    background-color: <%=IssueBricks.getGravityColor(Issue.GRAVITY_HIGH)%>;
    color: <%=HtmlColors.contrastColor(IssueBricks.getGravityColor(Issue.GRAVITY_HIGH))%>
  }

  [gravity="<%=Issue.GRAVITY_LOW%>"] {
    background-color: <%=IssueBricks.getGravityColor(Issue.GRAVITY_LOW)%>;
    color: <%=HtmlColors.contrastColor(IssueBricks.getGravityColor(Issue.GRAVITY_LOW))%>
  }

  [gravity="<%=Issue.GRAVITY_MEDIUM%>"] {
    background-color: <%=IssueBricks.getGravityColor(Issue.GRAVITY_MEDIUM)%>;
    color: <%=HtmlColors.contrastColor(IssueBricks.getGravityColor(Issue.GRAVITY_MEDIUM))%>
  }


</style>
<%-- --------------------------------------------------------------------  ISSUE TEMPLATE ------------------------------------------------------------------------------------------------------------------- --%>
<div id="templates" style="display:none;">
  <%=JST.start("ISSUE")%>
  <div id="(#=obj.id#)" task="(#=obj.taskId#)" assignee="(#=obj.assigneeId#)" status="(#=obj.statusId#)" class="issue" millis="(#=obj.shouldCloseBy#)" estim="(#=obj.estimatedDuration#)">
    <div class="title dragHandler" gravity="(#=obj.gravityId#)"></div>
    <div class="rightCol">
      <span class="code">I#(#=obj.icodid#)#</span>
      <div class="status">
        <span class="teamworkIcon" style="color:(#=obj.statusColor#);">©</span>
        &nbsp;(#=obj.statusName#)
      </div>
      <%--<div class="gravity"><div style="background-color:(#=gravityColor#);"></div>&nbsp;(#=gravityName#)</div>--%>

      <div class="body">(#=obj.description#)</div>
      <div class="duration">(#=getMillisInHoursMinutes(obj.estimatedDuration)#)</div>
      <div class="type"><i>(#=(obj.type)#)</i></div>
      <div class="task" title="(#=obj.taskName#)">(#=obj.tcodid!=null?"T#"+obj.tcodid+"#":"" #)</div>
      <div class="taskExpanded" onclick="goToTask($(this))">(#=obj.taskName#)</div>
    </div>
  </div>
  <%=JST.end()%>


  <%=JST.start("USER_ROW")%>
  <tr assigneeId="##assigneeId##" class="resHead tableSection" assigneeOrder="##assigneeName##">
    <td colspan="9" class="assigneeLabel"><div class="childNode"><h2><img src="##assigneeAvatarUrl##" class='face small' align="absmiddle">&nbsp;##assigneeName##&nbsp;&nbsp;<span onclick="removeRow($(this))" class="button textual"><small>[<%=I18n.get("HIDE")%>]</small></span>
    </h2></div></td>
  <tr assigneeId="##assigneeId##" class="resIssues" style="height:50px;">
    <td day="-1" class="wwDayBody wwResched"><label><%=I18n.get("TO_BE_SCHEDULED")%></label>
    </td>
    <%
      cc.set(CompanyCalendar.DAY_OF_WEEK, cc.getFirstDayOfWeek());
      for (int i = 0; i < 7; i++) {
    %>
    <td class="wwDayBody wwDayColumn" millis="<%=cc.getTimeInMillis()%>" day="<%=cc.get(CompanyCalendar.DAY_OF_WEEK)%>" valign="top" width="<%=cc.isWorkingDay()?15:8%>%">
      <div class="wwDayLoadBox"></div>
      <div class="wwDayLoadUnavail"></div>
    </td>
    <%
        cc.add(CompanyCalendar.DAY_OF_WEEK, 1);
      }
    %></tr>
  <%=JST.end()%>
</div>


<%--// --------------------------------------------------------- DESK BEGIN ------------------------------------------------------------------%>

  <div class="optionsBar clearfix">
    <div class="filterElement"><%
      SmartCombo resourceCombo = ResourceBricks.getResourceCombo("RESOURCE_COMBO", TeamworkPermissions.resource_canRead, "", Resource.class, pageState);
      resourceCombo.fieldSize = 30;
      resourceCombo.addAllowed = true;
      resourceCombo.separator = "<br>";
      resourceCombo.preserveOldValue = false;
      resourceCombo.label = I18n.get("ADD_RESOURCE");
      resourceCombo.onValueSelectedScript = "addResource();";
      resourceCombo.toHtml(pageContext);
    %>
    </div>
  </div>
   <table width="100%" cellspacing="0" cellpadding="0" border="0">
      <tr>
        <td class="calHeader left">
          <span class="button textual today noprint" onclick="goToMillis(new Date().getTime())"><%=I18n.get("TODAY")%></span>

          <div style="float:right">
            <span class="button textual noprint" onclick="goToMillis(millis - 3600000 * 24 * 7)"><span class="teamworkIcon" style="font-size:18px">{</span></span>
            <span class="button textual noprint" onclick="goToMillis(millis + 3600000 * 24 * 7)"><span class="teamworkIcon" style="font-size:18px">}</span></span>
          </div>
        </td>


        <td class="calHeader">
          <h2 style="margin:0">
            <div style="position:relative;height:30px;">
              <div id="topHeaderCentral"></div>
              <div class="headerCalendarOpener" title="<%=I18n.get("DATEFIELDCALENDAR")%>" id="openCal" onclick="$(this).dateField({inputField:$('#dummy'),callback:function(date){goToMillis(date.getTime());}}); ">
                <input type="hidden" id="dummy"><span class="teamworkIcon">m</span></div>
            </div>
          </h2>
        </td>
      </tr>
    </table>

  <div class="scrollingBox" >
    <table id="desk" width="100%" cellspacing="0" cellpadding="0" class="table edged fixHead">
     <thead>
     <tr>
        <th width="10%" class="label dayHeader" millis="0" day="-1" align="center">&nbsp;</th>
        <%
          cc.set(CompanyCalendar.DAY_OF_WEEK, cc.getFirstDayOfWeek());
          for (int i = 0; i < 7; i++) {
        %>
        <th class="dayHeader wwDayColumn <%=cc.isWorkingDay()?"":"dayHHeader"%>" nowrap day="<%=cc.get(CompanyCalendar.DAY_OF_WEEK)%>" width="<%=cc.isWorkingDay()?15:8%>%" onclick="toggleEnlarge($(this));"></th>

        <% cc.add(CompanyCalendar.DAY_OF_WEEK, 1);
          }
        %></tr>
     </thead>
    </table>
  </div>
  <div class="wrapperFooter"></div>


<%----------------------------------------------------------- DESK END --------------------------------------------------------------------%>

</div>

<%---------------------------------------------- RIGHT COLUMN START ---------------------------------------------------------%>
<jsp:include page="partIssueSidebBar.jsp"/>
<%------------------------------------------------ RIGHT COLUMN END ---------------------------------------------------------%>

<%
  pageState.attributes.put("FOCUSED_PERIOD", Period.getWeekPeriodInstance(new Date(focusedMillis),logged.getLocale()));// used in the bar
%>
<jsp:include page="../parts/timeBar.jsp"/>

<%
  form.end(pageContext);
%>

<%--------------------------------------------------------------------- START SCRIPT ----------------------------------------------------------------------%>
<script type="text/javascript">
var millis =<%=focusedMillis%>;
var weekStart, weekEnd;
var millisRange = 3600000 * 24 * 365 * 2;//2 year span

var dayMillis = {}; // used for speed-up decode day in millis

$(document).ready(function () {
  $("#ISSUES_MENU").addClass('selected');

  $("body").unselectable();

  //load templates and remove container
  $("#templates").loadTemplates().remove();


  deskRefill();

  //issue update events from black popup editor
  registerEvent("issueEvent", function (e, data) {
    //console.debug("issueEvent",data);

    if (data.type == "delete") {
      var issueId = data.response.deletedId;
      var oldDomIssue = $("#" + issueId);
      var cell = oldDomIssue.closest(".wwDayBody");
      updateCellLoadEstimation(cell, -parseInt(oldDomIssue.attr("estim")));

      oldDomIssue.effect("highlight", { color: "gray" }, 500, function () {
        $(this).fadeOut(200, function () {
          $(this).remove();
        });
      });
    } else if (data.type == "save") {
      //remove issue and update cell estimation
      var jsonIssue = data.response.issue;
      var oldDomIssue = $("#" + jsonIssue.id);
      var cell = oldDomIssue.closest(".wwDayBody");
      updateCellLoadEstimation(cell, -parseInt(oldDomIssue.attr("estim")));
      oldDomIssue.remove();

      //create the new issue id in the week range
      var newDomIssue = insertIssue(jsonIssue, $("#desk"));
      if (newDomIssue) {
        newDomIssue.effect("highlight", { color: "#F9EFC5" }, 400);
        cell = newDomIssue.closest(".wwDayBody");
        updateCellLoadEstimation(cell, parseInt(newDomIssue.attr("estim")));
      }
    }
  });
});

function deskRefill() {
  //load estimations for existing resources
  var resIds = [];
  $("#desk tr[assigneeid].resIssues").each(function () {
    resIds.push($(this).attr("assigneeid"));
  });

  $(".wwDayLoadBox").empty().attr("estim", 0);
  $(".wwDayLoadUnavail").empty().hide();

  if (resIds.length > 0) {
    ajaxLoadEstimations(resIds);
  }
  ajaxLoadIssues();
}


function getDayFromMillis(millis) {
  return new Date(millis).getDay() + 1;
}


function updateIssue(cell, issue) {
  //console.debug("updateIssue:",cell,issue);
  var board = $("#desk");
  var organizeBy = board.attr("organizeBy");

  var day = cell.attr("day");
  //get day millis
  var newMillis = dayMillis[day];

  //get assignee
  var newAssig = cell.closest("[assigneeId]").attr("assigneeId");

  //check if the value is changed
  if (newMillis != issue.attr("millis") || newAssig != issue.attr("assignee")) {

    var oldAssig = issue.attr("assignee");
    var oldMillis = issue.attr("millis");

    //update the issue value on dom
    issue.attr("assignee", newAssig);
    issue.attr("millis", newMillis);

    showSavingMessage();

    var req = {"CM": "ISSCHPL", issueId: issue.prop("id"), newMillis: newMillis, newAssignee: newAssig};

    showSavingMessage();
    $.getJSON("issueAjaxControllerJson.jsp", req, function (response) {
      jsonResponseHandling(response);
      if (response.ok) {

        issue.css({position: "relative", top: 0, left: 0});
        issue.appendTo(cell);
        var issueLoad = parseInt(issue.attr("estim"));

        //update old cell estimations
        var lb = issue.data("cell");
        updateCellLoadEstimation(lb, -issueLoad);

        //update new cell estimations
        updateCellLoadEstimation(cell, issueLoad);
        issue.removeData("cell");
      } else {

        //restore old values
        issue.attr("assignee", oldAssig);
        issue.attr("millis", oldMillis);

        issue.data("cell").append(issue);
        issue.css({position: "relative", top: 0, left: 0});


      }
      hideSavingMessage();
    });
  }
}


function ajaxLoadIssues() {
  var filter = {};
  $("form:first").fillJsonWithInputValues(filter);

  filter.CM = "ISS_PLANNER";
  filter.FOCUS_MILLIS = millis;
  //console.debug("ajaxLoadIssues", filter);
  showSavingMessage();
  $.getJSON("issueAjaxControllerJson.jsp", filter, function (response) {
    jsonResponseHandling(response);
    if (response.ok) {
      $("#desk .issue").remove();
      weekStart = response.weekStart;
      weekEnd = response.weekEnd;
      //clear headers and fill
      $("#topHeaderCentral").html(response.centralHeader);
      dayMillis = {}; //clear
      $("#desk .wwDayColumn.dayHeader").empty().each(function () {
        var day = $(this).attr("day");
        var dateInfo = response.dateHeaders[day];
        $(this).html(dateInfo.label).attr({"millis": dateInfo.millis, "inUserFormat": dateInfo.inUserFormat});
        if (dateInfo.today)
          $(this).addClass("dayTHeader");
        else
          $(this).removeClass("dayTHeader");

        dayMillis[day] = dateInfo.millis;
      });
      millis = response.centralDay;

      var desk = $("#desk");

      var involvedResources = [];


      //loop for issue in current week
      for (var i = 0; response.issues && i < response.issues.length; i++) {
        insertIssue(response.issues[i], desk);
        if (involvedResources.indexOf(response.issues[i].assigneeId) < 0)
          involvedResources.push(response.issues[i].assigneeId);
      }

      //loop for issues open out of week'scope
      for (var i = 0; response.toBeRescheduled && i < response.toBeRescheduled.length; i++) {
        insertIssue(response.toBeRescheduled[i], desk);
        if (involvedResources.indexOf(response.toBeRescheduled[i].assigneeId) < 0)
          involvedResources.push(response.toBeRescheduled[i].assigneeId);
      }

      for (var assId in response.savedFilter) {
        getOrCreateRow({assigneeId: assId, assigneeName: response.savedFilter[assId].displayName, assigneeAvatarUrl: response.savedFilter[assId].avatarUrl}, true);
        if (involvedResources.indexOf(assId) < 0)
          involvedResources.push(assId);
      }

      //console.debug("involvedResources",involvedResources)
      ajaxLoadEstimations(involvedResources);

      if (typeof(callback) == "function") {
        callback(response);
      }
    }
    hideSavingMessage();
  });
}


function insertIssue(issue, desk) {
  var day;
  //place issue in the week cell if is in the range
  if (issue.shouldCloseBy && issue.shouldCloseBy >= weekStart && issue.shouldCloseBy <= weekEnd) {
    day = (new Date(issue.shouldCloseBy).getDay() + 1);

    //place issue in "rescheduled" if open and without shouldClose or open and in the past
  } else if (issue.isOpen && (!issue.shouldCloseBy || issue.shouldCloseBy <= weekStart )) {
    day = -1;
  }

  var domIssue;
  if (day) {
    var row = getOrCreateRow(issue);
    domIssue = $.JST.createFromTemplate(issue, "ISSUE");
    makeIssueDraggable(domIssue);
    row.find("[day=" + day + "]").append(domIssue);


//    domIssue.click(function(){$(this).toggleClass("expanded")});

    domIssue.mouseover(function () {
      $(this).stopTime("tmrCollapse").oneTime(500, "tmrExpand", function () {$(this).addClass("expanded")})
    }).mouseout(function () {$(this).stopTime("tmrExpand").oneTime(100, "tmrCollapse", function () {$(this).removeClass("expanded")})});


  }
  return domIssue;
}


function getOrCreateRow(assignee, highLight) {
  //console.debug("getOrCreateRow",assignee,highLight)

  var desk = $("#desk");
  var row = desk.find("tr[assigneeId=" + assignee.assigneeId + "].resIssues");
  if (row.size() == 0) {
    row = $.JST.createFromTemplate(assignee, "USER_ROW");
    makeRowDroppable(row);
    manageAddIssue(row);

    //if in enlarged mode hide unused columns
    var thEnlarged = $(".dayHeader.enlarged");
    if (thEnlarged.size() > 0) {
      var enlargedDay = thEnlarged.attr("day");
      row.find(".wwDayColumn[day!='" + enlargedDay + "']").hide();
      row.find(".wwDayColumn[day=" + enlargedDay + "]").addClass("enlarged");

    }
    var appended = false;

    $("#desk tr[assigneeOrder]").each(function () {
      if ($(this).attr("assigneeOrder").toLowerCase() >= assignee.assigneeName.toLowerCase() || assignee.assigneeId == "-1") {
        $(this).before(row);
        appended = true;
        return false;
      }
    });
    if (!appended)
      desk.append(row);

    //get load from user if the row is of real user
    //if (assignee.assigneeId){
    //  ajaxLoadEstimations([assignee.assigneeId]);
    //}
  }
  if (highLight) {
    row.effect("highlight", { color: "#F9EFC5" }, 1500);
  }

  return row;
}

function goToTask(el) {
  var taskId = el.closest("[task]").attr("task");
  if (taskId)
    window.location = contextPath + "/applications/teamwork/task/taskOverview.jsp?<%=Commands.COMMAND%>=GUESS&<%=Fields.OBJECT_ID%>=" + taskId;

}

function removeRow(el) {
  var row = el.closest("tr[assigneeId]");
  var assigneeId = el.closest("tr[assigneeid]").attr("assigneeid");
  $("tr[assigneeId=" + assigneeId + "]").remove();
  refreshSavedFilter(assigneeId, false);

}

function ajaxLoadEstimations(resourcesIds, callback) {
  var request = {CM: "LDESTM", resId: resourcesIds.toString(), millis: millis};
  $.getJSON("issueAjaxControllerJson.jsp", request, function (response) {
    jsonResponseHandling(response);
    if (response.ok) {
      //console.debug("ajaxLoadEstimations",response)

      // set estimations
      for (var resId in response.estimations) {
        var ests = response.estimations[resId];
        var row = $("#desk tr[assigneeid=" + resId + "].resIssues");
        if (row.size() > 0) {
          row.find(".wwDayLoadBox").empty().attr("estim", 0);
          row.find(".wwDayLoadUnavail").empty().hide();

          for (var i = 0; i < ests.length; i++) {
            var est = ests[i];
            var cell = row.find("[day=" + getDayFromMillis(est.millis) + "].wwDayBody");
            updateCellLoadEstimation(cell, est.estim);
          }


        }
      }

      // set unavailabilities
      for (var i = 0; i < response.unavailabilities.length; i++) {
        var unavail = response.unavailabilities[i];
        var row = $("#desk tr[assigneeid=" + unavail.resId + "].resIssues");
        if (row.size() > 0) {
          row.find("[day=" + getDayFromMillis(unavail.millis) + "] .wwDayLoadUnavail").html(getMillisInHoursMinutes(unavail.unavail)).show();
        }
      }
    }
  });
}


function refreshSavedFilter(resourcesId, add, callback) {
  //console.debug("refreshSavedFilter");
  var req = {CM: "REFRESHFILTER", resId: resourcesId, add: add};
  $.getJSON("issueAjaxControllerJson.jsp", req, function (response) {
    if (callback) {
      callback(response);
    }
  });
}

function makeRowDroppable(row) {
  row.find(".wwDayBody").droppable({
    hoverClass: "dropOver",
    //accept:"["+organizeBy+"!="+value.id+"]",
    drop:       function (event, ui) {
      var dragged = ui.draggable;
      var cell = $(this);
      updateIssue(cell, dragged);
    }
  });
}


function manageAddIssue(row){
  row.find(".wwDayColumn").click(function(ev){
    if ($(ev.target).is(".wwDayColumn")) {
      var el = $(this);
      var task = $("#FLT_ISSUE_TASK").val();
      var millis = parseInt(el.closest("[millis]").attr("millis"));
      var resId = el.closest("[assigneeid]").attr("assigneeid");
      addIssue("ISSUE_TASK=" + task + "&ASSIGNEE=" + resId + (millis ? "&ISSUE_DATE_CLOSE_BY=" + (new Date(millis).format()) : ""));
    }
  })
}


function updateCellLoadEstimation(cell, load) {
  var cellLoad = cell.find(".wwDayLoadBox");
  var estim = parseInt(cellLoad.attr("estim"));
  estim = estim ? estim : 0;
  estim = estim + load;
  cellLoad.html(estim > 0 ? getMillisInHoursMinutes(estim) : "").attr("estim", estim);
}


function makeIssueDraggable(domIssue) {
  domIssue.draggable({
    revert: 'invalid',
    start:  function (event, ui) {
      var draIss = $(this);
      draIss.addClass("issueDrag");
      draIss.data("cell", draIss.closest(".wwDayBody "));
    },
    stop:   function (event, ui) {
      $(this).removeClass("issueDrag");
    }
  }).activateTeamworkLinks(true).emoticonize();
}

function goToMillis(newMillis) {
  $("body").stopTime("issuePlannerRefresh");
  millis = newMillis;
  var st = new Date(millis - 3600000 * 24 * 3);
  var en = new Date(millis + 3600000 * 24 * 3);
  if ($("td.enlarged").size() > 0)
    toggleEnlarge($("td.enlarged"));
  $("#topHeaderCentral").html(st.format("dd" + (st.getMonth() != en.getMonth() ? " MMMM" : "")) + " - " + en.format("dd MMMM yyyy") + "<sup>(" + en.format("ww") + ")</sup>");
  $("body").oneTime(500, "issuePlannerRefresh", function () {
    deskRefill();
  });
}


function addResource() {
  var resId = $("#RESOURCE_COMBO").val();
  if (resId) {
    //refresh savedfilter
    refreshSavedFilter(resId, true, function (response) {
      getOrCreateRow({assigneeId: response.resource.id, assigneeName: response.resource.displayName, assigneeAvatarUrl: response.resource.avatarUrl}, true);
      ajaxLoadEstimations([response.resource.id]);
    });
  }
}


function toggleEnlarge(el) {
  if (el.is(".enlarged")) {

    el.removeClass("enlarged");
    $("#desk .wwDayBody[day=" + el.attr("day") + "]").removeClass("enlarged");
    $("#desk .wwDayColumn[day!='" + el.attr("day") + "']").show();

  } else {

    $("#desk .wwDayBody[day=" + el.attr("day") + "]").addClass("enlarged");
    $("#desk .wwDayColumn[day!='" + el.attr("day") + "']").hide();
    el.addClass("enlarged");
  }
}

function addIssue(paramString) {
  //console.debug("addIssue",paramString)
  var editUrl=contextPath+"/applications/teamwork/issue/issueEditor.jsp?CM=AD"+(paramString?"&"+paramString:"");
  openBlackPopup(editUrl, 1024, $(window).height() - 50);
}

</script>
<%
  }
%>
