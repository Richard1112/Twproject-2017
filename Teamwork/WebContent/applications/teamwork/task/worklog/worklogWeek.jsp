<%@ page import="com.twproject.agenda.Event, com.twproject.agenda.PeriodEvent, com.twproject.operator.TeamworkOperator, com.twproject.resource.Person, com.twproject.resource.Resource,
                 com.twproject.resource.ResourceBricks, com.twproject.security.TeamworkPermissions, com.twproject.task.Assignment, com.twproject.task.TaskBricks, com.twproject.utilities.TeamworkComparators,
                 com.twproject.waf.TeamworkHBFScreen, com.twproject.waf.html.EventListWriter, net.sf.json.JSONObject, org.jblooming.agenda.CompanyCalendar, org.jblooming.agenda.Period,
                 org.jblooming.agenda.Scale, org.jblooming.persistence.PersistenceHome, org.jblooming.utilities.DateUtilities, org.jblooming.waf.ScreenArea, org.jblooming.waf.SessionState,
                 org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.core.JST, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.SmartCombo, org.jblooming.waf.html.layout.HtmlColors, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.ClientEntry, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.*, org.jblooming.security.License, org.jblooming.utilities.JSP"%>
<%

  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    body.areaHtmlClass="lreq10 lreqPage";
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);

    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {

    Person myself = Person.getLoggedPerson(pageState);
    Resource focusedResource = myself;
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    String resId = pageState.getEntry("RES_ID").intValueNoErrorCodeNoExc()+"";

    Resource res = (Resource) PersistenceHome.findByPrimaryKey(Resource.class, resId);
    if (res!=null) {
      if (res.hasPermissionFor(logged,TeamworkPermissions.resource_manage) || res.hasPermissionFor(logged,TeamworkPermissions.worklog_manage ))
        focusedResource = res;
    }

    // this will be used in additional buttons
    pageState.setMainObject(focusedResource);

    long focusedMillis= pageState.getEntry("FOCUS_MILLIS").longValueNoErrorNoCatchedExc();
    focusedMillis= focusedMillis==0?System.currentTimeMillis():focusedMillis;
    pageState.addClientEntry("FOCUS_MILLIS",focusedMillis);


    // get the current scale factor
  /*String ssct = pageState.getEntryOrDefault("SCALE_TYPE", Scale.ScaleType.SCALE_2WEEK.toString()).stringValue();

  // define the scale and increment of ticks
  Scale scale = Scale.getScaleAndSynch(Scale.ScaleType.valueOf(ssct), focusedMillis, true, SessionState.getLocale());

    */
    Scale scale = Scale.getScaleAndSynch(Scale.ScaleType.SCALE_1WEEK, focusedMillis, true, SessionState.getLocale());
    //set calender to first day of month
    CompanyCalendar cc = new CompanyCalendar();
    cc.setTime(scale.startPointDate);

    //to show in smart or combo
    pageState.addClientEntry("RES_ID", focusedResource.getId());
    pageState.removeEntry("RES_ID_txt");


%><script>$("#TIMESHEET_MENU").addClass('selected');</script><%

  // compute the startMillis and endMillis using focusedMillis and scaleType
  final long minMillisInBar = scale.startPointTime;
  final long maxMillisInBar = scale.endPointTime;

  PageSeed pageSeed = pageState.thisPage(request);
  pageSeed.mainObjectId = pageState.mainObjectId;

  // add the focused time client entries;
  pageSeed.addClientEntry("FOCUS_MILLIS", minMillisInBar + "");

  cc.setTimeInMillis(minMillisInBar);


  Form form = new Form(pageSeed);
  form.alertOnChange = true;
  pageState.setForm(form);
  form.start(pageContext);

  ClientEntry snaa = pageState.getEntryOrDefault("SHOW_NOTACTIVE_ASSIGS");
  ClientEntry showagenda = pageState.getEntryOrDefault("WORKLOG_SHOW_AGENDA",Fields.TRUE);

  boolean showActiveOnly = !snaa.checkFieldValue();


  // get active assignment in this week
  cc.setTimeInMillis(minMillisInBar);

  List<Assignment> assignmentList = focusedResource.getActiveAssignments( scale.getPeriod(), showActiveOnly, true, false);

  // get all assignment with wl recorder for the week
  List<Assignment> aLwl = focusedResource.getAssignmentsWithWorklog(scale.getPeriod());

  // add missing assignements
  for (Assignment ass: aLwl){
    if (!assignmentList.contains(ass))
      assignmentList.add(ass);
  }

%>
<%---------------------------------------------- MAIN COLUMN START ---------------------------------------------------------%>
<style>
 .columnTaskName {
    width: 40%;
  }
</style>

<div class="mainColumn"><%

  // ---------------------------------------------------------------------------------  START HEADER ---------------------------------------------------------------------------------

%><h1><%=I18n.get("TIMESHEET_MENU")%></h1>
<div class="optionsBar clearfix">
  <%
    ButtonSubmit oms = new ButtonSubmit(pageState.getForm());
    oms.variationsFromForm.setCommand(pageState.getCommand());

    SmartCombo assignee = ResourceBricks.getInspectableResourcesCombo("RES_ID", pageState);
    assignee.label = I18n.get("WORKLOGS_OF_RESOURCE");
    assignee.separator = "";
    assignee.fieldSize = 30;
    assignee.preserveOldValue = false;
    assignee.label = I18n.get("FOCUSED_RESOURCE");
    assignee.onValueSelectedScript=oms.generateJs().toString();
    assignee.separator = "<br>";

  %><div class="filterElement"><%assignee.toHtml(pageContext);%></div>
  <%
    List<String>assids=new ArrayList();
    for (Assignment ass: assignmentList)
      assids.add(ass.getId()+"");

    SmartCombo allMyAssignmentsCombo = TaskBricks.getAllAssignmentsOfCombo("ASSIG_PICKED", focusedResource, assids);
    allMyAssignmentsCombo.fieldSize = 25;
    allMyAssignmentsCombo.iframe_width =0;
    allMyAssignmentsCombo.label=I18n.get("SHOW_YET_ANOTHER_ASSIGNMENT");
    allMyAssignmentsCombo.onValueSelectedScript="addLine();";
    allMyAssignmentsCombo.preserveOldValue=false;
    allMyAssignmentsCombo.separator = "<br>";

  %><div class="filterElement"><%allMyAssignmentsCombo.toHtml(pageContext);%></div>
  <div class="filterElement"><small><%

    CheckField snaaCF = new CheckField("SHOW_NOTACTIVE_ASSIGS", "&nbsp;", false);
    snaaCF.preserveOldValue = false;
    snaaCF.additionalOnclickScript=oms.generateJs().toString();
    snaaCF.label=I18n.get("ASS_SHOW_DISABLED");
    snaaCF.toHtml(pageContext);

  %></small></div><div class="filterElement"><small>&nbsp;&nbsp;<%

  CheckField sag = new CheckField("WORKLOG_SHOW_AGENDA", "&nbsp;", false);
  sag.preserveOldValue = false;
  sag.additionalOnclickScript=oms.generateJs().toString();
  sag.toHtmlI18n(pageContext);
%></small></div></div>

<jsp:include page="../part/partCalendarHeader.jsp"/>

<%
  // ---------------------------------------------------------------------------------  END HEADER ---------------------------------------------------------------------------------




  // ---------------------------------------------------------------------------------  START MAIN TABLE ---------------------------------------------------------------------------------
%>

<%
  int daysInPeriod = (int)Math.round((scale.endPointTime-scale.startPointTime)*1.0/ CompanyCalendar.MILLIS_IN_DAY);
  int w=826/daysInPeriod;
  //String dateCompactFormat= StringUtilities.replaceAllNoRegex(DateUtilities.getFormat(DateUtilities.DATE_FULL), new String[]{",","EEEE","MMMM","d","yyyy"},new String[]{" ","EEE","MMM","d",""}); // "EEEE, MMMM d, yyyy" ->  "EEE d MMM"

  TreeMap<Integer,Long> workablePlan = focusedResource.getWorkablePlan(scale.getPeriod());
%>

<%------------------------------------- CALENDAR BODY ----------------------------------------------------%>
<table class="table edged fixHead fixFoot worklogWeek" cellpadding="5" cellspacing="1" border="0" id="multi"  style="width:100%;">

  <thead>
  <tr class="dayHead">

    <th class="dayHeader" style="text-align: left; width: 20%; min-width: 200px;" colspan="3" id="wwPutFilterHere"><%=I18n.get("TASK_TASK")%></th>
    <th class="dayHeader" width="1%"></th>
    <%
      cc.setTimeInMillis(minMillisInBar);
      CompanyCalendar cOggi = new CompanyCalendar(SessionState.getLocale());
      cOggi.setTime(new Date());
      cOggi.setMillisFromMidnight(cc.getMillisFromMidnight());

      while (cc.getTimeInMillis() <scale.endPointTime) {
    %><th align="left" day="<%=JSP.w(cc.getTime())%>" class="dayHeader <%=cc.isWorkingDay()?"":"dayHHeader"%> <%=cc.isToday()?"dayTHeader":""%>"><%=DateUtilities.dateToString(cc.getTime(),"EEE d MMM") %></th><%
      cc.add(CompanyCalendar.DATE, 1);
    }
  %>
    <th class="dayHeader total"><%=I18n.get("TOTAL")%></th>
  </tr>
  </thead>
  <tbody>
  <%
    //

    Comparator ctc=null;
    if (I18n.isActive("CUSTOM_FEATURE_ORDER_TASK_BY_CODE"))
      ctc = new TeamworkComparators.AssignmentComparatorByTaskCode();
    else
      ctc = new TeamworkComparators.AssignmentComparatorByTask();
    //sort them
    Collections.sort(assignmentList, ctc);

    List<String> printedIds = new ArrayList();

    if (assignmentList.size()>0) {
      for (Assignment ass : assignmentList) {
        printedIds.add(ass.getId() + "");
        // ---------------------------------------------------------------------------------  ROWS ---------------------------------------------------------------------------------

        JspHelper rowDrawer =new JspHelper("partWeekRow.jsp");
        rowDrawer.parameters.put("ass",ass);
        rowDrawer.parameters.put("scale",scale);
        rowDrawer.toHtml(pageContext);
      }
    } else {
  %><tr><td colspan="12" align="left"><h2 class="hint noassign" style="margin:10px 0"><%=I18n.get("NO_ASSIGNMENTS_HERE_FORYOU")%></h2></td></tr><%

    }
    // ---------------------------------------------------------------------------------  AGENDA ---------------------------------------------------------------------------------
    if(showagenda.checkFieldValue()){
  %><tr>
    <td class="agendaCell" colspan="4" style="font-size:16px;"><%

      PageSeed age = pageState.pageFromRoot("agenda/agendaWeekDay.jsp");
      if (focusedMillis>0)
        age.addClientEntry("FOCUS_MILLIS", focusedMillis);
      ButtonLink blAg = new ButtonLink(I18n.get("AGENDA"), age);
      blAg.iconChar="C";
      blAg.toHtmlInTextOnlyModality(pageContext);

    %></td><%

    ArrayList<Resource> ress=new ArrayList();
    ress.add(focusedResource);

    List<Event> eventsInTheWeek = Event.getFilteredEventsInPeriodWithCollisionFor(ress,  scale.getPeriod(), 0, false, false, false, true, false, false);

    HashMap<Resource, String> colorInfo = new HashMap<Resource, String>();
    Set<Resource> involvedResources = new HashSet();
    involvedResources.add(focusedResource);
    // get all resources from events
    for (Event event : eventsInTheWeek) {
      for (Resource resource : event.getTargets()) {
        involvedResources.add(resource);
      }
    }
    int depth=0;
    for (Resource resource : involvedResources) {
      String color = HtmlColors.distributeColor(depth, involvedResources.size());
      colorInfo.put(resource, color);
      depth++;
    }


    cc.setTimeInMillis(minMillisInBar);

    while (cc.getTimeInMillis() <scale.endPointTime) { //---------------------------------------- loop on days
      Period oneFullDayPeriod = Period.getDayPeriodInstance(cc.getTime());

      ArrayList<PeriodEvent> oneFullDayPeriodEvents = new ArrayList();
      for (Event event : eventsInTheWeek) {
        //get full day
        List<Period> periods = event.getSchedule().getPeriods(oneFullDayPeriod, true,event.getExceptions());
        for (Period period : periods) {
          period.setIdAsNew();
          oneFullDayPeriodEvents.add(new PeriodEvent(period, event));
        }
      }
      Collections.sort(oneFullDayPeriodEvents);

      EventListWriter elw = new EventListWriter(oneFullDayPeriodEvents, colorInfo);

  %><td class="agendaCell day <%=cc.isWorkingDay()?"":"dayHFooter"%>"><%elw.toHtml(pageContext);%><%=oneFullDayPeriodEvents.size()<=0?"<i><small>"+I18n.get("AGENO_EVENT")+"</small></i>":""%>&nbsp;</td><%
      cc.add(CompanyCalendar.DATE, 1);
    }

  %>
    <td class="agendaCell day">&nbsp;</td>
  </tr><%
    }

    // ---------------------------------------------------------------------------------  TOTALS ---------------------------------------------------------------------------------
  %>
  </tbody>
  <tfoot>
  <tr>
    <td colspan="4">&nbsp;</td>
    <%
      cc.setTimeInMillis(scale.startPointTime);
      cOggi = new CompanyCalendar();
      cOggi.setTime(new Date());
      cOggi.setMillisFromMidnight(cc.getMillisFromMidnight());


      while (cc.getTimeInMillis() <scale.endPointTime) {
        long workable=workablePlan.get(DateUtilities.dateToInt(cc.getTime()));
    %><td class="textSmall day <%=workable==-1?"dayH":workable==-2?"isUnavailability":""%>"><%=DateUtilities.dateToString(cc.getTime(), "EEE")%></td><%
      cc.add(CompanyCalendar.DATE, 1);
    }
  %>
    <td class="day">&nbsp;</td>
  </tr>


  <tr height="30" valign="top" class="totals">
    <td colspan="4"><%=I18n.get("WORKLOG_TOTAL_DAILY")%>
      <%
        String worklogEstimatedPerDayStr="";
        if (focusedResource!=null){
          PageSeed opt = pageState.pageFromRoot("resource/resourceEditor.jsp");
          opt.setCommand("EDIT");
          opt.mainObjectId = focusedResource.getId();

          worklogEstimatedPerDayStr = DateUtilities.getMillisInHoursMinutes(focusedResource.getWorkDailyCapacity());
        }
        long worklogEstimatedPerDay = DateUtilities.millisFromHourMinuteSmart(worklogEstimatedPerDayStr);
      %></td><%

    cc.setTimeInMillis(scale.startPointTime);

    while (cc.getTimeInMillis() <scale.endPointTime) {
      long workable=workablePlan.get(DateUtilities.dateToInt(cc.getTime()));

  %><td class="totByCol day <%=workable==-1?"dayH":workable==-2?"isUnavailability":""%>" day="<%=JSP.w(cc.getTime())%>"><span class="totDetail">&nbsp;</span></td><%
      cc.add(CompanyCalendar.DATE, 1);
    }

  %><td id="graTot">&nbsp;</td></tr>
  </tfoot>
</table>

</div>
<%---------------------------------------------- MAIN COLUMN END ---------------------------------------------------------%>

<%

  form.end(pageContext);

  pageState.attributes.put("FOCUSED_PERIOD", scale.getPeriod());// used in the bar
%> <jsp:include page="../../parts/timeBar.jsp"/>


<div id="wltpl" style="display: none">
  <%=JST.start("WLROWDETAIL")%>
  <tr style="cursor:pointer;" wlId="(#=obj.id#)">
    <td class="status"><span class="teamworkIcon" style="font-size:110%; color:(#=obj.stsColor?obj.stsColor:"#FFCC33"#)" title="(#=obj.stsName?obj.stsName:"<%=I18n.get("WORKLOG_STATUS_NONE")%>"#)">&copy;</span></td>
    <td class="duration">(#=getMillisInHoursMinutes(obj.duration)#)</td>
    <td><div style="max-width: 200px; overflow: hidden;text-overflow: ellipsis;">(#=obj.action?obj.action:''#)</div></td>
    <td align="center">
      <span class="button noprint textual icon delete" (#=obj.canWrite?"":"disabled"#) (#if (obj.canWrite){#) onclick="removeWorklog($(this),'(#=obj.id#)',event); return false;" (#}#) ><span class="teamworkIcon ">d</span></span>
    </td>
  </tr>

  <%=JST.end()%>
</div>

<script type="text/javascript">
var worklogEstimatedPerDay = <%=worklogEstimatedPerDay%>;
var workablePlan=<%=JSONObject.fromObject(workablePlan).toString()%>;

$(function () {
  $("#wltpl").loadTemplates().remove();
  if (<%=License.assertLevel(10)%>)
    bindCellEvents($("#multi"));
  refreshAllTotals();

  //inject the table search
  createTableFilterElement($("#wwPutFilterHere"),'table.worklogWeek tr[assId]','.columnTaskName,.columnTaskCode');

  //gestione dell'apertura del dettaglio
  if("EXPANDASS"=="<%=pageState.command%>"){
    $("tr[assid=<%=pageState.getEntry("ASS_ID").stringValueNullIfEmpty()%>] .wlDetail").click()
  }

});


function goToMillis(time) {
  $("[name=FOCUS_MILLIS]").val(time);
  $("#<%=form.id%>").submit();
}


function bindCellEvents(containerElement) {
  containerElement.find(".wlDayCell").click(function(event){
    var cell = $(this);
    var row=cell.closest("tr[assId]");
    openWorklogEditorCloseToElement(cell,{assId:row.attr("assId"),title:row.find(".columnTaskName").attr("title"),date:cell.attr("day")} ,function(wlBox){
      wlBox.find("#WORKLOG_DURATION").focus().keyup(cellKeyboardMovement)
    });
  }).bind("worklogSaved",function(ev,response){

    //var day = response.worklog.insertedMillis;
    //var assig = response.worklog.assId;

    var theCell = $(this);

    theCell.find(".wlmill").html(response.totMillis?getMillisInHoursMinutes(response.totMillis):"");
    refreshAllTotals();
    theCell.oneTime(200,"cellClick",function(){$(this).click()}); <%-- si riapre il box --%>
  });

  containerElement.find("span.wlmill").click(function (event) {
    event.stopPropagation();
    showDetail($(this));
  });
}




<%---------------------------------------------   KEYBOARD  ---------------------------------------------%>
function cellKeyboardMovement(event) {
  if ($(this).val()) <%-- se hai inserito un valore non fa niente --%>
    return;

  var cells=$(".wlDayCell");
  var quant=cells.size();

  var pos=cells.index($(".wlDayCell.mbBalloonOpener"));
  var oldPos=pos;

  var rows=parseInt(quant/7);
  var row=parseInt(pos/7);
  var col=pos%7;


  switch (event.keyCode) {

    case 37: //left arrow
      col--;
      break;

    case 38: //up arrow
      row--;
      break;

    case 39: //right arrow
      col++;
      break;

    case 40: //down arrow
      row++;
      break;

    case 36: //home
      row=0;
      col=0;
      ret = false;
      break;
    case 35: //end
      row=rows-1;
      col=6;
      break;

  }

  row=row>=rows?0 : (row<0?rows-1:row);
  col=col>=7?0:(col<0?6:col);
  pos=row*7+col;

  if (pos!=oldPos)
    cells.eq(pos).click();

}


function showDetail(span) {
  $(".olexplain").fadeOut(100,function(){$(this).remove()});

  var td = span.closest("td");
  var tr = td.closest("[assID]");
  var tf = td.find(".wlTime:first");
  var assId = tr.attr("assId");
  var date = td.attr("day");

  var request = {CM:"LOADDETAIL", assId:assId, date:date};

  if (hideWorklogEditorIfYouCan()) {
    //$(".wlDayCell.sel").removeClass("sel");
    //td.addClass("sel");
//   the opener id is stored as tfId param on olexplain div
    $.getJSON("worklogAjaxController.jsp", request, function (response) {
      jsonResponseHandling(response);
      if (response.ok && response.worklogs) {


        //se ce un solo wl si va subito il edit
        if (response.worklogs.length == 1) {
          var wl = response.worklogs[0];
          openWorklogEditorCloseToElement(td,{wlId:wl.id,title:td.closest("tr[assId]").find(".columnTaskName").attr("title")});

        } else {
          var explain = $("<div>").addClass("olexplain").css({marginTop: 50, opacity: 0});
          explain.append('<div class="mbBalloon n"><div style="top: -12px; left: 45%;" class="arrow n"></div>' +
            '<div style="top: -12px; left: 45%;border-color:#fff" class="arrow border n"></div>' +
            '<table class="table wlrows"></table></div>');


          var ndo = explain.find("table.wlrows");

          for (var i = 0; i < response.worklogs.length; i++) {
            var wl = response.worklogs[i];
            var row = $.JST.createFromTemplate(wl, "WLROWDETAIL");

            if (wl.canWrite)
              row.click(function (event) {
                editWlLine($(this), event);
              });
            ndo.append(row);
          }

          td.append(explain);

          explain.show();
          explain.animate({marginTop: 7, opacity: 1}, 300, $.bez([0, .96, 0, 1.02]));

          $(document).off("click.olexplain").on("click.olexplain", function (e) {
            if ($(e.target).parents().is(".olexplain"))
              return;
            hideDetail();
          });
        }
      }
      //hideSavingMessage()
    });
  }
  return false;
}

function hideDetail() {
  $(".olexplain").fadeOut(200,function(){$(this).remove();});
}


function editWlLine(el,event) {
  event.stopPropagation();
  var theTd=el.closest("td[day]");
  var wlId= el.attr("wlId");
  openWorklogEditorCloseToElement(theTd,{wlId:wlId,title:theTd.closest("tr[assId]").find(".columnTaskName").attr("title")});

  hideDetail();
}



function removeWorklog(el,wlId,event,alreadyConfirmed) {
  if(event)
    event.stopPropagation();
  function del() {
    hideDetail();
    showSavingMessage();
    //var wlId = el.closest("[wlId]").attr("wlId");
    var theTd = el.closest("td[day]");
    var req = {"CM": "REMOVEWL", wlId: wlId};

    $.getJSON("worklogAjaxController.jsp", req, function (response) {
      jsonResponseHandling(response);
      if (response.ok) {
        //update totals
        updateCellTot(theTd, response.numWl, response.totMillis);
      }
      hideSavingMessage();
    });
  };

  if (alreadyConfirmed){
    del()
  } else {
    el.confirm(del,"<%=I18n.get("FLD_CONFIRM_DELETE")%>");
  }


}


function addLine() {
  var assId = $("#ASSIG_PICKED").val();
  if (assId) {
    if ($("#multi tr[assId=" + assId + "]").size() > 0) {
      $("#multi tr[assId=" + assId + "]").effect("highlight", { color: "#F9EFC5" }, 1000);
    } else {
      showSavingMessage();

      $.get("worklogAjaxController.jsp", "<%=Commands.COMMAND%>=ADDROW&" + "assId=" + assId + "&minMillisInBar=<%=minMillisInBar%>&scaleType=<%=scale.scaleName%>",function(toBeInserted){
        // append a new line
        if ($("#multi tr[assId]:last").size() > 0)
          $("#multi tr[assId]:last").after(toBeInserted);
        else
          $("#multi tr:first").after(toBeInserted);

        //bind events on new line
        bindCellEvents($("#multi tr[assId]:last"));
        $("#multi tr[assId]:last").effect("highlight", { color: "#F9EFC5" }, 1000);
        hideSavingMessage();
      });
    }
  }
}


function updateCellTot(theTd, numWl, totMillis) {
  var tr = theTd.closest("[assID]");
  theTd.find(".wlmill").html(numWl > 0 ? getMillisInHoursMinutes(totMillis) : "").prop("title", numWl > 0 ? numWl + " records." : "");
  refreshRowTotal(tr);
  refreshColumnTotal(theTd.attr("day"));
}


function refreshRowTotal(theTR) {
  //update row total
  var totWl = 0;
  theTR.find("span.wlmill").each(function(idx, domElement) {
    var mill = millisFromHourMinute($(domElement).text());
    totWl = totWl + (mill ? mill : 0);
  });
  theTR.find(".totByRow").html(getMillisInHoursMinutes(totWl));
}

function refreshGrandTotal() {
  //update row total
  var totWl = 0;
  $("#multi").find(".totByRow").each(function(idx, domElement) {
    var mill = millisFromHourMinute($(domElement).text());
    totWl = totWl + (mill ? mill : 0);
  });
  $("#graTot").html(getMillisInHoursMinutes(totWl));
}


function refreshAllTotals() {
  $("#multi tr[assId]").each(function(idx, tr) {
    refreshRowTotal($(tr));
  });
  $("tr.dayHead th.dayHeader[day]").each(function(idx, th) {
    refreshColumnTotal($(th).attr("day"));
  });
  refreshGrandTotal();
}

function refreshColumnTotal(day) {
  //console.debug("refreshColumnTotal",day);
  //update col total
  var totByCol = 0;
  $("#multi [day='" + day + "'] .wlmill").each(function(index, domElement) {
    //console.debug($(domElement).text());
    var mill = millisFromHourMinute($(domElement).text());
    totByCol = totByCol + (mill ? mill : 0);
  });

  var worklogEstimatedPerDay=workablePlan[Date.parseString(day).toInt()];
  var totTD = $("#multi").find(".totByCol[day='" + day + "']");
  var diff = "";
  if (totByCol - worklogEstimatedPerDay != 0)
    diff = "(" + (totByCol - worklogEstimatedPerDay > 0 ? "+" : "") + getMillisInHoursMinutes(totByCol - worklogEstimatedPerDay) + ")";
  totTD.find(".totDetail").html(getMillisInHoursMinutes(totByCol) + "&nbsp;" + diff);
}

</script>
<%
  }
%>


