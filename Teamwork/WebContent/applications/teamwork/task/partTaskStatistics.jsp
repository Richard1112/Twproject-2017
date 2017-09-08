<%@ page import="com.twproject.resource.Person, com.twproject.task.Task, org.jblooming.oql.OqlQuery, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, com.twproject.task.TaskStatus, com.twproject.task.TaskBricks, net.sf.json.JSONObject, org.jblooming.waf.html.core.JST, org.jblooming.security.License, com.twproject.security.TeamworkPermissions, org.jblooming.operator.Operator" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  Task task = (Task) pageState.getMainObject();

%>

<div id="taskStatistics"  style="opacity: 1; display: none; max-width: 780px">
<h2><%=I18n.get("TASK_STATISTICS")%><div style="display: inline-block;" id="statTaskDates"></div></h2>

<%

  //num of operators
  String hql = "select count(p.id) from " + Person.class.getName() + " as p where p.myself is not null and p.myself.enabled=:truth";
  OqlQuery oql = new OqlQuery(hql);
  oql.getQuery().setBoolean("truth", Boolean.TRUE);
  double totalLoggable = (Long) oql.uniqueResult();

  JSONObject jTask = task.getCurrentData().jsonify();
  jTask.element("lastStatusChangeDate",task.getLastStatusChangeDate().getTime());
  Operator logged = pageState.getLoggedOperator();%>

<div class="taskStats clearfix" id="statBoxes"></div>
<div id="slider" style="display: none" class="lreq10 lreqHide"></div>

<div id="tmpts" style="display:none;">
  <%=JST.start("HISTDATES")%>
  <span style="font-size: 70%;">
    <i>&nbsp;@&nbsp;(#=atDate#)</i>&nbsp;&nbsp;
    <i><%=I18n.get("START")%>:</i>&nbsp;<b>(#=start#)</b>
    <i><%=I18n.get("END")%>:</i>&nbsp;<b>(#=end#)</b>
    <i><%=I18n.get("DURATION")%>:</i>&nbsp;<b>(#=duration#)</b>
  <%=JST.end()%>
</div>

<link href="<%=request.getContextPath()%>/commons/js/jquery/valueSlider/mb.slider.css?_=<%=ApplicationState.getBuild()%>" media="all" rel="stylesheet" type="text/css">
<script src="<%=request.getContextPath()%>/commons/js/circles.min.js?_=<%=ApplicationState.getBuild()%>"></script>
<script src="<%=request.getContextPath()%>/commons/js/jquery/valueSlider/jquery.mb.slider.js?_=<%=ApplicationState.getBuild()%>"></script>

<script>

var _TaskStatusDecoder=<%=TaskBricks.getTaskStatusDecoder()%>;
var lastDataHist=<%=jTask.toString()%>;

(function(){
  //console.debug(lastDataHist)
  $("#tmpts").loadTemplates().remove();
  drawStatus(lastDataHist);
  drawStatistics(lastDataHist);
})();

$(function(){


  $(".statsBlock").css({width: "25%"});
  $(".statsBlock").eq(0).css({width: "33%"}).addClass("mainBlock");
  $(".statsBlock").eq(1).css({width: "33%"}).addClass("mainBlock");
  $(".statsBlock").eq(2).css({width: "33%"}).addClass("mainBlock");

  $(".statsBlock").each(function(i){
    var circle = $(".circle",this).data("circle");
    var sb=$(this);
    sb.updateCircle(circle);
    if (i==2 || i==6 || i==10) {
      sb.after("<div class='clearfix'></div>"); // per andare a capo
    }
  });

  $("#taskStatistics").show().fadeTo(500,1);
  $(window).resize();

  //si carica la history server side
  if (<%=!License.assertLevel(10)%>) return;
  $.getJSON(contextPath+"/applications/teamwork/task/taskAjaxController.jsp", {CM: "GETHIST", OBJID:<%=task.getId()%>}, function (response) {
    jsonResponseHandling(response);
    //console.debug(response);
    if (response.ok == true) {
      var dh = response.dataHistory;
      if (dh && dh.length > 0) {
        dh.push(lastDataHist);
        // allora fa vedere la barra etc....
        var minVal = dh[0].createdOn;
        var maxVal = dh[dh.length - 1].createdOn;

        $("#slider").show().mbSlider({
          rangeColor : '#2f97c6',
          minVal     : minVal,
          maxVal     : maxVal,
          startAt    : maxVal,
          showVal    : true,
          formatValue: function (val) {
            return new Date(val).format();
          },
          onSlideLoad: function (obj) {
            var val = $(obj).mbgetVal();
          },
          onStart    : function (obj) {},
          onStop     : function (obj) {},
          onSlide    : function (obj) {
            var val = $(obj).mbgetVal();

            var closerHistory = dh[0];
            var last = true;
            for (var i = 0; i < dh.length; i++) {
              var d = dh[i];
              if (dh[i].createdOn > val) {
                last = false;
                break;
              }
              closerHistory = dh[i];
            }
            refreshStatistics(closerHistory, last);
          }
        });
      }
    }
  })

});


$.fn.updateCircle = function(circle){
  this.each(function(){
    if (circle) {
      var wrapperW = $(this).outerWidth();
      circle.updateRadius(wrapperW / 2.5);
      circle.updateWidth(wrapperW * 0.035);
    }
  })
};

$(window).on("resize.circles", function () {
  $(".statsBlock").each(function(){
    var circle = $(".circle",this).data("circle");
    $(this).updateCircle(circle);
  });
  resizeStatus();
});


function drawStatus(obj){
  //console.debug("drawStatus", obj);
  var ndo = $("#statBoxes");
  var stBox=drawstatBox("STATUS","<%=I18n.get("STATUS")%>");

  var circle = stBox.find(".circle");

  var status= $("<div>").prop("id","statusBall").css({'border-radius':'100%','margin':'0 auto',position:"relative", "display":"table"});
  var label=$("<div>").css({display: "table-cell","vertical-align":"middle","text-align": "center"}).append("<span class='_stsLabel'>"+_TaskStatusDecoder[obj.status].label.toLowerCase()+"<span>");
  var subLabel=$("<span>").addClass("additionalLabelText");

  label.append(subLabel);
  status.append(label);
  circle.append(status);
  ndo.append(stBox);
  refreshStatus(obj);
}

function refreshStatus(dataHistory){
  //console.debug("refreshStatus", dataHistory);
  var ball = $("#statusBall");
  ball.css("background-color",_TaskStatusDecoder[dataHistory.status].color);
  ball.css("color",_TaskStatusDecoder[dataHistory.status].contrastColor);
  ball.find("._stsLabel").html(_TaskStatusDecoder[dataHistory.status].label);

  ball.find(".additionalLabelText").css("color",_TaskStatusDecoder[dataHistory.status].contrastColor).html(dataHistory.lastStatusChangeDate?("<%=I18n.get("STATUS_CHANGE_DATE")%><br>"+new Date(dataHistory.lastStatusChangeDate).format()):"");
}

function resizeStatus(){
  //console.debug("resizeStatus");
  var stBox = $("#STATUS");
  var lato = stBox.outerWidth()/1.25;
  var pallaTonda=$("#statusBall").css({"width":lato,"height":lato});
  var lbl=pallaTonda.find("._stsLabel");
  var h=lbl.height();
  var w=lbl.width();
  var fontSize= lato*.4 * h/w;
  fontSize=Math.min(fontSize,22);
  lbl.css("font-size",fontSize+"px");
}



function drawStatistics(dataHistory){
  //console.debug("drawStatistics", obj);
  var data=computeStats(dataHistory,true);
  var ndo = $("#statBoxes");
  for (var type in data){
    ndo.append(drawstatBox(type, data[type].title));
    drawCircle(type, data);
  }
}


function drawCircle(type,data) {
  if (!type)
    return;

  var perc = data[type].perc;

  var value = Math.floor(perc);

  var colors=['#E8E8E8', '#2F97C6'];
  if (perc > 100) {
    colors=["#2F97C6","#C30000"];
    value = value - 100;
  }

  var circle = Circles.create({
    id:       type,
    radius:   getWidth(),
    width:    4,
    value:    value,
    duration: 10,
    text:     function (val) {return refreshOneCircleLabel(this) ;},
    colors:    colors
  });

  circle._origRadius = circle._radius;
  circle._data= data[type];
  $("#"+type).data("circle",circle);
}


function refreshOneCircleLabel(circle) {
  //console.debug(circle);
  var ret="";
  if (circle._data) {
    //console.debug(circle._data)
    ret= Math.floor(circle._data.perc) + '<small>%</small>' + (circle._data.label ? '<br><span class="additionalLabelText">' + circle._data.label + '</span>' : '');
    circle._value=circle._data.perc>100?(circle._data.perc%100):circle._data.perc;
  }
  return ret;
}


function refreshStatistics(dataHistory,last){
  //console.debug("refreshStatistics", obj);
  var data = computeStats(dataHistory,last);
  var ndo = $("#statBoxes");
  for (var type in data){
    var circleDom = $("#" + type);
    var circle = circleDom.data("circle");
    if (circle) {
      circle._data = data[type];
      circle.updateColors(data[type].perc<=100? ['#E8E8E8', '#2F97C6']:["#2F97C6","#C30000"]);
      circle.update(data[type].perc>100?data[type].perc%100:data[type].perc, 0);
    }
  }
  refreshStatus(dataHistory);
  refreshDates(dataHistory,last);
}


function refreshDates(dataHistory,last){
  //console.debug(dataHistory,last);
  $("#statTaskDates").empty();
  if (!last){
    var datesObj={
      atDate:new Date(dataHistory.createdOn).format(),
      start:new Date(dataHistory.startDate).format(),
      end:new Date(dataHistory.endDate).format(),
      duration:dataHistory.duration
    };

    $("#statTaskDates").append($.JST.createFromTemplate(datesObj ,"HISTDATES"));
  }
}

function computeStats(dataHistory,last) {
  var computedData={};

  <%---------------------- PROGRESS -------------------------------------%>
  computedData["TASK_PROGRESS"]={perc:dataHistory.progress,title:"<%=I18n.get("PROGRESS")%>",label:""};


  <%---------------------- TIME ELAPSED -------------------------------------%>
  var perc = 0;
  var dateToBeCompared = Date.now();
  //se sto guardando la storia devo sempre calcolare i giorni passati fino al giorno di ispezione
  if (!last)
    dateToBeCompared=dataHistory.createdOn;

  if (dataHistory.startDate < dateToBeCompared) {
    if ("STATUS_ACTIVE"!=dataHistory.status && "STATUS_SUSPENDED"!=dataHistory.status)
      dateToBeCompared= (dataHistory.lastStatusChangeDate? dataHistory.lastStatusChangeDate:dateToBeCompared);

    dateToBeCompared= new Date(dateToBeCompared);

    var timeElapsed = new Date(dataHistory.startDate).distanceInWorkingDays(dateToBeCompared);
    perc = (timeElapsed / dataHistory.duration) * 100;
    var label = timeElapsed + " / " + dataHistory.duration + "<br><%=I18n.get("DAYS").toLowerCase()%>";
    computedData["TIME_ELAPSED"]={perc:perc,label:label,title:"<%=I18n.get("TIME_ELAPSED")%>"};
  }

  <%---------------------- WORKLOG DOME / ESTIMATED -------------------------------------%>
  if (dataHistory.totalWorklogEstimated > 0) {
    perc = (dataHistory.totalWorklogDone / dataHistory.totalWorklogEstimated) * 100;
    var label = getMillisInHours(dataHistory.totalWorklogDone) + " / " + getMillisInHours(dataHistory.totalWorklogEstimated) + "<br><%=I18n.get("HOURS").toLowerCase()%>";
    computedData["WORK_DONE_OVER_ESTIMATED"]={perc:perc,label:label,title:"<%=I18n.get("WORK_DONE_OVER_ESTIMATED")%>"};
  }

  <%---------------------- COST DONE / ESTIMATED COSTS -------------------------------------%>
  if (dataHistory.totalCostsEstimated > 0 && <%= task.hasPermissionFor(logged,TeamworkPermissions.task_cost_canRead)%>) {
    perc = (dataHistory.totalCostsDone / dataHistory.totalCostsEstimated) * 100;
    var label = formatCurrency(dataHistory.totalCostsDone) + "<br>" + formatCurrency(dataHistory.totalCostsEstimated);
    computedData["COST_DONE_OVER_ESTIMATED"]={perc:perc,label:label,title:"<%=I18n.get("COST_DONE_OVER_ESTIMATED")%>"};
  }

  <%---------------------- COST DONE / BUDGET -------------------------------------%>
  if (dataHistory.forecasted> 0 && <%= task.hasPermissionFor(logged,TeamworkPermissions.task_cost_canRead)%>) {
    perc = (dataHistory.totalCostsDone / dataHistory.forecasted) * 100;
    var label = formatCurrency(dataHistory.totalCostsDone) + "<br>" + formatCurrency(dataHistory.forecasted);
    computedData["COST_DONE_OVER_BUDGET"]={perc:perc,label:label,title:"<%=I18n.get("COST_DONE_OVER_BUDGET")%>"};
  }

  <%---------------------- TASK_CLOSED_OVER_TOTAL -------------------------------------%>
  if (dataHistory.totalDescendant > 0) {
    perc = (dataHistory.totalDescendantClosed / dataHistory.totalDescendant ) * 100;
    var label = dataHistory.totalDescendantClosed + " / " + dataHistory.totalDescendant + "<br><%=I18n.get("TASKS").toLowerCase()%>";
    computedData["TASK_CLOSED_OVER_TOTAL"]={perc:perc,label:label,title:"<%=I18n.get("TASK_CLOSED_OVER_TOTAL")%>"};
  }

  <%---------------------- ISSUE_BLOCK_OVER_OPEN -------------------------------------%>
  var totScore = dataHistory.totalIssuesScoreOpen + dataHistory.totalIssuesScoreClosed;
  if (totScore > 0) {
    perc = (dataHistory.totalIssuesScoreClosed / totScore) * 100;
    var label = dataHistory.totalIssuesScoreClosed + " / " + totScore + "<br><%=I18n.get("POINTS").toLowerCase()%>";
    computedData["ISSUE_BLOCK_OVER_OPEN"]={perc:perc,label:label,title:"<%=I18n.get("ISSUE_BLOCK_OVER_OPEN")%>"};

  }

  <%---------------------- ISSUE_CLOSED_OVER_OPEN -------------------------------------%>
  if (dataHistory.totalIssues > 0) {
    perc = ((dataHistory.totalIssues - dataHistory.totalIssuesOpen) / dataHistory.totalIssues) * 100;
    var label = (dataHistory.totalIssues - dataHistory.totalIssuesOpen) + " / " + dataHistory.totalIssues + "<br><%=I18n.get("ISSUES").toLowerCase()%>";

    computedData["ISSUE_CLOSED_OVER_OPEN"]={perc:perc,label:label,title:"<%=I18n.get("ISSUE_CLOSED_OVER_OPEN")%>"};
  }


  <%---------------------- TEAM_OVER_USERS -------------------------------------%>
  if (dataHistory.teamSize > 0) {
    var totalLoggable =<%=totalLoggable%>;
    perc = (dataHistory.teamSize / totalLoggable) * 100;
    var label = dataHistory.teamSize + " / " + totalLoggable + "<br><%=I18n.get("OPERATORS").toLowerCase()%>";
    computedData["TEAM_OVER_USERS"]={perc:perc,label:label,title:"<%=I18n.get("TEAM_OVER_USERS")%>"};
  }

  return computedData;
}


function drawstatBox(id, label) {
  var statsBlock = $("<div>").addClass("statsBlock");
  var circle = $("<div>").addClass("circle").prop("id", id);
  var h3 = $("<h3>").html(label);
  statsBlock.append(circle).append(h3);
  return statsBlock;
}




function getWidth() {
  return ($(".statsBlock").width()/2.5);
}


</script>
</div>
