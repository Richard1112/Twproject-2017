<%@ page import="org.jblooming.waf.settings.I18n" %>
<%----------------------------------------------------------  PAGES DEFINITION  ---------------------------------------------------------%>

<%-------------------------------------  time counter page ---------------------------------------%>
<div data-role="page" id="timeCounters" title="<%=I18n.get("TIMECOUNTER_MENU")%>">
  <div data-role="content">
    <div id="timeCountersPlace"></div>
  </div>
</div>
<%----------------------------------------------------------  TEMPLATES  ---------------------------------------------------------%>
<div class="_mobileTemplates">
    <%-- ---------------------------------  COUNTER TEMPLATE  ------------------------------------------------------------------------------------------------------------------- --%>
    <%=JST.start("TIME_COUNTER")%>

    <div class="timecounter groupRow" class="_errorTemplateMessage" assigId="(#=obj.id#)">
      <div class="groupCell inputBox col12">
        <p align="center" ><b>(#=obj.taskName#)</b></p>
        <p align="center">
          <span class="counterwatch" style="cursor:pointer" onclick="hideFeedbackMessages();"></span>
          <span id="worklogduration"></span>
        </p>
      </div>
      <div class="groupCell inputBox touchEl col12">
        <textarea id="worklogaction" style="display:none" placeholder="<%=I18n.get("WORKLOG_ACTION_WRITE_HERE")%>"></textarea>
      </div>
    </div>
    <%=JST.end()%>

</div>
<%----------------------------------------------------------  TIMECOUNTERS PAGES FUNCTIONS  ---------------------------------------------------------%>
<script>


  function timeCounterEditorEnter(event, data, fromPage, isBack) {

    //si carica l'assignement
    var request = {"CM": "GETASSIGNMENT", assId: applicationCache.countedAssignment};

    callController(request, function (response) {
      if (response.assignment){
        var ndo = $("#timeCounterEditorPlace");
        var etm = $.JST.createFromTemplate(response.assignment, "TIME_COUNTER");
        var watch = etm.find(".counterwatch");
        var format = '%0h:%0m:%0s';
        watch.data("time", response.assignment.countingStartedMillis).activateTimeCounter(format);
        ndo.append(etm);
      }
    });


  }


  function timeCounterEditorLeave(event, data, fromPage, isBack) {
    $(this).find("#timeCounterEditorPlace").empty();
    $(this).find("button.save").removeClass("save").addClass("stop").attr("onclick", "stopTimeCounter($(this))").html("stop");
  }


  function saveTimeCounterWorklog(el){

    var request = {"CM": "SAVEWORKLOG"};

    request.ASSID =  currentPage.find("div.timeCounter").attr("assId");
    request.DATEINT = (new Date()).toInt();
    request.WORKLOG_ACTION =  currentPage.find("#worklogaction").val()
    request.DURATION = currentPage.find(".timecounter").attr("worklogtime")

    callController(request, function (response) {
      //update cache
      var worklog = response.worklog;

      if(worklog){

      currentDate = new Date(worklog.insertedMillis);
      var dateInt = currentDate.toInt();
      var monthWorklogs = applicationCache.worklogs[parseInt(dateInt / 100)];
      if (monthWorklogs) {
        var wlDay = monthWorklogs[dateInt];
        if (!wlDay) {
          wlDay = [];
          monthWorklogs[dateInt] = wlDay;
        }
        wlDay.push(worklog);
      }
        backPage();
      }else{
        //todo messaggino
      }

    });
  }


  function startTimeCounter(el) {

    var div = el.closest("div[assId]");
    var assId = div.attr("assId");

    if(applicationCache.countedAssignment){
      stopTimeCounter(true);
    }

    var request = {CM: "STARTCOUNTER", assId: assId};
    callController(request, function (response) {
      if(response.ok){
        showTimeCounterIcon();
        applicationCache.countedAssignment = assId;
        div.find(".timeCounter").data("time", new Date().getTime()).show().activateTimeCounter();
        el.removeClass("play").addClass("stop").html("s").attr("onclick","showTimeCounterEditor()");

      }
    });
  }


  function stopTimeCounter(switching) {

    var assId = applicationCache.countedAssignment;
    var request = {CM: "STOPCOUNTER", assId:  assId};
    callController(request, function (response) {
    if (response.ok && response.worklog) {

        hideTimeCounterIcon();
        applicationCache.countedAssignment = "";
        currentPage.find(".counterwatch").stopTimeCounterWatch().hide();

        var duration = getMillisInHoursMinutes(response.worklog.duration);

        var delay=0;
        if(!switching){ //se sei nella lista dei task e quindi non passi dall viewer del counter non deve fare back
          backPage();
          delay=500;
        }
        setTimeout(function(){
          goToPage("worklogEditor",{"assId":assId,"taskName":response.worklog.taskName, "duration": duration, "wlDoneOn": (new Date()).toInt() });
        },delay)

      }

    });

  }



  function showTimeCounterEditor(){
    goToPage("timeCounterEditor");
  }


  function showTimeCounterIcon() {
    return $("#__COUNTER").show().addClass("animate");
  }


  function hideTimeCounterIcon() {

    return $("#__COUNTER").hide().removeClass("animate");

  }

  $.fn.activateTimeCounter = function(format){
    if(!format)
      format = '%0h<span class="counterLabel">h</span> %0m<span class="counterLabel">m</span><span class="sec"> %0s<span class="counterLabel">s</span></span>';
    var el = $(this);
    var millis = el.data("time") || new Date().getTime();
    var d = new Date(millis).toString();
    el.tinyTimer({ from: d , format: format });
    return el;
  };

  $.fn.stopTimeCounterWatch = function(){
    var tt = $(this).data("tinyTimer");
    if (tt)
      tt.stop();
    return $(this).removeData("tinyTimer");
  };

</script>



