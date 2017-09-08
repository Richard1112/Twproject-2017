<%@ page import="com.twproject.operator.TeamworkOperator, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged= (TeamworkOperator) pageState.getLoggedOperator();

  // get the focused millis
  long focusedMillis= pageState.getEntry("FOCUS_MILLIS").longValueNoErrorNoCatchedExc();
  focusedMillis= focusedMillis==0?System.currentTimeMillis():focusedMillis;
  pageState.addClientEntry("FOCUS_MILLIS",focusedMillis);
%>

<div id="summaryBar" style="height:50px;overflow: hidden;" class="summaryBar"></div>

<style >
  .sbAgeEv{
    background-color: #BCFF3A;
    box-shadow: 0 0 0 1px #6e9325;
    opacity: .5;
  }
  .sbTask{
    background-color: #2f2f2f;
    opacity: .5;
  }
  .sbTaskMil{
    color: #2f2f2f;
    font-size: 12px;
    opacity: .7;
  }
  .sbIssue{
    color: #2f2f2f;
    font-size: 12px;
    opacity: .7;
  }
</style>

<script type="text/javascript">

  sb_startup(<%=focusedMillis%>);

  function sb_startup(focusMillis){
    var date = new Date(focusMillis);
    date.clearTime();
    date.setFirstDayOfThisWeek();

    var startMillis = date.getTime();
    date.setDate(date.getDate() + 6);
    date.setHours(23,59,59);
    var endMillis = date.getTime();


    var barFolio = sb_createBarFolio(startMillis,endMillis);
    barFolio.redraw();

    sb_loadAppointments(barFolio,focusMillis);
    sb_loadIssues(barFolio,focusMillis);
    sb_loadTasks(barFolio,startMillis,endMillis);
  }



  function sb_createBarFolio(startMillis, endMillis) {
    //console.debug("createBarFolio")
    var moveBar = $("#summaryBar");
    moveBar.empty();

    var barFolio = new Folio(moveBar);
    barFolio.width = endMillis - startMillis;
    barFolio.height = 50;
    barFolio.left = startMillis;
    barFolio.inPercent = true;

    var d = new Date(startMillis);
    while (d.getTime() < endMillis) {
      var headLbl = $("<span>").html(d.format("EEE dd")).addClass("moveBarEl");
      barFolio.addElement(headLbl, 0, d.getTime(), 24 * 3600000, 50);
      d.setDate(d.getDate() + 1);
    }

    //today
    var today = $("<span>").prop("title", "<%=I18n.get("TODAY")%>").addClass("moveBarToday");
    barFolio.addElement(today, 0, new Date().getTime(), barFolio.getPixelWidth() * 2, 50);

    return barFolio;
  }



  function sb_loadAppointments(folio,focusMillis){
    var request={
      CM: "GETEVENTS",
      type         : "WEEK",
      focusMillis  : focusMillis,
      FILTER       : "NONE",
      WG_IDS       : "<%=logged.getPerson().getId()%>"
    };

    showSavingMessage();
    $.getJSON(contextPath+"/applications/teamwork/agenda/agendaAjaxController.jsp", request, function (response) {
      jsonResponseHandling(response);
      if (response.ok == true) {
        //console.debug(response)

        for (var i=0;response.events && i<response.events.length;i++){
          var agev=response.events[i];
          var ev = $("<span>").prop("title", agev.summary ).addClass("sbAgeEv");
          folio.addElement(ev, 15, agev.startMillis, agev.endMillis-agev.startMillis, 10);
        }
        folio.redraw();
      }
      hideSavingMessage();
    });
  }

  function sb_loadTasks(folio,startMillis,endMillis){
    var request={
      CM: "GTSKOPPER",
      startMillis  : startMillis,
      endMillis  : endMillis
    };

    showSavingMessage();
    $.getJSON(contextPath+"/applications/teamwork/task/taskAjaxController.jsp", request, function (response) {
      jsonResponseHandling(response);
      if (response.ok == true) {
        //onsole.debug(response)

        for (var i=0;response.tasks && i<response.tasks.length;i++){
          var tsk=response.tasks[i];

          //var tk = $("<span>").prop("title", tsk.code + " " + tsk.name).addClass("sbTask");
          //folio.addElement(tk, 27, tsk.start, tsk.end-tsk.start, 10);


          //si disegnano le milestones
          if (tsk.startIsMilestone) {
            var tk = $("<span>").prop("title", tsk.code + " " + tsk.name).addClass("teamworkIcon sbTaskMil").html("^");
            folio.addElement(tk, 27, tsk.start, folio.getPixelWidth() * 10, 10);
          }
          if (tsk.endIsMilestone) {
            var tk = $("<span>").prop("title", tsk.code + " " + tsk.name).addClass("teamworkIcon sbTaskMil").html("^");
            folio.addElement(tk, 27, tsk.end- folio.getPixelWidth() * 10, folio.getPixelWidth() * 10, 10);
          }

        }
        folio.redraw();
      }
      hideSavingMessage();
    });
  }


  function sb_loadIssues(folio,focusMillis){
    var request={
      CM: "GETTASKISS",
      type         : "WEEK",
      focusMillis  : focusMillis,
      FILTER       : "NONE",
      ISSUEONLY    : "yes",
      WG_IDS       : "<%=logged.getPerson().getId()%>"
    };

    showSavingMessage();
    $.getJSON(contextPath+"/applications/teamwork/agenda/agendaAjaxController.jsp", request, function (response) {
      jsonResponseHandling(response);
      if (response.ok == true) {
        //console.debug(response)

        for (var i=0;response.issues && i<response.issues.length;i++){
          var iss=response.issues[i];
          var start=Date.fromInt(iss[0]);
            start.clearTime().getTime();
          var qta=iss[2];
          var wl=iss[3];

          var is = $("<span>").addClass("sbIssue").append($("<span>").addClass("teamworkIcon").append("i")).append(qta);
          folio.addElement(is, 35,start, wl, 10);
        }
        folio.redraw();
      }
      hideSavingMessage();
    });
  }


</script>


