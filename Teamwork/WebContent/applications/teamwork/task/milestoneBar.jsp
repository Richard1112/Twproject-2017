<%@ page import="com.twproject.task.Task,org.jblooming.agenda.CompanyCalendar,org.jblooming.agenda.Period, org.jblooming.waf.view.PageState, java.util.List, org.jblooming.agenda.Scale, org.jblooming.waf.SessionState, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.view.PageSeed, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.display.paintable.*, org.jblooming.waf.settings.I18n, net.sf.json.JSONArray" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  Task task = (Task) pageState.getMainObject();

  long min = Long.MAX_VALUE;
  long max = Long.MIN_VALUE;

  // includes start and end for the task or for its parent
  Period schedule = task.getSchedule();
  if (task.isNew() && task.getParent()!=null && task.getParent().getSchedule() !=null)
    schedule = task.getParent().getSchedule();

  if (schedule != null) {
    min = Math.min(min,schedule.getValidityStartTime());
    max = Math.max(max,schedule.getValidityEndTime());
  }

  //today
  min = Math.min(min,System.currentTimeMillis());
  max = Math.max(max,System.currentTimeMillis());


  // find max and min of milestones
  List<Task.TaskMilestone> tsmi = task.getMilestones(true, true);
  for (Task.TaskMilestone tm : tsmi) {
    min = Math.min(min,tm.when.getTime());
    max = Math.max(max,tm.when.getTime());
  }

  // get the best fitting scale and increment of ticks
  Scale scale = Scale.getScaleForPeriod(min, max, SessionState.getLocale());


  // draw milestones
  PageSeed editTask = pageState.thisPage(request);
  editTask.setCommand(Commands.EDIT);

  JSONArray miles=new JSONArray();
  for (Task.TaskMilestone tm : tsmi) {
    miles.add(tm.jsonify());
  }

  // draw folio
  %><div class="bottomBar" id="moveBar" ></div>

<script>
  (function createBarFolio() {
    var miles=<%=miles%>;

    var moveBar = $("#moveBar");
    moveBar.empty();

    var startMillis = <%= scale.startPointTime%>;
    var endMillis = <%=scale.endPointTime%>;

    //console.debug(new Date(startMillis),new Date(endMillis));

    var barFolio = new Folio(moveBar);
    barFolio.width = endMillis - startMillis;
    barFolio.height = 50;
    barFolio.left = startMillis;
    barFolio.inPercent = true;

    var incField="<%=scale.getJSIncrementField()%>";
    var incAmount=<%=scale.fieldIncrement%>;
    var d = new Date(startMillis);

    while (d.getTime() <= endMillis) {
      var headLbl = $("<span>").html(d.format("<%=scale.tickLabelDateFormat%>")).addClass("moveBarEl");
      barFolio.addElement(headLbl, 0, d.getTime(), 30 * 24 * 3600000, 50);
      d.add(incField,incAmount);
    }

    //task schedule
    var task=$("<div>").prop("title","<%=task.getName()%>").css({opacity:0.7,"background-color":"#ffff00"});
    barFolio.addElement(task, 25, <%=task.getSchedule().getValidityStartTime()%>, <%=task.getSchedule().getValidityEndTime()-task.getSchedule().getValidityStartTime()%>, 25);
    var progress=<%=task.getProgress()%>;
    if (progress>0 && progress<=100) {
      var perc = $("<div>").css({width:progress+"%",height:"100%","background-color":"#BCFF3A"});
      task.append(perc);
    } else if (progress>100){
      task.css({"background-color":"#ff0000"})
    }


    //milestones
    for (var i=0;i<miles.length;i++){
      var mil=miles[i];
      var m=$("<span>").addClass("teamworkIcon").html("^").prop("title",mil.taskName).attr("taskId",mil.taskId).css({overflow:"visible",width:0,"margin-left":-8,cursor:"pointer"}).click(seeMiles);
      barFolio.addElement(m,25,mil.when,0,25);
    }

    //today
    var today = $("<span>").prop("title", "<%=I18n.get("TODAY")%>").addClass("moveBarToday");
    barFolio.addElement(today, 0, new Date().getTime(), barFolio.getPixelWidth() * 2, 50);

    barFolio.redraw();

    function seeMiles(){
      self.location.href="taskOverview.jsp?CM=ED&OBJID="+$(this).attr("taskId");
    }

  })()



</script>
