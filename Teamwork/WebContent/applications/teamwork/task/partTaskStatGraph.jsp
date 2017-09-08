<%@ page import="com.twproject.task.Task, net.sf.json.JSONArray, org.jblooming.agenda.CompanyCalendar, org.jblooming.utilities.DateUtilities, org.jblooming.waf.constants.Commands, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.Date, java.util.List, java.util.TreeMap" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  Task task = (Task) pageState.getMainObject();


    List<Object[]> workHeartBeat = task.getWorkHeartBeat(true);
    TreeMap<Integer, Integer> issueHeartBeat = task.getIssueHeartBeat(true);


  //-------------------------------------------------------------------------------- WORKLOG HEART BEAT --------------------------------------------------------------------------------

    if (workHeartBeat.size() > 0 || issueHeartBeat.size() > 0) {
%>

<script type="text/javascript" src="<%=ApplicationState.contextPath%>/commons/js/jquery/sparkline/jquery.sparkline.js"></script>
<%
  Date start = new Date();
  Date end = new Date();

  if (workHeartBeat.size() > 0) {

    //insert start date if before first wl add the project start date
    if ((Integer) (workHeartBeat.get(0)[0]) > DateUtilities.dateToInt(task.getSchedule().getStartDate()))
      start = task.getSchedule().getStartDate();
    else
      start = DateUtilities.intToDate((Integer) (workHeartBeat.get(0)[0]));

    //check who is first wl or issue
    //if (issueHeartBeat.size() > 0 && issueHeartBeat.firstKey() < DateUtilities.dateToInt(start))
    //  start = DateUtilities.intToDate(issueHeartBeat.firstKey());


    end = task.getSchedule().getEndDate();

    //check who is last wl or issue
    //if (issueHeartBeat.size() > 0 && issueHeartBeat.lastKey() > DateUtilities.dateToInt(end))
    //  end = DateUtilities.intToDate(issueHeartBeat.lastKey());

    //if the project is open add today
    if (task.isActive() && end.before(new Date()))
      end = new Date();

    CompanyCalendar cc = new CompanyCalendar(start);

    int whbIndex = 0;


    boolean needAggregate = ((end.getTime() - start.getTime()) / CompanyCalendar.MILLIS_IN_DAY) > 150; //aggregate in case of task longer than 150 days

    JSONArray workDone = new JSONArray();
    JSONArray workDoneAggreg = new JSONArray();
    long montant = 0;


    //aggregate by week
    //start with the first day of week
    cc.set(CompanyCalendar.DAY_OF_WEEK, cc.getFirstDayOfWeek());
    start = cc.getTime();
    while (cc.getTime().compareTo(end) <= 0) {
      long aggrPeriodStart = cc.getTimeInMillis();
      cc.add(CompanyCalendar.DATE, needAggregate ? 7 : 1); //aggregate 7 or 1
      long aggrPeriodEnd = cc.getTimeInMillis() - 1;

      long weekWorklog = 0;

      while (whbIndex < workHeartBeat.size() && DateUtilities.intToDate((Integer) workHeartBeat.get(whbIndex)[0]).getTime() <= aggrPeriodEnd) {
        Long wl = (Long) workHeartBeat.get(whbIndex)[1];
        weekWorklog += wl;
        whbIndex++;
      }

      JSONArray weekVal = new JSONArray();
      weekVal.add(aggrPeriodStart);
      weekVal.add(weekWorklog);
      workDone.add(weekVal);

      montant += weekWorklog;
      JSONArray weekMontVal = new JSONArray();
      weekMontVal.add(aggrPeriodStart);
      weekMontVal.add(montant);
      workDoneAggreg.add(weekMontVal);
    }

    // add last point to task end
    JSONArray weekVal = new JSONArray();
    weekVal.add(end.getTime());
    weekVal.add(0);
    workDone.add(weekVal);

    JSONArray weekMontVal = new JSONArray();
    weekMontVal.add(end.getTime());
    weekMontVal.add(montant);
    workDoneAggreg.add(weekMontVal);


    //ideal trend
    JSONArray idealTrend = new JSONArray();
    JSONArray p;

    if (start.getTime() < task.getSchedule().getStartDate().getTime()) {
      p = new JSONArray();
      p.add(start.getTime());
      p.add(0);
      idealTrend.add(p);
    }


    p = new JSONArray();
    p.add(task.getSchedule().getStartDate().getTime());
    p.add(0);
    idealTrend.add(p);

    p = new JSONArray();
    p.add(task.getSchedule().getEndDate().getTime());
    p.add(task.getTotalWorklogEstimated());
    idealTrend.add(p);

    if (end.compareTo(task.getSchedule().getEndDate()) > 0) {
      p = new JSONArray();
      p.add(end.getTime());
      p.add(task.getTotalWorklogEstimated());
      idealTrend.add(p);
    }


    PageSeed wlAnali = pageState.pageInThisFolder("worklog/worklogList.jsp", request);
    wlAnali.command = Commands.FIND;
    wlAnali.addClientEntry("TASK", task);
    wlAnali.addClientEntry("TASK_WORKLOG_SHOW_CHILDREN", true);

    long rangeMax = Math.max(task.getTotalWorklogDone(), task.getTotalWorklogEstimated());
%>

<div style="position:relative;width:98%;height:115px; display: inline-block; padding-right: 10px">
  <div style="position:absolute;top:-10px;left:0;">
    <b style="color:#1FABEC;">---</b>&nbsp;<span style="opacity:.2;"><%=I18n.get("WORKLOG_IDEAL_TREND")%></span><br>
    <b style="color:#FFA322;">---</b>&nbsp;<span style="opacity:.2;"><%=I18n.get("WORKLOG_AGGREGATED")%></span><br>
    <b style="color:#8007bb;">---</b>&nbsp;<span style="opacity:.2;"><%=I18n.get("WORKLOG_DONE")%></span>
  </div>
  <div id="wlhb"></div>
</div>
<script type="text/javascript">
  $(function () {

    var sparklineHomeWlhb = function () {

      $('#wlhb')
        .empty()
        .sparkline(<%=idealTrend%>, {type: "line", height: 100, width: "100%", composite: false, lineColor: "#B2D5EB", fillColor: "#C6E9FF", chartRangeMin: 0, chartRangeMax:<%=rangeMax%>, highlightLineColor: null, spotColor: false, maxSpotColor: false, minSpotColor: false, tooltipSkipNull: false, tooltipFormatter: function (sparkline, options, fields) {
          return "";
        }})
        .sparkline(<%=workDoneAggreg%>, {type: "line", height: 100, width: "100%", composite: true, lineColor: "#FFA322", fillColor: false, spotRadius: 4, chartRangeMin: 0, chartRangeMax:<%=rangeMax%>, spotColor: false, maxSpotColor: false, minSpotColor: false, tooltipSkipNull: false, tooltipFormatter: function (sparkline, options, fields) {
          return "<div class='jptootipContent'>"+"&nbsp;<%=I18n.get("WEEK_OF")%> " + (new Date(fields.x).format()) + "&nbsp;<br>" + "&nbsp;<%=I18n.get("WORKLOG_AGGREGATED")%>: " + getMillisInHoursMinutes(fields.y) + "&nbsp;<br>";
        }})
        .sparkline(<%=workDone%>, {type: "line", height: 100, width: "100%", composite: true, lineColor: "#8007bb", fillColor: false, spotRadius: 4, chartRangeMin: 0, chartRangeMax:<%=rangeMax%>, spotColor: false, maxSpotColor: "#ff0000", minSpotColor: "#0000ff", tooltipSkipNull: false, tooltipFormatter: function (sparkline, options, fields) {
          return "&nbsp;<%=I18n.get("WORKLOG_DONE")%>: " + getMillisInHoursMinutes(fields.y) + "&nbsp;<br></div>";
        }})
        .bind('sparklineClick', function (ev) {
          var sparkline = ev.sparklines[1];
          var region = sparkline.getCurrentRegionFields();
          var wlAnali = "<%=wlAnali.toLinkToHref()%>&WORKLOG_AT_DAY=" + (new Date(region.x).format()) + ":" + (new Date(region.x + (3600000 * 24 * 6)).format());
          window.location.href = wlAnali;
        });
    };

    sparklineHomeWlhb();
    var sparkResize;
    $(window).resize(function (e) {
      clearTimeout(sparkResize);
      $('#wlhb').empty();
      sparkResize = setTimeout(function () {
        sparklineHomeWlhb();
      }, 500);
    });
  });
</script>
<%
  }
  //-------------------------------------------------------------------------------- ISSUES BURNDOWN --------------------------------------------------------------------------------
  if (issueHeartBeat.size() > 0) {

    //start = new Date();

    // le issue si fanno vedere tutte dentro le date del task, se erano state create prima skulo
    start = task.getSchedule().getStartDate();

    //inserisce uno zero come primo valore alla data di inizio, se serve
    int startInt = DateUtilities.dateToInt(start);
    if (issueHeartBeat.firstKey() > startInt)
      issueHeartBeat.put(startInt, 0);

    //add Today
    int today = DateUtilities.dateToInt(new Date());
    if (issueHeartBeat.lastKey() < today)
      issueHeartBeat.put(today, issueHeartBeat.lastEntry().getValue());

    //check who is last wl or issue
    //if (issueHeartBeat.lastKey() < DateUtilities.dateToInt(end))
    //  issueHeartBeat.put(DateUtilities.dateToInt(end), issueHeartBeat.lastEntry().getValue());


    int totIssue = 0;

    //aggregate by week
    JSONArray byWeek = new JSONArray();
    for (Integer intDate : issueHeartBeat.keySet()) {
      totIssue += issueHeartBeat.get(intDate);
      if (intDate<startInt) // le issue inserite prima della data di inizio del progetto si sommano sul primo valore
        continue;
      JSONArray weekVal = new JSONArray();
      weekVal.add(DateUtilities.intToDate(intDate).getTime());
      weekVal.add(totIssue);
      byWeek.add(weekVal);
    }

    PageSeed issues = pageState.pageFromRoot("issue/issueList.jsp");
    issues.command = Commands.FIND;
    issues.addClientEntry("FLT_ISSUE_TASK", task);
    issues.addClientEntry("FLT_TASK_ISSUE_SHOW_CHILDREN", true);


%>
<div style="position:relative;width:98%;height:115px; display: inline-block; padding-left: 10px">
  <div style="position:absolute;top:10px;left:0;opacity:.2;"><%=I18n.get("ISSUES_OPEN")%>
  </div>
  <div id="isshb"></div>
</div>
<script type="text/javascript">
  $(function () {

    var sparklineHomeIsshb = function () {
      var data =<%=byWeek%>;
      $('#isshb').empty().sparkline(data, {type: "line",chartRangeMin:0, height: 100, width: "100%", lineColor: "#7aad24", fillColor: "#bbff63", spotRadius: 4, maxSpotColor: "#ff0000", tooltipSkipNull: false, tooltipFormatter: function (sparkline, options, fields) {
        return ("<div class='jptootipContent'>"+new Date(fields.x).format()) + "<br> <%=I18n.get("ISSUES_OPEN")%>: " + fields.y+"</div>";
      }
      })
      /*.bind('sparklineClick', function(ev) {
       var sparkline = ev.sparklines[0];
       var region = sparkline.getCurrentRegionFields();
       var wlAnali="
      <%=issues.toLinkToHref()%>&FLT_ISSUE_STATUS_LAST_CHANGE="+(new Date(region.x).format())+":"+(new Date(region.x+3600000*24).format());
       window.location.href=wlAnali;
       });*/
    }

    sparklineHomeIsshb();
    var sparkResize;
    $(window).resize(function (e) {
      clearTimeout(sparkResize);
      $('#isshb').empty();
      sparkResize = setTimeout(function () {
        sparklineHomeIsshb();
      }, 500);
    });

  });
</script>
<%
      }
    }



%>
