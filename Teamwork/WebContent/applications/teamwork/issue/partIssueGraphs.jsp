<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.resource.Resource, com.twproject.security.TeamworkPermissions, com.twproject.task.*,
com.twproject.worklog.Worklog, net.sf.json.JSONArray, org.hibernate.Query, org.jblooming.oql.OqlQuery, org.jblooming.utilities.DateUtilities,
org.jblooming.utilities.JSP, org.jblooming.waf.SessionState, org.jblooming.waf.html.display.PercentileDisplay, org.jblooming.waf.settings.I18n,
org.jblooming.waf.view.PageState, java.util.*"%>
<%!
    /**
     * used to have in a sorted map two elements in the same millisecond
     */
  public class TimeId implements Comparable<TimeId> {
    public long time;

    public TimeId(long millisecond){
      this.time=millisecond;
    }
    public int compareTo(TimeId o) {

      int ret=1;
      if (time>o.time )
        ret=1;
      else if (time<o.time)
        ret=-1;
      else
        if (time==o.time && this!=o )
          ret= this.hashCode()-o.hashCode();
        else
          ret=0;
      return ret;
    }

    public boolean equals(TimeId o) {
      return compareTo(o)==0;
    }

  }


%><%

  PageState pageState = PageState.getCurrentPageState(request);

  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  Task sprint = (Task) pageState.getMainObject();
  sprint.testPermission(logged, TeamworkPermissions.task_canRead);

  if (sprint.getSchedule()!=null && sprint.getSchedule().getStartDate() != null && sprint.getSchedule().getEndDate() != null ) {


  //_______________________________________________________________________________________________________________________________________________________
  //FEED FROM ISSUES AND ASSIGS

    long totWorklogDone = 0;

    int totUnassignedIssues = 0;
    int totUnassignedIssuesHis = 0;
    Map<Resource, Long> worklogPerAssignee = new HashMap();
    Map<Resource, Long> worklogAssignedPerAssignee = new HashMap();
    Map<IssueStatus, Integer> totalIssuesPerStatus = new TreeMap();
    Map<Resource, Integer> totalIssuesPerAssignee = new TreeMap();
    TreeMap<String, Integer> totalIssuesPerGravity = new TreeMap(Collections.reverseOrder());
    Map<Resource, Integer> totalIssuesPerAssigneeHis = new TreeMap();
    TreeMap<String, Integer> totalIssuesPerGravityHis = new TreeMap(Collections.reverseOrder());


    TreeMap <TimeId,Integer> issues= new TreeMap();
    TreeMap <TimeId,Long> effort= new TreeMap();

    Set<Issue> issueList = sprint.getIssues();
    for (Issue issue : issueList) {

      Resource assignedTo = issue.getAssignedTo();

      List<IssueHistory> issueHistory = issue.getIssueHistories();

      //check the state of issue creation
      IssueStatus iIS= issue.getStatus();
      if (JSP.ex(issueHistory)) {
        IssueStatus oldStatus = issueHistory.iterator().next().getOldStatus();
        iIS= oldStatus==null?iIS:oldStatus;
      }

      if (iIS.isBehavesAsOpen()){
        issues.put(new TimeId(issue.getCreationDate().getTime()),1); //issue created
      }

      if (issue.getEstimatedDuration()>0)
        effort.put(new TimeId(issue.getCreationDate().getTime()),issue.getEstimatedDuration());

      if (JSP.ex(issueHistory)) {
        for (IssueHistory ih : issueHistory) {
          TimeId time = new TimeId(ih.getCreationDate().getTime());
          if (ih.getStatus()!=null && ih.getStatus().isBehavesAsClosed()) {
            if (ih.getOldStatus()==null || ih.getOldStatus().isBehavesAsOpen()){
              issues.put(time, -1); //issue closed
            }
          } else if (ih.getStatus()!=null && ih.getStatus().isBehavesAsOpen()){
            if (ih.getOldStatus()!=null && ih.getOldStatus().isBehavesAsClosed()){
               issues.put(time, 1); //issue re-opened
            }
          }
        }
      }

      if (assignedTo != null) {
        Long totWklg = worklogAssignedPerAssignee.get(assignedTo);
        worklogAssignedPerAssignee.put(assignedTo, (totWklg != null ? totWklg : 0) + issue.getEstimatedDuration());

        Integer tipa = totalIssuesPerAssigneeHis.get(assignedTo);
        totalIssuesPerAssigneeHis.put(assignedTo, (tipa != null ? tipa : 0) + 1);

        if (issue.getStatus().isBehavesAsOpen()){
          Integer tipao = totalIssuesPerAssignee.get(assignedTo);
          totalIssuesPerAssignee.put(assignedTo, (tipao != null ? tipao : 0) + 1);
        }

      } else {
        if (issue.getStatus().isBehavesAsOpen())
          totUnassignedIssues++;
        totUnassignedIssuesHis++;
      }

      IssueStatus status = issue.getStatus();
      if (JSP.ex(status)) {
        Integer ts = totalIssuesPerStatus.get(status);
        totalIssuesPerStatus.put(status, (ts != null ? totalIssuesPerStatus.get(status) : 0) + 1);
      }

      String gr = issue.getGravity();
      if (JSP.ex(gr)) {
        Integer gri = totalIssuesPerGravityHis.get(gr);
        totalIssuesPerGravityHis.put(gr, (gri != null ? totalIssuesPerGravityHis.get(gr) : 0) + 1);

        if (issue.getStatus().isBehavesAsOpen()){
          Integer grio = totalIssuesPerGravity.get(gr);
          totalIssuesPerGravity.put(gr, (grio != null ? grio : 0) + 1);
        }
      }
    }


    for (Assignment assignment : sprint.getAssignments()) {
      long worklogEstimated = assignment.getEstimatedWorklog();
      if (worklogEstimated > 0) {
        effort.put(new TimeId(assignment.getCreationDate().getTime()),worklogEstimated);
        Long totWklg = worklogAssignedPerAssignee.get(assignment.getResource());
        worklogAssignedPerAssignee.put(assignment.getResource(), (totWklg != null ? totWklg : 0) + worklogEstimated);
      }
    }


    // set worklog done
    String hql="select wl from "+Worklog.class.getName()+" as wl where wl.assig.task=:tsk order by wl.inserted";
    Query query = new OqlQuery(hql).getQuery();
    query.setEntity("tsk",sprint);
    List <Worklog> wls=query.list();


    for (Worklog worklog : wls) {
      effort.put(new TimeId(worklog.getInserted().getTime()),-worklog.getDuration());    
      Long totWklg = worklogPerAssignee.get(worklog.getAssig().getResource());
      worklogPerAssignee.put(worklog.getAssig().getResource(), (totWklg != null ? totWklg : 0) + worklog.getDuration());
      totWorklogDone = totWorklogDone + worklog.getDuration();
    }

    // compute the startMillis and endMillis using focusedMillis and scaleType
    final long minMillisInBar = sprint.getSchedule().getStartDate().getTime();//scale.startPointTime;
    final long maxMillisInBar = sprint.getSchedule().getEndDate().getTime();//scale.endPointTime;


    // -------------------- prepare ideal serie
    long totalEstimated=0;
    for (long l:effort.values() )
      if (l>0)
        totalEstimated+=l;

    JSONArray idealData= new JSONArray();
    JSONArray point= new JSONArray();
    point.add(minMillisInBar);
    point.add(totalEstimated);
    idealData.add(point);

    point= new JSONArray();
    point.add(maxMillisInBar);
    point.add(0);
    idealData.add(point);


    // --------------------  prepare issues remaining series
    int totOpenIssueOnTaskStart=sprint.getTotalIssuesOpen();
    for ( TimeId t: issues.keySet()){
      if (t.time>=minMillisInBar)
        totOpenIssueOnTaskStart-=issues.get(t);
    }

    JSONArray issuesLine= new JSONArray();
    point= new JSONArray();
    point.add(minMillisInBar);
    point.add(totOpenIssueOnTaskStart);
    issuesLine.add(point);

    long current=totOpenIssueOnTaskStart;
    for (TimeId t: issues.keySet()){

      if (t.time>=minMillisInBar){
        current+=issues.get(t);
        point= new JSONArray();
        point.add(t.time);
        point.add(current);
        issuesLine.add(point);
      }
    }
    point= new JSONArray();
    point.add(maxMillisInBar);
    point.add(current);
    issuesLine.add(point);

    // --------------------  prepare estimation done
    current=0;
    for ( TimeId t: effort.keySet()){
      Long a= effort.get(t);
      if (t.time<minMillisInBar){
        current+=a;
      } else
        break;
    }

    long totalEstimatedAtStart=current;
    JSONArray effortLine= new JSONArray();
    point= new JSONArray();
    point.add(minMillisInBar);
    //hack
    point.add(totalEstimatedAtStart);
    effortLine.add(point);

    current=0;
    for ( TimeId t: effort.keySet()){
      Long a= effort.get(t); 

      current+=a;
      if (t.time>=minMillisInBar){
        point= new JSONArray();
        point.add(t.time);
        point.add(current);
        effortLine.add(point);
      }
    }
    point= new JSONArray();
    point.add(maxMillisInBar);
    point.add(current);
    effortLine.add(point);



/*
________________________________________________________________________________________________________________________________________________________________________


 burn down

________________________________________________________________________________________________________________________________________________________________________

*/


  %><div class="container level_2">
<br>
   <h2><%=I18n.get("ISSUES_BURN_DOWN")%></h2>
<div id="plot" style="width:90%; height:400px; margin:0 auto"></div><br>



<%

  //_______________________________________________________________________________________________________________________________________________________
  // RESOURCE_BY_WORKLOG


%><div class="container level_1"><table class="table"><tr>
      <th class="tableHead"><%=I18n.get("ASSIGNEE")%></th>
      <th class="tableHead"><%=I18n.get("ISSUE_DONE/ASSIGNED")%></th>
      <th class="tableHead"><%=I18n.get("ISSUE_%ON_ASSIGNED")%></th>
      <th class="tableHead"><%=I18n.get("ISSUE_%ON_TOTAL_DONE")%></th>
    </tr><%

  for (Resource resource : worklogPerAssignee.keySet()) {

    if (resource!=null) {

    %> <tr class="alternate" >
    <td width="20%"><%=resource.getDisplayName()%></td>
    <%
      Long wapa = worklogAssignedPerAssignee.get(resource);
      if (wapa==null)
        wapa=0L;
      Long aLong = worklogPerAssignee.get(resource);
       if (aLong==null)
        aLong = 0L;
    %>
    <td width="10%" align="right"><%=aLong !=null ? DateUtilities.getMillisInHoursMinutes(aLong):"-"%>/<%=DateUtilities.getMillisInHoursMinutes(wapa)%></td>
    <td align="center"><%

    PercentileDisplay pd1=new PercentileDisplay( ( (double) aLong / (double)wapa )*100);
    pd1.width="120px";
    pd1.height="12px";
    pd1.backgroundColor="#e0e0e0";
    pd1.percentileColor="#ffff00";
    pd1.toHtml(pageContext);
    %></td>
    <td align="center"><%

    PercentileDisplay pd2=new PercentileDisplay( ( (double) aLong / (double)totWorklogDone )*100);
    pd2.width="120px";
    pd2.height="12px";
    pd2.backgroundColor="#e0e0e0";
    pd2.percentileColor="#ffff00";
    pd2.toHtml(pageContext);
    %></td></tr><%
  }
  }
   %></table></div><%



// -------------------------------------------------------------------------------------------- other charts ----------------------------------------------------------------------------------------

  if (issueList.size()>0){


  %><h2 style="padding-top: 30px"><%=I18n.get("ISSUES")%></h2><table  width="100%"><tr><%

  // --------------------------------------------------  ISSUES_BY_STATUS ----------------------------------------------------------------------
  JSONArray statusPie=new JSONArray();
  JSONArray statusColors=new JSONArray();
  int ibs=0;

  for (IssueStatus status : Issue.getStatusList()) {
    point= new JSONArray();
    Integer numOfIss = totalIssuesPerStatus.get(status);
    numOfIss=(numOfIss==null?0:numOfIss);
    if (numOfIss>0) {
      point.add(status.getDescription() + ": " + numOfIss);
      ibs += numOfIss;

      point.add(numOfIss);
      statusPie.add(point);
      statusColors.add(status.getColor());
    }
  }
  %><td valign="top"><div id="statusPie" style="width:100%;height:200px;"><%=ibs<=0?I18n.get("NO_ISSUES"):"" %></div></td><%


  // --------------------------------------------------  ISSUES_BY_GRAVITY on open ----------------------------------------------------------------------
  JSONArray gravityPie=new JSONArray();
  JSONArray gravityColors=new JSONArray();
  int ibg=0;
  for (String gravity : Issue.getGravities()){
    point= new JSONArray();

    Integer numOfIss = totalIssuesPerGravity.get(gravity);
    numOfIss=numOfIss==null?0:numOfIss;
    if (numOfIss>0) {
      point.add(I18n.get(gravity) + ": " + numOfIss);
      point.add(numOfIss);
      gravityPie.add(point);
      ibg += numOfIss;
      gravityColors.add(IssueBricks.getGravityColor(gravity));
    }
  }
  %><td valign="top"><div id="gravityPie" style="width:100%;height:200px;"><%=ibg<=0?I18n.get("NO_ISSUES"):"" %></div></td><%



  // --------------------------------------------------  ISSUES_BY_GRAVITY historic ----------------------------------------------------------------------
  JSONArray gravityPieHist=new JSONArray();
  int ibgh=0;
  for (String gravity : Issue.getGravities()){
    point= new JSONArray();
    Integer numOfIss = totalIssuesPerGravityHis.get(gravity);
    numOfIss=numOfIss==null?0:numOfIss;
    if (numOfIss>0) {
      point.add(I18n.get(gravity) + ": " + numOfIss);
      point.add(numOfIss);
      gravityPieHist.add(point);
      ibgh += numOfIss;
    }
  }
  %><td valign="top"><div id="gravityPieHis" style="width:100%;height:200px;"><%=ibgh<=0?I18n.get("NO_ISSUES"):"" %></div></td><%

%></tr></table><table  width="100%"><tr><%
     // --------------------------------------------------  ISSUES_BY_ASSIGNEE on open ----------------------------------------------------------------------
     JSONArray assigneePie=new JSONArray();
     int iba=0;
     for (Resource resource : totalIssuesPerAssignee.keySet()) {
       point= new JSONArray();
       Integer numOfIss = totalIssuesPerAssignee.get(resource);
       numOfIss=numOfIss==null?0:numOfIss;
       if (numOfIss>0) {
         point.add(resource.getDisplayName() + ": " + numOfIss);
         point.add(numOfIss);
         assigneePie.add(point);
         iba += numOfIss;
       }
     }
     if (totUnassignedIssues>0) {
       point= new JSONArray();
       point.add(I18n.get("UNASSIGNED")+": "+totUnassignedIssues);
       point.add(totUnassignedIssues);
       assigneePie.add(point);
       iba+=totUnassignedIssues;
     }
   %><td valign="top"><div id="assigneePie" style="width:100%;height:250px;"><%=iba<=0?I18n.get("NO_ISSUES"):"" %></div></td><%

  // --------------------------------------------------  ISSUES_BY_ASSIGNEE historic ----------------------------------------------------------------------
  JSONArray assigneePieHist=new JSONArray();
  int ibah=0;
  for (Resource resource : totalIssuesPerAssigneeHis.keySet()) {
    point= new JSONArray();
    Integer numOfIss = totalIssuesPerAssigneeHis.get(resource);
    numOfIss=numOfIss==null?0:numOfIss;
    if (numOfIss>0) {
      point.add(resource.getDisplayName() + ": " + numOfIss);
      point.add(numOfIss);
      assigneePieHist.add(point);
      ibah += numOfIss;
    }
  }
  if (totUnassignedIssuesHis>0) {
    point= new JSONArray();
    point.add(I18n.get("UNASSIGNED")+": "+totUnassignedIssuesHis);
    point.add(totUnassignedIssuesHis);
    assigneePieHist.add(point);
    ibah+=totUnassignedIssuesHis;
  }
  %><td valign="top"><div id="assigneePieHis" style="width:100%;height:250px;"><%=ibah<=0?I18n.get("NO_ISSUES"):"" %></div></td><%


  %></tr></table>


<script type="text/javascript">
  //console.debug("Ready step 0");

  var defs = [initialize(contextPath + "/commons/js/jquery/jqplot/jquery.jqplot.min.css", "css"),
initialize(contextPath + "/commons/js/jquery/jqplot/jquery.jqplot.min.js", "script")];


$.when.apply(null, defs).done(function () {
//console.debug("Ready step 1")
  var defs1 = [initialize(contextPath + "/commons/js/jquery/jqplot/plugins/jqplot.dateAxisRenderer.min.js", "script"),
    initialize(contextPath + "/commons/js/jquery/jqplot/plugins/jqplot.highlighter.min.js", "script"),
    initialize(contextPath + "/commons/js/jquery/jqplot/plugins/jqplot.cursor.min.js", "script"),
    initialize(contextPath + "/commons/js/jquery/jqplot/plugins/jqplot.pieRenderer.js", "script")];

  $.when.apply(null, defs1).done(startup)
});

function startup() {
//console.debug("Ready step 2")
  var plot = $.jqplot("plot", [<%=idealData%>, <%=issuesLine%>, <%=effortLine%>], {
    title:  "<%=sprint.getDisplayName()%>: <%=JSP.w(sprint.getSchedule().getStartDate())+" - "+JSP.w(sprint.getSchedule().getEndDate())%>",
    legend: {show: true},
    series: [
      {label: "<%=I18n.get("WORKLOG_IDEAL_TREND")%>"},
      {label:  "<%=I18n.get("ISSUES_OPEN")%>",
        yaxis: "y2axis"},
      {label: "<%=I18n.get("REMAINING_EFFORT")%>"}

    ],
    axes:   {
      xaxis:  {
        renderer:    $.jqplot.DateAxisRenderer,
        tickOptions: {formatString: '%b %#d'},
        numberTicks: 20
      },
      yaxis:  {
        label:       "hours",
        tickOptions: {formatter: durationFromMillis},
        min:         0,
        numberTicks: 10
      },
      y2axis: {
        label:       "issues",
        tickOptions: {formatString: "%i"},
        min:         0,
        padMin:      1
      }
    },
    cursor: {tooltipLocation: 'sw'}
  });
  drawPies();
}


function durationFromMillis(bo, millis) {
  return getMillisInHours(millis) + " h";
}


function drawPies() {
  if (parseInt("<%=ibs%>") > 0) {
    $.jqplot("statusPie", [<%=statusPie%>], {
      seriesColors: <%=statusColors%>,
      title:          "<%=I18n.get("ISSUE_STATUS")%>",
      legend:         {
        show:     true,
        rowSpacing:"0",
        location: "nw"
      },
      seriesDefaults: {renderer: $.jqplot.PieRenderer},
      cursor:         {show: false}
    });
  }


  if (parseInt("<%=ibg%>") > 0) {
    $.jqplot("gravityPie", [<%=gravityPie%>], {
      seriesColors: <%=gravityColors%>,
      title:          "<%=I18n.get("ISSUE_GRAVITY")%>",
      legend:         {
        show:     true,
        rowSpacing:"0",
        location: "nw"
      },
      seriesDefaults: {renderer: $.jqplot.PieRenderer},
      cursor:         {show: false}
    });
  }

  if (parseInt("<%=iba%>") > 0) {
    var tort = $.jqplot("assigneePie", [<%=assigneePie%>], {
      title:          "<%=I18n.get("ASSIGNEE")%>",
      legend:         {
        show:     true,
        rowSpacing:"0",
        location: "nw"
      },
      seriesDefaults: {renderer: $.jqplot.PieRenderer},
      cursor:         {show: false}
    });
    wrapLegend(tort);
  }

  if (parseInt("<%=ibgh%>") > 0) {
    $.jqplot("gravityPieHis", [<%=gravityPieHist%>], {
      seriesColors: <%=gravityColors%>,
      title:          "<%=I18n.get("ISSUE_GRAVITY_HISTORIC")%>",
      legend:         {
        show:     true,
        rowSpacing:"0",
        location: "nw"
      },
      seriesDefaults: {renderer: $.jqplot.PieRenderer},
      cursor:         {show: false}
    });
  }

  if (parseInt("<%=ibah%>") > 0){
    var tort = $.jqplot("assigneePieHis", [<%=assigneePieHist%>], {
      title:          "<%=I18n.get("ASSIGNEE_HISTORIC")%>",
      legend:         {
        show:     true,
        rowSpacing:"0",
        location: "nw"
      },
      seriesDefaults: {renderer: $.jqplot.PieRenderer},
      cursor:         {show: false}
    });

    wrapLegend(tort);
  }
}


function wrapLegend(tort) {
  var lege = tort.target.find("table");
  lege.css("position", "relative");
  lege.remove();
  tort.target.append(lege);
  lege.css({top: 0, left: 0});
  lege.wrap("<div style='height:190px;overflow:auto;position:absolute;top:28px;left:10px;'>");
}

</script>


   <%
  } else {
    %>
   <script>
     $(function(){
       var plot = $("#plot");
       plot.closest(".container").find("h2:first").html("<%=I18n.get("NO_ISSUE")%>");
       plot.remove();
     });
   </script>
   <%
  }

  } else {
    %><%=I18n.get("NEED_TIME__ON_TASK_TO_PRESENT_GRAPH")%><br><%
  }
%></div>
