<%@ page import="com.twproject.resource.Person,com.twproject.security.TeamworkPermissions,com.twproject.task.Task,com.twproject.task.TaskBricks, org.jblooming.agenda.CompanyCalendar, org.jblooming.agenda.Period, org.jblooming.oql.OqlQuery,
                 org.jblooming.utilities.JSP, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.display.PercentileDisplay, org.jblooming.waf.html.input.SmartCombo, org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.Date,
                 java.util.HashSet, java.util.Set"
        %><div id="wpPanicDiv" class="portletBox"><%

  PageState pageState = PageState.getCurrentPageState(request);


  String panicTaskId=pageState.getEntryOrDefault("PANIC_TASK").stringValueNullIfEmpty();
  Task task =null;
  if (JSP.ex(panicTaskId))
    task =Task.load(panicTaskId);

  PageSeed self = pageState.pagePart(request);
  Form f = new Form(self);
    pageState.setForm(f);
    f.start(pageContext);

  String bsa = " $(\"#cntdwn\").stopTime(\"ctd\"); " + ButtonSubmit.getAjaxButton(f, "wpPanicDiv").generateJs().toString();

  //if (task==null){
  %><h1><%=I18n.get("PANIC_BOARD")%></h1><%
   // }

  %><div style="display: inline-block; vertical-align: top;"><%
  if (task!=null){
%><div><%
  task.bricks.getStatusIcon(15, pageState).toHtml(pageContext);
%>&nbsp;<small title="<%=I18n.get("REFERENCE_CODE")%>"><%=task.isNew()?"":"T#"+task.getMnemonicCode()+"#&nbsp;&nbsp;"%></small></div><%
  } else {
    %>&nbsp;<%
  }

  %><%

  SmartCombo taskCombo = TaskBricks.getTaskCombo("PANIC_TASK", true, TeamworkPermissions.task_canRead, pageState);
  taskCombo.separator="";
  taskCombo.label="";
  taskCombo.innerLabel=I18n.get("EDITOR_CHOOSE");
  taskCombo.onValueSelectedScript=bsa;
  taskCombo.fieldClass="formElements formElementsBig smartCombo";
  taskCombo.script="style=width:300px";
  taskCombo.toHtml(pageContext);

%></div>

<%
  if (task==null){
%><div style="display: inline-block; padding: 25px 0 0 30px"><span class="descrEl"><%=I18n.get("CHOOSE_YOUR_PANIC_TASK")%></span></div> <%
    }


  if (task!=null) {
    long now= System.currentTimeMillis();
    //day to start
    String timeLbl="";
    String timeClass="";
    if (now<task.getSchedule().getStartDate().getTime()){
      timeLbl=I18n.get("TIME_TO_START");

    //day to end
    } else if (now<task.getSchedule().getEndDate().getTime()){
      timeLbl=I18n.get("TIME_TO_END");

    //task expired
    } else {
      timeLbl=I18n.get("EXPIRED_BY");
      timeClass="warning";

    }
    %>
<div style="display: inline-block;height: 120px">
  <h3 style="margin: 25px 0 30px 30px; display: inline-block; vertical-align: top;" class="<%=timeClass%>"><%=timeLbl%></h3>
  <span id="cntdwn" style="display: inline-block; margin: 10px;"></span>
</div>


<table cellspacing="3" cellpadding="1" width="100%"> <%

  %><tr><%
  long twe = task.getTotalWorklogEstimated();
  long twd = task.getTotalWorklogDone();

  double perc = twe==0 ? 0 : (twd/twe)*100;
  PercentileDisplay pd = new PercentileDisplay(perc);
  pd.percentileColor="#ffff00";
  pd.fractionDigits=0;
  pd.height="15px";
  pd.toolTip = twd+"/"+twe;

  %><td align="center" style="background-color:#f0f0f0;"><%=I18n.get("WORK_DONE_OVER_ESTIMATED")%><br><%pd.toHtml(pageContext);%></td><%

  Date startDate = null;
  double timeElapsed = 0;
  if (task.getSchedule()!=null && task.getSchedule().getStartDate()!=null && task.getSchedule().getStartDate().getTime()<System.currentTimeMillis()) {
    startDate = task.getSchedule().getStartDate();
    timeElapsed = CompanyCalendar.getWorkingDaysCountInPeriod(new Period(startDate,new Date()));
    perc = Math.min((timeElapsed/task.getDuration())*100,100);
  }
  pd = new PercentileDisplay(perc);
  pd.fractionDigits=0;
  pd.height="15px";
  pd.toolTip = timeElapsed+"/"+task.getDuration();

  %><td align="center" style="background-color:#f0f0f0;"><%=I18n.get("TIME_ELAPSED")%><br><%pd.toHtml(pageContext);%></td><%


  %><td align="center" style="background-color:#f0f0f0;"><%=I18n.get("TASK_CLOSED_OVER_TOTAL")%><br><%
  double ds = task.getDescendantsSize(Task.class);
  double closedDescs = 0;
  if (ds>0) {
    closedDescs = task.getTotalDescendantsSize().second;
    perc = (closedDescs/ds)*100;
  } else
    perc = 0;

  pd = new PercentileDisplay(perc);
  pd.height="15px";
  pd.fractionDigits=0;

  pd.toolTip = closedDescs+"/"+ds;
  pd.toHtml(pageContext);
  %></td><%

  %></tr><tr><%

  Set<Person> persons = task.getPersons();

  Set<Person> enabledPersonsThatCanLogin = new HashSet();
  for (Person person : persons) {
    if (person.getMyself()!=null && person.getMyself().isEnabled())
       enabledPersonsThatCanLogin.add(person);
  }
  double totalTeam = enabledPersonsThatCanLogin.size();
  String hql = "select count(p.id) from " + Person.class.getName() + " as p where p.myself is not null and p.myself.enabled=:truth";
  OqlQuery oql = new OqlQuery(hql);
  oql.getQuery().setBoolean("truth", Boolean.TRUE);
  double totalLoggable = (Long) oql.uniqueResult();
  perc = (totalTeam / totalLoggable) * 100;
  pd = new PercentileDisplay(perc);
  pd.height = "15px";
  pd.fractionDigits = 0;

  pd.toolTip = totalTeam + "/" + totalLoggable;

%><td align="center" style="background-color:#f0f0f0;"><%=I18n.get("TEAM_OVER_USERS")%><br><%pd.toHtml(pageContext);%></td><%


  double totalIssues = task.getTotalIssues();
  double totalOpenIssues = task.getTotalIssuesOpen();
  perc = totalIssues==0 ? 0 :((totalIssues-totalOpenIssues)/totalIssues)*100;
  pd = new PercentileDisplay(perc);
  pd.height="15px";
  pd.fractionDigits=0;

  pd.toolTip = (totalIssues-totalOpenIssues)+"/"+totalIssues;

  %><td align="center" style="background-color:#f0f0f0;"><%=I18n.get("ISSUE_CLOSED_OVER_OPEN")%><br><%pd.toHtml(pageContext);%></td><%


  double totOpenBlockIssues = task.getTotalIssuesScoreOpen();

  double totBlockIssues = task.getTotalIssuesScoreClosed()+task.getTotalIssuesScoreOpen();

  if (totOpenBlockIssues<=0)
    perc = 0d;
  else
    perc = ((totBlockIssues-totOpenBlockIssues)/totBlockIssues)*100;
  pd = new PercentileDisplay(perc);
  pd.height="15px";
  pd.fractionDigits=0;

  pd.toolTip = totBlockIssues +"/"+totOpenBlockIssues;


  %><td align="center" style="background-color:#f0f0f0;"><%=I18n.get("ISSUE_BLOCK_OVER_OPEN")%><br><%pd.toHtml(pageContext);%></td>
   </tr>
  </table>


<style type="text/css">

  .panicNumberWrapper {
    display: inline-block;
    margin-right:10px;
  }
  .panicNumber {
    font-size: 25px;
    font-weight: 600;
    display: block;
    border-radius:3px;
    text-align: center;
    padding: 10px 5px;
    width: 55px;

    /* Permalink - use to edit and share this gradient: http://colorzilla.com/gradient-editor/#e8e8e8+0,f1f1f1+50,e1e1e1+51,f6f6f6+100 */
    background: #e8e8e8; /* Old browsers */
    background: -moz-linear-gradient(top,  #e8e8e8 0%, #f1f1f1 50%, #e1e1e1 51%, #f6f6f6 100%); /* FF3.6-15 */
    background: -webkit-linear-gradient(top,  #e8e8e8 0%,#f1f1f1 50%,#e1e1e1 51%,#f6f6f6 100%); /* Chrome10-25,Safari5.1-6 */
    background: linear-gradient(to bottom,  #e8e8e8 0%,#f1f1f1 50%,#e1e1e1 51%,#f6f6f6 100%); /* W3C, IE10+, FF16+, Chrome26+, Opera12+, Safari7+ */
    filter: progid:DXImageTransform.Microsoft.gradient( startColorstr='#e8e8e8', endColorstr='#f6f6f6',GradientType=0 ); /* IE6-9 */


  }

  label {
    display: block;
    text-align: center;
  }


</style>

    <script type="text/javascript">
      $(document).ready(function () {
        $("#cntdwn").everyTime(1000,"ctd",drawTime);
      });

      function drawTime(){
        var taskStart=<%=task.getSchedule().getStartDate().getTime()%>;
        var taskEnd=<%=task.getSchedule().getEndDate().getTime()%>;
        var now= new Date().getTime();
        var diff=0;
        if (now<taskStart)
          diff=taskStart-now;
        else if (now<taskEnd)
          diff=taskEnd-now;
        else
          diff=now-taskEnd;

        $("#cntdwn").html(getMillisInDaysHoursMinutesSeconds(diff));

      }

      function getMillisInDaysHoursMinutesSeconds(millis) {
          var millisInDay =<%=CompanyCalendar.MILLIS_IN_DAY%>;

          var sgn=millis>=0?1:-1;
          millis=Math.abs(millis);
          var days = Math.floor(millis / millisInDay);
          var hour = Math.floor((millis % millisInDay) / 3600000);
          var min = Math.floor((millis-days*millisInDay-hour*3600000) / 60000);
          var sec = Math.floor((millis-days*millisInDay-hour*3600000-min*60000) / 1000);
          return (days > 0 ? "<div class='panicNumberWrapper'><span class='panicNumber'>"+days+"</span><label class='textSmall'><%=I18n.get("DAYS")%></label></div>" : "") +
                 ( hour>0 ?  "<div class='panicNumberWrapper'><span class='panicNumber'>"+pad(hour,2,"0")+"</span><label class='textSmall'><%=I18n.get("HOURS")%></label></div> ":"")+
                 "<div class='panicNumberWrapper'><span class='panicNumber'>"+ pad(min,2,"0")+"</span><label class='textSmall'><%=I18n.get("MINUTES")%></label></div> "+
                 "<div class='panicNumberWrapper'><span class='panicNumber'>"+pad(sec,2,"0")+"</span><label class='textSmall'><%=I18n.get("SECONDS")%></label></div>";

        /*return (days > 0 ? "<%//=I18n.get("DAYS")%>: <span class='warning' style='font-weight: 700'>"+days+"</span> " : "") +
            ( hour>0 ?  "<%//=I18n.get("HOURS")%>: <span class='warning' style='font-weight: 700'>"+pad(hour,2,"0")+"</span> ":"")+
            "<%//=I18n.get("MINUTES")%>: <span class='warning' style='font-weight: 700'>" + pad(min,2,"0")+"</span> "+
            "<//%=I18n.get("SECONDS")%>: <span class='warning' style='font-weight: 700'>"+pad(sec,2,"0")+"</span>";*/
      }


    </script>


    <%
  }


  f.end(pageContext);

%></div>

