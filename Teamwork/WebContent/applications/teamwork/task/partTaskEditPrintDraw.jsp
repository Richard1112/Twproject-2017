<%@ page import="com.opnlb.website.forum.ForumEntry, com.twproject.waf.html.TaskPrintDrawer, com.twproject.worklog.Worklog,
org.jblooming.agenda.CompanyCalendar, org.jblooming.designer.DesignerField, org.jblooming.oql.OqlQuery, org.jblooming.persistence.exceptions.FindException,
org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.core.JspIncluderSupport,
org.jblooming.waf.settings.I18n, org.jblooming.waf.view.ClientEntry, org.jblooming.waf.view.PageState, java.util.Iterator, java.util.List, java.util.Set, com.twproject.task.*, com.twproject.utilities.TeamworkComparators, org.jblooming.utilities.StringUtilities, org.jblooming.utilities.HtmlSanitizer"%><%@page pageEncoding="UTF-8"%>
<%!
  private void printRootAndChildren(ForumEntry forumEntry, int i, PageContext pageContext) throws FindException {

    JspHelper j = new JspHelper("/applications/teamwork/task/partTaskEditPrintForum.jsp");
    j.parameters.put("forum", forumEntry);
    j.parameters.put("depth", i);
    j.toHtml(pageContext);

    String hql = "from " + ForumEntry.class.getName() + " as fe where fe.parent=:parent order by fe.creationDate";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setEntity("parent", forumEntry);
    List<ForumEntry> fes = oql.list();
    for (ForumEntry fec : fes) {
      printRootAndChildren(fec, ++i, pageContext);
    }
  }
%>
<%
  TaskPrintDrawer tprint = (TaskPrintDrawer) JspIncluderSupport.getCurrentInstance(request);

  Task task = tprint.task;

  PageState pageState = PageState.getCurrentPageState(request);

  boolean canSeeCosts=tprint.canSeeCosts;

  boolean printASB = pageState.getEntry("PRINT_TASK_ASSIGS_SUBTASKS").checkFieldValue();
  ClientEntry printWorklogDetailCE = pageState.getEntry("PRINT_TASK_WORKLOG_DETAIL");
  boolean printWorklogDetail = printWorklogDetailCE.checkFieldValue();

  boolean printDiary = pageState.getEntry("PRINT_TASK_DIARY_DETAIL").checkFieldValue();
  boolean printLogs = pageState.getEntry("PRINT_TASK_LOGS").checkFieldValue();


%><br>

<table border="0" width="100%" align="center" cellpadding="5" cellspacing="0" style="<%=tprint.pageBreak ? "page-break-before: always;" : ""%>" class="table edged">
  <tr>
    <td colspan="3"><h2> <small>[<%=task.getId()%>]</small> <%=task.getDisplayName()%></h2> </td>
  </tr>
  <tr>
    <td><%=I18n.get("START")%>: <b><%=task.getSchedule() != null ? JSP.w(task.getSchedule().getStartDate()) : "-"%></b></td>
    <td><%=I18n.get("END")%>: <b><%=task.getSchedule() != null ? JSP.w(task.getSchedule().getEndDate()) : "-"%></b></td>
    <td valign="top"><%=I18n.get("TYPE")%>: <b><%=JSP.w(task.getType() != null ? task.getType().getDescription() : "-")%></b></td>
  </tr>
  <tr>
    <td><%=I18n.get("STATUS")%>: <b><%=I18n.get(task.getStatus())%></b></td>
    <td><%=I18n.get("RELEVANCE")%>: <b><%=JSP.w(task.getRelevance())%></b></td>
    <td><%=I18n.get("PROGRESS")%>: <b><%=JSP.perc(task.getProgress())%>%</b></td>
  </tr>
  <tr>
    <td valign="top" colspan="3" style="word-break: break-all"><%=I18n.get("TASK_DESCRIPTION")%>:<br><b><%=JSP.convertLineFeedToBR(JSP.w(task.getDescription()))%></b></td>
  </tr>
  <tr>
    <td valign="top" colspan="3"><%=I18n.get("TAGS")%>:<br><b><%=JSP.w(task.getTags())%></b></td>
  </tr>
  <tr>
    <td colspan="3"><%=I18n.get("NOTES_DELIVERABLES")%>:<br><b><%=JSP.convertLineFeedToBR(JSP.w(task.getNotes()))%></b></td>
  </tr>
  <tr><td colspan="99"><%
      for (int i=1; i<5; i++) {
        DesignerField dfStr = DesignerField.getCustomFieldInstance( "TASK_CUSTOM_FIELD_",i, task ,true,true, false, pageState);
        if (dfStr!=null){
          dfStr.separator="&nbsp;";
          dfStr.toHtml(pageContext);
    %>&nbsp;&nbsp;&nbsp;<%
        }
      }
    %></td></tr>
</table>



<%

  if (printDiary) {

    ForumEntry fe = task.getForumEntry();
    if (fe != null && fe.getChildrenSize() > 0) {
%><table border="0" width="100%" align="center" cellpadding="5" cellspacing="0">
  <tr>
    <td  colspan="5"><h4><%=I18n.get("TASK_TLOG_TAB","")%></h4></td>
  </tr><%

%><th class="tableHead"><%=I18n.get("FORUM_WHO")%></th><%
%><th class="tableHead"><%=I18n.get("FORUM_POST")%></th><%
%><th class="tableHead"><%=I18n.get("FORUM_ANSWERS")%></th><%
%><th class="tableHead"><%=I18n.get("FORUM_LASTPOST")%></th><%
%><th class="tableHead"><%=I18n.get("FORUM_LASTPOSTER")%></th><%



  String hql = "from " + ForumEntry.class.getName() + " as fe where fe.parent=:parent order by fe.creationDate";
  OqlQuery oql = new OqlQuery(hql);
  oql.getQuery().setEntity("parent", fe);
  List<ForumEntry> fes = oql.list();
  for (ForumEntry fec : fes) {
    printRootAndChildren(fec,0,pageContext);
  }
%></table><%
    }

  }

  if (printLogs) {
    Set<TaskStatusHistory> tsths = task.getStatusHistory();
    if (tsths.size() > 0) {
%><table border="0" width="100%" align="center" cellpadding="5" cellspacing="0"> <tr> <td><h4><%=I18n.get("STATUS_CHANGE_HISTORY")%></h4></td> </tr><%

%><tr>
  <th class="tableHead" valign="top"><%=I18n.get("STATUS_CHANGE_DATE")%></th>
  <th class="tableHead" valign="top"><%=I18n.get("STATUS_FROM")%></th>
  <th class="tableHead" valign="top"><%=I18n.get("STATUS_TO")%></th>
  <th class="tableHead" valign="top"><%=I18n.get("NOTES")%></th>
  <th class="tableHead" valign="top"><%=I18n.get("WHO_CHANGE")%></th>

</tr><%

  for (TaskStatusHistory tsh : tsths) {
%><tr>
  <td><%=JSP.w(tsh.getLastModified())%></td>
  <td align="center"><%=I18n.get(tsh.getFromStatus())%></td>
  <td align="center"><%=I18n.get(tsh.getToStatus())%></td>
  <td><%=JSP.w(tsh.getChangeLog())%></td>
  <td NOWRAP><%=tsh.getCreator()%></td>
</tr><%
  }

%></table><%

  }

  Set<TaskScheduleHistory> tshs = task.getScheduleHistory();
  if (tshs.size() > 0) {

%><table border="0" width="100%" align="center" cellpadding="5" cellspacing="0" class="table"><tr><td><h4><%=I18n.get("DATE_CHANGES")%></h4></td></tr><%

%><tr>
  <th class="tableHead" valign="top"><%=I18n.get("SCHEDULE_CHANGE_DATE")%></th>
  <th class="tableHead"  valign="top"><%=I18n.get("PREVIOUS_PERIOD")%></th>
  <th class="tableHead"  valign="top"><%=I18n.get("NOTES")%></th>
  <th class="tableHead"  valign="top"><%=I18n.get("WHO_CHANGE")%></th>
</tr><%

  for (TaskScheduleHistory tsh : tshs) {
%><tr>
  <td><%=JSP.w(tsh.getLastModified())%></td>
  <td><%=tsh.getSchedule()!=null ? JSP.w(tsh.getSchedule().getStartDate()) + "-" + JSP.w(tsh.getSchedule().getEndDate()) : ""%></td>
  <td><%=JSP.w(tsh.getChangeLog())%></td>
  <td NOWRAP><%=tsh.getCreator()%></td>
</tr><%
  }

%></table><%
    }

  }

  long totalEstimOnAssigs = 0;

  if (printASB && task.getAssignementsSize()>0) {

%><br>
<table border="0" width="100%" align="center" cellpadding="5" cellspacing="0" class="table dataTable">
  <tr>
    <th class="tableHead"><%=I18n.get("ASSIGNMENTS")%></th>
    <th class="tableHead"><%=I18n.get("WORKLOG_ESTIMATED_SHORT")%></th>
    <th class="tableHead"><%=I18n.get("WORKLOG_ESTIMATED_ISSUES")%></th>
    <th class="tableHead"><%=I18n.get("WORKLOG_DONE_SHORT")%></th>
    <th class="tableHead"><%=I18n.get("PLAN_BY_RESOURCE")%></th>

    <%if (canSeeCosts){%>
    <th class="tableHead"><%=I18n.get("HOURLY_COST_LONG")%></th>
    <%}%>
  </tr>
  <%
    List<Assignment> chA = task.getAssignementsSortedByRole();
    for (Assignment assig :chA) {
      totalEstimOnAssigs = totalEstimOnAssigs + assig.getEstimatedWorklog();

  %><tr class="alternate">
  <td valign="top" nowrap><%="<b>"+(assig.getResource().getDisplayName()) + "</b> " + I18n.get("ASSIG_AS") + " " + (assig.getRole().getName())%></td>
  <%--<td style="border-bottom:1px dotted #999999;" valign="top" nowrap><%=I18n.get(assig.getActivity())%></td>--%>
  <td align="right" valign="top" nowrap><%=DateUtilities.getMillisInHoursMinutes(assig.getEstimatedWorklog())%></td>


  <td align="right" valign="top"><%=DateUtilities.getMillisInHoursMinutes(assig.getWorklogEstimatedFromIssues())%></td>

  <td align="right" valign="top"><%=DateUtilities.getMillisInHoursMinutes(assig.getWorklogDone())%></td>
  <td align="right" valign="top"><%=DateUtilities.getMillisInHoursMinutes(assig.getWorklogPlanned(new CompanyCalendar().setAndGetTimeToDayStart()))%></td>

  <%if (canSeeCosts){%>
  <td align="right" valign="top"><%=JSP.currency(assig.getHourlyCost())%></td>
  <%}%>


  <%
    if (printWorklogDetail) {
      List<Worklog> wkls = assig.getWorklogs();
      if (JSP.ex(wkls)){
  %></tr><tr><td>&nbsp;</td><td colspan="5" align="left">
  <table width="100%" style="background-color: #EEEEEE">
    <tr>
      <td width="10%"><b><%=I18n.get("WORKLOG_DATE")%></b></td>
      <td align="center"  width="10%"><b><%=I18n.get("WORKLOG_DURATION")%></b></td>
      <td><b><%=I18n.get("WORKLOG_ACTION")%></b></td>
    </tr>
    <%

      for (Worklog workLog : wkls) {

    %><tr>
    <td nowrap valign="top"><%=DateUtilities.dateToString(workLog.getInserted())%></td>
    <td align="center" valign="top"><%=DateUtilities.getMillisInHoursMinutes(workLog.getDuration())%></td>
    <td valign="top"><%=JSP.w(workLog.getAction())%></td>
  </tr><%

    } %></table><%
    }
  }
%>

</td>
</tr><%
  }

%></table>
<br>

<%
  if (task.getChildrenSize()>0){
%>

<table border="0" width="100%" align="center" cellpadding="5" cellspacing="0" class="table dataTable">
  <tr>
    <td><h3><%=I18n.get("SUBTASKS")%></h3></td>
  </tr><%

  Iterator chl = task.getChildrenIterator(new TeamworkComparators.TaskManualOrderComparator());
  while (chl.hasNext()) {
    Task taskChild = (Task) chl.next();
%><tr class="alternate">
  <td><%=taskChild.getDisplayName()%></td>
  <td><b><%=I18n.get(taskChild.getStatus())%></b></td>
  <td><%=I18n.get("PROGRESS")%>: <b><%=JSP.perc(taskChild.getProgress())%>%</b></td>
  <td><%=I18n.get("ASSIGNMENTS")%>: <b><%=JSP.w(taskChild.getAssignementsSize())%></b></td>
</tr><%
  }
%></table><%
  }

  long worklogDone = task.getWorklogDone();

%><br>
<table border="0" width="100%" align="center" cellpadding="5" cellspacing="0"><tr class="totals">
  <td align="right" width="50%"><%=I18n.get("TOTAL_WORKLOG_ESTIMATED")%>:&nbsp;<%=DateUtilities.getMillisInHoursMinutes(totalEstimOnAssigs)%></td>
  <td align="right"><%=I18n.get("TOTAL_WORKLOG_DONE_HM")%>:&nbsp;<%=DateUtilities.getMillisInHoursMinutes(worklogDone)%></td>
</tr></table><hr><%


    tprint.totalWorklogDone=tprint.totalWorklogDone+worklogDone;
    tprint.totalEstimatedWorklog=tprint.totalEstimatedWorklog+totalEstimOnAssigs;
  }

  if (tprint.recurseOnChildren) {
    Iterator chl = task.getChildrenIterator(new TeamworkComparators.TaskManualOrderComparator());
    while (chl.hasNext()) {
      Task taskChild = (Task) chl.next();
      TaskPrintDrawer tpd = new TaskPrintDrawer(taskChild);
      tpd.canSeeCosts=canSeeCosts;
      tpd.toHtml(pageContext);
      tprint.totalEstimatedWorklog=tprint.totalEstimatedWorklog+tpd.totalEstimatedWorklog;
      tprint.totalWorklogDone=tprint.totalWorklogDone+tpd.totalWorklogDone;

    }
  }
%>