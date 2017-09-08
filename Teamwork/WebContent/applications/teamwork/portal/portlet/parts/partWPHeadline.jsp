<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.security.TeamworkPermissions, com.twproject.task.*, org.jblooming.agenda.CompanyCalendar, org.jblooming.oql.QueryHelper, org.jblooming.security.Permission, org.jblooming.utilities.JSP, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.HashSet, java.util.Set, java.util.TreeMap, org.jblooming.agenda.Period, java.util.Date, com.twproject.resource.Person, org.jblooming.waf.settings.ApplicationState, org.jblooming.utilities.DateUtilities, org.apache.commons.httpclient.util.DateUtil" %>
<%

  //ATTENZIONE A NON METTERE TROPPI SPAZI SE NON DISEGNA

  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  PageSeed issueList = pageState.pageFromRoot("issue/issueList.jsp");
  issueList.command = Commands.FIND;

  CompanyCalendar cc = new CompanyCalendar();

  //-------------------------- MILESTONES OVERDUE -------------------------------------
  if ("wphMilesOver".equals(pageState.command)) {
    //ricerca task su cui sei assegnato o tutti se hai i diritti in cui:
    //   lo stato è aperto e end è passata e end è milestone
    //   lo stato è sospeso e start è passato e start è milestone
    String hql = "select count(distinct task.id) from " + Task.class.getName() + " as task";
    QueryHelper qhelp = new QueryHelper(hql);
    //  security
    TaskBricks.addSecurityReadClauses(qhelp, pageState);
    qhelp.addOQLClause("task.schedule is not null");
    qhelp.addQueryClause("( task.status='" + TaskStatus.STATUS_SUSPENDED + "' and task.startIsMilestone=true and task.schedule.start<=:now");
    qhelp.addOrQueryClause("(task.status='" + TaskStatus.STATUS_SUSPENDED + "' or task.status='" + TaskStatus.STATUS_ACTIVE + "') and task.endIsMilestone=true and task.schedule.end<=:now )");
    qhelp.addParameter("now", cc.setAndGetTimeToDayStart());

    PageSeed taskList = pageState.pageFromRoot("task/taskList.jsp");
    taskList.command = Commands.FIND;
    taskList.addClientEntry("MILES_OVER", Fields.TRUE);

    long count = (Long) qhelp.toHql().uniqueResult();
    if (count > 0) {
%><a href="<%=taskList.toLinkToHref()%>">
  <h2><%=I18n.get("MILESTONES_OVERDUE")%></h2>
  <span class="wphlNumber"><%=count%></span>
  <p><%=I18n.get("MILESTONES_OVERDUE_DESCR")%></p>
</a><%
  }


  //-------------------------- BUDGET OVERFLOW -------------------------------------
} else if ("wphProjectBudgetOverflow".equals(pageState.command)) {

  String hql = "select count(distinct task.id) from " + Task.class.getName() + " as task";
  QueryHelper qhelp = new QueryHelper(hql);

  //security
  Set<Permission> perms = new HashSet();
  perms.add(TeamworkPermissions.task_cost_canRead);
  TaskBricks.addSecurityClauses(qhelp, perms, pageState);

  //qhelp.addOQLClause("task.parent is null");
  qhelp.addOQLClause("task.status='" + TaskStatus.STATUS_ACTIVE + "'");
  qhelp.addOQLClause("task.forecasted>0");
  qhelp.addOQLClause("task.forecasted<task.totalCostsDone");

  PageSeed taskList = pageState.pageFromRoot("task/taskList.jsp");
  taskList.command = Commands.FIND;
  //taskList.addClientEntry("ROOT_OR_STANDALONE", Fields.TRUE);
  taskList.addClientEntry("BUDGET_OVERFLOW", Fields.TRUE);
  taskList.addClientEntry("STATUS", TaskStatus.STATUS_ACTIVE);

  long count = (Long) qhelp.toHql().uniqueResult();
  if (count > 0) {
%><a href="<%=taskList.toLinkToHref()%>">
  <h2><%=I18n.get("BUDGET_OVERFLOW")%></h2>
  <span class="wphlNumber"><%=count%></span>
  <p><%=I18n.get("BUDGET_OVERFLOW_DESCR")%></p>
</a>
<%
  }


  //-------------------------- TASK OVER -------------------------------------
} else if ("wphTasksOver".equals(pageState.command)) {

  String hql = "select count(distinct task.id) from " + Task.class.getName() + " as task";
  QueryHelper qhelp = new QueryHelper(hql);
  //  security
  TaskBricks.addSecurityReadClauses(qhelp, pageState);

  qhelp.addOQLClause("task.schedule is not null");
  qhelp.addOQLClause("task.status='" + TaskStatus.STATUS_ACTIVE + "'");
  qhelp.addQueryClause("task.schedule.end<:now");
  qhelp.addParameter("now", cc.setAndGetTimeToDayStart());

  PageSeed taskList = pageState.pageFromRoot("task/taskList.jsp");
  taskList.command = Commands.FIND;
  taskList.addClientEntry("END", "<t");
  taskList.addClientEntry("STATUS", TaskStatus.STATUS_ACTIVE);

  long count = (Long) qhelp.toHql().uniqueResult();
  if (count > 0) {
%><a href="<%=taskList.toLinkToHref()%>">
  <h2><%=I18n.get("OVERDUE_TASK")%></h2>
  <span class="wphlNumber"><%=count%></span>
  <p><%=I18n.get("OVERDUE_TASK_DESCR")%></p>
</a><%
  }

  //-------------------------- ISSUES OVER -------------------------------------
} else if ("wphIssuesOver".equals(pageState.command)) {
  String hql = "select count(distinct issue.id) from " + Issue.class.getName() + " as issue";
  QueryHelper qhelp = new QueryHelper(hql);
  //  security
  IssueBricks.addSecurityClauses(qhelp, pageState);
  qhelp.addJoinAlias("join issue.task as task");
  qhelp.addJoinAlias("left outer join issue.assignedTo as resource");


  qhelp.addOQLClause("task.status='" + TaskStatus.STATUS_ACTIVE + "'");
  qhelp.addQueryClause("issue.status.behavesAsOpen=true");
  qhelp.addQueryClause("issue.shouldCloseBy<:now");
  qhelp.addParameter("now", cc.setAndGetTimeToDayStart());

  IssueBricks.addOpenStatusFilter(issueList);
  issueList.addClientEntry("FLT_ISSUE_DATE_CLOSE_BY", "<" + JSP.w(cc.getTime()));
  issueList.addClientEntry("FLT_ISSUE_TASK_STATUS", TaskStatus.STATUS_ACTIVE);


  long count = (Long) qhelp.toHql().uniqueResult();
  if (count > 0) {
%><a href="<%=issueList.toLinkToHref()%>">
  <h2><%=I18n.get("EXPIRED_ISSUES")%></h2>
  <span class="wphlNumber"><%=count%></span>
  <p><%=I18n.get("EXPIRED_ISSUES_DESCR")%></p>
</a><%
  }

  //-------------------------- FORTHCOMING MILESTONES -------------------------------------
} else if ("wphMilesForthcoming".equals(pageState.command)) {
  String hql = "select count(distinct task.id) from " + Task.class.getName() + " as task";
  QueryHelper qhelp = new QueryHelper(hql);
  //  security
  TaskBricks.addSecurityReadClauses(qhelp, pageState);
  qhelp.addOQLClause("task.status in ('" + TaskStatus.STATUS_ACTIVE + "','" + TaskStatus.STATUS_SUSPENDED + "')");
  qhelp.addOQLClause("task.schedule is not null");
  qhelp.addOQLClause("( task.startIsMilestone=true and task.schedule.start between :now and :nextWeek");
  qhelp.addOrQueryClause("task.endIsMilestone=true and task.schedule.end between :now and :nextWeek )");
  qhelp.addParameter("now", cc.setAndGetTimeToDayStart());
  cc.add(CompanyCalendar.DATE, 7);
  qhelp.addParameter("nextWeek", cc.setAndGetTimeToDayEnd());

  PageSeed taskList = pageState.pageFromRoot("task/taskList.jsp");
  taskList.command = Commands.FIND;
  taskList.addClientEntry("MILESTONE", "T:1w");
  taskList.addClientEntry("STATUS", TaskStatus.STATUS_ACTIVE + "," + TaskStatus.STATUS_SUSPENDED);

  long count = (Long) qhelp.toHql().uniqueResult();
  if (count > 0) {
%><a href="<%=taskList.toLinkToHref()%>">
  <h2><%=I18n.get("FORTHCOMING_MILESTONES")%></h2>
  <span class="wphlNumber"><%=count%></span>
  <p><%=I18n.get("FORTHCOMING_MILESTONES_DESCR")%></p>
</a><%
  }


  //-------------------------- FORTHCOMING END -------------------------------------
} else if ("wphTasksForthcoming".equals(pageState.command)) {
  String hql = "select count(distinct task.id) from " + Task.class.getName() + " as task";
  QueryHelper qhelp = new QueryHelper(hql);
  //  security
  TaskBricks.addSecurityReadClauses(qhelp, pageState);
  qhelp.addOQLClause("task.schedule is not null");
  qhelp.addOQLClause("task.schedule.end between :now and :nextWeek");
  qhelp.addOQLClause("task.status='" + TaskStatus.STATUS_ACTIVE + "'");

  qhelp.addParameter("now", cc.setAndGetTimeToDayStart());
  cc.add(CompanyCalendar.DATE, 7);
  qhelp.addParameter("nextWeek", cc.setAndGetTimeToDayEnd());

  PageSeed taskList = pageState.pageFromRoot("task/taskList.jsp");
  taskList.command = Commands.FIND;
  taskList.addClientEntry("END", "T:1w");
  taskList.addClientEntry("STATUS", TaskStatus.STATUS_ACTIVE);

  long count = (Long) qhelp.toHql().uniqueResult();
  if (count > 0) {
%><a href="<%=taskList.toLinkToHref()%>">
  <h2><%=I18n.get("FORTHCOMING_ENDS")%></h2>
  <span class="wphlNumber"><%=count%></span>
  <p><%=I18n.get("FORTHCOMING_ENDS_DESCR")%></p>
</a><%
  }


  //-------------------------- ISSUES ABOUT TO CLOSE -------------------------------------
} else if ("wphIssuesForthcoming".equals(pageState.command)) {
  String hql = "select count(distinct issue.id) from " + Issue.class.getName() + " as issue";

  QueryHelper qhelp = new QueryHelper(hql);
  //  security
  IssueBricks.addSecurityClauses(qhelp, pageState);
  qhelp.addJoinAlias("join issue.task as task");
  qhelp.addJoinAlias("left outer join issue.assignedTo as resource");


  qhelp.addOQLClause("task.status='" + TaskStatus.STATUS_ACTIVE + "'");
  qhelp.addQueryClause("issue.status.behavesAsOpen=true");
  qhelp.addQueryClause("issue.shouldCloseBy between :now and :nextWeek");

  qhelp.addParameter("now", cc.setAndGetTimeToDayStart());
  cc.add(CompanyCalendar.DATE, 7);
  qhelp.addParameter("nextWeek", cc.setAndGetTimeToDayEnd());

  IssueBricks.addOpenStatusFilter(issueList);
  issueList.addClientEntry("FLT_ISSUE_DATE_CLOSE_BY", "t:1w");
  issueList.addClientEntry("FLT_ISSUE_TASK_STATUS", TaskStatus.STATUS_ACTIVE);


  long count = (Long) qhelp.toHql().uniqueResult();
  if (count > 0) {
%><a href="<%=issueList.toLinkToHref()%>">
  <h2><%=I18n.get("FORTHCOMING_ISSUES")%></h2>
  <span class="wphlNumber"><%=count%></span>
  <p><%=I18n.get("FORTHCOMING_ISSUES_DESCR")%></p>
</a><%
  }


  //-------------------------- OPEN PROJECT -------------------------------------
} else if ("wphOpenProjects".equals(pageState.command)) {
  String hql = "select count(distinct task.id) from " + Task.class.getName() + " as task";
  QueryHelper qhelp = new QueryHelper(hql);
  //  security
  TaskBricks.addSecurityReadClauses(qhelp, pageState);
  qhelp.addOQLClause("task.parent is null");
  qhelp.addOQLClause("task.status='" + TaskStatus.STATUS_ACTIVE + "'");

  PageSeed taskList = pageState.pageFromRoot("task/taskList.jsp");
  taskList.command = Commands.FIND;
  taskList.addClientEntry("STATUS", TaskStatus.STATUS_ACTIVE);
  taskList.addClientEntry("ROOT_OR_STANDALONE", Fields.TRUE);

  long count = (Long) qhelp.toHql().uniqueResult();
  if (count > 0) {
%><a href="<%=taskList.toLinkToHref()%>">
  <h2><%=I18n.get("OPEN_PROJECTS")%></h2>
  <span class="wphlNumber"><%=count%></span>
  <p><%=I18n.get("OPEN_PROJECTS_DESCR")%></p>
</a><%
  }


  //-------------------------- WORKLOG -------------------------------------
} else if ("wphWorklog".equals(pageState.command)) {

  Date end = cc.setAndGetTimeToDayEnd();
  cc.add(CompanyCalendar.DATE, -6);
  Date start = cc.setAndGetTimeToDayStart();
  Period period = new Period(start, end);

  Person person = logged.getPerson();
  TreeMap<Integer, Long> workablePlan = person.getWorkablePlan(period); //for each day the workable millis for that day. If day is holidays -1  if is completely unaivailable millis are -2
  TreeMap<Integer, Long> workedPlan = person.getWorkedPlan(period); //for each day the worked millis for that day.

  PageSeed worlogDayForResource = pageState.pageFromRoot("/task/worklog/worklogWeek.jsp");
  worlogDayForResource.addClientEntry("FOCUS_MILLIS", start.getTime()); // set giorno giusto


  long totEstim=0;
  long totWorked=0;
  for (int day:workablePlan.keySet()) {
    long dayCapacity = workablePlan.get(day);
    Long wL = workedPlan.get(day);
    long worked = wL == null ? 0 : wL;
    totEstim+=dayCapacity>0?dayCapacity:0;
    totWorked+=worked;
  }


  if (totEstim > totWorked &&  totWorked>0) {
%><a href="<%=worlogDayForResource.toLinkToHref()%>">
  <h2><%=I18n.get("WORKLOG")%></h2>
  <span class="wphlNumber" style="font-size: 24px"><%=DateUtilities.getMillisInHoursMinutes(totWorked)%> / <%=DateUtilities.getMillisInHoursMinutes(totEstim)%></span>
  <p><%=I18n.get("WORKLOG_DESCR")%></p>
</a><%
    }



  //-------------------------- PROJECT WITH ACTIVE DISCUSSIONS -------------------------------------
} else if ("wphTaskDiscussions".equals(pageState.command)) {
  String hql = "select count(distinct task.id) from " + Task.class.getName() + " as task";
  QueryHelper qhelp = new QueryHelper(hql);
  //  security
  TaskBricks.addSecurityReadClauses(qhelp, pageState);
  //qhelp.addOQLClause("task.parent is null");
  //qhelp.addOQLClause("task.status='" + TaskStatus.STATUS_ACTIVE + "'");
  qhelp.addOQLClause("task.forumEntry.lastPostOnBranch>:lastWeek");


  cc.add(CompanyCalendar.DATE, -7);
  qhelp.addParameter("lastWeek", cc.setAndGetTimeToDayStart());


  PageSeed taskList = pageState.pageFromRoot("task/taskList.jsp");
  taskList.command = Commands.FIND;
  taskList.addClientEntry("LAST_POST", ">-2w");
  //taskList.addClientEntry("ROOT_OR_STANDALONE", Fields.TRUE);

  long count = (Long) qhelp.toHql().uniqueResult();
  if (count > 0) {
%><a href="<%=taskList.toLinkToHref()%>">
  <h2><%=I18n.get("PROJECTS_WITH_DISCUSSIONS")%></h2>
  <span class="wphlNumber"><%=count%></span>
  <p><%=I18n.get("PROJECTS_WITH_DISCUSSIONS_DESCR")%></p>
</a><%
    }

  //-------------------------- PROJECT CREATED IN LAST 7 DAYS -------------------------------------
} else if ("wphProjectCreated".equals(pageState.command)) {
  String hql = "select count(distinct task.id) from " + Task.class.getName() + " as task";
  QueryHelper qhelp = new QueryHelper(hql);
  //  security
  TaskBricks.addSecurityReadClauses(qhelp, pageState);
  qhelp.addOQLClause("task.parent is null");
  qhelp.addOQLClause("task.creationDate>:lastWeek");

  cc.add(CompanyCalendar.DATE, -7);
  qhelp.addParameter("lastWeek", cc.setAndGetTimeToDayEnd());


  PageSeed taskList = pageState.pageFromRoot("task/taskList.jsp");
  taskList.command = Commands.FIND;
  taskList.addClientEntry("CREATED_ON", ">-1w");
  taskList.addClientEntry("ROOT_OR_STANDALONE", Fields.TRUE);

  long count = (Long) qhelp.toHql().uniqueResult();
  if (count > 0) {
%><a href="<%=taskList.toLinkToHref()%>">
  <h2><%=I18n.get("PROJECTS_RECENTLY_CREATED")%></h2>
  <span class="wphlNumber"><%=count%></span>
  <p><%=I18n.get("PROJECTS_RECENTLY_CREATED_DESCR")%></p>
</a><%
    }


  //-------------------------- PROJECT CLOSED IN LAST 7 DAYS -------------------------------------
} else if ("wphProjectClosed".equals(pageState.command)) {
  String hql = "select count(distinct task.id) from " + Task.class.getName() + " as task";
  QueryHelper qhelp = new QueryHelper(hql);
  qhelp.addJoinAlias("join task.statusHistory as hist");

  //  security
  TaskBricks.addSecurityReadClauses(qhelp, pageState);
  qhelp.addOQLClause("task.parent is null");
  qhelp.addOQLClause("hist.creationDate>:lastWeek");
  qhelp.addOQLClause("task.status='" + TaskStatus.STATUS_DONE + "' or task.status='"+ TaskStatus.STATUS_FAILED + "'");


  cc.add(CompanyCalendar.DATE, -7);
  qhelp.addParameter("lastWeek", cc.setAndGetTimeToDayStart());

  PageSeed taskList = pageState.pageFromRoot("task/taskList.jsp");
  taskList.command = Commands.FIND;
  taskList.addClientEntry("STATUS_CHANGE_DATE", ">-1w");
  taskList.addClientEntry("ROOT_OR_STANDALONE", Fields.TRUE);
  taskList.addClientEntry("STATUS", TaskStatus.STATUS_DONE+","+TaskStatus.STATUS_FAILED);

  long count = (Long) qhelp.toHql().uniqueResult();
  if (count > 0) {
%><a href="<%=taskList.toLinkToHref()%>">
  <h2><%=I18n.get("PROJECTS_RECENTLY_CLOSED")%></h2>
  <span class="wphlNumber"><%=count%></span>
  <p><%=I18n.get("PROJECTS_RECENTLY_CLOSED_DESCR")%></p>
</a><%
    }

  //-------------------------- ASSIGNMENT CREATED IN LAST 7 DAYS -------------------------------------
} else if ("wphAssigCreated".equals(pageState.command)) {
  String hql = "select count(distinct task.id) from " + Task.class.getName() + " as task";
  QueryHelper qhelp = new QueryHelper(hql);

  //  security
  TaskBricks.addSecurityReadClauses(qhelp, pageState);
  //qhelp.addOQLClause("task.parent is null");
  qhelp.addOQLClause("assignment.creationDate>:lastWeek");

  cc.add(CompanyCalendar.DATE, -7);
  qhelp.addParameter("lastWeek", cc.setAndGetTimeToDayStart());

  PageSeed taskList = pageState.pageFromRoot("task/taskList.jsp");
  taskList.command = Commands.FIND;
  taskList.addClientEntry("ASSIG_CREATED_ON", ">-1w");
  //taskList.addClientEntry("ROOT_OR_STANDALONE", Fields.TRUE);
  //taskList.addClientEntry("STATUS", TaskStatus.STATUS_DONE+","+TaskStatus.STATUS_FAILED);

  long count = (Long) qhelp.toHql().uniqueResult();
  if (count > 0) {
%><a href="<%=taskList.toLinkToHref()%>">
  <h2><%=I18n.get("ASSIGMENT_RECENTLY_CREATED")%></h2>
  <span class="wphlNumber"><%=count%></span>
  <p><%=I18n.get("ASSIGMENT_RECENTLY_CREATED_DESCR")%></p>
</a><%
    }

  }

%>
