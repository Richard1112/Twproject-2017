<%@ page
  import="com.twproject.operator.TeamworkOperator, com.twproject.setup.SetupSupport, org.jblooming.scheduler.PlatformExecutionService, org.jblooming.tracer.Tracer, org.jblooming.waf.view.PageState, com.twproject.task.Task, org.jblooming.oql.OqlQuery, com.twproject.task.Assignment, org.jblooming.persistence.hibernate.PersistenceContext" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
  logged.testIsAdministrator();


  if (pageState.getEntry("go").checkFieldValue()) {
    if (pageState.getEntry("reset").checkFieldValue()) {
      new OqlQuery("update " + Assignment.class.getName() + " as ass set ass.worklogDone=0").getQuery().executeUpdate();
      new OqlQuery("update " + Task.class.getName() + " as tsk set tsk.totalCostsEstimated=0,tsk.totalCostsDone=0,tsk.totalWorklogEstimated=0,tsk.totalWorklogDone=0").getQuery().executeUpdate();
      PersistenceContext.getDefaultPersistenceContext().commitAndClose();
      PersistenceContext.getDefaultPersistenceContext();
    }

    JspWriter out1 = out;
    if (pageState.getEntry("asynch").checkFieldValue()) {
      PlatformExecutionService.executorService.submit(
        new Thread() {
          public void run() {
            try {
              SetupSupport.tw500UpdateTaskAssigIssueDenormFields();
            } catch (Throwable e) {
              Tracer.platformLogger.error("tw500UpdateTaskAssigIssueDenormFields failed", e);
            }
          }
        });

      %><h1>Update Task, Assig, Issue de-normalized fields is running in backgroud.</h1><%
    } else {
%><h1>Update Task, Assig, Issue de-normalized fields started.</h1>
<hr>
<%SetupSupport.tw500UpdateTaskAssigIssueDenormFields(out);%>
<hr>
<h1>Update Task, Assig, Issue de-normalized fields ended correctly.</h1>

<%
  }
} else {
%>
  <h2>
    <a href="updateDenormal.jsp?go=yes">Update only remaining (not yet updated) tasks and assignments - synch</a><br>
    <a href="updateDenormal.jsp?go=yes&reset=yes">Update all (will reset existing values) - synch </a>
  </h2><br>
<p>
  <a href="updateDenormal.jsp?go=yes&asynch=yes">Update only remaining (not yet updated) tasks and assignments - asynch</a><br>
  <a href="updateDenormal.jsp?go=yes&reset=yes&asynch=yes">Update all (will reset existing values) - asynch</a>
</p>
<%
  }
%>
