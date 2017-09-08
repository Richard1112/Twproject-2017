<%@ page buffer="16kb" %><%@ page import="com.twproject.operator.TeamworkOperator,
                 com.twproject.scheduler.TeamworkJobsLauncher, org.jblooming.waf.view.PageState" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
  logged.testIsAdministrator();

  TeamworkJobsLauncher.launch(PageState.getCurrentPageState(request).getLoggedOperator().getDisplayName());

  response.sendRedirect(request.getContextPath()+"/commons/scheduler/scheduleManager.jsp");
%>