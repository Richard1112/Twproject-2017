<%@ page import="com.twproject.operator.TeamworkOperator"%>
<%@ page import="com.twproject.task.Task"%>
<%@ page import="org.jblooming.oql.OqlQuery"%>
<%@ page import="org.jblooming.waf.view.PageState"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
  logged.testIsAdministrator();
  

  String oql = "from " + Task.class.getName() +" as task where task.id = :id";
  OqlQuery oqlQuery = new OqlQuery(oql);

  oqlQuery.getQuery().setString("id","5");
  Task t4 = (Task) oqlQuery.uniqueResult();

  oqlQuery.getQuery().setString("id","3");
  Task t3 = (Task) oqlQuery.uniqueResult();

  t3.setParentAndStore(t4);

%>ok