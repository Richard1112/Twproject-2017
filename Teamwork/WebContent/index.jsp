<%@ page buffer="164kb"%><%@ page import="org.jblooming.waf.settings.ApplicationState,org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState"%><%
  request.getRequestDispatcher(new PageSeed(ApplicationState.platformConfiguration.defaultIndex).toLinkToHref()).forward(request,response);
%>