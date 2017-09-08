<%@ page import="org.jblooming.utilities.NumberUtilities, org.jblooming.waf.view.ClientEntry, org.jblooming.waf.view.PageState"%><%
  ClientEntry businessLogic = PageState.getCurrentPageState(request).getEntry("EVAL");

  double val = businessLogic.doubleValue();
%><%=NumberUtilities.decimalNoGrouping(val,3)%>