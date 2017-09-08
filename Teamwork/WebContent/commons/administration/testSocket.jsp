<%@ page import="org.jblooming.PlatformRuntimeException, org.jblooming.waf.ScreenBasic, org.jblooming.waf.view.PageState, java.net.Socket" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  pageState.getLoggedOperator().testIsAdministrator();
  
  pageState.setPopup(true);
  
  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    ScreenBasic lw = ScreenBasic.preparePage(null, pageContext);
      lw.menu = null;
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);


  } else {
    %><p align="center"><br><big><%

    String server = pageState.getEntry("SOCKET_SERVER").stringValue();
    int port = pageState.getEntry("SOCKET_PORT").intValue();     
    boolean exc = false;
    try {
      Socket s = new Socket(server, port);
      if (s != null) {
        s.close();
        %>Server <%=server%> is timely answering on port <%=port%>.<%
    } else
      exc = false;
    } catch (Exception e) {
      exc = true;
      %><hr><%=PlatformRuntimeException.getStackTrace(e)%><hr><%
    }
    if (exc) {
      %>Server <%=server%> is NOT timely answering on port <%=port%>.<%



    }



    %></big></p><%
 }
%>