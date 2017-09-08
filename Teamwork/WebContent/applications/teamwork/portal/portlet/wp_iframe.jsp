<%@ page import="org.jblooming.utilities.JSP,
org.jblooming.waf.html.container.Container,
org.jblooming.waf.view.PageState" %>
<div id="wp_iframe" class="portletBox"><%

  PageState pageState = PageState.getCurrentPageState(request);

  String title = pageState.getEntry("IFRAMETITLE").stringValueNullIfEmpty();
  String url = pageState.getEntry("IFRAMEURL").stringValueNullIfEmpty();

   if (JSP.ex(url)) {

   %><h1><%=title%></h1>
  <iframe src="<%=url%>" width="100%"></iframe><%

   } else {
    %><label>To see content here, you must configure the iframe portlet from admin -> manage portlets.</label><%
  }
  %></div>
