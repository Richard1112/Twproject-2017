<%@ page import="org.jblooming.flowork.waf.FloworkPopUpScreen,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.html.display.HeaderFooter,
                 org.jblooming.waf.settings.Application,
                 org.jblooming.waf.view.PageState" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  pageState.setPopup(true);
  Application app = pageState.getApplication();

  FloworkPopUpScreen screen = (FloworkPopUpScreen) JspIncluderSupport.getCurrentInstance(request);

  HeaderFooter hf = pageState.getHeaderFooter();
  hf.toolTip = app.getName();
  hf.header(pageContext);

  screen.getBody().toHtml(pageContext);

  pageState.getHeaderFooter().footer(pageContext);

%>