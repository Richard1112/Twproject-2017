<%@ page import=" org.jblooming.tracer.Tracer, org.jblooming.waf.ScreenBasic, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.display.HeaderFooter,
                  org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, org.jblooming.waf.settings.ApplicationState"
%><%
  PageState pageState = PageState.getCurrentPageState(request);
  ScreenBasic screen = (ScreenBasic) JspIncluderSupport.getCurrentInstance(request);

  HeaderFooter hf = pageState.getHeaderFooter();
  hf.toolTip = "Teamwork";

  //added by robik to avoid loading of main pages following links on iframe or popup
  hf.keepAsTop = false;

  try {
    if (pageState.href.indexOf(".") > -1) {
      String name = pageState.href.substring(pageState.href.lastIndexOf("/") + 1, pageState.href.lastIndexOf("."));
      String histName = pageState.getI18n("HIS_" + name.toUpperCase());
      if (!histName.startsWith("HIS_"))
        hf.toolTip = "Twproject - " + histName;
    }
  } catch (Throwable e) {
    Tracer.platformLogger.error(e);
  }
  hf.header(pageContext);

  %>

<div id="twMainContainerPopup">
<div id="savingMessage" class="noprint"><%=I18n.get("SAVING_MESSAGE")%></div>

<%

  if (screen.menu!=null && !pageState.isPopup()) {
    screen.menu.toHtml(pageContext);
  }
  %><div id="__FEEDBACKMESSAGEPLACE" style="display:none;"></div>
    <div id="twInnerContainerPopup" class="<%=screen.getBody().areaHtmlClass%>"><%

  screen.getBody().toHtml(pageContext);

  %></div>
</div><%

  pageState.getHeaderFooter().footer(pageContext);
%>