<%@ page import="org.jblooming.waf.ScreenBasic,
                 org.jblooming.waf.settings.businessLogic.I18nController,
                 org.jblooming.waf.view.PageState, org.jblooming.waf.ScreenArea, com.twproject.waf.TeamworkHBFScreen"%><%


PageState pageState = PageState.getCurrentPageState(request);
pageState.getLoggedOperator().testIsAdministrator();

if (!pageState.screenRunning) {
  ScreenBasic screenBasic = ScreenBasic.preparePage(new I18nController(), pageContext);
  screenBasic.getBody().areaHtmlClass="lreq30 lreqPage";
  pageState.perform(request, response).toHtml(pageContext);

} else {

  %><jsp:include page="partI18nManager.jsp" />
<style type="text/css">
  code{
    color: #8b1007;
  }
</style>

<%
  }
%>
