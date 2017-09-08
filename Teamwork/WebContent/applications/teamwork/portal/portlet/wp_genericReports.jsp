<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.waf.settings.ReportBricks, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.TreeSet" %><%
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  boolean first=true;

  PageSeed reportDrawer= pageState.pageFromRoot("/portal/portlet/parts/partReportDrawer.jsp");
  reportDrawer.setPopup(true);

  TreeSet<ReportBricks.Report> genericReports = ReportBricks.getReports("");
  if (genericReports.size()>0){
    for (ReportBricks.Report r: genericReports){
      if (logged.hasPermissionFor(r.requiredPermission)){
        if (first){
          %><div class="portletBox small lreq30 lreqLabel"><h1><%=I18n.get("GENERIC_REPORTS")%></h1><%
          first=false;
        }
        %><p class="activityLine" style=""><%
        reportDrawer.addClientEntry("reportName",r.name);
        reportDrawer.addClientEntry("outType","PDF");
        ButtonLink bl= new ButtonLink(r.getBestName(),reportDrawer);
        bl.target="_blank";
        bl.toHtmlInTextOnlyModality(pageContext);
      }
    }
    if (!first){
      %></div><%
    }
  }
%>
