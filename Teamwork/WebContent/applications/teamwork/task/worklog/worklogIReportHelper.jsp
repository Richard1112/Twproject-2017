<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.waf.settings.ReportBricks, com.twproject.worklog.Worklog, com.twproject.worklog.businessLogic.WorklogAction, net.sf.jasperreports.engine.base.JRBaseParameter, org.jblooming.page.Page, org.jblooming.tracer.Tracer, org.jblooming.utilities.JSP, org.jblooming.waf.constants.Commands, org.jblooming.waf.view.PageState, java.io.Serializable, java.util.ArrayList, java.util.HashMap, java.util.List, java.util.Map, org.jblooming.security.License" %><%
  // NON CI DEVONO ESSERE RITORNI A CAPO !!!!!!! altrimenti da un warning

  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  String reportName = pageState.getEntryAndSetRequired("reportName").stringValue();
  String outType = pageState.getEntry("outType").stringValueNullIfEmpty();

  //si recupera il report
  ReportBricks.Report report = ReportBricks.getReport(reportName);
  //  se non c'è o non c'è più si esce
  if (report == null) {
    Tracer.platformLogger.error("No report found for: " + reportName);
    return;
  } else if (!report.file.exists()) {
    Tracer.platformLogger.error("Report file no longer exists: " + report.file.getAbsolutePath());
    return;
  }

  List<Serializable> wlIds = new ArrayList<Serializable>();

  if (Commands.FIND.equals(pageState.command) && License.assertLevel(30)) {
    WorklogAction wla = new WorklogAction(pageState);
    wla.cmdFind();
    Page wlPage = pageState.getPage();
    if (JSP.ex(wlPage)) {
      List<Worklog> wlList = wlPage.getAllElements();
      for (Worklog wl : wlList) {
        if (wl.hasPermissionFor(logged, report.requiredPermission))
          wlIds.add(wl.getId());
      }
    }

  } else {
    return;
  }

  if (JSP.ex(wlIds)) {
    Map parameters = new HashMap();
    parameters.put(JRBaseParameter.REPORT_LOCALE, pageState.getLoggedOperator().getLocale());

    //si settano i worklog id su cui siamo abilitati
    parameters.put("WORKLOG_IDS", wlIds);

    report.exportReport(outType, parameters, response);
  }
%>