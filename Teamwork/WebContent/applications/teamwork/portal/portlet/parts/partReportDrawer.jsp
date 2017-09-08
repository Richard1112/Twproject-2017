<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.security.SecurityBricks, com.twproject.waf.settings.ReportBricks, net.sf.jasperreports.engine.base.JRBaseParameter, org.jblooming.security.Area, org.jblooming.tracer.Tracer, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, java.util.HashMap, java.util.Map, java.util.Set, org.jblooming.waf.settings.ApplicationState, java.io.File" %><%
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  String reportName = pageState.getEntryAndSetRequired("reportName").stringValue();
  String outType = pageState.getEntry("outType").stringValueNullIfEmpty();

  ReportBricks.Report report = ReportBricks.getReport(reportName);
  if (report==null){
    Tracer.platformLogger.error("No report found for: "+reportName);
    return;
  } else if (!report.file.exists()){
    Tracer.platformLogger.error("Report file no longer exists: "+report.file.getAbsolutePath());
    return;
  } else if(!logged.hasPermissionFor(report.requiredPermission)){
    throw new SecurityException(I18n.get("PERMISSION_LACKING")+" "+report.requiredPermission.name);
  }
  Map parameters = new HashMap();
  parameters.put(JRBaseParameter.REPORT_LOCALE, pageState.getLoggedOperator().getLocale());

  //si passano sempre le aree su cui si hanno i diritti
  Set<Area> areas = SecurityBricks.getAreasForLogged(report.requiredPermission, pageState);
  parameters.put("AREA_IDS", ReportBricks.extractIds(areas));
  parameters.put("IMG_PATH", ApplicationState.webAppFileSystemRootPath+ File.separator+"img");


  report.exportReport(outType, parameters, response);
%>