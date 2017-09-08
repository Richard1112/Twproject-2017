<%@ page import="com.twproject.security.SecurityBricks" %>
<%@ page import="com.twproject.task.Task" %>
<%@ page import="com.twproject.task.businessLogic.TaskAction" %>
<%@ page import="com.twproject.waf.TeamworkHBFScreen" %>
<%@ page import="net.sf.jasperreports.engine.*" %>
<%@ page import="net.sf.jasperreports.engine.base.JRBaseParameter, net.sf.jasperreports.engine.export.HtmlExporter, net.sf.jasperreports.engine.export.JRHtmlExporterParameter, net.sf.jasperreports.engine.util.JRLoader, net.sf.jasperreports.j2ee.servlets.ImageServlet, org.jblooming.persistence.hibernate.PersistenceContext, org.jblooming.security.Area, org.jblooming.security.Permission, org.jblooming.waf.ScreenArea, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, java.io.File, java.util.*" %>

<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {

    pageState.screenRunning = true;

    final ScreenArea body = new ScreenArea(request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);

    pageState.toHtml(pageContext);

  } else {

    // per compilare il report
    JasperCompileManager.compileReportToFile(application.getRealPath("/applications/teamwork/customers/default/reports/openProjects.jrxml"));

    String realPath = application.getRealPath("/applications/teamwork/customers/default/reports/openProjects.jasper");
    File reportFile = new File(realPath);
    if (!reportFile.exists())
      throw new JRRuntimeException("File WebappReport.jasper not found. The report design must be compiled first.");

    JasperReport jasperReport = (JasperReport) JRLoader.loadObjectFromFile(reportFile.getPath());

    JRParameter[] requiredParams = jasperReport.getParameters();

%><%=requiredParams%><%

  Map parameters = new HashMap();
  parameters.put("REPORT_TITLE", I18n.get("TASK_LIST"));

  //parameters.put(JRBaseParameter.IS_IGNORE_PAGINATION, true);
  parameters.put(JRBaseParameter.REPORT_LOCALE, pageState.getLoggedOperator().getLocale());


  // call task action
  pageState.addClientEntry("NAME_DESCRIPTION", "*");
  pageState.command = "FN";
  TaskAction ta = new TaskAction(pageState);
  ta.cmdFind();
  ArrayList arrayList = new ArrayList();
  for (Object[] os : (List<Object[]>) pageState.getPage().getAllElements()) {
    arrayList.add(((Task) os[0]).getId());
  }


  //parameters.put("TASK_IDS", arrayList);
  Set<Area> areas = SecurityBricks.getAreasForLogged(new Permission("Report_test"), pageState);
  ArrayList areaList = new ArrayList();
  areaList.add("6");
  parameters.put("AREA_IDS", areaList);

  JasperPrint jasperPrint = JasperFillManager.fillReport(jasperReport, parameters, PersistenceContext.getDefaultPersistenceContext().session.connection());

  HtmlExporter exporter = new HtmlExporter();
  session.setAttribute(ImageServlet.DEFAULT_JASPER_PRINT_SESSION_ATTRIBUTE, jasperPrint);
  exporter.setParameter(JRExporterParameter.JASPER_PRINT, jasperPrint);

  // per sparare diretti sull'out
  //exporter.setParameter(JRExporterParameter.OUTPUT_WRITER, out);

  // per passare da un buffered out
  StringBuffer sbuffer = new StringBuffer();
  exporter.setParameter(JRExporterParameter.OUTPUT_STRING_BUFFER, sbuffer);

  // per razzare la paginazione
  exporter.setParameter(JRHtmlExporterParameter.HTML_HEADER, "");
  exporter.setParameter(JRHtmlExporterParameter.HTML_HEADER, "");
  exporter.setParameter(JRHtmlExporterParameter.BETWEEN_PAGES_HTML, "");
  exporter.setParameter(JRHtmlExporterParameter.HTML_FOOTER, "");


  exporter.exportReport();


  /*
  // PDF export
  JRPdfExporter pdfExporter = new JRPdfExporter();
  pdfExporter.setParameter(JRExporterParameter.JASPER_PRINT, jasperPrint);
  pdfExporter.setParameter(JRExporterParameter.OUTPUT_STREAM, outputStream);
  pdfExporter.exportReport();
  */

%><%=sbuffer%><%

  }

%>

