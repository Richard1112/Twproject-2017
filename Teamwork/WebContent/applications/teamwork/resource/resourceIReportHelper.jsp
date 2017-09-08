<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.resource.Resource, com.twproject.resource.businessLogic.ResourceAction, com.twproject.waf.settings.ReportBricks, net.sf.jasperreports.engine.base.JRBaseParameter, org.jblooming.page.Page, org.jblooming.tracer.Tracer, org.jblooming.utilities.JSP, org.jblooming.waf.constants.Commands, org.jblooming.waf.view.PageState, java.io.Serializable, java.util.ArrayList, java.util.HashMap, java.util.List, java.util.Map, org.jblooming.security.License" %><%
// NON CI DEVONO ESSERE RITORNI A CAPO !!!!!!! altrimenti da un warning

  response.reset();
  response.resetBuffer();
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

  List<Serializable> resIds = new ArrayList<Serializable>();
  List<Serializable> resDesc = new ArrayList<Serializable>();

  if (Commands.FIND.equals(pageState.command) && License.assertLevel(30)) {
    ResourceAction ra = new ResourceAction(pageState);
    ra.cmdFind();
    Page resPage = pageState.getPage();
    if (JSP.ex(resPage)) {
      List<Resource> resList = resPage.getAllElements();
      for (Resource res : resList) {
        if (res.hasPermissionFor(logged, report.requiredPermission)){
          resIds.add(res.getId());
          resDesc.add(res.getId());
          resDesc.addAll(res.getDescendantIds(Resource.class));

        }
      }
    }

  } else if (Commands.EDIT.equals(pageState.command) && License.assertLevel(30)) {
    ResourceAction ra = new ResourceAction(pageState);
    ra.cmdEdit();
    if (pageState.getMainObject() != null) {
      Resource resource = (Resource) pageState.getMainObject();
      if (resource.hasPermissionFor(logged, report.requiredPermission)){
        resIds.add(resource.getId());
        resDesc.add(resource.getId());
        resDesc.addAll(resource.getDescendantIds(Resource.class));
      }
    }

  } else {
    return;
  }


  if (JSP.ex(resIds)) {

    Map parameters = new HashMap();
    parameters.put(JRBaseParameter.REPORT_LOCALE, pageState.getLoggedOperator().getLocale());

    //si settano gli id su cui siamo abilitati
    parameters.put("RESOURCE_IDS", resIds);

    //si settano gli id dei descendant
    parameters.put("RESOURCE_DESCENDANTS", resDesc);


    report.exportReport(outType, parameters, response);
  }
%>