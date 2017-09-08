<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.task.Task, com.twproject.task.businessLogic.TaskAction, com.twproject.utilities.TeamworkComparators, com.twproject.waf.settings.ReportBricks, net.sf.jasperreports.engine.base.JRBaseParameter, org.jblooming.page.Page, org.jblooming.tracer.Tracer, org.jblooming.utilities.JSP, org.jblooming.waf.constants.Commands, org.jblooming.waf.view.PageState, java.io.Serializable, java.util.ArrayList, java.util.HashMap, java.util.List, java.util.Map, org.jblooming.security.License" %><%
// NON CI DEVONO ESSERE RITORNI A CAPO !!!!!!! altrimenti da un warning

  response.reset();
  response.resetBuffer();
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  String reportName = pageState.getEntry("reportName").stringValueNullIfEmpty()+"";

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

  List<Serializable> taskIds = new ArrayList<Serializable>();
  List<Serializable> taskDesc = new ArrayList<Serializable>();

  if (Commands.FIND.equals(pageState.command) && License.assertLevel(30)) {
    TaskAction ta = new TaskAction(pageState);
    ta.cmdFind();
    Page taskPage = pageState.getPage();
    if (JSP.ex(taskPage)) {
      List<Object[]> taskList = taskPage.getAllElements();
      for (Object[] os : taskList) {
        Task task = (Task) os[0];
        if (task.hasPermissionFor(logged, report.requiredPermission)){
          taskIds.add(task.getId());
          taskDesc.add(task.getId());
          //get descendant correctly sorted
          List<Task> descs = task.getDescendants(new TeamworkComparators.TaskManualOrderComparator());
          for (Task d:descs)
            taskDesc.add(d.getId());

        }
      }
    }

  } else if (Commands.EDIT.equals(pageState.command) && License.assertLevel(30)) {
    TaskAction ta = new TaskAction(pageState);
    ta.cmdEdit();
    if (pageState.getMainObject() != null) {
      Task task = (Task) pageState.getMainObject();
      if (task.hasPermissionFor(logged, report.requiredPermission)){
        taskIds.add(task.getId());
        taskDesc.add(task.getId());
        //get descendant correctly sorted
        List<Task> descs = task.getDescendants(new TeamworkComparators.TaskManualOrderComparator());
        for (Task d:descs)
          taskDesc.add(d.getId());
      }
    }

  } else {
    return;
  }


  if (JSP.ex(taskIds)) {

    Map parameters = new HashMap();
    parameters.put(JRBaseParameter.REPORT_LOCALE, pageState.getLoggedOperator().getLocale());

    //si settano i task id su cui siamo abilitati
    parameters.put("TASK_IDS", taskIds);

    //si settano i task descendants
    parameters.put("TASK_DESCENDANTS", taskDesc);


    report.exportReport(outType, parameters, response);
  }
%>