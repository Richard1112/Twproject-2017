<%@ page import="com.twproject.operator.TeamworkOperator, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.container.DivOnMouseover, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.constants.Commands, com.twproject.security.TeamworkPermissions" %><%
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();


  //ButtonJS opener = new ButtonJS(I18n.get("TASK_MENU"), "changeActionAndSubmit('taskList.jsp','FN')");
  //uttonJS opener = new ButtonJS("");
  //opener.label=I18n.get("TASK_MENU");
  //DivOnMouseover domo = new DivOnMouseover(null,opener,true);


  ButtonSupport but;
  if (request.getRequestURI().contains("taskListAsGantt.jsp")) {
    but = new ButtonJS(I18n.get("TASK_MENU"), "changeActionAndSubmit(contextPath+'/applications/teamwork/task/taskList.jsp','FN')");
  } else {
    PageSeed ol = pageState.pageFromRoot("task/taskList.jsp");
    but = new ButtonLink(I18n.get("TASK_MENU"), ol);
  }

  but.additionalCssClass = "hasDivomo";

  DivOnMouseover domo = new DivOnMouseover(null,but,true);
  domo.addButton(but);


  if (request.getRequestURI().contains("taskList.jsp")) {
    but = new ButtonJS(I18n.get("VIEW_AS_GANTT"), "changeActionAndSubmit('taskListAsGantt.jsp','FN')");
  } else {
    PageSeed ap = pageState.pageFromRoot("task/taskListAsGantt.jsp");
    but = new ButtonLink(I18n.get("VIEW_AS_GANTT"), ap);
    but.additionalCssClass="lreq10 lreqLabel";
  }
  domo.addButton(but);


  but = new ButtonLink(I18n.get("DOCUMENTS"), pageState.pageFromRoot("document/documentList.jsp"));
  domo.addButton(but);
  
  but = new ButtonLink(I18n.get("REPORTS"), pageState.pageFromRoot("report/reportList.jsp"));
  domo.addButton(but);

  if (logged.hasPermissionFor(TeamworkPermissions.task_cost_canRead)) {
    PageSeed ap = pageState.pageFromRoot("task/financial/additionalCostList.jsp");
    but = new ButtonLink(I18n.get("COST_ADDITIONAL"), ap);
    but.additionalCssClass="lreq20 lreqLabel";
    domo.addButton(but);
  }

  if (logged.hasPermissionFor(TeamworkPermissions.project_canCreate)) {
    domo.addSeparator();

    PageSeed newTask = new PageSeed(request.getContextPath() + "/applications/teamwork/task/taskNew.jsp");
    newTask.addClientEntry("ADD_TYPE", "ADD_TASK");
    newTask.setCommand(Commands.ADD);
    ButtonSupport bl = ButtonLink.getBlackInstance("<span class=\"teamworkIcon withLabel\" style=\"\">P</span> " + I18n.get("ADD_TASK"), 720, 800, newTask);
    domo.addButton(bl);

  }


  domo.additionalDropDownClass="divDark";
  domo.toHtml(pageContext);
%>
