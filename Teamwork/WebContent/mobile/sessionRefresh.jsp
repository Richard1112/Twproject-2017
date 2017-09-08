<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.task.TaskBricks, com.twproject.task.financial.FinancialBricks, org.jblooming.agenda.Period, org.jblooming.waf.html.input.SmartCombo, org.jblooming.waf.view.PageState, java.util.Date, org.jblooming.waf.settings.I18n, org.jblooming.designer.DesignerField, com.twproject.security.TeamworkPermissions, org.jblooming.security.Permission, java.util.HashSet, java.util.Set" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();


  //---------------------- issues page ---------------------------
  Set<Permission> perms = new HashSet();
  perms.add(TeamworkPermissions.issue_canCreate);
  perms.add(TeamworkPermissions.issue_canWrite);
  boolean showActiveOnly = true; //!JSP.ex(issue.getDescription()); // in order to see only active task when creating
  SmartCombo taskCombo = TaskBricks.getTaskCombo("taskId", showActiveOnly, perms, null, pageState);
  taskCombo.toHtml(pageContext);

  //---------------------- expenses page ---------------------------
  SmartCombo expAssigs = TaskBricks.getAllAssignmentsOfCombo("expAssId", logged.getPerson(), Period.getWeekPeriodInstance(new Date(),logged.getLocale()), false, false, false, false, null);
  expAssigs.toHtml(pageContext);

  SmartCombo costClass = FinancialBricks.getCostClassificationCombo("expClassification", null, pageState);
  costClass.toHtml(pageContext);

  //---------------------- worklog page ---------------------------
  SmartCombo wlAssigs = TaskBricks.getAllAssignmentsOfCombo("wlAssId", logged.getPerson(), Period.getWeekPeriodInstance(new Date(),logged.getLocale()), false, false, false, false, null);
  wlAssigs.toHtml(pageContext);

// --------------------------------------  INIT CUSTOM FIELDS ----------------------------------------------

  String[] customFieldsPrefix=new String[]{"TASK_CUSTOM_FIELD_","ISSUE_CUSTOM_FIELD_","RESOURCE_CUSTOM_FIELD_","ASSIGNMENT_CUSTOM_FIELD_","WORKLOG_CUSTOM_FIELD_","COST_CUSTOM_FIELD_","BUDGET_CUSTOM_FIELD_","COST_ADD_CUSTOM_FIELD_"};

  for (String prefix: customFieldsPrefix){
    for (int i=1;i<=6;i++){
      if (I18n.isActive(prefix+i)){
        DesignerField dfStr = DesignerField.getCustomFieldInstance( prefix,i, null,false, false, false, pageState);
      }
    }
  }



%>