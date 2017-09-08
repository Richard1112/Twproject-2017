<%@ page import="com.twproject.operator.TeamworkOperator,
                 com.twproject.resource.Resource,
                 com.twproject.resource.ResourceBricks,
                 com.twproject.security.TeamworkPermissions,
                 com.twproject.task.Assignment,
                 com.twproject.task.Issue,
                 com.twproject.task.Task,
                 org.jblooming.oql.OqlQuery,
                 org.jblooming.waf.html.core.JspHelper,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.html.input.SmartCombo,
                 org.jblooming.waf.view.PageState,
                 java.util.List, com.twproject.task.TaskBricks" %>
<%


  JspHelper resDrawer = (JspHelper) JspIncluderSupport.getCurrentInstance(request);

  Issue issue = (Issue) resDrawer.parameters.get("ISSUE");
  Task task = (Task) resDrawer.parameters.get("TASK");
  int fieldSize = 20;
  if (resDrawer.parameters.containsKey("SIZE"))
    fieldSize = (Integer) resDrawer.parameters.get("SIZE");

  PageState pageState = PageState.getCurrentPageState(request);

  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
  Resource resAssig = logged.getPerson();
  if (issue != null) {
    // make CE
    resAssig = issue.getAssignedTo();
  }

  if (resAssig != null)
    pageState.addClientEntry("ASSIGNEE", resAssig.getId());

  if (task == null && issue != null)
    task = issue.getTask();

  if (task != null) {

    SmartCombo assignee;
    //se hai il canManageAssignment puoi vedere anche altre risorse, altrimenti solo quelle che sono già assegnate
    if (task.hasPermissionFor(logged, TeamworkPermissions.assignment_canCRW)) {
      assignee = task.bricks.getAssignableResourceCombo("ASSIGNEE", false, pageState);
    } else {
      String hql = "select resource.id, resource.name from " + Resource.class.getName() + " as resource ";
      String whereForId = "where resource.id = :" + SmartCombo.FILTER_PARAM_NAME;
      // si mette una clausola che non trova niente, tanto quelle già assegnate si mettono comunque
      String whereForFiltering = "where resource.id='' and resource.id = :" + SmartCombo.FILTER_PARAM_NAME;
      assignee= new SmartCombo("ASSIGNEE",hql,whereForFiltering, whereForId);
      assignee.addAllowed=true;
      assignee.classic=true;
    }

    String hql = "select distinct assig.resource.id, assig.resource.name from " + Assignment.class.getName() + " as assig where assig.task = :task order by assig.resource.name";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setEntity("task", task);
    List<Object[]> resOfAssigsOnTask = oql.list();

    assignee.fieldSize = fieldSize;
    assignee.separator = "";
    assignee.label = "";
    assignee.readOnly = !task.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.issue_canWrite);

    assignee.additionalLines = resOfAssigsOnTask;
    assignee.linkToEntity = null;
    assignee.toHtmlI18n(pageContext);



  } else {
    //SmartCombo assignee = ResourceBricks.getPersonCombo("ASSIGNEE", TeamworkPermissions.assignment_manage, false, null, pageState);
    SmartCombo assignee =TaskBricks.getAssignableResourceCombo(null,"ASSIGNEE", false, pageState);
    assignee.fieldSize = fieldSize;
    assignee.separator = "";
    assignee.label = "";
    assignee.linkToEntity=null;
    assignee.readOnly=issue==null?false:!issue.hasPermissionFor(logged,TeamworkPermissions.issue_canWrite);
    assignee.toHtmlI18n(pageContext);
  }


%>