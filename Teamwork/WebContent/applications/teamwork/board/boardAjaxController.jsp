<%@ page import="com.twproject.document.TeamworkDocument, com.twproject.document.businessLogic.DocumentAction, com.twproject.operator.TeamworkOperator, com.twproject.resource.Resource, com.twproject.resource.ResourceBricks, com.twproject.security.TeamworkPermissions, com.twproject.task.Assignment, com.twproject.task.Task, com.twproject.task.businessLogic.AssignmentAction, com.twproject.task.businessLogic.TaskAction, com.twproject.task.financial.Cost, com.twproject.task.financial.CostClassification, com.twproject.worklog.Worklog, com.twproject.worklog.businessLogic.WorklogAction, net.sf.json.JSONArray, net.sf.json.JSONObject, org.jblooming.agenda.Period, org.jblooming.designer.DesignerField, org.jblooming.ontology.PersistentFile, org.jblooming.persistence.PersistenceHome, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.utilities.NumberUtilities, org.jblooming.waf.ActionUtilities, org.jblooming.waf.JSONHelper, org.jblooming.waf.html.input.Uploader, org.jblooming.waf.view.ClientEntry, org.jblooming.waf.view.PageState, java.util.Date, org.jblooming.waf.view.RestState, com.twproject.messaging.board.Board, com.twproject.messaging.board.businessLogic.BoardAction" %>
<%


  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  JSONHelper jsonHelper = new JSONHelper();
  JSONObject json = jsonHelper.json;

  try {
    Resource loggedRes = logged.getPerson();

    // --------------------------- DEELETE A BOARD --------------------------------
    if ("DELBOARD".equals(pageState.command)) {
      BoardAction ba = new BoardAction(pageState);
      ba.cmdDelete();
    }
  } catch (Throwable t) {
    jsonHelper.error(t);
  }

  jsonHelper.close(pageContext);

%>