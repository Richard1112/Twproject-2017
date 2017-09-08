<%@ page import="
 com.twproject.security.TeamworkPermissions,
 com.twproject.task.Task,
 com.twproject.task.businessLogic.TaskGanttAction,
 net.sf.json.JSONObject,
 org.jblooming.persistence.PersistenceHome,
 org.jblooming.utilities.StringUtilities,
 org.jblooming.waf.view.PageState, net.sf.json.JSONArray, com.twproject.task.businessLogic.TaskController"
%><%@ page pageEncoding="UTF-8"%><%
  PageState pageState = PageState.getCurrentPageState(request);
  Task task = (Task) PersistenceHome.findByPrimaryKey(Task.class, pageState.mainObjectId);
  task.testPermission(pageState.getLoggedOperator(), TeamworkPermissions.task_canRead);
  String taskId = task.getId() + "";
  JSONObject json= new JSONObject();
  new TaskGanttAction(pageState).cmdLoadProject(taskId, pageState, json, false);

  String filenameEncoded = StringUtilities.normalize(task.getName());
  response.setHeader("content-disposition", "attachment; filename=\"" + filenameEncoded + ".json\"");
  response.setContentType("application/json");

  out.print(json.toString(2));

%>