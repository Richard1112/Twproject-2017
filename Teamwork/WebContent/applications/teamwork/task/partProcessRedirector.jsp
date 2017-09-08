<%@ page import="com.twproject.security.TeamworkPermissions, com.twproject.task.Task, com.twproject.task.process.ProcessUtilities, net.sf.json.JSONObject, org.jblooming.persistence.PersistenceHome, org.jblooming.persistence.hibernate.PersistenceContext, org.jblooming.waf.JSONHelper, org.jblooming.waf.constants.Commands, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jbpm.graph.node.TaskNode" %><%
  PageState pageState= PageState.getCurrentPageState(request);
  long taskNodeId=pageState.getEntry("taskNodeId").longValueNoErrorNoCatchedExc();
  String parentTaskId=pageState.getEntry("parentTaskId").stringValueNullIfEmpty();
  Task parentTask= (Task) PersistenceHome.findByPrimaryKey(Task.class,parentTaskId);

  JSONHelper jsonHelper = new JSONHelper();
  JSONObject json = jsonHelper.json;
  try {
  if (parentTask!=null && taskNodeId>0){
    TaskNode taskNode = (TaskNode) PersistenceContext.getDefaultPersistenceContext().session.load(TaskNode.class, taskNodeId);
    Task t=ProcessUtilities.getTaskByJbpmTaskNode(parentTask,taskNode);

    if (t!=null) {
      if (t.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.task_canRead)){
        PageSeed ps=pageState.pageInThisFolder("taskOverview.jsp",request);
        ps.mainObjectId=t.getId();
        ps.command= Commands.EDIT;
        json.element("urlToGo",ps.toLinkToHref());
      } else {
        json.element("ok",false);
        throw new Exception(pageState.getI18n("PERMISSION_LACKING"));
      }
    } else {
      json.element("ok",false);
      throw new Exception(pageState.getI18n("STRUCTURAL_NODE_NO_TASK"));
    }

  }
  } catch (Throwable t) {
    jsonHelper.error(t);
  }

  jsonHelper.close(pageContext);

%>