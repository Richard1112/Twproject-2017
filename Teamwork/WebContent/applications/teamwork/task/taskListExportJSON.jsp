<%@ page import="
 com.twproject.security.TeamworkPermissions,
 com.twproject.task.Task,
 com.twproject.task.businessLogic.TaskGanttAction,
 net.sf.json.JSONObject,
 org.jblooming.persistence.PersistenceHome,
 org.jblooming.utilities.StringUtilities,
 org.jblooming.waf.view.PageState, net.sf.json.JSONArray, com.twproject.task.businessLogic.TaskController, com.twproject.task.businessLogic.TaskAction, com.twproject.task.Assignment, com.twproject.resource.Resource, org.jblooming.security.Role, org.jblooming.utilities.DateUtilities, java.util.*"
%><%@ page pageEncoding="UTF-8"%><%
  PageState pageState = PageState.getCurrentPageState(request);
  new TaskAction(pageState).cmdFind();

  List<Object[]> objs = pageState.getPage() !=null ? pageState.getPage().getAllElements() : new ArrayList();

  JSONObject json= new JSONObject();
  JSONArray prjs= new JSONArray();

  Set<Resource> ress= new HashSet();
  Set<Role> roles= new HashSet();

  for (Object[] os:objs){
    Task task = (Task)os[0];
    JSONObject jTask = task.jsonify(false, pageState);
    jTask.element("level",0);


    //add assignments
    JSONArray assigs = new JSONArray();
    for (Assignment ass : task.getAssignementsSortedByRole()) {
      JSONObject jAss = new JSONObject();
      jAss.element("id", ass.getId());
      jAss.element("roleId", ass.getRole().getId());
      jAss.element("resourceId", ass.getResource().getId());
      jAss.element("effort", ass.getEstimatedWorklog());
      assigs.add(jAss);

      ress.add(ass.getResource());
      roles.add(ass.getRole());
    }
    jTask.element("assigs", assigs);

    prjs.add(jTask);
  }
  json.element("tasks",prjs);



  JSONArray jRes= new JSONArray();
  for (Resource r: ress){
    JSONObject jr= new JSONObject();
    jr.element("id",r.getId());
    jr.element("name",r.getName());
    jRes.add(jr);
  }
  json.element("resources",jRes);

  JSONArray jRol= new JSONArray();
  for (Role r: roles){
    JSONObject jr= new JSONObject();
    jr.element("id",r.getId());
    jr.element("name",r.getName());
    jRol.add(jr);
  }
  json.element("roles",jRol);


  String filenameEncoded = "TwprojectTaskList_"+ DateUtilities.dateToInt(new Date());
  response.setHeader("content-disposition", "attachment; filename=\"" + filenameEncoded + ".json\"");
  response.setContentType("application/json");

  out.print(json.toString(2));

%>