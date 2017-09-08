<%@ page import="com.twproject.resource.Resource, com.twproject.resource.ResourceBricks, com.twproject.security.TeamworkPermissions, com.twproject.setup.WizardSupport,
com.twproject.task.Assignment, com.twproject.task.Task, com.twproject.task.TaskBricks, com.twproject.task.businessLogic.TaskController, com.twproject.waf.TeamworkHBFScreen,
org.jblooming.flowork.FlowBricks, org.jblooming.flowork.PlatformJbpmSessionFactory, org.jblooming.operator.Operator, org.jblooming.persistence.PersistenceHome, org.jblooming.security.Role,
org.jblooming.utilities.JSP, org.jblooming.utilities.ReflectionUtilities, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields,
org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.display.PathToObject, org.jblooming.waf.html.input.Combo, org.jblooming.waf.html.input.SmartCombo, org.jblooming.waf.html.input.TextField,
org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jbpm.JbpmContext, org.jbpm.db.GraphSession, org.jbpm.graph.def.Node, org.jbpm.graph.def.ProcessDefinition, org.jbpm.graph.def.Transition, org.jbpm.graph.node.TaskNode, org.jbpm.taskmgmt.def.Swimlane, java.util.*, com.twproject.utilities.TeamworkComparators" %><%
PageState pageState = PageState.getCurrentPageState(request);

if (!pageState.screenRunning) {

  pageState.screenRunning = true;
  final ScreenArea body = new ScreenArea(new TaskController(), request);
  TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
  lw.register(pageState);
  pageState.perform(request, response);

  pageState.toHtml(pageContext);


} else {

  Operator logged = pageState.getLoggedOperator();

  JbpmContext jbpmSession = PlatformJbpmSessionFactory.getJbpmContext(pageState);
  GraphSession gs = jbpmSession.getGraphSession();

  Task task = (Task) pageState.getMainObject();


  ProcessDefinition processDefinition=null;
  long processDefinitionId = pageState.getEntry("TASK_PROCESS").longValueNoErrorNoCatchedExc();
  if (processDefinitionId != 0) {
     processDefinition = gs.loadProcessDefinition(processDefinitionId);
  }


  //may come from clone
  String taskSourceId = pageState.getEntry("TASK_SOURCE").stringValueNullIfEmpty();
  if (JSP.ex(taskSourceId)) {
    Task taskSource = (Task) PersistenceHome.findByPrimaryKey(Task.class,taskSourceId);
    Task taskProcessRoot=taskSource.getProcessRootTask();
    if (!taskSource.equals(taskProcessRoot)){
      taskSource=taskProcessRoot;
      PageState.Message message = pageState.addMessageInfo(I18n.get("TW_WILL_COPY_PROCESS_FROM_ROOT"));
      message.title=I18n.get("CANNOT_COPY_CHILD_PROCESS");
    }

    pageState.addClientEntry("TASK_CODE","["+JSP.w(taskSource.getCode())+"]");
    pageState.addClientEntry("TASK_NAME","["+JSP.w(taskSource.getName())+"]");
    if (task.getType()!=null)
      pageState.addClientEntry("TASK_TYPE",taskSource.getType().getId());


    processDefinition = gs.findLatestProcessDefinition(taskSource.getProcess().getProcessDefinition().getName());
    long pdi =  processDefinition.getId();
    pageState.addClientEntry("TASK_PROCESS", pdi);
    for (Task child : taskSource.getChildrenSorted()) {
      pageState.addClientEntry("DURATION_"+child.getExternalCode(),child.getDuration());
      for (Assignment a: child.getAssignments()) {
        if (JSP.ex(a.getExternalCode())) {
          Resource resource = a.getResource();
          pageState.addClientEntry("RESOURCE_"+a.getExternalCode(), resource.getId());
        }
      }
    }
  }

  PageSeed self = pageState.thisPage(request);
  self.command= Commands.ADD;
  self.addClientEntry(pageState.getEntry(Fields.PARENT_ID));

  Form form = new Form(self);
  form.alertOnChange = true;
  pageState.setForm(form);
  form.start(pageContext);


  %><script>$("#TASK_MENU").addClass('selected');</script>
<%---------------------------------------------------- START path to object ------------------------------------------------%>
   <%
      PathToObject pto = new PathToObject(task);
      pto.comparator = new TeamworkComparators.TaskManualOrderComparator();
      pto.canClick = TeamworkPermissions.task_canRead;

      PageSeed back = new PageSeed(ApplicationState.contextPath+"/applications/teamwork/task/taskList.jsp");
      //back.setCommand(Commands.FIND);
      ButtonLink taskList = new ButtonLink(I18n.get("TASK_LIST")+" /", back);
      pto.rootDestination = taskList;

      pto.destination = pageState.pageFromRoot("task/taskOverview.jsp");
      pto.destination.setCommand(Commands.EDIT);
      pto.displayNameForNewObject="";
      pto.toHtml(pageContext);
    %>
<%-- -------------------------------------------------- END path to object --------------------------------------------------%>
<h1><%=I18n.get("TASK_CREATE_PROCESS")%></h1>

<div class="container level_2">
<table width="100%" border="0" cellpadding="5" cellspacing="0" >
<tr style="background-color: #eeeeee">
   <td valign="top">
     <%
      Combo processDefinitionsCombo = FlowBricks.getLatestProcessDefinitionsCombo("TASK_PROCESS", pageState);
      processDefinitionsCombo.label="TASK_PROCESS_DEFINITION";
      processDefinitionsCombo.separator="<br>";
      processDefinitionsCombo.required = true;
      ButtonSubmit submit = new ButtonSubmit(form);
      submit.alertOnChange=false;
      submit.alertOnRequired=false;
      processDefinitionsCombo.setJsOnChange = submit.generateJs()+"";
      processDefinitionsCombo.toHtmlI18n(pageContext);
    %><br>
  <%=JSP.wHelp(I18n.get("TASK_PROCESS_DEFINITION_DESCRIPTION"))%>
  </td>
  <td valign="top"><%

      // hack to have a default name
      if (processDefinition!=null && !JSP.ex(pageState.getEntry("TASK_NAME")))
        pageState.addClientEntry("TASK_NAME","["+processDefinition.getName()+"]");

      TextField tfn = new TextField("TASK_NAME", "<br>");
      tfn.fieldSize = 40;
    boolean canWrite = task.hasPermissionFor(logged,TeamworkPermissions.task_canWrite);
    tfn.readOnly = !canWrite;
      tfn.required = true;
      tfn.toHtmlI18n(pageContext);
    %><br><%=JSP.wHelp(I18n.get("TASK_PROCESS_NAME_DESCRIPTION"))%></td>

    <td valign="top"><%

       TextField tf = new TextField("TASK_CODE", "<br>");
      tf.fieldSize = 12;
      tf.readOnly = !canWrite;
      tf.toHtmlI18n(pageContext);

      %><br><%=JSP.wHelp(I18n.get("TASK_PROCESS_CODE_TYPE_DESCRIPTION"))%></td>
  <td valign="top"><%

      SmartCombo sc = TaskBricks.getTaskTypeCombo("TASK_TYPE", task, pageState);
      sc.separator = "<br>";
      sc.readOnly = !canWrite;
      sc.toHtmlI18n(pageContext);

    %>
    </td>

</tr>




<%

  ButtonBar bb= new ButtonBar();


// ---------------------------------------------------------------------------------- START TASK STRUCTURE ------------------------------------------------------

  boolean allRolesExisting=true;
  if (processDefinition != null) {


%>
<tr>
   <td colspan="4" valign="top"><h2><%=I18n.get("TASK_PROCESS_FLOW_DESCRIPTION")%></h2>
     <strong><%=processDefinition.getName()%></strong>
     <%
       if (JSP.ex(processDefinition.getDescription())){
         %><div style="font-size:smaller;"><%=processDefinition.getDescription()%></div><%
       }
    %>&nbsp;<small>(ver. <%=processDefinition.getVersion()%> id:<%=processDefinition.getId()%>)</small>
   </td>
  </tr>
  <tr><td colspan="4">

<table align="center" border="0" class="table edged" cellpadding="5">
  <tr>
    <th class="tableHead"><%=I18n.get("STEP_NAME")%> </th>
    <th width="250" class="tableHead"><%=I18n.get("TASK")%></th>
    <th width="250" class="tableHead"><%=I18n.get("SWIMLANE")%>    </th>
    <th width="250" class="tableHead"><%=I18n.get("ASSIGNMENT")%>    </th>
    <%--th><%=I18n.get("DURATION")%>    </th--%>
    <th class="tableHead"><%=I18n.get("TRANSITION")%>    </th>
  </tr>

  <%

    List<Node> sn = processDefinition.getNodes();

    Map<Swimlane,String> visitedSwims= new HashMap();

    for (Node n : sn) {
      Node node= (Node) ReflectionUtilities.getUnderlyingObjectAsObject(n);
      String nodeName = node.getName() + "";



  %>
  <tr>
    <td valign="top" title="id:<%=node.getId()%>" id="step_<%=nodeName.replace(' ','_')%>"><b><%=nodeName%></b>
      <%
        if (JSP.ex(node.getDescription())){
          %><br><span style="font-size:smaller;"><%=node.getDescription()%></span><%
        }
      %></td><%
      if (node instanceof TaskNode) {
        %><td colspan="3" valign="top" class="taskStep" style="background: #FFFCF3"><%
        TaskNode tn = (TaskNode) node;
        Set<org.jbpm.taskmgmt.def.Task> tset = tn.getTasks();
        %>
          <table cellpadding="4" cellspacing="0" border="0">
        <%
        for (org.jbpm.taskmgmt.def.Task jbTask : tset) {
          %><tr><%

          %><td width="220"><%
            boolean nn=false;
            if(!nodeName.equals(jbTask.getName())){
              nn=true;
              %><%=jbTask.getName()%><%
            }
            if(JSP.ex(jbTask.getDescription())){
              %><span style="font-size:smaller;"><%=(nn?"<br>":"")+jbTask.getDescription()%></span><%
            }
          %></td><%


          Swimlane swimlane = jbTask.getSwimlane();

          Role role=null;
          if (swimlane!=null) {
            role = WizardSupport.getRoleByNameAndArea(swimlane.getName(), task.getArea());

            if (role==null)
              role= (Role) PersistenceHome.findUniqueNullIfEmpty(Role.class,"name",swimlane.getName()); //try in a different area

            if (role==null)
              role = TaskBricks.getWorkerRole(task.getArea());

            if (role==null){
              %><td colspan="2" class="error"><%=I18n.get("ROLE_MISSING")%>: <%=swimlane.getName()%> </td><%
              allRolesExisting=false;
              pageState.addMessageError(I18n.get("ROLE_MISSING")+": "+ swimlane.getName());
            } else {
              TextField.hiddenInstanceToHtml("ROLE_"+jbTask.getId(),role.getId()+"",pageContext);

            %><td width="250"><%=swimlane.getName()%><small><%=swimlane.getName().equalsIgnoreCase(role.getName())?"":" ("+role.getName()+")"%></small></td><%


            %><td width="250"><%
            if (jbTask.getAssignmentDelegation()==null){
              if (!visitedSwims.containsKey(swimlane)){
                visitedSwims.put(swimlane,nodeName );

                //this is the case where the swimlane is defined by InstantiatorSwimlaneAssignmentHandler -> current logged user
                if (swimlane.getAssignmentDelegation()!=null && swimlane.getAssignmentDelegation().getClassName().indexOf("InstantiatorSwimlaneAssignmentHandler")>0){
                  %><%=logged.getDisplayName()%><%
                } else{
                  SmartCombo ress = TaskBricks.getAssignableResourceCombo(null, "RESOURCE_" + jbTask.getId(),true, pageState);
                  ress.label = "";
                  ress.script = "style=width:200px";
                  ress.iframe_width = 200;
                  ress.separator = "";
                  //ress.required=true;
                  ress.toHtml(pageContext);
                }
              } else {
                %><%=I18n.get("AS_IN")%>: <%=visitedSwims.get(swimlane)%><%
              }
            } else {
              %><%=I18n.get("FLUX_ASSIGNED_BY")%>: <%=jbTask.getAssignmentDelegation().getInstance().getClass().getSimpleName()%><br>
              <small><%=JSP.htmlEncode(jbTask.getAssignmentDelegation().getConfiguration())%></small>
              <%
            }
          }
            %></td><%
        } else if (jbTask.getAssignmentDelegation()!=null){
          %><td colspan="2" width="500"><%=I18n.get("FLUX_ASSIGNED_BY")%>: <%=jbTask.getAssignmentDelegation().getInstance().getClass().getSimpleName()%><br>
          <small><%=JSP.htmlEncode(jbTask.getAssignmentDelegation().getConfiguration())%></small></td>
          <%
        }
        %></tr><%
      }
          %></table></td><%
    } else {
      %><td colspan="3">&nbsp;</td><%
    }


    %><td><%
        //---------------------------------- leaving transitions ---------------------------------------------
        Collection lec = node.getLeavingTransitions();
        if (lec != null) {
          for (Object o3 : lec) {
            Transition transition = (Transition) o3;

            String transname = transition.getName();
            Node dest = transition.getTo();


      %><a href="#">
      <div
              <% if (dest != null) { %>
              onMouseOver="obj('step_<%=dest.getName().replace(' ','_')%>').style.backgroundColor='#ffff00';"
              onMouseOut="obj('step_<%=dest.getName().replace(' ','_')%>').style.backgroundColor='';"
              <% } %>
              >
        <%=(transname != null ? transname + " : " : "") + (dest != null ? dest.getName() : "")%>
      </div>
    </a><%
      if (JSP.ex(transition.getDescription())){
        %><span style="font-size:smaller;"><%=transition.getDescription()%></span><%
      }
      %><br><%


        }
      }
    %>
    </td>


  </tr>
  <% } %>

</table>

</td></tr>
<%

    }

// ---------------------------------------------------------------------------------- END TASK STRUCTURE ------------------------------------------------------

  ButtonSubmit saveInstance = ButtonSubmit.getSaveInstance(form, I18n.get("CREATE_PROCESS"),false);
  saveInstance.additionalCssClass="first big";
  //saveInstance.variationsFromForm.href=pageState.pageFromRoot("task/taskEditor.jsp").href;
  saveInstance.variationsFromForm.command="CREATE_PROCESS";
  saveInstance.enabled=processDefinition != null && allRolesExisting;
  saveInstance.additionalCssClass="lreq30 lreqLabel";
  bb.addButton(saveInstance);

  PageSeed graph = pageState.pageFromCommonsRoot("flowork/backoffice/nodesPlus.jsp");
    graph.mainObjectId=processDefinitionId;
    ButtonSupport graphLink = ButtonLink.getBlackInstance(I18n.get("GRAPH"),768,1024,graph);
    graphLink.additionalCssClass="big";
    if (processDefinition == null)
      graphLink.enabled=false;
    bb.addButton(graphLink);

  %></table><%

  bb.toHtml(pageContext);

 %></div><%
  form.end(pageContext);


}
%>
