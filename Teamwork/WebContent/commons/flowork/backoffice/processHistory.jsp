<%@ page import=" org.jblooming.flowork.FlowFields, org.jblooming.flowork.PlatformJbpmSessionFactory, org.jblooming.persistence.PersistenceHome, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenBasic, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.container.Container, org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jbpm.JbpmContext, org.jbpm.db.GraphSession, org.jbpm.graph.def.Node, org.jbpm.graph.def.ProcessDefinition, org.jbpm.graph.exe.ProcessInstance, org.jbpm.graph.node.TaskNode, org.jbpm.taskmgmt.def.Swimlane, org.jbpm.taskmgmt.def.Task, org.jbpm.taskmgmt.exe.TaskInstance, org.jbpm.taskmgmt.exe.TaskMgmtInstance, java.util.*" %><%

  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    ScreenBasic screenBasic = ScreenBasic.preparePage(null, pageContext);
    screenBasic.getBody().areaHtmlClass="lreq30 lreqPage";
    pageState.perform(request, response);
    pageState.toHtml(pageContext);

  } else {

    JbpmContext jbpmSession = PlatformJbpmSessionFactory.getJbpmContext(pageState);
    GraphSession gs = jbpmSession.getGraphSession();

    ProcessInstance pi = gs.loadProcessInstance(Long.parseLong(pageState.mainObjectId.toString()));
    ProcessDefinition def = pi.getProcessDefinition();

    TaskMgmtInstance tmi = pi.getTaskMgmtInstance();
    List<TaskInstance> tis = new ArrayList<TaskInstance>(tmi.getTaskInstances());
    Collections.sort(tis, new Comparator<TaskInstance>() {
      public int compare(TaskInstance ti1, TaskInstance ti2) {
        return ti1.getCreate().compareTo(ti2.getCreate());
      }
    });

    Container main = new Container("1", 0);
    main.title = I18n.get("FLUX_LOG_INST%%_PROC%%",pi.getId()+"", pi.getProcessDefinition().getName());
    main.start(pageContext);

    Container logs = new Container("2", 1);
    logs.title = I18n.get("STEPS_DONE");
    logs.start(pageContext);


%>
<table class="table">
  <tr>
    <th class="tableHead"><%=I18n.get("ROLE")%>/<%=I18n.get("ACTOR")%></th>
    <th class="tableHead"><%=I18n.get("STEP")%></th>
    <th class="tableHead"><%=I18n.get("START")%></th>
    <th class="tableHead"><%=I18n.get("END")%></th>
    <th class="tableHead"><%=I18n.get("DURATION")%></th>
  </tr>
  <%
    boolean swap = true;
    Set<Task> touchedNodes = new HashSet<Task>();
    for (TaskInstance ti : tis) {
      Swimlane swimlane = ti.getTask().getSwimlane();
      String descr="";
      if (swimlane==null)
        descr= "Actor id: "+ti.getActorId();
       else
        descr=swimlane.getName();
  %>
  <tr class="alternate" >
    <td><%=JSP.w(descr)%> </td>
    <%
      touchedNodes.add(ti.getTask());
      String stepName = JSP.w(ti.getName());
      FlowFields ff = (FlowFields) PersistenceHome.findUnique(FlowFields.class, "flowName", def.getName());
      if (ff != null && ff.getNodeDescriptions() != null && ff.getNodeDescriptions().containsKey(ti.getName()))
        stepName += ": " + ff.getNodeDescriptions().get(ti.getName());
    %>
    <td><%=stepName%>    </td>
    <td><%=DateUtilities.dateToString(ti.getCreate(), "dd MMM yyyy HH:mm:ss")%>    </td>
    <% Date end = ti.getEnd();%>
    <td><%=DateUtilities.dateToString(end, "dd MMM yyyy HH:mm:ss")%>
    </td>
    <%
      if (end == null)
        end = new Date();
    %>
    <td align="center"><%=DateUtilities.getMillisInHoursMinutes(end.getTime() - ti.getCreate().getTime())%>
    </td>
  </tr>
  <%

    }
  %></table>
<%

  logs.end(pageContext);


  if (pi.getEnd() == null) {
    Container cpe = new Container("3", 1);
    cpe.title = I18n.get("STEPS_TO_BE_DONE");
    cpe.start(pageContext);


%>
<table class="table">
  <tr>
     <th><%=I18n.get("ROLE")%></th>
    <th><%=I18n.get("STEP")%></th>
  </tr>
  <%
    swap = true;
    for (Object nodeObj : def.getNodes()) {
      Node node = (Node) nodeObj;
      if (node instanceof TaskNode) {
        TaskNode tn = (TaskNode) node;
        Task task = (Task) tn.getTasks().iterator().next();
        if (touchedNodes.contains(task))
          continue;

        Swimlane swimlane = task.getSwimlane();
        String descr="";
        if (swimlane==null)
          descr= "Delegation: "+task.getAssignmentDelegation().getInstance().getClass().getSimpleName()+ JSP.htmlEncode(task.getAssignmentDelegation().getConfiguration());
         else
          descr=swimlane.getName();

  %>
  <tr  class="alternate" >
    <td><%=JSP.w(descr)%>
    </td>
    <td><%=tn.getName()%>
    </td>
  </tr>
  <%
      }
    }
  %>
</table>
<%
      cpe.end(pageContext);

    }
    ButtonBar bb = new ButtonBar();
    PageSeed edit = new PageSeed("printProcessHistory.jsp");
    edit.setPopup(true);
    edit.setCommand(Commands.EDIT);
    edit.setMainObjectId(pi.getId());
    ButtonLink addBl = new ButtonLink(edit);
    addBl.label = I18n.get("PRINT");
    addBl.target = "print";
    addBl.popup_height = "550";
    addBl.popup_resizable = "yes";
    addBl.popup_scrollbars = "yes";
    bb.addButton(addBl);
    bb.toHtml(pageContext);
    main.end(pageContext);
  }
%>