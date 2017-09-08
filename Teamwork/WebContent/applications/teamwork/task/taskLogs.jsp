<%@ page import="com.twproject.operator.TeamworkOperator,
                 com.twproject.security.TeamworkPermissions,
                 com.twproject.task.Assignment,
                 com.twproject.task.Task,
                 com.twproject.task.TaskScheduleHistory,
                 com.twproject.task.TaskStatusHistory,
                 com.twproject.task.process.ProcessUtilities,
                 com.twproject.waf.TeamworkHBFScreen,
                 com.twproject.waf.html.TaskHeaderBar,
                 org.jblooming.utilities.DateUtilities,
                 org.jblooming.utilities.JSP,
                 org.jblooming.utilities.ReflectionUtilities,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.core.JspHelper,
                 org.jblooming.waf.html.input.CheckField,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState,
                 org.jbpm.graph.def.Node,
                 org.jbpm.graph.def.ProcessDefinition,
                 org.jbpm.graph.exe.ProcessInstance,
                 org.jbpm.graph.node.TaskNode,
                 org.jbpm.taskmgmt.exe.TaskInstance, org.jbpm.taskmgmt.exe.TaskMgmtInstance, java.util.*, com.twproject.waf.TeamworkPopUpScreen" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    //this is set by action
    Task rootTask = Task.load(pageState.mainObjectId);
    pageState.setMainObject(rootTask);

    // stop if no permission
    rootTask.testPermission(logged, TeamworkPermissions.task_canRead);


    PageSeed ps = pageState.thisPage(request);
    ps.command = "";
    ps.mainObjectId = rootTask.getId();
    Form form = new Form(ps);
    form.alertOnChange = true;
    form.start(pageContext);
    pageState.setForm(form);




%>
<div class="mainColumn"><%

%><div class="container level_2">
<h1><%=I18n.get("TASK_LOGS_TAB")%></h1><%
  // ----------------------------------------------------------------------------- START PROCESS LOGS ---------------------------------------------------------------------------------------

  if (rootTask.isProcessDriven()) {

    ProcessInstance pi = rootTask.getProcess();
    ProcessDefinition def = pi.getProcessDefinition();


    TaskMgmtInstance tmi = pi.getTaskMgmtInstance();
    List<TaskInstance> tis = new ArrayList<TaskInstance>(tmi.getTaskInstances());
    Collections.sort(tis, new Comparator<TaskInstance>() {
      public int compare(TaskInstance ti1, TaskInstance ti2) {
        return ti1.getCreate().compareTo(ti2.getCreate());
      }
    });


%><h2><%=I18n.get("PROCESS_HISTORY") + " '" + pi.getProcessDefinition().getName() + "'"%></h2>
<table class="table fixHead" border="0">
  <tr>
    <th class="tableHead"><%=I18n.get("STEPS_COMPLETED_RUNNING")%></th>
    <th class="tableHead"><%=I18n.get(("ROLE")) + "/" + I18n.get("ASSIGNMENT")%></th>
    <th class="tableHead"><%=I18n.get(("START"))%></th>
    <th class="tableHead"><%=I18n.get(("END"))%></th>
    <th class="tableHead"><%=I18n.get(("DURATION"))%></th>
  </tr>
  <%
    boolean swap = true;
    Set<org.jbpm.taskmgmt.def.Task> touchedNodes = new HashSet<org.jbpm.taskmgmt.def.Task>();
    for (TaskInstance taskInstance : tis) {
  %>
  <tr class="data" style="height:30px">
    <%
      touchedNodes.add(taskInstance.getTask());
      final TaskNode taskNode = taskInstance.getTask().getTaskNode();

      Task t = ProcessUtilities.getTaskByJbpmTaskNode(rootTask, taskNode);

    %>
    <td><b><%
      String label = JSP.w(taskNode.getName()) + " " + (taskInstance.getName().equalsIgnoreCase(taskNode.getName()) ? "" : " / " + taskInstance.getName());
      if (t != null &&t.hasPermissionFor(logged,TeamworkPermissions.task_canRead)) {
        PageSeed taskEdit = pageState.pageFromRoot("task/taskOverview.jsp");
        taskEdit.command = Commands.EDIT;
        taskEdit.mainObjectId = t.getId();
        ButtonLink bltaskEdit = new ButtonLink("", taskEdit);
        bltaskEdit.label = label;
        bltaskEdit.toHtmlInTextOnlyModality(pageContext);
      } else {
    %><%=label%><%
      }%>
      </b></td>
    <td>
      <%
        Assignment ass = ProcessUtilities.getAssignmentByJbpmTask(rootTask, taskInstance.getTask());
        if (ass != null) {
          %> <%=ass.getRole().getName()%> / <%=ass.getResource().getDisplayName()%><%
        } else {
          %><%=I18n.get("UNASSIGNED")%><%
        }
    %>

    </td>
    <td><%=DateUtilities.dateAndHourToString(taskInstance.getCreate())%>
    </td>
    <% Date end = taskInstance.getEnd();%>
    <td><%=DateUtilities.dateAndHourToString(end)%>
    </td>
    <%
      if (end == null)
        end = new Date();
    %>
    <td align="center"><%=DateUtilities.getMillisInDaysHoursMinutes(end.getTime() - taskInstance.getCreate().getTime())%>
    </td>
  </tr>
  <%

    }
  %></table>
<%

  if (pi.getEnd() == null) {

%>
<table class="table fixHead">
  <tr>
    <th class="tableHead"><%=I18n.get("STEPS_WAITING")%></th>
    <th class="tableHead"><%=I18n.get(("ROLE")) + "/" + I18n.get("ASSIGNMENT")%></th>
  </tr>
  <%
    swap = true;
    for (Object n : def.getNodes()) {
      Node node = (Node) ReflectionUtilities.getUnderlyingObjectAsObject(n);

      if (node instanceof TaskNode) {

        TaskNode taskNode = (TaskNode) node;
        Task t = ProcessUtilities.getTaskByJbpmTaskNode(rootTask, taskNode);

        Set<org.jbpm.taskmgmt.def.Task> tset = taskNode.getTasks();
        for (org.jbpm.taskmgmt.def.Task jbpmTask : tset) {
          if (touchedNodes.contains(jbpmTask))
            continue;
  %>
  <tr class="data" style="height:30px">
    <td><%
      String label = JSP.w(taskNode.getName()) + " " + (jbpmTask.getName().equalsIgnoreCase(taskNode.getName()) ? "" : " / " + jbpmTask.getName());
      if (t != null && t.hasPermissionFor(logged,TeamworkPermissions.task_canRead)) {
        PageSeed taskEdit = pageState.pageFromRoot("task/taskEditor.jsp");
        taskEdit.command = Commands.EDIT;
        taskEdit.mainObjectId = t.getId();
        ButtonLink bltaskEdit = new ButtonLink("", taskEdit);
        bltaskEdit.label = label;
        bltaskEdit.toHtmlInTextOnlyModality(pageContext);
      } else {
    %><%=label%><%
      }%>
    </td>
    <td>
      <%
        Assignment ass = ProcessUtilities.getAssignmentByJbpmTask(rootTask, jbpmTask);
        if (ass != null) {
         %> <%=ass.getRole().getDisplayName()%> / <%=ass.getResource().getDisplayName()%><%
        } else {
          %><%=I18n.get("UNASSIGNED")%><%
        }
    %>
    </td>
  </tr>
  <%
        }
      }
    }
  %>
</table>
<%

    }

    // ----------------------------------------------------------------------------- END PROCESS LOGS ---------------------------------------------------------------------------------------
  } else {


  boolean showDesc = pageState.getEntryOrDefault("SHOW_LOG_ON_DESCENDANT").checkFieldValue();

  CheckField showLogOnDesc = new CheckField("SHOW_LOG_ON_DESCENDANT", "", false);
  ButtonSubmit scwkt = new ButtonSubmit(pageState.getForm());
  showLogOnDesc.additionalOnclickScript = scwkt.generateJs().toString();
  showLogOnDesc.preserveOldValue = false;
  showLogOnDesc.toHtmlI18n(pageContext);

  List<Task> allTasks = new ArrayList();
  allTasks.add(rootTask);
  if (showDesc)
    allTasks.addAll(rootTask.getDescendants());


%>
<h2><%=I18n.get("STATUS_CHANGE_HISTORY")%></h2>
<table class="table edged fixHead" cellpadding="4">

  <%
    // draw status history
    for (Task task : allTasks) {
      %><tr class="tableSection"><td colspan="99"><div class="childNode"><h2><%=JSP.w(task.getDisplayName())%></h2></div></td></tr><%

      for (TaskStatusHistory tsh: task.getStatusHistory()){
        %>
        <tr>
          <td width="120"><%=JSP.w(tsh.getLastModified())%></td>
          <td width="300"><%=I18n.get("CHANGED_BY")%> <b><%=tsh.getCreator()%></b></td>
          <td><%=I18n.get("STATUS_FROM")%> <b><%=I18n.get(tsh.getFromStatus())%></b> <%=I18n.get("STATUS_TO")%> <b><%=I18n.get(tsh.getToStatus())%></b>
          <%
            if (JSP.ex(tsh.getChangeLog()))  {
          %><br><%=I18n.get("NOTES")%>: <%=JSP.w(tsh.getChangeLog())%><%
              }
          %></td>
        </tr>
        <%
      }
    }
    %>
</table><%


%><h2><%=I18n.get("DATE_CHANGES")%></h2>
<table class="table edged fixHead" cellpadding="4">
<%
  // draw status history
  for (Task task : allTasks) {
    %><tr class="tableSection"><td colspan="99"><div class="childNode"><h2><%=JSP.w(task.getDisplayName())%></h2></div></td></tr><%

    for (TaskScheduleHistory tsh: task.getScheduleHistory()){
      if (tsh.getSchedule() != null) {
        %>
        <tr>
        <td  width="120"><%=JSP.w(tsh.getLastModified())%></td>
          <td  width="300"><%=I18n.get("CHANGED_BY")%> <b><%=tsh.getCreator()%></b></td>
        <td nowrap><%=I18n.get("PREVIOUS_PERIOD")%>: <%=JSP.w(tsh.getSchedule().getStartDate()) + " - " + JSP.w(tsh.getSchedule().getEndDate())%><%
          if (JSP.ex(tsh.getChangeLog())) {
        %>
          <br><%=I18n.get("NOTES")%>: <%=JSP.w(tsh.getChangeLog())%><%
            }
          %></td>

        </tr>
        <%
      }
    }
  }
  %>
  <tr><td colspan="99" style="border-bottom:dotted 1px black;">&nbsp;</td></tr>
</table><%


  }
    %></div></div><%


    form.end(pageContext);
    
  }
%>
