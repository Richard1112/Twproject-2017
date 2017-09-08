<%@ page import=" org.jblooming.flowork.FlowFields,
                  org.jblooming.flowork.PlatformJbpmSessionFactory,
                  org.jblooming.flowork.waf.FloworkPopUpScreen,
                  org.jblooming.persistence.PersistenceHome,
                  org.jblooming.system.SystemConstants,
                  org.jblooming.utilities.DateUtilities,
                  org.jblooming.utilities.JSP,
                  org.jblooming.waf.ScreenArea,
                  org.jblooming.waf.html.container.Container,
                  org.jblooming.waf.html.display.Img,
                  org.jblooming.waf.settings.ApplicationState,
                  org.jblooming.waf.settings.I18n,
                  org.jblooming.waf.view.PageState,
                  org.jbpm.JbpmContext,
                  org.jbpm.db.GraphSession,
                  org.jbpm.graph.def.Node,
                  org.jbpm.graph.def.ProcessDefinition,
                  org.jbpm.graph.exe.ProcessInstance,
                  org.jbpm.graph.node.TaskNode,
                  org.jbpm.taskmgmt.def.Swimlane,
                  org.jbpm.taskmgmt.def.Task,
                  org.jbpm.taskmgmt.exe.TaskInstance, org.jbpm.taskmgmt.exe.TaskMgmtInstance, java.util.*"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(null, request);
    FloworkPopUpScreen lw = new FloworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);
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

    Img print = new Img("printIcon.png", pageState.getI18n("PRINT_PAGE"), "20", "20");
    Img logo = new Img(ApplicationState.getApplicationSetting(SystemConstants.PRINT_LOGO), "");
%>
<table class="table" border="0" align="center" cellpadding="5" cellspacing="0">
      <tr>
        <td width="1%" align="left" class="textSmallDark">
            <a href="javascript:obj('printButton').style.visibility='hidden';window.print();window.close()">
              <div id="printButton" style="visibility:visible;" >
                <%print.toHtml(pageContext);%>
                <%=pageState.getI18n("PRINT_PAGE")%>
              </div>
            </a>
          </td>
        <td width="79%" valign="middle" align="center" style="font-size:18px;"><b><%=pageState.getI18n("FLUX_HISTORY")%></b></td><td valign="middle" align="right" width="20%"><%logo.toHtml(pageContext);%></td>
      </tr>
</table>
<%
    Container main = new Container("1",0);
    main.title = I18n.get("FLUX_LOG_INST%%_PROC%%",pi.getId()+"", pi.getProcessDefinition().getName());
    main.start(pageContext);

%>
<br>
<%
    Container logs = new Container("2",1);
    logs.title = pageState.getI18n("STEPS_DONE");
    logs.start(pageContext);
%>
<table class="table">
<tr>
  <td height="18px" ><b>&nbsp;<%=pageState.getI18n("ROLE")%></b></td>
  <td height="18px" ><b>&nbsp;<%=pageState.getI18n("STEP")%></b></td>
  <td height="18px" ><b>&nbsp;<%=pageState.getI18n("START")%></b></td>
  <td height="18px" ><b>&nbsp;<%=pageState.getI18n("END")%></b></td>
  <td height="18px" ><b>&nbsp;<%=pageState.getI18n("DURATION")%></b></td>
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
    <td>&nbsp;<%=descr%></td>
            <%
              touchedNodes.add(ti.getTask());
                String stepName = JSP.w(ti.getName());
                FlowFields ff = (FlowFields) PersistenceHome.findUnique(FlowFields.class, "flowName", def.getName());
                if(ff != null && ff.getNodeDescriptions() != null && ff.getNodeDescriptions().containsKey(ti.getName()))
                  stepName+=": "+ff.getNodeDescriptions().get(ti.getName());
            %>
            <td>&nbsp;<%=stepName%></td>
            <td>&nbsp;<%=DateUtilities.dateToString(ti.getCreate(),"dd MMM yyyy HH:mm")%></td>
            <% Date end = ti.getEnd();%>
            <td><%=DateUtilities.dateToString(end,"dd MMM yyyy HH:mm")%></td>
  <%if (end==null)
    end = new Date();
  %>
            <td>&nbsp;<%=DateUtilities.getMillisInHoursMinutes(end.getTime()-ti.getCreate().getTime())%></td>
          </tr><%

    }
    %></table><%

    logs.end(pageContext);


  if (pi.getEnd()==null) {
%><br><%
    Container cpe = new Container("3",1);
  cpe.title = pageState.getI18n("STEPS_TO_BE_DONE");
    cpe.start(pageContext);


  %><table class="table">
  <tr>
    <td height="18px" ><b>&nbsp;<%=pageState.getI18n("ROLE")%></b></td>
    <td height="18px" ><b>&nbsp;<%=pageState.getI18n("STEP")%></b></td>
  </tr><%

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

        %><tr  class="alternate" >
            <td>&nbsp;<%=descr%></td>
            <td>&nbsp;<%=tn.getName()%></td>
        </tr><%
      }
    }
    %></table><%
    cpe.end(pageContext);

    }
     main.end(pageContext);
  }
%>