<%@ page import=" org.jblooming.flowork.FlowFields,
                  org.jblooming.flowork.FlowPermissions,
                  org.jblooming.flowork.PlatformJbpmSessionFactory,
                  org.jblooming.operator.Operator,
                  org.jblooming.persistence.PersistenceHome,
                  org.jblooming.utilities.JSP,
                  org.jblooming.waf.ScreenBasic,
                  org.jblooming.waf.SessionState,
                  org.jblooming.waf.html.button.ButtonLink,
                  org.jblooming.waf.html.button.ButtonSupport,
                  org.jblooming.waf.html.container.ButtonBar,
                  org.jblooming.waf.html.container.Container,
                  org.jblooming.waf.settings.I18n,
                  org.jblooming.waf.view.PageSeed,
                  org.jblooming.waf.view.PageState,
                  org.jbpm.JbpmContext,
                  org.jbpm.db.GraphSession,
                  org.jbpm.graph.def.Node,
                  org.jbpm.graph.def.ProcessDefinition,
                  org.jbpm.graph.def.Transition,
                  org.jbpm.graph.node.TaskNode,
                  org.jbpm.taskmgmt.def.Swimlane,
                  org.jbpm.taskmgmt.def.Task,
                  java.util.Collection, java.util.List, java.util.Set" %>
<%!
  void surfTree(Node current, List<Node> tree, ProcessDefinition def) {
    if (current == null || tree.contains(current))
      return;
    tree.add(current);
    for (Object transitionO : current.getLeavingTransitions()) {
      Transition transition = (Transition) transitionO;
      //if (!transition.getTo().equals(def.getEndState()))
      surfTree(transition.getTo(), tree, def);
    }
  }
%><%
  PageState pageState = PageState.getCurrentPageState(request);
  final Operator logged = pageState.getLoggedOperator();
  boolean isFlowManager = logged != null && (logged.hasPermissionFor(FlowPermissions.canManageFlows));
  if (logged == null || !isFlowManager)
    throw new SecurityException("No permission for accessing page " + request.getRequestURI());

  if (!pageState.screenRunning) {
    ScreenBasic.preparePage(null, pageContext);
    pageState.perform(request, response).toHtml(pageContext);

  } else {

    JbpmContext jbpmContext = PlatformJbpmSessionFactory.getJbpmContext(pageState);
    GraphSession gs = jbpmContext.getGraphSession();

    ProcessDefinition def = gs.loadProcessDefinition(Long.parseLong(pageState.mainObjectId.toString()));


%><p align="center"> <%

    Container df = new Container();
    df.title =I18n.get("FLUX_STRUCTURE");
    df.start(pageContext);

%>
<table width="90%" align="center" border="0" class="table">
  <tr>
    <th colspan="99">
      <%=def.getName()%>
      <br><small>(ver. <%=def.getVersion()%> id:<%=def.getId()%>)</small>&nbsp;

    </th>
  </tr>
  <tr>
    <th class="tableHead"><%=I18n.get("STEP_NAME")%></th>
    <th class="tableHead">tasks</th>
    <th class="tableHead"><%=I18n.get("ROLES")%></th>
    <th class="tableHead lreq30 lreqLabel"><%=I18n.get("FORM_DEFINITION")%></th>
    <th class="tableHead"><%=I18n.get("TRANSITION")%></th>
  </tr>

  <%
    List sn = def.getNodes();//Nnew ArrayList();
    //surfTree(def.getStartState(), sn, def);
    // sn.add(def.getEndState());
    for (Object o : sn) {
      Node node = (Node) o;
      String nodeName = node.getName()+"";
      String taskDescs = "";
      String swimlanesDescs = "";



      if (node instanceof TaskNode) {
        TaskNode tn = (TaskNode) node;
        Set<Task> tset = tn.getTasks();
        boolean first = true;
        for (Task task : tset) {

          taskDescs = taskDescs + (!first ? "<br>" : "") + "<span title=\""+(JSP.ex(task.getDescription())?task.getDescription():"")+"\">"+task.getName()+"</span>";
          Swimlane s = task.getSwimlane();

          swimlanesDescs = swimlanesDescs + (!first ? "<br>" : "") + (s != null ? s.getName() + "&nbsp;" : "no swiml.for " + task.getName() + "&nbsp;");
          first = false;
        }
      }


  %>
  <tr class="alternate" >
    <td title="id:<%=node.getId()%>" id="step_<%=nodeName.replace(' ','_')%>"><%=nodeName%>
    <%
       FlowFields ff = (FlowFields) PersistenceHome.findUnique(FlowFields.class, "flowName", def.getName());
      if (ff!=null) {
        %><br><i><%=JSP.w(ff.getNodeDescriptions().get(node.getName()))%></i><%      
      }
    %><%
        if (JSP.ex(node.getDescription())){
          %><div style="font-size:smaller;"><%=node.getDescription()%></div><%
        }
    %></td>
    <td align="center"><%=taskDescs%></td>
    <td align="center"><%=swimlanesDescs%></td>
    <td align="center" class="lreq30"><%
      if (node instanceof TaskNode) {
        PageSeed ps = pageState.pageInThisFolder("variables.jsp",request);
        ps.mainObjectId = def.getId();
        ps.addClientEntry("STEP_ID",node.getName());
        ButtonLink vars = new ButtonLink(ps);
        vars.label = I18n.get("FORM_DEFINITION")+" ("+ FlowFields.definitionStepFieldNumber(node.getName(),def.getName())+")";
        vars.toHtmlInTextOnlyModality(pageContext);
      } else {
        %>&nbsp;<%
      }

    %>  </td>
    <td>
      <%
        Collection lec = node.getLeavingTransitions();
        if (lec!=null) {
        for (Object o3 : lec) {
          Transition transition = (Transition) o3;

          String transname = transition.getName();
          Node dest = transition.getTo();


      %><a href="#"><span
      <% if (dest != null) { %>
      onMouseOver="obj('step_<%=dest.getName().replace(' ','_')%>').style.backgroundColor='#ffff00';"
      onMouseOut="obj('step_<%=dest.getName().replace(' ','_')%>').style.backgroundColor='';"
      <%  } %>
      >
      <%=(transname != null ? transname + " : " : "") + (dest != null ? dest.getName() : "")%>
    </span></a> <%
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
<%
    df.end(pageContext);

    ButtonBar bb= new ButtonBar();

    PageSeed psNodes = new PageSeed("nodesPlus.jsp");
    psNodes.mainObjectId = pageState.mainObjectId;
    ButtonSupport graph = ButtonLink.getBlackInstance(I18n.get("GRAPH"),768,1024,psNodes);
    bb.addButton(graph);

    bb.toHtml(pageContext);


  }
%>
