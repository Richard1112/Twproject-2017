<%@ page
        import="org.jblooming.flowork.FlowPermissions, org.jblooming.flowork.PlatformJbpmSessionFactory, org.jblooming.operator.Operator, org.jblooming.oql.OqlQuery, org.jblooming.persistence.hibernate.HibernateFactory, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.utilities.ReflectionUtilities, org.jblooming.waf.ScreenBasic, org.jblooming.waf.SessionState, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.state.Form, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jbpm.JbpmContext, org.jbpm.db.GraphSession, org.jbpm.graph.def.Node, org.jbpm.graph.def.ProcessDefinition, org.jbpm.graph.def.Transition, org.jbpm.graph.exe.ProcessInstance, org.jbpm.graph.exe.Token, org.jbpm.graph.node.TaskNode, org.jbpm.taskmgmt.def.Task, org.jbpm.taskmgmt.exe.TaskInstance, java.util.List" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  final Operator logged = pageState.getLoggedOperator();
  boolean isFlowManager = logged.hasPermissionFor(FlowPermissions.canManageFlows);
  if (logged == null || !isFlowManager)
    throw new SecurityException("No permission for accessing page " + request.getRequestURI());

  if (!pageState.screenRunning) {
    ScreenBasic.preparePage(null, pageContext);
    pageState.perform(request, response).toHtml(pageContext);
  } else {

    JbpmContext jbpmSession = PlatformJbpmSessionFactory.getJbpmContext(pageState);
    GraphSession gs = jbpmSession.getGraphSession();

    long instId = Long.parseLong(pageState.getMainObjectId() + "");
    ProcessInstance pi = gs.loadProcessInstance(instId);
    ProcessDefinition def = pi.getProcessDefinition();

    //----------------------------------------------  START  CONTROLLER ----------------------------------------
    if ("DOSTEP".equals(pageState.command)) {

      Transition transition = (Transition) HibernateFactory.getSession().load(Transition.class, pageState.getEntry("TRANS").longValue());
      Token token = (Token) HibernateFactory.getSession().load(Token.class, pageState.getEntry("TOKENID").longValue());


      Node node = (Node) ReflectionUtilities.getUnderlyingObjectAsObject(token.getNode());

      if (node instanceof TaskNode) {

        String hql = "from " + TaskInstance.class.getName() + " as ti where ti.token = :tk";
        OqlQuery oql = new OqlQuery(hql);
        oql.getQuery().setEntity("tk", token);
        List<TaskInstance> tis = oql.list();
        for (TaskInstance ti : tis) {
          if (ti.getEnd() == null){
            ti.end(transition);
            jbpmSession.save(ti);
          }
        }

      } else {
        token.signal(transition);
      }

    }
    //----------------------------------------------  END  CONTROLLER ----------------------------------------


    PageSeed ps = pageState.thisPage(request);
    ps.setCommand("VOID");
    ps.setMainObjectId(instId);
    ps.addClientEntry("TRANS", "VOID");
    ps.addClientEntry("TOKENID", "VOID");
    Form form = new Form(ps);

    form.start(pageContext);


    Container df = new Container();
    df.title = pageState.getI18n("FLUX_INSTANCE_MANAGEMENT");
    df.width = "98%";

    df.start(pageContext);


%>
  <table width="90%" align="center" border="0" class="table">
    <tr>
      <th colspan=6>
        <%=def.getName()%>
        <br><small>(id: <%=pi.getId()%> &nbsp; ver. <%=def.getVersion()%>)</small>
      </th>
    </tr>
    <tr>
      <th>ID</th>
      <th> <%=pageState.getI18n("NAME")%></th>
      <th> <%=pageState.getI18n("RESOURCE")%></th>
      <th> <%=pageState.getI18n("START")%> </th>
      <th>&nbsp;</th>
    </tr>
    <%

      String hql = "select p from " + Token.class.getName() + " as p where p.processInstance = :instance";
      OqlQuery j = new OqlQuery(hql);
      j.setParameter("instance", pi);
      List<Token> tokensForInstance = j.list();
      boolean swap = true;

      if (JSP.ex(tokensForInstance)) {
        for (Token token : tokensForInstance) {
          Node node = token.getNode();
          if (!token.hasEnded() && ReflectionUtilities.getUnderlyingObjectAsObject(node) instanceof TaskNode) {
              %> <tr class="alternate" >
                      <td><%=token.getId()%></td>
                      <td><%=node.getName()%>
                        <%
                  if (JSP.ex(node.getDescription())){
                    %><div style="font-size:smaller;"><%=node.getDescription()%></div><%
                  }
              %>
            </td>
            <td>
              <%
                if (node instanceof TaskNode){
                  TaskNode taskNode = (TaskNode) node;
                  Task task = (Task) taskNode.getTasks().iterator().next();

                  String descr="";
                  if (task.getSwimlane()==null)
                    descr= "Delegation "+task.getAssignmentDelegation().getInstance().getClass().getSimpleName()+" "+JSP.htmlEncode(task.getAssignmentDelegation().getConfiguration());
                  else
                    descr=task.getSwimlane().getName();

                  %><%=descr%><%

                } else {
                  %><%=pageState.getI18n("NOT_ASSIGNED")%><%
                }
              %>
            </td>
            <td><%=token.getStart() != null ? DateUtilities.dateAndHourToFullString(token.getStart()) : "-"%></td>
            <td align="center">
              <table class="table">
                <tr>
                  <th><%=pageState.getI18n("FLUX_ACTIONS")%></th>
                  <th><%=pageState.getI18n("FLUX_PROCEED_TO")%></th>
                </tr>
                <%
                Node n = node;
                ButtonSubmit step = new ButtonSubmit(form);
                step.variationsFromForm.command = "DOSTEP";
                boolean swap2=true;
                List<Transition> trans = n.getLeavingTransitions();
                for (org.jbpm.graph.def.Transition transition : trans) {
                  %><tr >
                    <td><%
                  step.label = transition.getName()!=null ? transition.getName() : "STEP";
                step.variationsFromForm.addClientEntry("TRANS",transition.getId());
                step.variationsFromForm.addClientEntry("TOKENID",token.getId());
                step.confirmRequire=true;
                step.confirmQuestion="Are you sure? This may corrupt the entire flux process.";
                step.toHtml(pageContext);

                if (JSP.ex(transition.getDescription())){
                  %><div style="font-size:smaller;"><%=transition.getDescription()%></div><%
                }
            %>
                <td><%=transition.getTo().getName()%></td>

              </tr><%
                  }
                %></table>
                </td><%
          }
        }
  } else {
  %><tr><td colspan=6><%=pageState.getI18n("NO_TOKEN_AVAILABLE")%></td></tr> <%
    }
  %>
  </table>


  <%

    ButtonBar bb= new ButtonBar();
    ButtonLink fp = new ButtonLink(new PageSeed("deployList.jsp"));
    fp.label = pageState.getI18n("FLUX_PUBLISHED");
    bb.addButton(fp);

    PageSeed psInst = new PageSeed("instances.jsp");
    psInst.setMainObjectId(def.getId());
    ButtonLink inst = new ButtonLink(psInst);
    inst.label = pageState.getI18n("FLUX_INSTANCES");
    bb.addButton(inst);

    PageSeed psNode = new PageSeed("nodes.jsp");
    psNode.setMainObjectId(def.getId());
    ButtonLink nodes =  new ButtonLink(psNode);
    nodes.label = pageState.getI18n("FLUX_NODES");
    bb.addButton(nodes);

    bb.toHtml(pageContext);
    df.end(pageContext);

    form.end(pageContext);

  }

%>
