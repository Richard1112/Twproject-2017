<%@ page import="org.jblooming.flowork.FlowPermissions,
                 org.jblooming.flowork.PlatformJbpmSessionFactory,
                 org.jblooming.flowork.security.FloworkOperator,
                 org.jblooming.operator.Operator,
                 org.jblooming.oql.OqlQuery,
                 org.jblooming.waf.ScreenBasic,
                 org.jblooming.waf.SessionState,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.container.Container,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState,
                 org.jbpm.JbpmContext,
                 org.jbpm.db.GraphSession,
                 org.jbpm.graph.def.ProcessDefinition, 
                 org.jbpm.taskmgmt.def.Swimlane,
                 org.jbpm.taskmgmt.def.TaskMgmtDefinition,
                 java.util.HashMap,
                 java.util.Iterator,
                 java.util.List,
                 java.util.Map"%>
<%

  PageState pageState = PageState.getCurrentPageState(request);
  final Operator logged = pageState.getLoggedOperator();
  boolean isFlowManager = logged != null && (logged.hasPermissionFor(FlowPermissions.canManageFlows));
  if (logged == null || !isFlowManager)
    throw new SecurityException("No permission for accessing page " + request.getRequestURI());

  if (!pageState.screenRunning) {
    ScreenBasic.preparePage(null, pageContext);
    pageState.perform(request, response).toHtml(pageContext);

  } else {

    JbpmContext jbpmSession = PlatformJbpmSessionFactory.getJbpmContext(pageState);
    GraphSession gs = jbpmSession.getGraphSession();

    ProcessDefinition def = gs.loadProcessDefinition(Long.parseLong(pageState.mainObjectId.toString()));

    Map<String, Swimlane> sws = new HashMap<String, Swimlane>();
    TaskMgmtDefinition taskMgmtDefinition = def.getTaskMgmtDefinition();
    if (taskMgmtDefinition != null) {
      sws = taskMgmtDefinition.getSwimlanes();
    }


%><p align="center"> <%

    Container c = new Container();

    c.title = pageState.getI18n("ROLES_FLUX")+" "+def.getName();
    c.width="98%";
    c.start(pageContext);

    if (sws != null && sws.size() > 0) {

      String hql = "from " + FloworkOperator.class.getName();
      OqlQuery oql = new OqlQuery(hql);
      List swimmers = oql.list();

      %><table class="table">
                  <tr>
                    <th><%=pageState.getI18n("ROLE")%></th>
                    <th><%=pageState.getI18n("OPERATOR")%></th>
                  </tr><%

      for (Iterator iterator = sws.keySet().iterator(); iterator.hasNext();) {
        String s = (String) iterator.next();
        Swimlane sw = (Swimlane)sws.get(s);
        %><tr class="alternate" >
              <td><span title="<%=sw.getId()%>"><%=s%></span></td><%

        boolean someDoesIt = false;
        %><td><%
        for (int i = 0; i < swimmers.size(); i++) {
          FloworkOperator swimmer = (FloworkOperator) swimmers.get(i);
          if(swimmer.getSwimminLanes().contains(s)){
            %><%="<span title=\""+swimmer.getId()+"\">"+swimmer.getFullname()+"</span>"%>&nbsp;<%
             someDoesIt = true;
          }
        }
        %><%
        if (!someDoesIt) {
          %><%=pageState.getI18n("FLUX_ROLE_NOT_ASSOCIATED")%><%
        }
        %></td></tr><%
      }
        %>
          <tr>
            <td colspan="2" align="right">
  <table><tr><td>
  <%
   ButtonLink fp = new ButtonLink(new PageSeed("deployList.jsp"));
   fp.label =pageState.getI18n("FLUX_PUBLISHED");
   fp.toHtml(pageContext);

  %></td><td><%

   PageSeed psInst = new PageSeed("instances.jsp");
   psInst.setMainObjectId(def.getId());
   ButtonLink inst = new ButtonLink(psInst);
   inst.label =pageState.getI18n("INSTANCES");
   inst.toHtml(pageContext);

  %>
  </td></tr></table>
            </td>
          </tr>
          </table>
      <%

    }
    c.end(pageContext);

  }

%>