<%@ page import="org.jblooming.flowork.PlatformJbpmSessionFactory, org.jblooming.operator.Operator, org.jblooming.oql.OqlQuery, org.jblooming.utilities.ReflectionUtilities, org.jblooming.waf.ScreenBasic, org.jblooming.waf.SessionState, org.jblooming.waf.html.container.Container, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, org.jbpm.JbpmContext, org.jbpm.db.GraphSession, org.jbpm.graph.def.ProcessDefinition, org.jbpm.graph.exe.ProcessInstance, org.jbpm.graph.log.ProcessInstanceCreateLog, org.jbpm.graph.log.SignalLog, org.jbpm.graph.log.TransitionLog, org.jbpm.logging.log.ProcessLog, org.jbpm.taskmgmt.log.TaskLog, java.util.List"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  final Operator logged = pageState.getLoggedOperator();
  boolean isFlowManager = logged != null;//todo: && (logged.hasPermissionFor(FlowPermissions.canManageFlows));
  if (logged == null || !isFlowManager)
    throw new SecurityException("No permission for accessing page " + request.getRequestURI());

  if (!pageState.screenRunning) {
    ScreenBasic.preparePage(null, pageContext);
    pageState.perform(request, response).toHtml(pageContext);

  } else {

    JbpmContext jbpmSession = PlatformJbpmSessionFactory.getJbpmContext(pageState);

    GraphSession gs = jbpmSession.getGraphSession();

/*
________________________________________________________________________________________________________________________________________________________________________


  controller

________________________________________________________________________________________________________________________________________________________________________

*/
    long instId = pageState.getEntry("INSTANCEID").longValue();
    ProcessInstance pi = gs.loadProcessInstance(instId);
    ProcessDefinition pd = pi.getProcessDefinition();

/*
________________________________________________________________________________________________________________________________________________________________________


  view

________________________________________________________________________________________________________________________________________________________________________

*/

    Container df = new Container();
    df.title = I18n.get("FLUX_LOG_FOR_%%_OF_%%", pi.getId() + "", pd.getName());
    df.start(pageContext);

%><table class="table">
        <tr>
          <th>ID</th>
          <th> DATE</th>
          <th> TOKEN NODE</th>
          <th> TYPE</th>
          <th> DETAILS</th>
        </tr> <%

   String hql = "select il from "+ProcessLog.class.getName()+" as il where il.token.processInstance=:pi order by il.date";
   final OqlQuery oqlQuery = new OqlQuery(hql);
   oqlQuery.getQuery().setEntity("pi",pi);
   List<ProcessLog> logs =  oqlQuery.list(); //executionService.findByQuery(freq);
   /*PageSeed undo = new PageSeed("log.jsp");
   final ButtonLink undoBut = new ButtonLink(undo);
   undoBut.label = "try to undo";
   undo.setCommand("UNDO");*/
   for (org.jbpm.logging.log.ProcessLog il : logs) {

    %><tr class="alternate" >
    <td><%=il.getId()%></td>
    <td><%=il.getDate()%></td>
    <td><%=il.getToken().getNode().getName()%></td>
    <td><%=ReflectionUtilities.unqualify(il.getClass().getName())%></td>
    <td><%

    if (il instanceof TransitionLog) {
      TransitionLog tl = (TransitionLog)il;
      
    } else  if (il instanceof ProcessInstanceCreateLog) {
    } else  if (il instanceof SignalLog) {
    } else  if (il instanceof TaskLog) {
    }

    %></td> <%

    %></tr><%
  }
  %></table><%

  df.end(pageContext);

  }
%>