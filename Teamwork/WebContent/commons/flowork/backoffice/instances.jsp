<%@ page import=" org.jblooming.flowork.FlowPermissions,
                  org.jblooming.flowork.PlatformJbpmSessionFactory,
                  org.jblooming.operator.Operator,
                  org.jblooming.oql.OqlQuery,
                  org.jblooming.utilities.DateUtilities, org.jblooming.waf.ScreenBasic,
                  org.jblooming.waf.constants.Commands,
                  org.jblooming.waf.constants.Fields,
                  org.jblooming.waf.html.button.ButtonLink,
                  org.jblooming.waf.html.button.ButtonSubmit,
                  org.jblooming.waf.html.container.ButtonBar,
                  org.jblooming.waf.html.container.Container,
                  org.jblooming.waf.html.state.Form,
                  org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jbpm.JbpmContext, org.jbpm.db.GraphSession, org.jbpm.graph.def.Node, org.jbpm.graph.def.ProcessDefinition, org.jbpm.graph.def.Transition, org.jbpm.graph.exe.ProcessInstance, java.util.List, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.settings.I18n" %>
<%!
  void surfTree(Node current, List tree, ProcessDefinition def) {
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


  JbpmContext jbpmSession = PlatformJbpmSessionFactory.getJbpmContext(pageState);
  GraphSession gs = jbpmSession.getGraphSession();
  ProcessDefinition def = gs.loadProcessDefinition(Long.parseLong(pageState.mainObjectId.toString()));

  if (!pageState.screenRunning) {
    ScreenBasic screenBasic = ScreenBasic.preparePage(null, pageContext);
    screenBasic.getBody().areaHtmlClass="lreq30 lreqPage";

    //------------------- CONTROLLER
    if ("INSTANTIATE".equals(pageState.getCommand())) {
      pageState.initializeEntries("table");
      ProcessInstance processInstance = new ProcessInstance(def);
      processInstance.getRootToken().signal();
      jbpmSession.save(processInstance);

    } else if ("CANCELINSTANCE".equals(pageState.getCommand())) {
      pageState.initializeEntries("table");
      long instId = pageState.getEntry("INSTANCEID").longValue();
      ProcessInstance pi = gs.loadProcessInstance(instId);
      pi.end();
    }

    pageState.perform(request, response).toHtml(pageContext);

  } else {

    OqlQuery oqlQuery = new OqlQuery("select pi from " + ProcessInstance.class.getName() + " as pi where pi.end is null and pi.processDefinition.id=:pid order by pi.start desc");
    oqlQuery.getQuery().setLong("pid",def.getId());
    List<ProcessInstance> open = oqlQuery.getQuery().list();


    oqlQuery = new OqlQuery("select pi from " + ProcessInstance.class.getName() + " as pi where pi.end is not null and pi.processDefinition.id=:pid order by pi.end desc");
    oqlQuery.getQuery().setLong("pid",def.getId());
    oqlQuery.getQuery().setMaxResults(10);
    List<ProcessInstance> closed = oqlQuery.getQuery().list();


    PageSeed me = pageState.thisPage(request);
    me.addClientEntry(pageState.getEntry(Fields.APPLICATION_NAME));
    me.setCommand(Commands.SAVE);
    me.setMainObjectId(def.getId());
    Form form = new Form(me);
    form.start(pageContext);

    Container main = new Container();
    main.title = I18n.get("FLUX_INSTANCES_FOR") + "&nbsp;" + def.getName() + " version " + def.getVersion();
    main.start(pageContext);

/*
________________________________________________________________________________________________________________________________________________________________________


RUNNING_FLUXES

________________________________________________________________________________________________________________________________________________________________________

*/
%><p align="center"> <%

  PageSeed psEdit = new PageSeed("instance.jsp");
  ButtonLink edit = new ButtonLink(psEdit);
  edit.label = I18n.get("FLUX_INSTANCE_STATE");

    PageSeed psGraph = pageState.pageFromCommonsRoot("/flowork/backoffice/nodesInstancePlus.jsp");
    ButtonLink blGraph = new ButtonLink(psGraph);
    blGraph.label = "graph";


  PageSeed psCancel = pageState.thisPage(request);
  psCancel.setCommand("CANCELINSTANCE");
  psCancel.setMainObjectId(def.getId());



  PageSeed processHistory = new PageSeed("processHistory.jsp");
            processHistory.setCommand(Commands.EDIT);
            ButtonLink phl = new ButtonLink(processHistory);
  phl.label ="log";

  Container df = new Container("rf",1);
  df.title = I18n.get("RUNNING_FLUXES");
  df.width="98%";
  df.start(pageContext);

   %>
<table width="90%" align="center" border="0" class="table">

  <tr>
    <th class="tableHead"><%=I18n.get("INSTANCE")%></th>
    <th class="tableHead"><%=I18n.get("STATUS")%></th>
    <th class="tableHead"><%=I18n.get("FLD_START")%></th>
    <th class="tableHead">&nbsp;</th>
  </tr>
  <%
    for (ProcessInstance processInstance : open) {
      psEdit.addClientEntry("INSTANCEID", processInstance.getId());
      psCancel.addClientEntry("INSTANCEID", processInstance.getId());
      processHistory.setMainObjectId(processInstance.getId());
      psEdit.setMainObjectId(processInstance.getId());

      psGraph.addClientEntry("PROCESS_INSTANCE_ID", processInstance.getId());

      ButtonJS cancel = new ButtonJS("window.location.href='"+psCancel.toLinkToHref()+"'");
      cancel.label = I18n.get("FLUX_INSTANCE_CANCEL");
      cancel.confirmRequire=true;
      cancel.confirmQuestion=I18n.get("DO_YOU_CONFIRM");

  %>
  <tr class="alternate" >
    <td><%=processInstance.getId()%></td>
    <td><%=processInstance.hasEnded() ? "ended" : "RUNNING"%></td>
    <td><%=DateUtilities.dateAndHourToFullString(processInstance.getStart())%></td>
    <td align="center">
      <table>
        <tr>
          <td><%edit.toHtml(pageContext);%></td>
          <td><%cancel.toHtml(pageContext);%></td>
          <td><%phl.toHtml(pageContext);%></td>
          <td><%blGraph.toHtml(pageContext);%></td>
        </tr>
      </table>
    </td>
  </tr>
  <%
    }
  %>


</table>
<%


  df.end(pageContext);

%><br><%
  /*
  ________________________________________________________________________________________________________________________________________________________________________


  CLOSED_FLUXES

  ________________________________________________________________________________________________________________________________________________________________________

  */
  Container dfc = new Container("cf",1);
  dfc.title = I18n.get("CLOSED_FLUXES")+" ("+I18n.get("TOP10")+")";
  dfc.width = "98%";
  dfc.collapsable = true;
  dfc.status = Container.COLLAPSED;
  dfc.start(pageContext);


%>
<table width="90%" align="center" border="0" class="table">

  <tr>
    <th class="tableHead"><%=I18n.get("INSTANCE")%></th>
    <th class="tableHead"><%=I18n.get("STATUS")%></th>
    <th class="tableHead"><%=I18n.get("FLD_START")%></th>
    <th class="tableHead"><%=I18n.get("FLD_END")%></th>
    <th class="tableHead">&nbsp;</th>
  </tr>
  <%


    for (ProcessInstance processInstance : closed) {
      processHistory.setMainObjectId(processInstance.getId());


  %>
  <tr class="alternate" >
    <td><%=processInstance.getId()%></td>
    <td><%=processInstance.hasEnded() ? "ended" : "RUNNING"%></td>
    <td><%=DateUtilities.dateAndHourToString(processInstance.getStart())%></td>
    <td><%=DateUtilities.dateAndHourToString(processInstance.getEnd())%></td>
    <td align="center">
      <%phl.toHtml(pageContext);%>
    </td>    
  </tr>
  <%
    }


  %></table><%

  dfc.end(pageContext);

  ButtonBar bb = new ButtonBar();

  ButtonLink fp = new ButtonLink(new PageSeed("deployList.jsp"));
  fp.label = I18n.get("FLUX_PUBLISHED");
  bb.addButton(fp);

  PageSeed psInst = new PageSeed("nodes.jsp");
  psInst.setMainObjectId(def.getId());
  ButtonLink inst = new ButtonLink(psInst);
  inst.label = I18n.get("FLUX_NODES");
  bb.addButton(inst);

  /*ButtonSubmit instNew = new ButtonSubmit(form);
  instNew.variationsFromForm.setCommand("INSTANTIATE");
  instNew.label = I18n.get("FLUX_INSTANTIATE");
  bb.addButton(instNew);*/

  bb.toHtml(pageContext);



  main.end(pageContext);

form.end(pageContext);

  }%>
