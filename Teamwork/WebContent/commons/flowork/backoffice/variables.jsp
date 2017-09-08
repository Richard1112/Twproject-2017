<%@ page import="org.jblooming.flowork.FlowFields, org.jblooming.flowork.FlowPermissions, org.jblooming.flowork.PlatformJbpmSessionFactory, org.jblooming.flowork.businessLogic.FlowFormSetupController, org.jblooming.operator.Operator, org.jblooming.waf.ScreenBasic,
                 org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.input.Collector,
                 org.jblooming.waf.html.input.TextArea, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jbpm.JbpmContext,
                 org.jbpm.db.GraphSession, org.jbpm.graph.def.ProcessDefinition, java.util.TreeMap" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  final Operator logged = pageState.getLoggedOperator();
  boolean isFlowManager = logged != null && (logged.hasPermissionFor(FlowPermissions.canManageFlows));
  if (logged == null || !isFlowManager)
    throw new SecurityException("No permission for accessing page " + request.getRequestURI());

  if (!pageState.screenRunning) {
    ScreenBasic screenBasic = ScreenBasic.preparePage(new FlowFormSetupController(), pageContext);
    screenBasic.getBody().areaHtmlClass="lreq30 lreqPage";
    pageState.perform(request, response).toHtml(pageContext);

  } else {

    JbpmContext jbpmSession = PlatformJbpmSessionFactory.getJbpmContext(pageState);
    GraphSession gs = jbpmSession.getGraphSession();
    ProcessDefinition def = gs.loadProcessDefinition(Long.parseLong(pageState.mainObjectId.toString()));

    String stepName = pageState.getEntry("STEP_ID").stringValue();

%> <p align="center"> <%

    PageSeed ps = pageState.thisPage(request);
    ps.mainObjectId = def.getId();
    ps.addClientEntry("STEP_ID",stepName);
    ps.setCommand(Commands.SAVE);
    Form form = new Form(ps);
    form.start(pageContext);

    Container df = new Container();
    df.title =def.getName()+" - "+pageState.getI18n("PROPERTIES")+" "+pageState.getI18n("FOR")+" "+pageState.getI18n("THE")+" "+pageState.getI18n("STEP")+" \""+stepName+"\"";
    df.width="98%";

    df.start(pageContext);

    %><br><%
    TextArea tf = new TextArea(pageState.getI18n("STEP_DESCRIPTION"),"STEP_DESC","<br>",80,4,"");
    tf.toHtml(pageContext);
    %><%

    Container collCon = new Container("flow_coll_cont");
    collCon.collapsable = true;
    collCon.start(pageContext);


    Collector c = new Collector("CPF", 400, form);
    c.setDefaultLabels(pageState);

    TreeMap<String, String> cb = new TreeMap<String, String>();
    cb.put(pageState.getI18n("MANDATORY"), FlowFields.MANDATORY_FIELD);
    cb.put(pageState.getI18n("READ_ONLY"), FlowFields.READ_ONLY_FIELD);

    c.checkBoxes = cb;

    c.toHtml(pageContext);

    collCon.end(pageContext);

    ButtonBar bb = new ButtonBar();

    PageSeed psNodes = new PageSeed("nodes.jsp");
    ButtonLink inst = new ButtonLink(psNodes);
    psNodes.setMainObjectId(def.getId());
    inst.label = pageState.getI18n("STRUCTURE");
    bb.addButton(inst);

    ButtonSubmit bs = ButtonSubmit.getSaveInstance(form, pageState.getI18n("SAVE"));
    bb.addButton(bs);


    bb.addButton(new ButtonLink(I18n.get("FIELDS_DEFINITION"), pageState.pageInThisFolder("fieldsAvailable.jsp", request)));


    bb.toHtml(pageContext);
    df.end(pageContext);


    form.end(pageContext);

  }
%>
