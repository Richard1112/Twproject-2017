<%@ page import="com.twproject.task.businessLogic.TaskController,
                com.twproject.waf.TeamworkPopUpScreen,
                org.jblooming.flowork.FlowPermissions,
                org.jblooming.flowork.PlatformJbpmSessionFactory,
                org.jblooming.flowork.waf.FlowDrawer,
                org.jblooming.operator.Operator,
                org.jblooming.waf.ScreenArea,
                org.jblooming.waf.SessionState,
                org.jblooming.waf.constants.Fields,
                org.jblooming.waf.html.button.ButtonSubmit,
                org.jblooming.waf.html.container.ButtonBar,
                org.jblooming.waf.html.state.Form,
                org.jblooming.waf.settings.I18n,
                org.jblooming.waf.view.PageSeed,
                org.jblooming.waf.view.PageState,
                org.jbpm.JbpmContext, org.jbpm.db.GraphSession, org.jbpm.graph.def.ProcessDefinition"%><%


  PageState pageState = PageState.getCurrentPageState(request);
  final Operator logged = pageState.getLoggedOperator();

  boolean isFlowManager = logged != null && (logged.hasPermissionFor(FlowPermissions.canManageFlows));

  if (!pageState.screenRunning) {


    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new TaskController(),request);
    TeamworkPopUpScreen  lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);


  } else {
    PageSeed thispage = pageState.thisPage(request);
    thispage.setPopup(pageState.isPopup());
    thispage.mainObjectId = pageState.mainObjectId;
    thispage.addClientEntry(pageState.getEntryOrDefault("SWIM",Fields.TRUE));
    thispage.addClientEntry(pageState.getEntryOrDefault("STRUCT",Fields.TRUE));


    Form form = new Form(thispage);
    form.start(pageContext);


    JbpmContext jbpmSession = PlatformJbpmSessionFactory.getJbpmContext(pageState);
    GraphSession gs = jbpmSession.getGraphSession();
    ProcessDefinition def = gs.loadProcessDefinition(Long.parseLong(pageState.mainObjectId.toString()));


    FlowDrawer drawer= new FlowDrawer(def, pageState);
    drawer.hideStructural=false;
    drawer.width=1024;
    drawer.height=768;


    boolean showSwim = pageState.getEntry("SWIM").checkFieldValue();
    if (showSwim)
      drawer.urlToInclude="/commons/flowork/layout/partFlowDrawerWithSwimlanes.jsp";

    boolean showStruct = pageState.getEntry("STRUCT").checkFieldValue();
    drawer.hideStructural=!showStruct;


    drawer.toHtml(pageContext);



    ButtonBar bb = new ButtonBar();

    ButtonSubmit bs=new ButtonSubmit(form);

    bs.variationsFromForm.addClientEntry("SWIM", !showSwim? Fields.TRUE: Fields.FALSE);
    bs.label = I18n.get(!showSwim ? "SHOW_SWIMLANES" : "SHOW_GRAPH");
    bs.additionalCssClass="big";
    bb.addButton(bs);

    bb.addSeparator(30);
    ButtonSubmit struct =new ButtonSubmit(form);
    struct.variationsFromForm.addClientEntry("STRUCT", !showStruct? Fields.TRUE: Fields.FALSE);
    struct.label = I18n.get(!showStruct ? "SHOW_STRUCTURAL_NODES" : "HIDE_STRUCTURAL_NODES");
    struct.additionalCssClass="big";
    bb.addToLeft(struct);

    bb.toHtml(pageContext);

    form.end(pageContext);

  }
    
%>

