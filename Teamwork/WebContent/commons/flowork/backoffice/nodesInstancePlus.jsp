<%@ page import="org.jblooming.flowork.PlatformJbpmSessionFactory,
                org.jblooming.flowork.waf.FlowDrawer,
                org.jblooming.waf.ScreenBasic,
                org.jblooming.waf.html.button.ButtonSubmit,
                org.jblooming.waf.html.container.ButtonBar,
                org.jblooming.waf.html.input.CheckField,
                org.jblooming.waf.html.input.TextField,
                org.jblooming.waf.html.state.Form,
                org.jblooming.waf.view.PageSeed,
                org.jblooming.waf.view.PageState,
                org.jbpm.JbpmContext,
                org.jbpm.db.GraphSession,
                org.jbpm.graph.def.ProcessDefinition,
                org.jbpm.graph.exe.ProcessInstance"%><%

  PageState pageState = PageState.getCurrentPageState(request);
  
  if (!pageState.screenRunning) {
    ScreenBasic.preparePage(null, pageContext);
    pageState.perform(request, response).toHtml(pageContext);
  } else {
    PageSeed thispage = pageState.thisPage(request);
    thispage.mainObjectId = pageState.mainObjectId;
    Form form = new Form(thispage);
    form.start(pageContext);
    TextField.hiddenInstanceToHtml("PROCESS_INSTANCE_ID", pageContext);
    Long processID = pageState.getEntry("PROCESS_INSTANCE_ID").longValue();
    JbpmContext jbpmSession = PlatformJbpmSessionFactory.getJbpmContext(pageState);
    GraphSession gs = jbpmSession.getGraphSession();
    ProcessInstance processInstance = gs.loadProcessInstance(processID);
    ProcessDefinition def = processInstance.getProcessDefinition();


    FlowDrawer drawer= new FlowDrawer(def, processInstance,pageState);
    drawer.hideStructural=!pageState.getEntry("showFork").checkFieldValue();
    drawer.toHtml(pageContext);

    ButtonBar bb = new ButtonBar();
    CheckField cf = new CheckField("showFork", "&nbsp;", true);
    cf.label = "show structural nodes";
    bb.addButton(cf);
    ButtonSubmit refresh = new ButtonSubmit(form);
    refresh.label = "refresh";
    bb.addButton(refresh);
    bb.toHtml(pageContext);
    form.end(pageContext);
  }
%>

