<%@ page import=" org.jblooming.flowork.FlowPermissions,
                  org.jblooming.flowork.PlatformJbpmSessionFactory,
                  org.jblooming.flowork.Starter,
                  org.jblooming.operator.Operator,
                  org.jblooming.persistence.PersistenceHome,
                  org.jblooming.waf.ScreenBasic,
                  org.jblooming.waf.SessionState,
                  org.jblooming.waf.constants.Commands,
                  org.jblooming.waf.html.button.ButtonLink,
                  org.jblooming.waf.html.button.ButtonSubmit,
                  org.jblooming.waf.html.container.ButtonBar,
                  org.jblooming.waf.html.container.Container,
                  org.jblooming.waf.html.input.Collector,
                  org.jblooming.waf.html.state.Form,
                  org.jblooming.waf.view.PageSeed,
                  org.jblooming.waf.view.PageState,
                  org.jbpm.JbpmContext,
                  org.jbpm.db.GraphSession,
                  org.jbpm.graph.def.ProcessDefinition,
                  org.jbpm.taskmgmt.def.Swimlane,
                  org.jbpm.taskmgmt.def.TaskMgmtDefinition,
                  java.util.Map, java.util.Set, java.util.TreeMap" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  final Operator logged = pageState.getLoggedOperator();
  boolean isFlowManager = logged != null && (logged.hasPermissionFor(FlowPermissions.canManageFlows));
  if (logged == null || !isFlowManager)
    throw new SecurityException("No permission for accessing page " + request.getRequestURI());

  if (!pageState.screenRunning) {
    ScreenBasic.preparePage(pageContext);
    pageState.perform(request, response);

    //CONTROLLER
    final String command = pageState.getCommand();

    JbpmContext jbpmSession = PlatformJbpmSessionFactory.getJbpmContext(pageState);
    GraphSession gs = jbpmSession.getGraphSession();

    ProcessDefinition def = gs.loadProcessDefinition(Long.parseLong(pageState.mainObjectId.toString()));

    Starter starter = (Starter) PersistenceHome.findUniqueNullIfEmpty(Starter.class, "definitionName", def.getName());

    if (starter == null) {
      starter = new Starter();
      starter.setDefinitionName(def.getName());
      starter.setSwimlaneNames("");
      starter.setIdAsNew();
      //PersistenceHome.store(starter);
    }

    pageState.setMainObject(starter);
    pageState.mainObjectId = def.getId(); // occhio mainobject e mainobjectid sono di due bestie diverse

    if (Collector.isCollectorCommand("swimColl", command)) {

      Collector.move("swimColl", pageState);

    } else if (Commands.SAVE.equals(command)) {

      TreeMap<String, String> tm = Collector.chosen("swimColl", pageState);
      String swntot = "";
      for (String swn : tm.keySet()) {
        swntot = swntot + "*" + swn;
      }
      starter.setSwimlaneNames(swntot);
      PersistenceHome.store(starter);

    } else {

      Set<String> chosen = starter.getSwimlanes();
      TreeMap<String, String> ctm = new TreeMap<String, String>();
      if (chosen != null && chosen.size() > 0) {
        for (String p : chosen) {
          if (p != null)
            ctm.put(p, p);
        }
      }

      TaskMgmtDefinition taskMgmtDefinition = def.getTaskMgmtDefinition();
      if (taskMgmtDefinition != null) {
        Map<String, Swimlane> msl = taskMgmtDefinition.getSwimlanes();
        if (msl != null && msl.values().size() > 0) {
          TreeMap candTm = new TreeMap();
          for (Swimlane sw : msl.values()) {
            if (chosen == null || !chosen.contains(sw.getName()))
              candTm.put(sw.getName(), sw.getName());
          }
          Collector.make("swimColl", candTm, ctm, pageState);
        }
      }
    }
    //CONTROLLER END

    pageState.toHtml(pageContext);

  } else {

    Starter starter = (Starter) pageState.getMainObject();

    PageSeed v = pageState.thisPage(request);
    v.mainObjectId = pageState.mainObjectId;
    Form form = new Form(v);
    form.start(pageContext);

    Container permStart = new Container();
    permStart.title = pageState.getI18n("DEF_START_PERMISSION") + "&nbsp;" + starter.getDefinitionName();
    permStart.start(pageContext);

    Collector c = new Collector("swimColl", 300, form);
    c.CHOSEN_LABEL = pageState.getI18n("FLOW_WHO_CAN_START");
    c.CANDIDATES_LABEL = "swimlanes";

    c.toHtml(pageContext);
    permStart.end(pageContext);

    ButtonBar bb = new ButtonBar();

    ButtonLink bdl = new ButtonLink(pageState.pageInThisFolder("deployList.jsp", request));
    bdl.label = pageState.getI18n("BACK");
    bb.addButton(bdl);

    bb.addButton(ButtonSubmit.getSaveInstance(form, pageState.getI18n("SAVE")));
    bb.toHtml(pageContext);


    form.end(pageContext);
  }

%>