<%@ page import="com.twproject.task.Task, com.twproject.task.businessLogic.TaskController, com.twproject.waf.TeamworkPopUpScreen, com.twproject.waf.html.TaskHeaderBar, org.jblooming.flowork.waf.FlowDrawer, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jbpm.graph.def.Node, org.jbpm.graph.def.ProcessDefinition, org.jbpm.graph.exe.ProcessInstance, java.util.List" %>
<%


  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new TaskController(), request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  }

  else {
    Task task = (Task) pageState.getMainObject();

    ProcessInstance processInstance = task.getProcess();
    ProcessDefinition def = processInstance.getProcessDefinition();

    PageSeed ps = pageState.thisPage(request);
    ps.mainObjectId=pageState.mainObjectId;
    ps.command="SHOW_GRAPH";
    ps.addClientEntry(pageState.getEntryOrDefault("SWIM", Fields.TRUE));
    ps.addClientEntry(pageState.getEntryOrDefault("STRUCT", Fields.TRUE));
    Form form = new Form(ps);
    form.start(pageContext);

    pageState.setForm(form);


/*    TaskHeaderBar head = new TaskHeaderBar(task);
    head.toHtml(pageContext);*/

    FlowDrawer drawer = new FlowDrawer(def, processInstance, pageState);
    drawer.width=1180;
//    drawer.height=600;
    boolean showSwim = pageState.getEntry("SWIM").checkFieldValue();
    boolean showStruct = pageState.getEntry("STRUCT").checkFieldValue();
    if (showSwim)
      drawer.urlToInclude = "/commons/flowork/layout/partFlowDrawerWithSwimlanes.jsp";

    drawer.hideStructural = !showStruct;

    for (Node node : (List<Node>) def.getNodes()) {
      if ((node.getId() + "").equals(task.getExternalCode())) {
        drawer.focusedNode = node;
        break;
      }
    }

    drawer.toHtml(pageContext);

    ButtonBar buttonBar = new ButtonBar();

    ButtonSubmit swims = new ButtonSubmit(form);
    swims.additionalCssClass="big";
    swims.variationsFromForm.addClientEntry("SWIM", !showSwim ? Fields.TRUE : Fields.FALSE);
    swims.label = I18n.get(!showSwim ? "SHOW_SWIMLANES" : "SHOW_GRAPH");
    buttonBar.addToLeft(swims);

    buttonBar.addSeparator(20);
    ButtonSubmit struct = new ButtonSubmit(form);
    struct.variationsFromForm.addClientEntry("STRUCT", !showStruct ? Fields.TRUE : Fields.FALSE);
    struct.additionalCssClass="big";
    struct.label = I18n.get(!showStruct ? "SHOW_STRUCTURAL_NODES" : "HIDE_STRUCTURAL_NODES");
    buttonBar.addToLeft(struct);
    buttonBar.toHtml(pageContext);
%>
<script>
  var nodeClick = function() {
    var request={
      parentTaskId:"<%=task.getId()%>",
      taskNodeId:$(this).attr("nodeId")};
    $.getJSON("<%=pageState.pageInThisFolder("partProcessRedirector.jsp",request).href%>",request ,function(response){
      jsonResponseHandling(response);
      if (response.ok)
        top.location = response.urlToGo;
    });
  };
  $("div[nodeId]").bind("click", nodeClick).css("cursor", "pointer");
</script>
<%
    form.end(pageContext);    
  }
%>