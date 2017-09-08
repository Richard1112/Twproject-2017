<%@ page buffer="16kb" %><%@ page import="com.twproject.security.TeamworkPermissions, com.twproject.task.Task,
com.twproject.task.TaskBricks, com.twproject.task.businessLogic.TaskController, com.twproject.waf.TeamworkPopUpScreen, org.jblooming.persistence.PersistenceHome,
org.jblooming.waf.ScreenArea, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.input.SmartCombo,
org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed,
org.jblooming.waf.view.PageState"%><%

  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new TaskController(), request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);

    pageState.toHtml(pageContext);

  } else {

    Task task = (Task) pageState.getMainObject();
    boolean isNew = PersistenceHome.NEW_EMPTY_ID.equals(task.getId());

    if (!isNew)
      PersistenceHome.refresh(task);

    PageSeed self = pageState.thisPage(request);
    self.mainObjectId = task.getId();
    self.setCommand("TSK_MOVE");
    Form f = new Form(self);
    f.id="moveForm";
    pageState.setForm(f);

    f.start(pageContext);

%>

<div class="mainColumn">
  <%
   //warn: has dependencies -> will be lost
  boolean hasDependencies = task.getPreviouses().size() > 0 || task.getNexts().size() > 0;


%><h1><%=I18n.get("MOVE")+": "%><%=task.getDisplayName()%></h1>
  <table width="100%"><tr><td><%
    if (hasDependencies) {
      %><div class="hint warning" style="text-align: left"><%=I18n.get("TASK_HAS_DEPENDENCIES_CANNOT_MOVE_IT")%></div><%
    }


/*
________________________________________________________________________________________________________________________________________________________________________


    MOVE

________________________________________________________________________________________________________________________________________________________________________

*/

      %><div class="hint" style="text-align: left"><%=I18n.get("EXPLAIN_MOVE")%></div><%

      if (task.getParent()!=null) {
        %><%=I18n.get("TASK_PARENT_IS")%>&nbsp;<b><%=task.getParent().getDisplayName()%></b><%
      } else {
        %><%=I18n.get("TASK_IS_ROOT")%><%
      }
      %><br><br><%
      SmartCombo newParent = TaskBricks.getTaskCombo("TASK_NEW_PARENT",false, TeamworkPermissions.task_canCreate,pageState);

      //se non hai i permessi di creare un progetto di radice il combo è required
      newParent.required=!pageState.getLoggedOperator().hasPermissionFor(TeamworkPermissions.project_canCreate);

    //exclude myself or any of my descendants
      String hqlId=" not (task.id=:tid or (task.ancestorIds like :descids and task.ancestorIds is not null)";
      if (task.getParent()!=null){
        hqlId+=" or task.id=:pid";
        newParent.queryHelperForFiltering.addParameter("pid",task.getParent().getId());
      }
      hqlId+=")";
      newParent.queryHelperForFiltering.addOQLClause(hqlId);
      newParent.queryHelperForFiltering.addParameter("tid",task.getId());
      newParent.queryHelperForFiltering.addParameter("descids",task.getChildAncentorIds()+"%");

      newParent.separator = "<br>";
      newParent.toHtmlI18n(pageContext);

      if (task.getParent()!=null) {
        %>&nbsp;<%=I18n.get("LEAVE_FIELD_EMPTY_TO_SET_TASK_AS_ROOT")%><%
      }


  //ButtonSubmit save = ButtonSubmit.getSaveInstance(f, I18n.get("TASK_MOVE"));
  //save.variationsFromForm.setCommand("MOVE");

  ButtonJS save = new ButtonJS(I18n.get("TASK_MOVE"),"moveTask()");
  save.additionalCssClass="first";
  save.enabled = !hasDependencies;

  ButtonBar bb2 = new ButtonBar();
  bb2.addButton(save);
  bb2.toHtml(pageContext);

%></td></tr></table></div>

<script>
  function moveTask(){
    //console.debug("moveTask");
    var request={};
    $("#moveForm").fillJsonWithInputValues(request);
    showSavingMessage();
    $.getJSON("taskAjaxController.jsp",request,function(response){
      jsonResponseHandling(response);
      if (response.ok == true && response.task) {
        parent.location.href=contextPath+"/applications/teamwork/task/taskEditor.jsp?CM=ED&OBJID="+response.task.id;
      }
      hideSavingMessage();
    })

  }
</script>


<%
  f.end(pageContext);
    }
%>