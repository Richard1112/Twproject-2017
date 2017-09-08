<%@ page buffer="16kb" %><%@ page import="com.twproject.security.TeamworkPermissions, com.twproject.task.Task,
com.twproject.task.TaskBricks, com.twproject.task.businessLogic.TaskController, com.twproject.waf.TeamworkPopUpScreen, org.jblooming.persistence.PersistenceHome,
org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonJS,
org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.input.CheckField,
org.jblooming.waf.html.input.DateField, org.jblooming.waf.html.input.SmartCombo,
org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n,
org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState"%><%

  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new TaskController(), request);
    body.areaHtmlClass="lreq20 lreqPage";
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);

    pageState.toHtml(pageContext);

  } else {


    Task task = (Task) pageState.getMainObject();
    boolean isNew = PersistenceHome.NEW_EMPTY_ID.equals(task.getId());

    if (!isNew)
      PersistenceHome.refresh(task);

    PageSeed self = pageState.pageFromRoot("task/taskEditor.jsp");//thisPage(request);
    self.mainObjectId = task.getId();
    self.setCommand("TSK_CLONE");
    Form f = new Form(self);
    f.id="cloneForm";
    pageState.setForm(f);

    f.start(pageContext);

    //warn: has dependencies -> will be lost
    boolean hasDependencies = task.getPreviouses().size() > 0 || task.getNexts().size() > 0;

%>
<div class="mainColumn">
    <h1><%=I18n.get("TASK_CLONE")+": "%><%=task.getDisplayName()%></h1>
<%

/*
________________________________________________________________________________________________________________________________________________________________________


    CLONE

________________________________________________________________________________________________________________________________________________________________________

*/

      if (task.isProcessDriven()) {
        Task copyFrom = task;
        if (task.getProcess() == null)
          copyFrom = task.getParent();

        PageSeed newProcess = pageState.pageFromRoot("processes/createProcess.jsp");
        newProcess.setCommand(Commands.ADD);
        newProcess.addClientEntry("TASK_SOURCE", copyFrom.getId());
        ButtonLink act = new ButtonLink(I18n.get("TASK_CLONE_PROCESS"), newProcess);
        act.additionalCssClass="lreq30 lreqLabel";
        act.toHtml(pageContext);
      } else {


      %><div class="hint"><%=I18n.get("EXPLAIN_CLONE")%></div>


      <table class="table" border="0" width="100%" cellpadding="5">
        <tr><td colspan="2">
        <%
        if (task.getParent()!=null) {
          CheckField presPar = new CheckField("CLONE_PRESERVE_PARENT","&nbsp;",false);
          presPar.additionalOnclickScript="oldParentClick($(this));";
          presPar.toHtmlI18n(pageContext);
        }
        CheckField newPar = new CheckField(I18n.get("CLONE_NEW_PARENT"),"cnpck","&nbsp;",false);
        newPar.additionalOnclickScript="newParentClick($(this));";
        newPar.toHtml(pageContext);
      %></td></tr><tr id="newParentRow" style="display:none;"><td colspan="2"><%

        SmartCombo newParent = TaskBricks.getTaskCombo("CLONE_NEW_PARENT",false, TeamworkPermissions.task_canCreate,pageState);

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

        newParent.separator = "";
        newParent.label = "";

        newParent.toHtmlI18n(pageContext);

        %></td></tr><tr><td><%



      TextField tfc = new TextField("TASK_CODE", "<br>");
      tfc.label = I18n.get("NEW_TASK_CODE");
      tfc.fieldSize = 20;
      pageState.addClientEntry("TASK_CODE",JSP.w("["+pageState.getEntry("TASK_CODE").stringValueNullIfEmpty()+"]"));
      tfc.toHtmlI18n(pageContext);
     %></td><td><%
      TextField tf = new TextField("TASK_NAME", "<br>");
      tf.fieldSize = 50;
      tf.label = I18n.get("NEW_TASK_NAME");
      pageState.addClientEntry("TASK_NAME",JSP.w("["+pageState.getEntry("TASK_NAME").stringValueNullIfEmpty()+"]"));
      tf.required = true;
      tf.toHtmlI18n(pageContext);
    %></td></tr>
  <tr><td><%

    DateField df = new DateField("CLONE_NEWSTARTDATE");
    df.separator="<br>";
    df.toHtmlI18n(pageContext);
  %></td><td><%


      SmartCombo typeCombo = TaskBricks.getTaskTypeCombo("NEW_TASK_TYPE", task, pageState);
      typeCombo.separator="<br>";
      typeCombo.toHtmlI18n(pageContext);

    %></td></tr>
  <tr><td valign="top" colspan="2"><div><%



      pageState.getEntryOrDefault("CLONE_CODES");
      CheckField codes = new CheckField("CLONE_CODES","&nbsp;",false);
      codes.toHtmlI18n(pageContext);
      %><br><%

      if (TaskBricks.hasCustomField()){
        pageState.getEntryOrDefault("CLONE_CUSTOM_FIELDS");
        CheckField cfs = new CheckField("CLONE_CUSTOM_FIELDS","&nbsp;",false);
        cfs.toHtmlI18n(pageContext);
        %><br><%
      }

      pageState.getEntryOrDefault("CLONE_ASSIGS");
      CheckField assigs = new CheckField("CLONE_ASSIGS","&nbsp;",false);
      assigs.toHtmlI18n(pageContext);
      %><br><%

      pageState.getEntryOrDefault("CLONE_ADDITIONAL_COSTS");
      CheckField cc = new CheckField("CLONE_ADDITIONAL_COSTS","&nbsp;",false);
      cc.id = "CLONE_ADDITIONAL_COSTS";
      cc.toHtmlI18n(pageContext);
      %></div>
    <div style="float: left"><%


      pageState.getEntryOrDefault("CLONE_ISSUES_COPY");
      CheckField ci = new CheckField("CLONE_ISSUES_COPY","&nbsp;",false);
      ci.additionalOnclickScript="cicBox($(this));";
      ci.toHtmlI18n(pageContext);

      pageState.getEntryOrDefault("CLONE_ISSUES_MOVE");
      ci = new CheckField("CLONE_ISSUES_MOVE","&nbsp;",false);
      ci.additionalOnclickScript="cicBox($(this));";
      ci.toHtmlI18n(pageContext);

      pageState.getEntryOrDefault("CLONE_ISSUES_CLOSED");
      CheckField cic = new CheckField("CLONE_ISSUES_CLOSED","&nbsp;",false);
      %></div><div class="highlight" style=" display:none;float: left" id="cicBox"><%
      cic.toHtmlI18n(pageContext);
      %></div><%

      %><br></td></tr></table><%
      }

  ButtonBar bb = new ButtonBar();

  ButtonJS save = new ButtonJS(I18n.get("TASK_CLONE"),"cloneTask()");
  save.additionalCssClass="first";
  //save.enabled = !hasDependencies;

  bb.addButton(save);
  bb.toHtml(pageContext);

%></div>
<script>
  $(function(){
    cicBox($("#ck_CLONE_ISSUES_COPY"));
    cicBox($("#ck_CLONE_ISSUES_MOVE"));
  });

  function cicBox(el){
    // apri box per issue chiuse
    if(el.is(":checked")) {
      $("#cicBox").show();
      if(el.is("[name=ck_CLONE_ISSUES_COPY]")){
        $("#ck_CLONE_ISSUES_MOVE").prop( "checked", false );
        $("#CLONE_ISSUES_MOVE").val("no");
      } else {
        $("#ck_CLONE_ISSUES_COPY").prop( "checked", false );
        $("#CLONE_ISSUES_COPY").val("no");
      }
    }
    if (!$("#ck_CLONE_ISSUES_COPY").is(":checked") && !$("#ck_CLONE_ISSUES_MOVE").is(":checked"))
      $("#cicBox").hide();

  }

  function oldParentClick(el){
    if(el.is(":checked")) {
      $("#cnpck").val("no");
      $("#ck_cnpck").prop("checked", false);
      $("#newParentRow").hide();
      $("#CLONE_NEW_PARENT").val("");
    }
  }

  function newParentClick(el){
    if(el.is(":checked")) {
      $("#CLONE_PRESERVE_PARENT").val("no");
      $("#ck_CLONE_PRESERVE_PARENT").prop("checked", false);
      $("#newParentRow").show();
    } else {
      $("#newParentRow").hide();
      $("#CLONE_NEW_PARENT").val("");
    }
  }

  function cloneTask(){
    //console.debug("cloneTask");
    var request={};
    $("#cloneForm").fillJsonWithInputValues(request);
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