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
    self.setCommand("TSK_ADD_TPL");
    Form f = new Form(self);
    f.id="cloneForm";
    pageState.setForm(f);

    f.start(pageContext);


%>

<div class="mainColumn">
    <h1><%=I18n.get("ADD_CHILD_TASK")+": "%><%=task.getDisplayName()%></h1>


  <div class="hint"><%=I18n.get("EXPLAIN_ADD_FROM_TEMPLATE")%></div>


      <table class="table" border="0" width="100%" cellpadding="5">
        <tr><td colspan="2">
        <%

        SmartCombo template = TaskBricks.getTaskCombo("CHOOSE_TEMPLATE",false, TeamworkPermissions.task_canRead,pageState);

        //exclude myself or any of my descendants
        String hqlId=" not (task.id=:tid or (task.ancestorIds like :descids and task.ancestorIds is not null)";
        if (task.getParent()!=null){
          hqlId+=" or task.id=:pid";
          template.queryHelperForFiltering.addParameter("pid",task.getParent().getId());
        }
        hqlId+=")";
        template.queryHelperForFiltering.addOQLClause(hqlId);
        template.queryHelperForFiltering.addParameter("tid",task.getId());
        template.queryHelperForFiltering.addParameter("descids",task.getChildAncentorIds()+"%");

        template.separator = "<br>";
        template.required=true;

          template.onValueSelectedScript="$('#TASK_NAME').val($('#CHOOSE_TEMPLATE_txt').val());";

        template.toHtmlI18n(pageContext);

        %></td></tr><tr><td><%


      TextField tfc = new TextField("TASK_CODE", "<br>");
      tfc.label = I18n.get("NEW_TASK_CODE");
      tfc.fieldSize = 20;
      pageState.addClientEntry("TASK_CODE","");
      tfc.toHtmlI18n(pageContext);
     %></td><td><%
      TextField tf = new TextField("TASK_NAME", "<br>");
      tf.fieldSize = 50;
      tf.label = I18n.get("NEW_TASK_NAME");
      pageState.addClientEntry("TASK_NAME","");
      //tf.required = true;
      tf.toHtmlI18n(pageContext);
    %></td></tr>
  <tr><td><%

    DateField df = new DateField("CLONE_NEWSTARTDATE");
    df.labelstr="ADDTEMPLATE_NEWSTARTDATE";
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


  ButtonBar bb = new ButtonBar();

  ButtonJS save = new ButtonJS(I18n.get("TASK_CLONE"),"addFromTemplate()");
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


  function addFromTemplate(){
    //console.debug("addFromTemplate");
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