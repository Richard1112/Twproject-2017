<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.resource.Person, com.twproject.resource.Resource, com.twproject.security.SecurityBricks, com.twproject.security.TeamworkPermissions,
                 com.twproject.task.Assignment, com.twproject.task.Task, com.twproject.task.TaskBricks, com.twproject.task.businessLogic.AssignmentController, com.twproject.task.financial.FinancialBricks, com.twproject.waf.TeamworkHBFScreen,
                 com.twproject.waf.html.TaskHeaderBar, org.jblooming.agenda.CompanyCalendar, org.jblooming.designer.DesignerField, org.jblooming.security.Area, org.jblooming.utilities.CodeValueList, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP,
                 org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.button.ButtonSupport,
                 org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.html.display.PercentileDisplay, org.jblooming.waf.html.input.*, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.Date, java.util.List, java.util.Set" %><%

PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new AssignmentController(), request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {
    //this is set by action
    Task task = (Task) pageState.attributes.get("REFERRAL_OBJECT");

    Assignment assig = (Assignment) pageState.getMainObject();

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    Person loggedPerson = logged.getPerson();



    PageSeed ps = pageState.thisPage(request);
    ps.command = Commands.EDIT;
    ps.addClientEntry("TASK_ID", task.getId());
    ps.mainObjectId = assig.getId();
    Form form = new Form(ps);
    form.alertOnChange = true;
    form.encType=Form.MULTIPART_FORM_DATA;
    form.start(pageContext);
    pageState.setForm(form);
%>
<div class="mainColumn">
<%

  //-----------------HEAD BAR START

  pageState.addClientEntry("TASK_TABSET","TASK_ASSIGNMENT_TAB");

  TaskHeaderBar head = new TaskHeaderBar(task);
  head.pathToObject.destination=pageState.pageInThisFolder("taskAssignmentList.jsp",request);
  head.pathToObject.alternativeCEForMainObject="TASK_ID";
  head.toHtml(pageContext);

  //-----------------HEAD BAR END

 %><div class="inlineContainerWrapper">
  <div class="container"><%

//---------------------------------------------------------------------------------  START PROCESS MANAGEMENT ------------------------------------------------------------------------------------
  if (task.isProcessDriven()){
    %><div class="box"><%

    // add parameter to form
    pageState.getForm().url.addClientEntry("TRANSITIONID","");
    pageState.getForm().url.addClientEntry("TASKINSTANCE","");

    // can you manage this flow?
    %><span class="teamworkIcon">&#xa3;</span> <b><%=I18n.get("TASK_STATE_MANAGED_BY_PROCESS")%></b> <%=I18n.get("PROCESS_CANNOT_ADD_REMOVE_ASSIG")%>
</div><%
  }
//---------------------------------------------------------------------------------  END PROCESS MANAGEMENT ------------------------------------------------------------------------------------


 %>
<table class="table" cellpadding="5" border="0">
<tr>
  <td valign="top" width="70" rowspan="5">
    <%if(assig.getResource()!=null){%>
    <img src="<%=assig.getResource().bricks.getAvatarImageUrl()%>" title="<%=assig.getResource().getDisplayName()+" ("+JSP.w(assig.getRole().getCode())+")"%>" class="face medium" align="top">
    <%}%>
  </td><td><%

      SmartCombo res = task.bricks.getAssignableResourceCombo("ASSIGNEE", false, pageState);

      //se l'assegnazione esiste si deve avere anche il permesso TeamworkPermissions.resource_manage sull'assegnazione, ovvero sulla risorsa.
      // Nel caso di nuova assegnaz. ci pensa il combo
      boolean assignment_canCRW=false;
      if (assig.isNew())
        assignment_canCRW = task.hasPermissionFor(logged,TeamworkPermissions.assignment_canCRW);
      else
        assignment_canCRW = task.hasPermissionFor(logged,TeamworkPermissions.assignment_canCRW) && assig.hasPermissionFor(logged,TeamworkPermissions.resource_manage);



      res.readOnly = !assignment_canCRW;
      res.required = true;
      res.onValueSelectedScript= "loadSubsAndCost()";
      res.fieldSize = 25;
      res.fieldClass = "formElements bold";
      res.separator = "<br>";


      //support for adding resources on the fly
      if (task.getArea().hasPermissionFor(logged,TeamworkPermissions.resource_canCreate))
        res.addAllowed=true;

      // re non hai il resource read non mostra link
      if (assig!=null && assig.getResource()!=null){
        if (!assig.getResource().hasPermissionFor(logged,TeamworkPermissions.resource_canRead))
          res.linkToEntity=null;
      } else {
        res.linkToEntity=null;
      }

      res.toHtmlI18n(pageContext);
%></td><td><%
    pageState.getEntryOrDefault("ASSIG_ROLE");
    SmartCombo roles = SecurityBricks.getRoleComboForAssignments("ASSIG_ROLE", task, false, pageState);
    roles.required = true;
    roles.readOnly = !assignment_canCRW;
    roles.fieldSize = 20;
    roles.separator = "<br>";
    roles.onValueSelectedScript = "loadSubscriptions()";
    roles.toHtmlI18n(pageContext);

  %></td><td class="lreq20 lreqLabel"><%

    ColorValueChooser chooser = TaskBricks.getAssignmentPriorityCombo("ASSPRIORITY", 40, pageState);
    chooser.label=I18n.get("PRIORITY");
    //chooser.label="";
    chooser.width = 120;
    chooser.height = 26;
    chooser.separator="<br>";
    chooser.showOpener = true;
    chooser.readOnly=!assignment_canCRW;
    chooser.toHtml(pageContext);
%></td><td><%
    CheckField cfEnab = new CheckField("ASSIG_ENABLED", "&nbsp;", false);
    cfEnab.disabled = !assignment_canCRW;
    cfEnab.toHtmlI18n(pageContext);


  %></td>
</tr>
<tr>

  <td><%
    List sts = Assignment.getActivityTypes();
    CodeValueList cvl = CodeValueList.getI18nInstance(sts, pageState);
    Combo cb = new Combo("ACTIVITY", "<br>", "", 255, cvl, null);
    cb.readOnly = !assignment_canCRW;
    cb.label = I18n.get(cb.fieldName);
    cb.toHtml(pageContext);
  %></td>
  <td><%
    TextField tf = TextField.getPercentileInstance("ASSIG_RISK");
    tf.fieldSize = 3;
    tf.separator = "<br>";
    tf.readOnly = !assignment_canCRW;
    tf.toHtmlI18n(pageContext);

  %></td>
  <td valign="top">


    <%
      tf = TextField.getDurationInMillisInstance("ESTIMATE_WORKLOG");
      tf.label = assig.isNew() ? "ASSIG_SET_EST":"ESTIMATE_WORKLOG";
      tf.separator = "<br>";
      tf.readOnly = !assignment_canCRW;
      tf.toHtmlI18n(pageContext);

      long worklogEstimatedFromIssues = assig.getWorklogEstimatedFromIssues();
      PageSeed issues = pageState.pageFromRoot("issue/issueList.jsp");
      issues.addClientEntry("FLT_ISSUE_ASSIGNED_TO",assig.getResource());
      issues.addClientEntry("FLT_ISSUE_TASK", task.getId());
      issues.addClientEntry("FLT_ISSUE_ESTIMATEDDURATION",">0");
      issues.command = Commands.FIND;


    %>&nbsp;<small>(<%=I18n.get("ASSIG_LOAD_ISSUES")%>&nbsp;
      <%new ButtonLink(DateUtilities.getMillisInHoursMinutes(worklogEstimatedFromIssues), issues).toHtmlInTextOnlyModality(pageContext);%>)
    </small>
  </td><td><%

      long we = assig.getEstimatedWorklog();
      if (!assig.isNew()) {
        long wd = assig.getWorklogDone();
        if (true || wd > 0) {
          PageSeed was = pageState.pageFromRoot("/task/worklog/worklogAssignmentList.jsp");
          was.addClientEntry("ASS_ID", assig.getId());
          was.command=Commands.FIND;
          was.setPopup(true);
          ButtonSupport wdbutton = ButtonLink.getBlackInstance("", 768, 1180, was);
          wdbutton.label=DateUtilities.getMillisInHoursMinutes(wd);
          %><label><%=I18n.get("ASSIGNMENT_WORKLOG_DONE")%>:</label> <%wdbutton.toHtmlInTextOnlyModality(pageContext);%><%

          if ( we > 0) {
            PercentileDisplay percentileDisplay = new PercentileDisplay(((double) wd) / ((double) we) * 100);
            percentileDisplay.height = "15px";
            percentileDisplay.toHtml(pageContext);

          }

        }
      }

      long hpd =  CompanyCalendar.MILLIS_IN_WORKING_DAY;
      if(assig.getResource() != null)
        hpd = (int)(assig.getResource().getWorkDailyCapacity() / CompanyCalendar.MILLIS_IN_HOUR);
      int missing = 0;

      if (task.getSchedule() != null && task.getSchedule().getEndDate() != null)
        missing = CompanyCalendar.getDistanceInWorkingDays((new Date(Math.max(System.currentTimeMillis(), task.getSchedule().getEndDate().getTime()))), task.getSchedule().getEndDate()) - 1;
      if (missing > 0) {
        %><%=JSP.wHelp(I18n.get("UNIT_HOUR"))%>;&nbsp;<%=JSP.wHelp(I18n.get("WORKING_DAYS_MISSING_FROM_TODAY"))%>:<%=missing%>&nbsp;(x8h=<%=missing * hpd%>)<%
      }

  %></td>
</tr>

<tr>
<td><%
  if ( task.hasPermissionFor(logged,TeamworkPermissions.task_cost_canRead)) {
    tf = TextField.getCurrencyInstance("ASSIG_COST");
    tf.fieldSize = 10;
    tf.separator = "<br>";
    tf.readOnly=! task.hasPermissionFor(logged,TeamworkPermissions.task_cost_canWrite);
    tf.toHtmlI18n(pageContext);

  } else {
    TextField.hiddenInstanceToHtml("ASSIG_COST", pageContext);
%>&nbsp;</td><td>&nbsp;<%
  }
%></td><td><%

  Set<Area> areas = logged.getAreasForPermission(TeamworkPermissions.resource_canRead);
  if (areas.size() > 0) {
    SmartCombo ccs = FinancialBricks.getCostAggregatorCombo("COST_COSTCENTER", areas, null, null, pageState);
    ccs.readOnly = !assignment_canCRW;
    ccs.fieldSize = 20;
    ccs.separator = "<br>";
    ccs.toHtmlI18n(pageContext);
  } else {
    TextField.hiddenInstanceToHtml("COST_COSTCENTER", pageContext);
  }

%></td>
  <td><div  class="lreq20 lreqHide"><%
    tf = TextField.getCurrencyInstance("ASSIG_BUDGET");
    tf.fieldSize = 10;
    tf.type= task.hasPermissionFor(logged,TeamworkPermissions.task_cost_canRead)?"text":"hidden";
    tf.label=I18n.get("BUDGET");
    tf.separator = "<br>";
    tf.readOnly = !task.hasPermissionFor(logged,TeamworkPermissions.task_cost_canWrite); // il budget sull'assignemnt è un costo di progetto
    tf.toHtml(pageContext);

  %></div></td>

</tr>

<tr>



  </tr>
<tr>
  <td colspan="4"><label><%=I18n.get("ASSIG_DESCRIPTION")%></label>
    <div class="linkEnabled formElements" style="font-size:14px;color:black;font-weight:normal;moz-border-radius:3px 3px 3px 3px; height:80px;padding:10px" onclick="$(this).hide().next().show().focus()"><%=JSP.encode(assig.getDescription())%></div><%
      TextArea ta = new TextArea("ASSIG_DESCRIPTION", "", 22, 9, null);
      ta.readOnly = !assignment_canCRW;
      ta.label = "";
      ta.script = "style='width:100%;height:160px;display:none;'";
      ta.maxlength=2000;
      ta.toHtml(pageContext); %>
  </td>

</tr>
<%
  if (Assignment.hasCustomField()) {
%><tr><td colspan="3"><%

  for (int i=1; i<7; i++) {
    DesignerField dfStr = DesignerField.getCustomFieldInstance( "ASSIGNMENT_CUSTOM_FIELD_",i, assig,!assignment_canCRW, false, false, pageState);
    if (dfStr!=null){
      dfStr.separator="<br>";
%><div style="float:left; margin-right: 10px"><%dfStr.toHtml(pageContext);%></div><%
      }
    }
    %></td></tr><%

  }


%><%
  if (I18n.isActive("LOAD_ACTIVITY_ON_ASSIGNMENTS")) {
%><tr><td colspan="3"><%
    try{
      String url = I18n.get("LOAD_ACTIVITY_ON_ASSIGNMENTS");
      JspHelper helper=new JspHelper(url);
      helper.parameters.put("task",task);
      helper.parameters.put("assig",assig);
      helper.toHtml(pageContext);
    }catch(Throwable e) {
      e.printStackTrace();
    }
%></td></tr><%
  }
%>
</table>

<%-----------------------------  SUBSCRIPTIONS --------------------------------------------------%>
  <br>

<div id="slideDivUp" style="display:none; ">
  <%
    ButtonJS up = new ButtonJS("$('#task_subs').slideUp('slow');$('#slideDivDown').css('display','');$('#slideDivUp').css('display','none')");
    up.label = I18n.get("HIDE_SUBSCRIPTIONS");
    up.additionalCssClass="opener";
    up.iconChar = "u";
    up.toolTip = I18n.get("HIDE_SUBSCRIPTIONS");
    up.toHtmlInTextOnlyModality(pageContext);
  %></div>
<div id="slideDivDown" style=""><%
  ButtonJS down = new ButtonJS("$('#task_subs').slideDown('slow');$('#slideDivUp').css('display','');$('#slideDivDown').css('display','none')");
  down.label = I18n.get("SHOW_SUBSCRIPTIONS");
  down.additionalCssClass="opener";
  down.iconChar = "&ugrave;";
  down.toolTip = I18n.get("SHOW_SUBSCRIPTIONS");
  down.toHtmlInTextOnlyModality(pageContext);
%></div>

<div id="task_subs" style="display:none;margin: 10px 0" class="lreq10 lreqLabel">
  <div id="task_subs_ajax"></div>
  <%
    // if yourself save subscriptions
    if (!assignment_canCRW && assig.getResource().equals(loggedPerson)){
      ButtonBar bb = new ButtonBar();
      ButtonSubmit save = ButtonSubmit.getSaveInstance(form, I18n.get("SAVE_SUBSCRIPTIONS"));
      save.variationsFromForm.setCommand("SAVE_SUBSCRIPTIONS");

      bb.addButton(save);
      bb.toHtml(pageContext);

    }

  %>
</div>

<%
  ButtonBar buttonBar = new ButtonBar();


    {
      ButtonSubmit save = ButtonSubmit.getSaveInstance(form, I18n.get("SAVE_ASSIGNMENT"),false);
      save.variationsFromForm.setCommand(Commands.SAVE);
      save.additionalCssClass="big first";
      save.enabled = assignment_canCRW;
      buttonBar.addToLeft(save);
    }


    if (!assig.isNew()) {
      DeletePreviewer deletePreviewer = new DeletePreviewer("ASS_DEL",AssignmentController.class, pageState);
      ButtonJS dP = deletePreviewer.getDeleteButton(I18n.get("ASSIG_DELETE"), assig.getId());
      dP.additionalCssClass="big delete";
      dP.enabled = assignment_canCRW && !(task.isProcessDriven()&&JSP.ex(assig.getExternalCode()));
      buttonBar.addToRight(dP);


    }

  buttonBar.loggableIdentifiableSupport=assig;

  buttonBar.toHtml(pageContext);
%>
</div>

<div class="container"><%
  if (!assig.isNew()) {
%>


  <div class="buttonBoxInline">
    <h5><%=I18n.get("PLAN_MENU")%></h5>
  <a href="javascript:openOperatorLoad();" class="button textual small lreq20 lreqLabel lreqActive"><%=I18n.get("OPERATOR_LOAD")%></a><%

    PageSeed pl = pageState.pageFromRoot("task/plan/planByTask.jsp");
    pl.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));
    pl.mainObjectId = assig.getTask().getId();
    pl.command = Commands.EDIT;
    ButtonLink bl = new ButtonLink(I18n.get("WORKLOG_SEE_PLAN"), pl);
    bl.additionalCssClass="small lreq30 lreqLabel lreqActive";
    bl.toHtmlInTextOnlyModality(pageContext);


  %><br>


  <%-----------------------------  ASSIG COSTS --------------------------------------------------%><%
  if ((assignment_canCRW ||assig.hasPermissionFor(logged,TeamworkPermissions.resource_manage) || assig.getResource().equals(logged.getPerson()))){

    %><div class="lreq20 lreqLabel">
    <h5><%=I18n.get("ASSIG_EXPENSES")%></h5><%
    PageSeed exp=pageState.pageFromRoot("task/financial/taskAssignmentCosts.jsp");
    exp.mainObjectId=assig.getId();
    ButtonSupport btmExp = ButtonLink.getBlackInstance(I18n.get("MANAGE_EXPENSES"), 768, 1180, exp);
    btmExp.additionalCssClass="small";
    btmExp.toHtmlInTextOnlyModality(pageContext);
      %></div><%
  }
%>


  <%-----------------------------  ASSIG MOVE --------------------------------------------------%>
  <%
    if (assignment_canCRW && !(task.isProcessDriven()&&JSP.ex(assig.getExternalCode()))){

      %><br>
    <h5><%=I18n.get("ACTIONS")%></h5><%
      ButtonJS bjs1= new ButtonJS(I18n.get("ASSIGNMENT_MOVE"), "showAssigMover($(this));");
      bjs1.additionalCssClass="small";
      bjs1.toHtmlInTextOnlyModality(pageContext);

  %><div id="assig_move" style="display:none;margin: 10px 0">
    <h1><%=I18n.get("ASSIGNMENT_MOVE")%></h1>
    <div class="hint"><%=I18n.get("EXPLAIN_MOVE_ASSIG")%></div>
    <%
      //SmartCombo newParent = TaskBricks.getTaskCombo("ASSIG_NEW_TASK",true,TeamworkPermissions.assignment_manage,"", pageState);
      SmartCombo newParent = TaskBricks.getTaskCombo("ASSIG_NEW_TASK",true,TeamworkPermissions.assignment_canCRW,"", pageState);
      //newParent.required = true;
      //exclude myself
      newParent.queryHelperForFiltering.addOQLClause(" not task.id=:tid","tid",task.getId());
      newParent.separator = "<br>";
      newParent.script = "style='margin-bottom:10px'";
      newParent.toHtmlI18n(pageContext);
    %><br><hr><%
    ButtonSubmit move = ButtonSubmit.getSaveInstance(form, I18n.get("MOVE"));
    move.variationsFromForm.setCommand("ASSIG_MOVE");
    move.alertOnRequired = true;
    move.toHtml(pageContext);

  %></div><%
  }
  %>
</div><%
    }


  // Operator Load
  PageSeed ol = pageState.pageFromRoot("task/plan/operatorLoad.jsp");
  ol.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));
  ol.setPopup(true);
  ol.addClientEntry("REFERRAL_TYPE", Resource.class.getName());
  ol.setCommand("FIND_BY_ENTITY");

%>
<script type="text/javascript">
  $(function(){
    loadSubsAndCost();
  });

  function showAssigMover(el){

    if (getBlackPopup().size()>0){
      closeBlackPopup();
    } else {
      var ndo= createModalPopup(600,400,null,null,el);
      var icche=$("#assig_move").clone();

      ndo.append(icche);
      icche.show();
    }
  }


  function showSubs(el){
    $('#task_subs').fadeIn(300,function(){el.remove()});
  }

  function loadSubscriptions(){
    var data={
      CM:"ED",
      //assId:"<%=assig.getId()%>",
      taskId:"<%=task.getId()%>",
      resId:$("#ASSIGNEE").val(),
      roleId:$("#ASSIG_ROLE").val(),
      assIsNew:"<%=(assig.isNew()?"1":"0")%>"
    };
    $("#task_subs_ajax").load("partTaskSubscription.jsp",data);
  }

  function loadCost(){
    var data={
      CM:"GETRESCOST",
      resId:$('#ASSIGNEE').val()
    };
    $.getJSON("taskAjaxController.jsp",data,function(response){
      jsonResponseHandling(response);
      if (response.ok){
        $("#ASSIG_COST").val(response.resCost);
      }
    });
  }


  function loadSubsAndCost(){
    loadSubscriptions();
    var assIsNew=<%=assig.isNew()?"true":"false"%>;
    if ( assIsNew|| (!getCurrencyValue($("#ASSIG_COST").val()) || getCurrencyValue($("#ASSIG_COST").val())<=0)) //load always if new
      loadCost();
  }


  function openOperatorLoad(){
    var resId=$("#ASSIGNEE").val();
    openBlackPopup("<%=ol.toLinkToHref()%>&REFERRAL_ID="+resId,1024,768);

  }
</script>
<%






  %></div></div></div>
<%---------------------------------------------- MAIN COLUMN END ---------------------------------------------------------%>

<%---------------------------------------------- RIGHT COLUMN START ---------------------------------------------------------%><%
  JspHelper side = new JspHelper("part/partTaskSideBar.jsp");
  side.parameters.put("TASK", task);
  side.toHtml(pageContext);

%><%---------------------------------------------- RIGHT COLUMN END ---------------------------------------------------------%><%

    form.end(pageContext);
  }
%>
