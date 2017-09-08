<%@ page import="com.twproject.operator.TeamworkOperator,com.twproject.resource.Person,com.twproject.resource.Resource,com.twproject.security.TeamworkPermissions,com.twproject.task.Assignment,
com.twproject.task.Task,com.twproject.task.TaskBricks,com.twproject.task.businessLogic.AssignmentController,com.twproject.utilities.TeamworkComparators,
com.twproject.waf.html.TaskAssignmentDrawer,org.jblooming.agenda.CompanyCalendar,org.jblooming.agenda.Period,org.jblooming.oql.OqlQuery,
org.jblooming.utilities.DateUtilities,org.jblooming.utilities.JSP,org.jblooming.utilities.ReflectionUtilities,org.jblooming.waf.Bricks,
org.jblooming.waf.constants.Commands,org.jblooming.waf.html.button.ButtonJS,org.jblooming.waf.html.button.ButtonLink,org.jblooming.waf.html.button.ButtonSupport,org.jblooming.waf.html.container.ButtonBar,
org.jblooming.waf.html.container.DivOnMouseover,org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.html.display.PercentileDisplay, org.jblooming.waf.html.input.ColorValueChooser, org.jblooming.waf.html.input.SmartCombo, org.jblooming.waf.html.input.TextField, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.ArrayList, java.util.Collections, java.util.Date, java.util.List"%> <%


  TaskAssignmentDrawer taskAssignmentDrawer = (TaskAssignmentDrawer) JspIncluderSupport.getCurrentInstance(request);
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged= (TeamworkOperator) pageState.getLoggedOperator();

// ------------------------------------------------------------------ INIT ------------------------------------------------------------------
  if ("INIT".equals(request.getAttribute(TaskAssignmentDrawer.ACTION))) {

    // prepare delete previewer
    new DeletePreviewer("ASS_DEL",AssignmentController.class, pageState);


%><script>
  $(document).ready(function () {
    $("#multi tr[assId] :text").bind("blur",function(){updateValue($(this))});
    refreshTotals();

    //bind for worklogSaved event
    $("body").on("worklogSaved",function(ev,data){
      worklogSaved(data);
    });

  });

  function deletePreviewAssig(assId){
    deletePreview("ASS_DEL", assId, function(response){  // callback function
      if (response && response.ok){
        $("tr[assId="+assId+"]").fadeOut(500, function () {$(this).remove();});
        refreshTotals();
      }
    });
  }

  function  updateValue(theField){
    //save only if something is changed
    //console.debug("updateValue",theField.validateField())
    if (theField.isValueChanged() && theField.validateField()) {
      showSavingMessage();
      var assId = theField.parents("tr[assId]:first").attr("assId");
      var data={
        CM:"UPDATEASSIGROW",
        assId:assId,
        fld:theField.prop("name"),
        val:theField.val()
      };
      $.getJSON(contextPath+"/applications/teamwork/task/taskAjaxController.jsp", data,function(response){
        jsonResponseHandling(response);
        if (response.ok){
          theField.updateOldValue();
          refreshTotals();
        }
        hideSavingMessage();
      });
    }
  }

  function refreshTotals(){
    var estWlg = computeFieldTotal("#multi .estWlg");
    $("#tot_estWlg").text(getMillisInHoursMinutes(estWlg));
    $("#tot_estWlgD").text(getMillisInDaysHoursMinutes(estWlg));

    var estIss = computeTdTotal("#multi .estIss");
    $("#tot_estIss").text(getMillisInHoursMinutes(estIss));
    $("#tot_estIssD").text(getMillisInDaysHoursMinutes(estIss));

    var wlg = computeTdTotal("#multi .wlg");
    $("#tot_wlg").text(getMillisInHoursMinutes(wlg));
    $("#tot_wlgD").text(getMillisInDaysHoursMinutes(wlg));


    var pl=computeTdTotal("#multi .futPlan");
    $("#tot_plan").text(getMillisInHoursMinutes(pl));
    $("#tot_planD").text(getMillisInDaysHoursMinutes(pl));

  }


  function computeFieldTotal(filter){
    var tot=0;
    $(filter).each(function(){
      var el=$(this);
      tot+=millisFromString(el.val(),true);
    });
    return tot;
  }

  function computeTdTotal(filter){
    var tot=0;
    $(filter).each(function(id,el){
      var txt=$(el).text().trim();
      tot+=millisFromString(txt,true);
    });
    return tot;
  }

  function worklogSaved(response){
    //update wl in the cell
    $("#multi tr[assId="+response.worklog.assId+"] .wlg").text(getMillisInHoursMinutes(response.worklog.totalWl));

    // update totals
    var wlgc = computeTdTotal("#multi .wlg");
    $("#tot_wlg").text(getMillisInHoursMinutes(wlgc));
    $("#tot_wlgD").text(getMillisInDaysHoursMinutes(wlgc));
  }


  function enabDisabAssig(assId){
    showSavingMessage();
    var row = $("#multi tr[assid=" + assId + "]");
    var data={
      CM:"ENABDISABASS",
      assId:assId,
      enable:row.is(".assDisabled")?"yes":"no"
    };
    $.getJSON(contextPath+"/applications/teamwork/task/taskAjaxController.jsp", data,function(response){
      jsonResponseHandling(response);
      if (response.ok){
        row.toggleClass("assDisabled")
      }
      hideSavingMessage();
    });
  }


  function updateResourceLoad(el){
    var row=el.closest("tr");
    computeResourceLoad(el,{startMillis:parseInt(row.attr("startMillis")),endMillis:parseInt(row.attr("endMillis"))},row.attr("resId")); // 20/9/16 R&S eliminati i parametri currentWorkload,assId dato che qui tutto è già salvato su db , millisFromString(row.find("[data-workload]").val()),row.attr("assId"));
  }

</script>

<%----------------------------------  LINE ACTIONS  visible in task part only --------------------------------------------------------------%>
<%
  if ((taskAssignmentDrawer.modality==TaskAssignmentDrawer.Draw_Modality.DRAW_TASK) && I18n.isActive("CUSTOM_FEATURE_ASSIGNMENT_SPLIT") && taskAssignmentDrawer.task.hasPermissionFor(logged,TeamworkPermissions.assignment_canCRW)) {
%>
<%---------------------------------- LINE ACTIONS --------------------------------------------------------------%>
<script type="text/javascript">
  function manageSplitBox(assId) {

    if (getBlackPopup().size()>0){
      closeBlackPopup();
    } else {

      var ndo= createModalPopup(600,400);
      var icche=$(".splitBox").clone();

      //get assig data
      var theTr=$("tr[assId="+assId+"]");
      var estim=millisFromString(theTr.find(".estWlg").val(),true);
      var done=millisFromString(theTr.find(".wlg").text(),true);
      var newEstim= estim-done;

      icche.find(".splitAssigName").append(theTr.find("h2 span:first").clone());

      //set proposed value
      icche.find("#hoursSplitted").val(newEstim>0?getMillisInHoursMinutes(newEstim):"");

      ndo.append(icche);
      icche.show();
      icche.data("assId",theTr.attr("assId"));

    }
  }

  function splitAssignment(el){

    var icche=el.closest(".splitBox");
    var assId=icche.data("assId");

    var splitResource = icche.find("[name=splitResource]");
    var hours = icche.find("[name=hoursSplitted]");
    if(splitResource.isFullfilled() && hours.isFullfilled()){

      var req={
        CM:"SPLITASS",
        fromAss:assId,
        toRes:splitResource.val(),
        hours:hours.val()
      };

      showSavingMessage();
      $.getJSON(contextPath+'/applications/teamwork/task/taskAjaxController.jsp', req, function(response) {
        jsonResponseHandling(response);
        if (response.ok) {
          window.location.reload(); // this works fine because AssignmentController call response.sendRedirect instead of pagestate.redirect so the url is complete
          var taskField=$("[name=TASK_ID]");
          if (taskField.size()>0){
            window.location.href="taskAssignmentList.jsp?TASK_ID="+taskField.val();
          } else {
            window.location.href="resourceAssignments.jsp?CM=ED&OBJ_ID="+$("[name=OBJID]").val();
          }
        }
        hideSavingMessage();
      });

    }

  }

</script>

<div class="splitBox" style="display:none;padding:15px;">

  <h2><%=I18n.get("SPLIT_ASSIG_TITLE")%> <span class="splitAssigName"></span> </h2>
  <%

    Task task = taskAssignmentDrawer.task;

    String hql = "select distinct assig.resource.id, assig.resource.name from " + Assignment.class.getName() + " as assig where assig.task = :task order by assig.resource.name";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setEntity("task", task);
    List<Object[]> resOfAssigsOnTask = oql.list();

    SmartCombo assignee = task.bricks.getAssignableResourceCombo("splitResource", false, pageState);
    assignee.fieldSize = 40;
    assignee.required = true;
    assignee.separator = "<br>";
    assignee.label = I18n.get("ASSIGNEE");
    assignee.readOnly = false;
    assignee.additionalLines = resOfAssigsOnTask;
    assignee.toHtmlI18n(pageContext);


    TextField h = TextField.getDurationInMillisInstance("hoursSplitted");
    h.separator = "<br>";
    h.required = true;
    h.label = I18n.get("HOURS_SPLITTED");
    h.toHtml(pageContext);


    ButtonBar bb = new ButtonBar();
    ButtonJS bl = new ButtonJS("", "splitAssignment($(this));");
    bl.label = I18n.get("PROCEED");
    bl.additionalCssClass = "first";
    bb.addButton(bl);

    bb.toHtml(pageContext);
%>
</div>
<%}



  // ------------------------------------------------------------------  DRAW TASK PART ------------------------------------------------------------------
} else if ("TASKPART".equals(request.getAttribute(TaskAssignmentDrawer.ACTION))) {

  Task task = taskAssignmentDrawer.task;

  List<Assignment> assignements = task.getAssignementsSortedByRole();

  if (assignements.size()>0) {
    for (com.twproject.task.Assignment assignment : assignements) {
      taskAssignmentDrawer.currentAssignment = assignment;
      taskAssignmentDrawer.drawAssignment(assignment,pageContext);
    }
  }

  // ------------------------------------------------------------------  CHILDREN   ------------------------------------------------------------------
  if (pageState.getEntry("ASS_SHOW_CHILDREN").checkFieldValue()) {
    PageSeed assigList = pageState.pageFromRoot("task/taskAssignmentList.jsp");
    List<Task> children = new ArrayList(task.getChildren());
    Collections.sort(children, new TeamworkComparators.TaskManualOrderComparator());


    for (Task child : children) {

      if (!child.equals(pageState.getMainObject())) {
        assigList.addClientEntry("TASK_ID",child);
        ButtonLink edit = ButtonLink.getTextualInstance(I18n.get(child.getDisplayName()), assigList);

        PercentileDisplay progressBar = child.bricks.getProgressBar(true);
        progressBar.width="100px";
        progressBar.height="15px";

        String color=task.getTaskColor();
        String styleColor="";
        if (I18n.isActive("CUSTOM_FEATURE_USE_PROJECT_COLOR") && JSP.ex(color)){
          styleColor=" style=\"border-left: 5px solid "+color+";\"";
        }


%><tr>
  <td colspan="20" style="height: 20px">
</tr><tr>
  <td colspan="20" class="tableSection">
    <table class="table">
      <tr>
        <td nowrap <%=styleColor%>><div class="childNode rightSpace"><h2><span class="pathSmall inline"><%=child.getPath(" / ", false)%> /</span><%edit.toHtml(pageContext);%></h2></div></td>
        <td nowrap style="padding-left: 40px" width="100"><%child.bricks.getStatusIcon(18, pageState).toHtml(pageContext);%></td>
        <td nowrap width="130"><%progressBar.toHtml(pageContext);%></td><%
        //PageSeed assigEdit = pageState.pageFromRoot("task/taskAssignmentEditor.jsp");
        PageSeed assigEdit = pageState.pageFromRoot("task/taskAssignmentNew.jsp");
        assigEdit.addClientEntry("TASK_ID",child);
        assigEdit.command = Commands.ADD;

        ButtonSupport editLinkD = ButtonLink.getBlackInstance("",600,800, assigEdit);
        //editLinkD.enabled = child.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.assignment_manage);
        editLinkD.enabled = child.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.assignment_canCRW);
        editLinkD.toolTip = I18n.get("ADD_ASSIGNMENTS");
        editLinkD.iconChar="P";
        editLinkD.additionalCssClass="edit";


      %><td align="center" width="40"><%editLinkD.toHtmlInTextOnlyModality(pageContext);%></td><%

      %></tr>
    </table>
  </td></tr><%
      }
      TaskAssignmentDrawer recurseAssigOnChildren = new TaskAssignmentDrawer(child);
      recurseAssigOnChildren.showWorload=taskAssignmentDrawer.showWorload;
      recurseAssigOnChildren.drawTask(pageContext);

    }

  }

// ------------------------------------------------------------------ DRAW SINGLE ASSIGNMENT ------------------------------------------------------------------
} else {

  boolean showDisabled = pageState.getEntry("ASS_SHOW_DISABLED").checkFieldValue();

  Assignment assignment = taskAssignmentDrawer.currentAssignment;
  Task task = taskAssignmentDrawer.currentAssignment.getTask();
  if (!assignment.isEnabled() && !showDisabled)
    return;

  if (taskAssignmentDrawer.task.hasPermissionFor(logged,TeamworkPermissions.task_canRead)) {

    // make the client entries
    long stamanPrest = Period.getDayPeriodInstance(new Date()).getStartDate().getTime();
    pageState.addClientEntry("ASSPRIO_" + assignment.getId(), assignment.getPriorityAtTime(stamanPrest));
    pageState.addClientEntry("ACTIVE" + assignment.getId(), assignment.isEnabled());
    pageState.addClientEntryTime("ESTIMATE_WORKLOG" + assignment.getId(), assignment.getEstimatedWorklog());
    pageState.addClientEntryCurrency("HOURLY_COST" + assignment.getId(), assignment.getHourlyCost());


    Resource resource = assignment.getResource();
    resource = (Resource) ReflectionUtilities.getUnderlyingObject(resource);

    boolean isAssignedToMe = taskAssignmentDrawer.currentAssignment.getResource().equals(logged.getPerson());




%><tr class="alternate <%=assignment.isEnabled()?"":"assDisabled"%>" assId="<%=assignment.getId()%>" resId="<%=assignment.getResource().getId()%>" startMillis="<%=task.getSchedule().getValidityStartDate().getTime()%>" endMillis="<%=task.getSchedule().getValidityEndDate().getTime()%>">
<td style="width:40px;overflow: hidden" nowrap>
<img src="<%=resource.bricks.getAvatarImageUrl()%>" onclick="location.href='taskAssignmentEditor.jsp?TASK_ID=<%=task.getId()%>&OBJID=<%=assignment.getId()%>&CM=ED';" title="<%=resource.getDisplayName()+" ("+JSP.w(assignment.getRole().getCode())+")"%>" class="face" align="top" style="cursor:pointer;">

</td><td class="columnAssigName" title="<%=assignment.getDisplayNameFull()%>"><%
  ButtonLink bEdit = ButtonLink.getEditInstance("../taskAssignmentEditor.jsp", assignment, request);

  PageSeed ps = bEdit.pageSeed;
  ps.addClientEntry("TASK_ID", task);
  ps.mainObjectId = assignment.getId();
  ps.setCommand(Commands.EDIT);

  if (taskAssignmentDrawer.modality==TaskAssignmentDrawer.Draw_Modality.DRAW_TASK) {
    String resName;
    if (resource instanceof Person)
      resName= JSP.w(((Person) resource).getPersonSurname()) + " " + JSP.w(((Person) resource).getPersonName());
    else
      resName=resource.getName();


    ButtonLink ed = new ButtonLink(resName,ps);

    ed.additionalCssClass="bolder";
    ed.toHtmlInTextOnlyModality(pageContext);

  } else {
    if (I18n.isActive("CUSTOM_FEATURE_ALWAYS_SHOW_TASK_PATH")){
      %><div class="pathSmall"><%=assignment.getTask().getPath(" / ", false)%></div><%
    }
      ButtonLink ed = new ButtonLink(assignment.getTask().getName(),ps);
    ed.toHtmlInTextOnlyModality(pageContext);
  }

  ButtonJS opener = new ButtonJS("");
  opener.label="";
  opener.iconChar="[";
  opener.additionalCssClass="arrowSmall";
  DivOnMouseover domo = new DivOnMouseover(opener);


  //-----------------------------  ASSIG COSTS --------------------------------------------------
  if (task.hasPermissionFor(logged,TeamworkPermissions.task_cost_canWrite) ||  task.hasPermissionFor(logged,TeamworkPermissions.assignment_canCRW)|| task.hasPermissionFor(logged,TeamworkPermissions.resource_manage) || isAssignedToMe){
    PageSeed exp=pageState.pageFromRoot("task/financial/taskAssignmentCosts.jsp");
    exp.mainObjectId=taskAssignmentDrawer.currentAssignment.getId();

    ButtonSupport bjs = ButtonLink.getBlackInstance(I18n.get("ASSIG_EXPENSES"), 768, 1180, exp);
    bjs.additionalCssClass="lreq20 lreqLabel";
    domo.addButton(bjs);
    domo.addSeparator();
  }

  //-----------------------------  OPTIONS --------------------------------------------------
  boolean showOptions= I18n.isActive("CUSTOM_FEATURE_ASSIGNMENT_SPLIT") &&  task.hasPermissionFor(logged,TeamworkPermissions.assignment_canCRW);
  if (showOptions && TaskAssignmentDrawer.Draw_Modality.DRAW_TASK==taskAssignmentDrawer.modality){
    ButtonJS bjs = new ButtonJS("manageSplitBox('"+assignment.getId()+"');");
    bjs.label = I18n.get("SPLIT_ASSIGNMENT");
    domo.addButton(bjs);

    domo.addSeparator();
  }

  //-----------------------------  RESOURCE EDIT --------------------------------------------------
  if (taskAssignmentDrawer.modality==TaskAssignmentDrawer.Draw_Modality.DRAW_TASK && resource.hasPermissionFor(logged,TeamworkPermissions.resource_canRead)) {

    PageSeed resEdit = pageState.pageFromRoot("resource/resourceEditor.jsp");
    resEdit.command = Commands.EDIT;
    resEdit.mainObjectId=assignment.getResource().getId();
    ButtonLink resEdBL = new ButtonLink("",resEdit);
    resEdBL.toolTip=I18n.get("EDIT");
    resEdBL.label=I18n.get("EDIT") +" \"" + assignment.getResource().getDisplayName()+"\"";
    domo.addButton(resEdBL);
    domo.addSeparator();
  }

  if (resource.getMyself() != null) {

    ButtonSupport buttonMess = resource.bricks.getPopupMessage(pageState);
    buttonMess.iconChar="";
    buttonMess.label=buttonMess.toolTip;
    domo.addButton(buttonMess);
  }

  {
    PageSeed ol = pageState.pageFromRoot("task/plan/operatorLoad.jsp");
    //ol.setPopup(true);
    ol.addClientEntry("REFERRAL_TYPE", Resource.class.getName());
    ol.addClientEntry("REFERRAL_ID", resource.getId());
    ol.setCommand("FIND_BY_ENTITY");
    ol.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));
    ButtonLink but = new ButtonLink(I18n.get("TASK_WORKGROUP_OPERATOR_LOAD"), ol);
    domo.addButton(but);
  }
  {
    PageSeed ps1 = pageState.pageFromRoot("task/plan/planByResource.jsp");
    ps1.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));
    Bricks.addReferral(resource.getId(), Resource.class, ps1);
    ps1.setCommand("FIND_BY_ENTITY");
    ButtonLink sub = new ButtonLink(ps1);
    sub.label=I18n.get("PLAN_BY_RESOURCE");
    sub.additionalCssClass="lreq30 lreqLabel";
    domo.addButton(sub);
  }

  {
    if ( task.hasPermissionFor(logged,TeamworkPermissions.assignment_canCRW) || assignment.hasPermissionFor(logged,TeamworkPermissions.resource_manage) || isAssignedToMe) {
      ButtonJS enabDisab = new ButtonJS(I18n.get("ENABLE_DISABLE_ASSIG"), "enabDisabAssig("+assignment.getId()+");");
      domo.addButton(enabDisab);
    }
  }

  {
    if ( task.hasPermissionFor(logged,TeamworkPermissions.assignment_canCRW) && !(task.isProcessDriven()&&JSP.ex(assignment.getExternalCode()))) {
      ButtonJS deleteButton = new ButtonJS(I18n.get("ASSIG_DELETE"), "deletePreviewAssig(" + assignment.getId() + ")");
      domo.addButton(deleteButton);
    }
  }
  domo.toHtmlInTextOnlyModality(pageContext);
%>
</td>

<td class='textSmall columnTaskCode'>
  <%if ((taskAssignmentDrawer.modality==TaskAssignmentDrawer.Draw_Modality.DRAW_RESOURCE)){%>
    <span><%=JSP.w(task.getCode())%></span>
  <%}%>
</td>
<td class="textSmall assRole"><%=JSP.w(assignment.getRole().getDisplayName())%></td>
<td align="left" class="lreq20" nowrap><div class="status">

  <%
    ColorValueChooser chooser = TaskBricks.getAssignmentPriorityCombo("ASSPRIO_" + assignment.getId(), 15, pageState);
    chooser.readOnly=!task.hasPermissionFor(logged,TeamworkPermissions.assignment_canCRW) && !assignment.hasPermissionFor(logged,TeamworkPermissions.resource_manage);
    chooser.displayValue=true;
    chooser.showOpener=true;
    chooser.width=15;
    chooser.onChangeScript="updateValue(hidden);";
    chooser.toHtml(pageContext);
  %></div></td>

<%


  TextField tf = TextField.getDurationInMillisInstance("ESTIMATE_WORKLOG" + assignment.getId());
  tf.separator="";
  tf.fieldSize=7;
  tf.script=" style=\"text-align:right;\" ";
  tf.label = "";
  tf.fieldClass=tf.fieldClass+" estWlg";
  tf.readOnly = !taskAssignmentDrawer.task.hasPermissionFor(logged,TeamworkPermissions.assignment_canCRW);
  if (taskAssignmentDrawer.showWorload)
    tf.script="data-workload onchange='updateResourceLoad($(this));'";

%><td class="fixedWidthAlignRight"><%tf.toHtml(pageContext);%></td><%

  if (taskAssignmentDrawer.showWorload){
    %><td class="fixedWidthAlignRight workloadInfo" align="center"></td><%
  }


%><td class="fixedWidthAlignRight estIss"><%
  long worklogEstimatedFromIssues = assignment.getWorklogEstimatedFromIssues();

  PageSeed psIs=pageState.pageFromRoot("issue/issueList.jsp");
  psIs.addClientEntry("FLT_ISSUE_TASK",assignment.getTask());
  psIs.addClientEntry("FLT_ISSUE_ASSIGNED_TO",assignment.getResource());
  psIs.addClientEntry("FLT_ISSUE_ESTIMATED",">0");
  psIs.command= Commands.FIND;
  ButtonLink bl = new ButtonLink(DateUtilities.getMillisInHoursMinutes(worklogEstimatedFromIssues),psIs);
  bl.toHtmlInTextOnlyModality(pageContext);
%></td>

<td class="fixedWidthAlignRight"><%
  Person mySelf = logged.getPerson();

  long wd = assignment.getWorklogDone();
  PageSeed was = pageState.pageFromRoot("/task/worklog/worklogAssignmentList.jsp");
  was.addClientEntry("ASS_ID", assignment.getId());
  was.command=Commands.FIND;
  ButtonSupport wdbutton = ButtonLink.getBlackInstance("", 768, 1180, was);
  wdbutton.label="<span class='wlg'>" + (wd>0?DateUtilities.getMillisInHoursMinutes(wd):"") + "</span>";
  wdbutton.additionalCssClass=assignment.getEstimatedWorklog()<assignment.getWorklogDone()?"warning":"";
  wdbutton.toolTip=assignment.getEstimatedWorklog()<assignment.getWorklogDone()?I18n.get("CK_WORKLOG_OVERFLOW"):"";


  wdbutton.toHtmlInTextOnlyModality(pageContext);


  if (resource.equals(mySelf)){
    ButtonJS bjsw=new ButtonJS("openWorklogEditorPopup($(this),{assId:'"+assignment.getId()+"',title:'"+JSP.javascriptEncode(JSP.limWr(task.getDisplayName(),30))+"'});");
    bjsw.iconChar="w";
    bjsw.toHtmlInTextOnlyModality(pageContext);
  }

%>

</td>
<td>
  <%
  if (resource.equals(mySelf)){
    ButtonJS bjsc=new ButtonJS("openExpenseEditorPopup($(this),{assId:'"+assignment.getId()+"',title:'"+JSP.javascriptEncode(JSP.limWr(task.getDisplayName(),30))+"'});");
    bjsc.toolTip=I18n.get("ADD_EXPENSE");
    bjsc.iconChar="4";
    bjsc.additionalCssClass="lreq20 lreqHide";
    bjsc.toHtmlInTextOnlyModality(pageContext);
  }
%>
</td>

<td  class="fixedWidthAlignRight futPlan lreq30">
  <%
    if (taskAssignmentDrawer.modality==TaskAssignmentDrawer.Draw_Modality.DRAW_TASK){
      ButtonLink planButton = resource.bricks.getPlanButton(pageState);
      planButton.label=DateUtilities.getMillisInHoursMinutes(assignment.getWorklogPlanned(new CompanyCalendar().setAndGetTimeToDayStart()));
      planButton.toolTip=I18n.get("WORKLOG_SEE_RESOURCE_PLAN");
      planButton.toHtmlInTextOnlyModality(pageContext);
    } else {
      ButtonLink planButton = assignment.getTask().bricks.getPlanButton(pageState);
      planButton.label=DateUtilities.getMillisInHoursMinutes(assignment.getWorklogPlanned(new CompanyCalendar().setAndGetTimeToDayStart()));
      planButton.toolTip=I18n.get("WORKLOG_SEE_PLAN");
      planButton.toHtmlInTextOnlyModality(pageContext);
    }


  %>
</td>

<td class="fixedWidthAlignRight"><%

  if (taskAssignmentDrawer.task.hasPermissionFor(logged,TeamworkPermissions.task_cost_canRead)) {
    tf = TextField.getCurrencyInstance("HOURLY_COST" + assignment.getId());
    tf.separator="";
    tf.fieldSize=7;
    tf.script=" style=\"text-align:right;\" ";
    tf.label = "";
    tf.readOnly = !taskAssignmentDrawer.task.hasPermissionFor(logged,TeamworkPermissions.task_cost_canWrite) ;
%><%tf.toHtml(pageContext);%><%
} else {
%>&nbsp;<%
  }
%></td>

</tr><%
  }
%><%



  }

%>
