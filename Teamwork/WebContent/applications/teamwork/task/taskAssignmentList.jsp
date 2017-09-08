<%@ page import="com.twproject.task.Task, com.twproject.task.businessLogic.AssignmentController, com.twproject.waf.TeamworkHBFScreen, com.twproject.waf.html.TaskAssignmentDrawer,
com.twproject.waf.html.TaskHeaderBar, org.jblooming.utilities.DateUtilities, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields,
org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n,
org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jblooming.waf.html.button.ButtonSupport, com.twproject.security.TeamworkPermissions" %><%


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

    PageSeed ps = pageState.thisPage(request);
    ps.addClientEntry("TASK_ID", task.getId());
    ps.command = Commands.FIND;
    Form form = new Form(ps);
    form.start(pageContext);
    pageState.setForm(form);
%>
<%---------------------------------------------- MAIN COLUMN START ---------------------------------------------------------%>
<div class="mainColumn">
<%

  //-----------------HEAD BAR START

  pageState.addClientEntry("TASK_TABSET","TASK_ASSIGNMENT_TAB");

  TaskHeaderBar head = new TaskHeaderBar(task);
  head.pathToObject.destination=pageState.pageInThisFolder("taskAssignmentList.jsp",request);
  head.pathToObject.alternativeCEForMainObject="TASK_ID";
  head.toHtml(pageContext);

  //-----------------HEAD BAR END



%><div class="" style="min-height: 360px"><%
//---------------------------------------------------------------------------------  START PROCESS MANAGEMENT ------------------------------------------------------------------------------------
if (task.isProcessDriven()){

  // add parameter to form
  pageState.getForm().url.addClientEntry("TRANSITIONID","");
  pageState.getForm().url.addClientEntry("TASKINSTANCE","");

  // can you manage this flow?
  %><div class="box">
<span class="teamworkIcon">&pound;</span> <b><%=I18n.get("TASK_STATE_MANAGED_BY_PROCESS")%></b> <%=I18n.get("PROCESS_CANNOT_ADD_REMOVE_ASSIG")%> <%
  PageSeed seeProcess=pageState.pageFromRoot("task/taskProcess.jsp");
  seeProcess.command= "SHOW_GRAPH";
  seeProcess.mainObjectId=task.getId();
  seeProcess.setPopup(true);

    ButtonSupport process = ButtonLink.getBlackInstance(I18n.get("SEE_PROCESS"), seeProcess);
    process.toHtmlInTextOnlyModality(pageContext);%><%

      %></div><%
}
//---------------------------------------------------------------------------------  END PROCESS MANAGEMENT ------------------------------------------------------------------------------------


  %><div><%

  //add assignment if in taskAssignList
  if(!task.isNew()  &&  task.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.assignment_canCRW) ) {
    PageSeed psAddAssignment = pageState.pageFromRoot("task/taskAssignmentNew.jsp");
    psAddAssignment.addClientEntry("TASK_ID",task.getId());
    psAddAssignment.command = Commands.ADD;
  }

  %><div class="listPagedisplayOptions"><%

  pageState.getEntryOrDefault("ASS_SHOW_CHILDREN");
  CheckField showCh = new CheckField("ASS_SHOW_CHILDREN", "&nbsp;", false);
  showCh.preserveOldValue = false;

  if (task.getChildrenSize() > 0) {
    showCh.additionalOnclickScript = new ButtonSubmit(form).generateJs().toString();
    showCh.toHtmlI18n(pageContext);
  }

  pageState.getEntryOrDefault("ASS_SHOW_DISABLED");
  CheckField showDisabled = new CheckField("ASS_SHOW_DISABLED", "&nbsp;", false);
  showDisabled.preserveOldValue = false;
  showDisabled.additionalOnclickScript = new ButtonSubmit(form).generateJs().toString();
  showDisabled.toHtmlI18n(pageContext);

  %><span class="lreq20 lreqLabel"><%
  boolean showWorload=pageState.getEntryOrDefault("ASS_SHOW_WORKLOAD").checkFieldValue();
  CheckField showWorloadCF = new CheckField("ASS_SHOW_WORKLOAD", "&nbsp;", false);
  showWorloadCF.preserveOldValue = false;
  showWorloadCF.additionalOnclickScript = new ButtonSubmit(form).generateJs().toString();
  showWorloadCF.toHtmlI18n(pageContext);
  %></span><%

  TaskAssignmentDrawer assignmentDrawer = new TaskAssignmentDrawer(task);
  assignmentDrawer.showWorload=showWorload;
  assignmentDrawer.init(pageContext);

  %></div>
</div>
  <table class="table dataTable fixHead fixFoot" id="multi"  border="0" cellpadding="0" cellspacing="2">

  <thead class="dataTableHead" style=""><tr>
    <th width="40" class="tableHead"></th>
    <th colspan="2" class="tableHead" id="assResNameHead"><span class="tableHeadEl"><%=I18n.get("NAME")%></span> </th>
    <th class="tableHead"><span class="tableHeadEl"><%=I18n.get("ROLE")%></span></th>
    <th width="40" class="tableHead lreq20 lreqLabel"><span class="tableHeadEl"><%=I18n.get("PRIORITY")%></span></th>
    <th class="tableHead fixedWidthAlignRight" colspan="<%=showWorload?2:1%>"><span class="tableHeadEl"><%=I18n.get("WORKLOG_ESTIMATED_SHORT")%></span></th>
    <th class="tableHead fixedWidthAlignRight"><span class="tableHeadEl"><%=I18n.get("WORKLOG_ESTIMATED_ISSUES")%></span></th>
    <th class="tableHead fixedWidthAlignRight"><span class="tableHeadEl"><%=I18n.get("WORKLOG_DONE_SHORT")%></span></th>
    <th class="tableHead fixedWidthAlignRight" width="1%"></th>
    <th class="tableHead fixedWidthAlignRight lreq30 lreqLabel" nowrap><span class="tableHeadEl"><%=I18n.get("PLAN_BY_RESOURCE")%></span></th>
    <th class="tableHead fixedWidthAlignRight"><span class="tableHeadEl"><%=I18n.get("HOURLY_COST_LONG")%></span></th>
  </tr></thead><%

  assignmentDrawer.modality = TaskAssignmentDrawer.Draw_Modality.DRAW_TASK;

  // qui si disegna la riga
  assignmentDrawer.drawTask(pageContext);


  long wlUnasigIssues=task.getWorklogEstimatedFromUnassignedIssues();
  if (wlUnasigIssues>0){

    %><tr><td colspan="6" align="right"><%=I18n.get("WORKLOG_UNASSIGNEED_ISSUES")%></td>
    <td colspan="1" align="right"><% PageSeed psIs=pageState.pageFromRoot("issue/issueList.jsp");
    psIs.addClientEntry("FLT_ISSUE_TASK",task);
    psIs.addClientEntry("FLT_ISSUE_UNASSIGNED", Fields.TRUE);
    psIs.command= Commands.FIND;
    new ButtonLink(DateUtilities.getMillisInHoursMinutes(wlUnasigIssues),psIs).toHtmlInTextOnlyModality(pageContext);%></td>
    <td colspan="3">&nbsp;</td> </tr><%
  }

%><tfoot>
  <tr valign="top" class="totals">
    <td align="right" colspan="<%=showWorload?6:5%>"><%=I18n.get("TOTALS")%> (hh:mm)</td>
    <td align="right" id="tot_estWlg">&nbsp;</td>
    <td align="right" id="tot_estIss">&nbsp;</td>
    <td align="right" id="tot_wlg">&nbsp;</td>
    <td align="right"></td>
    <td align="right" id="tot_plan">&nbsp;</td>
    <td class="totals">&nbsp;</td>
  </tr>

  <tr valign="top" class="totals">
    <td align="right" colspan="<%=showWorload?6:5%>"><%=I18n.get("TOTALS")%> (dd hh:mm)</td>
    <td align="right" id="tot_estWlgD">&nbsp;</td>
    <td align="right" id="tot_estIssD">&nbsp;</td>
    <td align="right" id="tot_wlgD">&nbsp;</td>
    <td align="right"></td>
    <td align="right" id="tot_planD">&nbsp;</td>
    <td class="totals">&nbsp;</td>
  </tr>
  </tfoot>

</table>
  </div></div>

<%---------------------------------------------- MAIN COLUMN END ---------------------------------------------------------%>

<%---------------------------------------------- RIGHT COLUMN END ---------------------------------------------------------%><%
  JspHelper side = new JspHelper("part/partTaskSideBar.jsp");
  side.parameters.put("TASK", task);
  side.toHtml(pageContext);
%>
<%---------------------------------------------- RIGHT COLUMN END ---------------------------------------------------------%>

<jsp:include page="plan/workloadUtilities.js.jsp"></jsp:include>
<script>

  //inject the table search
  createTableFilterElement($("#assResNameHead"),".dataTable [assId][resid]",".columnAssigName,.assRole");

  $(function(){
    if(<%=showWorload%>)
      $("[data-workload]").each(function(){updateResourceLoad($(this))});
  });
</script>
 <%
     if (task.getChildrenSize() > 0) {
       pageState.setFocusedObjectDomId("ck_" + showCh.fieldName);
     }

    form.end(pageContext);
   }
 %>


