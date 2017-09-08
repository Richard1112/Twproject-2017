<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.resource.Resource, com.twproject.resource.businessLogic.ResourceController, com.twproject.security.TeamworkPermissions, com.twproject.task.Assignment, com.twproject.waf.TeamworkHBFScreen, com.twproject.waf.html.ResourceHeaderBar, com.twproject.waf.html.TaskAssignmentDrawer, org.jblooming.agenda.Period, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.Img, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.Date, java.util.List, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.input.CheckField, java.util.Collections, com.twproject.utilities.TeamworkComparators" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new ResourceController(), request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);

    pageState.toHtml(pageContext);

  } else {

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    Resource resource = (Resource) pageState.getMainObject();



    PageSeed seed = pageState.thisPage(request);
    seed.setMainObjectId(resource.getId());
    seed.setCommand(Commands.EDIT);
    Form form = new Form(seed);
    pageState.setForm(form);
    form.alertOnChange = true;
    form.start(pageContext);


  %>
  <div class="mainColumn">
<%

  //---------------------------------------- HEAD BAR -------------------------------------------
  pageState.addClientEntry("RESOURCE_TABSET","RESOURCE_ASSIG_TAB");
  ResourceHeaderBar head = new ResourceHeaderBar(resource);
  head.toHtml(pageContext);


  pageState.getEntryOrDefault("ASS_SHOW_DISABLED");
  CheckField showDisabled = new CheckField("ASS_SHOW_DISABLED", "&nbsp;", false);
  showDisabled.preserveOldValue = false;
  showDisabled.additionalOnclickScript = new ButtonSubmit(form).generateJs().toString();


  %><div class="listPagedisplayOptions"><%showDisabled.toHtmlI18n(pageContext);%></div><%


  TaskAssignmentDrawer resAssigs = new TaskAssignmentDrawer(null);

  resAssigs.modality = TaskAssignmentDrawer.Draw_Modality.DRAW_RESOURCE;
  resAssigs.init(pageContext);

%><table id="multi" class="table fixHead fixFoot dataTable" >
  <thead>
    <tr>
    <th class="tableHead" ></th>
    <th class="tableHead" colspan="2" id="assNameHead"><%=I18n.get("ASSIGNMENTS_ON_OPEN_TASKS")%></th>
      <th class="tableHead"><%=I18n.get("ROLE")%></th>
      <th class="tableHead lreq20 lreqLabel" width="2%"><%=I18n.get("PRIORITY")%></th>
      <th class="tableHead" style="text-align: right"><%=I18n.get("WORKLOG_ESTIMATED_SHORT")%></th>
    <th class="tableHead" style="text-align: right"><%=I18n.get("WORKLOG_ESTIMATED_ISSUES")%></th>
    <th class="tableHead" style="text-align: right"><%=I18n.get("WORKLOG_DONE_SHORT")%></th>
    <th class="tableHead fixedWidthAlignRight" width="1%"></th>
    <th class="tableHead lreq30 lreqLabel" style="text-align: right" nowrap><%=I18n.get("PLAN_BY_TASK")%></th>
    <th class="tableHead" style="text-align: right" nowrap><%=I18n.get("HOURLY_COST_LONG")%></th>
  </tr>
  </thead>
  <%

    List<Assignment> assigs = resource.getActiveAssignments(null,false,true,false); // how all assignments for active tasks I#21034#
    Period dayPer = Period.getDayPeriodInstance(new Date());
    Collections.sort(assigs, new TeamworkComparators.AssignmentByPriority(dayPer.getStartDate()));


    for (Assignment assignment : assigs) {

      resAssigs.task = assignment.getTask();
      if(resAssigs.task.hasPermissionFor(logged, TeamworkPermissions.task_canRead)){
        resAssigs.drawAssignment(assignment, pageContext);
      }
    }

  %>
  <%
    if (assigs.size()>0) {
  %>
    <tfoot>
  <tr valign="top" class="totals">
    <td colspan="5" align="right"><%=I18n.get("TOTALS")%> (hh:mm)</td>
    <td align="right" id="tot_estWlg">&nbsp;</td>
    <td align="right" id="tot_estIss">&nbsp;</td>
    <td align="right" id="tot_wlg">&nbsp;</td>
    <td align="right" id="tot_plan">&nbsp;</td>
    <td colspan="3">&nbsp;</td>
  </tr>
  <tr valign="top" class="totals">
    <td colspan="5" align="right"><%=I18n.get("TOTALS")%> (dd hh:mm)</td>
    <td align="right" id="tot_estWlgD">&nbsp;</td>
    <td align="right" id="tot_estIssD">&nbsp;</td>
    <td align="right" id="tot_wlgD">&nbsp;</td>
    <td align="right" id="tot_planD">&nbsp;</td>
    <td colspan="3">&nbsp;</td>
  </tr>
    </tfoot>
<%
  }
%>
</table>
  <%
    if (assigs.size()==0) {
  %><div class="paginatorNotFound hint"><%=I18n.get("NO_RESULTS_HELP")%></div><%
    }
  %>
</div>
<script>
  //inject the table search
  createTableFilterElement($("#assNameHead"),".dataTable [assId][resid]",".columnAssigName,.columnTaskCode");
</script>

<%

    //---------------------------------------- SIDE BAR -------------------------------------------
    JspHelper side = new JspHelper("part/partResourceSideBar.jsp");
    side.parameters.put("RESOURCE", resource);
    side.toHtml(pageContext);

    form.end(pageContext);
  }
%>
