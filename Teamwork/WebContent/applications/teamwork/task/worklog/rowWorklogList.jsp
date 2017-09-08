<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.task.Assignment, com.twproject.worklog.Worklog, com.twproject.worklog.WorklogPlan, org.jblooming.agenda.CompanyCalendar, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.Paginator, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, com.twproject.worklog.businessLogic.WorklogBricks, org.jblooming.waf.html.input.ColorValueChooser, org.jblooming.security.License, com.twproject.resource.ResourceBricks, com.twproject.task.TaskBricks, com.twproject.security.TeamworkPermissions" %>
<%
  JspHelper rowDrawer = (JspHelper) JspHelper.getCurrentInstance(request);
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator loggedOperator = (TeamworkOperator) pageState.getLoggedOperator();
  boolean canReadCost = (Boolean) rowDrawer.parameters.get("canReadCost");

  if ("DRAW_PAGE_FOOTER".equals(request.getAttribute(Paginator.ACTION))) {


    //global totals from controller
    double worklogTotal = pageState.getEntry("WORLOG_TOTAL").doubleValueNoErrorNoCatchedExc();
    double worklogTotalCost = pageState.getEntry("WORLOG_TOTAL_COST").doubleValueNoErrorNoCatchedExc();

    //page totals from drawer
    Long totWL = (Long) rowDrawer.parameters.get("totWl");
    Double totCostFromAssig = (Double) rowDrawer.parameters.get("totCostFromAssig");


    //se il totale per pagina è uguale al totalone si elimina la riga per pagina
%>
<tfoot>
<tr class="totals" style="<%=totWL==worklogTotal?"display:none;":""%>">
  <td align="right" colspan="<%=License.assertLevel(20)?9:8%>">
    <i><%=I18n.get("WORKLOG_TOTAL_FOR_PAGE")%> (hh:mm):</i></td>
  <td style="border-top:1px solid #808080;" align="right">
    <%=DateUtilities.getMillisInHoursMinutes(totWL)%>
    </td>
  <td>&nbsp;</td>
  <td align="right"><%=canReadCost ? JSP.currency(totCostFromAssig) : "&nbsp;"%>
  </td>
  <td colspan="2">&nbsp;</td>
</tr>


<tr class="totals">
  <td align="right" colspan="<%=License.assertLevel(20)?9:8%>"><i><%=I18n.get("WORKLOG_TOTAL")%>
    (hh:mm):</i></td>
  <td align="right">
    <%=DateUtilities.getMillisInHoursMinutes(worklogTotal)%>
  </td>
  <td>&nbsp;</td>
  <td align="right">
    <%=canReadCost ? JSP.currency(worklogTotalCost) : "&nbsp;"%>
  </td>
  <td colspan="2">&nbsp;</td>
</tr>

<tr class="totals">
  <td align="right" colspan="<%=License.assertLevel(20)?9:8%>"><i><%=I18n.get("WORKLOG_TOTAL")%>
    (dd-hh:mm):</i></td>
  <td align="right">
    <%=DateUtilities.getMillisInDaysWorkHoursMinutes(worklogTotal)%>
    </td>
  <td colspan="4">&nbsp;</td>
</tr>
<tr><td id="bulkPlace" colspan="99"></td></tr>
</tfoot>
<%


  rowDrawer.parameters.remove("totWl");
  rowDrawer.parameters.remove("totCostFromAssig");


} else {
  Worklog workLog = (Worklog) rowDrawer.parameters.get("ROW_OBJ");

  // sono resettati alla fine di ogni pagina, se non ci sono si mettono a 0
  if (!rowDrawer.parameters.containsKey("totWl")) {
    rowDrawer.parameters.put("totWl", new Long(0));
  }
  Long totWL = (Long) rowDrawer.parameters.get("totWl");

  if (!rowDrawer.parameters.containsKey("totCostFromAssig")) {
    rowDrawer.parameters.put("totCostFromAssig", new Double(0));
  }


  Double totCostFromAssig = (Double) rowDrawer.parameters.get("totCostFromAssig");


  PageSeed wlw = pageState.pageInThisFolder("worklogWeek.jsp", request);
  boolean canWrite = workLog.bricks.canWrite(loggedOperator);


  totWL = totWL + workLog.getDuration();

  if (workLog.getInserted() != null)
    wlw.addClientEntry("FOCUS_MILLIS", "" + workLog.getInserted().getTime());

  Assignment assig = workLog.getAssig();

  wlw.addClientEntry("RES_ID", assig.getResource().getId());
  ButtonLink wlwb = new ButtonLink("", wlw);
  wlwb.toolTip = I18n.get("SEE_TIMESHEET");


  ColorValueChooser stsCh = WorklogBricks.getStatusChooser("STSCH"+workLog.getId(), false, pageState);
  stsCh.showOpener=false;
  stsCh.displayValue=false;
  stsCh.disabled=!!License.assertLevel(30);
  stsCh.onChangeScript="rowChangeStatus(hidden)";
  pageState.addClientEntry("STSCH"+workLog.getId(),workLog.getStatus());

//  PageSeed taskPS = pageState.pageFromRoot("task/taskAssignmentList.jsp");
//  taskPS.command=Commands.FIND;
//  taskPS.addClientEntry("TASK_ID", assig.getTask());

  PageSeed taskPS = pageState.pageFromRoot("task/taskOverview.jsp");
  taskPS.command=Commands.EDIT;
  taskPS.mainObjectId=assig.getTask().getId();

  ButtonLink taskAssList = new ButtonLink(taskPS);
  taskAssList.enabled=assig.hasPermissionFor(loggedOperator,TeamworkPermissions.task_canRead);

%>


<tr class="alternate <%=workLog.getClass().equals(WorklogPlan.class)?"plan":""%>" wlid="<%=workLog.getId()%>">
  <td><input type="checkbox" onclick="refreshBulk($(this));" class="selector"></td>
  <%if (License.assertLevel(20)){if(canWrite){%>
    <td align="center"><%stsCh.toHtml(pageContext);%></td>
  <%}else{%>
    <td align="center"><%=workLog.bricks.drawStatus(pageState)%></td>
  <%}}%>
  <td><%
    PageSeed resEd = pageState.pageFromRoot("resource/resourceEditor.jsp");
    resEd.command=Commands.EDIT;
    resEd.mainObjectId=assig.getResource().getId();

    ButtonLink resBL = new ButtonLink(resEd);
    resBL.label = assig.getResource().getDisplayName();
    resBL.enabled=assig.getResource().hasPermissionFor(loggedOperator, TeamworkPermissions.resource_canRead);
    resBL.toHtmlInTextOnlyModality(pageContext);
  %></td>
  <td>
    <%if (I18n.isActive("CUSTOM_FEATURE_ALWAYS_SHOW_TASK_PATH")){
      %><div class="pathSmall"><%=assig.getTask().getPath(" / ", false)%></div><%
    }

    taskAssList.label= JSP.w(assig.getTask().getName());
    taskAssList.toHtmlInTextOnlyModality(pageContext);
  %></td>
  <td><%
    taskAssList.label = JSP.w(assig.getTask().getCode());
    taskAssList.toHtmlInTextOnlyModality(pageContext);
  %></td>
  <td title="<%=JSP.w(assig.getRole().getDisplayName())%>"><%=JSP.w(assig.getRole().getCode())%>
  </td>
  <td class="textSmall"><%
    wlwb.label = JSP.w(workLog.getInserted());
    wlwb.toHtmlInTextOnlyModality(pageContext);
  %></td>

  <td style="word-break: break-all"><%=JSP.w(workLog.getAction())%></td>

  <td>
    <%if(workLog.getIssue()!=null){%>
      <span class="button textual textSmall" onclick="openIssueEditorInBlack('<%=workLog.getIssue().getId()%>','GUESS');return stopBubble(event);">I#<%=workLog.getIssue().getId()%>#</span>
    <%}%>
  </td>
  <td align="right" width="5%"><%=DateUtilities.getMillisInHoursMinutes(workLog.getDuration())%></td>
  <td align="right" width="5%"><%if (canReadCost) {%><%=JSP.currency(assig.getHourlyCost())%><%}%></td>

  <td align="right" width="5%">
    <%
      if (canReadCost) {
        double rowCost = 0;
        rowCost = (((double) workLog.getDuration()) / CompanyCalendar.MILLIS_IN_HOUR) * assig.getHourlyCost();
        totCostFromAssig = totCostFromAssig + rowCost;
    %><%=JSP.currency(rowCost)%> <%
    }%>
  </td>
  <td align="center" nowrap><%

    PageSeed edit = pageState.pageInThisFolder("worklogEdit.jsp", request);
    edit.setCommand(Commands.EDIT);
    edit.mainObjectId = workLog.getId();
    ButtonSupport link = ButtonLink.getBlackInstance("", 520, 680, edit);
    link.target = "wlEdit";
    link.iconChar="e";
    link.enabled=canWrite;
    link.toHtmlInTextOnlyModality(pageContext);



    ButtonJS delData = new ButtonJS("deleteWl($(this));");
    delData.confirmRequire = true;
    delData.confirmQuestion = I18n.get("CONFIRM_DELETE_WKL");
    delData.onClickScript = "deleteWl($(this));";
    delData.enabled = canWrite;
    delData.label = "";
    delData.iconChar = "d";
    delData.additionalCssClass = "delete";
    delData.toHtmlInTextOnlyModality(pageContext);
  %></td>
</tr>


<%--
    CUSTOM FIELDS
    for (int i = 1; i < 5; i++) {
      DesignerField dfStr = DesignerField.getCustomFieldInstance("WORKLOG_CUSTOM_FIELD_", i, workLog, true, true, false, pageState);
      if (dfStr != null) {
        dfStr.separator = "&nbsp;";
        dfStr.toHtml(pageContext);
        %>&nbsp;&nbsp;&nbsp;<%
      }
    }
--%>
<%


    rowDrawer.parameters.put("totWl", totWL);
    rowDrawer.parameters.put("totCostFromAssig", totCostFromAssig);

  }
%>



