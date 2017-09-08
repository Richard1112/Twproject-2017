<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.task.Assignment, com.twproject.worklog.Worklog, com.twproject.worklog.WorklogPlan, org.jblooming.agenda.CompanyCalendar, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.Paginator, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, com.twproject.worklog.businessLogic.WorklogBricks, org.jblooming.waf.html.input.ColorValueChooser, com.twproject.task.financial.Cost, com.twproject.security.TeamworkPermissions, org.jblooming.waf.html.button.AHref, org.jblooming.waf.html.display.Img, org.jblooming.security.License" %>
<%
  JspHelper rowDrawer = (JspHelper) JspHelper.getCurrentInstance(request);
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  if ("DRAW_PAGE_FOOTER".equals(request.getAttribute(Paginator.ACTION))) {


    //global totals from controller
    double costTotal = pageState.getEntry("COST_TOTAL").doubleValueNoErrorNoCatchedExc();

    //page totals from drawer
    Double totPgCosts = (Double) rowDrawer.parameters.get("totPgCosts");

    //se il totale per pagina è uguale al totalone si elimina la riga per pagina

%>
<tfoot>
<tr class="totals" style="<%=totPgCosts==costTotal?"display:none;":""%>">
  <td align="right" colspan="9"><i><%=I18n.get("COST_TOTAL_FOR_PAGE")%>:</i></td>
  <td style="border-top:1px solid #808080;" align="right" id="pageTotal"><%=JSP.currency(totPgCosts)%></td>
  <td>&nbsp;</td>
  <td colspan="2">&nbsp;</td>
</tr>


<tr class="totals"  id="costTotalRow">
  <td align="right" colspan="9"><i><%=I18n.get("COST_TOTAL")%>:</i></td>
  <td style="border-top:1px solid #808080;" align="right" id="costTotal"><%=JSP.currency(costTotal)%></td>
  <td>&nbsp;</td>
  <td colspan="2">&nbsp;</td>
</tr>

<tr><td id="bulkPlace" colspan="99"></td></tr>
</tfoot>
<%

  rowDrawer.parameters.remove("totPgCosts");

} else {
  Object[]ob= (Object[]) rowDrawer.parameters.get("ROW_OBJ");
    Assignment assig =(Assignment)ob[0];
    Cost cost =(Cost)ob[1];

  // sono resettati alla fine di ogni pagina, se non ci sono si mettono a 0
  if (!rowDrawer.parameters.containsKey("totPgCosts")) {
    rowDrawer.parameters.put("totPgCosts", new Double(0));
  }
  Double totPgCosts = (Double) rowDrawer.parameters.get("totPgCosts");

  boolean isYours = logged.getPerson().equals(assig.getResource());
  boolean canReadExp = assig.hasPermissionFor(logged, TeamworkPermissions.task_cost_canRead);
  boolean canManageExp = assig.hasPermissionFor(logged, TeamworkPermissions.expense_manage);

  boolean canRead = isYours||canReadExp||canManageExp;

  totPgCosts = totPgCosts + cost.getRealCost();

  ColorValueChooser stsCh = WorklogBricks.getStatusChooser("STSCH"+cost.getId(), false, pageState);
  stsCh.showOpener=false;
  stsCh.displayValue=false;
  stsCh.disabled=!License.assertLevel(30);
  stsCh.onChangeScript="rowChangeStatus(hidden)";
  pageState.addClientEntry("STSCH"+cost.getId(),cost.getStatus());

//    PageSeed taskAssListPS = pageState.pageFromRoot("task/taskAssignmentList.jsp");
//    taskAssListPS.command=Commands.FIND;
//    taskAssListPS.addClientEntry("TASK_ID", assig.getTask());

    PageSeed taskPS = pageState.pageFromRoot("task/taskOverview.jsp");
    taskPS.command=Commands.EDIT;
    taskPS.mainObjectId=assig.getTask().getId();
    ButtonLink taskAssList = new ButtonLink(taskPS);
    taskAssList.enabled=assig.hasPermissionFor(logged,TeamworkPermissions.task_canRead);

%>


<tr class="alternate" costId="<%=cost.getId()%>" assId="<%=assig.getId()%>">
  <td><input type="checkbox" onclick="refreshBulk($(this));" class="selector"></td>
  <%if(canRead){%>
    <td align="center"><%stsCh.toHtml(pageContext);%></td>
  <%}else{%>
    <td align="center"><%=WorklogBricks.drawStatus(cost.getStatus(),pageState)%></td>
  <%}%>
  <td><%
    PageSeed resEd = pageState.pageFromRoot("resource/resourceEditor.jsp");
    resEd.command=Commands.EDIT;
    resEd.mainObjectId=assig.getResource().getId();

    ButtonLink resBL = new ButtonLink(resEd);
    resBL.label = assig.getResource().getDisplayName();
    resBL.enabled=assig.getResource().hasPermissionFor(logged, TeamworkPermissions.resource_canRead);
    resBL.toHtmlInTextOnlyModality(pageContext);
  %></td>
  <td title="<%=JSP.w(assig.getRole().getDisplayName())%>"><%=JSP.w(assig.getRole().getCode())%></td>
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
  <td class="textSmall"><%=cost.getClassification()!=null?cost.getClassification().getDescription():"&nbsp;"%></td>
  <td class="textSmall"><%=cost.getCreationDate()%></td>

  <td style="word-break: break-all"><%=JSP.w(cost.getDescription())%></td>

  <td align="right" class="realCost"><%=JSP.currency(cost.getRealCost())%></td>

  <td align="center"><%
    if (cost.getAttachment()!=null) {
      String imgRoot = "mime/" + cost.getAttachment().getMimeImageName();
        %><a href="<%=cost.getAttachment().getDownloadOrViewLink().href%>" title="<%=cost.getAttachment().getOriginalFileName()%>"><%new Img(imgRoot, "", "20","").toHtml(pageContext);%></a><%
    } else {
      %>&nbsp;<%
    }
  %></td>


  <td align="center" nowrap><%

    PageSeed edit = pageState.pageFromRoot("task/financial/expenseEditor.jsp");
    edit.addClientEntry("assId",assig.getId());
    edit.addClientEntry("costId",cost.getId());
    ButtonSupport link = ButtonLink.getBlackInstance("", 500, 500, edit);
    link.target = "wlEdit";
    link.enabled=canRead;
    link.iconChar="e";
    link.toHtmlInTextOnlyModality(pageContext);



    ButtonJS delData = new ButtonJS("deleteCost($(this));");
    delData.confirmRequire = true;
    delData.confirmQuestion = I18n.get("CONFIRM_COST_DELETION");
    delData.onClickScript = "deleteCost($(this));";
    delData.enabled = canRead;
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


    rowDrawer.parameters.put("totPgCosts", totPgCosts);
  }
%>



