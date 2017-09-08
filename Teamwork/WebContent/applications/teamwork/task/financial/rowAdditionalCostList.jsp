<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.task.Assignment, com.twproject.worklog.Worklog, com.twproject.worklog.WorklogPlan, org.jblooming.agenda.CompanyCalendar, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.Paginator, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, com.twproject.worklog.businessLogic.WorklogBricks, org.jblooming.waf.html.input.ColorValueChooser, com.twproject.task.financial.Cost, com.twproject.security.TeamworkPermissions, org.jblooming.waf.html.button.AHref, org.jblooming.waf.html.display.Img, org.jblooming.designer.DesignerField, com.twproject.task.Task" %>
<%
  JspHelper rowDrawer = (JspHelper) JspHelper.getCurrentInstance(request);
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator loggedOperator = (TeamworkOperator) pageState.getLoggedOperator();

  if ("DRAW_PAGE_FOOTER".equals(request.getAttribute(Paginator.ACTION))) {


    //global totals from controller
    double costEstTotal = pageState.getEntry("COST_EST_TOTAL").doubleValueNoErrorNoCatchedExc();
    double costTotal = pageState.getEntry("COST_TOTAL").doubleValueNoErrorNoCatchedExc();

    //page totals from drawer
    Double totPgCosts = (Double) rowDrawer.parameters.get("totPgCosts");
    Double totPgEstCosts = (Double) rowDrawer.parameters.get("totPgEstCosts");

    //se il totale per pagina è uguale al totalone si elimina la riga per pagina

%>
<tfoot>
<tr class="totals" style="<%=totPgCosts==costTotal&&totPgEstCosts==costEstTotal?"display:none;":""%>">
  <td align="right" colspan="5"><i><%=I18n.get("COST_TOTAL_FOR_PAGE")%>:</i></td>
  <td style="border-top:1px solid #808080;" align="right" id="pageEstTotal"><%=JSP.currency(totPgEstCosts)%></td>
  <td style="border-top:1px solid #808080;" align="right" id="pageTotal"><%=JSP.currency(totPgCosts)%></td>
  <td>&nbsp;</td>
  <td colspan="2">&nbsp;</td>
</tr>


<tr class="totals"  id="costTotalRow">
  <td align="right" colspan="5"><i><%=I18n.get("COST_TOTAL")%>:</i></td>
  <td style="border-top:1px solid #808080;" align="right" id="costEstTotal"><%=JSP.currency(costEstTotal)%></td>
  <td style="border-top:1px solid #808080;" align="right" id="costTotal"><%=JSP.currency(costTotal)%></td>
  <td>&nbsp;</td>
  <td colspan="2">&nbsp;</td>
</tr>

<tr><td id="bulkPlace" colspan="99"></td></tr>
</tfoot>
<%

  rowDrawer.parameters.remove("totPgEstCosts");
  rowDrawer.parameters.remove("totPgCosts");

} else {
  Object[]ob= (Object[]) rowDrawer.parameters.get("ROW_OBJ");
  Task task =(Task)ob[0];
  Cost cost =(Cost)ob[1];

  // sono resettati alla fine di ogni pagina, se non ci sono si mettono a 0
  if (!rowDrawer.parameters.containsKey("totPgEstCosts")) {
    rowDrawer.parameters.put("totPgEstCosts", new Double(0));
  }
  if (!rowDrawer.parameters.containsKey("totPgCosts")) {
    rowDrawer.parameters.put("totPgCosts", new Double(0));
  }
  Double totPgEstCosts = (Double) rowDrawer.parameters.get("totPgEstCosts");
  Double totPgCosts = (Double) rowDrawer.parameters.get("totPgCosts");

  boolean canWrite = task.hasPermissionFor(loggedOperator, TeamworkPermissions.task_cost_canWrite);

  PageSeed edit = pageState.pageInThisFolder("additionalCostEditor.jsp", request);
  edit.addClientEntry("taskId", task.getId());
  edit.addClientEntry("costId",cost.getId());
  ButtonSupport link = ButtonLink.getBlackInstance("", 500, 500, edit);
  link.target = "wlEdit";
  link.enabled=canWrite;

  totPgEstCosts = totPgEstCosts + cost.getEstimatedCost();
  totPgCosts = totPgCosts + cost.getRealCost();
%>


<tr class="alternate" costId="<%=cost.getId()%>" taskId="<%=task.getId()%>">
  <td><%
    if (I18n.isActive("CUSTOM_FEATURE_ALWAYS_SHOW_TASK_PATH")){
      %><div class="pathSmall"><%=task.getPath(" / ", false)%></div><%
    }
    link.label = JSP.w(task.getName());
    link.toHtmlInTextOnlyModality(pageContext);
  %></td>
  <td><%
    link.label = JSP.w(task.getCode());
    link.toHtmlInTextOnlyModality(pageContext);
  %></td>
  <td class="textSmall"><%=cost.getClassification()!=null?cost.getClassification().getDescription():"&nbsp;"%></td>
  <td class="textSmall"><%
    link.label = JSP.w(cost.getCreationDate());
    link.toHtmlInTextOnlyModality(pageContext);
  %></td>

  <td style="word-break: break-all"><%=JSP.w(cost.getDescription())%></td>

  <td align="right" class="realCost"><%=JSP.currency(cost.getEstimatedCost())%></td>
  <td align="right" class="realCost"><%=JSP.currency(cost.getRealCost())%></td>

  <td align="center"><%
    if (cost.getAttachment()!=null) {
      String imgRoot = "mime/" + cost.getAttachment().getMimeImageName();
        %><a href="<%=cost.getAttachment().getDownloadOrViewLink().href%>" title="<%=cost.getAttachment().getOriginalFileName()%>"><%new Img(imgRoot, "", "20","").toHtml(pageContext);%></a><%
    } else {
      %>&nbsp;<%
    }
  %></td>

  <%
    //CUSTOM FIELDS
    for (int i = 1; i < 5; i++) {
      DesignerField dfStr = DesignerField.getCustomFieldInstance("ADD_COST_CUSTOM_FIELD_", i, cost, true, true, false, pageState);
      if (dfStr != null) {
        dfStr.separator = "";
        dfStr.label = "";
        %><td><%dfStr.toHtml(pageContext);%></td><%
      }
    }
  %>


  <td align="center"><%
    ButtonJS delData = new ButtonJS("deleteCost($(this));");
    delData.confirmRequire = true;
    delData.confirmQuestion = I18n.get("CONFIRM_COST_DELETION");
    delData.onClickScript = "deleteCost($(this));";
    delData.enabled = canWrite;
    delData.label = "";
    delData.iconChar = "d";
    delData.additionalCssClass = "delete";
    delData.toHtmlInTextOnlyModality(pageContext);
  %></td>
</tr>


<%


    rowDrawer.parameters.put("totPgEstCosts", totPgEstCosts);
    rowDrawer.parameters.put("totPgCosts", totPgCosts);
  }
%>



