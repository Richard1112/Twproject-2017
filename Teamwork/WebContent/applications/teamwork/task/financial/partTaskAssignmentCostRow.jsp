<%@ page import="com.twproject.operator.TeamworkOperator,
                 com.twproject.security.TeamworkPermissions,
                 com.twproject.task.Assignment,
                 com.twproject.task.financial.Cost,
                 com.twproject.worklog.businessLogic.WorklogBricks,
                 org.jblooming.designer.DesignerField,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.html.button.ButtonJS,
                 org.jblooming.waf.html.core.JspHelper,
                 org.jblooming.waf.html.display.Img,
                 org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, org.jblooming.waf.html.input.ColorValueChooser, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.view.PageSeed, org.jblooming.security.License" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged= (TeamworkOperator) pageState.getLoggedOperator();
  JspHelper currentInstance = (JspHelper) JspHelper.getCurrentInstance(request);
  Cost cost = null;
  Assignment assig;
  if (currentInstance != null) {
    cost = (Cost) currentInstance.parameters.get("cost");
    assig = (Assignment) currentInstance.parameters.get("assig");
  } else {
    cost = Cost.load(pageState.getEntry("costId").stringValueNullIfEmpty() + "");
    assig=Assignment.load(pageState.getEntry("assId").stringValueNullIfEmpty() + "");
  }

  boolean isYours = logged.getPerson().equals(assig.getResource());
  boolean canManageExpense = assig.hasPermissionFor(logged, TeamworkPermissions.expense_manage);
  boolean canReadTaskCost = assig.hasPermissionFor(logged, TeamworkPermissions.task_cost_canRead);

  if (!isYours && !canReadTaskCost && !canManageExpense)
    return;

  boolean lockedByDateOrStatus =cost.isLockedByDateOrStatus();

  boolean canModify=  (isYours && !lockedByDateOrStatus)  || canManageExpense;


  %>
<tr class="alternate" costId="<%=cost.getId()%>">
    <%--<td align="center"><%=WorklogBricks.drawStatus(cost.getStatus(), pageState)%></td>--%>

  <td align="center" class="lreq30 lreqHide">
    <%
      ColorValueChooser stsCh = WorklogBricks.getStatusChooser("STSCH"+cost.getId(), false, pageState);
      stsCh.showOpener=false;
      stsCh.displayValue=false;
      stsCh.disabled=!canModify || !License.assertLevel(30) ;
      stsCh.onChangeScript="rowChangeStatus(hidden)";
      pageState.addClientEntry("STSCH"+cost.getId(),cost.getStatus());
      stsCh.toHtml(pageContext);
    %>
  </td>
  <td><%=JSP.w(cost.getCreationDate())%></td>
    <td align="right" class="realCost"><%=JSP.currency(cost.getRealCost())%></td>
    <td style="word-break: break-all"><%=JSP.w(cost.getDescription())%></td>
    <td align="right"><%=cost.getClassification()==null?"&nbsp":JSP.w(cost.getClassification().getDisplayName())%></td>
    <td align="center"><%
      if (cost.getAttachment()!=null) {
        String imgRoot = "mime/" + cost.getAttachment().getMimeImageName();
        %><a href="<%=cost.getAttachment().getDownloadOrViewLink().href%>"><%new Img(imgRoot, "", "20","").toHtml(pageContext);%></a><%
      }
    %></td><%
  if (Cost.hasCustomField()) {
    for (int i = 1; i < 5; i++) {
     pageState.removeEntry("COST_CUSTOM_FIELD_" +i);
      DesignerField dfStr = DesignerField.getCustomFieldInstance("COST_CUSTOM_FIELD_", i, cost, true, true, false, pageState);
      if (dfStr != null) {
        dfStr.separator = "";
        dfStr.label="";
        dfStr.maxLength=20;
        %><td nowrap><%dfStr.toHtml(pageContext);%></td><%
      }
    }
  }

    %><td align="center" class="noprint">
    <%  if (canModify){%>
    <span class="teamworkIcon edit" style="cursor: pointer" onclick="editCost($(this));" style="cursor: pointer;">e</span>
    <span class="teamworkIcon delete" style="cursor: pointer" onclick="$(this).confirm(function(){deleteCost($(this))}, '<%=I18n.get("DO_YOU_CONFIRM")%>');" style="cursor: pointer;">d</span>
    <%}%>
  </td>
</tr>
