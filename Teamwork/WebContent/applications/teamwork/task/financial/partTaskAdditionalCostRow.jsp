<%@ page import="com.twproject.security.TeamworkPermissions,
                 com.twproject.task.Task,
                 com.twproject.task.financial.Cost,
                 org.jblooming.agenda.CompanyCalendar,
                 org.jblooming.designer.DesignerField,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.html.core.JspHelper,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.ClientEntry,
                 org.jblooming.waf.view.PageState, org.jblooming.waf.html.display.Img, org.jblooming.waf.html.button.ButtonJS" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);
  JspHelper currentInstance = (JspHelper) JspHelper.getCurrentInstance(request);
  Cost cost = null;
  Task task;
  boolean canManageCosts=true;
  if (currentInstance != null) {
    cost = (Cost) currentInstance.parameters.get("cost");
    task = (Task) currentInstance.parameters.get("task");
    canManageCosts=(Boolean)(currentInstance.parameters.get("canManageCosts"));
  } else {
    cost = Cost.load(pageState.getEntry("costId").stringValueNullIfEmpty() + "");
    task=Task.load(pageState.getEntry("taskId").stringValueNullIfEmpty() + "");
    canManageCosts=task.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.task_canWrite)||task.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.task_canWrite);
  }


  boolean readOnly=!canManageCosts || cost.getStatus()!=null;


  ButtonJS openEditor=new ButtonJS(cost.getDescription(),"editAddCost($(this));");
  %>

<tr costId="<%=cost.getId()%>" taskId="<%=task.getId()%>" class="data">
  <td><%openEditor.toHtmlInTextOnlyModality(pageContext);%></td>
  <td><%=cost.getClassification()==null?"":cost.getClassification().getDescription()%></td>
  <td><%=JSP.w(cost.getCreationDate())%></td>
  <td style="text-align: right" class="addCstEstim"><%=JSP.currency(cost.getEstimatedCost())%></td>
  <td style="text-align: right" class="addCstReal"><%=JSP.currency(cost.getRealCost())%></td>
  <%
    if ( DesignerField.hasCustomField("COST_ADD_CUSTOM_FIELD_", 4)) {
      for (int i = 1; i < 5; i++) {
        DesignerField dfStr = DesignerField.getCustomFieldInstance("COST_ADD_CUSTOM_FIELD_", i, cost, true, true, false, pageState);
        if (dfStr!=null){
          dfStr.label="";
          dfStr.separator="";
  %><td><%dfStr.toHtml(pageContext);%></td><%
      }
    }
  }

    %><td align="center"><%
    if (cost.getAttachment()!=null) {
      String imgRoot = "mime/" + cost.getAttachment().getMimeImageName();
      %><a href="<%=cost.getAttachment().getDownloadOrViewLink().href%>"><%new Img(imgRoot, "", "20","").toHtml(pageContext);%></a><%
    }
    %></td>

  <td align="center" class="noprint" nowrap>
    <%  if (!readOnly){%>
    <span class="teamworkIcon edit" style="cursor: pointer" onclick="editAddCost($(this));" style="cursor: pointer;">e</span>
    <span class="teamworkIcon delete" style="cursor: pointer" onclick="$(this).confirm(function(){deleteAddCost($(this))}, '<%=I18n.get("DO_YOU_CONFIRM")%>');" style="cursor: pointer;">d</span>
    <%}%>
  </td>
</tr>
