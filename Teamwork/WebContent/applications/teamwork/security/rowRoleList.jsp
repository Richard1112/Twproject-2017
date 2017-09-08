<%@ page import="com.twproject.operator.TeamworkOperator" %>
<%@ page import="com.twproject.security.RoleTeamwork" %>
<%@ page import="org.jblooming.security.PlatformPermissions" %>
<%@ page import="org.jblooming.utilities.JSP, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jblooming.waf.html.button.ButtonJS, com.twproject.security.SecurityBricks" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  JspHelper rowDrawer = (JspHelper) JspHelper.getCurrentInstance(request);
  RoleTeamwork role = (RoleTeamwork) rowDrawer.parameters.get("ROW_OBJ");
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();


  PageSeed pi = pageState.pageInThisFolder("roleEditor.jsp", request);
  pi.setMainObjectId(role.getId());
  pi.setCommand(Commands.EDIT);
  ButtonLink bEdit = new ButtonLink(pi);
  bEdit.iconChar = "e";
  bEdit.label = "";


  boolean isReservedRole=role.getCode().startsWith("(") && role.getCode().endsWith(")");

  ButtonJS delete = new ButtonJS("delRow($(this));");
  delete.toolTip = I18n.get("DELETE");
  delete.enabled = role.hasPermissionFor(logged, PlatformPermissions.role_canCreate) && !isReservedRole;
  delete.iconChar = "d";
  delete.label = "";
  delete.additionalCssClass = "delete";


%>
<tr class="alternate" roleId="<%=role.getId()%>">
  <td align="center"><%bEdit.toHtmlInTextOnlyModality(pageContext);%></td>
  <td><%=role.getCode()%> </td>
  <td><%=role.getName()%> </td>
  <td><%=JSP.w(role.getDescription())%> </td>
  <td class="<%=SecurityBricks.isSingleArea()?"displayNone":""%>"><%=role.getArea() != null ? JSP.w(role.getArea().getName()) : I18n.get("SYSTEM_ROLE")%> </td>
  <td><%=JSP.w(role.isLocalToAssignment())%> </td>

  <% pi.setCommand(Commands.DELETE_PREVIEW);%>
  <td align="center"><%delete.toHtmlInTextOnlyModality(pageContext);%></td>

</tr>




