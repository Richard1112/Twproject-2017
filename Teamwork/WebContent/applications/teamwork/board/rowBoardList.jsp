<%@ page import="com.twproject.messaging.board.Board, com.twproject.operator.TeamworkOperator, com.twproject.security.TeamworkPermissions, org.jblooming.utilities.JSP, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.view.PageSeed, org.jblooming.waf.constants.Commands" %>
<%
  JspHelper rowDrawer = (JspHelper) JspIncluderSupport.getCurrentInstance(request);
  Board b = (Board) rowDrawer.parameters.get("ROW_OBJ");

  PageState pageState = PageState.getCurrentPageState(request);

  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  if (!b.hasPermissionFor(logged, TeamworkPermissions.board_canRead))
    return;

  ButtonJS bEdit = new ButtonJS(b.getName(), "openBoardInBlack("+b.getId()+");");
%>
<tr class="alternate" boardId="<%=b.getId()%>">
  <td><%bEdit.toHtmlInTextOnlyModality(pageContext);%></td>
  <td width="40%"><%=JSP.w(b.getDescription())%></td>
  <td align="center"><%=JSP.w(b.getStickyNotes().size())%></td>
  <td align="center"><%=JSP.w(b.getLastPostedOn())%></td>
  <td align="center"><span class="teamworkIcon" title="<%=I18n.get(b.isActive()?"IS_ENABLED":"IS_DISABLED")%>"><%=I18n.get(b.isActive() ? ";" : "&iexcl;")%></span></td>
  <td align="center">
    <%
      if (b.hasPermissionFor(logged, TeamworkPermissions.board_canCreate)) {
        ButtonJS del = new ButtonJS("", "deleteBoard($(this));");
        del.iconChar = "d";
        del.additionalCssClass = "delete";
        del.confirmRequire=true;
        del.toHtmlInTextOnlyModality(pageContext);
      } else {
    %>-<%
    }%>
  </td>
</tr>
