<%@ page import=" com.opnlb.website.page.WebSitePage, org.jblooming.utilities.JSP, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.html.button.ButtonJS" %>
<%
  JspHelper rowDrawer = (JspHelper) JspIncluderSupport.getCurrentInstance(request);
  WebSitePage wsPage = (WebSitePage) rowDrawer.parameters.get("ROW_OBJ");

  PageSeed edit = new PageSeed("pageEditor.jsp");
  ButtonLink editLink = ButtonLink.getTextualInstance("", edit);
  edit.mainObjectId = wsPage.getId();
  edit.setCommand(Commands.EDIT);
  editLink.iconChar = "e";

%>
<tr class="alternate" pageid="<%=wsPage.getId()%>">
  <td align="center"><%editLink.toHtmlInTextOnlyModality(pageContext);%></td>
  <td style="cursor:pointer" align="center"><%
    ButtonLink id = ButtonLink.getTextualInstance(wsPage.getId().toString(), edit);
    id.toHtmlInTextOnlyModality(pageContext);

  %></td>
  <td style="cursor:pointer"><b><%
    ButtonLink name = ButtonLink.getTextualInstance(wsPage.getName(), edit);
    name.toHtmlInTextOnlyModality(pageContext);

    PageSeed editTemplate = new PageSeed("templateEditor.jsp");
    editTemplate.setCommand(Commands.EDIT);
    editTemplate.mainObjectId = wsPage.getDefaultTemplate().getId();

  %></b></td>
  <td align="left" valign="middle"><%=JSP.w(wsPage.getDescription())%>
  </td>
  <td align="left" valign="middle"><%ButtonLink.getTextualInstance(wsPage.getDefaultTemplate().getName(), editTemplate).toHtml(pageContext);%></td>
  <td align="center" valign="middle"><%=wsPage.isActive() ? I18n.get("YES") : I18n.get("NO")%>
  </td>
  <%
    ButtonJS delLink = new ButtonJS("delRow($(this))");
    edit.setCommand(Commands.DELETE_PREVIEW);
    delLink.iconChar = "d";
    delLink.label = "";
    delLink.toolTip = I18n.get("DELETE") + " id: " + wsPage.getId() + " - " + I18n.get("NAME") + ": " + wsPage.getName();
  %>
  <td align="center"><%delLink.toHtmlInTextOnlyModality(pageContext);%></td>
</tr>