<%@ page import=" com.twproject.operator.TeamworkOperator, com.twproject.security.TeamworkPermissions, org.jblooming.remoteFile.FileStorage, org.jblooming.remoteFile.RemoteFile, org.jblooming.utilities.JSP, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);


  JspHelper rowDrawer = (JspHelper) JspIncluderSupport.getCurrentInstance(request);
  FileStorage document = (FileStorage) rowDrawer.parameters.get("ROW_OBJ");

  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  PageSeed edit = pageState.pageInThisFolder("fileStorageEditor.jsp", request);
  edit.setCommand(Commands.EDIT);
  ButtonLink editLink = ButtonLink.getTextualInstance("", edit);

  PageSeed explore = pageState.pageInThisFolder("explorer.jsp", request);

  if (!document.hasPermissionFor(logged, TeamworkPermissions.fileStorage_canRead))
    return;

  edit.mainObjectId = document.getId();
  editLink.enabled = document.hasPermissionFor(logged, TeamworkPermissions.fileStorage_canRead);
  editLink.iconChar = "e";
  editLink.label = "";
  edit.setCommand(Commands.EDIT);

%>
<tr class="alternate" fsid="<%=document.getId()%>">
  <td align="center"><%editLink.toHtml(pageContext);%></td>
  <td><%
    RemoteFile rfs = RemoteFile.getInstance(document);
    boolean exist = rfs.exists();
    if (exist) {
      explore.mainObjectId = document.getId();
      ButtonSupport name = ButtonLink.getBlackInstance(document.getName(), explore);
      name.target = "winPopFS";
      name.toHtmlInTextOnlyModality(pageContext);
    } else {
      %><%=document.getName()%>&nbsp;<%=I18n.get("FILE_DOES_NOT_EXIST")%><%
    }
  %></td>
  <td><%=JSP.w(document.getCode())%></td>
  <td align="center"><%
    if (document.hasPermissionFor(logged, TeamworkPermissions.fileStorage_canCreate)) {

      ButtonJS delLink = new ButtonJS("delRow($(this))");
      delLink.iconChar = "d";
      delLink.label = "";
      delLink.additionalCssClass = "delete";
      delLink.toolTip = I18n.get("DELETE") + " id: " + document.getId() + " - " + I18n.get("NAME") + ": " + document.getName();
      delLink.toHtmlInTextOnlyModality(pageContext);
    } else {
      %>&nbsp;<%
    }
  %>
  </td>
</tr>