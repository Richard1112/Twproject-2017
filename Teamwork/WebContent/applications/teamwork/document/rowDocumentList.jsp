<%@ page import="com.twproject.document.TeamworkDocument, org.jblooming.utilities.JSP, org.jblooming.waf.html.button.AHref, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, org.jblooming.waf.html.display.Img, org.jblooming.utilities.HttpUtilities, com.twproject.security.TeamworkPermissions, org.jblooming.operator.Operator, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.constants.Commands, org.jblooming.waf.view.PageSeed, org.jblooming.security.Securable, org.jblooming.remoteFile.Document, org.jblooming.remoteFile.RemoteFile" %>
<%
  JspHelper rowDrawer = (JspHelper) JspHelper.getCurrentInstance(request);
  TeamworkDocument document = (TeamworkDocument) rowDrawer.parameters.get("ROW_OBJ");
  PageState pageState = PageState.getCurrentPageState(request);
  Operator logged = pageState.getLoggedOperator();


  boolean docCanRead = document.hasPermissionFor(logged, TeamworkPermissions.document_canRead);
  boolean canCreate = document.hasPermissionFor(logged, TeamworkPermissions.document_canCreate);
  boolean exists = document.existsFile();


  //in case of IS_FILE_STORAGE check even for fileStorage_explorer_canRead in case of "directory" otherwise is a doc_read standard
  if (Document.IS_FILE_STORAGE == document.getType()) {
    RemoteFile rf = document.getRemoteFile();
    if (exists && rf != null && rf.isDirectory()) {
      docCanRead = docCanRead && document.hasPermissionFor(logged, TeamworkPermissions.fileStorage_explorer_canRead);
    } else if (!exists) {
      docCanRead = docCanRead && document.hasPermissionFor(logged, TeamworkPermissions.fileStorage_explorer_canRead);
    }

  }


  PageSeed editDoc = pageState.pageFromRoot(document.getTask()!=null ? "task/taskDocumentEditor.jsp" : "resource/resourceDocumentEditor.jsp");
  editDoc.mainObjectId = document.getId();
  editDoc.command= Commands.EDIT;
  editDoc.addClientEntry(document.getTask()!=null?"TASK_ID":"RES_ID",document.getReferral().getId());
  ButtonSupport editLink = ButtonLink.getBlackInstance("",700,1000,editDoc);
  editLink.additionalCssClass="textual";
  editLink.iconChar="e";
  editLink.enabled = docCanRead;

  AHref bl = document.bricks.getContentLink(pageState);


%>
<tr class="alternate" docId="<%=document.getId()%>">
  <td style="width: 25px; text-align: center" ><%editLink.toHtml(pageContext);%></td>

  <td style="width: 25px; text-align: center"><div class="docLabelWrapper"><a href="<%=bl.href%>" target="<%=JSP.w(bl.target)%>"><%document.bricks.getMimeImage().toHtml(pageContext);%></a></div></td>
  <td><a href="<%=bl.href%>"><%=document.getDisplayName()%></a><%
    if (!exists){
      %><span class="teamworkIcon alert" style="cursor: default" title="<%=I18n.get("DOCUMENT_DATA_INVALID")%>">!</span><%
    }
  %>
  </td>
  <td><%=JSP.w(document.getTags())%></td>
  <td>
    <%
      if (TeamworkDocument.IS_UPLOAD==document.getType()){
      %><%=I18n.get("VERSION")+" "+I18n.get(document.getVersion())%><%
      if ((document.getLockedBy()!=null)) {
        %><span class="teamworkIcon">o</span> <%=document.getLockedBy().getDisplayName()%><%
      }
    }

  %>
  </td>
  <td>
    <%
      ButtonLink pointToRef = document.bricks.getReferralButton();
      if (pointToRef != null) {
        pointToRef.toHtmlInTextOnlyModality(pageContext);
      }
    %>
  </td>
  <td><%
    if (canCreate) {
      ButtonJS delete = new ButtonJS("delRow($(this));");
      delete.iconChar = "d";
      delete.toolTip = I18n.get("DELETE");
      delete.additionalCssClass = "delete";
      delete.toHtmlInTextOnlyModality(pageContext);
    }
  %>
  </td>

</tr>
