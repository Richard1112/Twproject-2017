<%@page import="com.teamwork.expand.TaskReport"%>
<%@ page import="com.twproject.document.TeamworkDocument, org.jblooming.utilities.JSP, org.jblooming.waf.html.button.AHref, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, org.jblooming.waf.html.display.Img, org.jblooming.utilities.HttpUtilities, com.twproject.security.TeamworkPermissions, org.jblooming.operator.Operator, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.constants.Commands, org.jblooming.waf.view.PageSeed, org.jblooming.security.Securable, org.jblooming.remoteFile.Document, org.jblooming.remoteFile.RemoteFile" %>
<%
  JspHelper rowDrawer = (JspHelper) JspHelper.getCurrentInstance(request);
  TaskReport document = (TaskReport) rowDrawer.parameters.get("ROW_OBJ");
  PageState pageState = PageState.getCurrentPageState(request);
  Operator logged = pageState.getLoggedOperator();


  boolean docCanRead = document.getCreator().equalsIgnoreCase(logged.getDisplayName());
  boolean canEdite = document.getCreator().equalsIgnoreCase(logged.getDisplayName());
  boolean exists = document.existsFile();


  //in case of IS_FILE_STORAGE check even for fileStorage_explorer_canRead in case of "directory" otherwise is a doc_read standard
//   if (Document.IS_FILE_STORAGE == document.getType()) {
//     RemoteFile rf = document.getRemoteFile();
//     if (exists && rf != null && rf.isDirectory()) {
//       docCanRead = docCanRead && document.hasPermissionFor(logged, TeamworkPermissions.fileStorage_explorer_canRead);
//     } else if (!exists) {
//       docCanRead = docCanRead && document.hasPermissionFor(logged, TeamworkPermissions.fileStorage_explorer_canRead);
//     }

//   }


  PageSeed editDoc = pageState.pageFromRoot( "task/taskReportEditor.jsp");
  editDoc.mainObjectId = document.getId();
  editDoc.command= Commands.EDIT;
  editDoc.addClientEntry("TASK_ID",document.getReferral().getId());
  ButtonSupport editLink = ButtonLink.getBlackInstance("",700,1000,editDoc);
  editLink.additionalCssClass="textual";
  editLink.iconChar="e";
  editLink.enabled = docCanRead;

  AHref bl = document.bricks.getContentLink(pageState);
  
  pageState.addClientEntry("CURRENT_FILE_DIR", "");
  pageState.addClientEntry("RELATIVE_FILE_DIR", document.getTask().getCode()+"/"+document.getTaskReportType().getStringValue());
  AHref bl2 = document.bricks.getExportLink(pageState,"RELATIVE_FILE_DIR");

%>
<tr class="alternate" reportId="<%=document.getId()%>">
  <td style="width: 25px; text-align: center" ><% if(canEdite)
  editLink.toHtml(pageContext);
  
  %></td>

  <td style="text-align: center"><a href="<%=bl2.href%>"><%=document.getTaskReportType().getStringValue()%></a> </td>
  <td><%=document.getYear()%> 
  </td>
  <td><%=document.getDictionaryName()%></td>
   <td><%=document.getCreator()%></td>
 <td><%=I18n.get("VERSION")+" "+I18n.get(document.getVersion())%></td>
  <td><%=document.getCompany()!=null?document.getCompany().getName():"" %> </td>
  
  <td>
   <%=JSP.w(document.getContent())%>
  </td>
   <td>
   <%=JSP.w(document.getCreationDate())%>
  </td>
   <td>
   <% 
      String documentStr="";
      if(document.getPersistentFile()!=null){
    	 // document.getPersistentFile().serialize();
    	  documentStr= document.getPersistentFile().getOriginalFileName();
      }
   %>
   <a href="<%=bl.href%>"><%=documentStr%> </a>
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
    if (canEdite) {
      ButtonJS delete = new ButtonJS("delRow($(this));");
      delete.iconChar = "d";
      delete.toolTip = I18n.get("DELETE");
      delete.additionalCssClass = "delete";
      delete.toHtmlInTextOnlyModality(pageContext);
    }
  %>
  </td>

</tr>
