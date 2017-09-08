<%@ page import="com.twproject.task.Issue, com.twproject.task.IssueHistory, com.twproject.worklog.Worklog, org.jblooming.ontology.PersistentFile, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.utilities.file.FileUtilities, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.display.Img, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.Set, java.util.List, com.twproject.task.IssueBricks, org.jblooming.designer.DesignerField, org.jblooming.utilities.ReflectionUtilities"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);
   Issue issue = (Issue) pageState.getMainObject();
%>
<table class="trBody edged" style="page-break-after: always;margin-top: 20px" border="0" width="99%" align="center" cellpadding="5" cellspacing="0">
    <tr>
        <td colspan="4" style="padding: 0 0 15px 0;border-top:3px solid #959595; "><h2 style="background-color: #959595;color:#fff;display: inline; padding: 0px 10px;">I#<%=issue.getMnemonicCode()%>#</h2>
          <h4><%=JSP.encode(issue.getDescription())%></h4>
        </td>
    </tr>
    <tr>
      <td ><%=I18n.get("ISSUE_TYPE")%>: <b><%=issue.getType() != null ? JSP.w(issue.getType().getDescription()) : "-"%></b></td>
      <td ><%=I18n.get("ISSUE_GRAVITY")%>: <b><%=JSP.w(I18n.get(issue.getGravity()))%></b></td>
      <td ><%=I18n.get("ISSUE_IMPACT")%>: <b><%=issue.getImpact() != null ? JSP.w(issue.getImpact().getDescription()) : "-"%></b></td>
    </tr>

    <tr>
      <td ><%=I18n.get("ASSIGNEE")%>: <b><%=JSP.w(issue.getAssignedTo())%></b>&nbsp;</td>
      <td ><%=I18n.get("ASSIGNED_BY")%>: <b><%=JSP.w(issue.getAssignedBy())%>&nbsp;</b></td>
      <td ><%=I18n.get("ISSUE_DATE_CLOSE_BY")%>: <b><%=JSP.w(issue.getShouldCloseBy())%>&nbsp;</b></td>
    </tr>

    <tr>
      <td ><%=I18n.get("ISSUE_STATUS")%>: <b><%=JSP.w(issue.getStatus().getDescription())%></b> (<%=JSP.w(issue.getLastStatusChangeDate())%>)</td>
      <td  colspan="2">&nbsp;</td>
    </tr>

  <%//---------------------------------------------------------- CUSTOM FIELDS ----------------------------------------------------------
  if (IssueBricks.hasCustomField()) {
  %>
  <tr><td colspan="3" valign="middle"><%

    for (int i=1; i<7; i++) {
      if (!JSP.ex((String) ReflectionUtilities.getFieldValue("customField" + i, issue)))
        continue;

      DesignerField dfStr = DesignerField.getCustomFieldInstance( "ISSUE_CUSTOM_FIELD_",i, issue,true, true, false, pageState);
      if (dfStr!=null){
        dfStr.separator=": ";
        %><div style="float:left;margin-right: 10px"><%dfStr.toHtml(pageContext);%></div><%
      }
    }

  %></td></tr><%
  }%>

  <tr>
        <td  colspan="4" style="height: 20px">
            <%
              for (PersistentFile pf:issue.getFiles()){
                if (FileUtilities.isImageByFileExt(FileUtilities.getFileExt(pf.getOriginalFileName()))) {
                  Img img = new Img(pf, I18n.get("ISSUE_FILE"));
                  img.toHtml(pageContext);
                } else {
                  %><%=I18n.get("NO_PREVIEW_AVAILABLE")%>:<%=pf.getOriginalFileName()%><%
                }
              }
            %>
        </td></tr>
  <%

  List<IssueHistory> hists = issue.getIssueHistories();

  if (JSP.ex(hists)){
    %><tr><td colspan="4"><b><%=I18n.get("ISSUE_HISTORY_TAB")%></b></td></tr><%

    for (IssueHistory his:hists){
      %><tr>
        <td valign="top" nowrap><%=JSP.w(his.getLastModifier())%><br><span style="font-size: 70%;"><%=DateUtilities.dateAndHourToString(his.getLastModified())%></span></td>
        <td valign="top" colspan="3">
          <%=his.getStatus()==null?"":I18n.get("ISSUE_STATUS")+": "+his.getStatus().getDescription()+"<br>"%>
          <%=his.getAssignee()==null?"":I18n.get("ISSUE_ASSIGNED_TO")+": "+his.getAssignee().getDisplayName()+"<br>"%>
          <%=his.getTask()==null?"":I18n.get("ISSUE_TASK")+": "+his.getTask().getDisplayName()+"<br>"%>
          <%=JSP.ex(his.getComment())?"<br>"+JSP.convertLineFeedToBR(his.getComment()):""%>
        </td>
      </tr><%
    }
  }



  Set<Worklog> worklogs = issue.getWorklogs();
    %><tr><td  colspan="4"><b><%=I18n.get("ISSUE_WORKLOGS")%></b> <%=JSP.ex(worklogs)?"":": "+I18n.get("NO_WORKLOGS")%></td></tr><%
    long totDone = 0;
    if (JSP.ex(worklogs)) {
      for(Worklog workLog: worklogs) {
          totDone = totDone + workLog.getDuration();
          %><tr class="alternate">
               <td><%=DateUtilities.dateToString(workLog.getInserted())%> - <%=workLog.getAssig().getResource().getDisplayName()%></td>
               <td><%=workLog.getAction()%></td>
               <td align="right"><%=DateUtilities.getMillisInHoursMinutes(workLog.getDuration())%></td>
          </tr><%
      }
      %><tr><td align="right" colspan="4" ><%=I18n.get("TOTAL")%>:<%=DateUtilities.getMillisInHoursMinutes(totDone)%></td></tr><%
    }


%> <tr><td colspan="4" ><%
  ButtonBar bb = new ButtonBar();
  bb.loggableIdentifiableSupport = issue;
  bb.toHtml(pageContext);
%></td></tr>
</table>
