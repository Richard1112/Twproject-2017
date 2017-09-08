<%@page import="org.jblooming.waf.html.button.ButtonLink"%>
<%@page import="org.jblooming.page.Page"%>
<%@page import="com.teamwork.expand.ReportController"%>
<%@ page import="org.jblooming.waf.html.display.DataTable, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.settings.ApplicationState, com.twproject.task.businessLogic.IssueController, org.jblooming.waf.settings.I18n, org.jblooming.waf.constants.Fields, org.jblooming.waf.view.PageState, com.twproject.task.Issue, com.twproject.task.IssueBricks, org.jblooming.utilities.JSP, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonSubmit, com.twproject.security.TeamworkPermissions, com.twproject.task.TaskBricks, com.twproject.resource.ResourceBricks, org.jblooming.waf.html.input.*, org.jblooming.waf.html.core.JST, org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.constants.Commands, com.twproject.task.IssueStatus, com.twproject.operator.TeamworkOperator" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
  String taskId = pageState.getEntry("TASK_ID").stringValueNullIfEmpty();

%>


<%-- ------------------------- UPLOADIZE -------------------------------------------- --%>
<%
  DataTable dataTable = new DataTable("REPORTFILTER", pageState.getForm(), new JspHelper("/applications/teamwork/task/report/rowReportList.jsp"), ReportController.class, pageState);
  dataTable.drawPageFooter=false;

  dataTable.addHeader("");
  dataTable.addHeader(I18n.get("REPORT_REPORT_TYPE"), "");
  dataTable.addHeader(I18n.get("REPORT_YEAR"), "");
  dataTable.addHeader(I18n.get("REPORT_DICTIONARY_NAME"), "");
  dataTable.addHeader(I18n.get("REPORT_CREATOR"), "");
  dataTable.addHeader(I18n.get("REPORT_VERSION"), "");
  dataTable.addHeader(I18n.get("REPORT_REPORT_DEPARTMENT"),null,null,null,"");
  dataTable.addHeader(I18n.get("REPORT_REPORT_CONTENT"), "");
  dataTable.addHeader(I18n.get("REPORT_UPLOADER_DATE"), "");
  dataTable.addHeader(I18n.get("REPORT_REPORT_File"), "");
  dataTable.addHeader("");
  pageState.getForm().start(pageContext);
  pageState.addClientEntry("TASK_ID", taskId);
  new DeletePreviewer("REPORT_DEL",ReportController.class, pageState);

  boolean saveFilterHidden=request.getRequestURI().indexOf("taskReportList")>-1;
%>

<%----------------------------------------------------------------------------  START FILTER ----------------------------------------------------------------------------%>
<div class="filterBar withButtons clearfix">
  <%
    new JspHelper("/applications/teamwork/task/report/partReportFilter.jsp").toHtml(pageContext);
    LoadSaveFilter lsfb = new LoadSaveFilter("REPORTFILTER", pageState.getForm());
    lsfb.label = I18n.get("WANT_TO_SAVE_FILTER");
    lsfb.drawEditor = true;
    lsfb.drawButtons = false;
    lsfb.id="issueLSF";
  %>

  <div class="filterButtons">
    <div class="filterButtonsElement filterAdd">
      <span class="button" id="filterSelectorOpener" title="<%=I18n.get("ADD_FILTER")%>" onclick="bjs_showMenuDiv('filterSelectorBox', 'filterSelectorOpener');"><span class="teamworkIcon">f</span></span>
    </div>
    <div class="filterButtonsElement filterSearch"><%dataTable.getSearchButton().toHtml(pageContext);%></div>

    <div class="filterActions">
      <div class="filterButtonsElement filterSave" style="<%=saveFilterHidden?"display:none;":""%>"><%lsfb.toHtml(pageContext);%></div>
      <div class="filterButtonsElement filterHelp"><%
        DataTable.getQBEHelpButton(pageState).toHtmlInTextOnlyModality(pageContext);%></div>
    </div>

  </div>
</div>
<script src="<%=request.getContextPath()%>/commons/js/filterEngine.js"></script>
<div style="position: relative">
  <%dataTable.drawPaginator(pageContext);%>
</div>
<%

  Page documents = pageState.getPage();
  if (documents != null || pageState.getCommand() != null) {

    dataTable.drawTable(pageContext);

  } else {
%>
<table class="table">
  <tr>
    <td align="center" valign="top"><span class="descrEl"><%=JSP.wHelp(I18n.get("HELP_DOCUMENTS"))%><%
      ButtonLink.getDescriptiveLinkInstance(I18n.get("HELP_MORE"), "http://twproject.com/support/documents-faq/").toHtml(pageContext);
    %></span></td>
  </tr>
</table>
<%
  }
  dataTable.drawPaginatorPagesOnly(pageContext);

%>
<%----------------------------------------------------------------------------  END FILTER ----------------------------------------------------------------------------%>

<%-- ------------------------------------------------------------- START CSS-------------------------------------------------------------- --%>
<%

  pageState.getForm().end(pageContext);

%>
<script type="text/javascript">
function delRow(el) {
    var docRow = $(el).closest("[reportId]");
    var docId = docRow.attr("reportId");
    deletePreview("REPORT_DEL", docId, function (response) {  // callback function
      if (response && response.ok) {
        docRow.fadeOut(500, function () {$(this).remove();});
      }
    });

  }

</script>