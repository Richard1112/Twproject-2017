<%@page import="com.sun.imageio.plugins.common.I18N"%>
<%@page import="com.teamwork.expand.TaskReportBricks"%>
<%@page import="com.teamwork.expand.TaskServiceBricks"%>
<%@page import="com.teamwork.expand.ReportController"%>
<%@ page import=" com.twproject.document.TeamworkDocument, com.twproject.document.businessLogic.DocumentController, com.twproject.operator.TeamworkOperator, com.twproject.resource.Resource, com.twproject.resource.ResourceBricks,
                  com.twproject.security.TeamworkPermissions, com.twproject.task.TaskBricks, com.twproject.waf.TeamworkHBFScreen, org.jblooming.page.Page, org.jblooming.utilities.CodeValueList,
                  org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonLink,
                  org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.DataTable, org.jblooming.waf.html.input.*, org.jblooming.waf.html.state.Form,
                   org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.Map, org.jblooming.waf.html.display.DeletePreviewer"%><%

  
PageState pageState = PageState.getCurrentPageState(request);
 if (!pageState.screenRunning) {
  pageState.screenRunning = true;
  final ScreenArea body = new ScreenArea(new ReportController(), request);
  TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
  lw.register(pageState);
  pageState.perform(request, response).toHtml(pageContext);
} else {
   TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
   

  PageSeed self = pageState.thisPage(request);
  self.setCommand(Commands.FIND);
  Form f = new Form(self);
  f.alertOnChange=false;

  pageState.setForm(f);
  f.start(pageContext);

  DataTable dataTable=new DataTable("REPORTLST",f,new JspHelper("/applications/teamwork/report/rowReportList.jsp"),ReportController.class,pageState);
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
  dataTable.addHeader(I18n.get("REFERRAL"));
  dataTable.addHeader("");

//non funziona il salvataggio del filtro

  LoadSaveFilter lsf = new LoadSaveFilter("REPORTFILTER", f);
  lsf.label=I18n.get("WANT_TO_SAVE_FILTER");

  SmartCombo reportTypeCombo = TaskReportBricks.getReportTypeCombo("REPORT_TYPE_ID", pageState);
  reportTypeCombo.fieldSize = 18;
  reportTypeCombo.linkToEntity=null;
  reportTypeCombo.separator="";
  reportTypeCombo.required=false;
  reportTypeCombo.label=I18n.get("REPORT_TYPE");
  SmartCombo supCombo = TaskServiceBricks.getDepartCombo("REPORT_DEPART_ID", pageState, "DEPARTMENT");
  supCombo.fieldSize = 18;
  supCombo.linkToEntity=null;
  supCombo.separator="";
  supCombo.required=false;
  supCombo.label=I18n.get("Department");
 // supCombo.toHtml(pageContext);
 // String taskId = pageState.getEntry("TASK_ID").stringValueNullIfEmpty();
  
  SmartCombo yearCombo = TaskReportBricks.getReportYearCombo("REPORT_YEAR", pageState);
  yearCombo.fieldSize = 18;
  yearCombo.linkToEntity=null;
  yearCombo.separator="";
  yearCombo.required=false;
  yearCombo.label=I18n.get("REPORT_YEAR");
  SmartCombo dicCombo = TaskReportBricks.getReportDictionNameCombo("REPORT_DIC_NAME", pageState);
  dicCombo.fieldSize = 18;
  dicCombo.linkToEntity=null;
  dicCombo.separator="";
  dicCombo.required=false;
  dicCombo.label=I18n.get("REPORT_DIC_NAME");
  
  DateField startDate = new DateField("UPLOAD_START_DATE");
  startDate.labelstr=I18n.get("REPORT_UPLOAD_DATE");
  startDate.separator="";
  startDate.required=false;
  startDate.dateFormat="yyyy-MM-dd";
  
  DateField endDate = new DateField("UPLOAD_END_DATE");
  endDate.labelstr="";
  endDate.separator="";
  endDate.required=false;

  TextField author = new TextField("TEXT", I18n.get("REPORT_CREATOR"), "REPORT_CREATOR", "<br>", 20, false);

  new DeletePreviewer("REPORT_DEL",ReportController.class, pageState);

  SmartCombo tsk = TaskBricks.getTaskCombo("TASK_ID", false, TeamworkPermissions.task_canRead,pageState);
  tsk.separator = "<br>";
  tsk.label=I18n.get("TASK");
  tsk.fieldSize = 20;
  
  
  String savedFilterName = pageState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty();
%>


<h1 class="filterTitle" defaultTitle="<%=I18n.get("DOCUMENT_LIST")%>">
  <%=JSP.ex(savedFilterName)?I18n.get(savedFilterName):I18n.get("DOCUMENT_LIST")%>
</h1>


<div class="filterBar clearfix">
  <div class="filterActiveElements"></div>

  <div class="filterInactiveElements">
  <div class="filterElement filterDefault"><%reportTypeCombo.toHtml(pageContext);%></div>
  <div class="filterElement filterDefault"><%supCombo.toHtml(pageContext);%></div>
    <div class="filterElement filterDefault"><%tsk.toHtml(pageContext);%></div>
    <div class="filterElement"><%yearCombo.toHtml(pageContext);%></div>
    <div class="filterElement"><%dicCombo.toHtml(pageContext);%></div>
    <div class="filterElement"><%startDate.toHtml(pageContext);%></div>
<%--     <div class="filterElement"><%author.toHtml(pageContext);%></div> --%>
  </div>
  <div class="filterButtons">
    <div class="filterButtonsElement filterAdd"><span class="button" id="filterSelectorOpener" title="<%=I18n.get("ADD_FILTER")%>" onclick="bjs_showMenuDiv('filterSelectorBox', 'filterSelectorOpener');"><span class="teamworkIcon">f</span></span></div>
    <div class="filterButtonsElement filterSearch"><%dataTable.getSearchButton().toHtml(pageContext);%></div>

    <div class="filterActions">
      <div class="filterButtonsElement filterHelp"><%DataTable.getQBEHelpButton(pageState).toHtmlInTextOnlyModality(pageContext);%></div>
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
     %> <table class="table"> <tr><td align="center" valign="top"><span class="descrEl"><%=JSP.wHelp(I18n.get("HELP_DOCUMENTS"))%><%
      ButtonLink.getDescriptiveLinkInstance(I18n.get("HELP_MORE"),"http://twproject.com/support/documents-faq/").toHtml(pageContext);
      %></span></td> </tr></table><%
  }
  dataTable.drawPaginatorPagesOnly(pageContext);
  f.end(pageContext);

    %>

<script>
  $("#TOOLS_MENU").addClass('selected');

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



<%}%>
