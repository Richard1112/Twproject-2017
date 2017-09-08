<%@ page import=" com.twproject.document.TeamworkDocument, com.twproject.document.businessLogic.DocumentController, com.twproject.operator.TeamworkOperator, com.twproject.resource.Resource, com.twproject.resource.ResourceBricks,
                  com.twproject.security.TeamworkPermissions, com.twproject.task.TaskBricks, com.twproject.waf.TeamworkHBFScreen, org.jblooming.page.Page, org.jblooming.utilities.CodeValueList,
                  org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonLink,
                  org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.DataTable, org.jblooming.waf.html.input.*, org.jblooming.waf.html.state.Form,
                   org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.Map, org.jblooming.waf.html.display.DeletePreviewer"%><%

  
PageState pageState = PageState.getCurrentPageState(request);
 if (!pageState.screenRunning) {
  pageState.screenRunning = true;
  final ScreenArea body = new ScreenArea(new DocumentController(), request);
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

  DataTable dataTable=new DataTable("DOCLST",f,new JspHelper("/applications/teamwork/document/rowDocumentList.jsp"),DocumentController.class,pageState);
   dataTable.addHeader("");
   dataTable.addHeader(I18n.get("TYPE"),"type");
   dataTable.addHeader(I18n.get("NAME"),"name");
   dataTable.addHeader(I18n.get("TAGS"));
  dataTable.addHeader("");
  dataTable.addHeader(I18n.get("REFERRAL"));
  dataTable.addHeader("");

//non funziona il salvataggio del filtro


  LoadSaveFilter lsf = new LoadSaveFilter(TeamworkDocument.DOCUMENT, f);
  lsf.label=I18n.get("WANT_TO_SAVE_FILTER");


  TextField tfCode = new TextField("TEXT", I18n.get("ID_CODE"), "ID_CODE", "<br>", 8, false);
  TextField tfName = new TextField("TEXT", I18n.get("NAME_DESCRIPTION"), "NAME_DESCRIPTION", "<br>", 40, false);

  CodeValueList cvl = new CodeValueList();
  Map<Integer, String> sts = TeamworkDocument.getTypeMap();
  for (java.lang.Integer integer : sts.keySet()) {
    cvl.add(integer+"",I18n.get(sts.get(integer)));
  }
  cvl.addChoose(pageState);
  Combo docType = new Combo("TYPE", "<br>", "", 255, cvl, null);
  docType.label = I18n.get(docType.fieldName);

  DateField authored = new DateField("DOCUMENT_AUTHORED");
  authored.labelstr=I18n.get("DOCUMENT_AUTHORED");
  authored.separator="<br>";
  authored.setSearchField(true);

  DateField lastMod = new DateField("DOCUMENT_LAST_MODIFIED");
  lastMod.labelstr = I18n.get("LAST_MODIFIED");
  lastMod.separator="<br>";
  lastMod.setSearchField(true);

  TagBox tags= new TagBox("DOCUMENT_TAGS",TeamworkDocument.class,logged.getPerson().getArea());
  tags.label=I18n.get("TAGS");
  tags.separator= "<br>";
  tags.fieldSize=30;
  tags.preserveOldValue=false;

  SmartCombo tsk = TaskBricks.getTaskCombo("task", false, TeamworkPermissions.task_canRead,pageState);
  tsk.separator = "<br>";
  tsk.label=I18n.get("TASK");
  tsk.fieldSize = 20;

  SmartCombo res = ResourceBricks.getResourceCombo("resource", TeamworkPermissions.resource_canRead, null, Resource.class, pageState);
  res.separator = "<br>";
  res.label=I18n.get("RESOURCE");
  res.fieldSize = 20;

  TextField author = new TextField("TEXT", I18n.get("DOCUMENT_AUTHOR"), "DOCUMENT_AUTHOR", "<br>", 20, false);

  new DeletePreviewer("DOC_DEL",DocumentController.class, pageState);

  String savedFilterName = pageState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty();
%>


<h1 class="filterTitle" defaultTitle="<%=I18n.get("DOCUMENT_LIST")%>">
  <%=JSP.ex(savedFilterName)?I18n.get(savedFilterName):I18n.get("DOCUMENT_LIST")%>
</h1>


<div class="filterBar clearfix">
  <div class="filterActiveElements"></div>

  <div class="filterInactiveElements">
    <div class="filterElement filterDefault"><%tfName.toHtml(pageContext);%></div>
    <div class="filterElement"><%tfCode.toHtml(pageContext);%></div>
    <div class="filterElement"><%docType.toHtml(pageContext);%></div>
    <div class="filterElement"><%authored.toHtml(pageContext);%></div>
    <div class="filterElement"><%lastMod.toHtml(pageContext);%></div>
    <div class="filterElement"><%tags.toHtml(pageContext);%></div>
    <div class="filterElement"><%tsk.toHtml(pageContext);%></div>
    <div class="filterElement"><%res.toHtml(pageContext);%></div>
    <div class="filterElement"><%author.toHtml(pageContext);%></div>
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
    var docRow = $(el).closest("[docId]");
    var docId = docRow.attr("docId");
    deletePreview("DOC_DEL", docId, function (response) {  // callback function
      if (response && response.ok) {
        docRow.fadeOut(500, function () {$(this).remove();});
      }
    });

  }


</script>



<%}%>
