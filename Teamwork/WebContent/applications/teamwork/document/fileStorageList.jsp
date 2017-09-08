<%@ page import=" com.twproject.document.businessLogic.FileStorageController, com.twproject.operator.TeamworkOperator, com.twproject.security.TeamworkPermissions, com.twproject.waf.TeamworkHBFScreen, org.jblooming.remoteFile.Document, org.jblooming.system.SystemConstants, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.SessionState, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.container.HeadBar, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.DataTable, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jblooming.waf.html.display.DeletePreviewer" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;

    final ScreenArea body = new ScreenArea(new FileStorageController(), request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);
  } else {

    new DeletePreviewer("FS_DEL",FileStorageController.class, pageState);



%><%---------------------------------------------- MAIN COLUMN START ---------------------------------------------------------%>


<div class="mainColumn">

  <script>$("#TOOLS_MENU").addClass('selected');</script><%

  PageSeed self = pageState.thisPage(request);
  self.setCommand(Commands.FIND);
  Form f = new Form(self);
  pageState.setForm(f);
  f.start(pageContext);

  PageSeed edit = pageState.pageInThisFolder("fileStorageEditor.jsp", request);
  edit.setCommand(Commands.EDIT);
  DataTable dataTable = new DataTable("DOCFS", f, new JspHelper("/applications/teamwork/document/rowFileStorageList.jsp"), FileStorageController.class, pageState);
  dataTable.addHeaderFitAndCentered(I18n.get("EDIT_SHORT"));
  dataTable.addHeader(I18n.get("NAME"), "document.name");
  dataTable.addHeader(I18n.get("CODE"), "document.code");
  dataTable.addHeaderFitAndCentered(I18n.get("DELETE_SHORT"));


%><h1><%=I18n.get("FILE_STORAGE_LIST")%></h1><%

  dataTable.drawPaginator(pageContext);
  dataTable.drawTable(pageContext);

  String spa = ApplicationState.getApplicationSetting(SystemConstants.STORAGE_PATH_ALLOWED);
  if (spa == null || spa.trim().length() == 0) {
    %><div><big><span class="warning">STORAGE_PATH_ALLOWED value not found or not set: set it in tools -> administration -> global settings</span></big></div><%
  }
%><div class="boxInfo"><span class="teamworkIcon info">|</span> <span class="descrEl"><%=I18n.get("HELP_FILE_STORAGES")%>&nbsp;<%
  ButtonLink.getDescriptiveLinkInstance(I18n.get("HELP_MORE"), "http://twproject.com/support/documents-faq/").toHtml(pageContext);
%></span></div>

<%---------------------------------------------- MAIN COLUMN END ---------------------------------------------------------%>
 </div>
<%---------------------------------------------- RIGHT COLUMN START ---------------------------------------------------------%>

<div class="rightColumn noprint"><div class="tools">
  <%
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    PageSeed add = new PageSeed("fileStorageEditor.jsp");
    add.setCommand(Commands.ADD);
    add.addClientEntry("DOCUMENT_TYPE", Document.IS_FILE_STORAGE);
    ButtonLink addBl = new ButtonLink(add);
    addBl.additionalCssClass = "first";
    addBl.label = I18n.get("CREATE_FILE_STORAGE");
    addBl.iconChar = "P";
    addBl.enabled = logged.hasPermissionFor(TeamworkPermissions.fileStorage_canCreate);
    addBl.toHtml(pageContext);
  %>

</div></div>

<script>

  function delRow(el) {
    var issueRow = $(el).closest("[fsid]");
    var issueId = issueRow.attr("fsid");
    deletePreview("FS_DEL", issueId, function(response) {  // callback function
      if (response && response.ok){
        issueRow.fadeOut(500, function () {$(this).remove();});
      }
    });
  }
</script>



<%

    f.end(pageContext);


  }
%>