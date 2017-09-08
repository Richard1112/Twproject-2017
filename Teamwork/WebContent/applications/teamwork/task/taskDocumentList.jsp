<%@ page import="com.twproject.document.businessLogic.DocumentController, com.twproject.operator.TeamworkOperator, com.twproject.security.TeamworkPermissions, com.twproject.task.Task,
com.twproject.waf.TeamworkHBFScreen, com.twproject.waf.html.DocumentDrawer, com.twproject.waf.html.TaskHeaderBar, org.jblooming.system.SystemConstants, org.jblooming.utilities.JSP,
org.jblooming.waf.ScreenArea, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.Uploader, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.input.SmartCombo, com.twproject.task.TaskBricks, org.jblooming.waf.html.input.TagBox, com.twproject.task.Issue" %><%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new DocumentController(), request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    //this is set by action
    Task task = (Task) pageState.attributes.get("REFERRAL_OBJECT");


    // stop if no permission
    task.testPermission(logged, TeamworkPermissions.task_canRead);
    task.testPermission(logged, TeamworkPermissions.document_canRead);

    PageSeed ps = pageState.thisPage(request);
    ps.addClientEntry("TASK_ID", task.getId());
    ps.command = "LIST_DOCS";

    Form form = new Form(ps);
    //form.usePost=false;
    form.start(pageContext);
    pageState.setForm(form);


    new DeletePreviewer("DOC_DEL",DocumentController.class, pageState);

%>

<%---------------------------------------------- MAIN COLUMN START ---------------------------------------------------------%>

<div class="mainColumn">

  <%
    //-----------------HEAD BAR START
    pageState.addClientEntry("TASK_TABSET","TASK_DOCS_TAB");
    TaskHeaderBar head = new TaskHeaderBar(task);
    head.pathToObject.destination=pageState.pageInThisFolder("taskDocumentList.jsp",request);
    head.pathToObject.destination.command="LIST_DOCS";
    head.pathToObject.alternativeCEForMainObject="TASK_ID";
    head.toHtml(pageContext);
    //-----------------HEAD BAR END

  %><div class="listPagedisplayOptions"><%
      ButtonSubmit sc1 = new ButtonSubmit(pageState.getForm());

      pageState.getEntryOrDefault("SHOW_VERSIONS");
      CheckField showVer = new CheckField("SHOW_VERSIONS", "&nbsp;", false);
      showVer.preserveOldValue = false;
      showVer.additionalOnclickScript = sc1.generateJs().toString();
      if (task.getChildrenSize()>0 || task.getDocuments().size()>0) {
        showVer.toHtmlI18n(pageContext);
      }
    %>&nbsp;&nbsp;<%
    ButtonSubmit sc = new ButtonSubmit(pageState.getForm());

  boolean showChildren = pageState.getEntryOrDefault("DOC_SHOW_CHILDREN").checkFieldValue();
  CheckField showCh = new CheckField("DOC_SHOW_CHILDREN", "&nbsp;", false);
    showCh.preserveOldValue = false;
    if (task.getChildrenSize()>0) {
      showCh.additionalOnclickScript = sc.generateJs().toString();
      showCh.toHtmlI18n(pageContext);
    }

%>&nbsp;&nbsp;<%
  pageState.getEntryOrDefault("DOC_SHOW_EMPTY_CHILD");
  CheckField showEmpty = new CheckField("DOC_SHOW_EMPTY_CHILD", "&nbsp;", false);
  showEmpty.preserveOldValue = false;
  if (task.getChildrenSize()>0 && showChildren) {
    showEmpty.additionalOnclickScript = sc.generateJs().toString();
    showEmpty.toHtmlI18n(pageContext);
  }

  %></div><%

    if (task.hasPermissionFor(logged,TeamworkPermissions.document_canCreate)){
      // document drop area
  %>
   <div id="trDropLine"><div id="docDropArea" style="display: none" class="uploadizeDrop"></div><div id="pendingUpload"></div></div>
    <script>
      $(function(){
        $("#docDropArea").uploadize({
          url:"taskAjaxController.jsp",
          fileAreaSelector:"#pendingUpload",
          maxSize:<%=Uploader.getMaxUploadSizeInByte()%>,
          additionalRequestParameters:{CM:"DROPDOC",OBJID:<%=task.getId()%>},
          onLoadCallback:function(response){
            if (response.ok){
              $.get("../document/partDocumentDrawerAjax.jsp",{CM:"DRAWDOCLINE",docId:response.docId},function(docLine){
                self.location.reload();
              });
            }
          }
        });
      });
    </script>
    <%
      }

      CheckField cf = new CheckField("","chall","",false);
      cf.toolTip=I18n.get("SELECT_DESELECT_ALL");
      cf.script=" onclick=\"selUnselAll($(this));\"";

    %>
    <table id="docsTable" class="table dataTable fixHead fixFoot" cellpadding="0" cellspacing="2" border="0">

      <thead>
      <tr>
        <th nowrap class="tableHead" width="10"><%cf.toHtml(pageContext);%></th>
        <th nowrap class="tableHead" width="30"><span class="tableHeadEl"><%=I18n.get("TYPE")%></span></th>
        <th nowrap class="tableHead"><span class="tableHeadEl"><%=I18n.get("NAME")%></span></th>
        <th nowrap class="tableHead"><span class="tableHeadEl">&nbsp;</span></th>
        <th nowrap class="tableHead"><span class="tableHeadEl"><%=I18n.get("LAST_MODIFIED")%></span></th>
        <th nowrap class="tableHead"><span class="tableHeadEl"><%=I18n.get("TAGS")%></span></th>
        <th class="tableHead" width="30">&nbsp;</th>

      </tr>
      </thead>
      <%

        DocumentDrawer docDrawer = new DocumentDrawer(task);
        docDrawer.drawOnlyRoots = !pageState.getEntry("SHOW_VERSIONS").checkFieldValue();
        docDrawer.sorted = false;
        docDrawer.drawDocumentable(pageContext);


      %>
      <tfoot>
      <tr><td id="bulkPlace" colspan="99"></td></tr>
      </tfoot>

    </table> <%

    if (task.getChildrenSize()>0) {
      pageState.setFocusedObjectDomId("ck_"+showCh.fieldName);
    }

    String ident=task.getMnemonicCode();

    String twEmail = ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_FROM);
    //verify if POP3 is configured
    boolean pop3isConf = JSP.ex(ApplicationState.getApplicationSetting(SystemConstants.FLD_POP3_HOST));
    String sendMessage = "You can add documents to this task by sending an e-mail<br> to:" + twEmail + "subject TASK #"+ident+"# ATTACHMENT";
    if (!pop3isConf )
      sendMessage = "You could add documents to this task by sending an e-mail<br> to:" + twEmail + "subject TASK #" + ident + "# ATTACHMENT  if a Twproject e-mail account was configured.<br>"+
          "Ask your Twproject administrator to set it up by going to<br> tools -> administration -> global settings and setting up POP3 data.";

  %><div class="boxInfo"><span class="teamworkIcon info">|</span> <span id="twmailhelp" class="descrEl"><%
    if (pop3isConf) {
  %> You can add documents to this task by sending an e-mail to <a href="mailto:<%=twEmail%>?subject=<%=JSP.urlEncode("TASK#"+ident+"#attachment")%>"><%=twEmail%></a>
    with attachments and as subject: TASK #<%=ident%># ATTACHMENT<%
    } else {
    %><%=sendMessage%><%
      }
    %></span></div>
</div>
<%//---------------------------------------------- MAIN COLUMN END ---------------------------------------------------------

//---------------------------------------------- RIGHT COLUMN START ---------------------------------------------------------

  JspHelper side = new JspHelper("part/partTaskSideBar.jsp");
  side.parameters.put("TASK", task);
  side.toHtml(pageContext);
//---------------------------------------------- RIGHT COLUMN END ---------------------------------------------------------

  form.end(pageContext);


  JspHelper bulkOp = new JspHelper("/applications/teamwork/document/partDocumentBulkOperations.jsp");

  bulkOp.toHtml(pageContext);
%>
<script>
  $(function(){
    var docId=<%=pageState.getEntry("DOC_ID").intValueNoErrorCodeNoExc()%>;
    var desDataId=<%=pageState.getEntry("DESDATA_ID").intValueNoErrorCodeNoExc()%>;
    if(docId) {
      $("#docsTable tr[docId="+docId+"] .button.edit").click();
    }else if (desDataId){
      $("#docsTable tr[desDataId="+desDataId+"] .button.edit").click();
    }

  })

function closeFormEditorCallback(){
  location.href=location.pathname+"?CM=LIST_DOCS&TASK_ID=<%=task.getId()%>";
}
</script>
<%
  }
%>


