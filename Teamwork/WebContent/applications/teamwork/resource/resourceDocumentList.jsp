<%@ page import="com.twproject.document.businessLogic.DocumentController, com.twproject.resource.Resource, com.twproject.security.TeamworkPermissions, com.twproject.waf.TeamworkHBFScreen,
com.twproject.waf.html.DocumentDrawer, com.twproject.waf.html.ResourceHeaderBar, org.jblooming.waf.ScreenArea, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.core.JspHelper,
org.jblooming.waf.html.display.Img, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.Uploader, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jblooming.waf.settings.I18n, org.jblooming.waf.html.display.DeletePreviewer" %><%
  PageState pageState = PageState.getCurrentPageState(request);

    if (!pageState.screenRunning) {

      pageState.screenRunning = true;
      final ScreenArea body = new ScreenArea(new DocumentController(), request);
      TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
      lw.register(pageState);
      pageState.perform(request, response);
      pageState.toHtml(pageContext);
    } else {
      //this is set by action
      Resource resource = (Resource) pageState.attributes.get("REFERRAL_OBJECT");

      PageSeed ps = pageState.thisPage(request);
      ps.addClientEntry("RES_ID",resource.getId());
      ps.command= "LIST_DOCS";

      Form form = new Form(ps);
      form.start(pageContext);
      pageState.setForm(form);

      new DeletePreviewer("DOC_DEL",DocumentController.class, pageState);

%>
<div class="mainColumn">
  <%
    //---------------------------------------- HEAD BAR -------------------------------------------
    pageState.addClientEntry("RESOURCE_TABSET","DOCUMENTS_TAB");
    ResourceHeaderBar head = new ResourceHeaderBar(resource);
    head.pathToObject.destination=pageState.pageInThisFolder("resourceDocumentList.jsp",request);
    head.pathToObject.destination.command="LIST_DOCS";
    head.pathToObject.alternativeCEForMainObject="RES_ID";
    head.toHtml(pageContext);

  %>
  <div class="listPagedisplayOptions"><%

  ButtonSubmit sc1 = new ButtonSubmit(pageState.getForm());

  pageState.getEntryOrDefault("SHOW_VERSIONS");
  CheckField showVer = new CheckField("SHOW_VERSIONS", "&nbsp;", false);
  showVer.preserveOldValue = false;
  showVer.additionalOnclickScript = sc1.generateJs().toString();
  showVer.toHtmlI18n(pageContext);

  boolean showChildren = pageState.getEntryOrDefault("DOC_SHOW_CHILDREN").checkFieldValue();
  ButtonSubmit sc = new ButtonSubmit(pageState.getForm());
  CheckField showCh = new CheckField("DOC_SHOW_CHILDREN", "&nbsp;", false);
  showCh.preserveOldValue = false;
  showCh.additionalOnclickScript = sc.generateJs().toString();
  showCh.toHtmlI18n(pageContext);

  %>&nbsp;&nbsp;<%
    pageState.getEntryOrDefault("DOC_SHOW_EMPTY_CHILD");
    CheckField showEmpty = new CheckField("DOC_SHOW_EMPTY_CHILD", "&nbsp;", false);
    showEmpty.preserveOldValue = false;
    if (resource.getChildrenSize()>0 && showChildren) {
      showEmpty.additionalOnclickScript = sc.generateJs().toString();
      showEmpty.toHtmlI18n(pageContext);
    }



  %></div><%

  if (resource.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.document_canCreate)){
    // document drop area
%>
  <div id="trDropLine"><div id="docDropArea" style="display: none" class="uploadizeDrop"></div><div id="pendingUpload"></div></div>
  <script>
    $(function(){
      $("#docDropArea").uploadize({
        url:"resourceAjaxController.jsp",
        fileAreaSelector:"#pendingUpload",
        maxSize:<%=Uploader.getMaxUploadSizeInByte()%>,
        additionalRequestParameters:{CM:"DROPDOC",OBJID:<%=resource.getId()%>},
        onLoadCallback:function(response){
          jsonResponseHandling(response);
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

    %><table id="docsTable" class="table dataTable fixHead fixFoot" cellpadding="0" cellspacing="2" border="0"><thead>
  <tr>

    <th nowrap class="tableHead" width="10"><%cf.toHtml(pageContext);%></th>
    <th nowrap class="tableHead" style="width: 25px"><span class="tableHeadEl"><%=I18n.get("TYPE")%></span></th>
    <th nowrap class="tableHead"><span class="tableHeadEl"><%=I18n.get("NAME")%></span></th>
    <th nowrap class="tableHead"><span class="tableHeadEl">&nbsp;</span></th>
    <th nowrap class="tableHead"><span class="tableHeadEl"><%=I18n.get("LAST_MODIFIED")%></span></th>
    <th nowrap class="tableHead"><span class="tableHeadEl"><%=I18n.get("TAGS")%></span></th>
    <th nowrap class="tableHead" style="width: 25px"></th>

  </tr>
  </thead><%



  DocumentDrawer docDrawer = new DocumentDrawer(resource);
  docDrawer.drawOnlyRoots = !pageState.getEntry("SHOW_VERSIONS").checkFieldValue();

  docDrawer.sorted = false;
  docDrawer.drawDocumentable(pageContext);


  %>
     <tfoot>
     <tr><td id="bulkPlace" colspan="99"></td></tr>
     </tfoot>


     </table> <%

  if (resource.getChildrenSize()>0) {
    pageState.setFocusedObjectDomId("ck_"+showCh.fieldName);
  }

%>


</div>
 <%

     //---------------------------------------- SIDE BAR -------------------------------------------
     JspHelper side = new JspHelper("part/partResourceSideBar.jsp");
     side.parameters.put("RESOURCE", resource);
     side.toHtml(pageContext);



    form.end(pageContext);


   JspHelper bulkOp = new JspHelper("/applications/teamwork/document/partDocumentBulkOperations.jsp");

   bulkOp.toHtml(pageContext);

 %>
<script>
  function closeFormEditorCallback(){
    location.href=location.pathname+"?CM=LIST_DOCS&RES_ID=<%=resource.getId()%>";
  }
</script>
<%
  }

%>