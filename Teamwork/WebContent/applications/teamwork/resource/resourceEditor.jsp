<%@ page import="com.twproject.document.DocumentBricks, com.twproject.document.TeamworkDocument, com.twproject.operator.TeamworkOperator, com.twproject.resource.Person,
                 com.twproject.resource.Resource, com.twproject.resource.ResourceBricks, com.twproject.resource.businessLogic.ResourceController, com.twproject.security.TeamworkPermissions, com.twproject.utilities.TeamworkComparators,
                 com.twproject.waf.TeamworkHBFScreen, com.twproject.waf.html.ResourceHeaderBar, net.sf.json.JSONArray, org.jblooming.anagraphicalData.AnagraphicalData, org.jblooming.designer.Designer,
                 org.jblooming.designer.DesignerData, org.jblooming.utilities.JSP, org.jblooming.utilities.ReflectionUtilities, org.jblooming.waf.Bricks, org.jblooming.waf.PageQuark, org.jblooming.waf.PluginBricks, org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.OperatorConstants, org.jblooming.waf.html.button.*, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.container.DivOnMouseover, org.jblooming.waf.html.core.JST, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.html.input.SmartCombo, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.input.Uploader, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.ArrayList, java.util.Collections, java.util.List, java.util.Set"
    %><%
  PageState pageState = PageState.getCurrentPageState(request);


  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new ResourceController(), request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);

    pageState.toHtml(pageContext);

  } else {

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    Resource resource = (Resource) pageState.getMainObject();
    boolean isNew = resource.isNew();
    boolean isPerson = (resource instanceof Person);

    boolean canWrite =resource.hasPermissionFor(logged,TeamworkPermissions.resource_canWrite);

    PageSeed seed = pageState.thisPage(request);
    seed.setMainObjectId(resource.getId());
    seed.addClientEntry("RESOURCE_TYPE", ReflectionUtilities.deProxy(resource.getClass().getName()));
    seed.setCommand(Commands.EDIT);
    Form form = new Form(seed);
    pageState.setForm(form);
    form.alertOnChange = true;
    form.start(pageContext);


%>
<div class="mainColumn">
<%

  //---------------------------------------- HEAD BAR -------------------------------------------
  pageState.addClientEntry("RESOURCE_TABSET","RESOURCE_GENERAL_TAB");
  ResourceHeaderBar head = new ResourceHeaderBar(resource);
  head.toHtml(pageContext);

%>
<div class="inlineContainerWrapper">

<div class="container  <%=(resource.isNew() ? "new" : "" )%> ">
  <div id="generalData">

    <table border="0" cellspacing="0" cellpadding="3" id="" class="table">
      <tr>
        <%
          TextField tf;
          if (isPerson) {
        %><td nowrap style="width: 15%"><%=resource.isNew() ? "" : "(Id:&nbsp;"+resource.getId()+")&nbsp;"%><%
        tf = new TextField(I18n.get("COURTESY_TITLE"),"COURTESY_TITLE","<br>",14,false);
        tf.readOnly = !canWrite;
        tf.toHtml(pageContext);

      %></td><td style="width: 20%"><%

        tf = new TextField(I18n.get(OperatorConstants.FLD_NAME), OperatorConstants.FLD_NAME, "<br>", 25, false);
        tf.readOnly = !canWrite;
        tf.fieldClass = "formElements bold";
        tf.toHtml(pageContext);
        if (resource.isNew())
          pageState.setFocusedObjectDomId(tf.id);

      %></td><td style="width: 40%"><%

        tf = new TextField(I18n.get(OperatorConstants.FLD_SURNAME), OperatorConstants.FLD_SURNAME, "<br>", 40, false);
        tf.readOnly = !canWrite;
        tf.required = true;
        tf.fieldClass = "formElements bold";
        tf.toHtml(pageContext);

      %></td><%

      } else {
      %><td colspan="2" style="width: 40%"><%
        tf = new TextField(I18n.get("NAME"),"NAME","<br>",50,false);
        tf.readOnly = !canWrite;
        tf.required = true;
        tf.fieldClass = "formElements bold";
        tf.toHtml(pageContext);

      %></td> <td nowrap style="width: 10%"><%
        SmartCombo sb =ResourceBricks.getDepartmentTypeCombo("TYPE", pageState);
        sb.readOnly = !canWrite;
        sb.toHtml(pageContext);

      %></td><%
        }
      %>
        <td  nowrap style="width: 10%">
          <%
            tf = new TextField(I18n.get("CODE"),"CODE","<br>",14,false);
            tf.readOnly = !canWrite;
            tf.toHtml(pageContext);
          %>
        </td>
      </tr>

    </table>
  </div>

  <div id="boxAddress" style=""></div>

  <div style="padding:10px 0; text-align: right"><%
    ButtonJS addData = new ButtonJS(I18n.get("ADD_ANAGRAPHICAL_DATA"),"addAD();");
    addData.enabled = canWrite;
    addData.toolTip= I18n.get("ADD_ANAGRAPHICAL_DATA");
    addData.iconChar="P";
    addData.additionalCssClass="small";
    addData.toHtml(pageContext);

  %>
  </div>
  <%--------------------PERSONAL START-----------------%>
  <br>
  <hr>
  <div id="personalData">
    <jsp:include page="partResourcePersonal.jsp"/>
  </div>
  <%--------------------PERSONAL END-------------------%>
  <%
    ButtonBar buttonBar = new ButtonBar();
    buttonBar.loggableIdentifiableSupport = resource;

    ButtonJS save = new ButtonJS(I18n.get("SAVE"),"saveResource()");
    save.enabled = canWrite ;
    save.additionalCssClass="big first";
    buttonBar.addButton(save);

    if (!isNew && !resource.equals(logged.getPerson())) {
      DeletePreviewer deletePreviewer = new DeletePreviewer("RES_DEL",ResourceController.class, pageState);
      deletePreviewer.normalizeInstanceToSuperclass = Resource.class;

      ButtonSupport delPrev = deletePreviewer.getDeleteButton(I18n.get("DELETE"), resource.getId() );
      delPrev.enabled = resource.hasPermissionFor(logged,TeamworkPermissions.resource_canDelete);
      delPrev.additionalCssClass="big delete";

      buttonBar.addButton(delPrev);
    }

    buttonBar.toHtml(pageContext);

    //this button is used to get the right submit script
    ButtonSubmit saveButton=ButtonSubmit.getSaveInstance(form,"fake");

    //-----------------  prepare anagraphical data for JS -----------------

    JSONArray jsaAD= new JSONArray();

    for (AnagraphicalData ad: resource.getAnagraphicalDataOrdered()){
      jsaAD.add(ad.jsonify());
    }
  %></div>

<div class="container <%=(resource.isNew() ? "new" : "" )%>">
<%if(!isPerson){%>
<div class="workgroup" style="max-width: 250px;">
<h2><%=I18n.get("WORKGROUP")%></h2>
<div class="clearfix"><%
  Set<Person> workGroup = resource.getPersons();
  int i=1;
  for (Person res: workGroup) {
    %><div class="workgroupElement"><img src="<%=res.bricks.getAvatarImageUrl()%>" title="<%=res.getDisplayName()%>" style="cursor:pointer;" class="face"></div><%

    if (i>30 && workGroup.size()>=i){
      %><span class="faceMore" title="<%=I18n.get("MORE")%>"><span class="teamworkIcon" style="line-height:36px; font-size:20px;cursor:pointer">&hellip;</span></span><%
      break;
    }
    i++;
  }


%>
</div></div>
<%}%>
<div class="buttonBoxInline">
  <h5><%=(!isPerson ? I18n.get("ACTIONS_FOR_THIS_WORKGROUP") : I18n.get("ACTIONS_FOR_THIS_RESOURCE"))%></h5>



  <%
    {
      PageSeed aw = pageState.pageFromRoot("agenda/agendaWeekDay.jsp");
      Bricks.addReferral(resource.getId(), Resource.class, aw);
      aw.setCommand("FIND_BY_ENTITY");
      ButtonLink buttonLink = new ButtonLink(I18n.get("AGENDA"), aw);
      buttonLink.toolTip = I18n.get("AGENDA");
      buttonLink.additionalCssClass="small";
      buttonLink.iconChar="m";
      buttonLink.toHtmlInTextOnlyModality(pageContext);
    }

    {
      PageSeed pssm = pageState.pageFromRoot("messaging/composeMessage.jsp");
      Bricks.addReferral(resource.getId(), Resource.class, pssm);
      pssm.setCommand("FIND_BY_ENTITY");
      ButtonSupport buttonLink = ButtonLink.getBlackInstance(isPerson?I18n.get("SEND_MESSAGE"):I18n.get("WORKGROUP_SEND_MESSAGE"),600,800, pssm);
      buttonLink.additionalCssClass="small";
      buttonLink.iconChar="&#xec;";
      buttonLink.toHtmlInTextOnlyModality(pageContext);
    }

    {
      PageSeed ol = pageState.pageFromRoot("task/worklog/worklogOverview.jsp");
      ol.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));
      Bricks.addReferral(resource.getId(), Resource.class, ol);
      ol.setCommand("FIND_BY_ENTITY");
      ButtonLink buttonLink = new ButtonLink(I18n.get("TIMESHETS_OVERVIEW"), ol);
      buttonLink.toolTip = I18n.get("TIMESHETS_OVERVIEW");
      buttonLink.additionalCssClass="small";
      buttonLink.iconChar="A";
      buttonLink.toHtmlInTextOnlyModality(pageContext);
    }

    {
      if (resource.hasPermissionFor(logged,TeamworkPermissions.worklog_manage) || logged.equals(resource.getMyself())) {
        PageSeed ps= pageState.pageFromRoot("/task/worklog/approval/worklogApprovalByResource.jsp");
        ps.command="dummy";
        ps.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));
        ps.addClientEntry("RES_ID", resource.getId());

        ButtonLink bl = new ButtonLink(I18n.get("WORKLOG_APPROVAL"), ps);
        bl.toolTip=I18n.get("WORKLOG_APPROVAL");
        bl.additionalCssClass="small lreq20 lreqLabel lreqActive";
        bl.iconChar="A";
        bl.toHtmlInTextOnlyModality(pageContext);

      }
    }

  %><br>

  <%if ((resource instanceof Person || resource.getPersons().size()>0) && (resource.hasPermissionFor(logged,TeamworkPermissions.resource_manage) || logged.equals(resource.getMyself()))) {%>
  <h5><%=I18n.get("PLAN_MENU")%></h5><%

    {
      PageSeed pl = pageState.pageFromRoot("task/plan/planByResource.jsp");
      Bricks.addReferral(resource.getId(), Resource.class, pl);
      pl.setCommand("FIND_BY_ENTITY");
      pl.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));

      ButtonLink buttonLink = new ButtonLink(I18n.get("PLAN_BY_RESOURCE"), pl);
      buttonLink.toolTip = I18n.get("PLAN_BY_RESOURCE");
      buttonLink.additionalCssClass="small lreq30 lreqLabel";
      buttonLink.toHtmlInTextOnlyModality(pageContext);
    }

    {
      PageSeed ol = pageState.pageFromRoot("task/plan/operatorLoad.jsp");
      ol.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));
      Bricks.addReferral(resource.getId(), Resource.class, ol);
      ol.setCommand("FIND_BY_ENTITY");
      ButtonLink buttonLink = new ButtonLink(I18n.get("RESOURCE_WORKGROUP_OPERATOR_LOAD"), ol);
      buttonLink.toolTip = I18n.get("RESOURCE_WORKGROUP_OPERATOR_LOAD");
      buttonLink.additionalCssClass="small lreq20 lreqLabel lreqActive";
      buttonLink.toHtmlInTextOnlyModality(pageContext);
    }

    {
      PageSeed ap = pageState.pageFromRoot("task/assignment/assignmentPriority.jsp");
      ap.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));
      Bricks.addReferral(resource.getId(), Resource.class, ap);
      ap.setCommand("FIND_BY_ENTITY");
      ButtonLink buttonLink = new ButtonLink(I18n.get("RESOURCE_WORKGROUP_PRIORITY"), ap);
      buttonLink.toolTip = I18n.get("RESOURCE_WORKGROUP_PRIORITY");
      buttonLink.additionalCssClass="small lreq20 lreqLabel lreqActive";
      buttonLink.toHtmlInTextOnlyModality(pageContext);
    }
  }
%></div><%

  //------------------CHILDREN or COLLEGUES START
  List<Resource> res =null;
  String boxTitle = I18n.get("RESOURCES");
  if(!isPerson && resource.getChildrenSize()>0) {
    res = resource.getChildrenOrdered();

  } else if (isPerson && resource.getParent()!=null && resource.getParent().getChildrenSize()>1){
    boxTitle =I18n.get("COLLEGUES%%",((Person)resource).getPersonSurname());
    res = resource.getParent().getChildrenOrdered();
  }

%>
<div class="workgroup">
  <%  if (res!=null){
  %><div class="separator"></div><%


%><h2><%=boxTitle%></h2><%

  for (Resource child : res) {
    if (child.equals(resource))
      continue;
    List<AnagraphicalData> orderAnagraphicalData =child.getAnagraphicalDataOrdered();

    PageSeed ps = pageState.pageFromRoot("resource/resourceEditor.jsp");
    ps.setCommand(Commands.EDIT);
    ps.setMainObjectId(child.getId());

    ButtonLink editChild = ButtonLink.getTextualInstance(child.getName(), ps);
    editChild.style="overflow: hidden;text-overflow: ellipsis; white-space: nowrap;width:100%;" ;
    editChild.additionalCssClass="small";

%><div class="workgroupResource">
  <%editChild.toHtml(pageContext);%>
  <%
    if(orderAnagraphicalData.size()>0) {
  %><div class="workgroupAD"><%
  AnagraphicalData data = orderAnagraphicalData.get(0);
  if(JSP.ex(data.getTelephone())) {
%><span class="teamworkIcon">&</span><%=data.getTelephone()%><br><%
  }
  if(JSP.ex(data.getMobile())) {
%><span class="teamworkIcon">U</span><%=data.getMobile()%><br><%
  }
  if(JSP.ex(data.getEmail())){
%><a href="mailto:<%=data.getEmail()%>" title="<%=data.getEmail()%>" class="button textual" style="color: #656565;"><span class="teamworkIcon">S</span><%=data.getEmail()%></a><%
  }
%></div><%
  }

%></div><%

    }
  }

%></div><%

  // ----------------------------- STAFF START --------------------------------
  if (isPerson) {
    if(resource.getMyStaff() != null && resource.getMyStaff().size()>0) {
      if (res!=null){
%><div class="separator"></div><%
  }
%>
<div class="staff">
  <h2><%=I18n.get("MY_STAFF")%></h2>

  <%
    List<Resource> ch = new ArrayList(resource.getMyStaff());
    Collections.sort(ch, new TeamworkComparators.ResourceComparator());

    for (Resource chicken :ch ) {
      PageSeed ps = pageState.thisPage(request);
      ps.setCommand(Commands.EDIT);
      ps.setMainObjectId(chicken.getId());
      ButtonLink editChicken = ButtonLink.getTextualInstance(chicken.getDisplayName(), ps);
      editChicken.enabled = resource.hasPermissionFor(logged,TeamworkPermissions.resource_canWrite);
  %>
  <div>
    <%editChicken.toHtml(pageContext);%>
  </div>
  <%
    }
  %>

</div>
<%
    }

  }

  //-----------------DOCUMENT
  if(true|| pageState.href.indexOf("resourceDocument")<=0) {

%>
<div class="separator"></div><%

  PageSeed psDocumentList = pageState.pageFromRoot("resource/resourceDocumentList.jsp");
  psDocumentList.setCommand("LIST_DOCS");
  psDocumentList.addClientEntry("RES_ID",resource.getId());
  ButtonLink blDocumentList = new ButtonLink(I18n.get("DOCUMENTS"), psDocumentList);
  blDocumentList.enabled = !isNew;
%>

<div class="documents">
  <h2><span <%=blDocumentList.generateLaunchJs()%> style="cursor:pointer;display: inline"><%=I18n.get("DOCUMENTS")%></span>
    <%
      if (resource.hasPermissionFor(logged,TeamworkPermissions.resource_canWrite)) {

        ButtonJS addOpener = new ButtonJS("");
        addOpener.toolTip = I18n.get("ADD");
        addOpener.label = "";
        addOpener.additionalCssClass = "edit";
        addOpener.iconChar = "P";
        DivOnMouseover add = new DivOnMouseover(addOpener);

        // ADD DOCUMENT UPLOAD
        PageSeed ps = pageState.pageFromRoot("resource/resourceDocumentEditor.jsp");
        ps.setCommand(Commands.ADD);
        ps.addClientEntry("RES_ID", resource.getId());
        ps.addClientEntry("DOCUMENT_TYPE", TeamworkDocument.IS_UPLOAD);
        ButtonSupport addDoc = ButtonLink.getBlackInstance("+ "+I18n.get("ADD_DOCUMENT_UPLOAD"),700,1000, ps);
        add.addButton(addDoc);

        // ADD DOCUMENT FILE STORAGE
        //if (logged.hasPermissionFor(TeamworkPermissions.fileStorage_canRead)) {
        ps = pageState.pageFromRoot("resource/resourceDocumentEditor.jsp");
        ps.setCommand(Commands.ADD);
        ps.addClientEntry("RES_ID", resource.getId());
        ps.addClientEntry("DOCUMENT_TYPE", TeamworkDocument.IS_FILE_STORAGE);
        addDoc = ButtonLink.getBlackInstance("+ "+I18n.get("ADD_DOCUMENT_REMOTE"),700,1000, ps);
        addDoc.enabled=logged.hasPermissionFor(TeamworkPermissions.fileStorage_canRead);
        add.addButton(addDoc);
        //}

        // ADD DOCUMENT URL
        ps = pageState.pageFromRoot("resource/resourceDocumentEditor.jsp");
        ps.setCommand(Commands.ADD);
        ps.addClientEntry("RES_ID", resource.getId());
        ps.addClientEntry("DOCUMENT_TYPE", TeamworkDocument.IS_URL);
        addDoc = ButtonLink.getBlackInstance("+ "+I18n.get("ADD_DOCUMENT_URL"),600,700, ps);
        add.addButton(addDoc);

        // ADD CONTENT
        ps = pageState.pageFromRoot("resource/resourceDocumentEditor.jsp");
        ps.setCommand(Commands.ADD);
        ps.addClientEntry("RES_ID", resource.getId());
        ps.addClientEntry("DOCUMENT_TYPE", TeamworkDocument.IS_CONTENT);
        addDoc = ButtonLink.getBlackInstance("+ "+I18n.get("ADD_DOCUMENT_CONTENT"),700,700, ps);
        add.addButton(addDoc);

        //add.toHtml(pageContext);

    %>

   <span style="float:right;font-size: 14px;padding-top: 7px;"><% add.toHtmlInTextOnlyModality(pageContext);%></span><%
      }
   %>
</h2>

  <%

  // ----------------------------- CUSTOM FORMS  --------------------------------
  boolean visibleForm =false;
  Set<PageQuark> pqs = PluginBricks.getPageQuarkGroup("RESOURCE_FORMS");
  for (PageQuark pq : pqs) {
    if (pq.isVisibleInThisContext(pageState) ) {
      visibleForm =true;
      break;
    }
  }

  if (visibleForm){

    PageSeed edf = pageState.pageFromRoot("document/drawModule.jsp");
    edf.mainObjectId = resource.getId();

    edf.setPopup(true);
    for (PageQuark pq : pqs) {
      if (pq.isVisibleInThisContext(pageState)) {
        edf.addClientEntry("DESIGNER_URL_TO_INCLUDE",pq.getHrefForInclude());
        edf.addClientEntry("DESIGNER_NAME",pq.getName());
        edf.addClientEntry("CLAZZ",Resource.class.getName());

        Designer designer = new Designer(pq.urlToInclude, pq.getName(), Resource.class, resource.getId());
        DesignerData designerData = designer.getDesignerData();
        boolean fulfilled = designerData.getValueMap().size()>0;


        ButtonSupport toForm = ButtonLink.getBlackInstance(I18n.get(pq.getName()), 819, 1152, edf);
        toForm.iconChar="&#xe0;";
        toForm.additionalCssClass="small";

        %><div class="customFormLink lreq30 lreqLabel"><%toForm.toHtmlInTextOnlyModality(pageContext);%><%
        if (fulfilled) {
        %><span class="teamworkIcon" style="position: absolute;right: 0;top:0; font-size:120%;cursor: default" title="<%=I18n.get("FORM_FILLED")%>">&#x2039;</span><%
        }
        %></div><%

      }
    }
  }

  if ( resource.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.document_canCreate) && !resource.isNew() ){
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

            var fileWrapper = $("<div/>").addClass("docLabelWrapper");
            var dr = $(response.docHRef);
            var image = $("<img>"); //response.docMime;
            image.attr({src:contextPath+"/img/mime/" + response.docMime}).css({width:20});
            fileWrapper.append(image).append(dr);
            $("#documentList").append(fileWrapper);
            dr.effect("highlight", { color: "#F9EFC5" }, 2500);

          }
        }
      });
    });
  </script>
  <%
    }
  %>
  <%------------------------------- DOCUMENTS START ----------------------------------%>
  <div id="documentList" class="textSmall"><%

    List docs = DocumentBricks.getLastVersionDocuments(resource);

    if(docs.size()>0){
      for (TeamworkDocument document: DocumentBricks.getLastVersionDocuments(resource)){
        AHref aHref = document.bricks.getContentLink(pageState);

  %><div class="docLabelWrapper">
    <%
      document.bricks.getMimeImage().toHtml(pageContext);
      aHref.toHtml(pageContext);
    %>
  </div><%
    }
  }else{
  %>
    <div class="docLabelWrapper"><%=I18n.get("NO_FILES")%></div>

    <%
      }
    %></div>

</div> <%
  }


%></div>
</div>
<%
  //------------------------------------------ JAVASCRIPT TEPLATES -----------------------------------------------
%>

<div id="jsTemplates" style="display:none;">
  <%=JST.start("AD_EDITOR")%>
  <table  class="ADEditor" idAD="##id##" order="##order##" border="0">

    <tr>
      <td class="dragHandler" width="13" rowspan="5" style="background-color: #eee">&nbsp;</td>

      <td style="padding-left: 10px"><%
        pageState.addClientEntry("location","##location##");
        tf = new TextField(I18n.get("DESCRIPTION"),"location","<br>",30,false);
        //tf.script="style='width:100%'";
        tf.innerLabel=I18n.get("LOCATION_OFFICE_HOME");
        tf.readOnly = !canWrite;
        tf.toHtml(pageContext);
      %>
      </td>
      <td>
        <%
          pageState.addClientEntry("telephone","##telephone##");
          tf = new TextField(I18n.get("TELEPHONE"),"telephone","<br>",30,false);
          tf.readOnly = !canWrite;
          tf.toHtml(pageContext);
        %>
      </td>
      <td>
        <%
          pageState.addClientEntry("email","##email##");
          tf = new TextField(I18n.get("EMAIL"), "email", "<br>", 30, false);
          tf.readOnly = !canWrite;
          tf.toHtml(pageContext);
        %>
      </td>

      <td rowspan="3" align="right" valign="top"><span onclick="closeEditAD($(this));" class="teamworkIcon" style="cursor:pointer" title="<%=I18n.get("CLOSE")%>">x</span></td>

    </tr>
    <tr>
      <td style="padding-left: 10px">
        <%
          pageState.addClientEntry("mobile","##mobile##");
          tf = new TextField(I18n.get("MOBILE"),"mobile","<br>",30,false);
          tf.readOnly = !canWrite;
          tf.toHtml(pageContext);
        %>
      </td>
      <td>
        <%
          pageState.addClientEntry("fax","##fax##");
          tf = new TextField(I18n.get("FAX"),"fax","<br>",30,false);
          tf.readOnly = !canWrite;
          tf.toHtml(pageContext);
        %>
      </td>
      <td>
        <%
          pageState.addClientEntry("url","##url##");
          tf = new TextField(I18n.get("URL"),"url","<br>",30,false);
          tf.readOnly = !canWrite;
          tf.toHtml(pageContext);
        %>
      </td>
    </tr>
<tr>
  <td colspan="3"><hr></td>
</tr>
<tr>
  <td style="padding-left: 10px"><%
    pageState.addClientEntry("address", "##address##");
    tf = new TextField(I18n.get("ADDRESS"), "address", "<br>", 30, false);
    tf.readOnly = !canWrite;
    tf.toHtml(pageContext);
  %>
  </td>
  <td><%
    pageState.addClientEntry("city", "##city##");
    tf = new TextField(I18n.get("CITY"), "city", "<br>", 30, false);
    tf.readOnly = !canWrite;
    tf.toHtml(pageContext);
  %>
  </td>
  <td>
    <%
      pageState.addClientEntry("province", "##province##");
      tf = new TextField(I18n.get("PROVINCE"), "province", "<br>", 30, false);
      tf.readOnly = !canWrite;
      tf.toHtml(pageContext);
    %></td>

</tr>
    <tr>
      <td style="padding-left: 10px"><%
      pageState.addClientEntry("zip", "##zip##");
      tf = new TextField(I18n.get("ZIP"), "zip", "<br>", 10, false);
      tf.readOnly = !canWrite;
      tf.toHtml(pageContext);
    %>
    </td>
      <td colspan="2">
        <%
          pageState.addClientEntry("country","##country##");
          tf = new TextField(I18n.get("COUNTRY"), "country", "<br>", 30, false);
          tf.readOnly = !canWrite;
          tf.toHtml(pageContext);
        %>
      </td>
      <td align="right"><%if(canWrite){%><span onclick="$(this).confirm(function(){deleteAD($(this))});" class="button textual icon delete" title="<%=I18n.get("DELETE")%>"><span class="teamworkIcon">d</span></span><%}%></td>
    </tr>
  </table>
  <%=JST.end()%>
  <%=JST.start("AD_VIEWER")%>
  <table class="ADViewer" idAD="(#=id#)" order="(#=order#)">
    <tr><td class="dragHandler" width="13" rowspan="2"style="background-color: #eee">&nbsp;</td>
      <td width="120" valign="top">
        <span onclick="editAD($(this));" style="cursor:pointer" ><label>(#=obj.location#)</label></span>
      </td>
      <td valign="top"  width="25%" nowrap>
        (#if (obj.telephone){#)<span id="telephone"><span class="teamworkIcon">&</span> (#=obj.telephone#)<br></span>(#}#)
        (#if (obj.mobile){#)<span id="mobile"><span class="teamworkIcon">U</span> (#=obj.mobile#)<br></span>(#}#)
         <%--(#if (obj.fax){#)<span id="fax"><span class="teamworkIcon">p</span> (#=obj.fax#)<br></span>(#}#)--%>
      </td>
      <td valign="top" width="25%">
        (#if (obj.email){#)<span class="teamworkIcon">S</span> <a href="mailto:(#=obj.email#)">(#=obj.email#)</a><br>(#}#)
      </td>
      <td valign="top" width="25%" nowrap>
        (#if (obj.url){#)<span class="teamworkIcon">W</span> <a href="(#=obj.url#)" target="_blank">(#=obj.url#)</a>(#}#)
      </td>
      <td align="right"><span onclick="editAD($(this));" class="teamworkIcon editButton" style="cursor:pointer" title="<%=I18n.get("EDIT")%>">e</span></td>
    </tr>
  </table>
  <%=JST.end()%>
</div>

</div>

<%--!---------------------------------------------- MAIN COLUMN END ---------------------------------------------------------%>

<%

  //---------------------------------------- SIDE BAR -------------------------------------------
  JspHelper side = new JspHelper("part/partResourceSideBar.jsp");
  side.parameters.put("RESOURCE", resource);
  side.toHtml(pageContext);
%>
<script type="text/javascript">

  var jsaAD=<%=jsaAD%>;

  $(function() {

    $("#jsTemplates").loadTemplates().remove();

    <%if (!resource.isNew()){%>
    $.JST.loadDecorator("AD_EDITOR", function (htmlBlock, jsonData) {
      if ((jsonData.id+"").startsWith("new"))
        htmlBlock.addClass("new");
    });
    <%}%>

    var boxAd=$("#boxAddress");

    //console.debug(jsaAD,jsaAD.length)
    if (jsaAD.length==0){
      addAD();
    }else {
      for (var i=0;i<jsaAD.length;i++){

        var ad=jsaAD[i];
        //if ad is empty open it in edit
        var empty=true;
        for (var key in ad){
          if (key=="id"||key=="order"||key=="location")
            continue;
          if(ad[key] != ""){
            empty=false;
            break;
          }
        }

        var viewer = $.JST.createFromTemplate(ad, empty ? "AD_EDITOR":"AD_VIEWER");
        boxAd.append(viewer);

      }
    }

    //se devo dare il focus alla mail
    if (<%=pageState.getEntry("focusToEmail").checkFieldValue()%>){
      $("#boxAddress .ADViewer:first .editButton").click();
      $("#email:first").focus();
    }

    boxAd.sortable({
      handle:".dragHandler",
      axis:"y",
      stop:function(ev,ui){
        var order=0;
        $(this).children().each(function(){
          $(this).attr("order",order++);
        });
      }
    });

    var canWrite = <%=canWrite%>;

/*
    if(canWrite)
      $(".profileImage img").on("click", function(){
        openProfileImageEditor('<%=resource.getId()%>');
      }).on("mouseover", function(){
        $(this).addClass("hover");
      }).on("mouseout", function(){
        $(this).removeClass("hover");
      })
*/
  });

  function getADfromId(id){
    for (var i=0;i<jsaAD.length;i++ ){
      if (jsaAD[i].id==id){
        return jsaAD[i];
      }
    }
    return false;
  }

  function editAD(el){
    var view=el.closest("[idAd]");
    var idAD=view.attr("idAD");
    var editor=$.JST.createFromTemplate(getADfromId(idAD), "AD_EDITOR");
    view.replaceWith(editor);
  }

  function addAD(){
    var boxAd=$("#boxAddress");
    var editor=$.JST.createFromTemplate({id:"new"+new Date().getTime(), order:0}, "AD_EDITOR");
    boxAd.append(editor);
  }

  function deleteAD(el) {
    var ad = el.closest("[idAD]");
    var idAD = ad.attr("idAD");
    var request = {
      CM:"DELAD",
      idAD:idAD,
      resId:"<%=resource.getId()%>"
    };

    $.getJSON("resourceAjaxController.jsp", request, function(response) {
      jsonResponseHandling(response);
      if (response.ok) {
        ad.fadeOut(500, function() {
          $(this).remove();
          $(window).resize();
        });

        for (var i=0;i<jsaAD.length;i++ ){
          if (jsaAD[i].id==idAD){
            jsaAD.splice(i,1);
            break;
          }
        }
      }
    });
  }

  function closeEditAD(el){
    var adEdit = el.closest("[idAD]");
    var idAD = adEdit.attr("idAD");

    var close=function(){
      if (idAD.startsWith("new")  )
        adEdit.fadeOut(500, function() { $(this).remove(); });
      else
        adEdit.replaceWith($.JST.createFromTemplate(getADfromId(idAD), "AD_VIEWER"));
    };

    if (adEdit.find("[oldvalue]:input").isValueChanged())
      el.confirm(close);
    else
      close();
  }


  function saveResource(){
    //set command to save
    var form =$("#<%=form.id%>");

    var ads=[];
    //get data from open editor
    $(".ADEditor").each(function(order){
      var editor=$(this);
      var idAD=editor.attr("idAD");
      var fromEdi={};
      fromEdi.id=idAD;
      fromEdi.order=parseInt(editor.attr("order")||order);  // la mette sempre per ultima
      editor.fillJsonWithInputValues(fromEdi);

      ads.push(fromEdi);
      editor.remove();// in order to avoid send inutil ce
    });

    //then check if viewer is resorted
    $(".ADViewer").each(function(){
      var viewer=$(this);
      var idAD=viewer.attr("idAD");
      var inMem=getADfromId(idAD);
      if (viewer.attr("order")!=inMem.order){
        inMem.order=parseInt(viewer.attr("order"));
        ads.push(inMem);
      }
    });

      if (ads.length>0){
        // inject hidden field on form
        var hid=$("<input type='hidden' name='ads'>");
        hid.val(JSON.stringify(ads));
        form.append(hid);
      }

    <%=saveButton.generateJs()%>
  }

  function openProfileImageEditor(resId,callback){
    $(".uploadizeDrop").attr("disabled", "true");
    openBlackPopup(contextPath+"/applications/teamwork/resource/resourceImageUploader.jsp?RES_ID="+resId,'500px','550px', function (response) {
      //console.debug("imageEditorCallback",response);
      if (response && response.imageUrl) {
        $(".profileImage img,.imageUploaderOpener img").prop("src", response.imageUrl);
        if (<%=resource.equals(logged.getPerson())%>) { //se sto cambiando l'utente loggato cambio subito anche il profilo
          $(".menuTools .avatarImage img").prop("src", response.imageUrl)
        }
      }
    });
  }


</script>

<%
    form.end(pageContext);
  }
%>
