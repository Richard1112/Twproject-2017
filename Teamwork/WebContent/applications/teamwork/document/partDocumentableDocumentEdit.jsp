<%@ page import="com.twproject.document.TeamworkDocument, com.twproject.operator.TeamworkOperator, com.twproject.security.TeamworkPermissions, org.jblooming.ontology.Documentable, org.jblooming.persistence.PersistenceHome, org.jblooming.utilities.CodeValueList, org.jblooming.utilities.JSP, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.html.input.*, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, java.util.Map, com.twproject.document.businessLogic.DocumentController, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.display.Img, com.twproject.resource.ResourceBricks, org.jblooming.security.License"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  //this is set by action
  Documentable documentable = (Documentable) pageState.attributes.get("REFERRAL_OBJECT");

  if ("DOCUMENT_DELETE_PREVIEW".equals(pageState.getCommand()))
    pageState.setCommand(Commands.DELETE_PREVIEW);

  //BEGIN PRETEND BEING DOCUMENT

  TeamworkDocument document = (TeamworkDocument) pageState.getMainObject();
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();


  boolean isNew = PersistenceHome.NEW_EMPTY_ID.equals(document.getId());

  TextField.hiddenInstanceToHtml("DOC_ROOT_ID", pageState.getEntry("DOC_ROOT_ID").stringValueNullIfEmpty(), pageContext);
  TextField.hiddenInstanceToHtml("DOCUMENT_TYPE", pageContext);


  boolean canWrite =  document.isEnabled(logged) && document.hasPermissionFor(logged, TeamworkPermissions.document_canWrite);
  canWrite=canWrite || (document.isNew() && documentable.hasPermissionFor(logged, TeamworkPermissions.document_canCreate));

  ButtonSubmit oms = new ButtonSubmit(pageState.getForm());
  oms.alertOnChange = false;
  oms.variationsFromForm.setCommand("ADD_DOCUMENT");
  int typeId = pageState.getEntry("DOCUMENT_TYPE").intValueNoErrorCodeNoExc();

  boolean canReadFileStorage = logged.hasPermissionFor(TeamworkPermissions.fileStorage_canRead);

  boolean isContent = typeId == TeamworkDocument.IS_CONTENT;
  boolean isFileStorage = typeId == TeamworkDocument.IS_FILE_STORAGE;
  boolean isUpload = typeId == TeamworkDocument.IS_UPLOAD;
  boolean isURL = typeId == TeamworkDocument.IS_URL;

  String label ="";
  if (isContent) {
    label = I18n.get("TEXT");
  } else if (isFileStorage) {
    label = I18n.get("IS_FILE_STORAGE");
  } else if (isUpload) {
    label = I18n.get("UPLOAD");
  } else if (isURL) {
    label = I18n.get("LINK");
  }


%><h1><%=label%> <%
  if (isURL) {
    Img img = new Img("external-storages.png","","","30");
    img.style="vertical-align:middle";
    %><span style="padding-left: 30px"><%img.toHtml(pageContext);%></span><%
  }
%></h1>
<table class="table" border="0" cellpadding="5">
  <tr><%


    %><td valign="top" colspan="2"><%

    if (isContent || typeId == 0) {
      // do nothing
    } else {
      if (isFileStorage) {
        %><div class="lreq30 lreqLabel"><%
        UrlFileStorage ufs = new UrlFileStorage("DOCUMENT_URL_TO_CONTENT");
        ufs.separator = "<br>";
        ufs.readOnly = !canWrite || !canReadFileStorage;
        ufs.required = true;
        ufs.referralObjectId = document.getId();
        ufs.label = I18n.get("DOCUMENT_URL_TO_CONTENT");
        ufs.parameters.put("document", document);
        ufs.toHtml(pageContext);
        %></div><%

      } else if (isUpload) {

        Uploader uploader = new Uploader("DOCUMENT_UPLOAD", pageState);
        uploader.label = I18n.get("DOCUMENT_UPLOAD");
        uploader.separator = "<br>";
        uploader.size = 60;
        uploader.treatAsAttachment = false;
        uploader.readOnly = !canWrite;
        uploader.required = canWrite;
        uploader.toolTip = I18n.get("DOCUMENT_UPLOAD");
        uploader.toHtml(pageContext);

      } else if (isURL) {
        TextField tf = new TextField("DOCUMENT_URL_TO_CONTENT", "<br>");
        tf.label = I18n.get("IS_URL");
        tf.fieldSize = 60; 
        tf.script = "style=width:100%";
        tf.toHtml(pageContext);
      }
    }

  %></td></tr>
<tr>
  <td colspan="4">
    <%
      TextField tf = new TextField("DOCUMENT_NAME", "<br>");
      tf.fieldSize = 50;
      tf.required = true;
      tf.readOnly = !canWrite;
      tf.fieldClass = "formElements bold";
      tf.script = "style=width:90%";
      if (isUpload) {
        tf.script = "style=width:80%";
        }


      tf.toHtmlI18n(pageContext);
    %>&nbsp;<small><%=document.isNew() ? "" : "(id: "+document.getId()+")"%></small>
    <%
      if (!isNew && isUpload) {

        CheckField cf = new CheckField("IS_LOCKED", "", false);
        cf.disabled = !canWrite;
        cf.toHtmlI18n(pageContext);

        if (document.getLockedBy() != null) {
    %>&nbsp;<%=JSP.b("(" + document.getLockedBy().getDisplayName() + ")")%><%
      }
    }
  %>
  </td>
</tr>
<%if (isUpload){%>
  <tr>
    <td>
      <%
        tf = new TextField("DOCUMENT_VERSION", "<br>");
        tf.fieldSize = 12;
        tf.readOnly = !canWrite;
        tf.toHtmlI18n(pageContext);
      %>
    </td>
    <td>
      <%
        tf = new TextField("DOCUMENT_VERSION_LABEL", "<br>");
        tf.fieldSize = 35;
        tf.readOnly = !canWrite;
        tf.toHtmlI18n(pageContext);
      %>
    </td>
<%}

if ( isUpload || isContent){
%>
  <td>
    <%
      DateField df = new DateField("DOCUMENT_AUTHORED");
      df.separator = "<br>";
      df.readOnly = !canWrite;
      df.toHtmlI18n(pageContext);
    %>
  </td>
  <td>
    <%
      SmartCombo auth = ResourceBricks.getPersonCombo("DOCUMENT_AUTHOR", false, null, pageState);
      auth.addAllowed=true;
      auth.separator="<br>";
      auth.fieldSize=35;
      auth.readOnly=!canWrite;
      auth.toHtmlI18n(pageContext);
      /*tf = new TextField("DOCUMENT_AUTHOR", "<br>");
      tf.fieldSize = 35;
      tf.readOnly = !canWrite;
      tf.toHtmlI18n(pageContext);*/
    %>
  </td>
</tr>
<%
  }

  %><tr><td colspan="4"><%
  TinyMCE tinyMinute = new TinyMCE(isContent ? I18n.get("TEXT"):I18n.get("SUMMA"), "SUMMA", "", "100%",isContent? "280px": "200px", pageState);
  //tinyMinute.objectClass = "linkEnabled";
  tinyMinute.textArea.preserveOldValue = true;
  tinyMinute.showHTMLButton=true;
  tinyMinute.required=isContent;
  tinyMinute.separator="<br>";

  if(I18n.isActive("CUSTOM_FEATURE_DOCUMENT_EDITOR_EXTENDED")){
      tinyMinute.mode = "advanced";
      tinyMinute.setTheme("advanced");
      tinyMinute.resize =true;
      tinyMinute.additionalPlugins.add("fullscreen");
  }


  tinyMinute.toHtml(pageContext);

%></td></tr>
<tr>
  <td colspan="4"><%
    TagBox tb = new TagBox("DOCUMENT_TAGS", TeamworkDocument.class,document.getArea());
    tb.separator="<br>";
    tb.label=I18n.get("TAGS");
    tb.fieldSize = 50;
    tb.readOnly = !canWrite;
    tb.script = "style=width:100%";
    tb.toHtml(pageContext);
  %>
  </td>

</tr></table><%

  ButtonBar buttonBar = new ButtonBar();

  TeamworkOperator loggedOperator = (TeamworkOperator) pageState.getLoggedOperator();
  //boolean canWrite = documentable.hasPermissionFor(loggedOperator, TeamworkPermissions.document_canWrite);

  ButtonSubmit save = ButtonSubmit.getSaveInstance(pageState.getForm(), I18n.get("SAVE_DOCUMENT"),false);
  save.variationsFromForm.setCommand(Commands.SAVE);
  save.additionalCssClass="first big";
  save.enabled = document.getParent()==null && document.isEnabled(loggedOperator) && canWrite;
  buttonBar.addToRight(save);

  if (!document.isNew() ) {

    if (document.getParent()==null && isUpload) {
      ButtonSubmit addVersion = new ButtonSubmit(pageState.getForm());
      addVersion.variationsFromForm.setCommand("ADD_VERSION");
      addVersion.label = I18n.get("ADD_VERSION");
      addVersion.enabled = document.isEnabled(loggedOperator) && documentable.hasPermissionFor(loggedOperator, TeamworkPermissions.document_canCreate);
      addVersion.additionalCssClass="big";
      buttonBar.addToRight(addVersion);
    }

    new DeletePreviewer("DOC_DEL",DocumentController.class, pageState);

    ButtonJS delB = new ButtonJS(I18n.get("DOCUMENT_DELETE"),"deleteDocumentPreview("+document.getId()+");");
    delB.enabled = documentable.hasPermissionFor(loggedOperator, TeamworkPermissions.document_canDelete);
    delB.additionalCssClass="big delete";


    buttonBar.addToRight(delB);


    buttonBar.loggableIdentifiableSupport=document;
  }

  buttonBar.toHtml(pageContext);

  pageState.setMainObject(documentable);

%><script>

  $(function(){
    var closeBl=<%=Commands.SAVE.equals(pageState.command)%>;
    var isValid=<%=pageState.validEntries()%>;
    if (isValid && closeBl) {
      window.parent.$("body").oneTime(100, "doclrl", function () { window.parent.location.reload() });
    }
  });


  function deleteDocumentPreview(docId){
    deletePreview("DOC_DEL", docId, function(response){  // callback function
      if (response && response.ok){
        //console.debug("deleteIssuePreview done")
        getBlackPopupOpener().$("tr[docId="+docId+"]").remove();
        closeBlackPopup({reload:false});
      }
    });
  }

</script>
