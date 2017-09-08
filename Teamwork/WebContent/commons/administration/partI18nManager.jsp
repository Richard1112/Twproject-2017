<%@ page import="org.jblooming.operator.Operator, org.jblooming.operator.User, org.jblooming.oql.OqlQuery, org.jblooming.security.PlatformPermissions, org.jblooming.utilities.CodeValueList, org.jblooming.utilities.JSP, org.jblooming.utilities.StringUtilities,
                 org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.core.JST, org.jblooming.waf.html.input.*,
                 org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.Application, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.settings.I18nEntryPersistent, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.*" %>
<%
  // number of rows displayed
  int MAXROWS = 200;

  PageState pageState = PageState.getCurrentPageState(request);

  User loggedUser = pageState.getLoggedOperator();

  I18n i18nManager = ApplicationState.i18n;

  // hack to temporary disable i18nedit
  String old_status = i18nManager.getEditStatus();
  i18nManager.setEditStatus(I18n.EDIT_STATUS_READ);
  // temporary disable catching labels
  boolean catchState = i18nManager.catchUsedLabels;
  i18nManager.catchUsedLabels = false;

  pageState.addClientEntry(Fields.FORM_PREFIX + "I18N_MODALITY", old_status);
  pageState.addClientEntry(Fields.FORM_PREFIX + "I18N_LENIENT", i18nManager.getLenient());
  pageState.addClientEntry("CATCHUSEDLABELS", catchState);
  pageState.addClientEntry("ENABLED_LANGUAGES", ApplicationState.applicationSettings.get("ENABLED_LANGUAGES"));


  String searchText = JSP.w(pageState.getEntry("SEARCH_TEXT").stringValueNullIfEmpty());

  //check if is in "customizationModality"
  boolean featureCustomization=searchText.indexOf("CUSTOM_FEATURE")==0;
  boolean customizationModality=searchText.indexOf("_CUSTOM_FIELD_")>0 || searchText.indexOf("CUSTOM_EXPORT_EXCEL_")==0 || searchText.indexOf("COLOR_")==0 || featureCustomization;


  if (customizationModality){
    %>
      <style type="text/css">
        .grayed{
          color: black;
        }
      </style>

    <%
  }

   %><script>$("#ADMINISTRATION_ROOT_MENU").addClass('selected');</script>
<%
  ButtonLink adminLink = new ButtonLink(I18n.get("ADMINISTRATION_ROOT_MENU") + " /",pageState.pageFromRoot("administration/administrationIntro.jsp"));
%>
<%adminLink.toHtmlInTextOnlyModality(pageContext);%>
<h1><%=customizationModality? StringUtilities.capitalize(searchText.replace("_"," ")):I18n.get("I18N_MANAGER")%></h1>

<div class="container" id="bx_18li"><%

  loggedUser.testPermission(PlatformPermissions.i18n_manage);

  PageSeed url = pageState.thisPage(request);

  url.setCommand(Commands.FIND);
  if (pageState.getEntry(Fields.APPLICATION_NAME).stringValueNullIfEmpty() != null)
    url.addClientEntry(Fields.APPLICATION_NAME, pageState.getEntry(Fields.APPLICATION_NAME).stringValueNullIfEmpty());
  url.addClientEntry(Fields.FORM_PREFIX + "code", "");
  Form form = new Form(url);
  form.encType = Form.MULTIPART_FORM_DATA;

  RadioButton rb_read = new RadioButton(I18n.get("MODALITY_READ"), Fields.FORM_PREFIX + "I18N_MODALITY", I18n.EDIT_STATUS_READ, "&nbsp;&nbsp;", null, false, null);
  RadioButton rb_edit = new RadioButton(I18n.get("MODALITY_EDIT"), Fields.FORM_PREFIX + "I18N_MODALITY", I18n.EDIT_STATUS_EDIT, "&nbsp;&nbsp;", null, false, null);
  RadioButton rb_append = new RadioButton(I18n.get("MODALITY_APPEND"), Fields.FORM_PREFIX + "I18N_MODALITY", I18n.EDIT_STATUS_APPEND, "&nbsp;&nbsp;", null, false, null);

  RadioButton rbl_none = new RadioButton(I18n.get("LENIENT_NONE"), Fields.FORM_PREFIX + "I18N_LENIENT", "" + I18n.LENIENT_NONE, "&nbsp;&nbsp;", null, false, null);
  RadioButton rbl_lang = new RadioButton(I18n.get("LENIENT_LANG"), Fields.FORM_PREFIX + "I18N_LENIENT", "" + I18n.LENIENT_LANG, "&nbsp;&nbsp;", null, false, null);
  RadioButton rbl_app = new RadioButton(I18n.get("LENIENT_APP"), Fields.FORM_PREFIX + "I18N_LENIENT", "" + I18n.LENIENT_APP, "&nbsp;&nbsp;", null, false, null);
  RadioButton rbl_appLang = new RadioButton(I18n.get("LENIENT_APP_LANG"), Fields.FORM_PREFIX + "I18N_LENIENT", "" + I18n.LENIENT_APP_LANG, "&nbsp;&nbsp;", null, false, null);

  Set<String> supportedLanguages = i18nManager.supportedLanguages;
  Set<String> filterForLanguages = new TreeSet();

  Map applics = ApplicationState.platformConfiguration.applications;

  CodeValueList cvl = new CodeValueList();
  cvl.add("", "-- all --");
  for (Iterator iterator = applics.keySet().iterator(); iterator.hasNext();) {
    String s = (String) iterator.next();
    Application app = (Application) applics.get(s);
    cvl.add(app.getName(), app.getName());
  }


  Combo applicationsCombo = new Combo(Fields.FORM_PREFIX + "APPLICATION", "&nbsp;", null, 20, cvl, "onChange=\"obj('" + form.id + "').submit();\"");
  applicationsCombo.label = I18n.get("APPLICATIONS");

  String focusedApplicationName = pageState.getEntry(Fields.FORM_PREFIX + "APPLICATION").stringValue();

  if (focusedApplicationName == null) {
    focusedApplicationName = "";
  }

  SortedMap entries;
  if (focusedApplicationName.length() <= 0) {
    entries = i18nManager.codeEntries;
  } else {
    entries = I18n.getEntriesForApplication(focusedApplicationName);
  }



  boolean searchTextEnabled = JSP.ex(searchText);

  boolean suspectSearchEnabled = pageState.getEntry("SUSPECT").checkFieldValue();
  boolean searchFromDBOnlyEnabled = !ApplicationState.platformConfiguration.development && pageState.getEntry("FROMDBONLY").checkFieldValue();

  boolean searchEnabled = searchTextEnabled || suspectSearchEnabled || searchFromDBOnlyEnabled;

  CheckField catchUsedLabels = new CheckField("CATCHUSEDLABELS", "&nbsp;", true);
  catchUsedLabels.label = I18n.get("CATCHUSEDLABELS");

  form.start(pageContext);

  if (!customizationModality){

    Container modality = new Container("i18Mod");
    modality.title = I18n.get("I18N_BEHAVIOUR");
    modality.collapsable = true;
    modality.status = Container.COLLAPSED;
    //modality.setCssPostfix(Css.postfixThin);
    modality.level=3;
    modality.start(pageContext);
%>
<table border="0" cellpadding="2" cellspacing="0">
  <tr>
    <td>
      <%=I18n.get("SELECT_I18N_MODALITY")%>:
      <%rb_read.toHtml(pageContext);%>&nbsp;&nbsp;
      <%rb_edit.toHtml(pageContext);%>&nbsp;&nbsp;
      <%rb_append.toHtml(pageContext);%>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
      <%catchUsedLabels.toHtml(pageContext);%>


      <br> <%=I18n.get("SELECT_I18N_LENIENT_LEVEL")%>
      : <%rbl_none.toHtml(pageContext);%>&nbsp;&nbsp; <%rbl_lang.toHtml(pageContext);%>&nbsp;&nbsp;<%rbl_app.toHtml(pageContext);%>&nbsp;&nbsp;<%rbl_appLang.toHtml(pageContext);%>
      <br>
    </td><td>
    <%
      ButtonSubmit save = new ButtonSubmit(form);
      save.variationsFromForm.setCommand(I18n.CMD_CHANGEMODALITY);
      save.label = I18n.get("CHANGE_MODALITY");
      save.toHtml(pageContext);
    %>
  </td>
    <td>
      <%
        PageSeed ps = pageState.pageFromCommonsRoot("administration/i18nMonitor.jsp");
        ps.setPopup(true);
        ButtonLink monitor = new ButtonLink(ps);
        monitor.label = "monitor";
        monitor.target = "_blank";
        monitor.popup_height = "500";
        monitor.popup_width = "500";
        monitor.popup_resizable = "yes";
        monitor.popup_scrollbars = "yes";

        monitor.toHtml(pageContext);


        SmartCombo users= new SmartCombo("USERTOASSIGN", Operator.class,"loginName");
        users.separator="</td><td>";
        users.label="Give to an user the permission to maintain a language.  Choose the user";

      %>
    </td>
  </tr>
</table>
<hr>
<table>
  <tr>
    <td><% users.toHtml(pageContext);%></td>
    <td>language code</td><td> <input type ="text" size="2" id="LANG_TO_EDIT" name="LANG_TO_EDIT"> </td>
    <td><%new ButtonJS("assign","assignLangToUser();").toHtml(pageContext);%></td>
    <td>(empty to remove)</td>
  </tr>
</table>
<hr>
<table>
  <tr>

    <td><%
      Uploader issueFile = new Uploader("I18N_FILE", form, pageState);
      issueFile.label = "";
      issueFile.separator = "</td><td nowrap>";
      issueFile.size = 15;
      issueFile.treatAsAttachment = true;
      issueFile.toHtml(pageContext);

    %></td><td><%

      ButtonSubmit doUpload = new ButtonSubmit(form);
      doUpload.variationsFromForm.setCommand("I18N_FILE");
      doUpload.label = I18n.get("I18N_FILE");
      doUpload.toHtml(pageContext);
    %>
    </td>

    <td><%
      PageSeed exp = pageState.pageInThisFolder("i18nExport.jsp", request);
      exp.setPopup(true);
      exp.addClientEntry(pageState.getEntry("SEARCH_TEXT"));
      exp.addClientEntry(pageState.getEntry("SUSPECT"));
      exp.addClientEntry(pageState.getEntry("FROMDBONLY"));
      ButtonLink export= new ButtonLink(I18n.get("I18N_EXPORT"),exp);
      export.target="_blank";
      export.toHtml(pageContext);
    %>
    </td>
    <td width="20">&nbsp;</td><td><%
      TextField enabledLang= new TextField(I18n.get("I18N_ENABLED_LANGUAGES"),"ENABLED_LANGUAGES","</td><td>",30,false);
      enabledLang.toHtml(pageContext);
    %></td><td><%
    ButtonSubmit sEL = new ButtonSubmit(form);
    sEL.variationsFromForm.setCommand("I18N_SAVE_ENABLED_LANG");
    sEL.label = I18n.get("SAVE");
    sEL.toHtml(pageContext);
    %>
    </td>
  </tr>
</table>
<%

  modality.end(pageContext);

%>
<table border="0" cellpadding="2" cellspacing="0">
  <tr>
    <td><%
      ButtonSubmit reload = new ButtonSubmit(form);
      reload.variationsFromForm.setCommand(I18n.CMD_RELOAD);
      reload.label = I18n.get("I18N_RELOAD");
      reload.toHtml(pageContext);
    %></td>

    <td><%
      ButtonSubmit dump = new ButtonSubmit(form);
      dump.id="dump_btn";
      dump.variationsFromForm.setCommand(I18n.CMD_DUMP);
      dump.label = I18n.get("I18N_DUMP");
      dump.hasFocus=I18n.dumpNeeded;
      dump.toHtml(pageContext);
    %></td>

    <td>
      &nbsp;&nbsp;<%
      TextField tfNewLanguage = new TextField("TEXT", "", Fields.FORM_PREFIX + "ADD_NEW_LANGUAGE", "&nbsp;", 2, false);
      tfNewLanguage.toHtml(pageContext);

      ButtonSubmit addLang = new ButtonSubmit(form);
      addLang.variationsFromForm.setCommand(I18n.CMD_NEW_LANGUAGE);
      addLang.label = I18n.get("I18N_NEW_LANGUAGE");
    %></td>

    <td><%
      addLang.toHtml(pageContext);

      ButtonSubmit addEntry = new ButtonSubmit(form);
      addEntry.variationsFromForm.setCommand(I18n.CMD_NEW_ENTRY);
      addEntry.variationsFromForm.setHref("i18nEdit.jsp");
      addEntry.label = I18n.get("I18N_NEW_ENTRY");
    %></td>

    <td><%
      addEntry.toHtml(pageContext);

    %>
    </td>
  </tr>
</table>
<%


  Container boxSearch = new Container("bx_18se");
  boxSearch.title = I18n.get("SEARCH");
  boxSearch.start(pageContext);
%>
<table width="100%" border="0" cellpadding="2" cellspacing="0">
  <tr>
    <td>
      <%applicationsCombo.toHtml(pageContext);%>&nbsp;&nbsp;&nbsp;&nbsp;<%=I18n.get("AVAILABLE_LANGUAGES")%>:
      <%

        for (Iterator iterator = supportedLanguages.iterator(); iterator.hasNext();) {
          String lang = (String) iterator.next();

          boolean langEnabled=pageState.getEntryOrDefault("SHOW_LANGUAGES_" + lang).checkFieldValue();

          CheckField chbx = new CheckField("SHOW_LANGUAGES_" + lang, "&nbsp;",true);
          chbx.additionalOnclickScript  = "obj('" + form.id + "').submit();";
          chbx.label = lang;

          if (langEnabled) {
            filterForLanguages.add(lang);
          }
      %><%chbx.toHtml(pageContext);%>&nbsp;&nbsp;<%
      }

      if(filterForLanguages.size()<=0)
        filterForLanguages.addAll(supportedLanguages);

      %></td></tr><tr><td><%
      if (!ApplicationState.platformConfiguration.development) {
        %><%

        CheckField chbxFromDB = new CheckField("FROMDBONLY", "&nbsp;", false);
        chbxFromDB.label = I18n.get("I18N_FROMDBONLY");
        chbxFromDB.script = "onChange=\"obj('" + form.id + "').submit();\"";
        chbxFromDB.toHtml(pageContext);

      %>&nbsp;&nbsp;&nbsp;&nbsp;  <%

      }


      CheckField chbxSuspect = new CheckField("SUSPECT", "&nbsp;", false);
      chbxSuspect.label = I18n.get("SUSPECT");
      chbxSuspect.script = "onChange=\"obj('" + form.id + "').submit();\"";
      chbxSuspect.toHtml(pageContext);
    %>&nbsp;&nbsp;&nbsp;&nbsp;  <%

      CheckField chbxMissing = new CheckField("SEARCH_MISSING_IN_LANGUAGE", "&nbsp;", false);
      chbxMissing.label = I18n.get("SEARCH_MISSING_IN_LANGUAGE");
      chbxMissing.additionalOnclickScript = "checkForMissingLanguage();";
      chbxMissing.toHtml(pageContext);
    %>&nbsp;&nbsp;&nbsp;&nbsp;  <%

      TextField tfSearch = new TextField("TEXT", I18n.get("SEARCH"), "SEARCH_TEXT", "&nbsp;", 40, false);
      tfSearch.addKeyPressControl(13, "obj('" + form.id + "').submit();", "onkeyup");
      pageState.setFocusedObjectDomId(tfSearch.id);
      tfSearch.script = "";
      tfSearch.toHtml(pageContext);
      %>
    </td>
  </tr>
</table>


<%
  boxSearch.end(pageContext);


  } else {  //you are editing customization -> inject english
    filterForLanguages.clear();
    filterForLanguages.add("EN");
  }

  form.end(pageContext);


  // ------------------------------------------------------------------------------------  MAIN TABLE  START ----------------------------------------------

  if (searchEnabled) {
    int howMany=0;
    %>

    <style type="text/css">
      textarea.unselected {
        font-size:12px;
        width:100%;
        height:<%=customizationModality?"40":"20"%>px;
        overflow:hidden;
        border:none;
        background-color:<%=customizationModality?"white":"transparent"%>;
        cursor:pointer;

      }
      textarea.selected {
        height:90px;
        overflow:auto;
        background-color:white;
        cursor:default;
      }
      .cell{
        width:100%;
        height:100%;
        border:none;
        padding:3px;
        cursor:pointer;
        font-size:11px;
      }
      .transl{
        font-size:9px;
        font-weight:bold;
        color:blue;
        cursor:pointer;
      }

    </style>


    <table class="table dataTable" id="multi">
    <thead>
      <th class="tableHead">&nbsp;</th>
      <th class="tableHead" id="thCode">code</th>

      <th class="tableHead"> application</th>
      <%
        for (String lang:filterForLanguages){
            %> <th class="tableHead"><%=lang%> </th> <%
        }

      if (!customizationModality) {
        %><th class="tableHead">&nbsp;</th><%
      }
      %>
    </thead>
    <%

    // load from db
    Set<String> entryFromDB = new HashSet();
    if (searchFromDBOnlyEnabled) {
      List<I18nEntryPersistent> en = new OqlQuery("from " + I18nEntryPersistent.class.getName()).list();
      for (I18nEntryPersistent i : en)
        entryFromDB.add(i.getCode());
    }

    int taCounter=1;

    for (Iterator iterator = entries.keySet().iterator(); iterator.hasNext();) {
      String code = (String) iterator.next();

      I18n.I18nEntry i18nEntry = (I18n.I18nEntry) (entries.get(code));

      if (searchTextEnabled && !i18nEntry.matches(searchText,filterForLanguages))
        continue;

      if (suspectSearchEnabled && !i18nEntry.isSuspect())
        continue;

      if (searchFromDBOnlyEnabled && !entryFromDB.contains(i18nEntry.code))
        continue;


      //nasconde i xxx_CUSTOM_FIELD_n
      if (code.matches(".*_CUSTOM_FIELD_\\d"))
        continue;

      //nasconde i CUSTOM_EXPORT_EXCEL_xxx
      if (code.matches("CUSTOM_EXPORT_EXCEL_.*"))
        continue;

      // if search for missing skip if all is complete (for selected languages)
      boolean filterForMissingInLanguage = pageState.getEntry("SEARCH_MISSING_IN_LANGUAGE").checkFieldValue();
      if (filterForMissingInLanguage && JSP.ex(focusedApplicationName)){
        boolean existsAll=true;
        boolean existsAtLeastOne=false;

        for (String lang:filterForLanguages ){
            // check if it exists
            if (!JSP.ex(I18n.getRawLabel(code,focusedApplicationName,lang))){
              existsAll=false;
              //break;
            } else {
              existsAtLeastOne=true;
            }
          }
        //}
        if (existsAll || !existsAtLeastOne)
          continue;
      }


      ButtonJS bsEdit =new ButtonJS("editEntry(this)");
      bsEdit.label = "";
      bsEdit.iconChar = "e";


      String tdStyle = "";
      if (!i18nEntry.isSeen() && catchState){
        tdStyle = "border:1px solid red;";
      }

      if (i18nEntry.isSuspect())
        tdStyle = tdStyle+"background-color:#ff6060;";

      for (Iterator iterator1 = i18nEntry.applicationEntries.keySet().iterator(); iterator1.hasNext();) {
        String applicationName = (String) iterator1.next();

        I18n.ApplicationEntry applicationEntry = i18nEntry.applicationEntries.get(applicationName);

        %>
        <tr class="alternate labelRow"  id="tr$$$$<%=applicationName%>$$$$<%=i18nEntry.code%>" an="<%=applicationName%>">
          <td valign="top"  width="1%"><%if(!customizationModality) bsEdit.toHtmlInTextOnlyModality(pageContext);%></td>
          <td class="lblCode"  style="<%=tdStyle%>" valign="top" nowrap width="<%=featureCustomization?"50":"20"%>%"><%

            pageState.addClientEntry("code"+taCounter,code);

            TextField codetf=new TextField("","code"+taCounter,"",2,false);
            codetf.fieldClass="cell";
            codetf.label="";
            codetf.preserveOldValue=true;
            codetf.readOnly = customizationModality;
            codetf.toHtml(pageContext);
            %></td>
          <td  valign="top" width="1%" nowrap class="appl"><%=applicationName%></td>
          <%
            for (String lang:filterForLanguages ){

              pageState.addClientEntry("ta"+taCounter,JSP.w(applicationEntry.entries.get(lang)));
              TextArea ta= new TextArea("ta"+taCounter,"",30,1,"labelta unselected");
              ta.label="*"; // per non far vedere *
              ta.showLabel=false;
              ta.preserveOldValue=true;
              ta.required=featureCustomization; // non si possono cancellare altrimenti spariscono
              ta.script=" lang=\""+lang+"\"";

              %><td  valign="top" cmenu='cm' class="lblValue"><%ta.toHtml(pageContext);%></td><%
            taCounter++;
          }

          if (!customizationModality) {
          %><td  valign="top"  width="1%" nowrap align="center">
          <span class="teamworkIcon" style="cursor: pointer" onclick="deleteLabel(this,'<%=i18nEntry.code%>','<%=applicationName%>')">d</span>
        </td>
          <%}
    %>
        </tr>
        <%

      }

      howMany++;
          if (howMany> MAXROWS){
        %><tr><td colspan="9">Result limited to <%=MAXROWS%> elements. Refine your search.</td></tr><%
        break;
      }
    }
  %></table><%

    // ------------------------------------------------------------------------------------  MAIN TABLE  END ----------------------------------------------

  }

  %></div><%

  // hack to temporary disable i18nedit
  i18nManager.setEditStatus(old_status);
  i18nManager.catchUsedLabels = catchState;





%>
<div id="jst" style="display: none;">
<%=JST.start("TRANSLBUTT")%>
<div class="translButt">
  <%
    for (String lang:filterForLanguages ){
      %><span class="transl" lang="<%=lang%>" onclick="translateSentence($(this),'<%=lang%>')" style="(#=lang=="<%=lang%>"?"display:none;":""#)"><%=lang%></span>&nbsp;&nbsp;<%
  }
%>
</div>
<%=JST.end()%>
</div>

<script type="text/javascript">

  $(document).ready(function () {
    //load templates
    $("#jst").loadTemplates().remove();


    $("#multi").find("textarea.labelta").bind("focus", taOnFocus).bind("blur", taOnBlur);
    $("#multi").find("input.cell").bind("blur",renameCode);

    //inject the table search
    createTableFilterElement($("#thCode"),"#multi tr.labelRow","td.lblCode,td.lblValue");


  });


  function deleteLabel(el,label,appname) {
    showSavingMessage();
    $.get("i18nAjaxController.jsp",{CM:"<%=I18n.CMD_REMOVE_LABEL%>",CODE:label,APPNAME:appname},function(){
      $(el).closest('tr').remove();
      hideSavingMessage();
    })
    //eval(getContent("i18nAjaxController.jsp", "<%=Commands.COMMAND%>=<%=I18n.CMD_REMOVE_LABEL%>&CODE=" + label + "&APPNAME="+appname));
    //$('#tr$$$$'+appname+'$$$$'+label).remove();
  }


  function taOnFocus() {
    $(".translButt").remove();
    var ta = $(this);
    var tr = ta.parents("tr").eq(0);
    if (!ta.hasClass("selected")) {
      $("textarea.labelta.selected").removeClass("selected");
      tr.find("textarea.labelta").addClass("selected");
    }

    <%if (ApplicationState.platformConfiguration.development) {%>
    var lang=ta.attr("lang");
    var btn=$.JST.createFromTemplate({lang:lang},"TRANSLBUTT");
    ta.after(btn);
    <%}%>
  }

  function taOnBlur() {
    var ta = $(this);
    saveTA(ta);
    //ta.parent().find(".translButt").remove();
  }


  function saveTA(ta){
    var tr=ta.parents("tr").eq(0);
    //if (ta.isValueChanged() && ta.isFullfilled()) {
    if (ta.isValueChanged() && ta.val()!="") {
      showSavingMessage();
      // get label
      var code=tr.find("input.cell").val();
      var lang=ta.attr("lang");
      var appl=tr.find("td.appl").html();
      var label=ta.val();
      var queryString ="code="+encodeURIComponent(code)+"&lang="+encodeURIComponent(lang)+"&appl="+encodeURIComponent(appl)+"&label="+encodeURIComponent(label);
      $.getJSON("i18nAjaxController.jsp", "<%=Commands.COMMAND%>=<%=Commands.SAVE%>&" + queryString,function(response){
        jsonResponseHandling(response);
        if (response.ok){
          ta.updateOldValue();
          showFeedbackMessage("OK","Saved correctly");

          if (response.mustDump)
            $("#dump_btn").addClass("focused");
        }
        hideSavingMessage();
      });       
    }
  }

  function renameCode() {
    var inp = $(this);

    if (inp.isValueChanged()) {
      showSavingMessage();
      var oldValue = inp.getOldValue();
      var tr = inp.parents("tr").eq(0);
      var appl = tr.find("td.appl").html()

      var queryString = "newcode=" + encodeURIComponent(inp.val()) + "&oldcode=" + encodeURIComponent(oldValue) + "&appl=" + encodeURIComponent(appl);
      $.getJSON("i18nAjaxController.jsp", "<%=Commands.COMMAND%>=RENAME&" + queryString, function(response) {
        jsonResponseHandling(response);
        if (response.ok) {
          // reset values on the other with the same code
          $("#multi").find("input.cell[value=" + oldValue + "]").each(function() {
            $(this).val(inp.val());
          })

          // set old values for the whole family
          $("#multi").find("input.cell[value=" + inp.val() + "]").updateOldValue();
          if (response.mustDump)
            $("#dump_btn").addClass("focused");
        }
        hideSavingMessage();

      });

    }
  }

  function editEntry(but){
    var butt=$(but);
    var tr=butt.parents("tr").eq(0);
    var code=tr.find("input.cell").val();    
    window.location="i18nEdit.jsp?<%=Commands.COMMAND%>=<%=Commands.EDIT%>&<%=Fields.FORM_PREFIX%>code="+encodeURIComponent(code)+"&appTabSet=appTabSet"+tr.attr("an");
  }


  function checkForMissingLanguage(){
    if ($("#<%=Fields.FORM_PREFIX%>APPLICATION").val()=="" && $("#SEARCH_MISSING_IN_LANGUAGE").val()=="<%=Fields.TRUE%>"){
      alert ("<%=I18n.get("MISSING_IN_LANGUAGE_CHOOSE_APPLICATION")%>");
    }

  }

  function assignLangToUser(){
    var queryString ="userId="+$("#USERTOASSIGN").val()+"&lang="+encodeURIComponent($("#LANG_TO_EDIT").val());
    $.get("i18nAjaxController.jsp", "<%=Commands.COMMAND%>=ASSLANG&" + queryString);
  }

</script>

<%   if (ApplicationState.platformConfiguration.development) {%>
<script type="text/javascript">


  function translateSentence(el,lang) {
    var start = $(el).parents("td:first").find("textarea");
    if (start.val()) {
      var apiurl = "https://www.googleapis.com/language/translate/v2";
      var data = {
        key:"<%=ApplicationState.applicationSettings.get("GOOGLE_TRANSLATE_API_KEY")%>",
        q:start.val(),
        source:start.attr("lang"),
        target:lang
      };

      $.ajax({
        url: apiurl,
        data:data,
        dataType: 'jsonp',
        success: function (ret) {
          if (!ret || !ret.data || !ret.data.translations)
            console.error(ret);
          var transl = ret.data.translations[0].translatedText;
          var dest = start.parents("tr:first").find("textarea[lang="+lang+"]");
          dest.val(dest.val() + (dest.val()!=""?"\n":"") + transl).blur();
        }
      });
    }
  }

</script><%
  }
%>

