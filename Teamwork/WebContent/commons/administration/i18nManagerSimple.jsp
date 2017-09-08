<%@ page import="org.jblooming.operator.Operator,
                 org.jblooming.utilities.CodeValueList,
                 org.jblooming.utilities.JSP,org.jblooming.utilities.StringUtilities,
                 org.jblooming.waf.ScreenBasic,
                 org.jblooming.waf.constants.Fields,
                 org.jblooming.waf.html.button.ButtonJS,
                 org.jblooming.waf.html.container.Container,
                 org.jblooming.waf.html.input.CheckField,
                 org.jblooming.waf.html.input.TextArea,
                 org.jblooming.waf.html.input.TextField,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.Application,
                 org.jblooming.waf.settings.ApplicationState,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.settings.businessLogic.I18nController,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState,
                 java.util.Iterator,
                 java.util.Map, java.util.Set, java.util.SortedMap"%>
 <%--
Jsp of the Open Lab JBlooming development platform - www.jblooming.org
--%>
<%
PageState pageState = PageState.getCurrentPageState(request);

if (!pageState.screenRunning) {

 ScreenBasic.preparePage(new I18nController(),pageContext);
 pageState.perform(request, response).toHtml(pageContext);

} else {

  // number of rows displayed
  int MAXROWS = 200;

  Operator loggedUser = pageState.getLoggedOperator();


  String langToBeFilled=loggedUser.getOption("CAN_EDIT_THIS_LANGUAGE"); // NO fallback!
  if (!JSP.ex(langToBeFilled)){
    if (loggedUser.hasPermissionAsAdmin())
      langToBeFilled="XX"; // only to test application
    else{
      %><h1>No language permissions defined!</h1><%
      return; // no special right - no admin iatavenne!
    }
  }

  String langUserInterface=I18n.getLocale(loggedUser.getLanguage()).getLanguage().toUpperCase(); // fallback on appl default

  if (langUserInterface.equalsIgnoreCase(langToBeFilled))
    langUserInterface="EN";  // default for english -> you can set user lang as xx while translating xx

  boolean requiresENColumn=!(langUserInterface.equalsIgnoreCase("EN")||langToBeFilled.equalsIgnoreCase("EN"));


  I18n i18nManager = ApplicationState.i18n;

  Container box = new Container("bx_18li");
  box.title = I18n.get("I18N_MANAGER");
  box.width = "100%";
  box.start(pageContext);

  PageSeed url = pageState.thisPage(request);

  Form form = new Form(url);
  form.encType = Form.MULTIPART_FORM_DATA;

  Map applics = ApplicationState.platformConfiguration.applications;

  CodeValueList cvl = new CodeValueList();
  cvl.add("", "-- all --");
  for (Iterator iterator = applics.keySet().iterator(); iterator.hasNext();) {
    String s = (String) iterator.next();
    Application app = (Application) applics.get(s);
    cvl.add(app.getName(), app.getName());
  }

  Set<String> filterForLanguages= StringUtilities.splitToSet(langUserInterface+","+langToBeFilled,",");

  SortedMap<String, I18n.I18nEntry> entries;
  entries = i18nManager.codeEntries;

  String searchText = pageState.getEntry("SEARCH_TEXT").stringValueNullIfEmpty();
  boolean searchTextEnabled = JSP.ex(searchText);

  form.start(pageContext);

  Container boxSearch = new Container("bx_18se");
  boxSearch.title = I18n.get("SEARCH");

  boxSearch.start(pageContext);
%>
<table width="100%" border="0" cellpadding="2" cellspacing="0">
  <tr>
    <td>
      <%
      boolean filterForMissingInLanguage = pageState.getEntry("SEARCH_MISSING_IN_LANGUAGE").checkFieldValue();
      CheckField chbxMissing = new CheckField("SEARCH_MISSING_IN_LANGUAGE", "&nbsp;", false);
      chbxMissing.label = I18n.get("SEARCH_MISSING_IN_LANGUAGE");
      chbxMissing.additionalOnclickScript = "checkForMissingLanguage();";
      chbxMissing.putLabelFirst = true;
      chbxMissing.toHtml(pageContext);
    %>&nbsp;&nbsp;&nbsp;&nbsp;  <%

      TextField tfSearch = new TextField("TEXT", I18n.get("SEARCH"), "SEARCH_TEXT", "&nbsp;", 20, false);
      tfSearch.addKeyPressControl(13, "obj('" + form.id + "').submit();", "onkeyup");
      pageState.setFocusedObjectDomId(tfSearch.id);
      tfSearch.script = "";
      tfSearch.toHtml(pageContext);
     
      new ButtonJS("SEARCH","obj('" + form.id + "').submit();").toHtmlI18n(pageContext);
    %>
    </td>
  </tr>
</table>


<%
  boxSearch.end(pageContext);

  form.end(pageContext);


  // ------------------------------------------------------------------------------------  MAIN TABLE  START ----------------------------------------------


    int howMany=0;
    %>

    <style type="text/css">

      .labelTR td {
        border:1px solid #eee;
      }
      textarea.unselected {
        font-size:12px;
        width:100%;
        height:30px;;
        overflow:hidden;
        border:none;
        background-color:transparent;
        cursor:pointer;
      }
      textarea.selected {
        height:120px;
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
        font-size:7px;
        font-weight:bold;
        color:blue;
        cursor:pointer;
      }

    </style>


    <table  id="multi" cellspacing="0" cellpadding="3">
    <tr>
      <th>code</th>

      <th> application</th>
      <%if (requiresENColumn){%>
        <th>EN</th>
      <%}%>
      <th><%=langUserInterface%> </th>
      <th><%=langToBeFilled%> </th>
    </tr>
    <%

    int taCounter=1;

    for ( I18n.I18nEntry i18nEntry :entries.values()) {

      if (searchTextEnabled && !i18nEntry.matches(searchText,filterForLanguages))
        continue;

      // if search for missing skip if is complete in langToBeFilled fro all apps
      if (filterForMissingInLanguage){
        boolean existForAllApps=true;
        for (I18n.ApplicationEntry applicationEntry:i18nEntry.applicationEntries.values()){
          if (JSP.ex(applicationEntry.entries.get("EN")) && JSP.ex(applicationEntry.entries.get(langUserInterface)) && !JSP.ex(applicationEntry.entries.get(langToBeFilled))){
            existForAllApps=false;
          }
        }
        if (existForAllApps)
          continue;
      }


      for (String appName: i18nEntry.applicationEntries.keySet()) {
        I18n.ApplicationEntry applicationEntry = i18nEntry.applicationEntries.get(appName);

        %>
        <tr id="tr$$$$<%=appName%>$$$$<%=i18nEntry.code%>" an="<%=appName%>" class="labelTR">
          <td valign="top" nowrap width="10%" class="code">
            <%=JSP.w(i18nEntry.code)%>
          </td>
          <td  valign="top" width="1%" nowrap class="appl"><%=appName%></td>
          <%if (requiresENColumn){%>
            <td  valign="top" cmenu='cm'>
                <div class="orig" lang="EN"><%=JSP.w(applicationEntry.entries.get("EN"))%></div>
                <span class="transl">translate</span>&nbsp;&nbsp;
            </td>
          <%}%>

          <td  valign="top" cmenu='cm'>
              <div class="orig" lang="<%=langUserInterface%>"><%=JSP.w(applicationEntry.entries.get(langUserInterface))%></div>
              <span class="transl">translate</span>&nbsp;&nbsp;
          </td><td width="50%" valign="top"><%

              pageState.addClientEntry("ta"+taCounter,JSP.w(applicationEntry.entries.get(langToBeFilled)));
              TextArea ta= new TextArea("ta"+taCounter,"",30,1,"labelta unselected");
              ta.label="";
              ta.preserveOldValue=false;
              ta.script=" lang=\""+ langToBeFilled +"\"";

              ta.toHtml(pageContext);
              taCounter++;
          %></td>
        </tr>
        <%
      }

      howMany++;
      if (howMany> MAXROWS){
        %><tr><td colspan="9" style="color:#DB2727;">Result limited to <%=MAXROWS%> elements. Refine your search.</td></tr><%
        break;
      }
    

  }
  %></table><%

    // ------------------------------------------------------------------------------------  MAIN TABLE  END ----------------------------------------------



  box.end(pageContext);

%>
<script type="text/javascript" src="http://www.google.com/jsapi"></script>
<script type="text/javascript">
  var lastTd;
  $(document).ready(function () {
    $("#multi").find("textarea.labelta").bind("focus", taOnFocus).bind("blur", saveTA);

      $("span.transl").bind("click",function(){
      translateSentence($(this),"<%=langToBeFilled%>");

    });

  });


  function taOnFocus() {
    var ta = $(this);
    ta.updateOldValue();
    var tr = ta.parents("tr").eq(0);
    if (!ta.hasClass("selected")) {
      $("textarea.labelta.selected").removeClass("selected");
      tr.find("textarea.labelta").addClass("selected");
    }
  }

  function saveTA(){
    var ta = $(this);
    var tr=ta.parents("tr").eq(0);
    if (ta.isValueChanged()){
      showSavingMessage();
      // get label
      var code=tr.find("td.code").text();
      var lang=ta.attr("lang");
      var appl=tr.find("td.appl").html();
      var label=ta.val();

      var req ={CM:"SVSMPL",code:code,lang:lang,appl:appl,label:label};

      var error = false;
      var message = "";
      var mustDump=false;
      $.getJSON("i18nAjaxController.jsp",req,function(response){
        jsonResponseHandling(response);
        if(response.ok==true){
          ta.updateOldValue();
        }

        hideSavingMessage();        
      });

    }
  }

  function checkForMissingLanguage(){
    if ($("#<%=Fields.FORM_PREFIX%>APPLICATION").val()=="" && $("#SEARCH_MISSING_IN_LANGUAGE").val()=="<%=Fields.TRUE%>"){
      alert ("<%=I18n.get("MISSING_IN_LANGUAGE_CHOOSE_APPLICATION")%>");
    }
  }


  function translateSentence(el,lang) {
    var start = $(el).parents("td:first").find("div.orig");
    if (start.html()) {
      var apiurl = "https://www.googleapis.com/language/translate/v2";
      var data = {
        key:"<%=ApplicationState.applicationSettings.get("GOOGLE_TRANSLATE_API_KEY")%>",
        q:start.html(),
        source:start.attr("lang"),
        target:lang
      };

      $.ajax({
        url: apiurl,
        data:data,
        dataType: 'jsonp',
        success: function (ret) {
          //console.debug(ret)
          if (!ret || !ret.data || !ret.data.translations)
            console.error(ret);
          var transl = ret.data.translations[0].translatedText;
          var dest = start.parents("tr:first").find("textarea[lang="+lang+"]");
          dest.val(dest.val() + (dest.val()!=""?"\n":"") + transl).blur();
        }
      });
    }
  }

</script>
<%
  }
%>
