<%@ page import="org.jblooming.operator.Operator,
                 org.jblooming.security.PlatformPermissions,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.constants.Fields,
                 org.jblooming.waf.html.button.ButtonJS,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.container.ButtonBar,
                 org.jblooming.waf.html.container.Tab,
                 org.jblooming.waf.html.container.TabSet,
                 org.jblooming.waf.html.input.TextArea,
                 org.jblooming.waf.html.input.TextField,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.Application,
                 org.jblooming.waf.settings.ApplicationState,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.settings.businessLogic.I18nAction,
                 org.jblooming.waf.settings.businessLogic.I18nController,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState,
                 java.util.Iterator,
                 java.util.Map,
                 java.util.Set"%><%

  I18nController i18nController=new I18nController();
  PageState pageState = PageState.getCurrentPageState(request);
  Operator loggedUser=pageState.getLoggedOperator();
  i18nController.perform(request,response);

  I18n i18nManager= ApplicationState.i18n;

  if (loggedUser!=null && loggedUser.hasPermissionFor(PlatformPermissions.i18n_manage)) {
    PageSeed url=pageState.thisPage(request);

    url.setCommand(Commands.FIND);
    if (pageState.getEntry(Fields.APPLICATION_NAME).stringValueNullIfEmpty() != null)
      url.addClientEntry(Fields.APPLICATION_NAME,pageState.getEntry(Fields.APPLICATION_NAME).stringValueNullIfEmpty());
    url.addClientEntry(pageState.getEntry(Fields.FORM_PREFIX+"SEARCH_TEXT"));
    url.addClientEntry(pageState.getEntry(Fields.FORM_PREFIX+"APPLICATION"));

    for (Iterator iterator = pageState.getClientEntries().getEntryKeys().iterator(); iterator.hasNext();) {
      String key   = (String) iterator.next();
      if (key.startsWith(Fields.FORM_PREFIX+"LANGUAGES"))
        url.addClientEntry(pageState.getEntry(key));
    }

    Form form = new Form(url);
    form.alertOnChange=true;

    Set<String> supportedLanguages=i18nManager.supportedLanguages;
    Map<String, Application> applics = ApplicationState.platformConfiguration.applications;

  form.start(pageContext);
%>
<style type="text/css">
  .elLabel{
    font-size:large;
    font-weight:bold;
  }
  .hl td{
    background-color: #FBFBD4 !important;
  }
  .transl td{
    padding-bottom: 10px;
  }
</style>
<script>$("#ADMINISTRATION_ROOT_MENU").addClass('selected');</script>
<h1><%=I18n.get("I18N_MANAGER")%></h1>
<div class="container level_2" id="bx_18li">
<table width="100%" border="0" cellpadding="2" cellspacing="0">
  <tr><td><%
    boolean readOnly=true;
    if (pageState.getEntry(Fields.FORM_PREFIX+"code").stringValueNullIfEmpty()==null)
      readOnly=false;

    readOnly=false;
    TextField tf_i18nKey = new TextField("text", Fields.FORM_PREFIX+"code","</td><td align=\"left\">",60,readOnly);
    tf_i18nKey.label="entry";
    tf_i18nKey.preserveOldValue=false;

   tf_i18nKey.toHtml(pageContext);

    %>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<%

    ButtonJS bsFind =new ButtonJS("findEntry()");
    bsFind.label=I18n.get("go to list and find");
    bsFind.toHtml(pageContext);

    ButtonLink addEntry = new ButtonLink(pageState.pageInThisFolder("i18nEdit.jsp",request));
    addEntry.pageSeed.setCommand(I18n.CMD_NEW_ENTRY);
    addEntry.label = I18n.get("I18N_NEW_ENTRY");
    addEntry.toHtml(pageContext);

    ButtonLink list = new ButtonLink(pageState.pageInThisFolder("i18nManager.jsp",request));
    list.label = I18n.get("LIST");
    list.toHtml(pageContext);

    ButtonSubmit save= ButtonSubmit.getSaveInstance(form,I18n.get("SAVE"));
    save.toHtml(pageContext);


  %>




  </td></tr>
  </table><br><%

  TabSet tabSet=new TabSet("appTabSet",pageState);
  for (Application appl: applics.values()){
    Tab tab = new Tab(appl.getName(), appl.getName());
    if (appl.getName().equalsIgnoreCase(pageState.getApplication().getName()))
      tab.focused=true;

    tabSet.addTab(tab);
  }

  tabSet.drawBar(pageContext);

  String sysLang = ApplicationState.SYSTEM_LOCALE.getLanguage().toUpperCase();
  for (Iterator iterator = applics.keySet().iterator(); iterator.hasNext();) {
    String s = (String)iterator.next();
    Application applic = (Application)applics.get(s);
    String appname = applic.getName();

    Tab tab=tabSet.getTab(appname);
    tab.start(pageContext);

    boolean swap=true;
    %><table class="table"><tr >
      <td valign="top" width="15%" class="elLabel" ><%=appname%></td>
      <td><table class="table"><%

      for (Iterator iterator1 = supportedLanguages.iterator(); iterator1.hasNext();) {
        String lang = (String) iterator1.next();

        TextArea textArea = new TextArea(Fields.FORM_PREFIX+ I18nAction.SEPARATOR +appname+I18nAction.SEPARATOR+lang,"",60,4,null) ;
        textArea.label = "";
        textArea.script=" style=\"width:100%;\"";
        %><tr class="alternate <%=sysLang.equals(lang)?"hl":""%>"><td align="right" valign="top" class="elLabel" ><%=lang%></td>
              <td><%textArea.toHtml(pageContext);%></td>
              </tr>

           <%if (ApplicationState.platformConfiguration.development) {%>
        <tr class="alternate <%=sysLang.equals(lang)?"hl":""%> transl">
              <td valign="top" align="right">from <%=lang%> to:</td>
              <td><%

              for (String trnslLang:supportedLanguages){
                if (!lang.equals(trnslLang)){
                  String js= "translateSentence('"+appname+"','"+lang+"','"+trnslLang+"');";
                  ButtonJS bjs=new ButtonJS(js);
                  bjs.label=trnslLang;
                  bjs.toHtmlInTextOnlyModality(pageContext);
                  %>&nbsp;&nbsp;&nbsp;&nbsp;<%
                }
              }
              %></td></tr><%
        }
          }

   %></table></td></tr></table><%
    tab.end(pageContext);
  }

  tabSet.end(pageContext);
%>
<table class="table"><tr>
  <%   if (ApplicationState.platformConfiguration.development) {%>
  <td width="15%">Translations powered by:<br><a href="http://translate.google.com"><img src="http://www.google.com/intl/en/images/translate_beta_res.gif" border="0" alt="Google translate"></a></td>
  <%}%>
  <td valign="bottom">Use "%%" to insert parameters</td>
   </tr></table>
<%

  ButtonBar bb = new ButtonBar();

  save.additionalCssClass="big first";
  bb.addButton(save);

  PageSeed psList = pageState.pageInThisFolder("i18nManager.jsp",request);
  ButtonLink bl = new ButtonLink(I18n.get("RETURN_TO_LIST"), psList);
  bl.additionalCssClass="big";
  bb.addButton(bl);

  addEntry.additionalCssClass="big";
  bb.addButton(addEntry);

  bb.toHtml(pageContext);

    form.end(pageContext);
  } else {
  %>NO RIGHTs<%
  }


%></div>
<script type="text/javascript">

  function findEntry(){
    var code=$("#<%=Fields.FORM_PREFIX%>code").val();
    window.location="i18nManager.jsp?<%=Commands.COMMAND%>=<%=Commands.FIND%>&SEARCH_TEXT="+encodeURIComponent(code);
  }
  


</script>


<%   if (ApplicationState.platformConfiguration.development) {%>
<script type="text/javascript">


  function translateSentence(application, from, to) {
    var start = $("#<%=Fields.FORM_PREFIX+ I18nAction.SEPARATOR%>" + application + "<%=I18nAction.SEPARATOR%>" + from);
    //console.debug("translate", start.val(), application, from, to)
    if (start.val()) {

      var apiurl = "https://www.googleapis.com/language/translate/v2";
      var data = {
        key:"<%=ApplicationState.applicationSettings.get("GOOGLE_TRANSLATE_API_KEY")%>",
        q:start.val(),
        source:from,
        target:to
      };

      $.ajax({
        url: apiurl,
        data:data,
        dataType: 'jsonp',
        success: function (ret) {
          //console.debug(ret);
          if (!ret || !ret.data || !ret.data.translations)
            console.error(ret);
          var transl = ret.data.translations[0].translatedText;
          var dest = $("#<%=Fields.FORM_PREFIX+ I18nAction.SEPARATOR%>" + application + "<%=I18nAction.SEPARATOR%>" + to);
          dest.val(dest.val() + (dest.val() != "" ? "\n" : "") + transl);
        }
      });
    }
  }


</script>
<%}%>
