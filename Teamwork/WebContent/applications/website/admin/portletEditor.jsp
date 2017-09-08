<%@ page import="com.opnlb.website.portlet.Portlet,
                 com.opnlb.website.portlet.businessLogic.PortletController,
                 com.opnlb.website.security.WebSitePermissions,
                 org.jblooming.operator.Operator,
                 org.jblooming.waf.ScreenBasic,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.container.ButtonBar,
                 org.jblooming.waf.html.container.Tab,
                 org.jblooming.waf.html.display.DeletePreviewer,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState,
                 java.util.List, org.jblooming.utilities.CodeValueList, java.io.File, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.constants.Fields, com.opnlb.website.content.Content, org.jblooming.oql.QueryHelper, com.opnlb.website.page.WebSitePage, org.jblooming.utilities.JSP, org.jblooming.waf.html.input.*, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.button.ButtonSupport"%><%@ page pageEncoding="UTF-8" %><%

  PageState pageState = PageState.getCurrentPageState(request);

  //verify permissions
  Operator logged = pageState.getLoggedOperator();
  if (logged == null || !logged.hasPermissionFor(WebSitePermissions.portlet_canManage))
    throw new SecurityException(I18n.get("PERMISSION_LACKING"));

  if (!pageState.screenRunning) {
    ScreenBasic.preparePage(new PortletController(), pageContext);
    pageState.perform(request, response).toHtml(pageContext);

  } else {
    Portlet portlet = (Portlet) pageState.getMainObject();

    PageSeed self = pageState.thisPage(request);
    self.mainObjectId = portlet.getId();
    self.setCommand(Commands.SAVE);
    Form form = new Form(self);
    form.encType = Form.MULTIPART_FORM_DATA;
    form.usePost = true;


    form.start(pageContext);

    PageSeed list = new PageSeed("portletList.jsp");
    list.setCommand(Commands.FIND);
    ButtonLink lista = new ButtonLink(list);
    lista.label = I18n.get("PORTLETS");

%><script>$("#HOME_MENU").addClass('selected');</script>
<h1><%lista.toHtmlInTextOnlyModality(pageContext);%> / <%=(portlet.getName()!=null ? portlet.getName() : "...")%></h1>

<%


  //----------------------------------------------------------- GENERAL DATA --------------------------------------------------------------------------
%>
  <table class="table" border="0">
  <tr>
  <td valign="top">
  <table class="table" border="0">
  <tr>
  <td valign="top"><%

  TextField tfName = new TextField("TEXT", I18n.get("NAME"), "NAME", "<br>", 30, false);
  tfName.required = true;
  tfName.toHtml(pageContext);

%><small><%=portlet.isNew() ? "" : "(id:&nbsp;"+ portlet.getId()+")"%></small></td>
<td align="left" valign="top"><%

  TextField tfDesc = new TextField("TEXT", I18n.get("DESCRIPTION"), "DESCRIPTION", "<br>", 30, false);
  tfDesc.toHtml(pageContext);

%></td>
<td valign="top" id="uploaderLabel"><%


  CodeValueList cvlTempl = new CodeValueList();
  cvlTempl.addChoose(pageState);
  for (File templFile : Portlet.getCandidateFiles()) {
    cvlTempl.add(templFile.getCanonicalPath().substring(ApplicationState.webAppFileSystemRootPath.length()+1).replace('\\','/'),templFile.getName());
  }



  Combo cbfile = new Combo("PORTLET_FILE", "<br>", null, 50, cvlTempl, null);
  cbfile.label = I18n.get("PORTLET_FILE");
  String jspName = pageState.getEntry("PORTLET_FILE").stringValueNullIfEmpty();
  cbfile.setJsOnChange = "reloadParamsAndScript($(this).val())";
  if (portlet.isNew())
    cbfile.required = true;

  cbfile.toHtml(pageContext);

%><br><i>Application will test whether the portlet has a param file - if not, a non fatal exception will be raised.</i>")<br>


</td>
</tr>
<tr>
  <td><%=I18n.get("INSTALLED")%>&nbsp;&nbsp;<%

    if (pageState.getEntry("INSTALLED").stringValue()==null)
      pageState.addClientEntry("INSTALLED", Fields.TRUE);

    RadioButton rb = new RadioButton(I18n.get("YES"),"INSTALLED",Fields.TRUE,"&nbsp;",null,false, "");
    rb.toHtml(pageContext);

    rb = new RadioButton(I18n.get("NO"),"INSTALLED", Fields.FALSE,"&nbsp;",null,false, "");
    rb.toHtml(pageContext);

  %></td>
  <%


    // when creating a new object only upload is possible
    boolean inWriteMode=false;
    if (!portlet.isNew()) {
  %><td><%=I18n.get("CHOOSE_MODALITY")%>&nbsp;&nbsp;<%

  if (pageState.getEntry("CHOOSER").stringValueNullIfEmpty()==null) {
    pageState.addClientEntry("CHOOSER", Fields.TRUE);
  }
  inWriteMode=!pageState.getEntry("CHOOSER").checkFieldValue();

  RadioButton chooser = new RadioButton(I18n.get("UPLOAD"),"CHOOSER",Fields.TRUE,"&nbsp;",null,false, "");
  chooser.script = " $('#PORTLET_TEXT').hide();";
  chooser.toHtml(pageContext);

  chooser = new RadioButton(I18n.get("WRITE_TEXT"),"CHOOSER", Fields.FALSE,"&nbsp;",null,false, "");
  chooser.script = " $('#PORTLET_TEXT').show(); ";
  chooser.toHtml(pageContext);

%></td><%
} else {
%><td colspan="4">&nbsp;</td><%
  }

%></tr>
</table>




<table class="table" border="0" cellspacing="0" cellpadding="2">
  <tr>

  </tr>
  <tr><td><div id="PORTLET_PARAMS"></div></td></tr>
</table>
</td>
</tr>

</table>
<table border="0" class="table">
  <tr>
    <td>
      <table width="90%" border="0" align="center" cellpadding="0" cellspacing="0">
        <tr>
          <td><%
            TextArea ta = new TextArea("", "PORTLET_TEXT", "", 140, 40, "");
            ta.preserveOldValue = false;
            ta.maxlength=0;
            ta.script = "style=\"width:100%; height:100%; display:" + (inWriteMode?"block":"none")+";\"";
            ta.toHtml(pageContext);
          %></td>
        </tr>
      </table>
    </td>
  </tr>
</table><%

  //----------------------------------------------------------- USED IN PAGES DATA --------------------------------------------------------------------------

  if (!portlet.isNew()) {
    String ql = " select distinct c.page from " + Content.class.getName() + " as c ";
    QueryHelper qh = new QueryHelper(ql);
    qh.addOQLClause(" c.portlet = :pt", "pt", portlet);
    List<WebSitePage> pages = qh.toHql().list();

    if (JSP.ex(pages)) {
%><br><br><b><%=I18n.get("USED_IN")%>: </b>&nbsp;<%
  for (int i = 0; i < pages.size(); i++) {
    WebSitePage wsPage = pages.get(i);
    PageSeed ps = new PageSeed(request.getContextPath() + "/applications/website/admin/pageEditor.jsp");
    ps.setCommand(Commands.EDIT);
    ps.setMainObjectId(wsPage.getId());
    ButtonLink editChild = ButtonLink.getTextualInstance(wsPage.getFrontOfficeTitle(), ps);
    editChild.toHtml(pageContext);
    if (i<pages.size()-1){
      %>,&nbsp;&nbsp;<%
        }
      }
    }
    %><br><br><%
  }

  //----------------------------------------------------------- POPRTLET PERMISSIONS --------------------------------------------------------------------------

  boolean hasPermissionsSet = JSP.ex(portlet.getPermissions()) || Collector.chosen("wpPermColl", pageState).size() > 0;


  if (!hasPermissionsSet) {
    ButtonJS bjs = new ButtonJS(I18n.get("PERMISSIONS"), "showPerm($(this));");
    bjs.additionalCssClass = "opener";
    bjs.iconChar = "g&ugrave;";
    bjs.toHtmlInTextOnlyModality(pageContext);
  }

%>
<div id="portlet_perm" style="display:<%=hasPermissionsSet?"block":"none"%>;margin: 10px 0"><%
  /**
   * permissions
   */
  Container contPerms = new Container("portletprm", 1);
  contPerms.title = I18n.get("PERMISSIONS");
  contPerms.level = 1;
  contPerms.collapsable = true;
  contPerms.status = Container.MAXIMIZED;
  contPerms.start(pageContext);
  Collector perms = new Collector("wpPermColl", 200, form);
  perms.setDefaultLabels(pageState);
  perms.toHtml(pageContext);
  contPerms.end(pageContext);
  // end permissions

%></div>
<%
  ButtonBar bbar = new ButtonBar();

  ButtonSubmit save = ButtonSubmit.getSaveInstance(form, I18n.get("SAVE"));
  save.label ="<b>"+I18n.get("SAVE")+"</b>" ;
  save.additionalCssClass = "big first";
  bbar.addButton(save);

  // delete
  if (!portlet.isNew()) {
    DeletePreviewer deletePreviewer = new DeletePreviewer("POR_DEL", PortletController.class, pageState);
    ButtonSupport del = deletePreviewer.getDeleteButton(I18n.get("DELETE"), portlet.getId());
    del.additionalCssClass = "big delete";
    bbar.addButton(del);
  }



  bbar.toHtml(pageContext);


  form.end(pageContext);

%>

<script>
  $(function(){
    reloadParamsAndScript("<%=jspName%>")
  });
  function reloadParamsAndScript(portlet){
    var origParams=<%=portlet.jsonify()%>.parameters;
    $("#PORTLET_PARAMS").empty();
    $.get(contextPath+"/"+portlet.replace('.jsp', '_param.jsp'),{OBJID:<%=pageState.mainObjectId%>},function(text){
      $("#PORTLET_PARAMS").append(text);
      if (origParams){
        for (var key in origParams){
          //console.debug(key, origParams[key]);
          $("#PT_PARAM_KEY_null_"+key).val(origParams[key]);
        }

      }
    });
    $("#PORTLET_TEXT").val("");
    $.get("parts/partPortletReader.jsp",{val:portlet},function(text){$("#PORTLET_TEXT").val(text)});
  }

  function showPerm(el) {
    $('#portlet_perm').fadeIn(300, function() {
      el.remove()
    });
  }

</script>


<%

  }

%>