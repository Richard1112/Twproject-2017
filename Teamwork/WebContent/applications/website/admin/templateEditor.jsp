<%@ page import=" com.opnlb.website.page.WebSitePage,
                  com.opnlb.website.security.WebSitePermissions,
                  com.opnlb.website.template.Template,
                  com.opnlb.website.template.businessLogic.TemplateController,
                  org.jblooming.operator.Operator,
                  org.jblooming.oql.QueryHelper,
                  org.jblooming.utilities.CodeValueList,
                  org.jblooming.utilities.HttpUtilities,
                  org.jblooming.utilities.JSP,
                  org.jblooming.waf.ScreenBasic,
                  org.jblooming.waf.constants.Commands,
                  org.jblooming.waf.html.button.ButtonLink,
                  org.jblooming.waf.html.button.ButtonSubmit,
                  org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.html.input.Combo, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.io.File, java.io.FileNotFoundException, java.util.List"%><%@ page pageEncoding="UTF-8" %><%

  PageState pageState = PageState.getCurrentPageState(request);

  //verify permissions
  Operator logged = pageState.getLoggedOperator();
  if (logged==null || !logged.hasPermissionFor(WebSitePermissions.template_canManage))
    throw new SecurityException(I18n.get("PERMISSION_LACKING"));

  if (!pageState.screenRunning) {
    ScreenBasic.preparePage(new TemplateController(),pageContext);
    pageState.perform(request, response).toHtml(pageContext);

  } else {


    Template template = (Template) pageState.getMainObject();

    PageSeed self = pageState.thisPage(request);
    self.mainObjectId = template.getId();
    Form form = new Form(self);
    form.encType = Form.MULTIPART_FORM_DATA;
    form.usePost = true;

    ButtonBar bbar=new ButtonBar();

    pageState.setButtonBar(bbar);
    pageState.setForm(form);
    form.start(pageContext);

    PageSeed list = new PageSeed("templateList.jsp");
    list.setCommand(Commands.FIND);
    ButtonLink lista = new ButtonLink(list);
    lista.label = I18n.get("TEMPLATES");


%><script>$("#ADMINISTRATION_ROOT_MENU").addClass('selected');</script>
<h1><%lista.toHtmlInTextOnlyModality(pageContext);%> / <%=(template.getName()!=null ? template.getName() : "...")%></h1>

<table border="0" width="100%"><tr>
  <td valign="top"><%

    TextField tfName=new TextField("TEXT", I18n.get("NAME"), "NAME", "<br>", 20, false);
    tfName.required = true;
    tfName.toHtml(pageContext);

  %>&nbsp;<small><%=template.isNew() ? "" : "(id:&nbsp;"+template.getId()+")"%></small></td>
  <td valign="top"><%

    TextField tfDesc=new TextField("TEXT", I18n.get("DESCRIPTION"), "DESCRIPTION", "<br>", 40, false);
    tfDesc.toHtml(pageContext);

  %></td><td valign="top"><%

  String templateLocation = ApplicationState.getApplicationSetting("TEMPLATE_LOCATION");
  File root = new File(HttpUtilities.getFileSystemRootPathForRequest(request) + File.separator + templateLocation);
  root.mkdirs();

  CodeValueList cvlTempl = new CodeValueList();
  cvlTempl.addChoose(pageState);
  for (File templFile : root.listFiles()) {
    if (templFile.isFile() && (templFile.getName().endsWith(".htm") || templFile.getName().endsWith(".html")))
      cvlTempl.add(templFile.getName());
  }

  Combo cbfile = new Combo("TEMPLATE_FILE", "<br>", null, 50, cvlTempl, null);
  cbfile.label = I18n.get("TEMPLATE_FILE");
  cbfile.required = true;
  String absolutePath = request.getContextPath() + "/" + templateLocation + "/";
  cbfile.setJsOnChange = "loadTemplate($(this))";
  cbfile.toHtml(pageContext);

  pageState.addClientEntry("TEMPLATE_AREAS","");
  TextField templateAreas = new TextField("TEMPLATE_AREAS", "<br>");
  templateAreas.type="hidden";
  templateAreas.label="";
  templateAreas.required=true;
  templateAreas.readOnly=true;
  templateAreas.fieldSize=40;
  templateAreas.toHtml(pageContext);


%><br><%=JSP.wHelp(I18n.get("APPLICATION_READ_FILES") + ": " + root.getAbsolutePath())%><%


%></td></tr></table><%

  // save
  ButtonSubmit save = ButtonSubmit.getSaveInstance(pageState.getForm(), I18n.get("SAVE"));
  save.additionalCssClass = "big first";
  bbar.addButton(save);

  // info page
  PageSeed infoPs = new PageSeed(request.getContextPath() + "/applications/website/admin/templateInfo.jsp");
  ButtonSupport info = ButtonLink.getBlackInstance(I18n.get("HELP"), 600, 550, infoPs);
  info.additionalCssClass = "big";
  bbar.addButton(info);

  // delete
  if (!template.isNew()) {
    DeletePreviewer deletePreviewer = new DeletePreviewer("TMPL_DEL", TemplateController.class, pageState);
    ButtonSupport delPrev = deletePreviewer.getDeleteButton(I18n.get("DELETE"), template.getId());
    delPrev.additionalCssClass = "big delete";
    bbar.addButton(delPrev);
  }


%>
  <h2><%=I18n.get("TEMPLATE_PREVIEW")%></h2>
  <div id="TEMPLATE_PREVIEW"></div><%

  pageState.setFocusedObjectDomId(tfName.id);

  if (!template.isNew()) {
    String ql = " from " + WebSitePage.class.getName() + " as wsp ";
    QueryHelper qh = new QueryHelper(ql);
    qh.addOQLClause(" wsp.defaultTemplate = :templ", "templ", template);
    List pages = qh.toHql().list();

    if (JSP.ex(pages)) {
      %><br><br><b><%=I18n.get("USED_IN")%>: </b>&nbsp;<%
      for (int i = 0; i < pages.size(); i++) {
        WebSitePage wsPage = (WebSitePage) pages.get(i);
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
  }
    bbar.toHtml(pageContext);
    form.end(pageContext);
    %>


<script type="text/javascript">
  $(function(){
    loadTemplate($("#TEMPLATE_FILE"))
  });

  function loadTemplate(el){
    $("#TEMPLATE_AREAS").val("");
    var ndo=$('#TEMPLATE_PREVIEW');
    if (el.val()){
      var tpFile="<%=absolutePath%>"+el.val();
      var wrp=$("<div>");
      $.get(tpFile,function(ret){
        wrp.append(bodyCleaner(ret));
        if (wrp.find("[areaname]").length>0){
          $("#ISVALIDTPL").val("yes");
          //wrp.find("[class]").attr("class","");
          var templateAreas=[];
          wrp.find("[areaname]").css( { "min-height":'60px' }).css("border","2px dashed orange").css("margin","10px").each(function(){
            var areaName = $(this).attr("areaname");
            templateAreas.push(areaName);
            $(this).prop("title", areaName)});
          ndo.html(wrp);
          $("#TEMPLATE_AREAS").val(templateAreas.join(",").toLocaleUpperCase())

        } else {
          ndo.html("<%=pageState.getI18n("ERR_TEMPLATE_WITH_NO_AREAS_UPLOAD")%>");
        }
      })
    }
  }

  function bodyCleaner(contenuto) {
    var ret = '';
    if (contenuto) {
      contenuto = contenuto.replace(/<script/g, '<div style="display:none;"');
      contenuto = contenuto.replace(/<\/script/g, '</div');
      contenuto = contenuto.replace('<form', '<disabled');
      contenuto = contenuto.replace('</form', '</disabled');
      contenuto = contenuto.replace(/required/g, 'disabled');
      contenuto = contenuto.replace(/<link/g, '<disabled');
      contenuto = contenuto.replace(/onclick/g, 'disabled');
      contenuto = contenuto.replace(/onClick/g, 'disabled');
      contenuto = contenuto.replace(/onchange/g, 'disabled');
      contenuto = contenuto.replace(/onChange/g, 'disabled');
      contenuto = contenuto.replace(/onkeyup/g, 'disabled');
      contenuto = contenuto.replace(/optgroup/g, 'disabled');
      contenuto = contenuto.replace(/submit/g, 'disabled');
      contenuto = contenuto.replace(/href/g, 'disabled');
      contenuto = contenuto.replace(/input/g, 'input disabled');
      contenuto = contenuto.replace(/pointer/g, 'default');
      contenuto = contenuto.replace(/obj/g, 'disabled');
      contenuto = contenuto.replace(/select/g, ' select disabled');
      contenuto = contenuto.replace(/SELECT/g, 'select disabled');
      //contenuto = contenuto.replace(/document.write/g, '//document.write');
      contenuto = contenuto.replace(/document.write/g, '');
      ret = contenuto;
    }
    return ret;
  }
</script>
<%
  }
%>