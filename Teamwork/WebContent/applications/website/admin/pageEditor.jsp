<%@ page import="com.opnlb.website.page.WebSitePage,
                 com.opnlb.website.page.businessLogic.WebSitePageController,
                 com.opnlb.website.security.WebSitePermissions,
                 org.jblooming.operator.Operator,
                 org.jblooming.persistence.PersistenceHome,
                 org.jblooming.waf.ScreenBasic,
                 org.jblooming.waf.SessionState,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.container.ButtonBar,
                 org.jblooming.waf.html.display.DeletePreviewer,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState, java.io.Serializable, org.jblooming.waf.html.input.TextField, org.jblooming.waf.constants.Fields, org.jblooming.oql.OqlQuery, com.opnlb.website.template.Template, org.jblooming.utilities.CodeValueList, org.jblooming.waf.html.input.Combo, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.input.TextArea, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.button.ButtonSubmit, com.opnlb.website.content.Content, net.sf.json.JSONArray, org.jblooming.waf.settings.ApplicationState, com.opnlb.website.portlet.Portlet, java.util.*, com.opnlb.website.util.TemplateManager, org.jblooming.utilities.JSP, org.jblooming.waf.html.button.ButtonSupport"%><%@ page pageEncoding="UTF-8"%><%

    PageState pageState = PageState.getCurrentPageState(request);

   if (!pageState.screenRunning) {
     ScreenBasic.preparePage(new WebSitePageController(),pageContext);
     pageState.perform(request,response).toHtml(pageContext);


   } else {
     Operator logged = pageState.getLoggedOperator();

     WebSitePage wspage = (WebSitePage)pageState.getMainObject();

     if (logged==null || !logged.hasPermissionFor(WebSitePermissions.page_canManage))
       throw new SecurityException(I18n.get("PERMISSION_LACKING"));

     PageSeed self = pageState.thisPage(request);
     self.addClientEntry("CHOSEN_WSPAGE", (String) null);
     self.addClientEntry("CHOSEN_TEMPLATE", (String) null);
     self.setMainObjectId(wspage.getId());

     Form form = new Form(self);
     form.encType = Form.MULTIPART_FORM_DATA;
     form.usePost = true;
     // customization tab
     pageState.setForm(form);

     ButtonBar bbar = new ButtonBar();
     bbar.loggableIdentifiableSupport = wspage;
     pageState.setButtonBar(bbar);
     form.start(pageContext);

%>
<script>$("#ADMINISTRATION_ROOT_MENU").addClass('selected');</script>
<h1>
  <%
    PageSeed backList = new PageSeed("pageList.jsp");
    backList.setCommand(Commands.FIND + "_NODE");
    new ButtonLink(I18n.get("PAGES"), backList).toHtmlInTextOnlyModality(pageContext);

    if (wspage.isNew()) {
  %> / <%=I18n.get("NEW_OBJECT")%><%
} else {
%> / <%=wspage.getName()%><%
  }

%></h1><%


  JSONArray usedAreas= new JSONArray();


          //verify permissions

          Serializable objId= pageState.getMainObjectId();

          TextField.hiddenInstanceToHtml(Fields.PARENT_ID, pageContext);

%><table border="0" class="table" cellpadding="5" cellspacing="0">
  <tr><td><%

    TextField foTitle = new TextField("TEXT", I18n.get("TITLE"), "FOTITLE","<br>", 30,false);
    foTitle.required = true;
    foTitle.toHtml(pageContext);

  %></td><%

    TextField tfName = new TextField("TEXT", I18n.get("NAME"), "NAME","<br>",30,false);

  %><td><%tfName.toHtml(pageContext);%></td>
    <td><%

      OqlQuery oqltypes = new OqlQuery("from " + Template.class.getName() + " as t order by t.name ");
      List lsts = oqltypes.list();
      CodeValueList cvls = new CodeValueList();
      if (lsts!=null) {
        Iterator itera = lsts.iterator();
        while (itera.hasNext()) {
          Template template =  (Template)itera.next();
          cvls.add(template.getId().toString(), template.getName());
        }
      }

      Combo combos = new Combo("DEF_TEMPLATE","<br>","formElements",0, cvls, "");
      combos.label = I18n.get("DEF_TEMPLATE");
      StringBuffer script = new StringBuffer();
      script.append("loadPortletUsage($(this));");
      if (!wspage.isNew())
        combos.setJsOnChange = script.toString();
      combos.required=true;
      combos.toHtml(pageContext);

      if (!wspage.isNew()&& wspage.getDefaultTemplate()!=null){
        PageSeed goTo = new PageSeed(request.getContextPath() + "/" + wspage.getDefaultTemplate().getTemplateFile().getFileLocation());
        goTo.setPopup(true);
        ButtonJS js = new ButtonJS();
        js.onClickScript = " centerPopup('"+goTo.toLinkToHref()+"', 'tmp', '800', '600', 'yes','yes'); ";

        js.iconChar="#";
        js.toolTip=I18n.get("GOTO_TEMPLATE");
        js.label="";


        PageSeed tempEdit = new PageSeed(request.getContextPath()+"/applications/website/admin/templateEditor.jsp");
        tempEdit.setCommand(Commands.EDIT);
        tempEdit.mainObjectId = wspage.getDefaultTemplate().getId();
        ButtonLink temp = new ButtonLink(tempEdit);
        temp.iconChar="8";
        temp.toolTip=I18n.get("TEMPLATE_PREVIEW");
        temp.label="";


    %>&nbsp;<%js.toHtmlInTextOnlyModality(pageContext);%>&nbsp;<%temp.toHtmlInTextOnlyModality(pageContext);%><%
      }

    %></td></tr><%

  TextArea tfNotes = new TextArea (I18n.get("PAGE_INTERNAL_NOTES"), "DESCRIPTION","<br>",40,6,"");
  tfNotes.script="style=width:90%";

%><tr><td valign="top" colspan="2"><%tfNotes.toHtml(pageContext);%></td>
  <td valign="top"><br><%

    CheckField checkCustom = new CheckField("CUSTOM", "&nbsp;", false);
    checkCustom.label =  I18n.get("CUSTOMIZABLE");

    if (pageState.getEntry("ACTIVE").stringValueNullIfEmpty()==null) {
      pageState.addClientEntry("ACTIVE", Fields.TRUE);
    }
    CheckField checkActive = new CheckField("ACTIVE", "&nbsp;", false);
    checkActive.label =  I18n.get("PAGE_ACTIVE");

    checkCustom.toHtml(pageContext);

    checkActive.toHtml(pageContext);

    pageState.addClientEntry("TEMPLATE_AREAS","");
    TextField templateAreas = new TextField("TEMPLATE_AREAS", "<br>");
    templateAreas.type="hidden";
    templateAreas.label="";
    templateAreas.required=true;
    templateAreas.readOnly=true;
    templateAreas.fieldSize=40;
    templateAreas.toHtml(pageContext);




  %><span style="font-size: 20px; color:#4ca454"><%
    if (!wspage.isNew()){
      PageSeed customPage = new PageSeed(request.getContextPath() + "/applications/website/admin/customizePage.jsp");
      customPage.addClientEntry("PAGEID", wspage);
      customPage.addClientEntry("GENCONF",true);
      ButtonLink bl = new ButtonLink(I18n.get("CUSTOMIZE"), customPage);
      bl.iconChar="e";
      bl.toHtmlInTextOnlyModality(pageContext);
    }
  %></span></td>

</tr>

</table>
<jsp:include page="parts/partPageRoles.jsp"/>
<%


  // DELETE
  if (!wspage.isNew()){
    DeletePreviewer deletePreviewer = new DeletePreviewer("PAGE_DEL",WebSitePageController.class, pageState);
    ButtonSupport delPrev = deletePreviewer.getDeleteButton(I18n.get("DELETE"), wspage.getId() );
    delPrev.additionalCssClass="big delete";
    bbar.addButton(delPrev);
  }


  ButtonSubmit save = ButtonSubmit.getSaveInstance(pageState.getForm(),I18n.get("SAVE"),false);
  save.additionalCssClass = "big first";
  bbar.addToRight(save);

  pageState.setFocusedObjectDomId(foTitle.id);

  if (!wspage.isNew()) {
    String hql = "select distinct c.area from " + Content.class.getName() + " as c where c.page.id=:pid ";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setString("pid", wspage.getId() + "");
    usedAreas.addAll((List<String>)oql.list());

    hql = "select distinct c.portlet from " + Content.class.getName() + " as c where c.page.id=:pid ";
    oql = new OqlQuery(hql);
    oql.getQuery().setString("pid", wspage.getId() + "");
    List<Portlet> portlets=oql.list();
    if (JSP.ex(portlets)){
      %><br><br><b><%=I18n.get("ASSOCIATED_PORTLETS")%>: </b>&nbsp;<%
      for (int i = 0; i < portlets.size(); i++) {
        Portlet portlet = (Portlet) portlets.get(i);
        PageSeed ps = new PageSeed(request.getContextPath() + "/applications/website/admin/portletEditor.jsp");
        ps.setCommand(Commands.EDIT);
        ps.setMainObjectId(portlet.getId());
        ButtonLink editChild = ButtonLink.getTextualInstance(portlet.getDisplayName(), ps);
        editChild.toHtml(pageContext);
        if (i<portlets.size()-1){
          %>,&nbsp;&nbsp;<%
        }
      }
    }

  }

%>
<br>
<%
  bbar.toHtml(pageContext);

  form.end(pageContext);
%>

<script>

  $(function(){
    loadPortletUsage($("#DEF_TEMPLATE"))
  });


  function loadPortletUsage(el){
    var usedAreas=<%=usedAreas%>;
    usedAreas=usedAreas.length>0?usedAreas.join(",").toUpperCase().split(","):[];
    if (el.val()) {
      var wrp=$("<div>");
      $.get(contextPath+"/applications/website/admin/ajax/getTemplate.jsp?tid="+el.val(),function(ret){
        wrp.append(ret);
        var areasFound=[];
        wrp.find("[areaname]").each(function(){
          areasFound.push($(this).attr("areaName"))
        });
        var stringAreas = areasFound.join(",").toUpperCase();
        $("#TEMPLATE_AREAS").val(stringAreas);
        areasFound=stringAreas.split(",");
        //console.debug("areasFound",areasFound)

        var missingAreas=[];
        for(var i=0;i<usedAreas.length;i++){
          if (areasFound.indexOf(usedAreas[i])<0){
            missingAreas.push(usedAreas[i]);
          }
        }
        //console.debug("missingAreas",missingAreas)
        if (missingAreas.length>0){
          showFeedbackMessage("ERROR","<%=I18n.get("CHANGE_TEMPLATE_REMOVE","%%")%>".replace("%%",missingAreas.join(", ")));
        } else {
          hideFeedbackMessages();
        }
      })
    }
  }



</script>



<%


   }

 %>