<%@ page
    import="com.twproject.security.TeamworkPermissions, com.twproject.task.Task, org.jblooming.designer.DesignerData, org.jblooming.oql.OqlQuery, org.jblooming.persistence.PersistenceHome, org.jblooming.utilities.JSP, org.jblooming.waf.PageQuark, org.jblooming.waf.PluginBricks, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List, java.util.Set, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.designer.Designer, org.jblooming.ontology.Documentable, com.twproject.resource.Resource, com.twproject.resource.Person" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  JspHelper cufd = (JspHelper) JspIncluderSupport.getCurrentInstance(request);


  Documentable documentable = (Documentable) cufd.parameters.get("documentable");

  Class docClass=null;
  String groupName ="";
  String documentableParamName="";

  String docClassName = documentable.getClass().getName().toLowerCase();
  if(docClassName.contains("task")) {
    docClass=Task.class;
    groupName ="TASK_FORMS";
    documentableParamName ="TASK_ID";
  } else if(docClassName.contains("person")) {
    docClass= Person.class;
    groupName ="RESOURCE_FORMS";
    documentableParamName ="RES_ID";
  } else if(docClassName.contains("resource")) {
    docClass= Resource.class;
    groupName ="RESOURCE_FORMS";
    documentableParamName ="RES_ID";
  }

  if (docClass==null)
    return;

  Set<PageQuark> pqs = PluginBricks.getPageQuarkGroup(groupName);

  int formsDisplayed =0;

  for (PageQuark pq : pqs) {

  if (pq.isVisibleInThisContext(pageState)) {
    formsDisplayed++;

    Designer designer = new Designer(pq.urlToInclude, pq.getName(), docClass, documentable.getId());
    DesignerData designerData = designer.getDesignerData();
    boolean fulfilled = designerData.getValueMap().size()>0;

    String formName = pq.getName();

    PageSeed edf = pageState.pageFromRoot("document/drawModule.jsp");
    edf.mainObjectId = documentable.getId();
    edf.setPopup(true);
    edf.addClientEntry("DESIGNER_URL_TO_INCLUDE",pq.getHrefForInclude());
    edf.addClientEntry("DESIGNER_NAME",formName);
    edf.addClientEntry("CLAZZ",docClass.getName());

    ButtonSupport toForm = ButtonLink.getBlackInstance("", edf,"closeFormEditorCallback");
    toForm.iconChar="e";

    boolean canWrite = documentable.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.document_canWrite);


%>
      <tr class="alternate lreq30 lreqLabel" style="height: 32px" desDataId="<%=designerData.isNew()?"":designerData.getId()%>">
        <td></td>
        <td align="center"><%toForm.iconChar="&#xe0;";toForm.toHtmlInTextOnlyModality(pageContext);%></td>
        <td><%toForm.label=I18n.get(formName);toForm.iconChar="";toForm.additionalCssClass="lreq30 lreqLabel";toForm.toHtmlInTextOnlyModality(pageContext);%></td>
        <td width="20%" class="textSmall"><%
          if (fulfilled) {
            %><span class="teamworkIcon" style="font-size:120%;cursor: default" title="<%=I18n.get("FORM_FILLED")%>">&#x2039;</span>&nbsp;<span><%=I18n.get("FORM_FILLED")%></span><%
          } else {
            %><span class="warning"><%=I18n.get("FORM_NOT_FILLED")%></span><%
          }
          %></td>
        <td class="textSmall"><%=JSP.w(designerData.getLastModified())%></td>
        <td>&nbsp;</td>
        <td width="50" align="left">
          <%

            toForm.iconChar="e";
            toForm.label="";
            toForm.additionalCssClass="edit";
            toForm.toHtmlInTextOnlyModality(pageContext);

            if (fulfilled&& canWrite) {
              ButtonJS reset = new ButtonJS("emptyCustomForm('"+designerData.getId()+"')");
              reset.confirmRequire=true;
              reset.confirmQuestion=I18n.get("SURE_TO_DELETE_FORM_VALUE");
              reset.label = "";
              reset.toolTip = I18n.get("CLEAR");
              reset.iconChar ="d";
              reset.toHtmlInTextOnlyModality(pageContext);
            }
          %>

        </td>
      </tr>
    <%

      }

}

pageState.addClientEntry("formsDisplayed", formsDisplayed);
%><script>
  function emptyCustomForm(desDatId) {
    var req = {CM: "EMPCF", desDatId: desDatId};
    showSavingMessage();
    $.getJSON(contextPath + "/applications/teamwork/document/documentAjaxController.jsp", req, function (response) {
      jsonResponseHandling(response);
      if (response.ok) {
        location.href=location.pathname+"?CM=LIST_DOCS&<%=documentableParamName%>="+getParameterByName("<%=documentableParamName%>");
      }
      hideSavingMessage();
    })
  }
</script>
