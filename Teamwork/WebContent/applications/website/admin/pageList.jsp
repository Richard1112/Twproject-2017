<%@ page import=" com.opnlb.website.page.businessLogic.WebSitePageController, com.opnlb.website.security.WebSitePermissions, org.jblooming.operator.Operator, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenBasic, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields,
                 org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.DataTable, org.jblooming.waf.html.input.RadioButton, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jblooming.waf.html.display.DeletePreviewer" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  //verify permissions
  Operator logged = pageState.getLoggedOperator();
  if (logged == null || !logged.hasPermissionFor(WebSitePermissions.page_canManage))
    throw new SecurityException(I18n.get("PERMISSION_LACKING"));


  if (!pageState.screenRunning) {
    ScreenBasic sb = ScreenBasic.preparePage(new WebSitePageController(), pageContext);
    sb.getBody().areaHtmlClass="lreq20 lreqPage";
    pageState.perform(request, response).toHtml(pageContext);

  } else {

    String savedFilterName = pageState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty();
%><%
  ButtonLink adminLink = new ButtonLink(I18n.get("ADMINISTRATION_ROOT_MENU") + " /",pageState.pageFromRoot("administration/administrationIntro.jsp"));
%>
<%adminLink.toHtmlInTextOnlyModality(pageContext);%>
<h1 class="filterTitle" defaultTitle="<%=I18n.get("PAGE_LIST")%>">
  <%=JSP.ex(savedFilterName)?I18n.get(savedFilterName):I18n.get("PAGE_LIST")%>
</h1>
<%

    PageSeed self = pageState.thisPage(request);
    self.setCommand(Commands.FIND);
    Form f = new Form(self);
    pageState.setForm(f);


  new DeletePreviewer("PAGE_DEL",WebSitePageController.class, pageState);



  f.start(pageContext);
%>
<script>$("#ADMINISTRATION_ROOT_MENU").addClass('selected');</script>

<table cellpadding="3" border="0">
  <tr height="50"><%

    TextField tfName = new TextField("TEXT", I18n.get("NAME_DESCRIPTION"), "NAME_DESCRIPTION", "</td><td colspan=\"3\">", 40, false);
    tfName.addKeyPressControl(13, "obj('" + f.getUniqueName() + "').submit();", "onkeyup");

  %>
    <td><%tfName.toHtml(pageContext);%></td>
    <td nowrap><%=I18n.get("ACTIVE")%>: <%

      RadioButton rbDef = new RadioButton(I18n.get("YES"), "ACTIVE", Fields.TRUE, "&nbsp;", null, false, "");
      rbDef.toHtml(pageContext);

      rbDef = new RadioButton(I18n.get("NO"), "ACTIVE", Fields.FALSE, "&nbsp;", null, false, "");
      rbDef.toHtml(pageContext);

      if (pageState.getEntry("ACTIVE").stringValue() == null) {
        pageState.addClientEntry("ACTIVE", "ALL");
      }
      rbDef = new RadioButton(I18n.get("ALL"), "ACTIVE", "ALL", "&nbsp;", null, false, "");
      rbDef.toHtml(pageContext);

    %></td>
  </tr>
</table>
<%

    ButtonBar bb = new ButtonBar();

    PageSeed edit = new PageSeed("pageEditor.jsp");
    edit.setCommand(Commands.ADD);
    ButtonLink addBl = new ButtonLink(edit);
    addBl.label = I18n.get("ADD");
    addBl.additionalCssClass="first";
    bb.addButton(addBl);
    bb.addSeparator(20);

    DataTable dataTable = new DataTable("WSPGMGR", f, new JspHelper("/applications/website/admin/rowPageList.jsp"), WebSitePageController.class, pageState);
    dataTable.addHeaderFitAndCentered(I18n.get("EDIT_SHORT"));
    dataTable.addHeader("id", "wspage.id");
    dataTable.addHeader(I18n.get("NAME"), "wspage.name");
    dataTable.addHeader(I18n.get("PAGE_INTERNAL_NOTES"), "wspage.description");
    dataTable.addHeader(I18n.get("TEMPLATE"), "wspage.defaultTemplate.name");
    dataTable.addHeader(I18n.get("PAGE_ACTIVE"), "wspage.active");
    dataTable.addHeaderFitAndCentered(I18n.get("DELETE_SHORT"));
    bb.addButton(dataTable.getSearchButton());

    bb.toHtml(pageContext);

    dataTable.drawPaginator(pageContext);
    dataTable.drawTable(pageContext);

    f.end(pageContext);
    pageState.setFocusedObjectDomId(tfName.id);
%>
<script>

  function delRow(el) {
    var issueRow = $(el).closest("[pageid]");
    var issueId = issueRow.attr("pageid");
    deletePreview("PAGE_DEL", issueId, function(response) {  // callback function
      if (response && response.ok){
        issueRow.fadeOut(500, function () {$(this).remove();});
      }
    });
  }


</script>



<%
  }
%>