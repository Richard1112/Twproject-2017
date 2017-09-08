<%@ page import=" com.opnlb.website.portlet.businessLogic.PortletController, com.opnlb.website.security.WebSitePermissions, org.jblooming.operator.Operator, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenBasic, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.DataTable, org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.html.input.RadioButton, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState" %>
<%@page pageEncoding="UTF-8" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);

  //verify permissions
  Operator logged = pageState.getLoggedOperator();
  if (logged == null || !logged.hasPermissionFor(WebSitePermissions.portlet_canManage))
    throw new SecurityException(I18n.get("PERMISSION_LACKING"));

  if (!pageState.screenRunning) {

    ScreenBasic sb = ScreenBasic.preparePage(new PortletController(), pageContext);
    sb.getBody().areaHtmlClass="lreq20 lreqPage";
    pageState.perform(request, response).toHtml(pageContext);

  } else {

    String savedFilterName = pageState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty();

%>
<%
  ButtonLink adminLink = new ButtonLink(I18n.get("ADMINISTRATION_ROOT_MENU") + " /",pageState.pageFromRoot("administration/administrationIntro.jsp"));
%>
<%adminLink.toHtmlInTextOnlyModality(pageContext);%>
<h1 class="filterTitle" defaultTitle="<%=I18n.get("PORTLET_LIST")%>">
  <%=JSP.ex(savedFilterName)?I18n.get(savedFilterName):I18n.get("PORTLET_LIST")%>
</h1>
<%

    PageSeed self = pageState.thisPage(request);
    self.setCommand(Commands.FIND);
    Form f = new Form(self);
    pageState.setForm(f);

    f.start(pageContext);

%>
<script>$("#ADMINISTRATION_ROOT_MENU").addClass('selected');</script>
<table cellpadding="3" border="0" class="filterActiveElements">
  <tr height="50"><%

    TextField tfName = new TextField("TEXT", I18n.get("NAME_DESCRIPTION"), "NAME_DESCRIPTION", "</td><td width=\"300\">", 30, false);
    tfName.addKeyPressControl(13, "obj('" + f.getUniqueName() + "').submit();", "onkeyup");

  %>
    <td><%tfName.toHtml(pageContext);%></td>
    <td nowrap><%=I18n.get("INSTALLED")%>: <%

      RadioButton rbIns = new RadioButton(I18n.get("YES") + "&nbsp;", "INSTALLED", Fields.TRUE, "", null, false, "");
      rbIns.toHtml(pageContext);

      rbIns = new RadioButton(I18n.get("NO") + "&nbsp;", "INSTALLED", Fields.FALSE, "", null, false, "");
      rbIns.toHtml(pageContext);

      if (pageState.getEntry("INSTALLED").stringValue() == null) {
        pageState.addClientEntry("INSTALLED", "ALL");
      }
      rbIns = new RadioButton(I18n.get("ALL"), "INSTALLED", "ALL", "", null, false, "");
      rbIns.toHtml(pageContext);

    %></td>
  </tr>
</table>
<%
    ButtonBar bb = new ButtonBar();

    ButtonSupport qbe = ButtonLink.getBlackInstance(JSP.wHelp(I18n.get("HELP")), 700, 800, pageState.pageFromCommonsRoot("help/qbe.jsp"));
    qbe.toolTip = I18n.get("HELP_QBE");
    bb.addButton(qbe);
    bb.addSeparator(10);

    bb.addSeparator(20);

    PageSeed edit = new PageSeed("portletEditor.jsp");
    edit.setCommand(Commands.ADD);
    ButtonLink addBl = new ButtonLink(edit);
    addBl.label = I18n.get("ADD");
    addBl.additionalCssClass="first";
    bb.addButton(addBl);
    bb.addSeparator(20);

    DataTable dataTable = new DataTable("WPMGR", f, new JspHelper("/applications/website/admin/rowPortletList.jsp"), PortletController.class ,pageState);
    dataTable.addHeader(I18n.get("NAME"), "wp.name");
    dataTable.addHeader(I18n.get("DESCRIPTION"), "wp.description");
    dataTable.addHeader(I18n.get("PX_WIDTH"), "wp.description");
    dataTable.addHeader(I18n.get("PX_HEIGHT"), "wp.description");
    dataTable.addHeader(I18n.get("PORTLET_FILE"), "wp.name");
    dataTable.addHeaderFitAndCentered(I18n.get("USED_IN"));
    dataTable.addHeaderFitAndCentered(I18n.get("INSTALLED_SHORT"));
    dataTable.addHeaderFitAndCentered(I18n.get("DELETE_SHORT"));
    bb.addButton(dataTable.getSearchButton());

    bb.toHtml(pageContext);

    dataTable.drawPaginator(pageContext);
    dataTable.drawTable(pageContext);

    new DeletePreviewer("PORT_DEL",PortletController.class, pageState);

    f.end(pageContext);
    pageState.setFocusedObjectDomId(tfName.id);
  }

%>