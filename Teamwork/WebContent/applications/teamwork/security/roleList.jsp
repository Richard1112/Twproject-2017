<%@ page import="com.twproject.operator.TeamworkOperator" %>
<%@ page import="com.twproject.security.SecurityBricks" %>
<%@ page import="com.twproject.security.TeamworkPermissions" %>
<%@ page import="com.twproject.security.businessLogic.TeamworkRoleController, com.twproject.waf.TeamworkHBFScreen, org.jblooming.security.Area, org.jblooming.security.PlatformPermissions, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.DataTable, org.jblooming.waf.html.input.Combo, org.jblooming.waf.html.input.LoadSaveFilter, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.display.DeletePreviewer, com.twproject.security.RoleTeamwork" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {

    pageState.screenRunning = true;

    final ScreenArea body = new ScreenArea(new TeamworkRoleController(), request);
    //body.areaHtmlClass="lreq30 lreqPage";
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);

    pageState.toHtml(pageContext);


  } else {

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    PageSeed self = pageState.thisPage(request);
    self.setCommand(Commands.FIND);
    Form f = new Form(self);
    pageState.setForm(f);

    f.start(pageContext);

    String savedFilterName = pageState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty();


    //sporco trucco per caricano/creano i ruoli di risorsa
    RoleTeamwork.getManagerRole();
    RoleTeamwork.getMyselfRole();


%>
<script>$("#ADMINISTRATION_ROOT_MENU").addClass('selected');</script>
<%
  ButtonLink adminLink = new ButtonLink(I18n.get("ADMINISTRATION_ROOT_MENU") + " /",pageState.pageFromRoot("administration/administrationIntro.jsp"));
%><%adminLink.toHtmlInTextOnlyModality(pageContext);%>
<h1 class="filterTitle" defaultTitle="<%=I18n.get("ROLES")%>">
  <%=JSP.ex(savedFilterName)?I18n.get(savedFilterName):I18n.get("ROLES")%>
</h1>

<div class="filterActiveElements optionsBar clearfix">
<div class="filterElement"><%
    TextField tf = new TextField("TEXT", I18n.get("ROLE_NAME"), "name", "<br>", 30, false);
    tf.addKeyPressControl(13, "obj('" + pageState.getForm().getUniqueName() + "').submit();", "onkeyup");
    //pageState.setFocusedObjectDomId(tf.id);
    tf.toHtml(pageContext);%>
</div>
<%if(!SecurityBricks.isSingleArea()){%>
<div class="filterElement"><%
      Combo cbb = SecurityBricks.getAreaCombo("AREA", PlatformPermissions.role_canRead, pageState);
      cbb.separator="<br>";
      cbb.cvl.addChoose(pageState);
      cbb.toHtml(pageContext);
    %>
</div>
<%}%>
</div>
<%
    DataTable dataTable = new DataTable("ROLELH", f, new JspHelper("/applications/teamwork/security/rowRoleList.jsp"), TeamworkRoleController.class, pageState);
    dataTable.addHeaderFitAndCentered(I18n.get("EDIT_SHORT"));
    dataTable.addHeader(I18n.get("CODE"), "role.code");
    dataTable.addHeader(I18n.get("NAME"), "role.name");
    dataTable.addHeader(I18n.get("FLD_ROLE_DESCRIPTION"), "role.description");
    if(!SecurityBricks.isSingleArea())
      dataTable.addHeader(I18n.get("AREA"), "role.area.name");
    dataTable.addHeader(I18n.get("LOCAL_TO_ASS_SHORT"), "role.localToAssignment");
    dataTable.addHeaderFitAndCentered(I18n.get("DELETE_SHORT"));


    new DeletePreviewer("ROLE_DEL",TeamworkRoleController.class, pageState);
    ButtonBar bb = new ButtonBar();

    bb.addButton(dataTable.getSearchButton());

    Area defaultArea = logged.getDefaultAreaForPermission(TeamworkPermissions.project_canCreate);

    final PageSeed addLc = pageState.pageInThisFolder("roleEditor.jsp", request);
    addLc.setCommand(Commands.ADD);
    addLc.addClientEntry("AREA", defaultArea);
    ButtonLink addLocal = new ButtonLink(addLc);
    addLocal.enabled = logged.hasPermissionFor(PlatformPermissions.role_canCreate) || logged.getAreasForPermission(PlatformPermissions.role_canCreate).size() > 0;
    addLocal.label = I18n.get("ROLE_ADD_LOCAL");
    addLocal.additionalCssClass="lreq30 lreqLabel";
    addLocal.pageSeed.addClientEntry("LOCAL_TO_ASS", Fields.TRUE);
    bb.addButton(addLocal);


    final PageSeed addL = pageState.pageInThisFolder("roleEditor.jsp", request);
    addL.setCommand(Commands.ADD);
    addL.addClientEntry("AREA", defaultArea);
    ButtonLink add = new ButtonLink(addL);
    add.enabled = logged.hasPermissionFor(PlatformPermissions.role_canCreate) || logged.getAreasForPermission(PlatformPermissions.role_canCreate).size() > 0;
    add.label = I18n.get("ROLE_ADD_GLOBAL");
    add.additionalCssClass="lreq30 lreqLabel";
    bb.addButton(add);


    ButtonSupport qbe = ButtonLink.getBlackInstance(JSP.wHelp(I18n.get("HELP")), 700, 800, pageState.pageFromCommonsRoot("help/qbe.jsp"));
    qbe.toolTip = I18n.get("HELP_QBE");
    bb.addButton(qbe);
    bb.addSeparator(10);

    LoadSaveFilter lsf = new LoadSaveFilter("Role", f);
    bb.addButton(lsf);

    bb.toHtml(pageContext);


    dataTable.drawPaginator(pageContext);
    dataTable.drawTable(pageContext);

    f.end(pageContext);

    %>

<script>
  $("#TOOLS_MENU").addClass('selected');

  function delRow(el) {
    var docRow = $(el).closest("[roleId]");
    var docId = docRow.attr("roleId");
    deletePreview("ROLE_DEL", docId, function (response) {  // callback function
      if (response && response.ok) {
        docRow.fadeOut(500, function () {$(this).remove();});
      }
    });

  }


</script>




<%
  }
%>




