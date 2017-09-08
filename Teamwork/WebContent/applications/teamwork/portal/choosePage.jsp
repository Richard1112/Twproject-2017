<%@ page
    import="com.opnlb.website.page.WebSitePage, com.opnlb.website.util.TemplateManager, com.opnlb.website.waf.WebSiteConstants, com.twproject.operator.TeamworkOperator, com.twproject.waf.TeamworkHBFScreen, org.jblooming.oql.OqlQuery, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.input.RadioButton, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {

    pageState.screenRunning = true;

    final ScreenArea body = new ScreenArea(request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    body.areaHtmlClass="lreq20 lreqPage";
    lw.register(pageState);
    pageState.perform(request, response);

    if (Commands.SAVE.equals(pageState.command)) {
      pageState.initializeEntries("row");

      String globalHome = pageState.getEntry("SET_AS_GLOBAL_HOME").stringValueNullIfEmpty();
      if (globalHome != null) {
        ApplicationState.applicationSettings.put(WebSiteConstants.HOME_PAGE, globalHome);
        ApplicationState.dumpApplicationSettings();
      }

      String persHome = pageState.getEntry("SET_AS_MY_HOME").stringValueNullIfEmpty();
      if (persHome != null) {
        TeamworkOperator loggedOnDb = (TeamworkOperator) pageState.getLoggedOperator();
        loggedOnDb.putOption(WebSiteConstants.HOME_PAGE, persHome);
      }
    }

    pageState.toHtml(pageContext);

  } else {
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();


    PageSeed self = pageState.thisPage(request);
    self.setCommand(Commands.FIND);
    Form f = new Form(self);
    pageState.setForm(f);
    f.start(pageContext);


    String hql = "from " + WebSitePage.class.getName() + " as page where page.name!='ROOT'";
    OqlQuery oql = new OqlQuery(hql);
    List<WebSitePage> wsp = oql.list();

%><script>$("#HOME_MENU").addClass('selected');</script>
<%
  ButtonLink adminLink = new ButtonLink(I18n.get("ADMINISTRATION_ROOT_MENU") + " /",pageState.pageFromRoot("administration/administrationIntro.jsp"));
%>
<%adminLink.toHtmlInTextOnlyModality(pageContext);%><h1><%=I18n.get("CHOOSE_PAGE")%></h1>
<table class="table">
  <tr>
    <th class="tableHead"><%=I18n.get("CUSTOMIZE")%>
    </th>
    <th class="tableHead"><%=I18n.get("NAME")%>
    </th>
    <th class="tableHead"><%=I18n.get("DESCRIPTION")%>
    </th>
    <th class="tableHead"><%=I18n.get("SET_AS_MY_HOME")%>
    </th>
    <%
      if (logged.hasPermissionAsAdmin()) {
    %>
    <th class="tableHead"><%=I18n.get("SET_AS_GLOBAL_HOME")%>
    </th>
    <%
      }
    %>
    <th class="tableHead" nowrap><%=I18n.get("SET_AS_HOME_FOR")%>
    </th>
  </tr>
  <%

    RadioButton cf = new RadioButton("", "SET_AS_MY_HOME", null, "", null, false, null);
    RadioButton sagh = new RadioButton("", "SET_AS_GLOBAL_HOME", null, "", null, false, null);
    pageState.addClientEntry("SET_AS_GLOBAL_HOME", ApplicationState.applicationSettings.get(WebSiteConstants.HOME_PAGE) + "");
    String customPageOption = logged.getOption(WebSiteConstants.HOME_PAGE);
    if (customPageOption != null)
      pageState.addClientEntry("SET_AS_MY_HOME", customPageOption + "");

    for (WebSitePage p : wsp) {
      if (p.isCustomizable() || !p.isCustomizable() && logged.hasPermissionAsAdmin()) {

        PageSeed see = new PageSeed(request.getContextPath() + "/" + p.getName() + ".page");

  %>
  <tr class="alternate" ><%

    String pageId = TemplateManager.getIdFromPageName(p.getName());
    PageSeed customPage = new PageSeed(request.getContextPath() + "/applications/website/admin/pageEditor.jsp");
    customPage.addClientEntry("DEF", pageState.getEntry("DEF").stringValueNullIfEmpty());
    customPage.setCommand(Commands.EDIT);
    customPage.setMainObjectId(pageId);

    ButtonLink custom = ButtonLink.getTextualInstance(I18n.get("CUSTOMIZE_THIS_%%_PAGE", p.getName()), customPage);

  %>
    <td><%custom.toHtml(pageContext);%></td>
    <%

    %>
    <td><%ButtonLink.getTextualInstance(p.getName(), see).toHtml(pageContext);%></td>
    <%

    %>
    <td><%=I18n.get(p.getDescription())%>
    </td>
    <%

      cf.buttonValue = p.getName() + ".page";
    %>
    <td align="center"><%cf.toHtmlI18n(pageContext);%></td>
    <%

      if (logged.hasPermissionAsAdmin()) {
        sagh.buttonValue = p.getName() + ".page";
    %>
    <td align="center"><%sagh.toHtmlI18n(pageContext);%></td>
    <%
      }

    %>
    <td align="center" width="5%">
      <%
        PageSeed ps = pageState.pageFromRoot("workgroup/workgroupPopupChoosePage.jsp");
        ps.addClientEntry("PAGE_NAME", p.getName());
        ps.command = "POPULATE";
        ps.mainObjectId = p.getId();

        ButtonSupport bs =  ButtonLink.getBlackInstance(I18n.get("WORKGROUP"), 500, 700, ps);
        bs.iconChar="M";
        bs.label="";
        bs.toolTip=I18n.get("WORKGROUP");
        bs.toHtmlInTextOnlyModality(pageContext);
      %>
    </td>
  </tr>
  <%
      }

    }

  %></table>
<%

    ButtonBar bb = new ButtonBar();
    ButtonSubmit save =  ButtonSubmit.getSaveInstance(f, I18n.get("SAVE"));
    save.additionalCssClass="first big";
    bb.addButton(save);
    bb.toHtml(pageContext);
    f.end(pageContext);
  }

%>