<%@ page import="com.opnlb.website.page.WebSitePage, com.opnlb.website.util.TemplateManager,
com.opnlb.website.waf.WebSiteConstants, com.twproject.operator.TeamworkOperator, org.jblooming.operator.Operator, org.jblooming.oql.OqlQuery,
org.jblooming.persistence.PersistenceHome, org.jblooming.utilities.JSP, org.jblooming.waf.FrontControllerFilter, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.container.DivOnMouseover, org.jblooming.waf.html.core.TextEmbedder, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List" %><%

  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged= (TeamworkOperator) pageState.getLoggedOperator();

  String homePage = Operator.getOperatorOption(pageState.getLoggedOperator(), WebSiteConstants.HOME_PAGE);
  if(logged!=null && !logged.hasPermissionAsAdmin() && "firstStart.page".equalsIgnoreCase(homePage))
    homePage = "/getsThingsDone.page";

  PageSeed homePs = pageState.pageFromRoot(homePage);
  ButtonLink home = new ButtonLink(I18n.get("DASHBOARD_MENU"), homePs);

  home.additionalCssClass = "hasDivomo";

  DivOnMouseover domo = new DivOnMouseover(null, home, true);

//--------------------- CUSTOM PAGES

  domo.addButton(new TextEmbedder("<span class='menuTitle'>"+I18n.get("DASHBOARDS")+"</span>"));
//  domo.addSeparator();

  String pageName = request.getAttribute("pageName") + "";
  String pageId = TemplateManager.getIdFromPageName(pageName);

  WebSitePage wsp = (WebSitePage) PersistenceHome.findUniqueNullIfEmpty(WebSitePage.class, "id", pageId);

  if ("SET_AS_MY_HOME".equals(pageState.command)) {
    TeamworkOperator loggedOnDb = (TeamworkOperator) pageState.getLoggedOperator();
    loggedOnDb.putOption(WebSiteConstants.HOME_PAGE, wsp.getName() + ".page");
  }

  boolean isPage=pageState.href.endsWith(".page");
  if (!isPage) {
    String forwardUrl = (String) request.getAttribute("javax.servlet.forward.request_uri");
    if (JSP.ex(forwardUrl))
      isPage=forwardUrl.endsWith(".page");
  }

  boolean aintMyHome=false;
  // custom page menu start
  boolean showCustom = false;
  if (wsp != null) {
    showCustom = wsp.isCustomizable();
    aintMyHome = !(wsp.getName() + ".page").equals(logged.getOptionOrDefault(WebSiteConstants.HOME_PAGE));
  }

  //add all pages
  String hql = "from " + WebSitePage.class.getName() + " as page where page.name!='ROOT' and page.active=true";
  OqlQuery oql = new OqlQuery(hql);
  List<WebSitePage> wsps = oql.list();

  for (WebSitePage p : wsps) {
    if (p.hasPermissionToSee(logged)) {
      PageSeed see = new PageSeed(request.getContextPath() + "/" + p.getName() + ".page");
      ButtonLink buttonLink = new ButtonLink(JSP.ex(p.getFrontOfficeTitle()) ? p.getFrontOfficeTitle() : p.getName(), see);
      if (homePage.contains(p.getName() + FrontControllerFilter.page )) {
        buttonLink.hasFocus = true;
      }
      domo.addButton(buttonLink);
    }
  }


  if (isPage&&showCustom) {
    domo.addSeparator();
    PageSeed customPage = new PageSeed(request.getContextPath() + "/applications/website/admin/customizePage.jsp");
    customPage.addClientEntry("PAGEID", pageId);
    ButtonLink bl = new ButtonLink(I18n.get("CUSTOMIZE"), customPage);
    bl.iconChar = "g";
    bl.additionalCssClass="lreq20 lreqLabel lreqActive";
    domo.addButton(bl);
    //bl.toHtmlInTextOnlyModality(pageContext);
    domo.addSeparator();
  }

  if (wsp!=null&&isPage&&aintMyHome) {

    PageSeed thisPage = new PageSeed(request.getContextPath() + "/" + wsp.getName()+".page");//pageState.thisPage(request);
    thisPage.command = "SET_AS_MY_HOME";
    ButtonLink bl = new ButtonLink(I18n.get("SET_AS_MY_HOME"), thisPage);
    bl.iconChar="*";
    domo.addButton(bl);
    //bl.toHtmlInTextOnlyModality(pageContext);
  }

  {
    PageSeed helpAbout = new PageSeed(request.getContextPath() + "/applications/teamwork/documentation/documentation.jsp");
    ButtonLink bl = new ButtonLink(helpAbout);
    bl.label = I18n.get("ABOUT_MENU");
    domo.addButton(bl);
  }

  domo.additionalDropDownClass="divDark";

  domo.toHtml(pageContext);
%>



