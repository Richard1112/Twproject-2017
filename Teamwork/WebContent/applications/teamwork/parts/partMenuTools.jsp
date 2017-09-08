<%@ page import="com.twproject.operator.TeamworkOperator, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.container.DivOnMouseover, org.jblooming.waf.html.display.Img, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, org.jblooming.waf.view.PageSeed, com.opnlb.website.security.WebSitePermissions, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.utilities.JSP, com.twproject.document.DocumentBricks, org.jblooming.remoteFile.FileStorage, com.twproject.security.TeamworkPermissions, java.util.List, org.jblooming.waf.html.core.TextEmbedder, org.jblooming.waf.settings.ApplicationState" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();


  ButtonLink toolsIntro = new ButtonLink(pageState.pageFromRoot("tools/toolsIntro.jsp"));
  toolsIntro.label = I18n.get("TOOLS_MENU");

  toolsIntro.additionalCssClass = "hasDivomo";


  DivOnMouseover domo = new DivOnMouseover(null, toolsIntro, true);


  {
    domo.addButton(toolsIntro);
  }
 {
    ButtonLink listBoard = new ButtonLink(pageState.pageFromRoot("board/boardList.jsp"));
    listBoard.label = I18n.get("BOARDS");
    listBoard.pageSeed.setCommand(Commands.FIND);
    domo.addButton(listBoard);
  }
  {
    ButtonSupport buttonInBlack = ButtonLink.getBlackInstance(I18n.get("COUNTERS"),600,600,pageState.pageFromRoot("tools/counters.jsp"));
    domo.addButton(buttonInBlack);
  }

  if (logged.hasPermissionFor(TeamworkPermissions.fileStorage_canRead)) {
      ButtonLink fSlist = new ButtonLink(pageState.pageFromRoot("document/fileStorageList.jsp"));
      fSlist.label = I18n.get("FILESTORAGE_LIST");
      domo.addButton(fSlist);
  }


  //------------------------------- admin

  if (logged.hasPermissionAsAdmin()) {

    domo.addSeparator();

    PageSeed ass = pageState.pageFromRoot("administration/administrationIntro.jsp");
    ass.setCommand(Commands.EDIT);
    ButtonLink bl = new ButtonLink(ass);
    bl.label = I18n.get("ADMINISTRATION_ROOT_MENU");
    bl.iconChar="g";
    domo.addButton(bl);
  }

  domo.addSeparator();

  //------------------------------- favorites
  if (logged.getFavoriteUrls() != null && logged.getFavoriteUrls().size() > 0) {

    domo.addButton(new TextEmbedder("<span class='menuTitle'>"+I18n.get("FAVORITES")+"</span>"));
//    domo.addSeparator();
    for (String label : logged.getFavoriteUrls().keySet()) {

      PageSeed ps = new PageSeed(logged.getFavoriteUrls().get(label));
      ButtonLink bl = new ButtonLink("- "+label, ps);
      bl.inhibitParams=true;
      if (!ps.getHref().startsWith("/"))  // open external link on a new page
        bl.target="_blank";
      domo.addButton(bl);
    }
    domo.addSeparator();

    ButtonLink bl = new ButtonLink(I18n.get("MANAGE_FAVORITES") + "&nbsp;", pageState.pageFromRoot("tools/manageFavorites.jsp"));
    bl.iconChar="g";
    domo.addButton(bl);
    domo.addSeparator();
  }


  ButtonJS addFav = new ButtonJS(I18n.get("ADD_VIEW_TO_FAVORITES"), "saveThisPageAsPreferred()");
  addFav.iconChar="P";
  domo.addButton(addFav);







  domo.additionalDropDownClass="divDark";
  domo.toHtml(pageContext);
%>
