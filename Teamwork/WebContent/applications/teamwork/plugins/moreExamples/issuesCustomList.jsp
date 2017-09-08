<%@ page import="com.twproject.task.Issue,
                 com.twproject.task.businessLogic.IssueAction,
                 com.twproject.waf.TeamworkPopUpScreen,
                 org.jblooming.system.SystemConstants,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.*,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.core.JspIncluder,
                 org.jblooming.waf.html.display.Img,
                 org.jblooming.waf.settings.ApplicationState,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState, java.util.List"
        %>
<%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8" %>
<%!

  public class IssueListPagePlugin extends PagePlugin {

    public boolean isVisibleInThisContext(PageState pagestate) {
      boolean ret = pagestate.href.indexOf("issueMulti") >= 0 && JSP.ex(pagestate.getPage());
      return ret;
    }

    public PageSeed getPageSeedForPlugin(PageState pageState) {
     PageSeed printCustom = new PageSeed(ApplicationState.contextPath+getFile().getFileLocation());
     printCustom.mainObjectId = pageState.mainObjectId;
     printCustom.setCommand(Commands.FIND);
     printCustom.addClientEntries(pageState.getClientEntries());
     printCustom.setPopup(true);
     return printCustom;
   }

    //you can eventualy overload thids method to have complete power on your hand
   /* public ButtonSupport getPluginLinkButton(PageState pageState){
      PageSeed seedForPlugin = getPageSeedForPlugin(pageState);
      boolean isPopup = seedForPlugin.isPopup();
      ButtonSupport bl;
      bl = new ButtonLink(pageState.getI18n(getName()), seedForPlugin);
      bl.target="_blank";
      return bl;
    }*/


  }
%><%

  if (JspIncluder.INITIALIZE.equals(request.getParameter(Commands.COMMAND))) {
    IssueListPagePlugin pp = new IssueListPagePlugin();
    pp.setPixelHeight(1024);
    pp.setPixelWidth(1280);

    PluginBricks.getPagePluginInstance("REPORTS", pp, request);
  } else {

    PageState pageState = PageState.getCurrentPageState(request);

    if (!pageState.screenRunning) {

      pageState.screenRunning = true;
      final ScreenArea body = new ScreenArea(request);
      ScreenBasic lw = new TeamworkPopUpScreen(body);
      lw.register(pageState);
      pageState.perform(request, response).toHtml(pageContext);

    } else {

      new Img(ApplicationState.getApplicationSetting(SystemConstants.PRINT_LOGO), "").toHtml(pageContext);


      IssueAction ra = new IssueAction(pageState);
      ra.cmdFind();

      %><h2>Issue custom  list</h2><%
      List<Object[]> os = pageState.getPage().getAllElements();
      for (Object[] o : os) {
         Issue issue = Issue.load((String)o[0]);
        %><h2>I#<%=issue.getId()%>#</h2><%=issue.getDisplayName()%><hr><%
      }

    }

  }

%>