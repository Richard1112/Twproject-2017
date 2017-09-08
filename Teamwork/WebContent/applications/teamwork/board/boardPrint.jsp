<%@ page
        import="com.twproject.messaging.board.Board, com.twproject.messaging.stickyNote.StickyNote, com.twproject.security.TeamworkPermissions, com.twproject.waf.TeamworkPopUpScreen, org.jblooming.persistence.PersistenceHome, org.jblooming.system.SystemConstants, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.SessionState, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.container.DivOnMouseover, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.Img, org.jblooming.waf.html.layout.HtmlColors, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea("/applications/teamwork/board/boardPrint.jsp");
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);

  } else {

    Board board = (Board) PersistenceHome.findByPrimaryKey(Board.class, pageState.mainObjectId);
    board.testPermission(pageState.getLoggedOperator(), TeamworkPermissions.board_canRead);

    PageSeed seed = pageState.thisPage(request);
    seed.setMainObjectId(board.getId());
    seed.setCommand(pageState.command);
    Form f = new Form(seed);
    pageState.setForm(f);
    f.start(pageContext);

    Img log = new Img(ApplicationState.getApplicationSetting(SystemConstants.PRINT_LOGO), "");
%>
<table border="0" width="99%" align="center" cellpadding="3" cellspacing="0" class="noprint">
    <tr>
       <td align="left" width="90%"><%log.toHtml(pageContext);%></td>
      <td align="right"><%

        ButtonJS print = new ButtonJS("window.print();");
        print.label = "";
        print.toolTip = I18n.get("PRINT_PAGE");
        print.iconChar = "p";
        print.toHtmlInTextOnlyModality(pageContext);

      %></td>


    </tr>
</table>

<div id="printFilter" style="visibility:visible;"></div>
<table width="100%" align="center"><tr><td align="center" width="100%" style="text-align: left"><%
    for (StickyNote sn : board.getStickyNotes()){
      %><div style="margin-top:10px; background-color:<%=sn.getColor()%>;padding: 2px 5px; color:<%=HtmlColors.contrastColor(sn.getColor())%>"><%=JSP.w(sn.getCreationDate())%> - <%=sn.getAuthor().getDisplayName()%></div>
      <big><b><%=JSP.w(sn.getTitle())%></b></big><br>
      <%=JSP.w(sn.getMessage())%><%
    }
 %></td></tr></table><%
    f.end(pageContext);

  }

%>
