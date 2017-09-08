<%@ page import="com.twproject.meeting.Meeting, com.twproject.messaging.board.Board, com.twproject.messaging.board.businessLogic.BoardController, com.twproject.messaging.stickyNote.StickyNote,
 com.twproject.messaging.stickyNote.StickyNoteDrawer, com.twproject.waf.TeamworkHBFScreen, com.twproject.waf.TeamworkPopUpScreen, org.jblooming.oql.QueryHelper, org.jblooming.utilities.JSP,
                 org.jblooming.waf.ScreenArea, org.jblooming.waf.ScreenBasic, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.DivOnMouseover, org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState,
                 java.util.Date, java.util.List, com.twproject.security.TeamworkPermissions" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new BoardController(), request);

    ScreenBasic lw;
    lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {

    Board board = (Board) pageState.getMainObject();

    PageSeed self = pageState.thisPage(request);
    self.setCommand(Commands.EDIT);
    self.mainObjectId = board.getId();
    Form f = new Form(self);
    pageState.setForm(f);
    f.start(pageContext);

%>
<style type="text/css">
  body {
    background-color: #2F2F2F;
    color: #FFFFFF;
  }

  .button.first:hover {
    background-color: #656565;
  }

  #twInnerContainerPopup{
    background: transparent;
  }

</style>

<%---------------------------------------------- MAIN COLUMN START ---------------------------------------------------------%>

<div class="board mainColumn">

  <div class="boardData">
    <h1><%=JSP.w(board.getName())%></h1>
    <span class="PathCode">B#<%=board.getId()%>#</span>
    <p class="boardDesc"><%=JSP.w(board.getDescription())%></p>
  </div>
  <div id="boardBox">
    <%
      for (StickyNote stickyNote : board.getStickyNotes()){
        StickyNoteDrawer snd = new StickyNoteDrawer(stickyNote);
        snd.belongsToBoard = true;
        snd.canWrite = board.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.board_canWrite);
        snd.containment="#boardBox";
        snd.toHtml(pageContext);
      }
    %>&nbsp;
  </div>

</div>
<%---------------------------------------------- MAIN COLUMN END ---------------------------------------------------------%>

<%---------------------------------------------- RIGHT COLUMN START ---------------------------------------------------------%>

<div class="rightColumn noprint boardColumn">
  <div class="tools">
    <%

      PageSeed ps = new PageSeed("");
      ps.addClientEntry("board", board.getId());
      ps.setCommand("ADD_TO_BOARD");
      ButtonSupport addSticky = new StickyNote().bricks.getEditorInBlack("",ps, pageState);
      addSticky.label=I18n.get("ADD_TO_BOARD");
      addSticky.enabled = !board.isNew() && board.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.board_canWrite);
      addSticky.iconChar="P";
      addSticky.additionalCssClass="first";
      %><div class="toolsElement"><%
      addSticky.toHtml(pageContext);
    %></div><%

      if (board!=null && !board.isNew()) {
        PageSeed print = pageState.pageInThisFolder("boardPrint.jsp", request);
        print.mainObjectId = board.getId();
        print.setCommand(Commands.EDIT);
        print.setPopup(true);
        ButtonJS bl = new ButtonJS( "centerPopup('" + print.toLinkToHref() + "', 'board', '800', '600', 'yes','yes')");
        bl.label = "";
        bl.toolTip = I18n.get("PRINT");
        bl.iconChar = "p";
  %><div class="toolsElement"><%
  bl.toHtml(pageContext);
  %></div><%
  }

      PageSeed editor = pageState.pageInThisFolder("boardEditor.jsp", request);
      editor.command=Commands.EDIT;
      editor.mainObjectId=board.getId();
      ButtonSupport blED = ButtonLink.getBlackInstance(I18n.get("EDIT_BOARD"), 600, 800, editor);
      blED.label = "";
      blED.toolTip = I18n.get("EDIT_BOARD");
      blED.iconChar = "e";
  %><div class="toolsElement"><%
  blED.toHtml(pageContext);
  %></div><%


      String query = "select meeting from " + Meeting.class.getName() + " as meeting";
      QueryHelper queryHelper = new QueryHelper(query);
      queryHelper.addOQLClause("meeting.board=:board", "board", board);
      List<Meeting> meetings = queryHelper.toHql().list();
      ButtonJS opener = new ButtonJS("");
      opener.iconChar = "g";
      opener.toolTip = I18n.get("AGENDA_SEE_MEETINGS");
      opener.label = "";
      DivOnMouseover dimo = new DivOnMouseover(opener);

      /*PageSeed editor = pageState.pageInThisFolder("boardEditor.jsp", request);
      editor.command=Commands.EDIT;
      editor.mainObjectId=board.getId();
      ButtonSupport blED = ButtonLink.getBlackInstance(I18n.get("EDIT_BOARD"), 600, 800, editor);

      dimo.addButton(blED);*/

      if (JSP.ex(meetings)) {
        for (Meeting meeting : meetings) {
          PageSeed psMeet = pageState.pageFromRoot("agenda/agendaEditor.jsp");
          psMeet.setCommand(Commands.EDIT);
          psMeet.mainObjectId = meeting.getEvent().getId();
          ButtonLink btMeeting = new ButtonLink(psMeet);
          Date interestingDate = meeting.getEvent().getSchedule().getPreviousDateBefore(new Date());
          if (interestingDate == null)
            interestingDate = meeting.getEvent().getSchedule().getNextDateAfter(new Date());
          if (interestingDate == null)
            interestingDate = meeting.getEvent().getSchedule().getStartDate();
          btMeeting.label = JSP.w(interestingDate)+ " - "+meeting.getEvent().getDisplayName();
          dimo.addButton(btMeeting);
        }
      }
  %><div class="toolsElement"><%
  dimo.toHtml(pageContext);
  %></div><%

  %>
  </div>
  <%--<div class="rightColumnInner"></div>--%>
</div>


<%
  f.end(pageContext);
%>
<script type="text/javascript">
  $(function(){
    $("#boardBox").height($(window).height-200);

  });
</script>


<%

  }
%>
