<%@ page import="com.twproject.messaging.board.Board,
                 com.twproject.messaging.board.businessLogic.BoardController,
                 com.twproject.security.SecurityBricks,
                 com.twproject.security.TeamworkPermissions,
                 com.twproject.waf.TeamworkPopUpScreen,
                 org.jblooming.messaging.MessagingSystem,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.ScreenBasic,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.container.ButtonBar,
                 org.jblooming.waf.html.display.DeletePreviewer,
                 org.jblooming.waf.html.input.CheckField,
                 org.jblooming.waf.html.input.Combo,
                 org.jblooming.waf.html.input.TextArea,
                 org.jblooming.waf.html.input.TextField,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new BoardController(), request);
    ScreenBasic lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {
    Board board = (Board) pageState.getMainObject();

    if (Commands.SAVE.equals(pageState.command)){
%><script>
  if (parent.location.href.indexOf("agenda")<0)
    getBlackPopupOpener().location.reload();
  closeBlackPopup({boardId:"<%=board.getId()%>",boardName:"<%=board.getName()%>"});
</script><%
  }


  ButtonBar bb2 = new ButtonBar();
  if (!board.isNew()) {
    bb2.loggableIdentifiableSupport = board;
  }
  pageState.setButtonBar(bb2);

  PageSeed self = pageState.thisPage(request);
  self.setCommand(Commands.EDIT);
  self.mainObjectId = board.getId();
  Form f = new Form(self);
  pageState.setForm(f);
  f.start(pageContext);
%>

<%---------------------------------------------- MAIN COLUMN START ---------------------------------------------------------%>

<div class="mainColumn">

  <h1><%=JSP.w(board.getName())%></h1>
  <span class="PathCode">B#<%=board.getId()%>#</span>
  - <span><%=board.getLastPostedOn() != null ? I18n.get("LAST_POST") + ":&nbsp;" + JSP.w(board.getLastPostedOn()) : I18n.get("NEVER_POSTED")%>&nbsp;<%=board.getLastPostedOn() != null ? " - "+I18n.get("POSTS") + ":&nbsp;" + JSP.w(board.getStickyNotes().size()) : ""%></span>

  <%----------------------------------------------------------------- EDITOR ---------------------------------------------------------%>

  <div id="boardEditor">
    <br>
    <table class="table" cellpadding="5" cellspacing="0"><%

      TextField name = new TextField("NAME", "<br>");
      name.required = true;
      name.fieldSize = 50;
      //l'edit della board lo fa solo chi le puoi aggiungere
      boolean boardCanCreate = board.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.board_canCreate);
      name.readOnly = !boardCanCreate;
    %>
      <tr>
        <td><%name.toHtmlI18n(pageContext);%></td>
      </tr>
      <tr>
        <%

          TextArea description = new TextArea("DESCRIPTION", "<br>", 50, 3, "");
          description.readOnly = !boardCanCreate;
          description.maxlength = 255;
        %>
        <td valign="top"><%description.toHtmlI18n(pageContext);%></td>
      </tr>
      <%

        Combo cb = SecurityBricks.getAreaCombo("AREA", board.isNew() ? TeamworkPermissions.board_canCreate : TeamworkPermissions.board_canCreate, pageState);
        cb.readOnly = !boardCanCreate;
        cb.separator = "<br>";
      %>
      <tr class="<%=SecurityBricks.isSingleArea()?"displayNone":""%>">
        <td><%cb.toHtmlI18n(pageContext);%></td>
      </tr>
      <%

        CheckField cf = new CheckField("BOARD_ACTIVE", "", false);
        cf.disabled = !boardCanCreate;
      %>
      <tr>
        <td><%cf.toHtmlI18n(pageContext);%></td>
      </tr>

      <tr><td>

        <%----------------------------------------------------------------- SUBSCRIPTIONS ---------------------------------------------------------%>

        <div id="boardSubscriptions" class="container level_2">
          <%=I18n.get("BOARD_SUBSCRIBE")%>:&nbsp;<%

          for (MessagingSystem.Media media : MessagingSystem.activeMedia) {

        %>&nbsp;<%
          String subscrField = "BOARD_SUBSCRIBE_" + media.toString().toUpperCase();

          CheckField mediaCf = new CheckField(subscrField, "", false);
          mediaCf.label = I18n.get(media.toString().toUpperCase());
          mediaCf.toHtml(pageContext);
        %>&nbsp;<%

          }%>

        </div>
      </td></tr>
    </table>
  </div>

    <%

  if (boardCanCreate){
    ButtonSubmit save = ButtonSubmit.getSaveInstance(pageState.getForm(), I18n.get("SAVE"));
    save.enabled = boardCanCreate;
    bb2.addButton(save);
  } else {

    ButtonSubmit  save = ButtonSubmit.getSaveInstance(pageState.getForm(), I18n.get("SAVE_SUBSCRIPTIONS"));
    save.variationsFromForm.setCommand("SAVE_SUBS");
    bb2.addButton(save);
  }

bb2.toHtml(pageContext);

  f.end(pageContext);


  }
%>
