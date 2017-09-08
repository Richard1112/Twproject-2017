<%@ page import="com.twproject.messaging.board.Board, com.twproject.messaging.stickyNote.StickyNote, com.twproject.operator.TeamworkOperator, com.twproject.resource.ResourceBricks,
com.twproject.waf.TeamworkPopUpScreen, org.jblooming.messaging.MailHelper, org.jblooming.messaging.SomethingHappened, org.jblooming.persistence.PersistenceHome, org.jblooming.utilities.JSP,
org.jblooming.waf.ActionUtilities, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit,
org.jblooming.waf.html.container.ButtonBar,
org.jblooming.waf.html.input.*, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed,
org.jblooming.waf.view.PageState, java.util.Date, java.util.Set"%><%

  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {


    TeamworkOperator operator = (TeamworkOperator) pageState.getLoggedOperator();

    String command = pageState.getCommand();


    StickyNote stik = null;
    stik = new StickyNote();
    stik.setIdAsNew();

    //use default operator color
    if(JSP.ex(operator.getOption("PREFERRED_COLOR"))) {
      stik.setColor(operator.getOption("PREFERRED_COLOR"));
    }


    if (Commands.SAVE.equals(command)) {
      if (PersistenceHome.NEW_EMPTY_ID.equals(pageState.getMainObjectId())) {
        stik.setAuthor(operator.getPerson());
      } else {
        stik = (StickyNote) PersistenceHome.findByPrimaryKey(StickyNote.class, pageState.mainObjectId);
      }

      ActionUtilities.setIdentifiable(pageState.getEntry("board"), stik, "board");
      ActionUtilities.setString(pageState.getEntry("subject"), stik, "title");

      ActionUtilities.setString(pageState.getEntry("STYCKY_COLOR"), stik, "color");

      String message = pageState.getEntry("messageBody").stringValue();
      stik.setMessage(JSP.limWr(message,4000));

      if(stik.getX() == 0 && stik.getY() == 0) {
        int x = (int) (200 + Math.floor(Math.random() * 200));
        int y = (int) (200 + Math.floor(Math.random() * 200));
        stik.setX(x);
        stik.setY(y);
      }

      Board board = stik.getBoard();
      if (board == null){
        ActionUtilities.setIdentifiable(pageState.getEntryAndSetRequired("receiver"), stik, "receiver");
        boolean sendMail=pageState.getEntry("SEND_MAIL").checkFieldValue();
        if (sendMail){
          String mailTo=stik.getReceiver().getDefaultEmail();
          String fromName=stik.getAuthor()==null?"":stik.getAuthor().getDisplayName();
          if (JSP.ex(fromName,mailTo)){
            Set<String> tos= MailHelper.mailTos(mailTo);
            MailHelper.sendHtmlMailInSeparateThread(fromName,tos,stik.getTitle(),stik.getMessage());
          }
        }
      }

      stik.store();

      if (board != null) {
        board.setLastPostedOn(new Date());
        board.store();

        SomethingHappened change = new SomethingHappened();
        change.setIdAsNew();
        change.setEventType("BOARD_POST_MODIFIED");
        change.setMessageTemplate("BOARD_POST_MODIFIED_MESSAGE_TEMPLATE");
        String boardName = board.getName();
        change.getMessageParams().put("board", boardName);
        change.getMessageParams().put("subject", stik.getTitle());
        change.getMessageParams().put("whom", pageState.getLoggedOperator().getDisplayName());
        change.setWhoCausedTheEvent(pageState.getLoggedOperator());
        PageSeed ps = new PageSeed(ApplicationState.serverURL + "/applications/teamwork/board/board.jsp");
        ps.setCommand(Commands.EDIT);
        ps.setMainObjectId(board.getId());
        ButtonLink edit = new ButtonLink(ps);
        edit.label = boardName;
        change.setLink(edit.toPlainLink());
        change.setIdentifiable(board);
        change.store();


%><script>getBlackPopupOpener().location.href="<%=ps.toLinkToHref()%>";</script><%

  }
%><script>closeBlackPopup(); </script><%

} else if ("REPLY".equals(pageState.getCommand())) {
  StickyNote stikOrig = (StickyNote) PersistenceHome.findByPrimaryKey(StickyNote.class, pageState.mainObjectId);
  stik.setReceiver(stikOrig.getAuthor());
  stik.setTitle("RE: " + stikOrig.getTitle());
  stik.setMessage(stikOrig.getMessage());
  stik.setColor(stikOrig.getColor());

} else if (Commands.EDIT.equals(pageState.getCommand())) {
  stik = (StickyNote) PersistenceHome.findByPrimaryKey(StickyNote.class, pageState.mainObjectId);

} else if ("ADD_TO_BOARD".equals(pageState.getCommand())) {
  ActionUtilities.setIdentifiable(pageState.getEntry("board"), stik, "board");

} else if ("CLOSE".equals(pageState.getCommand())) {
%><script>closeBlackPopup();</script><%

} else if (Commands.DELETE.equals(pageState.getCommand())) {
  stik = (StickyNote) PersistenceHome.findByPrimaryKey(StickyNote.class, pageState.mainObjectId);
  if (stik!=null){
%><script>getBlackPopupOpener().$("#<%=stik.getId()%>").remove(); closeBlackPopup(); </script><%
      stik.remove();
    }

  } else if (Commands.ADD.equals(pageState.getCommand())) {
    stik.setMessage(JSP.limWr(pageState.getEntry("messageBody").stringValueNullIfEmpty(),4000));
  }

  pageState.setMainObject(stik);

  // make
  if (stik.getReceiver() != null)
    pageState.addClientEntry("receiver", stik.getReceiver().getId());

  if (stik.getBoard()!=null)
    pageState.addClientEntry("board", stik.getBoard().getId());

  pageState.addClientEntry("subject", stik.getTitle());
  pageState.addClientEntry("STYCKY_COLOR", stik.getColor());
  pageState.addClientEntry("messageBody", stik.getMessage());



  //  StickyNote stik = (StickyNote) pageState.mainObject;
  boolean isSticky = stik.getBoard() == null;

  PageSeed self = pageState.thisPage(request);
  self.mainObjectId = stik.getId();
  self.setCommand(Commands.SAVE);
  self.setPopup(true);
  self.addClientEntry("board", pageState.getEntry("board").stringValueNullIfEmpty());

  Form form = new Form(self);
  form.encType = Form.MULTIPART_FORM_DATA;
  pageState.setForm(form);

  form.start(pageContext);


  //pageState.getEntryOrDefault("STYCKY_COLOR", "#FFFFAA");

  SmartCombo receiver = ResourceBricks.getPersonCombo("receiver", true,"",pageState);
  receiver.label = I18n.get("STICKY_RECEIVER");
  receiver.separator = "<br>";
  receiver.fieldSize = 29;
  receiver.required = isSticky;

  CheckField sendEmail=new CheckField(I18n.get("SEND_MAIL"),"SEND_MAIL","&nbsp;",false);

  ColorChooser colorChooser = new ColorChooser("STYCKY_COLOR");
  colorChooser.fieldSize = 2;

  TextField subject = new TextField("TEXT", "subject", "<br>", 50);
  subject.label = I18n.get("STICKY_MESSAGE_SUBJECT");
  subject.fieldClass+=" wide";

%> <h1><%=I18n.get(stik.isNew()?(isSticky ? "SEND_STICKY":"ADD_TO_BOARD"):"EDIT_MESSAGE")%></h1>
<table class="table">
  <%
    if (isSticky){
  %>
  <tr>
    <td ><%receiver.toHtml(pageContext);%></td>
  </tr>
  <%}%>

  <tr>
    <td><%subject.toHtml(pageContext);%></td>
  </tr><tr>
  <td style="padding-bottom: 15px">
    <%

      TinyMCE txtarea = new TinyMCE(I18n.get("DESCRIPTION"), "messageBody", "<br>", "100%", "210px", pageState);
      txtarea.setTheme(TinyMCE.THEME_SIMPLE);
      txtarea.showHTMLButton=true;
      txtarea.toHtml(pageContext);

    %>
  </td>
</tr>
  <tr><td><%colorChooser.toHtml(pageContext);%> <label><%=I18n.get("COLOR")%></label>&nbsp;&nbsp;&nbsp;<%sendEmail.toHtml(pageContext);%></td></tr>
</table>
<%

    ButtonBar bb=new ButtonBar();

    ButtonSubmit save = ButtonSubmit.getSaveInstance(form,I18n.get(stik.isNew()?"STICKY_SEND":"SAVE"));
    save.additionalCssClass="first";
    bb.addButton(save);


    if (!stik.isNew()){
      ButtonSubmit del= new ButtonSubmit(form);
      del.variationsFromForm.command=Commands.DELETE;
      del.additionalCssClass="delete";
      del.label=I18n.get("DELETE");
      bb.addButton(del);
    }

    bb.toHtml(pageContext);

    form.end(pageContext);
  }
%>
