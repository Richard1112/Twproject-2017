<%@ page import="com.twproject.messaging.stickyNote.StickyNote,
                 com.twproject.messaging.stickyNote.StickyNoteDrawer,
                 org.jblooming.utilities.DateUtilities,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonSupport,
                 org.jblooming.waf.html.container.Container,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.html.layout.HtmlColors,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);

  // -------------------------------------------------------------------------------------------------------------------- Container DRAW

  StickyNoteDrawer stickyDrawer = (StickyNoteDrawer) JspIncluderSupport.getCurrentInstance(request);
  StickyNote stickyNote = stickyDrawer.stickyNote;

  String color = stickyNote.getColor();

  boolean isInBoard = stickyNote.getBoard() != null;

  Container cp = new Container(stickyNote.getId() + "");
  cp.commandSuffix = StickyNoteDrawer.STICKY_COMMAND_SUFFIX;
  cp.setCssPostfix("sticky");
  cp.draggable = true;
  cp.closeable = (isInBoard && !JSP.ex(stickyNote.getMessage())) || !isInBoard;
  cp.iconizable = true;
  cp.resizable = true;

  cp.collapsable = false;
  cp.color = HtmlColors.modifyLight(color, 20);
  cp.width = stickyNote.getW() + "px";
  cp.height = stickyNote.getH() + "px";
  cp.top = stickyNote.getY();
  cp.left = stickyNote.getX();
  cp.containment = stickyDrawer.containment;

  cp.width = stickyNote.getW() == 0 ? "300px" : stickyNote.getW() + "px";
  cp.height = stickyNote.getH() == 0 ? "150px" : stickyNote.getH() + "px";
  cp.status = stickyNote.isIconized() ? Container.ICONIZED : Container.DEFAULT;

  //cp.title = JSP.w(stickyNote.getType());

/*
  if (stickyNote.getAuthor()!=null){
    Img avatar = stickyNote.getAuthor().bricks.getAvatarImage();
    avatar.script="class='face'";
    cp.embeddedInTitle= avatar;
  }*/


  cp.title = "<span style='color:" + HtmlColors.contrastColor(color) + "'>" + DateUtilities.dateToString(stickyNote.getCreationDate(), "EEEE dd MM yyyy HH:mm") + " <br>" + (stickyNote.getAuthor() != null ? stickyNote.getAuthor().getDisplayName() : "Teamwork") + "</span>";


  PageSeed com_CE = new PageSeed("");
  com_CE.command = "REPLY";

  // REPLY
  if (stickyNote.getReceiver() != null && stickyNote.getAuthor() != null) {
    ButtonSupport cstickyL = stickyNote.bricks.getEditorInBlack("", com_CE, pageState);
    cstickyL.enabled = stickyDrawer.canWrite;
    cstickyL.toolTip = I18n.get("REPLY");
    cstickyL.label = "";
    cstickyL.iconChar = "N";
    cstickyL.outputModality = "textual";
    cp.titleRightElements.add(cstickyL);
  }

  // edit
  com_CE.command = Commands.EDIT;
  ButtonSupport edst = stickyNote.bricks.getEditorInBlack("", com_CE, pageState);
  edst.enabled = stickyDrawer.canWrite;
  edst.label = "";
  edst.toolTip = I18n.get("STICKY_EDIT");
  edst.iconChar = "e";
  edst.outputModality = "textual";
  cp.titleRightElements.add(edst);

  cp.start(pageContext);

%>
<div class="stsSubject" nowrap style="background-color: <%=HtmlColors.modifyLight(color,20)%>">
  <span style="color:<%=HtmlColors.contrastColor(color)%>;"><%=JSP.w(stickyNote.getTitle())%>&nbsp;</span>
</div>
<%--<div class="stsContent linkEnabled"><%=JSP.convertLineFeedToBR(JSP.wSmile(stickyNote.getMessage(), pageContext))%>--%>
<div class="stsContent linkEnabled"><%=stickyNote.getMessage()%></div>
<%
  cp.end(pageContext);
%>
