<%@ page import="com.twproject.messaging.stickyNote.StickyNote, com.twproject.operator.TeamworkOperator, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.DivOnMouseover, org.jblooming.waf.html.display.Img, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState" %>
<%@ page import="org.jblooming.waf.html.core.TextEmbedder" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();



  Img img = logged.getPerson().bricks.getAvatarImage("");
  img.script="class='face'";

    %>
<div id="loggedOp" class="loggedOp">
<span><%//=img.toHtmlStringBuffer()%></span>
<%

  //ButtonJS opener = new ButtonJS("");
  //opener.label="<span><img class='face small' src='"+img.imageUrl+"'></span>";
//  opener.iconChar="[";

    ButtonJS opener = new ButtonJS("");
    opener.label="<span class='avatarImage'>"+img.toHtmlStringBuffer() + "</span><span class='loggedName'>"+logged.getPerson().getDisplayName()+"</span>";
    //opener.iconChar="[";
    opener.additionalCssClass="operatorButton";

  DivOnMouseover domo = new DivOnMouseover(opener);

  domo.addTextSeparator("<span class='menuTitle'>"+logged.getPerson().getDisplayName() + " <em>("+logged.getScore()+")</em></span>");

//  domo.addSeparator();

  {
    PageSeed edR = pageState.pageFromRoot("resource/resourceEditor.jsp");
    edR.setCommand(Commands.EDIT);
    edR.mainObjectId = logged.getPerson().getId();
    ButtonLink bl = new ButtonLink(edR);
    bl.label = I18n.get("PERSON_ANAG");
    domo.addButton(bl);
  }
  {
    PageSeed edRS = pageState.pageFromRoot("resource/resourceSecurity.jsp");
    edRS.setCommand(Commands.EDIT);
    edRS.mainObjectId = logged.getPerson().getId();
    ButtonLink bl = new ButtonLink(edRS);
    bl.label = I18n.get("PERSON_LOGIN");
    domo.addButton(bl);
  }
  {
    PageSeed opt = pageState.pageFromRoot("resource/resourceOptions.jsp");
    opt.setCommand("EDIT_OPT");
    opt.mainObjectId = logged.getPerson().getId();
    ButtonLink bl = new ButtonLink(opt);
    bl.label = I18n.get("PERSON_OPTIONS");
    domo.addButton(bl);
  }
  {
    PageSeed ass = pageState.pageFromRoot("resource/resourceAssignments.jsp");
    ass.setCommand(Commands.EDIT);
    ass.mainObjectId = logged.getPerson().getId();
    ButtonLink bl = new ButtonLink(ass);
    bl.label = I18n.get("RESOURCE_ASSIG_TAB");
    domo.addButton(bl);
  }
  {
    PageSeed ass = pageState.pageFromRoot("resource/resourceDocumentList.jsp");
    ass.setCommand("LIST_DOCS");
    ass.addClientEntry("RES_ID",logged.getPerson().getId());
    ButtonLink bl = new ButtonLink(ass);
    bl.label = I18n.get("RESOURCE_DOCUMENTS_TAB");
    domo.addButton(bl);
  }
/*
  {
    PageSeed ass = pageState.pageFromRoot("resource/resourceIssueList.jsp");
    ass.setCommand(Commands.EDIT);
    ass.mainObjectId = logged.getPerson().getId();
    ButtonLink bl = new ButtonLink(ass);
    bl.label = I18n.get("RESOURCE_ISSUE_TAB");
    domo.addButton(bl);
  }
*/

  domo.addSeparator();

  domo.addButton(new TextEmbedder("<span  class='menuTitle'>" + I18n.get("MESSAGES") + "</span>"));
//  domo.addSeparator();

  {
    PageSeed ps = new PageSeed("");
    ps.command = Commands.ADD;
    ButtonSupport cstickyL = new StickyNote().bricks.getEditorInBlack("", ps, pageState);
    cstickyL.label = I18n.get("SEND_STICKY");
    domo.addButton(cstickyL);
  }
  {
    PageSeed ps = new PageSeed("");
    ps = ps.addClientEntry("receiver", logged.getPerson().getId());
    ButtonSupport cstickyLM = new StickyNote().bricks.getEditorInBlack("", ps, pageState);
    cstickyLM.label = I18n.get("SEND_STICKY_TO_MYSELF");
    domo.addButton(cstickyLM);
  }

  domo.addSeparator();

  domo.addButton(ButtonLink.getDescriptiveLinkInstance(I18n.get("SUPPORT_MENU"),"http://twproject.com/support"));

  domo.addSeparator();

  {
    PageSeed logoff = pageState.pageFromRoot("security/login.jsp");
    logoff.setCommand(Commands.LOGOUT);
    ButtonLink bl = new ButtonLink(logoff);
    bl.target="_top";
    bl.label = I18n.get("LOGOFF_MENU");
    domo.addButton(bl);
  }

  domo.additionalDropDownClass="divDark";
  domo.toHtmlInTextOnlyModality(pageContext);
%>
</div>
