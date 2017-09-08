<%@ page import="com.twproject.messaging.board.businessLogic.BoardController, com.twproject.operator.TeamworkOperator, com.twproject.security.TeamworkPermissions, com.twproject.waf.TeamworkHBFScreen, org.jblooming.persistence.PersistenceHome, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.container.HeadBar, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.DataTable, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.button.ButtonLink" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {
    pageState.screenRunning = true;

    final ScreenArea body = new ScreenArea(new BoardController(), request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);

    pageState.toHtml(pageContext);

  } else {

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    PageSeed self = pageState.pageFromRoot("board/boardList.jsp");
    self.setCommand(Commands.FIND);
    Form f = new Form(self);
    pageState.setForm(f);

    f.start(pageContext);

%>

<%---------------------------------------------- MAIN COLUMN START ---------------------------------------------------------%>


<div class="mainColumn">

<script>$("#TOOLS_MENU").addClass('selected');</script>
<h1><%=I18n.get("BOARDS")%></h1>

<div class="optionsBar clearfix filterActiveElements"><%

  DataTable dataTable = new DataTable("ISSLH", f, new JspHelper("/applications/teamwork/board/rowBoardList.jsp"), BoardController.class, pageState);

  dataTable.addHeader(I18n.get("NAME"));
  dataTable.addHeader(I18n.get("DESCRIPTION"));
  dataTable.addHeader(I18n.get("POSTS"));
  dataTable.addHeader(I18n.get("LAST_POST"));
  dataTable.addHeader(I18n.get("ACTIVE"));
  dataTable.addHeaderFitAndCentered(I18n.get("DELETE_SHORT"));
  ButtonJS search = dataTable.getSearchButton();
  search.additionalCssClass = "small";

  %>
  <div class="filterElement centered"><%new TextField("SEARCH", "").toHtmlI18n(pageContext);%>&nbsp;&nbsp;<%search.toHtml(pageContext);%></div><%
  pageState.getEntryOrDefault("SHOW_NOTACTIVE_BOARDS");
  CheckField filterActive = new CheckField("SHOW_NOTACTIVE_BOARDS", "", false);
  %><div class="filterElement centered"><%filterActive.toHtmlI18n(pageContext);%></div>
</div><%
    dataTable.drawPaginator(pageContext);
    dataTable.drawTable(pageContext);
%></div>

<%---------------------------------------------- MAIN COLUMN END ---------------------------------------------------------%>

<%---------------------------------------------- RIGHT COLUMN START ---------------------------------------------------------%>

<div class="rightColumn noprint"><div class="tools">
  <div class="toolsElement"><%
    PageSeed editor = pageState.pageInThisFolder("boardEditor.jsp", request);
    editor.command=Commands.ADD;
    ButtonSupport addBl = ButtonLink.getBlackInstance("", 600, 800, editor);
    addBl.enabled = logged.hasPermissionFor(TeamworkPermissions.board_canCreate);
    addBl.additionalCssClass = "first";
    addBl.label = I18n.get("CREATE_NEW_BOARD");
    addBl.iconChar = "P";
    addBl.toHtml(pageContext);
  %></div>
</div></div>

<script>
  function deleteBoard(el){
    var row=el.closest("[boardId]");
    var boardId=row.attr("boardId");
    var request={CM:"DELBOARD","OBJID":boardId};
    //console.debug("deleteBoard",request);
    showSavingMessage();
    $.getJSON("boardAjaxController.jsp", request, function (response) {
      jsonResponseHandling(response);
      if (response.ok == true) {
        row.remove();
      }
      hideSavingMessage();
    });
  }
</script>


<%


    f.end(pageContext);

  }

%>
