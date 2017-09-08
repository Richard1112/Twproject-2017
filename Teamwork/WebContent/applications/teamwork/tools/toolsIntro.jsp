<%@ page import="com.opnlb.website.security.WebSitePermissions, com.twproject.document.DocumentBricks, com.twproject.messaging.board.Board, com.twproject.messaging.board.businessLogic.BoardAction, com.twproject.operator.TeamworkOperator, com.twproject.security.TeamworkPermissions, org.jblooming.remoteFile.Document, org.jblooming.remoteFile.FileStorage,
org.jblooming.utilities.JSP, org.jblooming.utilities.file.fileStorage.FileStorageUtilities, org.jblooming.waf.ScreenBasic, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.Container, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List" %><%

  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    ScreenBasic.preparePage(pageContext);
    pageState.perform(request, response).toHtml(pageContext);
  } else {

%>

<script>$("#TOOLS_MENU").addClass('selected');</script>


<div class="mainColumn toolsIntro">
<style type="text/css">
.teamworkIcon {
  font-size: 160%;
}
</style>
<div class="inlineContainerWrapper">

<div style="width: 30%" class="container ">


<table width="100%" border="0" cellpadding="2" class="panel">
  <tr>
    <td valign="top">
      <h2><%=I18n.get("BOARDS_MESSAGES")%></h2>
      <ul><li><%
      ButtonLink listBoard = new ButtonLink(pageState.pageFromRoot("board/boardList.jsp"));
      listBoard.label = I18n.get("BOARD_LIST");
      listBoard.pageSeed.setCommand(Commands.FIND);
    %><%listBoard.toHtmlInTextOnlyModality(pageContext);%><%

      TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
      if (logged.hasPermissionFor(TeamworkPermissions.board_canCreate)) {
        PageSeed editor = pageState.pageFromRoot("board/boardEditor.jsp");
        editor.command=Commands.ADD;
        ButtonSupport addBl = ButtonLink.getBlackInstance(I18n.get("CREATE_NEW_BOARD"), 600, 600, editor);
        addBl.enabled = logged.hasPermissionFor(TeamworkPermissions.board_canCreate);
        addBl.additionalCssClass = "edit";
    %> - <%addBl.toHtmlInTextOnlyModality(pageContext);%><%
      }
      %><br><label><%=I18n.get("HELP_BOARDS")%></label>
      </li><li><%
        PageSeed pssm = pageState.pageFromRoot("messaging/composeMessage.jsp");
        ButtonSupport compose = ButtonLink.getBlackInstance(I18n.get("WORKGROUP_SEND_MESSAGE"),600,800, pssm);
      %><%compose.toHtml(pageContext);%><%
      %><br><label><%=I18n.get("HELP_SENDMESSAGE")%></label>
    </li></ul>

    </td></tr><tr><td valign="top">

  <h2><%=I18n.get("MY_BOARDS")%></h2>

  <ul><%

    List<Board> boardsL = BoardAction.findBoards(false);

    boolean printedSomething = false;
    for (Board board : boardsL) {
      if (!board.hasPermissionFor(logged, TeamworkPermissions.board_canRead))
        continue;

      ButtonJS nome = new ButtonJS(board.getName(), "openBoardInBlack("+board.getId()+");");
        %><li><%nome.toHtmlInTextOnlyModality(pageContext);%></li><%
      printedSomething =true;
    }

    if (!printedSomething) {
  %><%=I18n.get("BOARDS_NONE")%><%
    }

  %></ul>

  <%


    if (logged.hasPermissionFor(TeamworkPermissions.classificationTree_canManage) ) {


  %>
<hr>

  <table width="100%" border="0" cellpadding="2" class="panel">
    <tr>
      <td valign="top">
        <h2><%=I18n.get("COMPANY_NEWS")%></h2><%


        if (logged.hasPermissionFor(WebSitePermissions.news_canWrite)) {
      %>
        <%

          ButtonLink listNews = new ButtonLink(I18n.get("NEWS_MANAGEMENT"), new PageSeed(request.getContextPath() + "/applications/website/admin/news/newsList.jsp"));
          listNews.toHtmlInTextOnlyModality(pageContext);

        %> - <%

        PageSeed edit = new PageSeed(request.getContextPath()+"/applications/website/admin/news/newsEditor.jsp");
        edit.setCommand(Commands.ADD);
        ButtonLink addNews = new ButtonLink(I18n.get("INTRO_NEW_COMPANY"),edit);
        addNews.additionalCssClass = "edit";
        addNews.toHtmlInTextOnlyModality(pageContext);

      %><br><label><%=I18n.get("COMPANY_NEWS_HELP")%></label><br><%
        }


        if (!logged.hasPermissionAsAdmin() && logged.hasPermissionFor(TeamworkPermissions.system_canManageCalendar)) {
          ButtonLink holidays = new ButtonLink("Holidays", pageState.pageFromCommonsRoot("/administration/holidays.jsp"));
      %> - <% holidays.toHtmlInTextOnlyModality(pageContext);

      }

      %></td>
    </tr>
  </table><%

  }

%>
  </td>
  </tr>
</table>




</div>
<div style="width: 30%" class="container">

  <table width="100%" border="0" cellpadding="2" class="panel">
    <tr>
      <td valign="top">
        <h2><%=I18n.get("DOCUMENTS")%></h2><%

        ButtonLink buttonLink = new ButtonLink(I18n.get("DOCUMENTS"), pageState.pageFromRoot("document/documentList.jsp"));
        buttonLink.toHtmlInTextOnlyModality(pageContext);
      %>
        <label><%=I18n.get("DOCUMENTS_IN_TEAMWORK")%></label>
      </td>
    </tr>
  </table>

  <hr>



  <table width="100%" border="0" cellpadding="2">
    <tr>
      <td valign="top">
        <h2><%=I18n.get("COUNTERS")%></h2><%
        ButtonSupport buttonInBlack = ButtonLink.getBlackInstance(I18n.get("COUNTERS"),600,600,pageState.pageFromRoot("tools/counters.jsp"));
        buttonInBlack.toHtmlInTextOnlyModality(pageContext);
      %>
        <label><%=I18n.get("TEAMWORK_TOOLS")%></label>

      </td>
    </tr>
  </table>

<%


  if (logged.hasPermissionFor(TeamworkPermissions.classificationTree_canManage) ) {


%><hr>

<table width="100%" border="0" cellpadding="2" class="panel">
  <tr>
    <td valign="top">
      <h2><%=I18n.get("LOOKUPS")%></h2>

      <ul><li><%

        buttonInBlack = ButtonLink.getBlackInstance(I18n.get("TASK_TYPE_MENU"), pageState.pageFromRoot("task/taskType.jsp"));
        buttonInBlack.iconChar="t";
        buttonInBlack.toHtmlInTextOnlyModality(pageContext);
      %>&nbsp;-&nbsp;
      <%
      buttonInBlack = ButtonLink.getBlackInstance("CUSTOMER FIELD", pageState.pageFromRoot("task/taskImpact.jsp"));
      buttonInBlack.additionalCssClass="lreq30 lreqLabel";
      buttonInBlack.toHtmlInTextOnlyModality(pageContext);
      %>
      </li><li><%
      buttonInBlack = ButtonLink.getBlackInstance("AuditSubject",600,600, pageState.pageFromRoot("task/taskAuditSubjectEdit.jsp"));
      buttonInBlack.iconChar="i";
      buttonInBlack.toHtmlInTextOnlyModality(pageContext);
      %>
      </li><li><%

          buttonInBlack = ButtonLink.getBlackInstance(I18n.get("DEPARTMENT_TYPE_MENU"), pageState.pageFromRoot("resource/departmentType.jsp"));
          buttonInBlack.iconChar="M";
          buttonInBlack.toHtmlInTextOnlyModality(pageContext);
        %></li><li><%

      buttonInBlack = ButtonLink.getBlackInstance(I18n.get("ISSUE_STATUS_MENU"), pageState.pageFromRoot("issue/issueStatus.jsp"));
      buttonInBlack.iconChar="i";
      buttonInBlack.toHtmlInTextOnlyModality(pageContext);
    %>&nbsp;-&nbsp;<%
      buttonInBlack = ButtonLink.getBlackInstance(I18n.get("ISSUE_IMPACT_MENU"), pageState.pageFromRoot("issue/issueImpact.jsp"));
      buttonInBlack.additionalCssClass="lreq30 lreqLabel";
      buttonInBlack.toHtmlInTextOnlyModality(pageContext);
    %>&nbsp;-&nbsp;<%
      buttonInBlack = ButtonLink.getBlackInstance(I18n.get("ISSUE_TYPE_MENU"), pageState.pageFromRoot("issue/issueType.jsp"));
      buttonInBlack.additionalCssClass="lreq30 lreqLabel";
      buttonInBlack.toHtmlInTextOnlyModality(pageContext);

    %></li><li><%
      buttonInBlack = ButtonLink.getBlackInstance(I18n.get("COST_CLASSIFICATION_MENU"), pageState.pageFromRoot("task/costClassification.jsp"));
      buttonInBlack.iconChar="$";
      buttonInBlack.toHtmlInTextOnlyModality(pageContext);
    %>&nbsp;-&nbsp;<%
      PageSeed cagr = pageState.pageFromRoot("task/financial/costAggregatorManager.jsp");
      cagr.command = Commands.FIND;
      buttonInBlack = ButtonLink.getBlackInstance(I18n.get("COST_AGGREGATOR"), cagr);
      buttonInBlack.toHtmlInTextOnlyModality(pageContext);
    %></li><li><%
      buttonInBlack = ButtonLink.getBlackInstance(I18n.get("WORKLOG_STATUS"), pageState.pageFromRoot("task/worklog/worklogStatus.jsp"));
      buttonInBlack.iconChar="A";
      buttonInBlack.additionalCssClass="lreq20 lreqLabel";
      buttonInBlack.toHtmlInTextOnlyModality(pageContext);

    %></li><li><%
      buttonInBlack = ButtonLink.getBlackInstance(I18n.get("EVENT_TYPE_MENU"), pageState.pageFromRoot("agenda/eventType.jsp"));
      buttonInBlack.iconChar="m";
      buttonInBlack.toHtmlInTextOnlyModality(pageContext);
    %>&nbsp;-&nbsp;<%
      buttonInBlack = ButtonLink.getBlackInstance(I18n.get("DP_TYPE_MENU"), pageState.pageFromRoot("meeting/discussionPointType.jsp"));
      buttonInBlack.additionalCssClass="lreq20 lreqLabel";
      buttonInBlack.toHtmlInTextOnlyModality(pageContext);

    %>&nbsp;-&nbsp;<%
      buttonInBlack = ButtonLink.getBlackInstance(I18n.get("DISCUSSION_POINT_STATUS"), pageState.pageFromRoot("meeting/discussionPointStatus.jsp"));
      buttonInBlack.additionalCssClass="lreq20 lreqLabel";
      buttonInBlack.toHtmlInTextOnlyModality(pageContext);

    %></li></ul>
    <label><%=I18n.get("TEAMWORK_LOOKUPS")%></label>
    </td>
  </tr>
</table><%
  }
  %>


</div>
<div style="width: 30%" class="container">

  <table border="0" cellpadding="2" class="panel">
    <tr>
      <td valign="top">
        <h2><%=I18n.get("FILESTORAGE_MENU")%></h2><div class="lreq30 lreqLabel"><%

        List<FileStorage> fsdocs = DocumentBricks.getFileStoragesOrdered(logged, pageState);


        if (JSP.ex(fsdocs) && logged.hasPermissionFor(TeamworkPermissions.fileStorage_canRead)){
          ButtonLink list = new ButtonLink(pageState.pageFromRoot("document/fileStorageList.jsp"));
          list.label = I18n.get("FILESTORAGE_LIST");
          list.pageSeed.setCommand(Commands.FIND);
          %><%list.toHtmlInTextOnlyModality(pageContext);%> - <%
        }

          if (logged.hasPermissionFor(TeamworkPermissions.fileStorage_canCreate)) {
            PageSeed add = pageState.pageFromRoot("document/fileStorageEditor.jsp");
            add.setCommand(Commands.ADD);
            add.addClientEntry("DOCUMENT_TYPE", Document.IS_FILE_STORAGE);
            ButtonLink addBl = new ButtonLink(add);
            addBl.additionalCssClass = "edit";
            addBl.label = I18n.get("CREATE_FILE_STORAGE");


        %><%addBl.toHtmlInTextOnlyModality(pageContext);%><%
        }

        if (logged.hasPermissionFor(TeamworkPermissions.fileStorage_canRead)) {



      %><label><%=I18n.get("HELP_FILE_STORAGES")%></label><br>
        <ul><%
        PageSeed explore = pageState.pageFromRoot("document/explorer.jsp");
        explore.command = Commands.EDIT;
        PageSeed fsEdit = pageState.pageFromRoot("document/fileStorageEditor.jsp");
        fsEdit.command = Commands.EDIT;
        for (FileStorage document : fsdocs) {
          if (document.hasPermissionFor(logged, TeamworkPermissions.fileStorage_canRead)) {
            explore.mainObjectId = document.getId();
            fsEdit.mainObjectId = document.getId();
            ButtonSupport nome = ButtonLink.getBlackInstance(document.getName(), explore);
            nome.target = "winPopFS";
            nome.enabled = !Document.ConnectionType.FS.equals(document.getConnType()) || Document.ConnectionType.FS.equals(document.getConnType())&& FileStorageUtilities.validUrlToContent(document.getContent());
            nome.style="overflow: hidden; text-overflow: ellipsis; white-space: nowrap;width:300px";

            ButtonLink edit = new ButtonLink("",fsEdit);
            edit.iconChar="e";
            edit.additionalCssClass="small";

        %><li><%nome.toHtmlInTextOnlyModality(pageContext);%><span style="float: right"><%edit.toHtmlInTextOnlyModality(pageContext);%></span></li><%
          }
        }
      %></ul><%
      } else {
      %><br><label><%=I18n.get("FILESTORAGE_SEES_NONE")%></label><%
        }
      %>
        </div>
      </td></tr></table><%

  if (logged.isAreaManager() || logged.hasPermissionAsAdmin()) {
%><hr>

  <table class="panel">
    <tr>
      <td valign="top">
        <h2><%=I18n.get("TEAMWORK_SECURITY")%></h2><ul class="lreq30 lreqLabel lreqActive"><li><%

        ButtonLink roleManagement = new ButtonLink(I18n.get("ROLE_MANAGEMENT"), pageState.pageFromRoot("security/roleList.jsp"));
        roleManagement.toHtmlInTextOnlyModality(pageContext);
        %></li><%
        if (logged.hasPermissionAsAdmin()) {
          %><li><%
          ButtonLink areaManagement = new ButtonLink(I18n.get("AREA_MANAGEMENT"), pageState.pageFromRoot("security/area.jsp"));
          areaManagement.toHtmlInTextOnlyModality(pageContext);
          %></li><li><%

          new ButtonLink( I18n.get("SECURITY_WIZARD"), pageState.pageFromRoot("security/security.jsp")).toHtmlInTextOnlyModality(pageContext);
          %></li><li><%
          new ButtonLink( I18n.get("SECURITY_TEST"), pageState.pageFromRoot("administration/securityTest.jsp")).toHtmlInTextOnlyModality(pageContext);
          %></li><%
      }
        %></ul>
        <label><%=I18n.get("MANAGE_AREAS_ROLES")%></label>

      </td>

    </tr>
  </table><%

   }
 %>
</div>

</div>

</div><%


  }
%>

