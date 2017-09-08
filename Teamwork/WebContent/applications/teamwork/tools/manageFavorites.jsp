<%@ page import="com.twproject.operator.TeamworkOperator,
                 com.twproject.waf.TeamworkHBFScreen,
                 org.jblooming.operator.Operator,
                 org.jblooming.persistence.PersistenceHome,
                 org.jblooming.security.businessLogic.LoginAction,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  Operator logged = pageState.getLoggedOperator();

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;


    // this page is free so must check for command correctly and eventually redirect to login
    if (logged == null) {
      if (("POSTLINK".equals(pageState.command) || "VIEWLINK".equals(pageState.command))  && pageState.getEntry("USR").stringValueNullIfEmpty() != null) {
        TeamworkOperator logCand = (TeamworkOperator) PersistenceHome.findByPrimaryKey(TeamworkOperator.class, pageState.getEntry("USR").stringValueNullIfEmpty());
        if (logCand.getPassword().equals(pageState.getEntry("CK").stringValueNullIfEmpty()))
          LoginAction.doLog(logCand, pageState.sessionState);
      }
    }

    // if logged is still null redirect
    if (pageState.getLoggedOperator() == null)
      response.sendRedirect(ApplicationState.serverURL+"/applications/teamwork/security/login.jsp");
    

    final ScreenArea body = new ScreenArea(request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);


    pageState.toHtml(pageContext);
  } else {
%> <script>$("#HOME_MENU").addClass('selected');</script>
 <style type="text/css">

   .bookmarksDiv, .customPagesDiv, .bookmarkletDiv {
     width:30%;
   }

 </style>
<div class="inlineContainerWrapper">
  <jsp:include page="../portal/portlet/wp_links.jsp" />
  <jsp:include page="../portal/portlet/wp_customizablePages.jsp" /><%


  if (logged!=null){
    %><div class="bookmarkletDiv">
  <h1><%=I18n.get("BOOKMARKLET")%></h1>
      <%=I18n.get("LINK_TO_BROWSER_TOOLBAR_ADD_DESCRIPTION")%>:
      <a class="drag" href="javascript:location.href='<%=ApplicationState.serverURL%>/applications/teamwork/tools/manageFavorites.jsp?<%=Commands.COMMAND%>=POSTLINK&USR=<%=logged.getId()%>&URL='+encodeURIComponent(location.href)+'&TITLE='+encodeURIComponent(document.title)+'&CK=<%=logged.getPassword()%>'"><%=I18n.get("LINK_TO_BROWSER_TOOLBAR_ADD")%></a>
      <br><br>
      <%=I18n.get("LINK_TO_BROWSER_TOOLBAR_VIEW_DESCRIPTION")%>:
      <a class="drag" href="javascript:location.href='<%=ApplicationState.serverURL%>/applications/teamwork/tools/manageFavorites.jsp?<%=Commands.COMMAND%>=VIEWLINK&USR=<%=logged.getId()%>&CK=<%=logged.getPassword()%>'"><%=I18n.get("LINK_TO_BROWSER_TOOLBAR_VIEW")%></a><br>
      </div>
    <%
  }

  %></div><%
  }
%>
