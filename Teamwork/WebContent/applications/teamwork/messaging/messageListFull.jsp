<%@ page import="com.twproject.waf.TeamworkHBFScreen, org.jblooming.waf.ScreenArea, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);

    pageState.perform(request, response).toHtml(pageContext);

  } else {


%>

<h1><%=I18n.get("MESSAGES")%></h1>

<div id="messageListPlaceFull" class="messageListPlace" >
</div>

<script>

  $(function(){
    $("#messageListPlaceFull").load(contextPath + "/applications/teamwork/messaging/messageList.jsp?isInPage=true");
  })


</script>


<%
  }
%>
