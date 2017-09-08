<%@ page import="com.twproject.task.businessLogic.TimeCounterAction" %>
<%@ page import="com.twproject.waf.TeamworkPopUpScreen" %>
<%@ page import="org.jblooming.waf.ScreenArea, org.jblooming.waf.view.PageState" %>
<%
    PageState pageState = PageState.getCurrentPageState(request);

    pageState.setPopup(true);

    if (!pageState.screenRunning) {

        pageState.screenRunning = true;
        final ScreenArea body = new ScreenArea(request);
        //body.areaHtmlClass="lreq10 lreqPage";
        TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
        lw.register(pageState);
        pageState.perform(request, response);

        pageState.toHtml(pageContext);

    } else {
%>
<%---------------------------------------------- MAIN COLUMN START ---------------------------------------------------------%>
<div class="mainColumn lreq10 lreqLabel">
    <jsp:include page="partTimeCounter.jsp" />
</div>
<%---------------------------------------------- MAIN COLUMN END ---------------------------------------------------------%>

<script>
  $("#TIMESHEET_MENU").addClass('selected');
</script>

<%
    }
%>
