<%@ page
        import="com.twproject.operator.TeamworkOperator, com.twproject.rank.EntityGroupRank, com.twproject.rank.RankUtilities, org.jblooming.waf.view.PageState, java.util.Date, java.util.List" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  List<EntityGroupRank> egrs = RankUtilities.getEntityRankByFreshness(logged.getIntId(), new Date());

  %><div id="scrxx" style="position:absolute;"><jsp:include page="../portal/portlet/wp_activity.jsp" /></div>