<%@ page import="com.twproject.mobile.MobileAjaxController, com.twproject.operator.TeamworkOperator, net.sf.json.JSONArray, net.sf.json.JSONObject, org.jblooming.agenda.CompanyCalendar, org.jblooming.system.SystemConstants, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.constants.OperatorConstants, org.jblooming.waf.settings.ApplicationState,org.jblooming.waf.view.PageState, org.jblooming.waf.SessionState, org.jblooming.waf.settings.I18n" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  TeamworkOperator logged= (TeamworkOperator) pageState.getLoggedOperator();

  String twEmail = ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_FROM);
  boolean pop3isConf = JSP.ex(ApplicationState.getApplicationSetting(SystemConstants.FLD_POP3_HOST));

%>

<!DOCTYPE html>
<html>
<head>
  <title>Twproject</title>

  <meta http-equiv="Content-type" content="text/html; charset=utf-8">

  <meta http-equiv="content-type" content="text/html; charset=UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no, minimum-scale=1.0, maximum-scale=1.0"/>
  <meta name="apple-mobile-web-app-capable" content="yes"/>
  <meta name="apple-mobile-web-app-status-bar-style" content="black"/>
  <link rel="apple-touch-startup-image" href="images/splash-mobile-hand.png"/>
  <link rel="apple-touch-icon" href="images/apple-touch-icon.png"/>

  <link rel="stylesheet" href="css/mobile.css?_33=<%=ApplicationState.getBuild()%>" type="text/css"/>
  <link rel="stylesheet" href="css/twMobile.css?44_=<%=ApplicationState.getBuild()%>" type="text/css"/>

  <script src="../commons/js/jquery/jquery-2.1.3.min.js?_=<%=ApplicationState.getBuild()%>"></script>
  <script src="<%=request.getContextPath()+"/commons/js/jquery/ui/jquery-ui-1.10.4.min.js"%>?_=<%=ApplicationState.getBuild()%>"></script>
  <script src="../commons/js/jquery/jquery.livequery.1.1.1.min.js?_=<%=ApplicationState.getBuild()%>"></script>
  <script src="../commons/js/platform.js.jsp?isMobile=true&_=<%=ApplicationState.getBuild()%>"></script>

  <script src="../commons/js/i18nJs.jsp?_=<%=System.currentTimeMillis()%>"></script>

  <script src="js/iscroll.js?_=<%=ApplicationState.getBuild()%>"></script>
  <script src="js/jquery.touchSwipe.min.js?_=<%=ApplicationState.getBuild()%>"></script>
  <script src="js/mobileEngine.js?7"></script>
  <%--<script src="js/mobileEngine.js?_=<%=ApplicationState.getBuild()%>"></script>--%>

  <script src="<%=request.getContextPath()+"/commons/js/jquery/tinyTimers/jquery.tinyTimers.js?_=" + ApplicationState.getBuild()%>"></script>
  <script></script>
</head>
<body class="level<%=License.getLicense().level%>">

<%//todo si potrebbe condizionere il caricamento di una determinata parte con delle custom features%>
<%@include file="mobileHome.jsp" %>
<%@include file="mobileTask.jsp" %>
<%@include file="mobileResource.jsp" %>
<%@include file="mobileIssue.jsp" %>
<%@include file="mobileCalendar.jsp" %>
<%@include file="mobileDocument.jsp" %>
<%@include file="mobileWorklog.jsp" %>
<%@include file="mobileTimecounter.jsp" %>
<%@include file="mobileMessage.jsp" %>
<%@include file="mobileExpenses.jsp" %>
<%@include file="mobileAssignment.jsp" %>

</body>
</html>
