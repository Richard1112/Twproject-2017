<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.resource.Person, com.twproject.security.TeamworkArea, com.twproject.waf.TeamworkHBFScreen, org.jblooming.waf.ScreenArea, org.jblooming.waf.view.PageState" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {

    pageState.screenRunning = true;

    final ScreenArea body = new ScreenArea(request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);

  } else {


%>


<div id="wp_buyTeamwork" class="mainColumn" style="width: 100%; padding:20px 60px; text-align: center">

  <h1>Twproject Demo</h1>



  <h2> This is a <b>free</b> trial version of Twproject with <b>unlimited</b> projects and users.</h2>


  <h2><b>Trial</b> version is shared between thousands of people trying Twproject like you, each one in its own "sandbox".</h2>



  <%
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    Person p = logged.getPerson();
    TeamworkArea a = TeamworkArea.load(p.getArea().getId());

  %>


  <div style="">
    <h2>Your trial period is <%= a.getExpiry()!=null && a.getExpiry().getTime()<System.currentTimeMillis()?"expired":"about to expire"%>, but there is no reason to stop using Twproject!</h2>
    <h2>Get right now Twproject on your dedicated server!</h2>
    <br><br>
    <a class="button noprint first big" href="http://twproject.com/prices/#tab-id-2">YES, I want it!</a>
    <br><br>
  </div>


  <i>To keep this demo simple, we have disabled a few "administrator" tools. The full set of features will be, of course, available in your working environment.</i>

  <div align="right" style="margin-right:30px;margin-top: 30px;"><a href="mailto:info@twproject.com"><span class="teamworkIcon" style="font-size: 40px; color:#444"></span> <i>Need help? Contact us</i></a> </div>

</div><%

  }

%>


