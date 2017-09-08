<%@ page import="com.twproject.waf.TeamworkPopUpScreen"%>
<%@ page import="org.jblooming.operator.Operator"%>
<%@ page import="org.jblooming.waf.ScreenArea"%>
<%@ page import="org.jblooming.waf.SessionState"%>
<%@ page import="org.jblooming.waf.settings.I18n"%>
<%@ page import="org.jblooming.waf.view.PageState"%>
<%@page pageEncoding="UTF-8" %><%
  
  PageState pageState = PageState.getCurrentPageState(request);

  //verify permissions
  Operator logged = pageState.getLoggedOperator();
  if (logged==null || !logged.hasPermissionAsAdmin())
    throw new SecurityException(I18n.get("PERMISSION_LACKING"));  

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    //put controller !
    final ScreenArea body = new ScreenArea(request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);

  } else {

%><h2><%=I18n.get("TEMPLATE_INFO_TITLE")%></h2>

<p>
  <%=I18n.get("TEMPLATE_INFO")%>
</p>
      <!--

      <div class="headerHome" areaname="HEADER" custom="no"></div>
<div class="rightColumnHome" >
    <div areaname="RIGHT" custom="yes"></div>
    <div areaname="RIGHT_BOTTOM" custom="no"></div>
</div>
<div class="mainColumnHome">
    <div areaname="LEFT" custom="yes"></div>
    <div areaname="LEFT_BOTTOM" custom="no"></div>
</div>
<div class="bottomHome" areaname="BOTTOM" custom="no"></div>

      -->

      &lt;DIV <cite>areaname="HEADER" custom="yes"</cite>&gt;&lt;/DIV&gt;<br>
      &lt;DIV &gt;<br>
<div style="padding-left: 20px;">
      &lt;DIV <cite>areaname="RIGHT" custom="yes"</cite>&gt;&lt;/DIV&gt;<br>
      &lt;DIV <cite>areaname="RIGHT_BOTTOM" custom="yes"</cite>"&gt;&lt;/DIV&gt;<br>
  </div>
      &lt;/DIV&gt;<br>
      &lt;DIV&gt;<br>
<div style="padding-left: 20px;">

      &lt;DIV <cite>areaname="LEFT" custom="yes"</cite>&gt;&lt;/DIV&gt;<br>
      &lt;DIV <cite>areaname="LEFT_BOTTOM" custom="no"</cite>"&gt;&lt;/DIV&gt;<br>
  </div>
      &lt;/DIV&gt;<br>
      &lt;DIV <cite>areaname="BOTTOM" custom="no"</cite>&gt;&lt;/DIV&gt;<br>



<%


  }
%>
