<%@ page import="com.twproject.waf.TeamworkLoginScreen,
                  org.jblooming.waf.html.core.JspIncluderSupport,
                  org.jblooming.waf.html.display.HeaderFooter,
                  org.jblooming.waf.view.PageState, org.jblooming.waf.html.display.Img"%><%

  TeamworkLoginScreen screen = (TeamworkLoginScreen)JspIncluderSupport.getCurrentInstance(request);

  PageState pageState = PageState.getCurrentPageState(request);

  HeaderFooter hf = pageState.getHeaderFooter();
  hf.toolTip = "teamwork";
  hf.meta="<meta name=\"viewport\" content=\"width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no\">\n" +
    "  <meta name=\"apple-mobile-web-app-capable\" content=\"yes\"/>\n" +
    "  <meta name=\"apple-mobile-web-app-status-bar-style\" content=\"black\"/>\n" +
    "\n" +
    "  <link rel=\"apple-touch-icon\" href=\"images/apple-touch-icon.png\"/>";
  hf.header(pageContext);

%>
<style type="text/css">

  html {
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
  }

  body {
    background-color: #356a8c;
  }

</style>
<div id="__FEEDBACKMESSAGEPLACE" style="display:none;"></div>


<div class="loginScreenContent">
  <div class="headingLogo"><%new Img("logo_login.png", "", "", "").toHtml(pageContext);%></div>

  <div class="loginBox">
    <div class="accessWrapper">
      <%
        screen.getBody().toHtml(pageContext);
        pageState.getHeaderFooter().footer(pageContext);
      %>
    </div>
  </div>
</div>
