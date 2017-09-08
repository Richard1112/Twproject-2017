<%@ page import="org.jblooming.waf.settings.ApplicationState" %>
<!DOCTYPE html>
<html>
<head>
  <title>Teamwork</title>
  <meta http-equiv="Content-type" content="text/html; charset=utf-8">  
  <meta name="viewport" content="width=320, user-scalable=yes">
  <meta name="apple-mobile-web-app-capable" content="yes"/>
  <meta name="apple-mobile-web-app-status-bar-style" content="black"/>

  <link rel="apple-touch-icon" href="images/apple-touch-icon.png"/>

  <link rel="stylesheet" href="css/twMobile.css?_=<%=ApplicationState.getBuild()%>" type="text/css"/>

</head>
<body><%
  ApplicationState.applicationParameters.remove("FILTER_ERROR");
%>
<!-- -----------------------------------  buy page -------------------------------------- -->
<div data-role="page" id="buyPage" style="display:block">

  <div data-role="content">
    <div class="content" style="text-align:center">
        <img src="images/splash-mobile-hand.png" style="width:80%; margin: 50px auto; display: block ">

      <h2 style="margin-top:-80px;">Your license requires your attention.</h2>
      <p>Maybe it is expired or the number<br>of users exceed the license, or some <br>modules are not activated.</p>
        We suggest you to access your Twproject from a web browser and visit Twproject Shop.<br>

      <div align="center"><i>Thanks from Twproject staff</i></div>
      <br>
      <button class="first full" onclick="location.href='http://twproject.com/prices/'">Go to buy...</button>

    </div>
  </div>
</div>


<!-- loader -->
<div id="loader"></div>

<script type="text/javascript">

</script>


</body>
</html>
