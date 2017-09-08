<%@ page import="org.jblooming.utilities.StringUtilities" %>
<html>
<body style="font-family: sans-serif">
<h3>Generate a random AES key</h3>
Copy the line below and then use it here: <a href="i18nManager.jsp?CM=FN&SEARCH_TEXT=CUSTOM_FEATURE_AES_CRYPTO&_FP_LANGUAGESEN=yes" target="_top">AES_CRYPTO_KEY</a>.
<b style="color: #ff8e63;">Keep a copy of your key in a safe place!</b>
<hr>
<%
  for (int i=1;i<2;i++){
%><p style="font-family: monospace; font-size: 18px;font-weight:bolder;" align="center"><%=StringUtilities.generateAESKey()%></p>
<%}%>
<hr>
<p align="right" onclick="location.reload()" style="cursor: pointer;">generate a new one</p>
</body>
</html>
