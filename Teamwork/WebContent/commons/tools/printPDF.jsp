<%@ page import="org.jblooming.utilities.JSP,
                 org.jblooming.utilities.StringUtilities,
                 org.jblooming.waf.SessionState,
                 org.jblooming.waf.settings.ApplicationState,
                 org.jblooming.waf.view.PageState" %><%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!JSP.ex(ApplicationState.serverURL )) {

%>
<hr>
<p align="center"><big><span class="warning">MUST set PUBLIC_SERVER_NAME in
  global settings to make PDF prints work.</span>&nbsp;</big></p>
<hr>
<%

} else {


    /*
    MODIFICA ILARIA - 17/04/2007
    adjustScreenWidth="true" necessario per bord data entry per far rientrare tutta la schermata nel pdf
    */


%><%@ taglib uri="/WEB-INF/tlds/pd4ml.tld" prefix="pd4ml" %><%@ page contentType="text/html;charset=UTF-8" language="java" %><%
  
  String fnfa = pageState.getEntry("FILE_NAME_FOR_ATTACHMENT").stringValueNullIfEmpty();
if (JSP.ex(fnfa)) {
    fnfa = StringUtilities.replaceAllNoRegex(JSP.stripToLegal(fnfa)," ","_");
  }
  String ptp = pageState.getEntry("PAGE_TO_PRINT").stringValue();
  String fnfaBoolS = (fnfa != null ? "false" : "true");

  String pageOrientation=pageState.getEntry("pageOrientation").stringValueNullIfEmpty();
  pageOrientation=JSP.ex(pageOrientation)?pageOrientation:"portrait";
%><pd4ml:transform
        fileName="<%=(fnfa != null ? fnfa : StringUtilities.deCamel(ptp))%>"

        screenWidth="800"

        pageFormat="A4"

        pageInsets="20,20,20,20,points"

        enableImageSplit="false"

        adjustScreenWidth="true"

        encoding="default"

        pageOrientation="<%=pageOrientation%>"

        inline="<%=fnfaBoolS%>"
        ><jsp:include page="<%=ptp%>"></jsp:include><%


  %></pd4ml:transform><%
  }
%>