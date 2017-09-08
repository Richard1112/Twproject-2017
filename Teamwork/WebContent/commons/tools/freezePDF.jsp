<%@ page import="org.jblooming.system.SystemConstants, org.jblooming.utilities.DateUtilities, org.jblooming.waf.settings.ApplicationState,
org.jblooming.waf.view.PageState, java.util.Date" %><%
  PageState pageState = PageState.getCurrentPageState(request);

  if (ApplicationState.serverURL == null || ApplicationState.serverURL.trim().length() == 0) {

%>
<hr>
<p align="center"><big><span class="warning">MUST set PUBLIC_SERVER_NAME in
  global settings to make PDF prints work.</span>&nbsp;</big></p>
<hr>
<%

} else {
  String redirURL = pageState.getEntry("REDIR_URL").stringValue();

    String prefix = pageState.getEntry("FREEZE_PREFIX").stringValue();
    String name = prefix+"_"+DateUtilities.dateToString(new Date(),"yyyyMMddHHmmssSSS")+".pdf";

 %><%@ taglib uri="/WEB-INF/tlds/pd4ml.tld" prefix="pd4ml" %><pd4ml:transform

        screenWidth="800"

        pageFormat="A4"

        pageInsets="20,20,20,20,points"

        enableImageSplit="false"

        adjustScreenWidth="true"

        encoding="default"

        >
  <pd4ml:usettf from="java:fonts" />
  <pd4ml:savefile dir="<%=ApplicationState.getApplicationSetting(SystemConstants.FLD_REPOSITORY_URL)%>"
                  name="<%=name%>"
                  redirect="<%=redirURL%>" />
  <% String ptp = pageState.getEntry("PAGE_TO_PRINT").stringValue(); %>
  <jsp:include page="<%=ptp%>"></jsp:include><%


%></pd4ml:transform><%

  }
%>