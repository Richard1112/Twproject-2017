<%@ page import="org.jblooming.waf.ScreenBasic, org.jblooming.waf.html.container.Container, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState" %><%

  PageState pageState = PageState.getCurrentPageState(request);

    if (!pageState.screenRunning) {
      ScreenBasic.preparePage(pageContext);
      pageState.perform(request, response).toHtml(pageContext);
    } else {
      String type = pageState.getEntry("TYPE").stringValueNullIfEmpty();
      String cause = pageState.getEntry("CAUSE").stringValueNullIfEmpty();
      String ref = pageState.getEntry("REF").stringValueNullIfEmpty();
      String recode = type.substring(0, 1).toUpperCase() + "#" + ref + "#";


      if (!pageState.isPopup()&&"ISSUE".equalsIgnoreCase(type)){
        %><script>$("#ISSUES_MENU").addClass('selected');</script><%
      } else if ("TASK".equalsIgnoreCase(type)){
        %><script>$("#TASK_MENU").addClass('selected');</script><%
      } else if ("RESOURCE".equalsIgnoreCase(type)){
        %><script>$("#RESOURCE_MENU").addClass('selected');</script><%
      }

      %><div style="text-align: center; margin: 50px 30%"><%





%><h1><%=I18n.get("INVALID_REFERENCE") + ": " + recode%></h1>
<img src="<%=request.getContextPath()%>/img/invalidReference.png" alt="<%=I18n.get("INVALID_REFERENCE")%>" />
<h3><%=I18n.get(type)%>: <%=I18n.get(cause)%> </h3>
<%


      %></div><%
  }
%>
