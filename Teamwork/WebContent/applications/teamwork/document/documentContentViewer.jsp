<%@ page import="com.twproject.document.TeamworkDocument,
                  com.twproject.waf.TeamworkPopUpScreen,
                  org.jblooming.persistence.PersistenceHome,
                  org.jblooming.utilities.JSP,
                  org.jblooming.waf.ScreenArea,
                  org.jblooming.waf.view.PageState, org.jblooming.waf.settings.I18n, org.jblooming.waf.html.button.ButtonJS" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {

    TeamworkDocument document = (TeamworkDocument) PersistenceHome.findByPrimaryKey(TeamworkDocument.class, pageState.getMainObjectId());

    %>
<span style="position:absolute;top:18px;right:40px;">
<%
  ButtonJS print = new ButtonJS("window.print();");
  print.label = "";
  print.toolTip = I18n.get("PRINT_PAGE");
  print.iconChar = "p";
  print.toHtmlInTextOnlyModality(pageContext);
  %>
</span>

<h1><%=document.getName()%></h1>
      <div class="linkEnabled"><%=JSP.w(document.getContent())%></div><%

    if(JSP.ex(document.getAuthor())){
      %><%=I18n.get("DOCUMENT_AUTHOR")%>: <%=document.getAuthor()%><br><%
    }
    if(JSP.ex(document.getAuthored())){
      %><%=I18n.get("DOCUMENT_AUTHORED")%>: <%=JSP.w(document.getAuthored())%><br><%
    }
  }
%>