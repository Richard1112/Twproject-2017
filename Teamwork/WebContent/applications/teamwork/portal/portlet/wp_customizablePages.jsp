<%@ page import="org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.view.PageSeed, com.opnlb.website.page.WebSitePage, org.jblooming.waf.settings.I18n, java.util.List, org.jblooming.oql.OqlQuery" %>
<%
  /*
________________________________________________________________________________________________________________________________________________________________________


  CUSTOMIZABLE_PAGES

________________________________________________________________________________________________________________________________________________________________________

*/

  String hql = "from " + WebSitePage.class.getName() + " as page where page.name!='ROOT'";
  OqlQuery oql = new OqlQuery(hql);
  List<WebSitePage> wsp = oql.list();

  if (wsp.size() > 0) {


%><div class="customPagesDiv portletBox small">
  <h1><%=I18n.get("CUSTOMIZABLE_PAGES")%></h1><%

  for (WebSitePage p : wsp) {
    PageSeed see = new PageSeed(request.getContextPath() + "/" + p.getName() + ".page");
    ButtonLink line = new ButtonLink(p.getName(), see);

%><div class="linkLine"><%line.toHtmlInTextOnlyModality(pageContext);%></div><%

  }
%></div>
<%
  }
%>
