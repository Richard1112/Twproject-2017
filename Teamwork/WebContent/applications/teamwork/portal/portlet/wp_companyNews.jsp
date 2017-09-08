<%@ page import="com.opnlb.website.news.News,
                 org.jblooming.ontology.PersistentFile,
                 org.jblooming.oql.OqlQuery,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.html.display.MultimediaFile,
                 org.jblooming.waf.settings.I18n,
                 java.util.Date, java.util.List"%><%@page pageEncoding="UTF-8" %><%
  // news layout normal
  OqlQuery oqltypes = new OqlQuery("from " + News.class.getName() + " as news where news.visible=:yes and (news.endingDate >= :ora or news.endingDate is null) and news.startingDate <= :ora" +
    " order by  news.orderFactor");
  oqltypes.getQuery().setBoolean("yes", true);
  oqltypes.getQuery().setTimestamp("ora", new Date());
  List<News> lists = oqltypes.list();

  if (!JSP.ex(lists))
    return;

  %>
<div class="portletBox small"><h1><%=I18n.get("COMPANY_NEWS")%></h1>


  <%for (News news: lists) {%>


  <div class="companyNews">
    <div style="font-size: 12px"><%=JSP.w(news.getStartingDate())%></div>
    <div style="font-size:18px;"><%=JSP.w(news.getTitle())%></div>
    <div style="font-size:15px;"><%=JSP.w(news.getSubTitle())%></div>
    <div><%
        PersistentFile pf = news.getImage();
        if (pf != null) {
          MultimediaFile media = new MultimediaFile(pf, request);
          if (news.getImageWidth() != null && news.getImageWidth() > 0)
            media.width = news.getImageWidth() + "";

          media.style = "max-width:100%";
          media.toHtml(pageContext);
        }
      %><%=JSP.w(news.getText())%>
      </div>
  </div>

<%--
<table width="100%" border="0" cellpadding="0" cellspacing="0">
  <tr><td style="font-size: 12px"><%=JSP.w(news.getStartingDate())%></td></tr>
  <tr> <td style="font-size:18px;"><%=JSP.w(news.getTitle())%></td></tr>
  <tr> <td style="font-size:15px;"><%=JSP.w(news.getSubTitle())%></td></tr>
  <tr>
    <td valign="top"><div style="max-width: 320px; position: relative"><%
      PersistentFile pf = news.getImage();
      if (pf != null) {
        MultimediaFile media = new MultimediaFile(pf, request);
        if (news.getImageWidth() != null && news.getImageWidth() > 0)
          media.width = news.getImageWidth() + "";

        media.style = "max-width:100%";
        media.toHtml(pageContext);
    %></div><%
      }
    %><%=JSP.w(news.getText())%>
    </td>
  </tr>
  </table>
--%>
  <%}%>
</div>
