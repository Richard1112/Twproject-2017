<%@ page import="com.opnlb.website.page.WebSitePageBricks,
                  com.opnlb.website.portlet.Portlet,
                  org.jblooming.tracer.Tracer,
                  org.jblooming.waf.html.input.TextField,
                  org.jblooming.waf.view.PageState, org.jblooming.utilities.JSP" %><%

  PageState pageState = PageState.getCurrentPageState(request);

  try {
    String portletId = WebSitePageBricks.preparePortletParam(pageState);
    String areaName = request.getParameter("AREANAME");
    String paramPrefix = Portlet.FLD_PT_PARAM_KEY_+ portletId;

%><table width="100%" border="0">
   <tr><td><%

     TextField title = new TextField("TEXT", "Title", paramPrefix+"_IFRAMETITLE", "&nbsp;", 40, false);
     title.toHtml(pageContext);

     TextField url = new TextField("TEXT", "External URL", paramPrefix+"_IFRAMEURL", "&nbsp;", 40, false);
     url.toHtml(pageContext);


      %></td></tr>
  </table><%

} catch( Throwable t ) {
  out.write("WEBWORK_ERROR: " + t);
  Tracer.platformLogger.error("docList_param : Exception"+t.getClass().getName()+" : "+t.getMessage() , t);
  throw t;
}
%>