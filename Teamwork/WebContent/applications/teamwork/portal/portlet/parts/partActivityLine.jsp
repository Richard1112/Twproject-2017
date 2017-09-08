<%@ page import="com.twproject.rank.EntityGroupRank, org.jblooming.ontology.Identifiable, org.jblooming.waf.Bricks, org.jblooming.waf.EntityViewerBricks, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.view.PageState, java.util.List" %>
<%
  JspHelper drawer = (JspHelper) JspIncluderSupport.getCurrentInstance(request);
  String label = (String) drawer.parameters.get("label");
  List<EntityGroupRank> egrs = (List<EntityGroupRank>) drawer.parameters.get("egrs");

  PageState pageState = PageState.getCurrentPageState(request);

%><h5><%=label%></h5>
  <div class="linkForEntity">
    <%
      for (EntityGroupRank eGR : egrs) {
        Identifiable is = eGR.getEntity();
        EntityViewerBricks.EntityLinkSupport edi = Bricks.getLinkSupportForEntity(is, pageState);
        if (edi !=null && edi.getButton()!=null ){
          edi.getButton().iconChar="";
          %><p class="activityLine" style=""><%edi.bs.toHtmlInTextOnlyModality(pageContext);%></p><%
      }
  }
  %></div><%


%>
