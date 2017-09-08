<%@ page
  import="com.twproject.rank.Hit, org.jblooming.ontology.Identifiable, org.jblooming.persistence.PersistenceHome, org.jblooming.utilities.JSP, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.core.JspIncluderSupport, java.util.Date" %>
<%

  JspHelper rowDrawer = (JspHelper) JspIncluderSupport.getCurrentInstance(request);
  Hit hit = (Hit) rowDrawer.parameters.get("ROW_OBJ");
%>
<tr class="alternate">
  <td><%
    Identifiable is = PersistenceHome.findByPrimaryKey((Class<? extends Identifiable>) Class.forName(hit.getEntityClass()), hit.getEntityId());
    if (is != null) {
  %><%=is.getName()%><%
  } else {
  %>Hit instance damaged: entity does not exist.<%
    }
  %></td>
  <td><%=JSP.w(hit.getEntityClass())%></td>
  <td><%=JSP.w(hit.getEntityId())%></td>
  <td><%=JSP.w(hit.getOperatorId())%></td>
  <td><%=JSP.w(hit.getWeight())%></td>
  <td><%=JSP.w(new Date(hit.getWhen()))%></td>
</tr>
