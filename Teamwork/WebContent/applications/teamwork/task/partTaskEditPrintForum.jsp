<%@ page import="com.opnlb.website.forum.ForumEntry, org.jblooming.utilities.JSP, org.jblooming.utilities.StringUtilities, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.core.JspIncluderSupport" %>
<%
  JspHelper container = (JspHelper) JspIncluderSupport.getCurrentInstance(request);
  int depth =  (Integer) container.parameters.get("depth");
  ForumEntry fec = (ForumEntry) container.parameters.get("forum");

%><tr>
      <td align="left"><%=StringUtilities.getRepeated("&nbsp;", depth * 4)%><%=fec.getAuthor() != null ? fec.getAuthor().getDisplayName() : "anonymous"%></td>
      <td align="center" nowrap><%=JSP.w(fec.getTitle())%></td>
      <td align="left" ><%=JSP.w(fec.getContent())%></td>
      <td align="center" nowrap><%=JSP.w(fec.getLastPostOnBranch())%></td>
      <td align="center" nowrap><%=JSP.w(fec.getLastPosterOnBranch())%></td>
      </tr>