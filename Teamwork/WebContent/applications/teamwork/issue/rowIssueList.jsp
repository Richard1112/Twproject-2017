<%@ page import="com.twproject.task.Issue, com.twproject.waf.html.IssueDrawer, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.view.PageState, org.jblooming.waf.html.display.Paginator" %><%

PageState pageState = PageState.getCurrentPageState(request);
  JspHelper rowDrawer = (JspHelper) JspIncluderSupport.getCurrentInstance(request);

  if ("DRAW_PAGE_FOOTER".equals(request.getAttribute(Paginator.ACTION))) {
    %><tfoot>
<tr><td id="bulkPlace" colspan="99"></td></tr>
</tfoot>
<%
  } else {
    String issueId = (String) ((Object[]) rowDrawer.parameters.get("ROW_OBJ"))[0];
    int rowCount=(Integer)rowDrawer.parameters.get("ROW_COUNT");

    // A T T E N Z I O N E !!!!!!!!!!!!!!!!!!!!!!!
    // per ottimizzare l'impressione di velocità si fanno disegnare solo le prime x righe complete con il drawer,
    // poi invece della linea si inserisce un tr placeholder con issueId e classe giusta e chiamiamo il refresh che farà le sue n chiamate con comodo

    if (rowCount<10) {
      Issue issue = Issue.load(issueId);
      if (issue == null)
        return;

      IssueDrawer issueDrawer = new IssueDrawer(null);
      issueDrawer.dragEnabled = true;
      issueDrawer.readMode = true;

      issueDrawer.drawIssue(issue, pageContext);
    } else {
      %><tr class="issueRow" issueId="<%=issueId%>"><td colspan="99"><script>refreshIssueRow("<%=issueId%>",0,false);</script></td></tr><%
    }
  }
%>