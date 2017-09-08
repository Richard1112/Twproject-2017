<%@ page import="org.jblooming.persistence.hibernate.PersistenceContext, org.jblooming.tracer.Tracer, org.jblooming.utilities.JSP, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.container.Container, org.jblooming.waf.view.PageState, java.sql.Connection, java.sql.ResultSet, java.sql.Statement" %>
<div class="portletBox"><%
  PageState pageState = PageState.getCurrentPageState(request);
  PersistenceContext pc = null;
  pc = new PersistenceContext();
  Connection conn = null;
  Statement statement = null;
  ResultSet rs = null;
  try {
    pc = new PersistenceContext();
    conn = pc.session.connection();
    statement = conn.createStatement();
    rs = statement.executeQuery("select inserted,action,lastModifier from twk_worklog ");
    Container cont = new Container("My Result set");
    cont.overflow = "auto";
    cont.title = pageState.getI18n("My Result set");
    cont.start(pageContext);
%>
  <table style="border:solid lightblue 1px;" width="100%">
    <tr>
      <td style="border:solid lightblue 1px;">Inserted</td>
      <td style="border:solid lightblue 1px;">Action</td>
      <td style="border:solid lightblue 1px;">Last modifier</td>
    </tr>
    <%
      while (rs.next()) {
    %>
    <tr>
      <%
        for (int i = 1; i <= rs.getMetaData().getColumnCount(); i++) {
      %>
      <td style="border:solid lightblue 1px;">
        <%=JSP.w(rs.getObject(i))%>&nbsp;
      </td>
      <%
        }
      %>
    </tr>
    <%
      }
    %></table>
  <%
      cont.end(pageContext);
    } catch (Throwable t) {
      Tracer.desperatelyLog("", true, t);
    } finally {
      rs.close();
      statement.close();
      if (pc != null)
        pc.commitAndClose();
    }
  %>
  <li><%
    ButtonLink bldisabled = new ButtonLink("area management", pageState.pageFromRoot("security/area.jsp"));
    bldisabled.enabled = false;
    bldisabled.toHtmlInTextOnlyModality(pageContext);
  %></li>
</div>