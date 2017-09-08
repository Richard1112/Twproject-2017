<%@ page import="com.twproject.worklog.WorklogStatus, com.twproject.worklog.businessLogic.WorklogBricks, org.jblooming.oql.OqlQuery, org.jblooming.waf.view.PageState, java.util.List, org.jblooming.waf.settings.I18n" %><%
  PageState pageState= PageState.getCurrentPageState(request);
%>
<table align="left" cellspacing=5 class="legendaPlan"><tr><td><i><%=I18n.get("LEGENDA")%>:&nbsp;</i></td><td><%=WorklogBricks.drawStatus(null,pageState)%></td><td><%=I18n.get("WORKLOG_STATUS_NONE")%></td>
  <%
    List<WorklogStatus> wls=new OqlQuery("select wl from "+ WorklogStatus.class.getName()+" as wl order by wl.intValue").list();
    for (WorklogStatus i : wls) {
      %><td><%=WorklogBricks.drawStatus(i,pageState)%></td><td><%=i.getName()%></td><%
    }
    

  %>
</tr>
</table>
