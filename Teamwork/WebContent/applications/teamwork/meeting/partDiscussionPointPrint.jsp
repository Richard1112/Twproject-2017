<%@ page
    import="com.twproject.meeting.DiscussionPoint, org.jblooming.agenda.CompanyCalendar, org.jblooming.persistence.PersistenceHome, org.jblooming.remoteFile.Document, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  DiscussionPoint d = null;
    JspHelper dDrawer = (JspHelper) JspIncluderSupport.getCurrentInstance(request);
    d = (DiscussionPoint) dDrawer.parameters.get("discussionPoint");

  if (d != null) {
%>
<br>
<table border="0" width="99%" align="center" cellpadding="3" cellspacing="0">
  <tr>
    <td valign="top" colspan="3">
      <b><%=JSP.w(d.getTitle())%> (<%=d.getOrderBy()+1%>)</b>
    </td>
  </tr>
  <tr>
    <td colspan="3" style="border:1px solid #e0e0e0;padding:8px;">
      <%=d.getMinute() != null ? JSP.w(d.getMinute()) : "&nbsp;"%>
    </td>
  </tr>
  <tr class="alternate">
    <td valign="top" width="60%">
      <%=pageState.getI18n("DISCUSSION_POINT_LEAD")%>: <b><%=d.getLead() != null ? d.getLead().getDisplayName() : "&nbsp;"%></b>
    </td>
    <td valign="top" colspan="2">
      <%=pageState.getI18n("DISCUSSION_POINT_TASK")%>: <b><%=d.getTask() != null ? d.getTask().getName() : "&nbsp;"%>
      </b>
    </td>
  </tr>
  <tr class="alternate">
    <td valign="top">
      <%=pageState.getI18n("DURATION")%>: <b><%=d.getTimeScheduled() != 0 ? (DateUtilities.getMillisInHoursMinutes(d.getTimeScheduled()- CompanyCalendar.MILLIS_IN_HOUR)) : "&nbsp;"%>
      </b>
    </td>

    <td valign="top">
      <%=pageState.getI18n("STATUS")%>: <b><%=d.getStatus() != null ? JSP.w(d.getStatus().getDescription()) : "&nbsp;"%>
      </b>
    </td>
    <td valign="top">
      <%=pageState.getI18n("DISCUSSION_POINT_TYPE")%>: <%=d.getType() != null ? JSP.w(d.getType().getDescription()) : "&nbsp;"%>
    </td>
  </tr>
  <%if(d.getDocuments().size()>0){%>
  <tr class="alternate">
    <td colspan="3"  valign="top">
      <%=pageState.getI18n("DOCUMENTS")%>:

  <%for(Object doc : d.getDocuments()){
      Document docD = (Document) PersistenceHome.findByPrimaryKey(Document.class, doc.toString());

    %><%=docD.getName()%>&nbsp;&nbsp;<%

      } %>
    </td>
  </tr><%

 }%>


</table>
<%
  }
%>
