<%@ page import="com.twproject.resource.Resource, com.twproject.task.Assignment, com.twproject.task.Task, org.jblooming.agenda.Period, org.jblooming.oql.OqlQuery, org.jblooming.persistence.PersistenceHome, org.jblooming.persistence.exceptions.FindException, org.jblooming.utilities.DateUtilities, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.view.PageState, java.io.Serializable, java.util.*" %>
<%!
  Map<Serializable, Long[]> getResourceWorklogGrouByRoot(Task task, Map<Serializable, Long[]> resources, Period period) throws FindException {
    for (Assignment a : task.getAssignments()) {
      Long estimated = a.getEstimatedWorklog();
      Long done = period != null ? a.getWorklogDone(period).second : a.getWorklogDone();
      Long[] value = resources.get(a.getResource().getId());
      if (value != null) {
        estimated = estimated + value[1];
        done = done + value[0];
      }
      resources.put(a.getResource().getId(), new Long[]{done, estimated});
    }

    if (task.getChildrenSize() > 0) {
      for (Task child : task.getChildrenSorted())
        getResourceWorklogGrouByRoot(child, resources, period);
    }
    return resources;
  }

  long getTotalWorklogDoneInPeriod(Task task, Period period) {
    long twd = 0;
    for (Assignment a : task.getAssignments()) {
      twd += a.getWorklogDone(period).second;
    }

    Iterator i = task.getChildrenIterator();
    while (i.hasNext()) {
      Task child = (Task) i.next();
      twd += getTotalWorklogDoneInPeriod(child, period);
    }
    return twd;
  }
%>
<% PageState pageState = PageState.getCurrentPageState(request);
  JspHelper dDrawer = (JspHelper) JspIncluderSupport.getCurrentInstance(request);
  Task task = (Task) dDrawer.parameters.get("task");
  int rowid = (Integer) dDrawer.parameters.get("rowId");
  boolean showTotal = (Boolean) dDrawer.parameters.get("showTotal"); // see also worklog done
  boolean root = (Boolean) dDrawer.parameters.get("root"); // group by root task
  boolean assig = (Boolean) dDrawer.parameters.get("assig"); // if true do not show assignemt
  Date from = (Date) dDrawer.parameters.get("from");
  Date to = (Date) dDrawer.parameters.get("to");
  Period period = null;
  if (from != null || to != null) {
    period = new Period(from, to);
  }
%>
<tr class="alternate">
  <td>
    <%
      for (int i = 1; i < rowid; i++) {
    %>&nbsp;&nbsp;&nbsp;&nbsp;
    <%
      }%>-<%=task.getDisplayName()%>
  </td>
    <% if (showTotal) {%>
  <td align="right">&nbsp;
    <%  if (period != null) {
    %><b><%=DateUtilities.getMillisInHoursMinutes(getTotalWorklogDoneInPeriod(task, period))%>
  </b>
    <% } else {
    %><b><%=DateUtilities.getMillisInHoursMinutes(task.getTotalWorklogDone())%>
  </b>
    <%
        }
        %></td><%
      }
    %>
  <td align="right"><b><%=(DateUtilities.getMillisInHoursMinutes(task.getWorklogEstimated()))%>
  </b></td>
  <td align="right"><b><%=(DateUtilities.getMillisInHoursMinutes(task.getTotalWorklogEstimated()))%>
  </b>
  </td>
</tr>
<%
  String hql = "from " + Assignment.class.getName() + " as assig where assig.task = :myself order by assig.resource.personSurname";
  OqlQuery oql = new OqlQuery(hql);
  oql.getQuery().setEntity("myself", task);
  List<Assignment> assigs = oql.list();


  if (!assig && !root) {
    for (Assignment a : assigs) {

      long done = 0;
      long estimated = a.getEstimatedWorklog();
      if (showTotal && period != null) {
        done = a.getWorklogDone(period).second;
      } else if (showTotal && period == null) {
        done = a.getWorklogDone();
      }
      if (estimated != 0 || done != 0) {
%>
<tr class="alternate">
  <td>
    <%
      for (int i = 1; i < rowid; i++) {
    %>&nbsp;&nbsp;&nbsp;&nbsp;
    <%
      }%>&nbsp;&nbsp;&nbsp;&nbsp;
    <b>-<%=a.getResource().getDisplayName()%></b>
  </td>
  <% if (showTotal) {

    %><td align="right">&nbsp;
  <b><%=DateUtilities.getMillisInHoursMinutes(done)%>
  </b></td>
    <%
      }
    %>
  <td align="right"><b><%=DateUtilities.getMillisInHoursMinutes(estimated)%>
  </b>
  </td>
  <td>&nbsp;</td>
</tr>
<% }
}
} else if (!assig && root) {

  Map<Serializable, Long[]> resource = new HashMap<Serializable, Long[]>();
  resource = getResourceWorklogGrouByRoot(task, resource, period);

  Set<Serializable> keys = resource.keySet();

  for (Serializable key : keys) {
    Long[] values = resource.get(key); // this array contains in position 0 total worklog done and in position 1 the estimated
    Resource res = (Resource) PersistenceHome.findByPrimaryKey(Resource.class, key);
%>
<tr class="alternate">
  <td>
    <%
      for (int i = 1; i < rowid; i++) {
    %>&nbsp;&nbsp;&nbsp;&nbsp;
    <%
      }%>&nbsp;&nbsp;&nbsp;&nbsp;
    <b>-<%=res.getDisplayName()%></b>
  </td>
  <td>&nbsp;
    <% if (showTotal) {
    %> <b><%=DateUtilities.getMillisInHoursMinutes(values[0])%>
  </b>
    <%
      }
    %></td>
  <td><b><%=DateUtilities.getMillisInHoursMinutes(values[1])%>
  </b></td>
  <td align="center">&nbsp;
  </td>
</tr>
<%
    }

  }
%>
