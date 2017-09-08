<%@ page import="com.twproject.resource.Resource, com.twproject.task.Assignment, com.twproject.task.TaskBricks, org.jblooming.agenda.Period, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.input.ColorValueChooser, org.jblooming.waf.view.PageState, java.util.Date, java.util.List, java.util.Collections, com.twproject.utilities.TeamworkComparators" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  JspHelper dDrawer = (JspHelper) JspIncluderSupport.getCurrentInstance(request);
  Resource resource = (Resource)dDrawer.parameters.get("resource");
  List<Assignment> assigs = resource.getActiveAssignments(Period.getDayPeriodInstance(new Date()),false,true,false);
  if (JSP.ex(assigs)){
    Collections.sort(assigs, new TeamworkComparators.AssignmentByPriority(new Date()));

%>
<table  border="0" width="99%" align="center" cellpadding="3" cellspacing="0" class="table edged">
  <tr><td colspan="10" ><hr><h3><%=pageState.getI18n("ASSIGNMENTS")%></h3></td></tr>
  <tr>
    <th class="tableHead" align="center">pr.</th>
    <th class="tableHead"><%=pageState.getI18n("TASK")%></th>
    <th class="tableHead"><%=pageState.getI18n("ROLE")%></th>
    <th class="tableHead fixedWidthAlignRight"><%=pageState.getI18n("WORKLOG_ESTIMATED")%></th>
    <th class="tableHead fixedWidthAlignRight"><%=pageState.getI18n("WORKLOG_DONE")%></th>
  </tr>
  <%
    for (Assignment assignment : assigs) {
%><tr>
  <td align="center"><%
    pageState.addClientEntry("ASSPRIO_"+assignment.getId(), assignment.getPriorityAtTime(new Date().getTime()));
    ColorValueChooser chooser = TaskBricks.getAssignmentPriorityCombo("ASSPRIO_" + assignment.getId(), 19, pageState);
    chooser.readOnly=true;
    chooser.displayValue=false;
    chooser.toHtml(pageContext);
  %></td>
  <td><%=assignment.getTask().getDisplayName()%></td>
  <td align="left"><%=assignment.getRole().getDisplayName()%></td>
  <td align="right"><%=DateUtilities.getMillisInHoursMinutes(assignment.getEstimatedWorklog())%></td>
  <td align="right"><%=DateUtilities.getMillisInHoursMinutes(assignment.getWorklogDone())%></td>
  </tr><%

  }
%></table><%
  }
%>