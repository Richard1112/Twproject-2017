<%@ page import="com.twproject.resource.Resource, com.twproject.task.Assignment, com.twproject.task.Task, org.jblooming.agenda.CompanyCalendar, org.jblooming.agenda.Period,
org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState,
java.util.Date, java.util.List" %><%


  PageState pageState = PageState.getCurrentPageState(request);
  JspHelper includer = (JspHelper) JspHelper.getCurrentInstance(request);
  Resource person=  (Resource) includer.parameters.get("person");
  final long minMillisInBar=  (Long) includer.parameters.get("minMillisInBar");


  // make the resource editor link
  PageSeed psEdRes = pageState.pageFromRoot("resource/resourceEditor.jsp");
  psEdRes.command = Commands.EDIT;
  psEdRes.mainObjectId = person.getId();


  boolean showInactiveAssig= pageState.getEntry("SHOW_NOTACTIVE_ASSIGS").checkFieldValue();
  boolean showRoutinary= pageState.getEntry("PRIORITY_SHOW_ROUTINARY").checkFieldValue();
  boolean showClosedTask= pageState.getEntry("WORKLOG_CLOSED_TASKS").checkFieldValue();

  ButtonLink bl = new ButtonLink(person.getDisplayName(), psEdRes);
%>
<tr class="rowAssig tableSection"><td colspan="7"><div class="childNode"><h2><img src="<%=person.bricks.getAvatarImageUrl()%>" title="<%=person.getDisplayName()%>" class='face small' align='absmiddle'> <%bl.toHtmlInTextOnlyModality(pageContext);%></h2></div></td></tr>
<tr class="rowPrio">
<%

  // iterate for 7 days
  CompanyCalendar cc= new CompanyCalendar();
  cc.setTimeInMillis(minMillisInBar);
  for (int i = 1; i <= 7; i++) {
    Date day = cc.getTime();
    long time = cc.getTimeInMillis();
    Period fullDay = Period.getDayPeriodInstance(day);

    List<Assignment> aL = person.getAssignmentsByPriority(fullDay,!showInactiveAssig,!showClosedTask);

%><td valign="top" class="day <%=cc.isWorkingDay()?"":"dayH"%> <%=cc.isToday()?"dayT":""%>">
<table class="table" cellspacing="3" cellpadding="2" border="0"><%
// iterate for the assignements
for (Assignment ass : aL) {
  if (ass.getActivity().equals(Assignment.ACTIVITY_ALL_IN_ONE )|| showRoutinary) {
    %><tr><%
    Task task = ass.getTask();

    JspHelper cellDrawer=new JspHelper("partPriorityCell.jsp");
    cellDrawer.parameters.put("ass",ass);
    cellDrawer.parameters.put("task",task);
    cellDrawer.parameters.put("time",time);

    cellDrawer.toHtml(pageContext);


    %></tr><%
  }
}

%><tr><td>&nbsp;</td></tr></table><%
cc.add(CompanyCalendar.DAY_OF_MONTH, 1);
%></td><%
      }
%></tr><%






%>