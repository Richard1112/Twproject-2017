<%@ page import="org.jblooming.waf.view.PageState, org.jblooming.waf.SessionState, org.jblooming.waf.view.PageSeed, org.jblooming.waf.constants.Commands, com.twproject.resource.Person, org.jblooming.waf.settings.I18n, org.jblooming.agenda.CompanyCalendar, java.util.Date, java.util.Calendar, org.jblooming.agenda.Period, com.twproject.agenda.Event, java.util.List, java.util.Collections, org.jblooming.utilities.DateUtilities, org.jblooming.waf.html.button.ButtonLink, org.jblooming.utilities.JSP, com.twproject.utilities.TeamworkComparators" %>
<% PageState pageState = PageState.getCurrentPageState(request);

  PageSeed ps = new PageSeed(request.getContextPath() + "/applications/teamwork/agenda/agendaEditor.jsp");
  ps.command= Commands.EDIT;


  Person resource = Person.getLoggedPerson(pageState);

%>
<div class="portletBox small myMeetings"><h1><%=I18n.get("NEXT_MEETING")%></h1>
<%

  CompanyCalendar cc = new CompanyCalendar();
  cc.setTimeInMillis(System.currentTimeMillis());
  Date start = cc.getTime();

  cc.add(Calendar.DAY_OF_YEAR, -1);
  Date yesterday = cc.getTime();
  cc.add(Calendar.DAY_OF_YEAR, 1);

  cc.add(Calendar.MONTH, 3);
  Date end = cc.getTime();
  Period periodMonth = new Period(start, end);

  List<Event> resultEvent = Event.getMeetingsInPeriodFor(resource, periodMonth, false, true);
  Collections.sort(resultEvent, new  TeamworkComparators.EventPeriodComparator());

  if (resultEvent.size() > 0) {
    int i = 0;
    String formerDayOfMeeting = null;
    for (Event event : resultEvent) {
      ps.setMainObjectId(event.getId());
      String dateOfmeeting = DateUtilities.dateToFullString(event.getSchedule().getNextDateAfter(yesterday)).toLowerCase();

      if (formerDayOfMeeting == null || !(formerDayOfMeeting.equals(dateOfmeeting))) {
        formerDayOfMeeting = dateOfmeeting;
      %><h5><%=dateOfmeeting%></h5><%
        }
        ps.setMainObjectId(event.getId());
      %><div class="event">
                <% ButtonLink edit =  ButtonLink.getTextualInstance(
                        DateUtilities.dateToHourMinutes(event.getSchedule().getValidityStartDate())+
                            "-"+DateUtilities.dateToHourMinutes(event.getSchedule().getValidityEndDate()),ps);
                  edit.toHtml(pageContext);
                  %>&nbsp;<%=JSP.w(event.getSummary())%><%

                  if (event.getLocation()!=null) { %>
                  (<%=JSP.w(event.getLocation())%>)<%
                  }%>
          </div><%
        //}
              if (i < 4) {
                i++;
              } else {
                break;
              }
       }
    } else {
          %><div class="event"><%=I18n.get("MEETING_NOMEETING")%></div><%
          }
       %></div>
