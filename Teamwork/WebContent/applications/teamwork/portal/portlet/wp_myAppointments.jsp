<%@ page import="com.twproject.agenda.Event, com.twproject.resource.Person, com.twproject.resource.Resource, org.jblooming.agenda.CompanyCalendar, org.jblooming.agenda.Period,
                org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.settings.I18n,
                org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.text.SimpleDateFormat, java.util.ArrayList, java.util.Date,
                java.util.List, java.util.TreeMap" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  //Date oggi = new Date();

  PageSeed ps = new PageSeed(request.getContextPath() + "/applications/teamwork/agenda/agendaWeekDay.jsp");
  ButtonLink sub = new ButtonLink(ps);
  sub.label="";
  sub.toolTip=I18n.get("AGENDA");
  sub.iconChar="C";


  Person resource = Person.getLoggedPerson(pageState);

  CompanyCalendar cc = new CompanyCalendar();
  cc.setTimeInMillis(System.currentTimeMillis());

%>
<div class="myAppointments portletBox small">

  <style type="text/css">



    .myAppointments .eventDetail {
      padding: 1px 4px;
    }

    .myAppointments .eventLine {
      margin-bottom: 5px;
      color: #444;
    }

    .myAppointments .eventLine em {
        color: #a5a5a5;
    }

    .myAppointments h4 {
      border-bottom: 1px solid #cccccc;
      font-weight: 600;
    }

    .myAppointments .button.textual.icon {
        margin: 12px 8px 0 0;
    }

      .eventDetail > i {
            font-size: 90%;
      }

    .eventDetail .teamworkIcon {
        background-color: #afdcff;
        color: transparent;
    }

    .eventDetail .teamworkIcon {
        margin-right: 2px;
        display: inline-block;
        color: transparent;
        box-sizing: border-box;
        border: 2px solid #fff;
    }

    .teamworkIcon.isMeeting {
        background-color: #CFF7EA;
    }
    .teamworkIcon.isPersonal {
        background-color: #afdcff;
    }
    .teamworkIcon.isReminder {
        background-color: #FCEC6B;
    }
    .teamworkIcon.isUnavailability {
        background-color: #f4cdcb;
    }

  </style>

  <div style="float:right;"><%//sub.toHtmlInTextOnlyModality(pageContext);%></div>
  <%
    sub.iconChar="";
    sub.label=I18n.get("MY_APPOINTMENTS");
  %>
  <h1><%sub.toHtmlInTextOnlyModality(pageContext);%></h1>

  <div class="events"><h5><%=I18n.get("TODAY")%></h5><%
    List<Resource> resources = new ArrayList<Resource>();
    resources.add(resource);
    for (int i = 0; i <= 1; i++) {
      Period today = Period.getDayPeriodInstance(cc.getTime());

      if(i==1) {

  %><div class="event"><h5><%=I18n.get("TOMORROW")%> - <%=DateUtilities.dateToString(cc.getTime(),"EEEE d")%></h5></div><%

  } else {

  %><div class="event today"></div><%

    }
    // iterate for candidate events
    TreeMap<Period, Event> result = Event.getPeriodsInPeriodFor(resource, today, false);

    //List<Event> result = Event.getFilteredEventsInPeriodWithCollisionFor(resources, today, 0, true, false, false, false, false);

    if (result.size() > 0) {
      for (Period p : result.keySet()) {
        //for (Event event : result) {
        Event event = result.get(p);
        ps = new PageSeed(request.getContextPath() + "/applications/teamwork/agenda/agendaEditor.jsp");
        ps.setCommand(Commands.EDIT);
        ps.setMainObjectId(event.getId());

  %><div class="eventLine">
    <%

      String addCss = "";
      if (event.isUnavailability()) {
        addCss= "isUnavailability";
      } else if (event.isPersonal()) {
        addCss= "isPersonal";
      } else if (event.isReminder()) {
        addCss= "isReminder";
      } else if (event.getMeeting()!=null) {
        addCss= "isMeeting";
      }

        String addTitle = "";
      if (event.isUnavailability()) {
          addTitle= I18n.get("IS_UNAVAILABLE");
      } else if (event.isPersonal()) {
          addTitle= I18n.get("IS_PERSONAL");
      } else if (event.isReminder()) {
          addTitle= I18n.get("IS_REMINDER");
      } else if (event.getMeeting()!=null) {
          addTitle= I18n.get("MEETING");
      }


    String label = DateUtilities.dateToHourMinutes(p.getValidityStartDate()) + "-" + DateUtilities.dateToHourMinutes(p.getValidityEndDate());
      if (p.getDurationInMillis()>CompanyCalendar.MILLIS_IN_HOUR*23.8)
        label=I18n.get("ALL_DAY");

    ButtonLink edit =  ButtonLink.getTextualInstance(label,ps);

    //edit.additionalCssClass=addCss;
    edit.toHtml(pageContext);
  %><span class="eventDetail"><i class="teamworkIcon <%=addCss%>" title="<%=addTitle%>">&copy;</i><%=JSP.w(event.getSummary())%></span><%

    if (event.getLocation()!=null) { %>
    (<%=JSP.w(event.getLocation())%>)<%
    }
  %></div><%
    }

  } else {
  %>
    <div class="eventLine"><em><%=I18n.get("AGENO_EVENT")%></em></div>
    <%
        }


        cc.add(CompanyCalendar.DAY_OF_MONTH, 1);

      }
    %>

  </div>
</div>
