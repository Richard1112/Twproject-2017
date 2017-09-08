<%@ page import="com.twproject.agenda.Event,
                 com.twproject.agenda.businessLogic.AgendaController,
                 com.twproject.meeting.DiscussionPoint,
                 com.twproject.meeting.Meeting,
                 com.twproject.resource.Resource,
                 com.twproject.waf.TeamworkPopUpScreen,
                 org.jblooming.agenda.ScheduleSupport,
                 org.jblooming.system.SystemConstants,
                 org.jblooming.utilities.JSP,
                 org.jblooming.utilities.ReflectionUtilities,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.html.button.ButtonJS,
                 org.jblooming.waf.html.core.JspHelper,
                 org.jblooming.waf.html.display.Img,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new AgendaController(), request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);

  } else {
    Event event = (Event) pageState.getMainObject();
    PageSeed seed = pageState.thisPage(request);
    seed.setMainObjectId(event.getId());
    seed.setCommand("FIND_TARGET");
    Form f = new Form(seed);
    pageState.setForm(f);
    f.start(pageContext);


    Img log = new Img(ApplicationState.getApplicationSetting(SystemConstants.PRINT_LOGO), "");
%>
    <table border="0" width="99%" align="center" cellpadding="3" cellspacing="0">
      <tr>
        <td align="left" width="90%"><%log.toHtml(pageContext);%></td>
        <td align="right"><%

          ButtonJS print = new ButtonJS("window.print();");
          print.label = "";
          print.toolTip = I18n.get("PRINT_PAGE");
          print.iconChar = "p";
          print.toHtmlInTextOnlyModality(pageContext);

        %></td>

      </tr>
    </table>
    <%

%>
<table border="0" width="99%" align="center" cellpadding="3" cellspacing="0">

        <tr class="alternate"><td valign="top" colspan="2"><h1><%=event.getSummary()%> <small>(id <%=event.getId()%>)</small></h1></td></tr>

        <tr class="alternate">
          <td valign="top"><%=I18n.get("AGENDA_AUTHOR")%>: <b><%=event.getAuthor().getDisplayName()%></b></td>
          <td valign="top" width="50%"><%=I18n.get("AGENDA_DESCRIPTION")%>: <b><%=JSP.ex(event.getDescription()) ? JSP.w(event.getDescription()) : "&nbsp;"%></b></td>
        </tr>
        <tr class="alternate">
          <td valign="top"><%=I18n.get("LOCATION")%>: <b><%=(event.getLocation()!=null ? event.getLocation() : "&nbsp;")%></b></td>
          <td valign="top"><%=I18n.get("TYPE")%>: <b><%=(event.getType()!=null ? event.getType().getName() : "&nbsp;")%></b></td>
        </tr>
  <tr>
    <td valign="top" colspan="2" style="padding-top: 10px"><% if (event.isReminder()) {
    %><span class="teamworkIcon">b</span>&nbsp;<b><%=I18n.get("IS_REMINDER")%></b>&nbsp;&nbsp;&nbsp;<%
     }
   if (event.isPersonal()) {
    %><span class="teamworkIcon">o</span>&nbsp;<b><%=I18n.get("IS_PERSONAL")%></b>&nbsp;&nbsp;&nbsp;<%
     }

  if (event.isUnavailability()) {
   %><b class="isUnavailability"><%=I18n.get("IS_UNAVAILABLE")%></b><%
    }


   %></td>
</tr>
 </table><br>

<table border="0" width="99%" align="center" cellpadding="3" cellspacing="0" class="table">
  <%
    ScheduleSupport schedule = (ScheduleSupport) ReflectionUtilities.getUnderlyingObject(event.getSchedule());
%>  <tr>
    <td valign="top"><h2><%=I18n.get("SCHEDULE_EVENT")%> (<%=I18n.get("SCHED_"+schedule.jsonify().getString("type").toUpperCase())%>)</h2></td>
  </tr>
  <tr>
    <td>
      <b><%=JSP.convertLineFeedToBR(schedule.getScheduleDescription(""))%></b>
    </td>
  </tr>
</table>
<br>

<table border="0" width="99%" align="center" cellpadding="3" cellspacing="0">
  <tr>
    <td valign="top"><h2><%=I18n.get("TARGETS")%></h2></td>
    </tr><%
      for (Resource resource : event.getTargets()) {
      %><tr class="alternate"><td><%=resource.getDisplayName()%></td></tr><%
            }
      %>
      </table>
<%
    if(event.getMeeting() != null) {
      Meeting m = event.getMeeting();
      %>
<br><table border="0" width="99%" align="center" cellpadding="3" cellspacing="0">
  <tr>
    <td valign="top" colspan="2">
       <h2><%=I18n.get("MEETING_DATA")%></h2>
    </td>
  </tr>
  <tr>
    <td valign="top">
      <%=I18n.get("MEETING_BOARD")%>: <b><%=(m.getBoard() != null) ? (JSP.w(m.getBoard().getDescription())) : "&nbsp;" %></b>
    </td>
  </tr>
</table>
<%
 for(DiscussionPoint dip : m.getDiscussionPoints()){
    JspHelper dipPrint = new JspHelper("/applications/teamwork/meeting/partDiscussionPointPrint.jsp");
    dipPrint.parameters.put("discussionPoint",dip);
    dipPrint.toHtml(pageContext);
 }
  }
    f.end(pageContext);
  }
%>
