<%@ page import=" com.twproject.agenda.Event,
                 com.twproject.agenda.PeriodEvent,
                 com.twproject.resource.Person,
                 com.twproject.waf.html.EventListWriter,
                 org.jblooming.utilities.DateUtilities,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  Person loggedPerson = Person.getLoggedPerson(pageState);
  EventListWriter eventListWriter = (EventListWriter) JspIncluderSupport.getCurrentInstance(request);

  %><div  style="overflow: hidden; font-size:80%;"><%

  for (PeriodEvent pev : eventListWriter.periodEvents) {
    Event event = pev.event;

    PageSeed ps = new PageSeed(request.getContextPath() + "/applications/teamwork/agenda/agendaEditor.jsp");
    ps.setCommand(Commands.EDIT);
    ps.setMainObjectId(event.getId());

    String description;


    %><%=DateUtilities.dateToHourMinutes(event.getSchedule().getValidityStartDate())%>-<%=DateUtilities.dateToHourMinutes(event.getSchedule().getValidityEndDate()) %>&nbsp;<%


  boolean canSeeDetail = !event.isPersonal() || event.getTargets().contains(loggedPerson);

        boolean first=true;
        if (canSeeDetail) {
          description = JSP.encode(event.getSummary()) + (event.getSummary() == null ? JSP.encode(JSP.limWr(event.getDescription(), 20)) : "");
        } else {
          description=JSP.makeTag("I","", I18n.get("IS_PERSONAL"));
        }
    
    String addCss = "";
    if (event.isUnavailability())
      addCss+= "isUnavailability ";

    if (event.isPersonal())
      addCss+= "isPersonal ";

    if (event.isReminder())
      addCss+= "isReminder ";

    if (event.getMeeting()!=null)
      addCss+= "isMeeting ";


      ButtonLink bEdit = ButtonLink.getTextualInstance(description, ps);
      bEdit.additionalCssClass=addCss;
      bEdit.enabled=canSeeDetail;
      bEdit.toHtml(pageContext);

      %><%
  }

     %> </div> <%

    %>
