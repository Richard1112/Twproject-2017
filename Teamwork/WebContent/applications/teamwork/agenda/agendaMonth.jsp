<%@ page import="com.twproject.agenda.AgendaBricks, com.twproject.agenda.Event, com.twproject.agenda.PeriodEvent, com.twproject.agenda.businessLogic.AgendaController, com.twproject.resource.Person,
                 com.twproject.resource.Resource, com.twproject.resource.ResourceBricks, com.twproject.security.TeamworkPermissions, com.twproject.waf.TeamworkHBFScreen, net.sf.json.JSONArray,
                 net.sf.json.JSONObject, org.jblooming.agenda.CompanyCalendar, org.jblooming.agenda.Period, org.jblooming.ontology.SerializedList, org.jblooming.persistence.PersistenceHome,
                 org.jblooming.security.Permission, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.SessionState,
                 org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.*" %> <%

    PageState pageState = PageState.getCurrentPageState(request);

    if (!pageState.screenRunning) {
        pageState.screenRunning = true;
        final ScreenArea body = new ScreenArea(new AgendaController(), request);
        TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
        lw.register(pageState);
        pageState.perform(request, response).toHtml(pageContext);
    } else {
        Person loggedPerson = Person.getLoggedPerson(pageState);
        Locale locale = SessionState.getLocale();

        long focusedMillis= pageState.getEntry("FOCUS_MILLIS").longValueNoErrorNoCatchedExc();
        focusedMillis= focusedMillis==0?System.currentTimeMillis():focusedMillis;
        pageState.addClientEntry("FOCUS_MILLIS",focusedMillis);

        String agendaType = pageState.getEntry("AGENDA_TYPE").stringValueNullIfEmpty();
        agendaType = agendaType != null ? agendaType : "MONTH";
        pageState.sessionState.setAttribute("AGENDA_TYPE", agendaType);

        PageSeed ps = pageState.thisPage(request);
        ps.addClientEntry("FOCUS_MILLIS", focusedMillis);
        ps.addClientEntry("AGENDA_TYPE",agendaType);

        Form form = new Form(ps);

        pageState.setForm(form);

        CompanyCalendar cal = new CompanyCalendar(locale);
        cal.setTime(new Date());
        cal.setMillisFromMidnight(0);
        long adesso = cal.getTimeInMillis();

        cal.setTimeInMillis(focusedMillis);
        cal.set(Calendar.DAY_OF_MONTH, 1);
        cal.setMillisFromMidnight(0);
        long startMonthInMillis = cal.getTimeInMillis();

        cal.set(Calendar.DAY_OF_WEEK, cal.getFirstDayOfWeek());
        cal.setMillisFromMidnight(0);
        long startMillis = cal.getTimeInMillis();

        cal.setTimeInMillis(focusedMillis);
        cal.add(Calendar.MONTH, 1);
        cal.set(Calendar.DAY_OF_MONTH, 1);
        cal.setMillisFromMidnight(0);
        cal.add(Calendar.MILLISECOND, -1);
        long endMonthInMillis = cal.getTimeInMillis();

        cal.add(Calendar.DAY_OF_WEEK, 6);
        long endMillis = cal.getTimeInMillis();

        int weeks = (int) (1 + ((endMillis - startMillis) / CompanyCalendar.MILLIS_IN_DAY)) / 7;

        List<Resource> resourcesFromFilter= ResourceBricks.fillWorkGroup(pageState);
        ps.addClientEntry("WG_IDS",JSP.w(pageState.getEntry("WG_IDS").stringValueNullIfEmpty()));

        SerializedList<Permission> permissions = new SerializedList();
        permissions.add(TeamworkPermissions.resource_canRead);
//        permissions.add(TeamworkPermissions.assignment_manage);
        ps.addClientEntry("PERM_REQUIRED", permissions);

        String filter = pageState.getEntry("FILTER").stringValueNullIfEmpty();
        if (filter == null)
            pageState.addClientEntry("FILTER", "NONE");
        pageState.sessionState.setAttribute("AGENDA_TYPE", "MONTH");

        form.start(pageContext);

%><div class="mainColumn">

<jsp:include page="partAgendaHeader.jsp"/>
<%
    //eventi  del mese
    boolean onlyMeInIt = false;
    boolean showAuthored = false;
    boolean showTargets = false;
    boolean isPersonal = false;
    boolean showWork = false;
    boolean unavail = false;

    filter = pageState.getEntry("FILTER").stringValueNullIfEmpty();
    if ("WORK".equals(filter)) {
        onlyMeInIt = false;
        showAuthored = false;
        showTargets = false;
        showWork = true;
        isPersonal = false;
    } else if ("PERSONAL".equals(filter)) {
        onlyMeInIt = false;
        showAuthored = false;
        showTargets = false;
        isPersonal = true;
        showWork = false;
    } else if ("AUTHOR".equals(filter)) {
        onlyMeInIt = false;
        showAuthored = true;
        showTargets = false;
        isPersonal = false;
        showWork = false;
    } else if ("ONLY_ME_IN_IT".equals(filter)) {
        onlyMeInIt = true;
        showAuthored = false;
        showTargets = false;
        isPersonal = false;
        showWork = false;
    } else if ("UNAVAIL".equals(filter)) {
        unavail = true;
        showTargets = true;
    } else {
        onlyMeInIt = false;
        showAuthored = false;
        showTargets = true;
        isPersonal = false;
        showWork = false;
    }

    Date startMonth = new Date(startMonthInMillis);
    Date endMonth = new Date(endMonthInMillis);

    CompanyCalendar cc = new CompanyCalendar(startMonth);
    cc.set(Calendar.DAY_OF_WEEK, cc.getFirstDayOfWeek());
    Date date = cc.getTime();
    cc.setTime(endMonth);
    cc.set(Calendar.DAY_OF_WEEK, cc.getFirstDayOfWeek());
    cc.add(Calendar.DAY_OF_YEAR, 7);
    Date dateEnd = cc.getTime();
    Period periodMonth = new Period(date, dateEnd);

    List<Event> candEventsInPeriod = Event.getFilteredEventsInPeriodWithCollisionFor(resourcesFromFilter, periodMonth, 0, showWork, onlyMeInIt, showAuthored, showTargets, isPersonal, unavail);
    candEventsInPeriod.addAll(AgendaBricks.getIcalExternalEvents(pageState, periodMonth));

    //remove duplicated events
    HashSet<String> visitedIcalId = new HashSet();
    List<Event> eventsInPeriod = new ArrayList();
    for (Event ev:candEventsInPeriod ){
      if (!visitedIcalId.contains(ev.getIcalId())){
        visitedIcalId.add(ev.getIcalId());
        eventsInPeriod.add(ev);
      }
    }


    Set<Resource> involvedResources = new HashSet();

    involvedResources.add(loggedPerson);

    // get all resources from events
    for (Event event : eventsInPeriod) {
        for (Resource resource : event.getTargets()) {
            involvedResources.add(resource);
        }
    }

    involvedResources.addAll(resourcesFromFilter);

    long millis = startMillis;
    CompanyCalendar counter = new CompanyCalendar(locale);
    counter.setTimeInMillis(millis);
    cal.set(Calendar.DAY_OF_WEEK, cal.getFirstDayOfWeek());

    int hg = 100 / weeks;

%><table width="100%" border="0" cellpadding="5" cellspacing="0" style="" class="edged calendarMonth" id="calendarTable">
    <tr class="dayHead">
      <th class="dayHeader" title="<%=I18n.get("WEEK")%>"><%=I18n.get("WEEK_SHORT")%></th>
      <%

        for (int i = 0; i < 7; i++) {

    %>
        <th style="width:14.14%;" class="dayHeader <%=cal.isToday() ? "dayTHeader" :  (cal.isWorkingDay() ? "" : "dayHHeader")%>">
            <%=DateUtilities.dateToString(cal.getTime(), "EEEE")%>
        </th>
        <%
                cal.add(CompanyCalendar.DAY_OF_WEEK, 1);
            }
        %></tr><%

        counter.setTimeInMillis(millis);

        for (int j = 0; j < weeks; j++) {
          cal.setTimeInMillis(millis);
    %>
    <tr valign="top" style="height:<%=hg%>%">
      <td valign="middle" style="font-size: 60%;opacity: .6;"><%=DateUtilities.dateToString(cal.getTime(), "ww")%></td>
        <%
            for (int i = 0; i < 7; i++) {
                cal.setTimeInMillis(millis);
        %>
        <td style="width:14.14%;"  style="position: relative;" class="day <%=cal.isToday() ? "dayT" :  (cal.isWorkingDay() ? "" : "dayH")%>  <%=((millis < startMonthInMillis || millis > endMonthInMillis) ? "prevNextMonth" : "")%>">
          <table width="100%" border=0 cellspacing=0 cellpadding=0 >
            <tr>
              <td align="left">
                <span class="dayMonth"><%
                  ps = new PageSeed("agendaWeekDay.jsp");
                  ps.addClientEntry("FOCUS_MILLIS", cal.getTimeInMillis());
                  ps.addClientEntry("AGENDA_TYPE","DAY");
                  String giorno = DateUtilities.dateToString(cal.getTime(), "dd");
                  ButtonSubmit bsub = ButtonSubmit.getTextualInstance(giorno, form);
                  bsub.variationsFromForm = ps;
                  bsub.toHtml(pageContext);
                %></span>
              </td>
              <td align="right">
                <%
                  ps = new PageSeed("agendaEditor.jsp");
                  ps.addClientEntry("FOCUS_MILLIS", cal.getTimeInMillis());
                  ps.setCommand(Commands.ADD);
                  ButtonSubmit bLink2 = new ButtonSubmit(form);
                  bLink2.label = "";
                  bLink2.toolTip = I18n.get("AGEADD");
                  bLink2.variationsFromForm = ps;
                  bLink2.iconChar="P" ;
                  bLink2.additionalCssClass = "addEvent";
                  bLink2.toHtmlInTextOnlyModality(pageContext);
                %>
              </td>
            </tr>
          </table><%
                        Date d1 = cal.getTime();
                        cal.add(Calendar.DAY_OF_MONTH, 1);
                        cal.add(CompanyCalendar.MILLISECOND, -1);
                        Date d2 = cal.getTime();
                        Period theWorkingDayPeriod = new Period(d1, d2);
                        if (JSP.ex(eventsInPeriod)) {

                    %><div class="calendarEvent" style="margin-top: 5px">
                            <%
                                ArrayList<PeriodEvent> oneDayPeriodEvents = new ArrayList();
                                for (Event event : eventsInPeriod) {

                                    // get working day
                                    List<Period> periods = event.getSchedule().getPeriods(theWorkingDayPeriod, true,event.getExceptions());
                                    for (Period period : periods) {
                                        period.setIdAsNew();
                                        oneDayPeriodEvents.add(new PeriodEvent(period, event));
                                    }
                                }
                                Collections.sort(oneDayPeriodEvents);

                                for (PeriodEvent pe : oneDayPeriodEvents) {
                                    Event event = pe.event;
                                    Period period = pe.period;


                                    String addCss = "";
                                    if (event.isUnavailability()) {
                                        addCss += " isUnavailability";
                                    }
                                    if (event.isPersonal()) {
                                        addCss += " isPersonal";
                                    }
                                    if (event.isReminder()) {
                                        addCss += " isReminder";
                                    }
                                    if (event.getMeeting() != null) {
                                        addCss += " isMeeting";
                                    }
                                    if (event.isFromExternalCalendar()) {
                                        addCss += " external";
                                    }



                                    String description = event.getSummary();
                                    boolean canSeeDetail = !event.isPersonal() || event.getTargets().contains(loggedPerson);
                                    boolean isNew = PersistenceHome.NEW_EMPTY_ID.equals(event.getId());
                                    if (!canSeeDetail)
                                        description = JSP.makeTag("I", "", I18n.get("IS_PERSONAL"));


                                    String slimDescription;
                                    if (description != null && description.length() > 23)
                                        slimDescription = description.substring(0, 22) + "...";
                                    else
                                        slimDescription = description;
                                    ps = new PageSeed("agendaEditor.jsp");
                                    ps.setCommand(Commands.EDIT);
                                    ps.setMainObjectId(event.getId());
                                    String toolTip = "";
                                    for (Resource resource : event.getTargets()) {
                                        if (resourcesFromFilter.contains(resource))
                                            toolTip += "\n" + resource.getName();
                                    }
                                    String descrizione = description + (event.getLocation() == null ? "" : " [" + event.getLocation() + "]") +
                                            " " + DateUtilities.dateToHourMinutes(period.getValidityStartDate()) +
                                            "-" + DateUtilities.dateToHourMinutes(period.getValidityEndDate());
                                    ButtonLink bedit = ButtonLink.getTextualInstance(slimDescription, ps);
                                    bedit.toolTip = descrizione + toolTip;
                                    bedit.enabled = canSeeDetail && !isNew;
                                    if (event.isUnavailability())
                                        bedit.style = " style=\"color:#e6c2df;\"";
                                    bedit.additionalCssClass=addCss;
                                    if (event.isReminder())
                                        bedit.iconChar="b";
                                    if (event.isPersonal())
                                      bedit.iconChar="o";
                                    bedit.toHtml(pageContext);

                                    if (event.getTargets().size() > 0) {
                                        for (Resource resource : event.getTargets()) {
                                            if (event.getTargets().size() > 1 || (!event.getTargets().iterator().next().equals(loggedPerson)) ) {

                            %><img src="<%=resource.bricks.getAvatarImageUrl()%>" class="face smaller" title="<%=resource.getDisplayName()%>" align="absmiddle"><%
                                    }
                                }
                            }

                        %><hr style="border: none; height: 1px;border-bottom: 1px dotted rgba(0, 0, 0, 0.15); background: transparent; margin: 2px; padding: 0"><%

                            }
                        %>
                        </div><%
                        }
                        counter.add(CompanyCalendar.DAY_OF_MONTH, 1);
                        millis = counter.getTimeInMillis();
                    %>

        </td>
        <%
            }
        %>
    </tr>
    <%
        }
    %>
</table>
</div>
<%
    form.end(pageContext);

    pageState.attributes.put("FOCUSED_PERIOD", periodMonth);// used in the bar


    JSONArray workgroupRes= new JSONArray();
    for (Resource res: resourcesFromFilter){
        JSONObject jres = new JSONObject();
        jres.element("resId",res.getId());
        jres.element("resName",res.getDisplayName());
        jres.element("resAvatarUrl",res.bricks.getAvatarImageUrl());
        workgroupRes.add(jres);
    }

    JSONArray involedRes= new JSONArray();
    for (Resource res: involvedResources){
        JSONObject jres = new JSONObject();
        jres.element("resId",res.getId());
        jres.element("resName",res.getDisplayName());
        jres.element("resAvatarUrl",res.bricks.getAvatarImageUrl());
        involedRes.add(jres);
    }

%>

<div class="bottomBar"  id="moveBar" onclick="moveBarClick($(this),event);"></div>

<script type="text/javascript">

    var agendaType="<%=agendaType%>";
    var focusMillis=<%=focusedMillis%>;

    $(function() {
        $("#topHeaderCentral").html(new Date(focusMillis).format("MMMM yyyy"));
        $(".agendaTemplates").loadTemplates().remove();
        var attendees = <%=workgroupRes%>;    // reset
        var legenda = $("#workgroupLegenda").empty();
        for (var i=0;i<attendees.length;i++) {
            legenda.append($.JST.createFromTemplate(attendees[i], "WORKGROUPELEMENT"));
        }
        drawMoveBar();
        $(window).resize(winResize).resize();
    });


    function winResize(){

      //Set the height of the calendar area on window resize
        var win=$(this);
        var fh = $(window).height() - 255;
        fh = fh < 200 ? 200 : fh;
        $("#calendarTable").height(fh);
    }


    function drawMoveBar() {
        var barFolio = createBarFolio();
        //displayed period
        d = new Date(focusMillis);
        d.clearTime();
        d.setDate(1);
        var st = new Date(d.getTime());
        d.setMonth(d.getMonth()+1);
        d.setDate(d.getDate()-1);
        var en = new Date(d.getTime());
        var highlight = $("<span>").addClass("moveBarHL");
        barFolio.addElement(highlight, 0, st.getTime(), en.getTime() - st.getTime(), 50);
        barFolio.redraw();
    }

    function moveBarClick(el, event) {
        var pos = event.clientX - el.offset().left;
        var folio = el.data("folio");
        var millis = parseInt(folio.getVirtualLeft(pos));
        goToMillis(millis);
    }

    function createBarFolio() {
        var moveBar = $("#moveBar");
        moveBar.empty();

        var date = new Date(focusMillis);
        date.clearTime();
        date.setDate(1);
        date.setMonth(date.getMonth() - 12);

        var startMillis = date.getTime();
        date.setMonth(date.getMonth() + 24);
        var endMillis = date.getTime();


        var barFolio = new Folio(moveBar);
        barFolio.width = endMillis - startMillis;
        barFolio.height = 50;
        barFolio.left = startMillis;
        barFolio.inPercent=true;


        var d = new Date(startMillis);
        while (d.getTime() < endMillis) {
            var headLbl = $("<span>").html(d.format("MM yyyy")).addClass("moveBarEl");
            barFolio.addElement(headLbl, 0, d.getTime(), 30 * 24 * 3600000, 50);
            d.setMonth(d.getMonth() + 1);
        }

        //today
        var today = $("<span>").prop("title", "<%=I18n.get("TODAY")%>").addClass("moveBarToday");
        barFolio.addElement(today, 0, new Date().getTime(), barFolio.getPixelWidth() * 2, 50);

        return barFolio;
    }


    function goToMillis(millis){
        $("[name=FOCUS_MILLIS]").val(millis);
        $("#<%=form.id%>").submit();
    }

</script>
<%
    }
%>
