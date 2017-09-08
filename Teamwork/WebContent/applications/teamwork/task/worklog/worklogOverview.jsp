<%@ page import="com.twproject.resource.Resource,
                 com.twproject.resource.ResourceBricks,
                 com.twproject.security.TeamworkPermissions,com.twproject.waf.TeamworkHBFScreen, org.jblooming.agenda.CompanyCalendar, org.jblooming.agenda.Period,
                 org.jblooming.agenda.Scale, org.jblooming.ontology.IdentifiableSupport, org.jblooming.ontology.SerializedList, org.jblooming.security.Permission,
                 org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.Bricks, org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.SessionState, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.container.HeadBar, org.jblooming.waf.html.display.Img,
                 org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState, java.text.ParseException, java.util.Date, java.util.List, java.util.TreeMap, org.jblooming.utilities.StringUtilities, com.twproject.operator.TeamworkOperator" %><%

    PageState pageState = PageState.getCurrentPageState(request);

    if (!pageState.screenRunning) {
        pageState.screenRunning = true;
        final ScreenArea body = new ScreenArea(request);
        TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
        lw.register(pageState);
        pageState.perform(request, response).toHtml(pageContext);
    } else {

      TeamworkOperator loggedOperator = (TeamworkOperator) pageState.getLoggedOperator();


      List<Resource> resources= ResourceBricks.fillWorkGroup(pageState);

        // get the focused millis
        long focusedMillis= pageState.getEntry("FOCUS_MILLIS").longValueNoErrorNoCatchedExc();
        focusedMillis= focusedMillis==0?System.currentTimeMillis():focusedMillis;
        pageState.addClientEntry("FOCUS_MILLIS",focusedMillis);

        // get the current scale factor
        String ssct=pageState.getEntryOrDefault("SCALE_TYPE", Scale.ScaleType.SCALE_2WEEK.toString()).stringValue();

        // define the scale and increment of ticks
        Scale scale = Scale.getScaleAndSynch(Scale.ScaleType.valueOf(ssct), focusedMillis, true, SessionState.getLocale());

        //set calender to first day of month
        CompanyCalendar cc = new CompanyCalendar();
        cc.setTime(scale.startPointDate);

        //get the eventual referral
        IdentifiableSupport referral = Bricks.getReferral(pageState);

        PageSeed self = pageState.thisPage(request);
        if (referral != null)
            Bricks.addReferral(referral.getId(), referral.getClass(), self);
        self.addClientEntry("WG_IDS",JSP.w(pageState.getEntry("WG_IDS").stringValueNullIfEmpty()));

        SerializedList<Permission> permissions = new SerializedList();
        permissions.add(TeamworkPermissions.resource_manage);
        permissions.add(TeamworkPermissions.worklog_manage);
        self.addClientEntry("PERM_REQUIRED", permissions);

        self.addClientEntry("FOCUS_MILLIS", focusedMillis + "");
        self.addClientEntry("SCALE_TYPE", scale.scaleName);
        Form f = new Form(self);
        f.usePost=false;
        f.alertOnChange = false;
        pageState.setForm(f);

        f.start(pageContext);

        //check if logged can inspect more see more resources
        boolean canInspectMoreResources = loggedOperator.hasPermissionFor(TeamworkPermissions.resource_manage) || loggedOperator.hasPermissionFor(TeamworkPermissions.worklog_manage);
        canInspectMoreResources = canInspectMoreResources || JSP.ex(loggedOperator.getPerson().getMyStaff());


        pageState.addClientEntry("TIMESHEET_TABSET","TIMESHETS_OVERVIEW_TAB");

%>
<div class="mainColumn">
<h1><%=I18n.get("TIMESHETS_OVERVIEW")%> </h1>
<div class="optionsBar clearfix"><div class="filterElement centered"><%

    ButtonSupport workgroup = ButtonSubmit.getSubmitInstanceInBlack(pageState.getForm(), request.getContextPath() + "/applications/teamwork/workgroup/workgroupPopup.jsp", 700, 550);
    workgroup.toolTip = I18n.get("CHANGE_WORK_GROUP");
    workgroup.label = "";
    workgroup.additionalCssClass = "rounded";
    workgroup.iconChar = "r";
    workgroup.enabled=canInspectMoreResources;


    for (Resource res : resources) {
%><div class="workgroupElement"><img src="<%=res.bricks.getAvatarImageUrl()%>" title="<%=res.getDisplayName()%>" class='face' align='absmiddle'></div><%
    }
%><div class="workgroupElement"><%workgroup.toHtml(pageContext);%></div>
</div></div>

<table class="table dataTable edged fixHead fixFoot" cellspacing="0" cellpadding="0">

    <%----------------------------------------------- CALENDAR DAYS BEGIN -------------------------------------------------------%>
      <thead class="dataTableHead">
      <tr>
        <td colspan="99" style="padding:0;">
          <jsp:include page="../plan/monthsMovementBar.jsp"/>
        </td>
      </tr>
      <tr>
        <td style="background-color:white;" colspan="1">&nbsp;&nbsp;</td>

        <%
          long todayMorning=new CompanyCalendar().setAndGetTimeToDayStart().getTime();
            while (cc.getTimeInMillis() <scale.endPointTime) {
        %><td t="<%=DateUtilities.dateToInt(cc.getTime())%>" dateLabel="<%=DateUtilities.dateToString(cc.getTime(), "EEEE dd" )%>" class="dayHeader <%=cc.isToday()?"dayTHeader":""%> <%=!cc.isWorkingDay()?"dayHHeader":""%> <%=cc.getTimeInMillis()>todayMorning?"inTheFuture":""%>"><%=DateUtilities.dateToString(cc.getTime(), "EEE")%><br><%=DateUtilities.dateToString(cc.getTime(), "dd")%></td><%
            cc.add(CompanyCalendar.DAY_OF_MONTH,1);
        }
    %>

        <td class="dayHeader" style="text-align: right"><%=I18n.get("TOTAL")%></td>
    </tr>
      </thead>

    <%----------------------------------------------- CALENDAR DAYS END-------------------------------------------------------%>
    <%
        //--------------------------  BEGIN LOOP RESOURCES --------------------------------------------

        Period period = scale.getPeriod();

        for (Resource resource:resources){
            cc.setTime(scale.startPointDate);

            TreeMap<Integer,Long> workablePlan = resource.getWorkablePlan(period); //for each day the workable millis for that day. If day is holidays -1  if is completely unaivailable millis are -2
            TreeMap<Integer,Long> workedPlan = resource.getWorkedPlan(period); //for each day the worked millis for that day.

            long defaultDailyCapacity = resource.getWorkDailyCapacity();

            PageSeed agendaDayForResource=new PageSeed(ApplicationState.serverURL+"/applications/teamwork/agenda/agendaWeekDay.jsp");
            PageSeed resourceEditor=new PageSeed(ApplicationState.serverURL+"/applications/teamwork/resource/resourceEditor.jsp");
            resourceEditor.command=Commands.EDIT;
            resourceEditor.mainObjectId=resource.getId(); // set persona giusta

            agendaDayForResource.addClientEntry("AGENDA_TYPE", "DAY"); // day
            agendaDayForResource.addClientEntry("WG_IDS",resource.getId()); // set persona giusta

            ButtonLink resbl=new ButtonLink(resource.getDisplayName(),resourceEditor);
            resbl.enabled=resource.hasPermissionFor(loggedOperator,TeamworkPermissions.resource_canRead);


            // -------------------------------------------------------------------------------------------------  HERE STARTS THE CORE ----------------------------------------------------------------------------
    %>
    <tr resId="<%=resource.getId()%>">
        <td>
            <div style="float: left; position:relative;">
                <img src="<%=resource.bricks.getAvatarImageUrl()%>" class='face small' align='absmiddle'>
            </div>

            <%resbl.toHtmlInTextOnlyModality(pageContext);%><br>
            <span style="font-size:10px;"><%=I18n.get("WORKING_HOUR_TOTAL")%>: <%=DateUtilities.getMillisInHoursMinutes(defaultDailyCapacity)%></span>
        </td>
        <%


            // ------------------------------------------------ START loop the month ------------------------------------------------
            cc.setTime(scale.startPointDate);
            long totWorked=0;
            long totWorkable=0;
            while (cc.getTimeInMillis() <scale.endPointTime) {
                int day = DateUtilities.dateToInt(cc.getTime());

                Date date = DateUtilities.intToDate(day);
                long dayCapacity = workablePlan.get(day);
                Long wL = workedPlan.get(day);
                long worked=wL==null?0:wL;


                totWorked+=worked;
                totWorkable+=dayCapacity>0?dayCapacity:0;

                agendaDayForResource.addClientEntry("FOCUS_MILLIS",date.getTime()); // set giorno giusto
                resourceEditor.addClientEntry("FOCUS_MILLIS", date.getTime()); // set giorno giusto


                // chech the agenda for unavailability
                boolean inVacation=  dayCapacity<defaultDailyCapacity && dayCapacity!=-1;
                boolean isHoliday = !cc.isWorkingDay();



                String tdClass="";
                if (inVacation)
                    tdClass = "isUnavail ";


                if (cc.getTimeInMillis()>todayMorning){
                    tdClass+="inTheFuture ";
                } else if (Math.abs(worked - dayCapacity) <= 5*60000) { //5 min
                    tdClass += "isOk ";
                } else if (worked <dayCapacity){
                    tdClass += "isMissing ";
                } else {
                    tdClass += "isOverwork ";
                }


                if (isHoliday)
                    tdClass += "dayH ";



                // DRAW THE CELL
        %><td class="tdCell <%=tdClass%>"  t="<%=day%>" title="<%=DateUtilities.getMillisInHoursMinutes(worked)%> / <%=DateUtilities.getMillisInHoursMinutes(dayCapacity)%>">
        <%=isHoliday&&worked==0?"":DateUtilities.getMillisInHoursMinutes(worked)%>
    </td><%

            cc.add(CompanyCalendar.DAY_OF_MONTH, 1);
        }
        // ------------------------------------------------ END loop the month ------------------------------------------------
    %>
        <td class="totByRow" title="<%=DateUtilities.getMillisInHoursMinutes(totWorked)%>/<%=DateUtilities.getMillisInHoursMinutes(totWorkable)%>"><%=DateUtilities.getMillisInHoursMinutes(totWorked)%></td>
    </tr><%
    } // ------------------------------------------------ END loop the resources ------------------------------------------------
%>
<tfoot>
<tr>
  <td colspan="99">
    <table align="left" cellspacing="5" cellpadding="0" class="legendaPlan">
      <tr >
        <td align="center"><i><%=I18n.get("PLAN_LEGENDA")%>:</i></td>
        <td class="isOk color" >&nbsp;</td>
        <td><%=I18n.get("WL_CELL_EMPTY")%>&nbsp;&nbsp;&nbsp;</td>
        <td class="isMissing color">&nbsp;</td>
        <td><%=I18n.get("WL_CELL_ERROR")%>&nbsp;&nbsp;&nbsp;</td>
        <td class="isUnavail color" align="center">*</td>
        <td><%=I18n.get("WL_CELL_UNAVAILABLE")%>&nbsp;&nbsp;&nbsp;</td>
        <td class="isOverwork color">9:30</td>
        <td><%=I18n.get("WL_CELL_OVER")%></td>
      </tr>
    </table>
  </td>
</tr>
</tfoot>

</table>


</div>

<%---------------------------------------------------------------------------------  JAVSCRIPT BEGIN ---------------------------------------------------------------------%>
<script type="text/javascript">

    $(function(){


//        $.rightPanel.unpin();
//        $(".rightColumn").fadeIn();

        $("#TIMESHEET_MENU").addClass('selected');
        $(".tdCell[t]").click(function(){
            var cell=$(this);
            var millis= Date.fromInt(cell.attr("t")).getTime();
            var resId=cell.closest("tr[resId]").attr("resId");
            location.href="<%=pageState.pageFromRoot("/task/worklog/worklogWeek.jsp")%>&FOCUS_MILLIS="+millis+"&RES_ID="+resId;
        })

    });


</script>

<%-------------------------------------------------------------------------------  TIME BAR -------------------------------------------------------------------%>
<%
    pageState.attributes.put("FOCUSED_PERIOD", period);// used in the bar
%><jsp:include page="../../parts/timeBar.jsp"/><%


        f.end(pageContext);

    }
%>
