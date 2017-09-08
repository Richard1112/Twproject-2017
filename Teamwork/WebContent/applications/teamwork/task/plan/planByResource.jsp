<%@ page import="com.twproject.operator.TeamworkOperator,
                 com.twproject.plan.PlanUtilities,
                 com.twproject.resource.Resource,com.twproject.resource.ResourceBricks, com.twproject.security.TeamworkPermissions, com.twproject.task.Assignment,
                 com.twproject.task.Task, com.twproject.utilities.TeamworkComparators, com.twproject.waf.TeamworkHBFScreen, org.jblooming.agenda.CompanyCalendar,
                 org.jblooming.agenda.Period, org.jblooming.agenda.Scale, org.jblooming.ontology.IdentifiableSupport, org.jblooming.ontology.SerializedList,
                 org.jblooming.security.Permission, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.Bricks,
                 org.jblooming.waf.ScreenArea, org.jblooming.waf.SessionState, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.core.JspHelper,
                 org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.ArrayList, java.util.Collections, java.util.List, java.util.Map, com.twproject.task.TaskStatus, com.twproject.resource.Person" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    body.areaHtmlClass="lreq30 lreqPage";
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    //lw.areaHtmlClass="lreq20 lreqPage";
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);
  } else {

    TeamworkOperator logged= (TeamworkOperator) pageState.getLoggedOperator();
    Person loggedPerson = logged.getPerson();


    List<Resource> candRes= ResourceBricks.fillWorkGroup(pageState);
    // robicch 7/7/2015 perche altrimenti se puoi leggere una risorsa e.g.: company vedi anche tutti quelli sotto
    List<Resource> resources=new ArrayList();
    for (Resource res : candRes) {
      if (res.equals(loggedPerson)||res.hasPermissionFor(logged, TeamworkPermissions.resource_manage))
        resources.add(res);
    }


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
      Bricks.addReferral(referral.getId(), referral.getClass(),  self);
    self.addClientEntry("WG_IDS",JSP.w(pageState.getEntry("WG_IDS").stringValueNullIfEmpty()));

    SerializedList<Permission> permissions = new SerializedList();
    permissions.add(TeamworkPermissions.resource_manage);
    self.addClientEntry("PERM_REQUIRED", permissions);

    self.addClientEntry("FOCUS_MILLIS", focusedMillis + "");
    self.addClientEntry("SCALE_TYPE", scale.scaleName);
    Form form = new Form(self);
    form.usePost=false;
    form.alertOnChange = false;
    pageState.setForm(form);

    form.start(pageContext);

%>
<h1><%=I18n.get("PLAN_BY_RESOURCE")%></h1>

<div class="optionsBar clearfix">
  <div class="filterElement centered"><%

    //check if logged can inspect more see more resources
    boolean canInspectMoreResources = logged.hasPermissionFor(TeamworkPermissions.resource_manage);
    canInspectMoreResources = canInspectMoreResources || JSP.ex(loggedPerson.getMyStaff());

    ButtonSupport workgroup = ButtonSubmit.getSubmitInstanceInBlack(pageState.getForm(), request.getContextPath() + "/applications/teamwork/workgroup/workgroupPopup.jsp", 700, 550);
    workgroup.toolTip = I18n.get("CHANGE_WORK_GROUP");
    workgroup.label = "";
    workgroup.iconChar = "r";
    workgroup.enabled=canInspectMoreResources;


    for (Resource res : resources) {

  %><div class="workgroupElement">
    <img src="<%=res.bricks.getAvatarImageUrl()%>" title="<%=res.getDisplayName()%>" class='face' align='absmiddle'>
  </div><%
    }
  %>

    <div class="workgroupElement"><%workgroup.toHtml(pageContext);%></div>
  </div>

  <div class="listPagedisplayOptions"><%

    pageState.getEntryOrDefault("SHOW_ONLY_ACTIVE_ASSIGS");
    CheckField onlyActive = new CheckField(I18n.get("SHOW_ONLY_ACTIVE_ASSIGS"),"SHOW_ONLY_ACTIVE_ASSIGS", "&nbsp;", false);
    onlyActive.additionalOnclickScript="obj('"+form.id+"').submit();";
    onlyActive.toHtmlI18n(pageContext);

    pageState.getEntryOrDefault("SHOW_ONLY_I_CAN_MANAGE");
    CheckField onlyCanManage = new CheckField(I18n.get("SHOW_ONLY_I_CAN_MANAGE"),"SHOW_ONLY_I_CAN_MANAGE", "&nbsp;", false);
    onlyCanManage.additionalOnclickScript="obj('"+form.id+"').submit();";
    onlyCanManage.toHtmlI18n(pageContext);

  %></div>

</div>
<jsp:include page="planCss.jsp"/>

<table class="table dataTable edged fixHead fixFoot planResWL" cellspacing="0" cellpadding="0">

<%----------------------------------------------- CALENDAR DAYS BEGIN -------------------------------------------------------%>
<thead class="dataTableHead">
<tr>
  <td colspan="99" style="padding:0;">
    <jsp:include page="monthsMovementBar.jsp"/>
  </td>
</tr>

<tr>
  <th class="dayHeader" colspan="4">&nbsp;</th>
  <%
    long todayMorning=new CompanyCalendar().setAndGetTimeToDayStart().getTime();
    while (cc.getTimeInMillis() <scale.endPointTime) {
  %><th style="white-space: normal" t="<%=DateUtilities.dateToInt(cc.getTime())%>" dateLabel="<%=DateUtilities.dateToString(cc.getTime(), "EEEE dd" )%>" class="dayHeader <%=cc.isToday()?"dayTHeader":""%> <%=!cc.isWorkingDay()?"dayHHeader":""%> <%=cc.getTimeInMillis()<todayMorning?"inThePast":""%>"><%=DateUtilities.dateToString(cc.getTime(), "EEE")%><br><%=DateUtilities.dateToString(cc.getTime(), "dd")%></th><%
    cc.add(CompanyCalendar.DAY_OF_MONTH,1);
  }
%>
</tr>
</thead>
<%----------------------------------------------- CALENDAR DAYS END-------------------------------------------------------%>
<%

  PlanUtilities planUtilities = new PlanUtilities();
  planUtilities.ignoreWorklogWhenComputingLoad=I18n.isActive("CUSTOM_FEATURE_OPLOAD_IGNORE_WORKLOG");
  //planUtilities.ignorePlanByIssue=true;

  if (I18n.isActive("CUSTOM_FEATURE_USE_SIMPLE_PLAN")) {
    planUtilities.preloadPlanCache(resources, scale.getPeriod());  // prolad complete plan for resources
  }

  //--------------------------  BEGIN LOOP RESOURCES --------------------------------------------
  for (Resource resource:resources){
    cc.setTime(scale.startPointDate);

    PlanUtilities.ResourceLoad workLoad = planUtilities.getTotalLoadAndWork(resource, scale.getPeriod());

    PageSeed resed= pageState.pageFromRoot("resource/resourceAssignments.jsp");
    resed.mainObjectId=resource.getId();
    resed.command= Commands.EDIT;
    ButtonLink resbl=new ButtonLink(resource.getDisplayName(),resed);


%>
<tbody resId="<%=resource.getId()%>">
<tr class="tableSection"><td colspan="99" style="background-color: #fff; border-right: none; border-top: 3px solid #ccc">
  <div class="childNode">
    <h2><img src="<%=resource.bricks.getAvatarImageUrl()%>" title="<%=resource.getDisplayName()%>" class='face small' align='absmiddle'>
      <%resbl.toHtmlInTextOnlyModality(pageContext);%></h2>
  </div>
</td></tr>

<%------------------------------------------------- START TOTALS --------------------------------------------------%>
<tr class="totalRow">
  <td class="total" style="text-align:right;" colspan="4"><label><%=I18n.get("OPERATOR_LOAD")%>&nbsp;</label></td>

  <%
    int col = 0;
    cc.setTime(scale.startPointDate);
    while (cc.getTimeInMillis() <scale.endPointTime) {
      int day = DateUtilities.dateToInt(cc.getTime());

      // chech the agenda for unavailability
      boolean inVacation=  workLoad.resourceWorkPower.get(day)<=-2;
      //compute the working hours per day for the resource (in milllis)
      long workingHourPerDay = workLoad.resourceWorkPower.get(day);

      long totEstimated = workLoad.totalLoadAndWork.get(day).workLoad;
      String sTotEstimated = totEstimated<0?"&nbsp;":DateUtilities.getMillisInHoursMinutes(totEstimated);

      String tdClass = "";
      if (inVacation)
        tdClass += "notAvailable ";
      if (totEstimated>workingHourPerDay+2)
        tdClass += "exceeded ";
      else if (totEstimated<workingHourPerDay)
        tdClass += "underplanned ";

      if (!cc.isWorkingDay())
        tdClass += "dayH ";

      if (cc.getTimeInMillis()<todayMorning)
        tdClass += "inThePast ";



  %><td t="<%=day%>" class="<%=tdClass%> total day" style="cursor:pointer;text-align:center; min-width: 35px" onclick="headerClick($(this))"><%=sTotEstimated%></td><%

    cc.add(CompanyCalendar.DAY_OF_MONTH, 1);
    col++;
  }

%>
</tr>
<%------------------------------------------------- END TOTALS --------------------------------------------------%>

<%
  // -------------------------------------------------------------------------------------------------  HERE STARTS THE CORE ----------------------------------------------------------------------------

  // used for display purpose only
  List<Period> periodsOfTasks=new ArrayList();

  int maxRow = workLoad.assignementsDetailLine.size() - 1;
  int row = 0;
  // iterate on resources


  boolean showICanManage = pageState.getEntry("SHOW_ONLY_I_CAN_MANAGE").checkFieldValue();

  List<Assignment> assignmentList = resource.getActiveAssignments(scale.getPeriod(), pageState.getEntry("SHOW_ONLY_ACTIVE_ASSIGS").checkFieldValue(), false, true);


  if (I18n.isActive("CUSTOM_FEATURE_ORDER_TASK_BY_CODE")){
    Collections.sort(assignmentList, new TeamworkComparators.AssignmentComparatorByTaskCode());
  } else {
    Collections.sort(assignmentList, new TeamworkComparators.AssignmentComparatorByTask());
  }

  for (Assignment assig : assignmentList) {
    Task task= assig.getTask();

    //devo ignorare i task in stato STATUS_UNDEFINED del tutto, mentre gli aborted sono eliminati giorno per giorno in base alla data di cambio status
    if (TaskStatus.STATUS_UNDEFINED.equals(task.getStatus()))
      continue;



    boolean assigResourceCanManage=assig.hasPermissionFor(logged,TeamworkPermissions.resource_manage);

    //salta se non lo puoi gestire e se hai attiva l'opzione
    if (showICanManage && ( !assigResourceCanManage || !task.isAssigned(loggedPerson) ))
      continue;


    //add periods for display task occupancy (very late in the page....).estimOnPlanned input
    periodsOfTasks.add(task.getSchedule());

%> <tr taskId="<%=task.getId()%>" assId="<%=assig.getId()%>" class="<%=assig.isEnabled()?"":"assDisabled"%>">
  <td cellpadding="4" nowrap title="<%=task.getDisplayName()%>" class="columnTaskName">


    <%--<div class="planTaskname">--%>
      <%if (I18n.isActive("CUSTOM_FEATURE_ALWAYS_SHOW_TASK_PATH")){
      %><div class="pathSmall"><%=task.getPath(" / ", false)%></div><%
      }


      PageSeed editR = pageState.pageInThisFolder("planByTask.jsp", request);
      editR.mainObjectId = task.getId();
      editR.addClientEntry("FOCUS_MILLIS", focusedMillis);
      //ButtonLink link2Assig = new ButtonLink(task.bricks.getDisplayName() + " (" + task.getAssignments().size() + ")", editR);
      ButtonLink link2Assig = new ButtonLink(task.getName(), editR);
      //link2Assig.enabled=task.bricks.canRead;
      link2Assig.enabled=task.hasPermissionFor(logged,TeamworkPermissions.resource_manage) || assig.getResource().equals(logged.getPerson());
      link2Assig.toHtmlInTextOnlyModality(pageContext);


      cc.setTimeInMillis(System.currentTimeMillis());
      long planned = assig.getWorklogPlanned(cc.setAndGetTimeToDayStart());


    %>
<%--</div>--%>

        <span class="estimOnPlanned" nowrap >
          <input type="text" class="validated" entrytype="DURATIONMILLIS"
                 value="<%=DateUtilities.getMillisInHoursMinutes(assig.getEstimatedWorklog())%>"
                 title="<%=I18n.get("WORKLOG_ESTIMATED")%>"
          <%--=task.bricks.canManageAssignment?"":"readOnly"--%>
            <%=assigResourceCanManage?"":"readOnly"%>
                 onblur="updateEstimation($(this));" oldvalue="1">
          - <span class="totDone button textual" onclick="openWorklogList($(this));" title="<%=I18n.get("WORKLOG_DONE")%>" totDone="<%=assig.getWorklogDone()%>"><%=DateUtilities.getMillisInHoursMinutes(assig.getWorklogDone())%></span>
          =
          <span class="<%=planned>(assig.getEstimatedWorklog()-assig.getWorklogDone())?"overPlanned":""%>" title="<%=I18n.get("PLAN_OVERPLANNED_LEGENDA")%>">
            <span class="toBeDone" title="<%=I18n.get("TO_BE_DONE")%>" toBeDone="<%=assig.getEstimatedWorklog()-assig.getWorklogDone()%>"><%=DateUtilities.getMillisInHoursMinutes(assig.getEstimatedWorklog() - assig.getWorklogDone())%></span>
            (<span class="totPlanned" title="<%=I18n.get("PLANNED_WORK")%>" totPlanned="<%=planned%>"><%=DateUtilities.getMillisInHoursMinutes(planned)%></span>)
          </span>
        </span>
  </td>
  <td class='textSmall columnTaskCode'><span><%=JSP.w(task.getCode())%></span></td>
  <td class='textSmall'><%= assig.getRole().getCode() %></td>
  <td align="center" width="30"><%
    boolean showActions=I18n.isActive("CUSTOM_FEATURE_RECONCILE_PLAN")||I18n.isActive("CUSTOM_FEATURE_FILL_PLAN_0");
  %><%if(showActions){%><span class="teamworkIcon" style="cursor:pointer;" onclick="openLineAction($(this))">g</span><%}%>
  </td>
  <%


    Period taskPeriod = task.getSchedule().getPeriod();

    // ignoring task longer than 5 years
    if (taskPeriod.getEndDate().getTime() - taskPeriod.getStartDate().getTime() >= CompanyCalendar.MILLIS_IN_YEAR * 5) {
  %><td colspan="99" align="center"><%=I18n.get("LONG_TASK_ARE_NOT_PLANNABLE")%></td></tr><%
    continue;
  }


  // ------------------------------------------------ START loop the month ------------------------------------------------
  col = 0;
  cc.setTime(scale.startPointDate);
  while (cc.getTimeInMillis() <scale.endPointTime) {

    int day = DateUtilities.dateToInt(cc.getTime());

    //compute the working hours per day for the resource (in milllis)
    long workingHourPerDay = workLoad.resourceWorkPower.get(day);

    //get plan
    Map<Integer, PlanUtilities.AssigLoadAndWork> assigLine = workLoad.assignementsDetailLine.get(assig);
    long byPlan = -1;
    boolean hasNotes = false;
    if (assigLine != null) {
      PlanUtilities.AssigLoadAndWork aDayPlan = assigLine.get(day);
      if (aDayPlan != null) {
        byPlan = aDayPlan.loadByPlan;
        hasNotes = aDayPlan.hasNotes;
      }
    }



    //byPlan==0 se non devi lavorare   byPlan==1 se non pianificato
    String sByPlan = byPlan <0 ? "" : DateUtilities.getMillisInHoursMinutes(byPlan);

    PlanUtilities.LoadAndWork totalLoadAndWork = workLoad.totalLoadAndWork.get(day);
    long totEstimated = totalLoadAndWork==null?0:totalLoadAndWork.workLoad;

    // chech the agenda for unavailability
    boolean inVacation=  workLoad.resourceWorkPower.get(day)<=-2;

    boolean inTaskScope=true;
    String tdClass="";
    if (inVacation)
      tdClass = "notAvailable ";
    if (totEstimated>workingHourPerDay+2)
      tdClass += "exceeded ";
    else if (totEstimated<workingHourPerDay)
      tdClass += "underplanned ";

    if (!taskPeriod.contains(cc.getTime())){
      tdClass += "outOfScope ";
      inTaskScope=false;
    }
    if (!cc.isWorkingDay())
      tdClass += "dayH ";
    if (! Assignment.ACTIVITY_ALL_IN_ONE.equals(assig.getActivity()))
      tdClass += "routine";
    if (cc.getTimeInMillis()<todayMorning)
      tdClass += "inThePast ";
    if (byPlan==0)
      tdClass += "zero_panned ";

    if (byPlan>0)
      tdClass += "hasPlan ";

    if (hasNotes)
      tdClass += "hasNotes ";


    // DRAW THE INPUT
%><td class="day tdCell <%=tdClass%>"  t="<%=day%>" whpd="<%=workingHourPerDay%>">
  <input <%=assigResourceCanManage && inTaskScope?"":"readOnly"%> type="text" class="cell" value="<%=JSP.w(sByPlan)%>" oldvalue="y">
  <span class="noteEditorButton" title="<%=I18n.get("NOTES")%>"></span>
</td><%

        cc.add(CompanyCalendar.DAY_OF_MONTH, 1);
        col++;
      }
      // ------------------------------------------------ END loop the month ------------------------------------------------

row++;
%></tr><%
  } // ------------------------------------------------ END loop the assigs ------------------------------------------------

%></tbody><tbody><tr><td colspan="99">&nbsp;</td></tr></tbody><%
  } // ------------------------------------------------ END loop the resources ------------------------------------------------


%><tfoot>
<tr><td colspan="99">
  <%
    JspHelper wh = new JspHelper("planLegenda.jsp");
    wh.toHtml(pageContext);
%>
</td></tr>
</tfoot>
</table>
<%
  Container explain = Container.getPopupInstance(I18n.get("OPERATOR_LOAD_EXPLAIN") + "&nbsp;<span id='expltitle'></span>");
  explain.id="LDDETAIL";
  explain.width = "50%";
  explain.closeable = true;
  explain.draggable=true;
  explain.saveStatus=true;
  explain.start(pageContext);
  %><div id="olexplain"></div><%
  explain.end(pageContext);

%>

<%-- ------------------------------------------------ legenda ------------------------------------------------ --%>



<%---------------------------------------------------------------------------------  JAVSCRIPT BEGIN ---------------------------------------------------------------------%>
<jsp:include page="planJS.jsp"/>
<script type="text/javascript">

  $(function(){
    $("#PLAN_MENU").addClass('selected');
    <%-- BINDING ---%>
    $(":text.cell").bind("focus", cellFocus).bind("blur",saveCell).bind("keydown", cellKey).parent().dblclick(noteCellDBLClick);
    $(".noteEditorButton").click(noteButtonClick);
  });


  <%---------------------------------------------   FOCUS  ---------------------------------------------%>
  function headerClick(theTd) {
    var theTr = theTd.closest("tr");
    var request = {resId:theTr.closest("[resId]").attr("resId"),day:theTd.attr("t"),forPage:"plan"};
    $("#olexplain").load("partExplainWorkload.jsp", request);

    var isHidden = $("#<%=explain.getContainerId()%>").is(":hidden");
    $("#<%=explain.getContainerId()%>").trigger("show");
    if (isHidden) {
      nearBestPosition(theTd, "<%=explain.getContainerId()%>");
    }
    return false;

  }

</script>

<%-------------------------------------------------------------------------------  TIME BAR -------------------------------------------------------------------%>
<%
  pageState.attributes.put("FOCUSED_PERIOD",scale.getPeriod());// used in the bar
%><jsp:include page="../../parts/timeBar.jsp"/><%


    form.end(pageContext);

  }
%>
