<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.plan.PlanUtilities, com.twproject.resource.Resource, com.twproject.resource.ResourceBricks,
                 com.twproject.security.TeamworkPermissions, com.twproject.task.Assignment, com.twproject.task.Task, com.twproject.waf.TeamworkHBFScreen, org.jblooming.agenda.CompanyCalendar,
                 org.jblooming.agenda.Period, org.jblooming.agenda.Scale, org.jblooming.ontology.IdentifiableSupport, org.jblooming.ontology.SerializedList, org.jblooming.security.Permission,
                 org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.Bricks, org.jblooming.waf.ScreenArea, org.jblooming.waf.SessionState,
                 org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.button.ButtonSupport,
                 org.jblooming.waf.html.container.Container, org.jblooming.waf.html.display.Img, org.jblooming.waf.html.layout.HtmlColors, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.HashSet, java.util.List, java.util.Map, java.util.Set, org.jblooming.waf.html.input.CheckField" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    body.areaHtmlClass="lreq20 lreqPage";
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);
  } else {

    // -------------------------------------------------------------------------------  BASIC DATA ----------------------------------------------------------------------------

    PageSeed self = pageState.thisPage(request);
    TeamworkOperator loggedOperator = (TeamworkOperator) pageState.getLoggedOperator();


    // get the focused millis
    long focusedMillis= pageState.getEntry("FOCUS_MILLIS").longValueNoErrorNoCatchedExc();
    focusedMillis= focusedMillis==0?System.currentTimeMillis():focusedMillis;
    pageState.addClientEntry("FOCUS_MILLIS",focusedMillis);

    List<Resource> resources= ResourceBricks.fillWorkGroup(pageState);
    self.addClientEntry("WG_IDS",JSP.w(pageState.getEntry("WG_IDS").stringValueNullIfEmpty()));


    //get the eventual referral
    IdentifiableSupport referral = Bricks.getReferral(pageState);

    if (referral != null)
      Bricks.addReferral(referral.getId(), referral.getClass(), self);


    // get the current scale factor
    String ssct=pageState.getEntryOrDefault("SCALE_TYPE", Scale.ScaleType.SCALE_3WEEK.toString()).stringValue();

    // define the scale and increment of ticks
    Scale scale = Scale.getScaleAndSynch(Scale.ScaleType.valueOf(ssct), focusedMillis, true, SessionState.getLocale());

    SerializedList<Permission> permissions = new SerializedList();
    permissions.add(TeamworkPermissions.resource_manage);


    self.addClientEntry("PERM_REQUIRED", permissions);
    self.addClientEntry("FOCUS_MILLIS", focusedMillis + "");
    self.addClientEntry("SCALE_TYPE",scale.scaleName);
    self.addClientEntry("enableTags",true);

    self.setPopup(pageState.isPopup());
    Form form = new Form(self);
    form.usePost=false;
    form.alertOnChange = true;
    pageState.setForm(form);

    form.start(pageContext);

    //check if logged can inspect more see more resources
    boolean canInspectMoreResources = loggedOperator.hasPermissionFor(TeamworkPermissions.resource_manage);
    canInspectMoreResources = canInspectMoreResources || JSP.ex(loggedOperator.getPerson().getMyStaff());

    ButtonSupport workgroup = ButtonSubmit.getSubmitInstanceInBlack(pageState.getForm(), request.getContextPath() + "/applications/teamwork/workgroup/workgroupPopup.jsp", 700, 550);
    workgroup.toolTip = I18n.get("CHANGE_WORK_GROUP");
    workgroup.label = "";
    workgroup.iconChar = "r";
    workgroup.enabled= canInspectMoreResources;



  //define the month
  CompanyCalendar cc = new CompanyCalendar();

  Period thePeriod=scale.getPeriod();

  //set calender to first day of month
  cc.setTime(thePeriod.getStartDate());

  int height=150;


  PageSeed oploadtm = pageState.pageInThisFolder("operatorLoadTextual.jsp",request);
  oploadtm.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));
  ButtonLink textual = new ButtonLink(I18n.get("TEXTUAL"),oploadtm);

%>

<script>$("#PLAN_MENU").addClass('selected');</script>
<h1><%=I18n.get("OPERATOR_LOAD")%>&nbsp;&nbsp; <span style="font-size:50%;"><%textual.toHtmlInTextOnlyModality(pageContext);%></span></h1>

<jsp:include page="planCss.jsp"/>
<style type="text/css">

    .loadBounderE{
        height:<%=height%>px;
    }


</style>
<div class="optionsBar">
  <div class="filterElement centered"><%

      for (Resource res : resources) {
    %><div class="workgroupElement"><img src="<%=res.bricks.getAvatarImageUrl()%>" title="<%=res.getDisplayName()%>" class='face' align='absmiddle'></div><%
      }
    %><div class="workgroupElement"><%workgroup.toHtml(pageContext);%></div>
  </div>
</div>
<div class="listPagedisplayOptions"><%
  pageState.getEntryOrDefault("OPLOAD_IGNORE_WORKLOG");
  CheckField showCh = new CheckField("OPLOAD_IGNORE_WORKLOG", "&nbsp;", false);
  showCh.preserveOldValue = false;
  showCh.additionalOnclickScript = new ButtonSubmit(form).generateJs().toString();
  showCh.toHtmlI18n(pageContext);

%></div>



<table class="table dataTable edged fixHead">

  <thead class="dataTableHead">
  <tr>
    <td colspan="99" style="padding:0;">
      <jsp:include page="monthsMovementBar.jsp"/>
    </td>
  </tr>
  </thead>


  <%
    // -------------------------------------------------------------------------------  HERE STARTS THE CORE ----------------------------------------------------------------------------

    Set<Task> visitedTask=new HashSet();

    // iterate on resources
    for (Resource res : resources) {

      double hScaleFactor= ((double)height)/((res.getWorkDailyCapacity()+CompanyCalendar.MILLIS_IN_HOUR));


      PlanUtilities planUtilities = new PlanUtilities();
      // si sovrascrive il default
      planUtilities.ignoreWorklogWhenComputingLoad=pageState.getEntryOrDefault("OPLOAD_IGNORE_WORKLOG","NO").checkFieldValue();
      PlanUtilities.ResourceLoad loadPlan = planUtilities.getTotalLoadAndWork(res, thePeriod);


  %><tr resId="<%=res.getId()%>" class="tableSection"><td nowrap width="1%" colspan="35"><div class="childNode"><%
  PageSeed editR = pageState.pageFromRoot("resource/resourceEditor.jsp");
  editR.setCommand(Commands.EDIT);
  editR.mainObjectId = res.getId();
  //ButtonLink bl = new ButtonLink(res.getDisplayName() + " (" + loadPlan.assignmentsSorted.size() + ")", editR);
  ButtonLink bl = new ButtonLink(res.getDisplayName(), editR);
  bl.enabled=res.hasPermissionFor(loggedOperator,TeamworkPermissions.resource_canRead);

%><h2><img src="<%=res.bricks.getAvatarImageUrl()%>" title="<%=res.getDisplayName()%>" class='face small' align='absmiddle'> <%

  bl.toHtmlInTextOnlyModality(pageContext);
%>&nbsp;&nbsp;<%
  if (res.hasPermissionFor(loggedOperator,TeamworkPermissions.resource_manage) || res.equals(loggedOperator.getPerson()) ) {
    PageSeed pl = pageState.pageFromRoot("task/plan/planByResource.jsp");
    Bricks.addReferral(res.getId(), Resource.class, pl);
    pl.addClientEntry("FOCUS_MILLIS", focusedMillis);
    pl.setCommand("FIND_BY_ENTITY");
    ButtonLink link2Plan = new ButtonLink(I18n.get("WORKLOG_SEE_PLAN"), pl);
    link2Plan.additionalCssClass = "small lreq30 lreqLabel";
    link2Plan.toHtmlInTextOnlyModality(pageContext);
  }


%></h2></div></td></tr>
  <tr><%

    PageSeed agenda= pageState.pageFromRoot("agenda/agendaWeekDay.jsp");
    cc.setTime(thePeriod.getStartDate());
    while (cc.getTimeInMillis() <scale.endPointTime) {
      agenda.addClientEntry("FOCUS_MILLIS",cc.getTimeInMillis());
      int day= DateUtilities.dateToInt(cc.getTime());
      long todayCapacity=loadPlan.resourceWorkPower.get(day);
      PlanUtilities.LoadAndWork tlpd= loadPlan.totalLoadAndWork.get(day);

      String tdClass="";
      if (todayCapacity<=-2)
        tdClass="notAvailable";
      else if (cc.isToday())
        tdClass="dayTHeader";
      else if (!cc.isWorkingDay())
        tdClass="dayHHeader";

      ButtonLink agendaBl=new ButtonLink(agenda);
      agenda.addClientEntry("FOCUS_MILLIS", focusedMillis);
      agenda.command="FIND_BY_ENTITY";
      Bricks.addReferral(res.getId(),Resource.class,agenda);


  %><td class=" dayLabel <%=tdClass%> dayHeader" onclick="<%=agendaBl.generateJs().toString()%>"
        title="<%=I18n.get("WORKING_HOUR_TOTAL")+": "+DateUtilities.getMillisInHoursMinutes(todayCapacity)+"&nbsp;"+I18n.get("WORKLOG_DONE_SHORT")+": "+DateUtilities.getMillisInHoursMinutes(tlpd==null?0:tlpd.workDone)%>">
    <%=DateUtilities.dateToString(cc.getTime(), "EEE")%><br><%=DateUtilities.dateToString(cc.getTime(), "dd")%></td><%
      cc.add(CompanyCalendar.DATE,1);
    }
  %></tr>
  <tr resId="<%=res.getId()%>"><%





  // loop the month
  cc.setTime(thePeriod.getStartDate());
  while (cc.getTimeInMillis() <scale.endPointTime) {
    int day= DateUtilities.dateToInt(cc.getTime());
    long todayWorkPower=loadPlan.resourceWorkPower.get(day);
    PlanUtilities.LoadAndWork twlaw = loadPlan.totalLoadAndWork.get(day);
    long todayWorkLoad = twlaw==null? 0:twlaw.workLoad;
    long todayWorkDone= twlaw==null? 0:twlaw.workDone;

    long millisUnavailToday = res.getWorkDailyCapacity() - todayWorkPower;


    boolean inThePast= (cc.getTimeInMillis()+CompanyCalendar.MILLIS_IN_DAY)<System.currentTimeMillis();

    String tdClass="";
    if (todayWorkPower<=-2)
      tdClass="notAvailable";
    else if (cc.isToday())
      tdClass="dayT";
    else if (!cc.isWorkingDay())
      tdClass="dayH";



%> <td valign="bottom" class="day <%=tdClass%> <%=inThePast?"inThePast":""%>"  t="<%=day%>"  ><div class="loadBounderE"><%
%><div class="loadBounderI"><%

  for (int ai=loadPlan.assignmentsSorted.size()-1;ai>=0;ai--){
    Assignment ass= loadPlan.assignmentsSorted.get(ai);
    visitedTask.add(ass.getTask());
    Map<Integer, PlanUtilities.AssigLoadAndWork> assLoad = loadPlan.assignementsDetailLine.get(ass);
    if (assLoad== null)
      continue;
    PlanUtilities.AssigLoadAndWork assigLoadAndWork = assLoad.get(day);
    if (assigLoadAndWork!=null && assigLoadAndWork.workLoad >0){
      //draw the div
      int h = (int) (assigLoadAndWork.workLoad * hScaleFactor);
      boolean canRead = ass.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.task_canRead);
      %><div style="height:<%=h<=0?1:h%>px;" class="loadElem color<%=ass.getTask().getId()%>" title="<%=(canRead?ass.getName():"----")+": "+DateUtilities.getMillisInHoursMinutes(assigLoadAndWork.workLoad)%>" ></div><%
    }
  }

  //draw unavailabilities
  if (todayWorkPower>0 && millisUnavailToday>0)  {
    int h = (int) (millisUnavailToday * hScaleFactor);
%><div style="height:<%=h<=0?1:h%>px;" class="loadElem notAvailable" title="<%=I18n.get("PLAN_IN_VACATION_LEGENDA")%>" ></div><%
  }


  if (todayWorkPower>0 ){
%><div class="dayWorkCap" style="top:-<%=((int)(res.getWorkDailyCapacity()*hScaleFactor))%>px;" title="<%=I18n.get("WORKING_HOUR_TOTAL")+": "+DateUtilities.getMillisInHoursMinutes(todayWorkPower)%>"></div><%
  }

  int top = height - 15 - (int) ((double) (todayWorkLoad+millisUnavailToday) * hScaleFactor);
%></div><%

  //draw work done
  int h = (int) ((double) (todayWorkDone) * hScaleFactor);
%><div style="height:<%=h<=0?1:h%>px; top:<%=height-h%>px;" class="workDone" title="<%=I18n.get("WORKLOG_DONE")+": "+DateUtilities.getMillisInHoursMinutes(todayWorkDone)%>" ></div><%


    double todayPerc = (double) todayWorkLoad * 100d / (double) todayWorkPower;
    boolean printPerc = todayWorkLoad > 0 && todayWorkPower > 0;
  %> <div class="loadPerc" title="<%=I18n.get("ASSIGNMENT_WORKLOG_ESTIMATION")+": "+DateUtilities.getMillisInHoursMinutes(todayWorkLoad)%>" style="top:<%=top<5?5:top%>px;"><span style="display:<%=printPerc?"inline":"none"%>;"><%=printPerc ?JSP.perc(todayPerc,0)+"%":""%></span></div><%
  //check milestones
  Set<Task> milestones = loadPlan.milestones.get(day);
  if (JSP.ex(milestones)){
    for (Task t: milestones){
      Img miles = new Img("milestone.png", t.getDisplayName());
      miles.style="position:absolute;bottom:0px;";
      miles.toHtml(pageContext);
    }
  }

%></div><%
%></td><%

    cc.add(CompanyCalendar.DAY_OF_MONTH,1);
  }
%></tr><%
  }
%></table>
<style type="text/css">
  <%
    // generate color styles for task
    for (Task t : visitedTask){
      long tot=1;
      String name = t.getDisplayName();
      for (int i=0;i< name.length();i++){
        tot=tot*name.charAt(i);
      }
      %>.color<%=t.getId()%> {
    background-color: <%=HtmlColors.distributeColor((int)(tot%5000),5000)%>;
  }

  <%
    }
  %>
</style><%



  // -------------------------------------------------------------------------------  TIME BAR -------------------------------------------------------------------
  pageState.attributes.put("FOCUSED_PERIOD",thePeriod);// used in the bar
  if (!pageState.isPopup()){
%><jsp:include page="../../parts/timeBar.jsp"/><%
  }
%><jsp:include page="loadLegenda.jsp"/><%

  form.end(pageContext);

  Container explain= Container.getPopupInstance(I18n.get("OPERATOR_LOAD_EXPLAIN")+"&nbsp;<span id='expltitle'></span>");
  explain.width="50%";
  explain.level=1;
  explain.closeable=true;
  explain.start(pageContext);
%><div id="olexplain"></div><%
  explain.end(pageContext);

%>

<script type="text/javascript">
  function showExplain () {
    var theTd=$(this);
    var theTr=theTd.parent();
    var request = {resId:theTr.attr("resId"),day:theTd.attr("t"),forPage:"load"};
    $("#olexplain").load("partExplainWorkload.jsp",request);


    var isHidden=$("#<%=explain.getContainerId()%>").css("display")=="none";
    $("#<%=explain.getContainerId()%>").show().centerOnScreen();
    if(isHidden){
      $("#<%=explain.getContainerId()%>").keepItVisible();
    }
    return false;

  };
  $("td[t]").bind("click",showExplain).css("cursor","pointer");
</script>
<div class="boxInfo"><span class="teamworkIcon info">|</span> <span class="descrEl"><%=I18n.get("OPERATOR_LOAD_EXPLAIN_DETAIL")%></span></div><%
  }
%>
