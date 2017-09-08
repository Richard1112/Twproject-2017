<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.plan.PlanUtilities, com.twproject.resource.Resource, com.twproject.resource.ResourceBricks,
                 com.twproject.security.TeamworkPermissions, com.twproject.waf.TeamworkHBFScreen, org.jblooming.agenda.CompanyCalendar, org.jblooming.agenda.Scale, org.jblooming.ontology.IdentifiableSupport,
                 org.jblooming.ontology.SerializedList, org.jblooming.security.Permission, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.Bricks,
                 org.jblooming.waf.ScreenArea, org.jblooming.waf.SessionState, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState, java.util.List, org.jblooming.waf.html.input.CheckField" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

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

    List<Resource> resources= ResourceBricks.fillWorkGroup(pageState);
    self.addClientEntry(pageState.getEntry("WG_IDS"));
    self.addClientEntry("WG_IDS", JSP.w(pageState.getEntry("WG_IDS").stringValueNullIfEmpty()));

    // get the focused millis
    long focusedMillis= pageState.getEntry("FOCUS_MILLIS").longValueNoErrorNoCatchedExc();
    focusedMillis= focusedMillis==0?System.currentTimeMillis():focusedMillis;
    pageState.addClientEntry("FOCUS_MILLIS",focusedMillis);

    // get the current scale factor
    String ssct=pageState.getEntryOrDefault("SCALE_TYPE", Scale.ScaleType.SCALE_2WEEK.toString()).stringValue();

    // define the scale and increment of ticks
    Scale scale = Scale.getScaleAndSynch(Scale.ScaleType.valueOf(ssct), focusedMillis, true, SessionState.getLocale());

    //get the eventual referral
    IdentifiableSupport referral = Bricks.getReferral(pageState);

    if (referral != null)
      Bricks.addReferral(referral.getId(), referral.getClass(), self);


    SerializedList<Permission> permissions = new SerializedList();
    permissions.add(TeamworkPermissions.resource_manage);

    self.addClientEntry("PERM_REQUIRED", permissions);
    self.addClientEntry("FOCUS_MILLIS", focusedMillis);
    self.addClientEntry("SCALE_TYPE", scale.scaleName);
    Form form = new Form(self);
    form.usePost=false;
    form.id = "WORKGROUP";
    form.alertOnChange = true;
    pageState.setForm(form);

    form.start(pageContext);

%><script>$("#PLAN_MENU").addClass('selected');</script><%

  //check if logged can inspect more see more resources
  boolean canInspectMoreResources = logged.hasPermissionFor(TeamworkPermissions.resource_manage);
  canInspectMoreResources = canInspectMoreResources || JSP.ex(logged.getPerson().getMyStaff());

  ButtonSupport workgroup = ButtonSubmit.getSubmitInstanceInBlack(pageState.getForm(), request.getContextPath() + "/applications/teamwork/workgroup/workgroupPopup.jsp", 700, 550);
  workgroup.label = "";
  workgroup.toolTip = I18n.get("CHANGE_WORK_GROUP");
  workgroup.iconChar = "r";
  workgroup.enabled=canInspectMoreResources;

  PageSeed oploadtm = pageState.pageInThisFolder("operatorLoad.jsp",request);
  oploadtm.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));
  ButtonSubmit graphical = new ButtonSubmit(I18n.get("GRAPHIC"),"" ,form);
  graphical.additionalCssClass="lreq20 lreqLabel lreqActive";
  graphical.variationsFromForm.href=oploadtm.href;


  //set calender to first day of month
  CompanyCalendar cc = new CompanyCalendar();
  cc.setTime(scale.startPointDate);

%>

<h1><%=I18n.get("OPERATOR_LOAD")%>&nbsp;&nbsp;<small>(<%=I18n.get("TEXTUAL")%>)</small></h1>
<%
  if (referral != null){
    Bricks.getButtonForEntity(referral, pageState);
  }
%>

<div class="optionsBar">
  <div class="filterElement centered"><%

    for (Resource res : resources) {
  %><div class="workgroupElement"><img src="<%=res.bricks.getAvatarImageUrl()%>" title="<%=res.getDisplayName()%>" class='face' align='absmiddle'></div><%
    }
  %><div class="workgroupElement"><%workgroup.toHtml(pageContext);%></div></div>
</div>
<div class="listPagedisplayOptions"><%
  pageState.getEntryOrDefault("OPLOAD_IGNORE_WORKLOG");
  CheckField showCh = new CheckField("OPLOAD_IGNORE_WORKLOG", "&nbsp;", false);
  showCh.preserveOldValue = false;
  showCh.additionalOnclickScript = new ButtonSubmit(form).generateJs().toString();
  showCh.toHtmlI18n(pageContext);

%></div>

<style type="text/css">
  .exceeded{
    background-color:#EF5656;
  }
  .notAvailable{
    background-color:#f7e7df;
  }
</style>


<table class="table dataTable edged fixHead" cellspacing="0" cellpadding="4">
  <thead class="dataTableHead">
  <tr>
    <td colspan="99" style="padding:0;">
      <jsp:include page="monthsMovementBar.jsp"/>
    </td>
  </tr>
  <tr>
    <%
      while (cc.getTimeInMillis() <scale.endPointTime) {
        String tdClass="";
        if (cc.isToday())
          tdClass="dayTHeader";
        else if (!cc.isWorkingDay())
          tdClass="dayHHeader";

    %><td class="dayHeader <%=tdClass%>"><%=DateUtilities.dateToString(cc.getTime(), "EEE")%><br><%=DateUtilities.dateToString(cc.getTime(), "dd")%></td><%
      cc.add(CompanyCalendar.DAY_OF_MONTH,1);
    }
  %>
  </tr>
  </thead>
 <%


  // -------------------------------------------------------------------------------  HERE STARTS THE CORE ----------------------------------------------------------------------------

  // get the collector entries



  // iterate on resources
  for (  Resource res :resources) {

    PlanUtilities planUtilities = new PlanUtilities();
    // si sovrascrive il default
    planUtilities.ignoreWorklogWhenComputingLoad=pageState.getEntryOrDefault("OPLOAD_IGNORE_WORKLOG","NO").checkFieldValue();
    PlanUtilities.ResourceLoad loadPlan = planUtilities.getTotalLoadAndWork(res, scale.getPeriod());

    //List<Assignment> resAssigs = res.getActiveAssignments(scale.getPeriod(), false, false, true);

%> <tr resId="<%=res.getId()%>" class="tableSection">
  <td colspan="35">
    <div class="childNode"><h2><img src="<%=res.bricks.getAvatarImageUrl()%>" title="<%=res.getDisplayName()%>" class='face small' align='absmiddle'><%
    PageSeed editR = pageState.pageFromRoot("resource/resourceEditor.jsp");
    editR.setCommand(Commands.EDIT);
    editR.mainObjectId = res.getId();
    //ButtonLink bl = new ButtonLink(res.getDisplayName() + " (" + resAssigs.size() + ")", editR);
    ButtonLink bl = new ButtonLink(res.getDisplayName() , editR);
    bl.toHtmlInTextOnlyModality(pageContext);
  %></h2></div></td></tr>
  <tr resId="<%=res.getId()%>"><%

  // loop the month
  cc.setTime(scale.startPointDate);
  while (cc.getTimeInMillis() <scale.endPointTime) {
    int day=DateUtilities.dateToInt(cc.getTime());
    long todayCapacity=loadPlan.resourceWorkPower.get(day);
    long todayLoad = loadPlan.totalLoadAndWork.get(day).workLoad;

    String tdClass="";
    if (todayCapacity<=-2)
      tdClass="notAvailable";
    else if (todayCapacity>0 && todayLoad>todayCapacity)
      tdClass="exceeded";
    else if (cc.isToday())
      tdClass="dayT";
    else if (!cc.isWorkingDay())
      tdClass="dayH";



%><td class="day <%=tdClass%>" t="<%=day%>" id="<%=res.getId()+"_"+cc.getTimeInMillis()%>" style="padding:10px 0; text-align: center"><%

  if (todayCapacity>0){
%><%=DateUtilities.getMillisInHoursMinutes(todayLoad)%><%
}else {
%>&nbsp;<%
  }


%></td><%
    cc.add(CompanyCalendar.DAY_OF_MONTH,1);
  }

%></tr><%
  }


%></table><%

  // -------------------------------------------------------------------------------  TIME BAR -------------------------------------------------------------------
  pageState.attributes.put("FOCUSED_PERIOD",scale.getPeriod());// used in the bar
%><jsp:include page="../../parts/timeBar.jsp"/>
<jsp:include page="loadLegenda.jsp"/><%

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
    $("#<%=explain.getContainerId()%>").show();
    if(isHidden){
      //alert(theTd.prop("id"));
      nearBestPosition(theTd.prop("id"),"<%=explain.getContainerId()%>");
    }
    return false;

  };
  $("td[t]").bind("click",showExplain).css("cursor","pointer");
</script>
<%


  }
%>
