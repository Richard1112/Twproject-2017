<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.plan.PlanUtilities, com.twproject.resource.Resource, com.twproject.security.TeamworkPermissions, com.twproject.task.Assignment,
                 com.twproject.task.Task, com.twproject.waf.TeamworkHBFScreen, org.jblooming.agenda.CompanyCalendar, org.jblooming.agenda.Period, org.jblooming.agenda.Scale, org.jblooming.persistence.PersistenceHome,
                 org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.Bricks, org.jblooming.waf.ScreenArea, org.jblooming.waf.SessionState, org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.container.HeadBar, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.input.CheckField,
                 org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.*, com.twproject.task.TaskBricks, com.twproject.utilities.TeamworkComparators, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonSupport, com.twproject.waf.html.StatusIcon, org.jblooming.waf.html.core.JspIncluderSupport, com.twproject.waf.html.TaskHeaderBar, org.jblooming.waf.html.display.PathToObject, org.jblooming.waf.settings.ApplicationState, com.twproject.task.TaskStatus" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    body.areaHtmlClass="lreq30 lreqPage";
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);
  } else {

    // get the task
    Task parentTask = (Task) PersistenceHome.findByPrimaryKey(Task.class, pageState.mainObjectId);

    // check security
    parentTask.testPermission(logged, TeamworkPermissions.task_canRead);

    pageState.setMainObject(parentTask);
%>

<style type="text/css">
  .pathToObject,.currentNode{
  display: inline-block;
}
.pathToObject .currentNode .objectName{
  font-size: 100%;
  font-weight: normal;
}
</style>
<h1><%=I18n.get("PLAN_BY_TASK")%></h1>
<%

    // get the focused millis
    long focusedMillis= pageState.getEntry("FOCUS_MILLIS").longValueNoErrorNoCatchedExc();

    //se il millis non è stato passato vado all'inizio del task se nel futuro altrimenti ad oggi
    if (focusedMillis==0) {
      if (parentTask.getSchedule().getStartDate().getTime()>System.currentTimeMillis())
        focusedMillis = parentTask.getSchedule().getStartDate().getTime();
      else
        focusedMillis = System.currentTimeMillis();
    }

    pageState.addClientEntry("FOCUS_MILLIS",focusedMillis);

    // get the current scale factor
    String ssct=pageState.getEntryOrDefault("SCALE_TYPE", Scale.ScaleType.SCALE_2WEEK.toString()).stringValue();

    // define the scale and increment of ticks
    Scale scale = Scale.getScaleAndSynch(Scale.ScaleType.valueOf(ssct), focusedMillis, true, SessionState.getLocale());

    //set calender to first day of month
    CompanyCalendar cc = new CompanyCalendar();
    cc.setTime(scale.startPointDate);


  
    PageSeed self = pageState.thisPage(request);
    self.mainObjectId = pageState.mainObjectId;
    self.addClientEntry("FOCUS_MILLIS", focusedMillis + "");
    self.addClientEntry("SCALE_TYPE", scale.scaleName);
    Form form = new Form(self);
    form.alertOnChange = false;
    pageState.setForm(form);

    form.start(pageContext);

  // PATH TO OBJECT
  PathToObject pathToObject = new PathToObject(parentTask);
  pathToObject.comparator = new TeamworkComparators.TaskManualOrderComparator();
  pathToObject.canClick = TeamworkPermissions.task_canRead;

  PageSeed back = new PageSeed(ApplicationState.contextPath+"/applications/teamwork/task/taskList.jsp");
  //pathToObject.rootDestination =new ButtonLink(I18n.get("TASK_LIST")+" /", back);


  if (pathToObject.destination==null){
    pathToObject.destination = pageState.thisPage(request);
    pathToObject.destination.setCommand(Commands.EDIT);
  }

  PageSeed taskEd = pageState.pageFromRoot("task/plan/planByTask.jsp");
  taskEd.command=Commands.EDIT;
  pathToObject.selfDestination= taskEd;
  pathToObject.separator="/ ";


%>
<div class="optionsBar clearfix">

  <%  pathToObject.toHtml(pageContext);%>
    <div class="listPagedisplayOptions"><%

      boolean showChildren = pageState.getEntryOrDefault("PLAN_SHOW_CHILDREN").checkFieldValue();
      CheckField showCh = new CheckField("PLAN_SHOW_CHILDREN", "&nbsp;", false);
      showCh.preserveOldValue = false;

      if (parentTask.getChildrenSize() > 0) {
        showCh.additionalOnclickScript = new ButtonSubmit(form).generateJs().toString();
        showCh.label=I18n.get("ASS_SHOW_CHILDREN");
        showCh.toHtml(pageContext);
      }


      boolean activeonly = pageState.getEntryOrDefault("SHOW_ONLY_ACTIVE_ASSIGS").checkFieldValue();
    CheckField onlyActive = new CheckField(I18n.get("SHOW_ONLY_ACTIVE_ASSIGS"),"SHOW_ONLY_ACTIVE_ASSIGS", "&nbsp;", false);
    onlyActive.additionalOnclickScript="obj('"+form.id+"').submit();";
    onlyActive.toHtmlI18n(pageContext);

    %>&nbsp;&nbsp;&nbsp;<%
    new ButtonJS(I18n.get("SHOW_WORK_DETAILS"),"showHideExplain();").toHtmlInTextOnlyModality(pageContext);
  %>
  </div>
</div>


<div class="pathCodeWrapper">
  <%
    // se non hai permesso di vedere la root, ma è attiva la custom feature mostra un path
    if (I18n.isActive("CUSTOM_FEATURE_ALWAYS_SHOW_TASK_PATH")&&!parentTask.getRoot().hasPermissionFor(pageState.getLoggedOperator(),TeamworkPermissions.task_canRead)){
  %><div class="pathSmall" style="max-width:1024px;"><%=parentTask.getPath(" / ", true)%></div><%
  }
%>

</div>

<jsp:include page="planCss.jsp"/>

    <table class="table dataTable edged planResWL fixHead fixFoot" cellspacing="0" cellpadding="2">

      <%----------------------------------------------- CALENDAR DAYS BEGIN -------------------------------------------------------%>
      <thead class="dataTableHead">
      <tr>
        <td colspan="99" style="padding:0;">
          <jsp:include page="monthsMovementBar.jsp"/>
        </td>
      </tr>
      <tr>
        <th colspan="3" class="dayHeader">&nbsp;</td>
          <%
            long todayMorning=new CompanyCalendar().setAndGetTimeToDayStart().getTime();

            while (cc.getTimeInMillis() <scale.endPointTime) {
              %><th t="<%=DateUtilities.dateToInt(cc.getTime())%>" dateLabel="<%=DateUtilities.dateToString(cc.getTime(), "EEEE dd" )%>" class="dayHeader <%=cc.isToday()?"dayTHeader":""%> <%=!cc.isWorkingDay()?"dayHHeader":""%> <%=cc.getTimeInMillis()<todayMorning?"inThePast":""%>"><%=DateUtilities.dateToString(cc.getTime(), "EEE")%><br><%=DateUtilities.dateToString(cc.getTime(), "dd")%></th><%
              cc.add(CompanyCalendar.DAY_OF_MONTH,1);
            }
          %>
      </tr>
      </thead>

      <%----------------------------------------------- CALENDAR DAYS END -------------------------------------------------------%>

      <%-------------------------------------------------------------------------------------------------  HERE STARTS THE CORE ------------------------------------------------------------------------------%>

      <%
        List<Task> tasks = new LinkedList();
        tasks.add(0, parentTask);
        if (showChildren)
          tasks.addAll(parentTask.getDescendants());

        for (Task task:tasks){
            %>


              <tr class="tableSection">
                <td colspan="99">
                  <div class="childNode">
                    <h2><%

                      Date startDate = task.getSchedule().getStartDate();
                      Date endDate = task.getSchedule().getEndDate();
                      Date now=new Date();

                      boolean startInError=false;
                      String startErrorMessage="";
                      if (TaskStatus.STATUS_SUSPENDED.equals(task.getStatus()) && task.getPreviousesSize()>0 && startDate.before(now) ){
                        startInError=true;
                        startErrorMessage+=I18n.get("TASK_SHOULD_BE_ALREADY_OPEN");
                      }

                      boolean endInError=false;
                      String endErrorMessage="";
                      if (TaskStatus.STATUS_ACTIVE.equals(task.getStatus()) &&  endDate.before(now) ){
                        endInError=true;
                        endErrorMessage+=I18n.get("TASK_SHOULD_BE_ALREADY_CLOSED");
                      }

                      PageSeed tasked= pageState.pageFromRoot("task/taskOverview.jsp");
                      tasked.mainObjectId=task.getId();
                      tasked.command= Commands.EDIT;
                      ButtonLink taskbl=new ButtonLink(task.bricks.getDisplayName(),tasked);
                      taskbl.toHtmlInTextOnlyModality(pageContext);


                      ButtonJS startBtn = new ButtonJS(JSP.w(startDate),"goToMillis("+startDate.getTime()+");");
                      startBtn.toolTip=startErrorMessage;
                      startBtn.additionalCssClass="small";
                      startBtn.additionalCssClass+=startInError?" warning":"";

                      ButtonJS endBtn = new ButtonJS(JSP.w(endDate),"goToMillis("+endDate.getTime()+");");
                      endBtn.toolTip=endErrorMessage;
                      endBtn.additionalCssClass="small";
                      endBtn.additionalCssClass+=endInError?" warning":"";

                    %>

                      &nbsp;
                      <span class='pathCode' >
                      <%=I18n.get("START")%>: <%startBtn.toHtmlInTextOnlyModality(pageContext);%>&nbsp;&nbsp;&nbsp;
                      <%=I18n.get("END")%>: <%endBtn.toHtmlInTextOnlyModality(pageContext);%>&nbsp;&nbsp;&nbsp;
                      (<%=I18n.get("DAYS")%>: <b><%=task.getDuration()%></b>)
                      </span>

                      <span style="float:right"><%

                    if ( task.hasPermissionFor(logged,TeamworkPermissions.assignment_canCRW)) {
                      //JOIN THIS PROJECT
                      PageSeed psAddAssignment = pageState.pageFromRoot("task/taskAssignmentNew.jsp");
                      psAddAssignment.addClientEntry("TASK_ID",task.getId());
                      psAddAssignment.command = Commands.ADD;

                      ButtonSupport jtp = ButtonLink.getBlackInstance("",600,800, psAddAssignment);
                      jtp.toolTip=I18n.get("ADD_ASSIGNMENT");
                      jtp.iconChar="r";
                      jtp.additionalCssClass="small";
                      jtp.toHtml(pageContext);

                    }
                    %></span></h2>


                  </div></td>
              </tr>
      <%

              if(task.getSchedule().overlap(new Period(scale.getPeriod().getStartDate().getTime() - CompanyCalendar.MILLIS_IN_3_MONTH, scale.getPeriod().getEndDate().getTime() + CompanyCalendar.MILLIS_IN_MONTH))){


              // ------------------------------------------------ START loop the assigs ------------------------------------------------
              PlanUtilities planUtilities = new PlanUtilities();
              planUtilities.ignoreWorklogWhenComputingLoad=pageState.getEntryOrDefault("OPLOAD_IGNORE_WORKLOG","NO").checkFieldValue();
              //planUtilities.ignorePlanByIssue=true;

              List<Assignment> assigs = task.getActiveAssignements(activeonly);
                Collections.sort(assigs, new TeamworkComparators.AssignmentComparatorByResource());
              int maxRow = assigs.size() - 1;
              int row = 0;


              if (I18n.isActive("CUSTOM_FEATURE_USE_SIMPLE_PLAN")) {
                Set<Resource> ress = new HashSet<Resource>();
                for (Assignment assig : assigs) {
                  ress.add(assig.getResource());
                }
                planUtilities.preloadPlanCache(ress, scale.getPeriod());  // prolad complete plan for resources
              }

              // iterate on resources
              for (Assignment assig : assigs) {

                //skip routinary activities
                if (Assignment.ACTIVITY_REPEATED_IN_TIME.equals(assig.getActivity()))
                  continue;

                Resource res = assig.getResource();

                // get agenda events for this period (month) for this resource
                List<Resource> meOnly = new ArrayList();
                meOnly.add(res);

                boolean assigResourceCanManage=assig.hasPermissionFor(logged,TeamworkPermissions.resource_manage);

                //get the full resource load (plan included...)
                PlanUtilities.ResourceLoad workLoad = planUtilities.getTotalLoadAndWork(res, scale.getPeriod());


            %> <tr resId="<%=res.getId()%>" assId="<%=assig.getId()%>"  class="<%=assig.isEnabled()?"":"assDisabled"%>">
                  <td nowrap id="row_<%=row%>" style="width: 200px">
                    <div>
                      <img src="<%=res.bricks.getAvatarImageUrl()%>" title="<%=res.getDisplayName()%>" class='face' align='absmiddle' style="float: left; margin-top: 4px; margin-right: 4px"><%
                      PageSeed editR = pageState.pageInThisFolder("planByResource.jsp",request);
                      Bricks.addReferral(res.getId(), Resource.class, editR);
                      editR.setCommand("FIND_BY_ENTITY");
                      editR.addClientEntry("FOCUS_MILLIS",focusedMillis);
                      ButtonLink buttonLink = new ButtonLink(res.getDisplayName(), editR);
                      buttonLink.enabled=res.hasPermissionFor(logged, TeamworkPermissions.resource_manage);
                      buttonLink.toHtmlInTextOnlyModality(pageContext);

                      cc.setTimeInMillis(System.currentTimeMillis());
                      long planned = assig.getWorklogPlanned(cc.setAndGetTimeToDayStart());

                  %>
                  <span class="estimOnPlanned block" nowrap>
                    <input type="text" size="4"
                           class="validated" entrytype="DURATIONMILLIS" style="font-size:10px;"
                           value="<%=DateUtilities.getMillisInHoursMinutes(assig.getEstimatedWorklog())%>"
                           title="<%=I18n.get("WORKLOG_ESTIMATED")%>"
                           <%--=task.bricks.canManageAssignment?"":"readOnly"--%>
                           <%=assigResourceCanManage?"":"readOnly"%>
                           onblur="updateEstimation($(this));" oldvalue="1">
                    - <span class="totDone button textual" onclick="openWorklogList($(this));" title="<%=I18n.get("WORKLOG_DONE")%>" totDone="<%=assig.getWorklogDone()%>"><%=DateUtilities.getMillisInHoursMinutes(assig.getWorklogDone())%></span>
                    =
                    <span class="toBeDone <%=planned>(assig.getEstimatedWorklog()-assig.getWorklogDone())?"overPlanned":""%>" title="<%=I18n.get("TO_BE_DONE")%>" toBeDone="<%=assig.getEstimatedWorklog()-assig.getWorklogDone()%>"><%=DateUtilities.getMillisInHoursMinutes(assig.getEstimatedWorklog()-assig.getWorklogDone())%>
                    (<span class="totPlanned" title="<%=I18n.get("PLANNED_WORK")%>" totPlanned="<%=planned%>"><%=DateUtilities.getMillisInHoursMinutes(planned)%></span>)
                    </span>
                    </span>
                    </div>
                  </td>
              <td class="textSmall" style="border-left: none"><%=JSP.w(assig.getRole().getCode())%></td>
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
              int col=0;
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
                    //byPlan = aDayPlan.loadByPlan+aDayPlan.loadByIssue;
                    byPlan = aDayPlan.loadByPlan;
                    hasNotes = aDayPlan.hasNotes;
                  }
                }


                //byPlan==0 se non devi lavorare   byPlan==-1 se non pianificato
                String sByPlan = byPlan <0? "" : DateUtilities.getMillisInHoursMinutes(byPlan);

                PlanUtilities.LoadAndWork loadAndWork = workLoad.totalLoadAndWork.get(day);
                long totEstimated = loadAndWork==null?0:loadAndWork.workLoad;

                // chech the agenda for unavailability
                boolean inVacation=  workLoad.resourceWorkPower.get(day).longValue()<=-2;


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
                  tdClass += "routine ";
                if (cc.getTimeInMillis()<todayMorning)
                  tdClass += "inThePast ";
                if (byPlan==0)
                  tdClass += "zero_panned ";

                if (byPlan>0)
                  tdClass += "hasPlan ";

                if (hasNotes)
                  tdClass += "hasNotes ";



                // DRAW THE INPUT
                %><td class="day tdCell <%=tdClass%>" t="<%=day%>" whpd="<%=workingHourPerDay%>">
                    <input <%=assigResourceCanManage && inTaskScope ?"":"readonly"%> type="text" class="cell" value="<%=JSP.w(sByPlan)%>" oldvalue="y">
                    <span class="noteEditorButton" title="<%=I18n.get("NOTES")%>"></span>
                  </td><%
                cc.add(CompanyCalendar.DAY_OF_MONTH, 1);
                col++;
              }
              // ------------------------------------------------ END loop the month ------------------------------------------------

        row++;
        %></tr><%
        // ------------------------------------------------ END loop the assigs ------------------------------------------------
        }
      } else if (parentTask.equals(task)){
        %><tr><td colspan="99" ><h3 class="hint"><%=I18n.get("TASK_PLAN_OUT_OF_SCOPE")%></h3></td></tr><%
      }
    }
%>
<tfoot>
<tr>
  <td colspan="99">
    <%
    JspHelper wh = new JspHelper("planLegenda.jsp");
    wh.toHtml(pageContext);
    %>
  </td>
</tr>
</tfoot>


    </table>
<%-- ------------------------------------------------ legenda ------------------------------------------------ --%>
<%
  Container explain = Container.getPopupInstance(I18n.get("OPERATOR_LOAD_EXPLAIN") + "&nbsp;<span id='expltitle'></span>");
  explain.width = "600px";
  explain.level=1;
  explain.closeable = true;
  explain.centeredOnScreen = true;
  explain.id="OPERATOR_LOAD_EXPLAIN";
  explain.start(pageContext);
%><div id="olexplain"><%=I18n.get("CLICK_CELL_TO_SEE_WORK_DETAIL")%></div><%
explain.end(pageContext);
%>


<%---------------------------------------------------------------------------------  JAVSCRIPT BEGIN ---------------------------------------------------------------------%>
<jsp:include page="planJS.jsp"/>

<script type="text/javascript">

  $(function(){
    $("#PLAN_MENU").addClass('selected');
    // ----------------------------------------------------  BINDING -------------------------------------------------------
    $(":text.cell").bind("focus", cellFocus).bind("blur",saveCell).bind("keydown", cellKey).parent().dblclick(noteCellDBLClick);
    $(".noteEditorButton").click(noteButtonClick);

  });


  function showExplain (theCell) {
    var theTr=theCell.parents("tr:first");
    var helper = $("#OPERATOR_LOAD_EXPLAIN");

    if (helper.is(":visible")) {
      var request = {resId: theTr.attr("resId"), day: theCell.parent().attr("t"),forPage:"plan"};
      $("#olexplain").load("partExplainWorkload.jsp", request);
      helper.show();
    }

    return false;
  }


  function showHideExplain(){
    var helper = $("#OPERATOR_LOAD_EXPLAIN");
    if (helper.is(":visible"))
      helper.hide();
    else {
      helper.show();
      helper.css({right:20, top:60, left:"auto"});
    }

  }


</script>
<%

  // -------------------------------------------------------------------------------  TIME BAR -------------------------------------------------------------------
  List<Period> peds=new ArrayList();
  peds.add(parentTask.getSchedule());
  pageState.attributes.put("FOCUSED_PERIOD",scale.getPeriod());// used in the bar
  pageState.attributes.put("HIGHLIGHT_PERIODS",peds);// used in the bar
  %><jsp:include page="../../parts/timeBar.jsp"/><%


  form.end(pageContext);


  }
%>
