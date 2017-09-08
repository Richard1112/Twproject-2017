<%@ page import="com.twproject.operator.TeamworkOperator,
                 com.twproject.security.TeamworkPermissions, com.twproject.task.Assignment,
                 com.twproject.task.Task, com.twproject.task.TaskBricks,
                 com.twproject.waf.TeamworkHBFScreen, com.twproject.worklog.Worklog,
                 com.twproject.worklog.businessLogic.WorklogBricks, org.jblooming.PlatformRuntimeException,
                 org.jblooming.agenda.CompanyCalendar, org.jblooming.agenda.Period,
                 org.jblooming.agenda.Scale, org.jblooming.oql.OqlQuery,
                 org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.SessionState, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.PathToObject, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.*, org.jblooming.waf.html.input.*, com.twproject.worklog.businessLogic.WorklogController"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new WorklogController(),request);
    body.areaHtmlClass="lreq20 lreqPage";
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);
  } else {
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    // get the focused millis
    long focusedMillis= pageState.getEntry("FOCUS_MILLIS").longValueNoErrorNoCatchedExc();
    focusedMillis= focusedMillis==0?System.currentTimeMillis():focusedMillis;
    pageState.addClientEntry("FOCUS_MILLIS",focusedMillis);

    // set the current scale factor to 1WEEK
    Scale.ScaleType scaleType = Scale.ScaleType.SCALE_1WEEK;

    // define the scale and increment of ticks
    Scale scale = Scale.getScaleAndSynch(scaleType, focusedMillis, true, SessionState.getLocale());

    // compute the startMillis and endMillis using focusedMillis and scaleType
    final long minMillisInBar = scale.startPointTime;

    CompanyCalendar cc = new CompanyCalendar();


    pageState.getEntryOrDefault("HIDE_HOLIDAYS");
    boolean hideHolidays=pageState.getEntry("HIDE_HOLIDAYS").checkFieldValue();


    Task task = Task.load(pageState.getEntry("TASK_ID").intValueNoErrorCodeNoExc()+"");
    if (task==null)
      throw (new PlatformRuntimeException("Invalid Object"));

    pageState.setMainObject(task);

    // we will select a list of wl
    String hql= "select wl from "+ Worklog.class.getName()+" as wl where (wl.inserted between :stad and :endd)";
    hql=hql+" and wl.assig.task=:tsk";
    hql=hql+" order by wl.inserted";

    OqlQuery oql=new OqlQuery(hql);
    oql.getQuery().setTimestamp("stad",scale.startPointDate);
    oql.getQuery().setTimestamp("endd",scale.endPointDate);
    oql.getQuery().setEntity("tsk",task);



    List<Worklog> wls= oql.list();

    Map<Assignment,Map<Integer,List<Worklog>>> plan= new Hashtable<Assignment, Map<Integer, List<Worklog>>>();
    for (Worklog wl:wls){
      Assignment assignment= wl.getAssig();

      // is already in the map?
      Map<Integer,List<Worklog>> resWls= plan.get(assignment);
      if (resWls==null){
        resWls=new Hashtable<Integer, List<Worklog>>();
        plan.put(assignment, resWls);
      }

      // wich day of week?
      cc.setTime(wl.getInserted());
      if (hideHolidays && cc.isWorkingDay() || !hideHolidays) {
        int day = cc.get(CompanyCalendar.DAY_OF_WEEK);
        List<Worklog> wlPerdayPerRes = resWls.get(day);
        if (wlPerdayPerRes == null) {
          wlPerdayPerRes = new ArrayList<Worklog>();
          resWls.put(day, wlPerdayPerRes);
        }
        wlPerdayPerRes.add(wl);
      }
    }


    //get all assigs
    List<Assignment> assigs = new LinkedList();
    assigs.addAll(plan.keySet());
    //sort by name
    Collections.sort(assigs, new Comparator<Assignment>() {
      public int compare(Assignment t1, Assignment t2) {
        return t1.getResource().getName().compareToIgnoreCase(t2.getResource().getName());
      }
    });


    PageSeed pageSeed = pageState.thisPage(request);

    // add the focused time client entries;
    pageSeed.addClientEntry("FOCUS_MILLIS", minMillisInBar + "");
    pageSeed.addClientEntry("TASK_ID",task);

    // BEGIN RIBBON ----------------------------------------------
%><script>$("#TIMESHEET_MENU").addClass('selected');</script><%

%><%---------------------------------------------- MAIN COLUMN START ---------------------------------------------------------%>


<div class="mainColumn"><%
  Form form = new Form(pageSeed);
  form.id = "Wl_PLAN";
  form.alertOnChange = true;
  pageState.setForm(form);
  form.start(pageContext);


%><h1><%=I18n.get("WORKLOG_APPROVAL")%></h1>
  <div class="optionsBar clearfix">

    <div class="filterElement"><%

  // -------------------------------------------------------------------------------  START PATH TO OBJECT -------------------------------------------------------------------
  PathToObject pto = new PathToObject(task);
  pto.canClick = TeamworkPermissions.task_canRead;
  pto.alternativeCEForMainObject="TASK_ID";

      PageSeed tp = pageState.thisPage(request);
      tp.command="dummy";
      pto.destination = tp;
  pto.toHtml(pageContext);


  // -------------------------------------------------------------------------------  END PATH TO OBJECT -------------------------------------------------------------------
%></div><%

// ------------------------------------------ MESSAGE -------------------------------------------
  if (JSP.ex(pageState.getEntry("WL_MOVED"))){
%><div class="filterElement"><%=I18n.get("WORKLOGS_MOVED_FEEDBACK_%%",pageState.getEntry("WL_MOVED").stringValue())%></div><%
  }

%><div class="filterElement"><%
  ButtonSubmit oms = new ButtonSubmit(form);
  CheckField shHolyCF = new CheckField("HIDE_HOLIDAYS", "&nbsp;", false);
  shHolyCF.preserveOldValue = false;
  shHolyCF.additionalOnclickScript=oms.generateJs().toString();
  shHolyCF.toHtmlI18n(pageContext);

%></div>
</div>

  <jsp:include page="../../part/partCalendarHeader.jsp"/><%
  
  boolean allowBulkOperations = task.hasPermissionFor(logged,TeamworkPermissions.worklog_manage);


  form.end(pageContext);


  %>
<div class="scrollingBox">
  <table class="table edged fixHead fixFoot dataTable" cellpadding="4" cellspacing="0" border="0" id="multi" style="margin: 0">
    <thead class="dataTableHead">
  <tr>
    <th class="dayHeader" colspan="3" style="text-align: left;width:30%"><%if (allowBulkOperations){%><input type="checkbox" onclick="selAll(this)"><%}else{%>&nbsp;<%}%></th>
    <%
      cc.setTimeInMillis(minMillisInBar);

      for (int i = 1; i <= 7; i++) {
    %><th class="dayHeader <%=cc.isToday() ?"dayTHeader" : ""%>  <%=!cc.isWorkingDay() ?"dayHHeader" : ""%>" day="<%=cc.get(CompanyCalendar.DAY_OF_WEEK)%>" style="width: <%=!cc.isWorkingDay() ?"5" : "12"%>%"><%
    if ((hideHolidays && cc.isWorkingDay() || !hideHolidays) && allowBulkOperations) {
  %><%=DateUtilities.dateToString(cc.getTime(), "EEE dd MMM")%><input type="checkbox" onclick="colSel(this)"><%
  } else {
  %><%=DateUtilities.dateToString(cc.getTime(), "EEE")%><%
    }
  %></th><%
      cc.add(CompanyCalendar.DAY_OF_MONTH, 1);
    }

  %></tr></thead> <tbody><%

  boolean swap = true;
  // iterate for all resources selected
  for (Assignment assignment: assigs) {

    JspHelper rowDrawer =new JspHelper("partApprovalRow.jsp");
    rowDrawer.parameters.put("assig",assignment);
    rowDrawer.parameters.put("plan",plan);
    rowDrawer.parameters.put("minMillisInBar",minMillisInBar);
    rowDrawer.parameters.put("hideHoly",hideHolidays);
    rowDrawer.parameters.put("allowBulkOperations", allowBulkOperations);
    rowDrawer.parameters.put("type", "TASK");


    rowDrawer.toHtml(pageContext);
    swap=!swap;
  }

  // totals
%></tbody><tfoot><tr class="totals">
  <td colspan="3"><%=I18n.get("TOTALS")%></td>
  <%
    cc.setTimeInMillis(minMillisInBar);
    for (int i = 1; i <= 7; i++) {
  %><td align="center" day="<%=cc.get(CompanyCalendar.DAY_OF_WEEK)%>" class="tdTotal">0</td><%
    cc.add(CompanyCalendar.DAY_OF_MONTH, 1);
  }
%>
</tr>
  <tr><td id="bulkPlace" colspan="99"></td></tr>
  </tfoot>
</table>
</div>
</div>

<%------------------------------------------------ MAIN COLUMN END ---------------------------------------------------------%>

<%

  // -------------------------------------------------------------------------------  TIME BAR -------------------------------------------------------------------
  List<Period> peds=new ArrayList();
  peds.add(task.getSchedule());
  pageState.attributes.put("FOCUSED_PERIOD",scale.getPeriod());// used in the bar
  pageState.attributes.put("HIGHLIGHT_PERIODS",peds);// used in the bar
%><jsp:include page="approvalLegenda.jsp"/><jsp:include page="../../../parts/timeBar.jsp"/>


<%----------------------------------------------- START BULK OPERATIONS ---------------------------------------------------------%>

<div id="bulkOp" style="display:none;">
  <div id="bulkRowSel"></div>
  <div><%

    ButtonJS changeStatusAll = new ButtonJS("openBulkAction('changeStatusAll');");
    changeStatusAll.label = I18n.get("WORKLOG_CHANGESTATUS_ALL");
    changeStatusAll.iconChar = "h";
    changeStatusAll.toHtmlInTextOnlyModality(pageContext);

  %></div><div><%

  ButtonJS changeTaskAll = new ButtonJS("openBulkAction('moveToAssigAll');");
  changeTaskAll.label = I18n.get("WORKLOG_CHANGEASSIG_ALL");
  changeTaskAll.iconChar = "y";
  changeTaskAll.toHtmlInTextOnlyModality(pageContext);

  %></div><div><%

  ButtonJS changeDateAll = new ButtonJS("openBulkAction('moveToDateAll');");
  changeDateAll.label = I18n.get("WORKLOG_CHANGEDATE_ALL");
  changeDateAll.iconChar = "y";
  changeDateAll.toHtmlInTextOnlyModality(pageContext);

%></div>
</div>

<%----------------------------------------------- END BULK OPERATIONS ---------------------------------------------------------%>


  <div id="changeStatusAll" style="display:none;"  class="bulkData">
    <h1><%=I18n.get("WORKLOG_CHANGESTATUS_ALL")%></h1>
      <%
        ColorValueChooser statusCombo = WorklogBricks.getStatusChooser("WL_STATUS", false, false, pageState);
        statusCombo.showOpener=true;
        statusCombo.separator="<br>";
        statusCombo.toHtml(pageContext);
      %><br><br><%
        ButtonJS doMove = new ButtonJS("performBulkAction('BULK_SET_STATUS', $(this));");
        doMove.label=I18n.get("PROCEED");
        doMove.additionalCssClass="first";
        doMove.toHtml(pageContext);

      %>
  </div>

  <div id="moveToAssigAll" style="display:none;"  class="bulkData">
    <h1><%=I18n.get("WORKLOG_CHANGEASSIG_ALL")%></h1>
    <%
        Set perms= new HashSet();
        //perms.add(TeamworkPermissions.assignment_manage);
        perms.add(TeamworkPermissions.worklog_manage);
        SmartCombo tasks = TaskBricks.getTaskCombo("NEW_TASK",false, perms,null,pageState);
        tasks.label=I18n.get("WORKLOG_MOVE_TO_ASSIG_TASK");
        tasks.separator="<br>";
        tasks.required=true;
        tasks.toHtml(pageContext);
      %><br><br><%
        doMove = new ButtonJS("performBulkAction('BULK_MOVE_TO_TASK', $(this));");
        doMove.label=I18n.get("PROCEED");
        doMove.additionalCssClass="first";
        doMove.toHtml(pageContext);
      %>
  </div>

  <div id="moveToDateAll" style="display:none;"  class="bulkData">
    <h1><%=I18n.get("WORKLOG_CHANGEDATE_ALL")%></h1>
    <%
      DateField df= new DateField("NEW_DATE");
      df.labelstr="WORKLOG_MOVE_TO_DATE";
      df.separator="<br>";
      df.required=true;

      df.toHtmlI18n(pageContext);

      %><br><br><%
        doMove = new ButtonJS("performBulkAction('BULK_MOVE_TO_DATE', $(this));");
        doMove.label=I18n.get("PROCEED");
        doMove.additionalCssClass="first";
        doMove.toHtml(pageContext);

      %>
  </div>

<jsp:include page="partApprovalJs.jsp"/><%
  }

 %>
