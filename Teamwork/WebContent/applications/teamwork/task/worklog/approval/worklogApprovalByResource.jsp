<%@ page import="com.twproject.operator.TeamworkOperator,
                 com.twproject.resource.Resource, com.twproject.resource.ResourceBricks,
                 com.twproject.security.TeamworkPermissions, com.twproject.task.Assignment,
                 com.twproject.task.TaskBricks, com.twproject.utilities.TeamworkComparators,
                 com.twproject.waf.TeamworkHBFScreen, com.twproject.worklog.Worklog,
                 com.twproject.worklog.businessLogic.WorklogBricks, com.twproject.worklog.businessLogic.WorklogController,
                 org.jblooming.PlatformRuntimeException, org.jblooming.agenda.CompanyCalendar,
                 org.jblooming.agenda.Scale, org.jblooming.oql.OqlQuery, org.jblooming.persistence.PersistenceHome,
                 org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.SessionState,
                 org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonJS,
                 org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.ColorValueChooser,
                 org.jblooming.waf.html.input.DateField, org.jblooming.waf.html.input.SmartCombo, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.*"%><%


  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new WorklogController(), request);
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


    Resource resource = (Resource) PersistenceHome.findByPrimaryKey(Resource.class,pageState.getEntry("RES_ID").stringValue());
    if (resource==null)
      throw (new PlatformRuntimeException("Invalid Object"));

    pageState.setMainObject(resource);

    Date start = scale.startPointDate;
    Date date = scale.endPointDate;

    // we will select a list of wl
    String hql= "select wl from "+ Worklog.class.getName()+" as wl where (wl.inserted between :stad and :endd) and wl.assig.resource=:res";


    hql=hql+" order by wl.inserted";

    OqlQuery oql=new OqlQuery(hql);
    oql.getQuery().setTimestamp("stad", start);
    oql.getQuery().setTimestamp("endd", date);
    oql.getQuery().setEntity("res",resource);


    List<Worklog> wls= oql.list();

    Map<Assignment,Map<Integer,List<Worklog>>> plan= new Hashtable<Assignment, Map<Integer, List<Worklog>>>();
    for (Worklog wl:wls){
      Assignment assig= wl.getAssig();

      // is already in the map?
      Map<Integer,List<Worklog>> tskWls= plan.get(assig);
      if (tskWls==null){
        tskWls=new Hashtable<Integer, List<Worklog>>();
        plan.put(assig, tskWls);
      }

      // wich day of week?
      cc.setTime(wl.getInserted());
      if (hideHolidays && cc.isWorkingDay() || !hideHolidays) {
        int day = cc.get(CompanyCalendar.DAY_OF_WEEK);
        List<Worklog> wlPerdayPerRes = tskWls.get(day);
        if (wlPerdayPerRes == null) {
          wlPerdayPerRes = new ArrayList<Worklog>();
          tskWls.put(day, wlPerdayPerRes);
        }
        wlPerdayPerRes.add(wl);
      }
    }


    //get all assigs
    List<Assignment> assigs = new LinkedList();
    assigs.addAll(plan.keySet());
    //sort by name
    Collections.sort(assigs, new TeamworkComparators.AssignmentComparatorByTask());


    PageSeed pageSeed = pageState.thisPage(request);

    // add the focused time client entries;
    pageSeed.addClientEntry("FOCUS_MILLIS", minMillisInBar + "");


    // BEGIN RIBBON ----------------------------------------------
%><script>$("#TIMESHEET_MENU").addClass('selected');</script><%


%><%---------------------------------------------- MAIN COLUMN START ---------------------------------------------------------%>


<div class="mainColumn">
<%
  Form form = new Form(pageSeed);
  form.id = "Wl_PLAN";
  form.alertOnChange = true;
  pageState.setForm(form);
  form.start(pageContext);

%>
<h1><%=I18n.get("WORKLOG_APPROVAL")%></h1>
<div class="optionsBar clearfix">
    <div class="resourceChooser filterElement"><%

      PageSeed resourceEdit = pageState.pageFromRoot("resource/resourceEditor.jsp");
      resourceEdit.command=Commands.EDIT;
      resourceEdit.mainObjectId=resource.getId();

      SmartCombo assignee = ResourceBricks.getInspectableResourcesCombo("RES_ID", pageState);
      assignee.label = "";
      assignee.separator = "<br>";
      assignee.fieldSize = 25;
      assignee.addLinkToEntity(resourceEdit,I18n.get("HIS_RESOURCEEDITOR"));
      assignee.onValueSelectedScript="if ($('#RES_ID').val()) {obj('"+form.id+"').submit();}";
      assignee.preserveOldValue=false;
      assignee.label=I18n.get("FOCUSED_RESOURCE");
      assignee.toHtml(pageContext);

      resource.bricks.getPopupMessage(pageState).toHtml(pageContext);

%></div><div class="filterElement centered"><%
      ButtonSubmit oms = new ButtonSubmit(form);

      CheckField shHolyCF = new CheckField("HIDE_HOLIDAYS", "&nbsp;", false);
      shHolyCF.preserveOldValue = false;
      shHolyCF.additionalOnclickScript=oms.generateJs().toString();
      shHolyCF.toHtmlI18n(pageContext);


    // ------------------------------------------ MESSAGE -------------------------------------------
      if (JSP.ex(pageState.getEntry("WL_MOVED"))){
    %><div><%=I18n.get("WORKLOGS_MOVED_FEEDBACK_%%",pageState.getEntry("WL_MOVED").stringValue())%></div><%
      }

    %></div>
</div>
<jsp:include page="../../part/partCalendarHeader.jsp"/>
<%

  boolean allowBulkOperations = logged.hasPermissionFor(TeamworkPermissions.worklog_manage);

  form.end(pageContext);

%><div class="scrollingBox">
<table class="table edged fixHead fixFoot worklogApproval dataTable" cellpadding="4" cellspacing="0" border="0" id="multi" style="margin: 0">
    <thead class="dataTableHead">
  <tr>
    <th class="dayHeader" style="text-align: left; width:30%;" colspan="4"><%if (allowBulkOperations){%><input type="checkbox" onclick="selAll(this)"><%}else{%>&nbsp;<%}%></th>
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

  %></tr>
    </thead>
    <tbody><%

  boolean swap = true;
  // iterate for all resources selected
  for (Assignment ass : assigs) {

    JspHelper rowDrawer =new JspHelper("partApprovalRow.jsp");
    rowDrawer.parameters.put("assig", ass);
    rowDrawer.parameters.put("plan",plan);
    rowDrawer.parameters.put("minMillisInBar",minMillisInBar);
    rowDrawer.parameters.put("hideHoly",hideHolidays);
    rowDrawer.parameters.put("allowBulkOperations", allowBulkOperations);
    rowDrawer.parameters.put("type", "RESOURCE");

    rowDrawer.toHtml(pageContext);
    swap=!swap;
  }

  // totals
%> </tbody>
    <tfoot><tr class="totals">
  <td class="tdTotal" colspan="4"><%=I18n.get("TOTALS")%></td>
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
<%

  // -------------------------------------------------------------------------------  TIME BAR -------------------------------------------------------------------
  pageState.attributes.put("FOCUSED_PERIOD",scale.getPeriod());// used in the bar
%><jsp:include page="approvalLegenda.jsp"/><jsp:include page="../../../parts/timeBar.jsp"/>


<%---------------------------------------------- MAIN COLUMN END ---------------------------------------------------------%>


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

<%------------------------------------------------ BULK BOXES START ---------------------------------------------------------%>
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
      SmartCombo tasks = TaskBricks.getTaskCombo("NEW_TASK", false, perms, null, pageState);
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



<%------------------------------------------------ BULK BOXES END ---------------------------------------------------------%>



<jsp:include page="partApprovalJs.jsp"/><%
  }
%>
