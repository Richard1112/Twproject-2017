<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.resource.ResourceBricks, com.twproject.security.TeamworkPermissions, com.twproject.waf.TeamworkHBFScreen, com.twproject.waf.settings.ReportBricks, com.twproject.worklog.Worklog, com.twproject.worklog.businessLogic.WorklogBricks, com.twproject.worklog.businessLogic.WorklogController, net.sf.json.JSONObject, org.jblooming.designer.DesignerField, org.jblooming.persistence.PersistenceHome, org.jblooming.security.Permission, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.*, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.container.DivOnMouseover, org.jblooming.waf.html.container.SideBar, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.DataTable, org.jblooming.waf.html.input.*, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.HashSet, java.util.Set, java.util.TreeSet, org.jblooming.utilities.CodeValueList, org.jblooming.security.Area, org.jblooming.waf.PluginBricks, java.util.List, com.twproject.resource.Person, com.twproject.resource.Resource, com.twproject.task.*, org.jblooming.security.License" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator loggedOperator = (TeamworkOperator) pageState.getLoggedOperator();

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new WorklogController(), request);
    body.areaHtmlClass="lreq10 lreqPage";
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);

    pageState.perform(request, response);
    pageState.toHtml(pageContext);

  } else {

    TeamworkOperator logged = loggedOperator;

/*
________________________________________________________________________________________________________________________________________________________________________


  prepare entries

________________________________________________________________________________________________________________________________________________________________________

*/
    PageSeed self = pageState.pageFromRoot("task/worklog/worklogList.jsp");
    self.setCommand(Commands.FIND);

    boolean canReadCost = logged.hasPermissionFor(TeamworkPermissions.task_cost_canRead);

    Form f = new Form(self);
    f.id = "WORKLOG";
    f.alertOnChange = false;
    pageState.setForm(f);

    f.start(pageContext);

    Task task = (Task) PersistenceHome.findByPrimaryKey(Task.class, pageState.getEntry("TASK").intValueNoErrorCodeNoExc()+"");
    boolean taskCanMoveWorklog=false;
    if (task != null) {
      taskCanMoveWorklog = task.hasPermissionFor(logged,TeamworkPermissions.worklog_manage);
    }


    CheckField cf = new CheckField("","chall","",false);
    cf.toolTip=I18n.get("SELECT_DESELECT_ALL");
    cf.script=" onclick=\"selUnselAll($(this));\"";

    JspHelper rowDrawer = new JspHelper("/applications/teamwork/task/worklog/rowWorklogList.jsp");
    rowDrawer.parameters.put("canReadCost",canReadCost);
    DataTable dataTable = new DataTable("WKLGSLH", f, rowDrawer, WorklogController.class, pageState);
    dataTable.drawPageFooter=true;
    dataTable.addHeader(cf);
    if (License.assertLevel(20))
      dataTable.addHeaderFitAndCentered(I18n.get("STATUS"));
    dataTable.addHeader(I18n.get("RESOURCE"), "worklog.assig.resource.name");
    dataTable.addHeader(I18n.get("TASK"), "worklog.assig.task.name");
    dataTable.addHeader(I18n.get("CODE"), "worklog.assig.task.code");
    dataTable.addHeader(I18n.get("ROLE"), "worklog.assig.role.code");
    dataTable.addHeader(I18n.get("DATE"), "worklog.inserted");
    dataTable.addHeader(I18n.get("WORKLOG_ACTION"));

    dataTable.addHeader("");
    dataTable.addHeader(I18n.get("WORKLOG_DURATION"), "worklog.duration");
    dataTable.addHeader(I18n.get("HOURLY_COST_FROM_ASSIGNMENT"), "worklog.assig.hourlyCost");
    dataTable.addHeader(I18n.get("COMPUTED_COST"));

    //dataTable.addHeaderFitAndCentered(I18n.get("DELETE_SHORT"));
    dataTable.addHeaderFitAndCentered("");

    String savedFilterName = pageState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty();


%>

<div class="mainColumn">

  <h1 class="filterTitle" defaultTitle="<%=I18n.get("WORKLOG_LIST")%>">
    <%=JSP.ex(savedFilterName)?I18n.get(savedFilterName):I18n.get("WORKLOG_LIST")%>
  </h1>
  <%----------------------------------------------------------------------------  START FILTER ----------------------------------------------------------------------------%>

  <%
    //task
    SmartCombo taskSC = TaskBricks.getTaskCombo("TASK", false, TeamworkPermissions.task_canRead, pageState);
    taskSC.fieldSize = 30;
    taskSC.separator = "";
    taskSC.addAllowed = true;
    taskSC.label="";

    //include children
    CheckField taskChildren = new CheckField("WORKLOG_SHOW_CHILDREN_SHORT", "TASK_WORKLOG_SHOW_CHILDREN", "&nbsp;", false);
    taskChildren.preserveOldValue = false;



    //task type
    SmartCombo taskType = new SmartCombo("TASK_TYPE", TaskType.class);
    taskType.separator = "<br>";
    taskType.fieldSize = 15;
    taskType.required = false;

    SmartCombo resourceCombo = ResourceBricks.getInspectableResourcesCombo("RES_ID", pageState);
    resourceCombo.label = I18n.get("RESOURCES");
    resourceCombo.fieldSize = 30;
    resourceCombo.addAllowed = true;
    resourceCombo.preserveOldValue = false;
    resourceCombo.separator = "<br>";

    SmartCombo customersCombo = ResourceBricks.getInspectableCustomersCombo("CUST_ID", pageState);
    customersCombo.label = I18n.get("CUSTOMERS");
    customersCombo.fieldSize = 30;
    customersCombo.preserveOldValue = false;
    customersCombo.separator = "<br>";

    ColorValueChooser wlStatus = WorklogBricks.getStatusChooser("WORKLOG_STATUS", true, false, pageState);
    wlStatus.setType("WLFLTSTS");
    wlStatus.height = 30;
    wlStatus.separator = "<br>";

    //time
    DateField forDay = new DateField("WORKLOG_AT_DAY");
    forDay.toolTip = I18n.get("WORKLOG_AT_DAY_HELP");
    forDay.setSearchField(true);
    forDay.separator = "<br>";

    //time
    DateField wlCreated = new DateField("WORKLOG_CREATED");
    wlCreated.toolTip = I18n.get("WORKLOG_CREATED_HELP");
    wlCreated.separator = "<br>";
    wlCreated.setSearchField(true);

    // worklog action
    TextField wlAction = new TextField("TEXT", I18n.get("WORKLOG_ACTION"), "WORKLOG_ACTION", "<br>", 25, false);
    wlAction.searchField = true;

    TagBox tags = new TagBox("TASK_TAGS", Task.class, logged.getPerson().getArea());
    tags.label = I18n.get("TAGS");
    tags.separator = "<br>";
    tags.fieldSize = 25;
    tags.preserveOldValue = false;

    TextField issueId = new TextField("ISSUE_ID", "<br>");
    issueId.fieldSize = 5;

    TextField wlDur = new TextField("WORKLOG_DURATION", "<br>");
    wlDur.fieldSize = 5;

    //only from issues
    CodeValueList cvl= new CodeValueList();
    cvl.add("",I18n.get("ALL"));
    cvl.add(Fields.TRUE,I18n.get("ONLY_WORLOG_FROM_ISSUES"));
    Combo onlyFromIssues= new Combo("ONLY_WORLOG_FROM_ISSUES","<br>",null,20,cvl,"");
    onlyFromIssues.toolTip = I18n.get("ONLY_WORLOG_FROM_ISSUES_HELP");

    Set<Area> areas = logged.getAreasForPermission(TeamworkPermissions.worklog_manage);
    areas.addAll(logged.getAreasForPermission(TeamworkPermissions.resource_manage));

    cvl = CodeValueList.getI18nInstanceForIdentifiables(areas, pageState);
    cvl.addChoose(pageState);
    Combo cbbArea = new Combo("AREA", "<br>", null, 15, null, cvl, "");
    cbbArea.preserveOldValue=false;


    LoadSaveFilter lsfb = new LoadSaveFilter("WORKLOGSFILTER", f);
    lsfb.label=I18n.get("WANT_TO_SAVE_FILTER");
    lsfb.drawButtons = false;
    lsfb.drawEditor = true;

    ButtonSupport qbe = ButtonLink.getBlackInstance(JSP.wHelp(I18n.get("HELP")), 700, 800, pageState.pageFromCommonsRoot("help/qbe.jsp"));
    qbe.toolTip = I18n.get("HELP_QBE");


  %>
  <div class="filterBar withButtons clearfix">
    <div class="filterActiveElements"></div>

    <div class="filterInactiveElements">
      <%--<div class="filterElement"><%taskSC.toHtmlI18n(pageContext);%> </div>--%>
      <%--<div class="filterElement"><%taskChildren.toHtmlI18n(pageContext);%></div>--%>

      <div class="filterElement">
        <label for="<%=taskSC.id%>_txt"><%=I18n.get("TASK")%></label> &nbsp;&nbsp;&nbsp;&nbsp;<%taskChildren.toHtmlI18n(pageContext);%><br><%taskSC.toHtmlI18n(pageContext);%>
      </div>


      <div class="filterElement"><%taskType.toHtmlI18n(pageContext);%></div>
      <div class="filterElement"><%resourceCombo.toHtml(pageContext);%></div>
      <div class="filterElement"><%customersCombo.toHtml(pageContext);%></div>
      <%if (License.assertLevel(20)){%>
        <div class="filterElement"><%wlStatus.toHtmlI18n(pageContext);%></div>
      <%}%>
      <div class="filterElement"><%forDay.toHtmlI18n(pageContext);%></div>
      <div class="filterElement"><%wlCreated.toHtmlI18n(pageContext);%></div>
      <div class="filterElement"><%wlAction.toHtmlI18n(pageContext);%></div>
      <div class="filterElement"><%tags.toHtmlI18n(pageContext);%></div>
      <div class="filterElement"><%issueId.toHtmlI18n(pageContext);%></div>
      <div class="filterElement"><%wlDur.toHtmlI18n(pageContext);%></div>
      <div class="filterElement"><%onlyFromIssues.toHtmlI18n(pageContext);%></div>
      <div class="filterElement"><%cbbArea.toHtmlI18n(pageContext);%></div>

      <%
        // ------------------------------------------------------------------------------------------- custom fields -------------------------------------------------------------------------------------------
        if (Worklog.hasCustomField()) {
          for (int i = 1; i < 5; i++) {
            DesignerField dfStr = DesignerField.getCustomFieldInstance("WORKLOG_CUSTOM_FIELD_", i, null, false, false, true, pageState);
            if (dfStr != null) {
              dfStr.usedForSearch = true;
              dfStr.preserveOldValue = false;
              dfStr.separator = "<br>";
      %><div class="filterElement"><%dfStr.toHtml(pageContext);%></div> <%
          }
        }
      }
    %>

    </div>
    <div class="filterButtons">
      <div class="filterButtonsElement filterAdd"><span class="button" id="filterSelectorOpener" title="<%=I18n.get("ADD_FILTER")%>" onclick="bjs_showMenuDiv('filterSelectorBox', 'filterSelectorOpener');"><span class="teamworkIcon">f</span></span></div>
      <div class="filterButtonsElement filterSearch"><%dataTable.getSearchButton().toHtml(pageContext);%></div>

      <div class="filterActions">
        <div class="filterButtonsElement filterSave"><%lsfb.toHtml(pageContext);%></div>
        <div class="filterButtonsElement filterHelp"><%DataTable.getQBEHelpButton(pageState).toHtmlInTextOnlyModality(pageContext);%></div>
      </div>

    </div>

    <script>$("#TIMESHEET_MENU").addClass('selected');</script>
    <script src="<%=request.getContextPath()%>/commons/js/filterEngine.js"></script>

    <%----------------------------------------------------------------------------  END FILTER ----------------------------------------------------------------------------%>
    <div id="bulkOp" style="display:none;">
      <div id="bulkRowSel"></div>

      <div><%

        ButtonJS changeStatusAll = new ButtonJS("openBulkAction('bulkOpStatus');");
        changeStatusAll.label = I18n.get("WORKLOG_CHANGESTATUS_ASSIG");
        changeStatusAll.additionalCssClass = "bulk lreq20 lreqHide";
        changeStatusAll.iconChar = "h";
        if (logged.hasPermissionFor(TeamworkPermissions.worklog_manage))
          changeStatusAll.toHtmlInTextOnlyModality(pageContext);

      %></div>
      <div><%

        ButtonJS changetaskAll = new ButtonJS("openBulkAction('bulkOpTask');");
        changetaskAll.label = I18n.get("WORKLOG_CHANGEASSIG_ALL");
        changetaskAll.additionalCssClass = " bulk";
        changetaskAll.iconChar = "y";
        changetaskAll.toHtmlInTextOnlyModality(pageContext);


      %>
      </div>
    </div>
  </div>
<%
      dataTable.drawPaginator(pageContext);
      //---------------------------------  INIZIO TABELLA ----------------------------------------------
      dataTable.drawTable(pageContext);
      //---------------------------------  FINE TABELLA ----------------------------------------------
  dataTable.drawPaginatorPagesOnly(pageContext);
    %>

</div>

<div class="rightColumn noprint">
  <div class="tools"><%

    ButtonExportXLS excel = new ButtonExportXLS(f, WorklogController.class, Commands.FIND);
    excel.addImportClasses(Task.class.getName(), Assignment.class.getName(), Resource.class.getName(), Worklog.class.getName(), Person.class.getName(), Issue.class.getName());
    excel.outputFileName = "worklogList";
    excel.toolTip = I18n.get("EXPORT_EXCEL");
    excel.label = "";
    excel.additionalCssClass="lreq20 lreqLabel";

    if (I18n.isActive("CUSTOM_EXPORT_EXCEL_WORKLOG")) {
      JSONObject properties = JSONObject.fromObject(I18n.get("CUSTOM_EXPORT_EXCEL_WORKLOG"));
      excel.addFieldsToExport(properties);
    } else {

      excel.addFieldToExport("inserted", I18n.get("WORKLOG_DATE"));
      excel.addFieldToExport("assig.task.code", I18n.get("CODE"));
      excel.addFieldToExport("status", I18n.get("STATUS"));
      excel.addFieldToExport("assig.task.name", I18n.get("ASSIGNMENT"));
      excel.addFieldToExport("assig.resource.name", I18n.get("ASSIGNEE"));
      excel.addFieldToExport("issue.id", I18n.get("ISSUE"));
      excel.addFieldToExport("action", I18n.get("WORKLOG_ACTION"));
      excel.addBSHFieldToExport("obj.getDuration()/3600000D", I18n.get("WORKLOG_DURATION"));
      if (canReadCost)
        excel.addBSHFieldToExport("obj.assig.hourlyCost", I18n.get("HOURLY_COST_FROM_ASSIGNMENT"));

      for (int i = 1; i < 5; i++) {
        if (I18n.isActive("WORKLOG_CUSTOM_FIELD_" + i)) {
          excel.addFieldToExport("customField" + i, DesignerField.getCustomFieldInstance("WORKLOG_CUSTOM_FIELD_", i, null, true, false, false, null).label);
        }
      }
    }
    //excel.addFieldToExport("assignedTo.name", "assigned to");
    excel.iconChar = "I";
    excel.id="btn_excel";

  %><div class="toolsElement"><%excel.toHtml(pageContext); %></div><%

    ButtonJS print = new ButtonJS("");
    print.iconChar="p";
    print.toolTip = I18n.get("PRINT");
    print.label = "";
    print.id="btn_print";
    print.additionalCssClass="toolsElement";
    DivOnMouseover pr = new DivOnMouseover(print);


    ButtonSupport p = ButtonSubmit.getBlackInstance(f, request.getContextPath() + "/applications/teamwork/task/worklog/worklogListPrint.jsp");
    p.label = I18n.get("PRINT");
    p.id="btn_print_2";
    pr.addButton(p);


    //reports
    TreeSet<ReportBricks.Report> reports = ReportBricks.getReports("worklog");
    if (reports.size()>0){
      pr.addSeparator();
      f.url.addClientEntry("reportName","");
      f.url.addClientEntry("outType","");
      for (ReportBricks.Report r:reports){
        ButtonSubmit pri = new ButtonSubmit(pageState.getForm());
        pri.variationsFromForm.command = Commands.FIND;
        pri.variationsFromForm.href = pageState.pageFromRoot("task/worklog/worklogIReportHelper.jsp").href;
        pri.variationsFromForm.addClientEntry("reportName",r.name);
        pri.target = "_blank";
        pri.label = r.getBestName();
        pri.additionalCssClass="lreq30 lreqLabel";
        pri.preserveFormStatus = true;
        pr.addButton(pri);
      }
    }

    //inject custom reports
    pr.addButtons(PluginBricks.createPageQuarkItemsButtons("REPORTS", pageState));

    //inject WIZARDS
    List<ButtonSupport> bss2 = PluginBricks.createPageQuarkItemsButtons("WIZARDS", pageState);
    for (ButtonSupport bs:bss2 ){
      bs.additionalCssClass="special";
      pr.addButton(bs);
    }


  %><div class="toolsElement"><%pr.toHtml(pageContext);%></div></div>
  <div class="rightColumnInner">
    <%
      PageSeed pageSeed = pageState.pageFromRoot("task/worklog/worklogsMissing.jsp");
      pageSeed.addClientEntry("ISMISSING",Fields.TRUE);
      ButtonSupport buttonLink = ButtonLink.getBlackInstance(I18n.get("MISSING_WORKLOGS"), 800 , 1000, pageSeed);
      buttonLink.additionalCssClass="lreq20 lreqLabel lreqActive";
      buttonLink.toHtmlInTextOnlyModality(pageContext);

      pageSeed.addClientEntry("ISMISSING",Fields.FALSE);
      buttonLink = ButtonLink.getBlackInstance(I18n.get("EXCEEDED_WORKLOGS"), 800 , 1000, pageSeed);
      buttonLink.additionalCssClass="lreq20 lreqLabel lreqActive";
      buttonLink.toHtmlInTextOnlyModality(pageContext);
    %>


    <div class="separator"></div>

    <div class="noprint filters">
      <h2><%=I18n.get("MY_SAVED_FILTERS")%></h2>
      <%

        lsfb.drawEditor = false;
        lsfb.drawButtons = true;
        lsfb.toHtml(pageContext);

        String cmd = pageState.getEntry(Fields.FLD_FILTER_NAME ).stringValueNullIfEmpty();

        {
          PageSeed ps = pageState.thisPage(request);
          ps.command = Commands.FIND;
          ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_MY_WEEK_WL");
          ButtonLink bl = new ButtonLink(I18n.get("PF_MY_WEEK_WL"), ps);
          bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
          bl.toHtmlInTextOnlyModality(pageContext);
        }

        {
          PageSeed ps = pageState.thisPage(request);
          ps.command = Commands.FIND;
          ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_MY_LASTWEEK_WL");
          ButtonLink bl = new ButtonLink(I18n.get("PF_MY_LASTWEEK_WL"), ps);
          bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
          bl.toHtmlInTextOnlyModality(pageContext);
        }

        {
          PageSeed ps = pageState.thisPage(request);
          ps.command = Commands.FIND;
          ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_MY_MONTH_WL");
          ButtonLink bl = new ButtonLink(I18n.get("PF_MY_MONTH_WL"), ps);
          bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
          bl.toHtmlInTextOnlyModality(pageContext);
        }


        {
          PageSeed ps = pageState.thisPage(request);
          ps.command = Commands.FIND;
          ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_MY_ODD_WL");
          ButtonLink bl = new ButtonLink(I18n.get("PF_MY_ODD_WL"), ps);
          bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
          bl.toHtmlInTextOnlyModality(pageContext);
        }

      %></div>

  </div>
</div>

<%

  f.end(pageContext);

%>

<div id="bulkOpTask" class="bulkData" style="display:none;">
  <table class="table">
    <tr><td><h1><%=I18n.get("WORKLOG_CHANGEASSIG_ALL")%></h1></td></tr>
    <tr>
      <td><%
        ButtonJS close = null;
        SmartCombo assigs = null;

        if (loggedOperator.hasPermissionFor(TeamworkPermissions.worklog_manage)|| taskCanMoveWorklog) {
          Set<Permission> perms = new HashSet();
          //perms.add(TeamworkPermissions.assignment_manage);
          perms.add(TeamworkPermissions.worklog_manage);
          assigs = TaskBricks.getAssignmentCombo("NEW_ASSIG", null, perms, null, pageState);
        } else {
          //your dealing with your own
          assigs = TaskBricks.getAllMyAssignmentsCombo("NEW_ASSIG", null, pageState);
        }
        assigs.label = I18n.get("MOVE_WORKLOG_TO_ASSIG");
        assigs.fieldSize = 30;
        assigs.separator = "<br>";
        assigs.toHtml(pageContext);
      %></td>
    </tr><tr>
    <td><br><%
      ButtonJS moveInstance = new ButtonJS("performBulkAction('WORKLOG_MOVE', $(this));");
      moveInstance.label = I18n.get("PROCEED");
      moveInstance.additionalCssClass ="first";
      moveInstance.toHtml(pageContext);
    %></td>
  </tr>
  </table>
</div>

<div id="bulkOpStatus" class="bulkData" style="display:none;">
  <table class="table">
    <tr><td><h1><%=I18n.get("WORKLOG_CHANGESTATUS_ASSIG")%></h1></td></tr>
    <tr>
      <td><%

        ColorValueChooser statusCombo = WorklogBricks.getStatusChooser("WL_STATUS",false,false, pageState);
        statusCombo.setType("WL_STATUS_BLK");
        statusCombo.label = I18n.get("WL_STATUS");
        statusCombo.separator = "<br>";

        statusCombo.toHtmlI18n(pageContext);
      %></td>
    </tr><tr>
    <td ><br><%
      ButtonJS statusInstance = new ButtonJS("performBulkAction('WORKLOG_CHANGESTATUS', $(this));");
      statusInstance.label = I18n.get("PROCEED");
      statusInstance.additionalCssClass ="first";
      statusInstance.toHtml(pageContext);
    %></td>
  </tr>
  </table>
</div>

<script type="text/javascript">

  function performBulkAction(command, el) {
    var form = $("#WORKLOG");
    var ids = getCheckedIds();
    if (ids.length > 0) {
      //change command
      form.find("[name=CM]").val(command);

      // inject hidden ids field on form
      var hid = $("<input type='hidden' name='wlIds'>");
      hid.val(ids.join(","));
      form.append(hid);

      if (el) {
        // move div "moveToTaskAll" to form in order to submit ce
        form.append(el.closest(".bulkData").hide());
      }
      form.submit();
    }
  }


  function getCheckedIds() {
    var ret = [];
    $("[wlId] .selector:checked").each(function () {
      ret.push($(this).closest("[wlId]").attr("wlId"));
    });
    return ret;
  }


  function deleteWl(el) {
    showSavingMessage();
    $.getJSON("worklogAjaxController.jsp", {CM: "DL", wlId: el.closest('[wlid]').attr("wlId")}, function (response) {
      jsonResponseHandling(response);
      if (response.ok) {
        el.closest('[wlid]').remove();
      }
      hideSavingMessage();
    });
  }


  //chiamata ogni volta che si rinfresca la tabella
  function dataTableCallback(totalNumberOfElements){
    //console.debug("dataTabeCallback",totalNumberOfElements);
    if (totalNumberOfElements>0)
      $("#btn_print,#btn_print_2,#btn_excel").removeAttr("disabled").show();
    else
      $("#btn_print,#btn_print_2,#btn_excel").attr("disabled",true).hide();
  }


  function rowChangeStatus(el){
    showSavingMessage();
    var theRow = el.closest("tr[wlId]");
    var wlId = theRow.attr("wlId");
    //console.debug("rowChangeStatus",wlId,el.val());
    var req = {"CM":"WLCHSTS",wlId:wlId,stsId:el.val()};

    $.getJSON("worklogAjaxController.jsp", req, function(response) {
      jsonResponseHandling(response);
      if (response.ok) {
        //do nothing

      }
      hideSavingMessage();
    });

  }


</script>

<%
  }
%>
