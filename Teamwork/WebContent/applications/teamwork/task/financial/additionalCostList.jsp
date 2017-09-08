<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.resource.Person, com.twproject.resource.Resource, com.twproject.resource.ResourceBricks, com.twproject.security.TeamworkPermissions, com.twproject.task.*, com.twproject.task.businessLogic.AdditionalCostController, com.twproject.task.businessLogic.ExpenseController, com.twproject.waf.TeamworkHBFScreen, com.twproject.worklog.Worklog, com.twproject.worklog.businessLogic.WorklogBricks, net.sf.json.JSONObject, org.jblooming.designer.DesignerField, org.jblooming.security.Area, org.jblooming.utilities.CodeValueList, org.jblooming.utilities.JSP, org.jblooming.waf.PluginBricks, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.*, org.jblooming.waf.html.container.DivOnMouseover, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.DataTable, org.jblooming.waf.html.input.*, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List, java.util.Set, com.twproject.task.financial.Cost, org.jblooming.utilities.CollectionUtilities" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator loggedOperator = (TeamworkOperator) pageState.getLoggedOperator();

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new AdditionalCostController(), request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);

    pageState.perform(request, response);
    pageState.toHtml(pageContext);

  } else {

    TeamworkOperator logged = loggedOperator;
    logged.testPermission(TeamworkPermissions.task_cost_canRead);

/*
________________________________________________________________________________________________________________________________________________________________________


  prepare entries

________________________________________________________________________________________________________________________________________________________________________

*/
    PageSeed self = pageState.pageFromRoot("task/financial/additionalCostList.jsp");
    self.setCommand(Commands.FIND);

    boolean canReadCost = logged.hasPermissionFor(TeamworkPermissions.task_cost_canRead);

    Form f = new Form(self);
    f.id = "COSTSFORM";
    f.alertOnChange = false;
    pageState.setForm(f);

    f.start(pageContext);

    JspHelper rowDrawer = new JspHelper("/applications/teamwork/task/financial/rowAdditionalCostList.jsp");
    rowDrawer.parameters.put("canReadCost",canReadCost);
    DataTable dataTable = new DataTable("COSTLISTDTBL", f, rowDrawer, AdditionalCostController.class, pageState);
    dataTable.drawPageFooter=true;
    dataTable.addHeader(I18n.get("TASK"), "task.name");
    dataTable.addHeader(I18n.get("CODE"), "task.code");
    dataTable.addHeader(I18n.get("COST_CLASSIFICATION"));
    dataTable.addHeader(I18n.get("DATE"), "cost.creationDate");
    dataTable.addHeader(I18n.get("DESCRIPTION"));
    dataTable.addHeader(I18n.get("COST_ESTIMATED"), "cost.estimatedCost");
    dataTable.addHeader(I18n.get("COST_DONE"), "cost.realCost");
    dataTable.addHeader(I18n.get("ATTACHMENT"), "cost.attachment");
    for (int i = 1; i < 5; i++) {
      if (I18n.isActive("COST_CUSTOM_FIELD_" + i)) {
        dataTable.addHeader(I18n.get(I18n.get("COST_CUSTOM_FIELD_" + i).split(",")[0]));
      }
    }

    dataTable.addHeaderFitAndCentered(I18n.get("DELETE_SHORT"));
    String savedFilterName = pageState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty();
%>

<div class="mainColumn">

  <h1 class="filterTitle" defaultTitle="<%=I18n.get("COST_ADDITIONAL")%>">
    <%=JSP.ex(savedFilterName)?I18n.get(savedFilterName):I18n.get("COST_ADDITIONAL")%>
  </h1>
  <%----------------------------------------------------------------------------  START FILTER ----------------------------------------------------------------------------%>

  <%
    //task
    SmartCombo taskSC = TaskBricks.getTaskCombo("TASK", false, TeamworkPermissions.task_cost_canRead, pageState);
    taskSC.fieldSize = 30;
    taskSC.separator = "";
    taskSC.addAllowed = true;
    taskSC.label="";

    //include children
    CheckField taskChildren = new CheckField("COST_SHOW_CHILDREN_SHORT", "TASK_COST_SHOW_CHILDREN", "&nbsp;", false);
    taskChildren.preserveOldValue = false;


    //task type
    SmartCombo taskType = new SmartCombo("TASK_TYPE", TaskType.class);
    taskType.separator = "<br>";
    taskType.fieldSize = 15;
    taskType.required = false;

    SmartCombo resourceCombo = ResourceBricks.getInspectableResourcesCombo("RES_ID", CollectionUtilities.toList(TeamworkPermissions.task_cost_canRead), pageState);
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

    //cost for the day
    DateField forDay = new DateField("COST_AT_DAY");
    forDay.labelstr = I18n.get("DATE");
    forDay.setSearchField(true);
    forDay.separator = "<br>";

    // cost description
    TextField costDescription = new TextField("TEXT", I18n.get("DESCRIPTION"), "DESCRIPTION", "<br>", 25, false);
    costDescription.searchField = true;

    // real cost
    TextField realCost = TextField.getCurrencyInstance("REAL_COST");
    realCost.separator="<br>";
    realCost.label= I18n.get("COST_DONE");
    realCost.searchField = true;

    // cost estimated
    TextField estimatedCost = TextField.getCurrencyInstance("COST_ESTIMATED");
    estimatedCost.separator="<br>";
    estimatedCost.label= I18n.get("COST_ESTIMATED");
    estimatedCost.searchField = true;

    TagBox tags = new TagBox("TASK_TAGS", Task.class, logged.getPerson().getArea());
    tags.label = I18n.get("TAGS");
    tags.separator = "<br>";
    tags.fieldSize = 25;
    tags.preserveOldValue = false;


    //only with attachment
    CodeValueList cvl= new CodeValueList();
    cvl.add("",I18n.get("ALL"));
    cvl.add(Fields.TRUE,I18n.get("YES"));
    cvl.add(Fields.FALSE,I18n.get("NO"));
    Combo onlyCostWithAttachment=new Combo("ONLY_COST_WITH_ATTACHMENT","<br>",null,20,cvl,"");
    onlyCostWithAttachment.label="ATTACHMENT";


    // è tenuto largo, tanto c'è il controllo sulle assegnazioni
    Set<Area> areas = logged.getAreasForPermission(TeamworkPermissions.expense_manage);
    areas.addAll(logged.getAreasForPermission(TeamworkPermissions.resource_manage));
    areas.addAll(logged.getAreasForPermission(TeamworkPermissions.task_cost_canRead));

    cvl = CodeValueList.getI18nInstanceForIdentifiables(areas, pageState);
    cvl.addChoose(pageState);
    Combo cbbArea = new Combo("AREA", "<br>", null, 15, null, cvl, "");
    cbbArea.preserveOldValue=false;


    LoadSaveFilter lsfb = new LoadSaveFilter("ADDCOSTFILTER", f);
    lsfb.label=I18n.get("WANT_TO_SAVE_FILTER");
    lsfb.drawButtons = false;
    lsfb.drawEditor = true;

    ButtonSupport qbe = ButtonLink.getBlackInstance(JSP.wHelp(I18n.get("HELP")), 700, 800, pageState.pageFromCommonsRoot("help/qbe.jsp"));
    qbe.toolTip = I18n.get("HELP_QBE");


  %>
  <div class="filterBar withButtons clearfix">
    <div class="filterActiveElements"></div>

    <div class="filterInactiveElements">
      <div class="filterElement">
        <label for="<%=taskSC.id%>_txt"><%=I18n.get("TASK")%></label> &nbsp;&nbsp;&nbsp;&nbsp;<%taskChildren.toHtmlI18n(pageContext);%><br><%taskSC.toHtmlI18n(pageContext);%>
      </div>


      <div class="filterElement"><%taskType.toHtmlI18n(pageContext);%></div>
      <div class="filterElement"><%resourceCombo.toHtml(pageContext);%></div>
      <div class="filterElement"><%customersCombo.toHtml(pageContext);%></div>
      <div class="filterElement"><%forDay.toHtml(pageContext);%></div>
      <div class="filterElement"><%costDescription.toHtmlI18n(pageContext);%></div>
      <div class="filterElement"><%tags.toHtmlI18n(pageContext);%></div>
      <div class="filterElement"><%estimatedCost.toHtmlI18n(pageContext);%></div>
      <div class="filterElement"><%realCost.toHtmlI18n(pageContext);%></div>
      <div class="filterElement filterDefault"><%onlyCostWithAttachment.toHtmlI18n(pageContext);%></div>
      <div class="filterElement"><%cbbArea.toHtmlI18n(pageContext);%></div>

      <%
        // ------------------------------------------------------------------------------------------- custom fields -------------------------------------------------------------------------------------------
        for (int i = 1; i < 5; i++) {
          DesignerField dfStr = DesignerField.getCustomFieldInstance("COST_ADD_CUSTOM_FIELD_", i, null, false, false, true, pageState);
          if (dfStr != null) {
            dfStr.usedForSearch = true;
            dfStr.preserveOldValue = false;
            dfStr.separator = "<br>";
            %><div class="filterElement"><%dfStr.toHtml(pageContext);%></div> <%
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

    <script src="<%=request.getContextPath()%>/commons/js/filterEngine.js"></script>

    <%----------------------------------------------------------------------------  END FILTER ----------------------------------------------------------------------------%>

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
    // ----------------------------------------   RIGHT COLUMN ----------------------------------------------------------


    ButtonExportXLS excel = new ButtonExportXLS(f, AdditionalCostController.class, Commands.FIND);
    excel.addImportClasses(Task.class.getName(), Assignment.class.getName(), Resource.class.getName(), Cost.class.getName(), Person.class.getName(), Issue.class.getName());
    excel.additionalCssClass="lreq20 lreqLabel";

    excel.outputFileName = "additionalCostList";
    excel.toolTip = I18n.get("EXPORT_EXCEL");
    excel.label = "";

    if (I18n.isActive("CUSTOM_EXPORT_EXCEL_ADDITIONAL_COST")) {
      JSONObject properties = JSONObject.fromObject(I18n.get("CUSTOM_EXPORT_EXCEL_ADDITIONAL_COST"));
      excel.addFieldsToExport(properties);
    } else {
      excel.addBSHFieldToExport("obj0.code", I18n.get("CODE"));
      excel.addBSHFieldToExport("obj0.name", I18n.get("TASK"));
      if (I18n.isActive("CUSTOM_FEATURE_ALWAYS_SHOW_TASK_PATH"))
        excel.addBSHFieldToExport("obj0.getPath(\" / \", false)", I18n.get("ROOT_PROJECT"));
      excel.addBSHFieldToExport("obj1.classification", I18n.get("COST_CLASSIFICATION"));
      excel.addBSHFieldToExport("obj1.creationDate", I18n.get("WORKLOG_DATE"));
      excel.addBSHFieldToExport("obj1.description", I18n.get("DESCRIPTION"));
      excel.addBSHFieldToExport("obj1.estimatedCost", I18n.get("COST_ESTIMATED"));
      excel.addBSHFieldToExport("obj1.realCost", I18n.get("COST_DONE"));
      for (int i = 1; i < 5; i++) {
        if (I18n.isActive("COST_ADD_CUSTOM_FIELD_" + i)) {
          excel.addBSHFieldToExport("obj1.customField" + i, DesignerField.getCustomFieldInstance("COST_ADD_CUSTOM_FIELD_", i, null, true, false, false, null).label);
        }
      }

    }
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


    ButtonSupport p = ButtonSubmit.getBlackInstance(f, request.getContextPath() + "/applications/teamwork/task/financial/additionalCostListPrint.jsp");
    p.label = I18n.get("PRINT");
    p.id="btn_print_2";
    pr.addButton(p);


    /*//reports
    TreeSet<ReportBricks.Report> reports = ReportBricks.getReports("cost");
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
        pri.preserveFormStatus = true;
        pr.addButton(pri);
      }
    }*/

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
          ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_MONTH_ADD_COSTS");
          ButtonLink bl = new ButtonLink(I18n.get("PF_MONTH_ADD_COSTS"), ps);
          bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
          bl.toHtmlInTextOnlyModality(pageContext);
        }

        {
          PageSeed ps = pageState.thisPage(request);
          ps.command = Commands.FIND;
          ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_WEEK_ADD_COSTS");
          ButtonLink bl = new ButtonLink(I18n.get("PF_WEEK_ADD_COSTS"), ps);
          bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
          bl.toHtmlInTextOnlyModality(pageContext);
        }



      %></div>

  </div>
</div>
<%
  f.end(pageContext);
%>

<script type="text/javascript">
  $("#TASK_MENU").addClass('selected');

  function deleteCost(el) {
    var theRow = el.closest("tr[costId]");
    var costId = theRow.attr("costId");
    var taskId = theRow.attr("taskId");
    var req = {"CM":"DLADDCOST",costId:costId,taskId:taskId};

    showSavingMessage();
    $.getJSON("costAjaxController.jsp", req, function (response) {
      jsonResponseHandling(response);
      if (response.ok) {
        el.closest('[costId]').remove();
        $("#costTotalRow").hide();<%-- si nasconde perchè non si sanno ricalcolare --%>
        recomputeCostTotal();
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


  function recomputeCostTotal(){
    //console.debug("recomputeCostTotal");
    var tot=0;
    $("td.estCost").each(function(){
      tot+=getCurrencyValue($(this).text());
    });
    $("#pageEstTotal").html(formatCurrency(tot));
    var tot=0;
    $("td.realCost").each(function(){
      tot+=getCurrencyValue($(this).text());
    });
    $("#pageTotal").html(formatCurrency(tot));
  }


</script>

<%
  }
%>
