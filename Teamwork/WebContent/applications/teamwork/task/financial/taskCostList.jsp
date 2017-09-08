<%@ page import="com.twproject.security.TeamworkPermissions, com.twproject.task.Assignment, com.twproject.task.Task, com.twproject.task.financial.Cost,
com.twproject.task.financial.FinancialBricks, com.twproject.utilities.TeamworkComparators, com.twproject.waf.TeamworkHBFScreen,
com.twproject.waf.html.TaskHeaderBar, com.twproject.worklog.businessLogic.WorklogBricks, net.sf.json.JSONArray,
net.sf.json.JSONObject, org.jblooming.agenda.CompanyCalendar, org.jblooming.designer.DesignerField,
org.jblooming.ontology.PlatformComparators, org.jblooming.oql.OqlQuery, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP,
org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields,
org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.ColorValueChooser, org.jblooming.waf.html.input.SmartCombo, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.ArrayList, java.util.Collections, java.util.List, org.jblooming.operator.Operator"%><%
    PageState pageState = PageState.getCurrentPageState(request);

    if (!pageState.screenRunning) {

        pageState.screenRunning = true;
        final ScreenArea body = new ScreenArea(request);
        TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
        lw.register(pageState);
        pageState.perform(request, response);
        pageState.toHtml(pageContext);
    } else {
        //this is set by action
        Task task = Task.load(pageState.mainObjectId);
        pageState.setMainObject(task);

        PageSeed ps = pageState.thisPage(request);
        ps.command="";
        ps.mainObjectId=task.getId();
        Form form = new Form(ps);
        form.alertOnChange=true;
        form.start(pageContext);
        pageState.setForm(form);

%>
<div class="mainColumn">
<%

    //-----------------HEAD BAR START
    pageState.addClientEntry("TASK_TABSET","TASK_COST_TAB");
    TaskHeaderBar head = new TaskHeaderBar(task);
    head.toHtml(pageContext);
    //-----------------HEAD BAR END

  Operator logged = pageState.getLoggedOperator();
  if (! task.hasPermissionFor(logged,TeamworkPermissions.task_cost_canRead)) {
%><div class="hint warning"><%=I18n.get("TASK_COST_READ_LACKING")%></div><%
} else {

  %><div class="box" style="float:left;"><%
  //make
  boolean task_cost_canWrite = task.hasPermissionFor(logged,TeamworkPermissions.task_cost_canWrite);

  pageState.addClientEntryCurrency("COST_ESTIMATED",task.getForecasted());

  TextField estimated = TextField.getCurrencyInstance("COST_ESTIMATED");
  estimated.separator="&nbsp;";
  estimated.label = "<span style='font-size:20px'>"+I18n.get("BUDGET")+"</span>";
  estimated.fieldSize = 10;
  estimated.script = " style=\"text-align:right;\" onblur=\"recomputeGranTotale();\"  ";
  estimated.readOnly = !task_cost_canWrite;
  estimated.toHtmlI18n(pageContext);

  if (task.getChildrenSize()>0) {
    String hql = "select sum(task.forecasted) from " + Task.class.getName() + " as task where task.parent = :myself";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setEntity("myself", task);
    Double f = (Double) oql.uniqueResultNullIfEmpty();
    if (f==null)
      f = 0.0;
    %>&nbsp;&nbsp;<small>(<%=I18n.get("BUDGET_ON_CHILDREN")%>: <%=JSP.currency(f)%>)</small><%
  }


%></div><div class="listPagedisplayOptions" style="padding-top: 40px; "><%
  ButtonSubmit sc = new ButtonSubmit(pageState.getForm());
  boolean includeDescendantCosts = pageState.getEntryOrDefault("INCLUDE_DESCS_COSTS", Fields.TRUE).checkFieldValue();
  CheckField includeDescsCosts = new CheckField("INCLUDE_DESCS_COSTS","",false);
  includeDescsCosts.additionalOnclickScript = sc.generateJs().toString();
  includeDescsCosts.preserveOldValue = false;
  includeDescsCosts.toHtmlI18n(pageContext);

  %>&nbsp;&nbsp;&nbsp;&nbsp;<span class="lreq20 lreqHide"><%


  ColorValueChooser chooser = WorklogBricks.getStatusChooser("WORKLOG_STATUS", true, pageState);
  chooser.label=I18n.get("WORKLOG_STATUS");
  chooser.separator="&nbsp;";
  chooser.width = 120;
  chooser.height = 26;
  chooser.showOpener = true;
  chooser.onChangeScript = sc.generateJs().toString();
  chooser.preserveOldValue=false;
  chooser.toHtml(pageContext);

%></span></div><%

  //try to know if there is custom fields
  if (DesignerField.hasCustomField("BUDGET_CUSTOM_FIELD_", 4)){

    //---------------------------------------------------- CUSTOM FIELDS ------------------------------------------------------------------------
%><div class="clearfix" id="budgCustomFields"><%
  for (int i=1; i<5; i++) {
    DesignerField dfStr = DesignerField.getCustomFieldInstance( "BUDGET_CUSTOM_FIELD_","budgetCustomField",i, task,!task_cost_canWrite, false, false, pageState);
    if (dfStr!=null){
      dfStr.separator="<br>";
%><div style="float:left;margin-right: 10px"><%dfStr.toHtml(pageContext);%></div><%
    }
  }

%></div><%

  }
%>

<br><br>
<table class="table fixHead fixFoot costs" id="assigCostsTbl"  cellpadding="0" cellspacing="2">

    <thead>
    <tr><th rowspan="2" class="tableHead" width="30%"><span class="tableHeadEl"><%=I18n.get("ASSIGNEE")%></span></th>
        <th colspan="2"  class="tableHead firstRow"><span class="tableHeadEl"><%=I18n.get("WORKLOG")%></span></th>
        <th colspan="3" class="tableHead firstRow"><span class="tableHeadEl"><%=I18n.get("WORKLOG_MONEY")%></span></th>
        <th colspan="2" class="tableHead firstRow lreq20 lreqHide"><span class="tableHeadEl"><%=I18n.get("ASSIG_EXPENSES")%></span></th>
        <th rowspan="2" valign="middle" style="text-align: right" class="tableHead"><span class="tableHeadEl"><%=I18n.get("WORKLOG_DELTA")%></span></th>
    </tr>
    <tr>
        <th nowrap style="text-align: right" class="tableHead secondRow"><span class="tableHeadEl"><%=I18n.get("ESTIMATED")%></span></th>
        <th nowrap style="text-align: right" class="tableHead secondRow"><span class="tableHeadEl"><%=I18n.get("DONE")%></span></th>
        <th nowrap style="text-align: right" class="tableHead secondRow"><span class="tableHeadEl"><%=I18n.get("HOURLY_COST")%></span></th>
        <th nowrap style="text-align: right" class="tableHead secondRow"><span class="tableHeadEl"><%=I18n.get("HOURLY_COST_TOTAL_EST")%></span></th>
        <th nowrap style="text-align: right" class="tableHead secondRow"><span class="tableHeadEl"><%=I18n.get("HOURLY_COST_TOTAL_DONE")%></span></th>
        <th nowrap style="text-align: right" class="tableHead secondRow lreq20 lreqHide"><span class="tableHeadEl"><%=I18n.get("PERSONAL_BUDGET")%></span></th>
        <th nowrap style="text-align: right" class="tableHead secondRow lreq20 lreqHide"><span class="tableHeadEl"><%=I18n.get("COST_DONE")%></span></th>
    </tr>
    </thead>

    <%

        List<Task> allTasks = new ArrayList();
        allTasks.add(task);
        if (includeDescendantCosts)
          allTasks.addAll(task.getDescendants(new TeamworkComparators.TaskManualOrderComparator()));

        JSONObject values= new JSONObject();
        //JSONObject additionalCosts= new JSONObject();
        //JSONArray jAllTasks= new JSONArray();

        // filter for WorklogStatus
        String wlsid=pageState.getEntry("WORKLOG_STATUS").stringValue();
        boolean firstRow=true;

        int numOfAssig=0;

      PageSeed costList = pageState.pageFromRoot("task/financial/taskCostList.jsp");
      costList.command=Commands.EDIT;

      for (Task t : allTasks) {
        costList.mainObjectId=t.getId();
        ButtonLink edit = ButtonLink.getTextualInstance(t.getDisplayName(), costList);


        String color=t.getTaskColor();
        String styleColor="";
        if (I18n.isActive("CUSTOM_FEATURE_USE_PROJECT_COLOR") && JSP.ex(color)){
          styleColor=" style=\"border-left: 5px solid "+color+";\"";
        }
        if (!firstRow){
          %><tr class="tableSection <%=t.getAssignments().size()<=0?"isEmpty":""%>"><td colspan="9" <%=styleColor%>>
          <div class="childNode"><h2><span class="pathSmall inline"><%=t.getPath(" / ", false)%> <%=t.getParent()==null?"":"/"%></span><%edit.toHtml(pageContext);%></h2></div>
          </td></tr><%
        }
        firstRow=false;
  List<Assignment> assignementsSortedByRole = t.getAssignementsSortedByRole();
  if (JSP.ex(assignementsSortedByRole)){
  for (Assignment assignment : assignementsSortedByRole) {
        double ew = assignment.getEstimatedWorklog();
        double wd= assignment.getWorklogDone(wlsid);

%><tr class="data" assId="<%=assignment.getId()%>"><%

    PageSeed editAssig = pageState.pageFromRoot("/task/taskAssignmentEditor.jsp");
    editAssig.setCommand(Commands.EDIT);
    editAssig.setMainObjectId(assignment.getId());
    editAssig.addClientEntry("TASK_ID",t.getId());
    ButtonLink editAssigB = ButtonLink.getTextualInstance("", editAssig);
    editAssigB.label = assignment.getResource().getDisplayName() + " - '"+assignment.getRole().getCode()+"'";  // default
    editAssigB.enabled =  task.hasPermissionFor(logged,TeamworkPermissions.assignment_canCRW);


    double assReal=0;
    double assBudget=assignment.getBudget();
    for (Cost c:assignment.getCosts()){
        assReal+=c.getRealCost();
    }


%><td nowrap="">
    <%editAssigB.toHtmlInTextOnlyModality(pageContext);%>
</td>
    <td align="right"><%
        pageState.addClientEntryTime("WL_ESTIM_"+assignment.getId(), assignment.getEstimatedWorklog());
        TextField ewl = TextField.getTimeInstance("WL_ESTIM_"+assignment.getId());
        ewl.separator="";
        ewl.fieldSize = 5;
        ewl.fieldClass="wlEstimInput formElements formElementsSmall";
        ewl.label = "";
        ewl.script=" onchange='wlEstimChanged($(this));' style=\"text-align:right;width:100%\" ";
        ewl.readOnly = ! task.hasPermissionFor(logged,TeamworkPermissions.assignment_canCRW);
        ewl.toHtml(pageContext);
    %></td>
    <td align="right"><span class="wlReal"><%
        PageSeed was = pageState.pageFromRoot("/task/worklog/worklogAssignmentList.jsp");
        was.addClientEntry("ASS_ID", assignment.getId());
        was.command=Commands.FIND;

        ButtonSupport wasL = ButtonLink.getBlackInstance("", 768, 1180, was);
        wasL.label=DateUtilities.getMillisInHoursMinutes(((long)wd));
        wasL.enabled=wd>0;
        wasL.toHtmlInTextOnlyModality(pageContext);

    %></span></td>

    <td align="right"><%

        double hourlyCost = assignment.getHourlyCost();
        pageState.addClientEntry("HOURLY_COST_"+assignment.getId(),hourlyCost);

        TextField hc = TextField.getCurrencyInstance("HOURLY_COST_"+assignment.getId());
        hc.separator="";
        hc.fieldSize = 5;
        hc.fieldClass="hourlyCost formElements formElementsSmall";
        hc.label = "";
        hc.script=" onchange='hourlyCostChanged($(this));' style=\"text-align:right;width:100%\" ";
        hc.readOnly = !task_cost_canWrite;
        hc.toHtml(pageContext);
    %></td><%
        double wlEstimCost = (((double)ew)/CompanyCalendar.MILLIS_IN_HOUR) * hourlyCost;

        double wlReal = ((double)wd/CompanyCalendar.MILLIS_IN_HOUR) * hourlyCost;
        double deltaCost = wlEstimCost+assBudget-(wlReal+assReal);

    %><td align="right" class="wlEstimTot "><%=JSP.currency(wlEstimCost)%></td>
    <td align="right" class="wlRealTot "><%=JSP.currency(wlReal)%></td>

    <td align="right" class="assBudget lreq20 lreqHide">
        <%
            double budget = assignment.getBudget();
            pageState.addClientEntry("BUDGET_"+assignment.getId(),budget);

            TextField hcb = TextField.getCurrencyInstance("BUDGET_"+assignment.getId());
            hcb.separator="";
            hcb.fieldSize = 5;
            hcb.fieldClass="assBudget formElements formElementsSmall";
            hcb.label = "";
            hcb.script=" onchange='budgetChanged($(this));' style=\"text-align:right;width:100%\" ";
            hcb.readOnly = !task_cost_canWrite;
            hcb.toHtml(pageContext);
        %></td>
    <td align="right" class="lreq20 lreqHide" style="cursor:pointer;" onclick="editAssigCosts('<%=assignment.getId()%>');"><span class="button textual assReal"><%=JSP.currency(assReal)%></span></td>

    <td align="right" class="assDelta" style="color:<%=deltaCost>=0?"black":"#D30202"%>;"><%=JSP.currency(deltaCost)%></td>
</tr><%

        JSONObject json= new JSONObject();
        json.element("wlEstim",ew);
        json.element("wlReal",wd);
        json.element("hourlyCost",hourlyCost);
        json.element("assBudget",assBudget);
        json.element("assReal",assReal);

        values.element(assignment.getId()+"",json);
    }
    numOfAssig++;
  }

    JSONObject taskAddCosts= new JSONObject();
    taskAddCosts.element("taskId", t.getId());
    taskAddCosts.element("taskName",t.getName());
    taskAddCosts.element("taskCode",t.getCode());
    taskAddCosts.element("taskDepth",t.getDepth());

    JSONArray addCosts= new JSONArray();
    List<Cost> costs = new ArrayList(t.getCosts());
    Collections.sort(costs, new PlatformComparators.CreationDateComparator());
    for (Cost cost: costs){
        JSONObject jsonCost = cost.jsonify();
        jsonCost.element("taskId",t.getId());
        addCosts.add(jsonCost);
    }
    taskAddCosts.element("costs", addCosts);
    //additionalCosts.element(t.getId()+"",taskAddCosts);

%><tr class="tableSectionSeparator"><td colspan="9"></td></tr><%
    }

  if (numOfAssig>0){
%>
    <tfoot>

    <tr id="gtWL" class="gtWL data totals">
        <td><%=I18n.get("TOTAL")%></td>
        <td align="right" class="wlEstim"></td>
        <td align="right" class="wlReal"></td>
        <td align="right"><span class="avg"></span><%=I18n.get("AVERAGE_SHORT")%></td>
        <td align="right" class="wlEstimTot" id="gtEstimWl"></td>
        <td align="right" class="wlRealTot"  id="gtRealWl"></td>

        <td align="right" class="assBudget lreq20 lreqHide" id="gtassBudget"></td>
        <td align="right" class="assReal lreq20 lreqHide"  id="gtAssReal"></td>

        <td align="right" class="assDelta"></td>
    </tr>
    </tfoot>
  <%
    } else {
  %><tr><td colspan="99"><h2 class="hint" style="text-align: center"><%=I18n.get("NO_ASSIGNMENTS_FOR_TASK")%></h2></td></tr><%
  }
  %>
    <tr>
      <td colspan="99" class="buttonArea"><div class="bbButtons"><%
        ButtonJS save = new ButtonJS(I18n.get("SAVE"),"saveCosts();");
        save.enabled = task_cost_canWrite;
        save.additionalCssClass="big first";
        save.toHtml(pageContext);
      %></div></td>
    </tr>
</table>

<br>
<div class="lreq20 lreqHide">
<h2 class="sectionTitle"><%=I18n.get("COST_ADDITIONAL")%></h2>
<%
    pageState.addClientEntry("classification","##classificationId##");
    pageState.addClientEntry("classification_txt","##classificationName##");

    SmartCombo costClass =FinancialBricks.getCostClassificationCombo("classification",task.getArea(),pageState);
    costClass.label="";
    costClass.separator="";
    costClass.id="";
    costClass.script="style=width:100%";

%>
<table class="table fixHead fixFoot" id="tableAddCosts" cellpadding="0" cellspacing="1">
    <thead>
    <tr>
        <th class="tableHead"><div class="tableHeadEl"><%=I18n.get("DESCRIPTION")%></div></th>
        <th class="tableHead"><div class="tableHeadEl"><%=I18n.get("COST_CLASSIFICATION")%></div></th>
        <th class="tableHead"><%=I18n.get("DATE")%></th>
        <th class="tableHead" style="text-align: right" ><div class="tableHeadEl"><%=I18n.get("COST_ESTIMATED")%></div></th>
        <th class="tableHead" style="text-align: right" ><div class="tableHeadEl"><%=I18n.get("COST_DONE")%></div></th>


      <%
        int numOfCustomFields=0;
        if ( DesignerField.hasCustomField("COST_ADD_CUSTOM_FIELD_", 4)) {
          for (int i = 1; i < 5; i++) {
            if (I18n.isActive("COST_ADD_CUSTOM_FIELD_" + i)) {
              numOfCustomFields++;
            %><th nowrap class="tableHead"><%=I18n.get(I18n.get("COST_ADD_CUSTOM_FIELD_" + i).split(",")[0])%></th><%
          }
        }
      }
    %>

        <th class="tableHead">&nbsp;</th>
        <th class="tableHead" width="40">&nbsp;</th>
    </tr>
    </thead>
  <%
    for (Task t : allTasks) {
      costList.mainObjectId=t.getId();
      ButtonLink edit = ButtonLink.getTextualInstance(t.getDisplayName(), costList);

      ButtonJS add=new ButtonJS("addAddCost($(this))");
      add.additionalCssClass="edit addAddCostBtn";
      add.iconChar="P";

      String color=t.getTaskColor();
      String styleColor="";
      if (I18n.isActive("CUSTOM_FEATURE_USE_PROJECT_COLOR") && JSP.ex(color)){
        styleColor=" style=\"border-left: 5px solid "+color+";\"";
      }

  %>
      <tr class="tableSectionSeparator"><td colspan="99" ></td></tr>

      <tr taskId="<%=t.getId()%>" class="tableSection">
        <td colspan='<%=6+numOfCustomFields%>' class="childNode" <%=styleColor%>>
          <h2><%
            if (t.equals(pageState.getMainObject())){
              %><%=t.getDisplayName()%><%
            }else {
              %><span class="pathSmall inline"><%=t.getPath(" / ", false)%> <%=t.getParent()==null?"":"/"%></span><%edit.toHtml(pageContext);%><%
            }%>
          </h2>
        </td>
        <td align="center"><%add.toHtmlInTextOnlyModality(pageContext);%></td></tr>
      <%
      boolean canManageCosts=t.hasPermissionFor(logged, TeamworkPermissions.task_canWrite);
      List<Cost> costs = new ArrayList(t.getCosts());
      Collections.sort(costs, new PlatformComparators.CreationDateComparator());
      for (Cost cost: costs){
        JspHelper costrow = new JspHelper("/applications/teamwork/task/financial/partTaskAdditionalCostRow.jsp");
        costrow.parameters.put("cost", cost);
        costrow.parameters.put("task", t);
        costrow.parameters.put("canManageCosts", canManageCosts);
        costrow.toHtml(pageContext);
      }
    }
  %>
  <tfoot>
  <tr class="data totals">
    <td colspan="3"><%=I18n.get("TOTAL")%></td>
    <td  style="text-align: right" id="addCostEstimTot"></td>
    <td  style="text-align: right" id="addCostRealTot"></td>
    <td colspan="<%=2+numOfCustomFields%>">&nbsp;</td>
  </tr>
  </tfoot>

</table>
</div>
<br>
<div id="granTotale" style="border-top:1px solid #a5a5a5;" class="buttonArea">

<table class="granTotale table" border="0">
    <tr class="budgetRow">
        <td></td>
        <td align="right"><%=I18n.get("BUDGET")%></td>
        <td>&nbsp;</td>
        <td align="right"><%=I18n.get("PLANNED_COST_TOTAL")%></td>
        <td>&nbsp;</td>
        <td align="right"><%=I18n.get("ESTIMATED_BUDGET_DELTA")%></td>
        <td align="right"><%=I18n.get("PERCENTAGE_SHORT")%></td>
    </tr>
    <tr style="font-size:18px" class="budgetRow">
        <td width="60%" align="right"><%=I18n.get("COST_FORECAST")%></td>
        <td align="right" class="gtBudget"></td>
        <td align="center">-</td>
        <td align="right" class="gtEstimated"></td>
        <td align="center">=</td>
        <td align="right" class="gtEstimDeltaBudget"></td>
        <td align="right" class="gtEstimDeltaBudgetPerc" style="font-size:15px"></td>
    </tr>
    <tr style="border-top:1px solid #cccccc;">
        <td></td>
        <td align="right"><%=I18n.get("PLANNED_COST_TOTAL")%></td>
        <td>&nbsp;</td>
        <td align="right"><%=I18n.get("HOURLY_COST_TOTAL_DONE")%></td>
        <td>&nbsp;</td>
        <td align="right"><%=I18n.get("WORKLOG_DELTA")%></td>
    </tr>
    <tr style="font-size:18px;border-bottom:1px solid #cccccc;">
        <td width="60%" align="right"><%=I18n.get("COST_PLANNED")%></td>
        <td align="right" class="gtEstimated"></td>
        <td align="center" style="width: 50px">-</td>
        <td align="right" class="gtReal"></td>
        <td align="center" style="width: 50px">=</td>
        <td align="right" class="gtDelta"></td>
        <td align="right" class="gtDeltaPerc" style="font-size:15px"></td>
    </tr>
    <tr class="budgetRow">
        <td></td>
        <td align="right"><%=I18n.get("BUDGET")%></td>
        <td>&nbsp;</td>
        <td align="right"><%=I18n.get("HOURLY_COST_TOTAL_DONE")%></td>
        <td>&nbsp;</td>
        <td align="right"><%=I18n.get("BUDGET_DELTA")%></td>
    </tr>
    <tr style="font-size:18px"  class="budgetRow">
        <td width="60%" align="right"><%=I18n.get("COST_ACTUAL")%></td>
        <td align="right" class="gtBudget"></td>
        <td align="center">-</td>
        <td align="right" class="gtReal"></td>
        <td align="center">=</td>
        <td align="right" class="gtDeltaBudget"></td>
        <td align="right" style="font-size:15px" class="gtDeltaBudgetPerc"></td>
    </tr>
</table>
</div>


<script type="text/javascript">

var costValues =<%=values%>;

$(function () {
  recomputeTotals();
  recomputeAddCosts();
});


function wlEstimChanged(inputEl) {
  var row = inputEl.closest("[assid]");
  var assId = row.attr("assId");
  costValues[assId].wlEstim = millisFromString(inputEl.val(), true);
  recomputeRow(row);
  recomputeTotals();
}

function hourlyCostChanged(inputEl) {
  var row = inputEl.closest("[assid]");
  var assId = row.attr("assId");
  costValues[assId].hourlyCost = getCurrencyValue(inputEl.val());
  recomputeRow(row);
  recomputeTotals();
}

function budgetChanged(inputEl) {
  var row = inputEl.closest("[assid]");
  var assId = row.attr("assId");
  costValues[assId].assBudget = getCurrencyValue(inputEl.val());
  recomputeRow(row);
  recomputeTotals();
}


function recomputeRow(row) {
  var assId = row.attr("assId");
  costs = costValues[assId];
  var delta = costs.wlEstim * costs.hourlyCost / 3600000 - costs.wlReal * costs.hourlyCost / 3600000 + costs.assBudget - costs.assReal;

  row.find(".wlEstimTot").html(formatCurrency(costs.wlEstim * costs.hourlyCost / 3600000));
  row.find(".wlRealTot").html(formatCurrency(costs.wlReal * costs.hourlyCost / 3600000));
  row.find(".assDelta").html(formatCurrency(delta)).css("color", delta >= 0 ? "black" : "#D30202");
}


function recomputeTotals() {
  var tot = {
    wlEstim:    0,
    wlReal:     0,
    hourlyCost: 0,
    wlTotEstim: 0,
    wlTotReal:  0,
    assBudget:  0,
    assReal:    0,
    delta:      0
  };
  var quant = 0;
  for (var i in costValues) {
    quant++;
    var cost = costValues[i];
    tot.wlEstim += cost.wlEstim;
    tot.wlReal += cost.wlReal;
    tot.hourlyCost += cost.hourlyCost;
    tot.wlTotEstim += cost.wlEstim * cost.hourlyCost / 3600000;
    tot.wlTotReal += cost.wlReal * cost.hourlyCost / 3600000;
    tot.assBudget += cost.assBudget;
    tot.assReal += cost.assReal;
    tot.delta += cost.wlEstim * cost.hourlyCost / 3600000 - cost.wlReal * cost.hourlyCost / 3600000 + cost.assBudget - cost.assReal;
  }
  tot.hourlyCost /= quant;

  var row = $("#gtWL");
  row.find(".wlEstim").html(getMillisInHoursMinutes(tot.wlEstim));
  row.find(".wlReal").html(getMillisInHoursMinutes(tot.wlReal));
  row.find(".avg").html(formatNumber(tot.hourlyCost));
  row.find(".wlEstimTot").html(formatCurrency(tot.wlTotEstim));
  row.find(".wlRealTot").html(formatCurrency(tot.wlTotReal));
  row.find(".assBudget").html(formatCurrency(tot.assBudget));
  row.find(".assReal").html(formatCurrency(tot.assReal));
  row.find(".assDelta").html(formatCurrency(tot.delta)).css("color", tot.delta >= 0 ? "black" : "#D30202");

  recomputeGranTotale();
  $(window).resize();
}


function recomputeAddCosts() {
  var addCostEstim = 0;
  var addCostReal = 0;
  $("#tableAddCosts .addCstEstim").each(function () {addCostEstim +=getCurrencyValue( $(this).text());});
  $("#tableAddCosts .addCstReal").each(function () {addCostReal +=getCurrencyValue( $(this).text());});
  $("#addCostEstimTot").html(formatCurrency(addCostEstim));
  $("#addCostRealTot").html(formatCurrency(addCostReal));

  recomputeGranTotale();
}


function recomputeGranTotale() {
  var bdg = getCurrencyValue($("#COST_ESTIMATED").val());
  var estim = getCurrencyValue($("#addCostEstimTot").text()) + getCurrencyValue($("#gtEstimWl").text()) + getCurrencyValue($("#gtassBudget").text());
  var real = getCurrencyValue($("#addCostRealTot").text()) + getCurrencyValue($("#gtRealWl").text()) + getCurrencyValue($("#gtAssReal").text());

  //console.debug("recomputeGranTotale",bdg,estim,real);

  //percentage
  var percPrevisto=parseFloat(((bdg - estim)*100)/bdg).toFixed(2);
  var percLavorato=parseFloat((estim - real)*100/estim).toFixed(2);
  var percTotale=parseFloat(((bdg - real))*100/bdg).toFixed(2);

  if (bdg) {
    $(".budgetRow").show();
    $(".gtBudget").html(formatCurrency(bdg));
    $(".gtDeltaBudget").html(formatCurrency(bdg - real)).css("color", bdg - real < 0 ? "rgb(182, 30, 45)" : "black");
    $(".gtDeltaBudgetPerc").html(percTotale+"%").css("color", percTotale < 0 ? "rgb(182, 30, 45)" : "black");
  } else {
    $(".budgetRow").hide();
  }

  $(".gtEstimated").html(formatCurrency(estim));
  $(".gtEstimDeltaBudget").html(formatCurrency(bdg - estim)).css("color", bdg - estim < 0 ? "rgb(182, 30, 45)" : "black");
  $(".gtReal").html(formatCurrency(real));
  $(".gtDelta").html(formatCurrency(estim - real)).css("color", estim - real < 0 ? "rgb(182, 30, 45)" : "black");

  $(".gtDeltaPerc").html(percLavorato+"%").css("color", percLavorato < 0 ? "rgb(182, 30, 45)" : "black");
  $(".gtEstimDeltaBudgetPerc").html(percPrevisto+"%").css("color", percPrevisto < 0 ? "rgb(182, 30, 45)" : "black");



}


function saveCosts() {
  var request = {CM: "SAVECOSTSBULK", OBJID: "<%=task.getId()%>"};

  //----------------save budget
  var bdg = $("#COST_ESTIMATED");
  if (bdg.isValueChanged()) {
    request.budget = bdg.val();
  }

  //---------------- save budget custom fields if any
  $('#budgCustomFields').fillJsonWithInputValues(request);


  //----------------save worklog estimations
  var worklogChanges = {};
  $("input.wlEstimInput").each(function () {
    var input = $(this);
    if (input.isValueChanged()) {
      var assId = input.closest("[assId]").attr("assId");
      worklogChanges[assId] = millisFromString(input.val(), true);
    }
  });
  if (Object.size(worklogChanges) > 0)
    request.worklogChanges = JSON.stringify(worklogChanges);


  //----------------save hourly costs
  var hourlyCostChanges = {};
  $("input.hourlyCost").each(function () {
    var input = $(this);
    if (input.isValueChanged()) {
      var assId = input.closest("[assId]").attr("assId");
      hourlyCostChanges[assId] = input.val();
    }
  });
  if (Object.size(hourlyCostChanges) > 0)
    request.assCosts = JSON.stringify(hourlyCostChanges);

  //----------------save budgets
  var budgetChanges = {};
  $("input.assBudget").each(function () {
    var input = $(this);
    if (input.isValueChanged()) {
      var assId = input.closest("[assId]").attr("assId");
      budgetChanges[assId] = input.val();
    }
  });
  if (Object.size(budgetChanges) > 0)
    request.budgetChanges = JSON.stringify(budgetChanges);


  $.getJSON("costAjaxController.jsp", request, function (response) {
    jsonResponseHandling(response);
    if (response.ok) {
      //if new costs added
      if (response.newlyAddedCosts && response.newlyAddedCosts.length > 0) {
        //remove all new rows
        $("#tableAddCosts tr[costId=new]").remove();

        for (var i = 0; i < response.newlyAddedCosts.length; i++) {
          var cost = response.newlyAddedCosts[i];
          $("#tableAddCosts [taskId=" + cost.taskId + "]:last").after($.JST.createFromTemplate(cost, "ADDCSTTMPL"));
        }
      }
      bdg.val(response.budget);
      $(":input[oldValue]").updateOldValue();
      showFeedbackMessage("OK", "<%=I18n.get("COSTS_CORRECTLY_SAVED")%>");
    }
  });
}


function editAssigCosts(assId) {
  openBlackPopup(contextPath+"/applications/teamwork/task/financial/taskAssignmentCosts.jsp?OBJID=" + assId, '1180px', '768px', function (response) {

    //call server to load updated values
    var request = {
      CM:    "GETCOSTS",
      assId: assId
    };
    $.getJSON('costAjaxController.jsp', request, function (response) {
      jsonResponseHandling(response);
      if (response.ok) {
        costValues[assId].assBudget = response.assBudget;
        costValues[assId].assReal = response.assReal;
        var row = $("tr[assId=" + assId + "]");
        recomputeRow(row);
        recomputeTotals();
        row.find("input.assBudget").val(formatCurrency(response.assBudget)).updateOldValue();
        row.find(".assReal").html(formatCurrency(response.assReal));
      }
    });
  });
}

<%-------------------------------- ADDITIONAL COST  MANAGEMENT -----------------------------------%>

function addAddCost(el) {
  var taskId = el.closest("[taskId]").attr("taskId");
  var url=contextPath+"/applications/teamwork/task/financial/additionalCostEditor.jsp?taskId="+taskId;
  openBlackPopup(url,500,500,saveAddCostCallback);
}

function saveAddCostCallback(cost){
  //console.debug("saveCostCallback",cost);
  if (cost){
    $.get("partTaskAdditionalCostRow.jsp?costId="+cost.id+"&taskId="+cost.taskId,function(row){
      row=$(row);
      var ndo= $("#tableAddCosts");
      var tr=ndo.find("tr[costId="+cost.id+"]");
      if (tr.size()>0) {
        tr.replaceWith(row);
      } else {
        ndo.find("tr[taskId="+cost.taskId+"].tableSection").after(row);
      }
      window.scrollTo(0,row.offset().top-$(window).height()/2);
      recomputeAddCosts();
      row.effect("highlight", { color: "#F9EFC5" }, 1500);
    });
  }
}


function editAddCost(el){
  var row=el.closest("[costId]");
  var url=contextPath+"/applications/teamwork/task/financial/additionalCostEditor.jsp?costId="+row.attr("costId")+"&taskId="+row.attr("taskId");
  openBlackPopup(url,500,500,saveAddCostCallback);
}


function deleteAddCost(el) {
  var row = el.closest("tr");
  var request = {
    CM:   "DLADDCOST",
    costId:parseInt(row.attr("costId")),
    taskId:parseInt(row.attr("taskId"))
  };
  $.getJSON('costAjaxController.jsp', request, function (response) {
    jsonResponseHandling(response);
    if (response.ok) {
      row.fadeOut(500, function () {
        $(this).remove();
        recomputeAddCosts();
      });
    }
  });
}




</script>
<%

    }
%></div>

<%---------------------------------------------- RIGHT COLUMN START ---------------------------------------------------------%><%
    JspHelper side = new JspHelper("/applications/teamwork/task/part/partTaskSideBar.jsp");
    side.parameters.put("TASK",task);
    side.toHtml(pageContext);

//---------------------------------------------- RIGHT COLUMN END ---------------------------------------------------------

        form.end(pageContext);
    }
%>
