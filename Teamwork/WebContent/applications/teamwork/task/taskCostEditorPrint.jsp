<%@ page import="com.twproject.task.Assignment, com.twproject.task.Task, com.twproject.task.businessLogic.TaskController, com.twproject.task.financial.Cost, com.twproject.utilities.TeamworkComparators, com.twproject.waf.TeamworkPopUpScreen,
                 com.twproject.worklog.businessLogic.WorklogBricks, org.jblooming.agenda.CompanyCalendar, org.jblooming.oql.OqlQuery, org.jblooming.persistence.PersistenceHome, org.jblooming.security.Area, org.jblooming.system.SystemConstants, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP,
                 org.jblooming.utilities.NumberUtilities, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.display.Img,
                 org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.ColorValueChooser, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.ClientEntry, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState,
                 java.util.*, org.jblooming.ontology.PlatformComparators, org.jblooming.designer.DesignerField" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new TaskController(), request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);

  } else {
    PageSeed self = pageState.thisPage(request);
    self.setCommand(Commands.EDIT);
    self.mainObjectId = pageState.mainObjectId;
    Form f = new Form(self);
    pageState.setForm(f);
    f.start(pageContext);

    Img logo = new Img(ApplicationState.getApplicationSetting(SystemConstants.PRINT_LOGO), "");

%><table border="0" width="100%" align="center" cellpadding="5" cellspacing="0" class="noprint">
  <tr>
    <td align="left" width="90%"><%logo.toHtml(pageContext);%></td>
    <td align="right"><%

      ButtonJS print = new ButtonJS("window.print();");
      print.label = "";
      print.toolTip = I18n.get("PRINT_PAGE");
      print.iconChar = "p";
      print.toHtmlInTextOnlyModality(pageContext);

    %></td>
  </tr>
</table><%

  boolean includeOtherCosts = pageState.getEntryOrDefault("INCLUDE_OTHER_COSTS", Fields.TRUE).checkFieldValue();
  boolean includeDescendantCosts = pageState.getEntryOrDefault("INCLUDE_DESCS_COSTS", Fields.TRUE).checkFieldValue();
  Task task = (Task) pageState.getMainObject();


%><div id="printFilter" style="visibility:visible;position:relative;" class="noprint">
  <table width="100%" align="center">
    <tr>
      <td align="left" width="100%">
  <%
    Container filter = new Container("IP");
    filter.level=2;
    filter.start(pageContext);

        CheckField includeOtherCostsCheck = new CheckField("INCLUDE_OTHER_COSTS", "&nbsp;", false);
        includeOtherCostsCheck.preserveOldValue = false;
        includeOtherCostsCheck.toHtmlI18n(pageContext);

        CheckField includeDescsCosts = new CheckField("INCLUDE_DESCS_COSTS", "&nbsp;", false);
        includeDescsCosts.preserveOldValue = false;
        includeDescsCosts.toHtmlI18n(pageContext);


        %>&nbsp;&nbsp;&nbsp;<span  class="lreq20 lreqHide"><%
        ColorValueChooser chooser = WorklogBricks.getStatusChooser("WORKLOG_STATUS", true, pageState);
        chooser.label = I18n.get("WORKLOG_STATUS");
        chooser.height=26;
        chooser.separator = "&nbsp;";
        chooser.preserveOldValue = false;
        chooser.showOpener = true;
        chooser.toHtml(pageContext);

        %>&nbsp;&nbsp;&nbsp;</span><%
        ButtonSubmit sc = new ButtonSubmit(f);
        sc.label = I18n.get("REFRESH");
        sc.additionalCssClass = "small";
        sc.toHtml(pageContext);

  filter.end(pageContext);
%></td></tr></table>
</div>

<table width="100%" align="center" border="0">
  <tr>
     <td><h3><small>[<%=task.getId()%>]</small> <%=task.getDisplayName()%></h3><br></td>
  </tr>
  <tr>
    <td><b><%=I18n.get("BUDGET")%></b>:&nbsp;<%=JSP.currency(task.getForecasted())%><%
      if (task.getChildrenSize() > 0) {
        String hql = "select sum(task.forecasted) from " + Task.class.getName() + " as task where task.parent = :myself";
        OqlQuery oql = new OqlQuery(hql);
        oql.getQuery().setEntity("myself", task);
        Double e = (Double) oql.uniqueResultNullIfEmpty();
        if (f == null)
          e = 0.0;
    %>&nbsp;&nbsp;&nbsp;&nbsp;(<%=I18n.get("BUDGET_ON_CHILDREN")%>:<%=JSP.currency(e)%>)<%
      }
    %></td>
  </tr>
</table>
<%
//try to know if there is custom fields
if (DesignerField.hasCustomField("BUDGET_CUSTOM_FIELD_", 4)){

//---------------------------------------------------- CUSTOM FIELDS ------------------------------------------------------------------------
%><div class="clearfix"><%
  for (int i=1; i<5; i++) {
    DesignerField dfStr = DesignerField.getCustomFieldInstance("BUDGET_CUSTOM_FIELD_", "budgetCustomField", i, task, true, true, false, pageState);
    if (dfStr!=null){
      dfStr.separator=":&nbsp; ";
      %><div style="float:left;margin-right: 30px"><%dfStr.toHtml(pageContext);%></div><%
    }
  }

%></div><%

  }
%>
<h2 class="sectionTitle"><%=I18n.get("WORKLOG_ESTIMATION_FROM_ASSIGS")%></h2>
<table width="100%" align="center" cellpadding="4" cellspacing="0" class="table">
<tr>
  <th class="tableHead">&nbsp;</th>
  <th class="tableHead" style="text-align:center;" colspan="4"><%=I18n.get("WORKLOG_WORKLOG")%></th>
  <th class="tableHead" style="text-align:center;" colspan="2"><%=I18n.get("WORKLOG_MONEY")%></th>
  <th class="tableHead lreq20 lreqHide" style="text-align:center;" colspan="2"><%=I18n.get("ASSIG_EXPENSES")%></th>
  <th class="tableHead" style="text-align:right;">&nbsp;</th>
</tr>
<tr>
  <th class="tableHead secondRow"><%=I18n.get("ASSIGNMENT")%></th>
  <th class="tableHead secondRow" style="text-align:right;"><%=I18n.get("WORKLOG_ESTIMATED")%></th>
  <th class="tableHead secondRow" style="text-align:right;"><%=I18n.get("WORKLOG_DONE")%></th>
  <th class="tableHead secondRow" style="text-align:right;"><%=I18n.get("WORKLOG_DELTA")%></th>
  <th class="tableHead secondRow" style="text-align:right;"><%=I18n.get("HOURLY_COST")%></th>
  <th class="tableHead secondRow" style="text-align:right;"><%=I18n.get("HOURLY_COST_TOTAL_EST")%></th>
  <th class="tableHead secondRow" style="text-align:right;"><%=I18n.get("HOURLY_COST_TOTAL_DONE")%></th>
  <th class="tableHead secondRow lreq20 lreqHide" style="text-align:right;"><%=I18n.get("PERSONAL_BUDGET")%></th>
  <th class="tableHead secondRow lreq20 lreqHide" style="text-align:right;"><%=I18n.get("COST_DONE")%></th>
  <th class="tableHead secondRow" style="text-align:right;"><%=I18n.get("WORKLOG_DELTA")%></th>
</tr><%
  String hql = "from " + Assignment.class.getName() + " as assig where ";
  hql = hql + "assig.task.id=:tid ";
  if (includeDescendantCosts)
    hql = hql + "or assig.task.ancestorIds like (:descs)";
  hql = hql + " order by assig.task.id";
  OqlQuery oql = new OqlQuery(hql);
  oql.getQuery().setParameter("tid", task.getId());
  if (includeDescendantCosts)
    oql.getQuery().setParameter("descs", task.getChildAncentorIds()+"%");

  List<Assignment> assigs = oql.list();


  double totalEst = 0;
  double totalDone = 0;
  double totalCostEst = 0;
  double totalCostDone = 0;

  double totalCostWorklogEst = 0;
  double totalCostWorklogDone = 0;

  // filter for WorklogStatus
  String wlsid = pageState.getEntry("WORKLOG_STATUS").stringValue();

  for (Assignment assignment : assigs) {

    double ew = assignment.getEstimatedWorklog();
    double wd = assignment.getWorklogDone(wlsid);

    double assCosts= assignment.computeCosts();
    double assBudget= assignment.getBudget();


    if (ew > 0 || wd > 0 || assBudget>0 || assCosts>0) {

%><tr class="alternate">
  <td><%=assignment.getDisplayNameFull()%></td>
  <td align="right"><%=DateUtilities.getMillisInHoursMinutes(ew)%></td><%
    if (wd > 0) {
 %><td align="right"><%=wd > 0 ? DateUtilities.getMillisInHoursMinutes(((long) wd)) : "&nbsp;"%></td><%
  } else {
  %><td align="right"><%=0D%></td><%
    }

    double delta = ew - wd;
    totalEst += ew;
    totalDone += wd;
    totalCostEst += assBudget;
    totalCostDone += assCosts;
    double estCost = (((double) ew) / CompanyCalendar.MILLIS_IN_HOUR) * assignment.getHourlyCost();
    totalCostWorklogEst += estCost;
    double currentCostOfWklDone = ((double) wd / CompanyCalendar.MILLIS_IN_HOUR) * assignment.getHourlyCost();
    totalCostWorklogDone += currentCostOfWklDone;
    double deltaCost = estCost + assBudget- currentCostOfWklDone-assCosts;

    %>
    <td align="right"><%=delta >= 0 ? "" : "<font color=\"red\">"%><%=DateUtilities.getMillisInHoursMinutes((long) delta)%><%=delta >= 0 ? "" : "</font>"%></td>
    <td align="right"><%=assignment.getHourlyCost()%></td>
    <td align="right"><%=JSP.currency(estCost)%></td>
    <td align="right"><%=JSP.currency(currentCostOfWklDone)%></td>
    <td align="right" class="lreq20 lreqHide"><%=JSP.currency(assBudget)%></td>
    <td align="right" class="lreq20 lreqHide"><%=JSP.currency(assCosts)%></td>
    <td align="right"><%=deltaCost >= 0 ? "" : "<font color=\"red\">"%><%=JSP.currency(deltaCost)%><%=deltaCost >= 0 ? "" : "</font>"%></td>
    </tr>
    <%
    }
  }
  if (assigs.size() > 0) {
    double totalDelta = totalCostWorklogEst - totalCostWorklogDone+totalCostEst-totalCostDone;

    %>
    <tr class="totals">
      <td><%=I18n.get("TOTAL")%></td>
      <td align="right" ><%=DateUtilities.getMillisInHoursMinutes(totalEst)%></td>
      <td align="right"><%=DateUtilities.getMillisInHoursMinutes(totalDone)%></td>
      <td align="right"><%=totalEst - totalDone >= 0 ? "" : "<font color=\"red\">"%><%=DateUtilities.getMillisInHoursMinutes(totalEst - totalDone)%><%=totalEst - totalDone >= 0 ? "" : "</font>"%></td>
      <td align="right"><%
        if (totalEst > 0) {
          %>(<%=NumberUtilities.decimal(totalCostWorklogEst * CompanyCalendar.MILLIS_IN_HOUR / ((double) totalEst), 2)%><%=I18n.get("AVERAGE_SHORT")%>)<%
          } else {
          %>&nbsp;<%
        }
      %></td>
      <td align="right"><%=JSP.currency(totalCostWorklogEst)%></td>
      <td align="right"><b><%=JSP.currency(totalCostWorklogDone)%></b></td>
      <td align="right" class="lreq20 lreqHide"><%=JSP.currency(totalCostEst)%></td>
      <td align="right" class="lreq20 lreqHide"><b><%=JSP.currency(totalCostDone)%></b></td>
      <td align="right"><b><%=totalDelta >= 0 ? "" : "<font color=\"red\">"%><%=JSP.currency(totalDelta)%><%=totalDelta >= 0 ? "" : "</font>"%>
      </b></td>
    </tr>
    <%

  }
%></table><%
  double totalEstAdd = 0;
  double totalRealAdd = 0;
  if (includeOtherCosts) {
%><br>
<br>
<h2 class="sectionTitle"><%=I18n.get("COST_ADDITIONAL")%></h2>
<table width="100%" align="center" cellpadding="4" cellspacing="0" class="table">
  <tr>
    <th class="tableHead"><%=I18n.get("DESCRIPTION")%></th>
    <th class="tableHead" style="text-align:center;"><%=I18n.get("COST_CLASSIFICATION")%></th>
    <th class="tableHead" style="text-align:center;"><%=I18n.get("DATE")%></th>
    <th class="tableHead" style="text-align:right;"><%=I18n.get("COST_ESTIMATED")%></th>
    <th class="tableHead" style="text-align:right;"><%=I18n.get("COST_DONE")%></th>

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

  </tr><%

    Set<Area> areas = new HashSet();
    areas.add(task.getArea());

    //loop on persisted costs
    List<Cost> costs = new ArrayList(task.getCosts());
    Collections.sort(costs, new PlatformComparators.CreationDateComparator());

    //direct costs on current task
    for (Cost cost : costs) {
      totalEstAdd += cost.getEstimatedCost();
      totalRealAdd += cost.getRealCost();
  %><tr class="alternate">
    <td align="left"><%=JSP.w(cost.getDescription())%></td>
  <td align="center"><%=cost.getClassification() != null ? cost.getClassification().getName(): ""%></td>
  <td align="right"><%=JSP.w(cost.getCreationDate())%></td>
  <td align="right"><%=JSP.currency(cost.getEstimatedCost())%></td>
    <td align="right"><%=JSP.currency(cost.getRealCost())%></td>

    <%
    if ( DesignerField.hasCustomField("COST_ADD_CUSTOM_FIELD_", 4)) {
      for (int i = 1; i < 5; i++) {
        DesignerField dfStr = DesignerField.getCustomFieldInstance("COST_ADD_CUSTOM_FIELD_", i, cost, true, true, false, pageState);
        if (dfStr!=null){
          dfStr.label="";
          dfStr.separator="";
          %><td><%dfStr.toHtml(pageContext);%></td><%
        }
      }
    }
    %>
  </tr><%
    }

  %>
  <tr height="1"></tr>
  <%
    //eventually loop on child costs
    if (includeDescendantCosts) {
      List<Task> picciriddi = task.getDescendants();
      for (Task t : picciriddi) {
        //loop on persisted costs
        List<Cost> chCosts = new ArrayList(t.getCosts());
        Collections.sort(chCosts,new PlatformComparators.CreationDateComparator());
        for (Cost cost : chCosts) {
          totalEstAdd += cost.getEstimatedCost();
          totalRealAdd += cost.getRealCost();
  %><tr>
    <td align="left"><%=JSP.w(cost.getDescription())%></td>
    <td align="center"><%=cost.getClassification() != null ? cost.getClassification().getName(): ""%></td>
  <td align="right"><%=JSP.w(cost.getCreationDate())%></td>
    <td align="right"><%=JSP.currency(cost.getEstimatedCost())%></td>
    <td align="right"><%=JSP.currency(cost.getRealCost())%></td>
  <%
    if ( DesignerField.hasCustomField("COST_ADD_CUSTOM_FIELD_", 4)) {
      for (int i = 1; i < 5; i++) {
        DesignerField dfStr = DesignerField.getCustomFieldInstance("COST_ADD_CUSTOM_FIELD_", i, cost, true, true, false, pageState);
        if (dfStr!=null){
          dfStr.label="";
          dfStr.separator="";
  %><td><%dfStr.toHtml(pageContext);%></td><%
      }
    }
  }
%>
  </tr><%
        }


      }

    }

    //loop on just added costs
    Map<String, ClientEntry> costsAdded = pageState.getClientEntries().getEntriesStartingWithStripped("COST_DESCRIPTION_" + PersistenceHome.NEW_EMPTY_ID + "_");
    //find the max
    //int maxId = 0;

    for (String costId : costsAdded.keySet()) {
      Cost cost = (Cost) PersistenceHome.findByPrimaryKey(Cost.class, costId);
      //maxId = Math.max(maxId,Integer.parseInt(costId));
    }
  %><tr class="totals">
    <td align="center" colspan="3">&nbsp;</td>
    <td align="right" style="text-align:right;"><%=JSP.currency(totalEstAdd)%></td>
    <td align="right" style="text-align:right;"><%=JSP.currency(totalRealAdd)%></td>
    <%
      if (numOfCustomFields>0){
        %><td align="center" colspan="<%=numOfCustomFields%>">&nbsp;</td><%
      }
    %>
  </tr>
 </table><%
  } // END ADDITIONAL COSTS
  double totalEstimated = totalCostWorklogEst + totalEstAdd + totalCostEst;
  double totalRealCost = totalCostWorklogDone + totalRealAdd +totalCostDone;
  double totalDelta = totalEstimated - totalRealCost;
  double deltaOnBudget = task.getForecasted() - totalRealCost;
%><br>
<br>
<table width="100%" align="center" cellpadding="4" cellspacing="0" class="table">
  <tr>
    <th class="tableHead" style="text-align:center;"><%=I18n.get("HOURLY_COST_TOTAL_EST")%></th>
    <th class="tableHead" style="text-align:center;"><%=I18n.get("HOURLY_COST_TOTAL_DONE")%></th>
    <th class="tableHead" style="text-align:center;"><%=I18n.get("WORKLOG_DELTA")%></th>
    <th class="tableHead" style="text-align:center;"><%=I18n.get("BUDGET_DELTA")%></th>
  </tr>
  <tr class="alternate">
    <td align="center"><big><b><%=JSP.currency(totalEstimated)%></b></big></td>
    <td align="center"><big><b><%=JSP.currency(totalRealCost)%></b></big></td>
    <td align="center"><big><b><%=totalDelta > 0 ? "" : "<font color=\"red\">"%><%=JSP.currency(totalDelta)%><%=totalDelta > 0 ? "" : "</font>"%></b></big></td>
    <td align="center"><big><b><%=deltaOnBudget > 0 ? "" : "<font color=\"red\">"%><%=JSP.currency(deltaOnBudget)%><%=deltaOnBudget > 0 ? "" : "</font>"%></b></big></td>
  </tr>
</table>
<br>
<p align="right"><i><%=I18n.get("PRINTED_ON", JSP.timeStamp(new Date()))%>
</i></p><%

    f.end(pageContext);

  }
%>
