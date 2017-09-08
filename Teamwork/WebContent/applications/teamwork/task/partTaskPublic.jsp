<%@ page import="com.twproject.resource.Resource, com.twproject.task.*, com.twproject.task.financial.Cost, org.jblooming.agenda.CompanyCalendar, org.jblooming.designer.DesignerField, org.jblooming.ontology.PlatformComparators, org.jblooming.oql.OqlQuery, org.jblooming.oql.QueryHelper, org.jblooming.utilities.*, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.container.Tab, org.jblooming.waf.html.container.TabSet, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.display.PercentileDisplay, org.jblooming.waf.html.input.TextArea, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.input.Uploader, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.*, com.twproject.security.TeamworkPermissions" %>
<%@page pageEncoding="UTF-8"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  Task task = null;
  JspHelper dDrawer = (JspHelper) JspIncluderSupport.getCurrentInstance(request);
  task = (Task) dDrawer.parameters.get("task");
  pageState.setMainObject(task);


  boolean canSeeAssignee = "true".equals(dDrawer.parameters.get("canSeeAssignee") + "");
  boolean canSeeCosts = "true".equals(dDrawer.parameters.get("canSeeCosts") + "");
  boolean canSeeAdditionalCosts = "true".equals(dDrawer.parameters.get("canSeeAdditionalCosts") + "");
  boolean canSeeIssues = "true".equals(dDrawer.parameters.get("canSeeIssues") + "");
  boolean canSeeIssuesCF = "true".equals(dDrawer.parameters.get("canSeeIssuesCF") + "");
  boolean canSeeWorklog = "true".equals(dDrawer.parameters.get("canSeeWorklog") + "");
  boolean canAddIssue = "true".equals(dDrawer.parameters.get("canAddIssue") + "");
  boolean canAddFile = "true".equals(dDrawer.parameters.get("canAddFile") + "");
  boolean showGantt = "true".equals(dDrawer.parameters.get("showGantt") + "");
  boolean showSummary = "true".equals(dDrawer.parameters.get("showSummary") + "");
  boolean canSeeChildren = "true".equals(dDrawer.parameters.get("canSeeChildren") + "");
  boolean noCaptcha = "true".equals(dDrawer.parameters.get("noCaptcha") + "");




  TabSet tabSet = new TabSet("tpp", pageState);

  boolean oneTabFocused=false;

  Tab tabOverview=null;
  if (showSummary) {
    tabOverview = new Tab("tskStat", I18n.get("TASK_GENERAL_TAB"));
    tabSet.addTab(tabOverview);

    if (!JSP.ex(pageState.getEntry("tpp")) && !oneTabFocused) {
      tabOverview.focused = true;
      oneTabFocused=true;
    }
  }


  Tab tabAddTicket =null;
  if (canAddIssue ) {
    tabAddTicket = new Tab("tskAddIss", I18n.get("ADD_PROPOSAL"));
    tabSet.addTab(tabAddTicket);

    if (!JSP.ex(pageState.getEntry("tpp")) && !oneTabFocused){
      tabAddTicket.focused = true;
      oneTabFocused=true;
    }
  }


  Tab tabGantt =null;
  if (showGantt) {
    tabGantt = new Tab("tskGantt", I18n.get("TASK_GANTT_TAB"));
    tabGantt.additionalScript="refreshGanttIframe();";
    tabSet.addTab(tabGantt);

    if (!JSP.ex(pageState.getEntry("tpp")) && !oneTabFocused){
      tabGantt.focused = true;
      oneTabFocused=true;
    }
  }


  Tab tabCosts =null;
  if (canSeeCosts || canSeeAdditionalCosts) {
    tabCosts = new Tab("tskCst", I18n.get("TASK_COST_TAB"));
    tabSet.addTab(tabCosts);
    if (!JSP.ex(pageState.getEntry("tpp")) && !oneTabFocused){
      tabCosts.focused = true;
      oneTabFocused=true;
    }
  }


  Tab tabIssues =null;
  if (canSeeIssues ) {
    tabIssues = new Tab("tskIss", I18n.get("ISSUES"));
    tabSet.addTab(tabIssues);
    if (!JSP.ex(pageState.getEntry("tpp")) && !oneTabFocused){
      tabIssues.focused = true;
      oneTabFocused=true;
    }
  }

  if (tabSet.tabs.size()>1)
   tabSet.drawBar(pageContext);


  //------------------------------------------------------------  GENERAL DATA & STATS ----------------------------------------------------------------
  if (tabOverview !=null) {

  tabOverview.start(pageContext);


  PercentileDisplay progressBar = task.bricks.getProgressBar(false);
  progressBar.width="100px";
  progressBar.height="15px";


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


%>
<div class="inlineContainerWrapper">
  <div style="max-width: 350px;" >
    <table class="taskOverview table" >
      <tr>
        <td><label><%=I18n.get("START")%></label></td><td><b class="<%=startInError?"warning":""%>" title="<%=startErrorMessage%>"><%=JSP.w(startDate)%></b> <%=(task.isStartIsMilestone()?" <span class='teamworkIcon' title='"+I18n.get("MILESTONE")+"'>^</span> ":"")%></td>
      </tr>
      <tr>
        <td><label><%=I18n.get("END")%></label></td><td> <b class="<%=endInError?"warning":""%>" title="<%=endErrorMessage%>"><%=JSP.w(task.getSchedule().getEndDate())%></b> <%=(task.isEndIsMilestone()?" <span class='teamworkIcon' title='"+I18n.get("MILESTONE")+"'>^</span> ":"")%></td>
      </tr>
      <tr>
        <td style="border-color: #cccccc"><label><%=I18n.get("DAYS")%></label></td><td style="border-color: #cccccc"><b><%=task.getDuration()%></b></td>
      </tr>
      <tr>
        <td colspan="2" style="border: none;border: none;font-size: 1px;">&nbsp;</td>
      </tr>
      <%if (task.getType() != null) {%>
        <tr><td><label><%=I18n.get("TYPE")%></label></td><td><%=JSP.w(task.getType() != null ? task.getType().getDescription() : "")%></td></tr>
      <%}%>
      <tr><td><label><%=I18n.get("RELEVANCE")%></label></td><td><%=JSP.w(task.getRelevance())%>%</td></tr>
      <tr><td valign="top" style="min-width: 120px"><label><%=I18n.get("PROGRESS")%></label></td><td valign="top"><%progressBar.toHtml(pageContext);%>
        <%
          Map<String,Double> doubleMap = task.fullCheckup();

          double max=0;
          String message ="";

          for (String s : doubleMap.keySet()){
            message += (message.length()>0?", ":"")+I18n.get(s);
            max=max>doubleMap.get(s)?max:doubleMap.get(s);
          }

          if (max<=0)
            message=I18n.get("CK_ALL_OK");
        %><span class="textSmall"><span class="teamworkIcon"  style="margin-left:-18px" >!</span> <%=message%></span>
      </td></tr>
      <tr>
        <td valign="top" nowrap><label title="<%=I18n.get("HELP_ASSIGNMENT_WORKLOG_ESTIMATION")%>"><%=I18n.get("FROM_ASSIG_SHORT")%>:</label></td>
        <td><%=DateUtilities.getMillisInDaysWorkHoursMinutes( task.getTotalWorklogEstimated())%>
      </td>
      </tr>
      <%if(canSeeWorklog){%>
        <tr><td nowrap><label title="<%=I18n.get("HELP_ASSIGNMENT_WORKLOG_ESTIMATION")+" - "+I18n.get("UNIT_DAY_HOUR")%>"><%=I18n.get("WORKLOG_DONE")%>:&nbsp;</label></td>
          <td><%=DateUtilities.getMillisInDaysWorkHoursMinutes(task.getTotalWorklogDone())%></td>
        </tr>
      <%}%>
      <%if (JSP.ex(task.getDescription())) {%>
        <tr>
          <td colspan="2" style="padding-top: 10px"><label><%=I18n.get("DESCRIPTION")%></label>
            <div class="textSmall linkEnabled"style="padding-bottom: 10px"><%=JSP.encode(task.getDescription())%></div>
          </td>
        </tr>
      <%
      }

      if (JSP.ex(task.getTags())) {
        List<String> tags = StringUtilities.splitToList(task.getTags(), ",");
        %><tr><td colspan="2"><%for (String tag:tags){%><span class="button textual tag"><%=tag%></span><%}%></td></tr><%
      }

      if (JSP.ex(task.getNotes())) {
        %><tr><td colspan="2"><label><%=I18n.get("NOTES_DELIVERABLES")%></label><br><span class="textSmall"style="padding-bottom: 10px"><%=JSP.encode(task.getNotes())%></span></td></tr><%
      }



      if (JSP.ex(task.getMilestones())) {
    %>
      <tr><td><label><%=I18n.get("MILESTONES")%></label></td><td>
        <%
          // ----------------------------- MILESTONES START --------------------------------

          List<Task.TaskMilestone> tml = task.getMilestones();

          if (tml.size() > 0) {

        %><div id="milestones"><%

        PageSeed edChTsk = pageState.thisPage(request);
        edChTsk.setCommand(Commands.EDIT);
        //ButtonLink bl = ButtonLink.getTextualInstance("", edChTsk);
        for (Task.TaskMilestone tm : tml) {
          edChTsk.mainObjectId = tm.task.getId();
          %><span title="<%=tm.task.getDisplayName()%>"><%=JSP.w(tm.when)%><small title="<%=I18n.get("RELEVANCE")%>"><%=tm.task.getRelevance()>0?"("+tm.task.getRelevance()+ ")":""%></small>&nbsp;</span><%
        }
        %></div><%
        }
      %>
      </td></tr><%
      }

      //try to know if there is custom forms or custom field
      if (TaskBricks.hasCustomField()){
        //---------------------------------------------------- CUSTOM FIELDS ------------------------------------------------------------------------
        for (int i=1; i<7; i++) {
          if (!JSP.ex((String) ReflectionUtilities.getFieldValue("customField" + i, task)))
            continue;
          DesignerField dfStr = DesignerField.getCustomFieldInstance( "TASK_CUSTOM_FIELD_",i, task,!task.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.task_canWrite), false, false, pageState);
          if (dfStr!=null){
            dfStr.exportable=true;
            dfStr.separator="</td><td>";
            %><tr><td><%dfStr.toHtml(pageContext);%></td></tr><%
          }
        }

      }

    %>
    </table>
    <%



    //------------------------------------------------------------  ASSIGNMENTS ----------------------------------------------------------------
    if (canSeeAssignee && task.getAssignementsSize()>0) {
      %>
    <br>
      <h2><%=I18n.get("WORKGROUP")%></h2>
      <table border="0" cellpadding="5" cellspacing="3" class="table" id="tskAssig">

        <%

          // Show workgroup
          int count=1;
          Set<Resource> visited = new HashSet();
          for (Resource res: task.getWorkGroup()) {
            if (visited.contains(res))
              continue;
            // add ....
            if (count>40 ){
              %><a class="faceMore" title="<%=I18n.get("MORE")%>"><span class="teamworkIcon" style="line-height:36px; font-size:20px">&hellip;</span></a><%
              break;
            }

            Assignment ass = task.getFirstAssignmentsForResource(res,true);
            %>
              <tr>
                <td><img src="<%=res.bricks.getAvatarImageUrl()%>" class="face"></td>
                <td width="90%" ><%=res.getDisplayName()%>&nbsp;</td>
                <td title="<%=ass.getRole().getDisplayName()%>"><%=ass.getRole().getCode()%>&nbsp;</td>
                <%--td nowrap><%=DateUtilities.getMillisInHoursMinutes(ass.getEstimatedWorklog())%>
                  <%=canSeeWorklog?" / "+DateUtilities.getMillisInHoursMinutes(ass.getWorklogDone()):""%>
                </td--%>
              </tr>
            <%
            count++;
            visited.add(res);
          }

        %>
      </table>
        <%

    }

    %>

  </div>

  <div  style="width: 70%" class="container " >
  <jsp:include page="partTaskStatistics.jsp"/>
  </div>
</div>
  <%
  tabOverview.end(pageContext);
  }

  // -------------------------------------------------------------------- GANTT WIDGET ----------------------------------------------------
  if (tabGantt !=null) {
    tabGantt.start(pageContext);
      int h = task.getDescendantsSize(Task.class) * 30 + 150;
      //h = h<550?550:h;
      long t = System.currentTimeMillis();
      %>
        <iframe id="ganttIframe" src='' frameborder=0 id='gnt_<%=t%>' width='100%' height='<%=h%>' style='ddisplay:none;'></iframe>
        <script>
          function refreshGanttIframe(){
            if($("#ganttIframe").attr("src") == "")
              $("#ganttIframe").attr("src","<%=ApplicationState.serverURL%>/applications/teamwork/task/gantt/widget.jsp?key=<%=task.getKey()%>");
          }

          <%if(tabSet.tabs.size()==1){%>
            refreshGanttIframe();
          <%}%>

        </script>
      <%
    tabGantt.end(pageContext);

  }



  //------------------------------------------------------------  COSTS ----------------------------------------------------------------
  if ( tabCosts !=null) {
    tabCosts.start(pageContext);


    //----------------------------------------------------------- WORKLOG COSTS  ---------------------------------------------------------
    if (canSeeCosts) {

    %>
    <table border="0" class="table" id="tskWrkCosts">
    <tr>
      <td colspan="300" style="padding-bottom: 10px">
        <span style="font-size:20px"><label><%=I18n.get("BUDGET")%></label>: <span id="COST_ESTIMATED"><%=JSP.currency(task.getForecasted())%></span></span>
        <%
        if (task.getChildrenSize() > 0) {
          String hql = "select sum(task.forecasted) from " + Task.class.getName() + " as task where task.parent = :myself";
          OqlQuery oql = new OqlQuery(hql);
          oql.getQuery().setEntity("myself", task);
          Double e = (Double) oql.uniqueResultNullIfEmpty();
          %>&nbsp;&nbsp;&nbsp;&nbsp;(<%=I18n.get("BUDGET_ON_CHILDREN")%>: <%=JSP.currency(e)%>)<%
        }
    %>
      <tr>
        <th class="tableHead">&nbsp;</th>
        <th class="tableHead" colspan="2" align="center"><%=I18n.get("WORKLOG")%></th>
        <th class="tableHead" colspan="3" align="center"><%=I18n.get("WORKLOG_MONEY")%></th>
        <th colspan="2" class="tableHead" align="center"><%=I18n.get("ASSIG_EXPENSES")%></th>
        <th  style="text-align: right" class="tableHead" rowspan="2" align="right" nowrap><%=I18n.get("WORKLOG_DELTA")%></th>
      </tr>
      <tr>
        <th class="tableHead secondRow" valign="top" nowrap style="min-width: 300px"><%=I18n.get("ASSIGNMENT")%></th>
        <th style="text-align: right" class="tableHead secondRow"><%=I18n.get("ESTIMATED")%></th>
        <th style="text-align: right" class="tableHead secondRow"><%=I18n.get("DONE")%></th>
        <th style="text-align: right" class="tableHead secondRow"><%=I18n.get("HOURLY_COST")%></th>
        <th style="text-align: right" class="tableHead secondRow"><%=I18n.get("HOURLY_COST_TOTAL_EST")%></th>
        <th style="text-align: right" class="tableHead secondRow"><%=I18n.get("HOURLY_COST_TOTAL_DONE")%></th>
        <th style="text-align: right" class="tableHead secondRow"><%=I18n.get("PERSONAL_BUDGET")%></th>
        <th style="text-align: right" class="tableHead secondRow"><%=I18n.get("COST_DONE")%></th>

      </tr>
      <%
        String hql = "from " + Assignment.class.getName() + " as assig where ";
        hql = hql + "assig.task.ancestorIds like (:descs) or assig.task.id=:tid ";
        hql = hql + "order by assig.task.id ";
        OqlQuery oql = new OqlQuery(hql);
        oql.getQuery().setParameter("descs", task.getChildAncentorIds()+"%");
        oql.getQuery().setParameter("tid", task.getId());

        List<Assignment> assigs = oql.list();

        double totalEst = 0;
        double totalDone = 0;

        double totalCostWorklogEst = 0;
        double totalHourlyCost = 0;
        double totalCost = 0;
        double totalAssigBdg = 0;
        double totalAssigExpenses = 0;

        // filter for WorklogStatus
        for (Assignment assignment : assigs) {

          double estimatedWorklog = assignment.getEstimatedWorklog();
          double worklogDone = assignment.getWorklogDone();
          double assBudget=assignment.getBudget();
          double assReal=0;
          for (Cost c:assignment.getCosts()){
            assReal+=c.getRealCost();
          }

          totalAssigBdg+=assBudget;
          totalAssigExpenses+=assReal;

          if (estimatedWorklog > 0 || worklogDone > 0 || assBudget>0 || assReal>0) {

            double delta = estimatedWorklog - worklogDone;
            totalEst += estimatedWorklog;
            totalDone += worklogDone;
            totalHourlyCost += assignment.getHourlyCost();
            double estCost = (((double) estimatedWorklog) / CompanyCalendar.MILLIS_IN_HOUR) * assignment.getHourlyCost();
            totalCostWorklogEst += estCost;

            double currentCostOfWklDone = ((double) worklogDone / CompanyCalendar.MILLIS_IN_HOUR) * assignment.getHourlyCost();
            totalCost += currentCostOfWklDone;

            double deltaCost = estCost - currentCostOfWklDone+assBudget-assReal;



            %><tr class="alternate">
                  <td><%=assignment.getDisplayName()%></td>
                  <td align="right" ><%=DateUtilities.getMillisInHoursMinutes(estimatedWorklog)%></td>
                  <td align="right"><%=DateUtilities.getMillisInHoursMinutes(((long) worklogDone))%></td>
                  <td align="right" ><%=JSP.currency(assignment.getHourlyCost())%></td>
                  <td align="right" ><%=JSP.currency(estCost)%></td>
                  <td align="right" ><%=JSP.currency(currentCostOfWklDone)%></td>

                  <td align="right" style="cursor:pointer;"><%=JSP.currency(assBudget)%></td>
                  <td align="right" style="cursor:pointer;"><%=JSP.currency(assReal)%></td>
                  <td align="right"><%=deltaCost >= 0 ? "" : "<font color=\"red\">"%><%=JSP.currency(deltaCost)%><%=deltaCost >= 0 ? "" : "</font>"%></td>
              </tr><%
          }
        }
      double totalDelta = totalCostWorklogEst - totalCost+totalAssigBdg-totalAssigExpenses;

      %>
      <tr class="totals">
        <td width="160"><%=I18n.get("WORKLOG_ESTIMATION_FROM_ASSIGS")%></td>
        <td align="right"><%=DateUtilities.getMillisInHoursMinutes(totalEst)%></td>
        <td align="right"><%=DateUtilities.getMillisInHoursMinutes(totalDone)%></td>
        <td align="right"><%=totalEst > 0? "("+JSP.currency(totalCostWorklogEst* CompanyCalendar.MILLIS_IN_HOUR/(double)totalEst)+" "+I18n.get("AVERAGE_SHORT")+")":""%></td>
        <td align="right" id="gtEstimWl"><%=JSP.currency(totalCostWorklogEst)%></td>
        <td align="right" id="gtRealWl"><%=JSP.currency(totalCost)%></td>

        <td align="right" id="gtassBudget"><%=JSP.currency(totalAssigBdg)%></td>
        <td align="right" id="gtAssReal"><%=JSP.currency(totalAssigExpenses)%></td>
        <td align="right"><%=totalDelta >= 0 ? "" : "<font color=\"red\">"%><%=JSP.currency(totalDelta)%><%=totalDelta >= 0 ? "" : "</font>"%></td>
      </tr>
    </table>
<%
  }


  //----------------------------------------------------------- ADDITIONAL COSTS  ---------------------------------------------------------
  if (canSeeAdditionalCosts) {
    List<Cost> costs = new ArrayList(task.getCosts());
    Collections.sort(costs,new PlatformComparators.CreationDateComparator());
    %>

   <h2 class="sectionTitle"><%=I18n.get("COST_ADDITIONAL")%></h2>
    <table border="0" cellpadding="5" cellspacing="3" class="table" id="tableAddCosts">
      <tr>
        <th class="tableHead"><%=I18n.get("DESCRIPTION")%></th>
        <th class="tableHead"><%=I18n.get("COST_CLASSIFICATION")%></th>
        <th class="tableHead"><%=I18n.get("DATE")%></th>
        <th  style="text-align: right" class="tableHead"><%=I18n.get("COST_ESTIMATED")%></th>
        <th  style="text-align: right" class="tableHead"><%=I18n.get("COST_DONE")%></th>
        <th  style="text-align: right" class="tableHead"><%=I18n.get("WORKLOG_DELTA")%></th>
      </tr>
      <%
        double totalEstAdd = 0;
        double totalRealAdd = 0;
        for (Cost cost : costs) {
          totalEstAdd += cost.getEstimatedCost();
          totalRealAdd += cost.getRealCost();
          %>
          <tr>
            <td align="left"><%=JSP.w(cost.getDescription())%></td>
            <td align="left"><%=cost.getClassification() != null ? cost.getClassification().getName() : ""%></td>
            <td align="right" ><%=JSP.w(cost.getCreationDate())%></td>
            <td align="right" class="addCstEstim"><%=JSP.currency(cost.getEstimatedCost())%></td>
            <td align="right" class="addCstReal"><%=JSP.currency(cost.getRealCost())%></td>
            <td align="right"><font color="<%=cost.getEstimatedCost()-cost.getRealCost() >= 0 ? "" :"red"%>"><%=JSP.currency(cost.getEstimatedCost()-cost.getRealCost())%></font></td>

          </tr>
          <%
        }
      %>
      <tr height="1"></tr>
      <%
        //eventually loop on child costs
        List<Task> picciriddi = task.getDescendants();
        for (Task t : picciriddi) {
          //loop on persisted costs
          List<Cost> chCosts = new ArrayList(t.getCosts());
          Collections.sort(chCosts,new PlatformComparators.CreationDateComparator());
          for (Cost cost : chCosts) {
            totalEstAdd += cost.getEstimatedCost();
            totalRealAdd += cost.getRealCost();
            %>
            <tr>
              <td align="left"><%=JSP.w(cost.getDescription())%></td>
              <td align="left"><%=cost.getClassification() != null ? cost.getClassification().getName() : ""%></td>
              <td align="right"><%=JSP.w(cost.getCreationDate())%></td>
              <td align="right"><%=JSP.currency(cost.getEstimatedCost())%></td>
              <td align="right"><%=JSP.currency(cost.getRealCost())%></td>
              <td align="right"><font color="<%=cost.getEstimatedCost()-cost.getRealCost() >= 0 ? "" :"red"%>"><%=JSP.currency(cost.getEstimatedCost()-cost.getRealCost())%></font></td>

            </tr>
            <%
          }
        }
        %>
        <tr class="totals">
          <td align="center" colspan="3">&nbsp;</td>
          <td align="right" id="addCostEstimTot"><%=JSP.currency(totalEstAdd)%></td>
          <td align="right" id="addCostRealTot"><%=JSP.currency(totalRealAdd)%></td>
          <td align="right"><font color="<%=totalEstAdd-totalRealAdd >= 0 ? "" :"red"%>"><%=JSP.currency(totalEstAdd-totalRealAdd)%></font></td>

        </tr>
    </table>
<%
  }
%>


<div id="granTotale" >

  <table border="0" cellpadding="3" cellspacing="3" style="border: 1px solid #eee; background-color: #fefefe; padding: 10px;">
    <tr class="budgetRow">
      <td></td>
      <td align="right"><%=I18n.get("BUDGET")%></td>
      <td>&nbsp;</td>
      <td align="right"><%=I18n.get("PLANNED_COST_TOTAL")%></td>
      <td>&nbsp;</td>
      <td align="right"><%=I18n.get("ESTIMATED_BUDGET_DELTA")%></td>
    </tr>
    <tr style="font-size:18px" class="budgetRow">
      <td ><%=I18n.get("COST_FORECAST")%></td>
      <td align="right" class="gtBudget"></td>
      <td align="center">-</td>
      <td align="right" class="gtEstimated"></td>
      <td align="center">=</td>
      <td align="right" class="gtEstimDeltaBudget"></td>
    </tr>
    <tr style="border-top:1px solid #cccccc;">
      <td></td>
      <td><%=I18n.get("PLANNED_COST_TOTAL")%></td>
      <td>&nbsp;</td>
      <td align="right"><%=I18n.get("HOURLY_COST_TOTAL_DONE")%></td>
      <td>&nbsp;</td>
      <td align="right"><%=I18n.get("WORKLOG_DELTA")%></td>
    </tr>
    <tr style="font-size:18px;border-bottom:1px solid #cccccc;">
      <td><%=I18n.get("COST_PLANNED")%></td>
      <td align="right" class="gtEstimated"></td>
      <td align="center" style="width: 50px">-</td>
      <td align="right" class="gtReal"></td>
      <td align="center" style="width: 50px">=</td>
      <td align="right" class="gtDelta"></td>
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
      <td ><%=I18n.get("COST_ACTUAL")%></td>
      <td align="right" class="gtBudget"></td>
      <td align="center">-</td>
      <td align="right" class="gtReal"></td>
      <td align="center">=</td>
      <td align="right" class="gtDeltaBudget"></td>
    </tr>
  </table>
</div>

<script>
  function recomputeGranTotale() {
    var bdg = getCurrencyValue($("#COST_ESTIMATED").text());
    var estim = getCurrencyValue($("#addCostEstimTot").text()) + getCurrencyValue($("#gtEstimWl").text()) + getCurrencyValue($("#gtassBudget").text());
    var real = getCurrencyValue($("#addCostRealTot").text()) + getCurrencyValue($("#gtRealWl").text()) + getCurrencyValue($("#gtAssReal").text());

    //console.debug("recomputeGranTotale",bdg,estim,real);

    if (bdg) {
      $(".budgetRow").show();
      $(".gtBudget").html(formatCurrency(bdg));
      $(".gtDeltaBudget").html(formatCurrency(bdg - real)).css("color", bdg - real < 0 ? "rgb(182, 30, 45)" : "black");
    } else {
      $(".budgetRow").hide();
    }

    $(".gtEstimated").html(formatCurrency(estim));
    $(".gtEstimDeltaBudget").html(formatCurrency(bdg - estim)).css("color", bdg - estim < 0 ? "rgb(182, 30, 45)" : "black");
    $(".gtReal").html(formatCurrency(real));
    $(".gtDelta").html(formatCurrency(estim - real)).css("color", estim - real < 0 ? "rgb(182, 30, 45)" : "black");
  }
  recomputeGranTotale();
</script>


<%


  tabCosts.end(pageContext);
  }



  //----------------------------------------------------------- ISSUES ---------------------------------------------------------
    if (tabIssues!=null) {
      tabIssues.start(pageContext);

      String hql = "select issue from " + Issue.class.getName() + " as issue order by issue.gravity desc,issue.orderFactorByResource,issue.orderFactor";
      QueryHelper q = new QueryHelper(hql);
      q.addQBEClause("issue.task.id", "t", task.getId() + "", QueryHelper.TYPE_CHAR);
      List<Issue> issues = q.toHql().list();
      %>
      <table border="0" cellpadding="5" cellspacing="3" class="table">
      <tr>
        <th class="tableHead" width="100"><%=I18n.get("ISSUE_CODE")%></th>
        <th class="tableHead" width="100"><%=I18n.get("STATUS")%></th>
        <th class="tableHead" colspan="300"><%=I18n.get("DESCRIPTION")%></th>
        <th class="tableHead"><%=I18n.get("RESOURCE")%></th>
      </tr>
      <%
        for (Issue issue : issues) {
      %>
      <tr>
        <td style="border-bottom:1px dotted #999999;line-height:5px;" valign="top"><b>I#<%=issue.getMnemonicCode()%>#</b></td>
        <td style="border-bottom:1px dotted #999999;line-height:5px;" valign="top"><%=issue.getStatus()==null?"":issue.getStatus().getDescription()%></td>
        <td style="border-bottom:1px dotted #999999;" colspan="300" valign="top"><%=JSP.encode(issue.getDescription())%></td>
        <td style="border-bottom:1px dotted #999999;" nowrap valign="top" width="180"><%=issue.getAssignedTo() != null ? issue.getAssignedTo().getDisplayName() : ""%>&nbsp;  </td>
      </tr>
      <% }%>
      </table>
      <%
      tabIssues.end(pageContext);

    }



  //----------------------------------------------------------- SUBMIT TICKET ---------------------------------------------------------
  if (tabAddTicket !=null) {
    tabAddTicket.start(pageContext);

    %><h2 class="sectionTitle" style="padding-bottom: 10px"><%=I18n.get("SEND_YOUR_REQUEST")%></h2><%


  %><table class="table" cellpadding="5">
  <tr><td valign="top"><%
    TextField requesterName = new TextField("REQUESTER_NAME","<br>");
    requesterName.required=true;
    requesterName.fieldSize = 40;
    requesterName.label=I18n.get("REQUESTER_NAME");
    requesterName.toHtml(pageContext);
    %>
    </td><td valign="top"><%
    TextField requesterCompany = new TextField("REQUESTER_COMPANY","<br>");
    requesterCompany.fieldSize = 40;
    requesterCompany.label=I18n.get("REQUESTER_COMPANY");
    requesterCompany.toHtml(pageContext);
    %>
    </td></tr>
  <tr><td valign="top"><%
    TextField email = TextField.getEmailInstance("EXTERNAL_REQUESTER");
    email.required=true;
    email.separator="<br>";
    email.fieldSize = 40;
    email.label=I18n.get("YOUR_EMAIL");
    email.toHtml(pageContext);

  %><br><small class="">(<%=I18n.get("REQUEST_MAIL_HELP")%>)</small>
  </td>
<td valign="top"><%
  //---------------------------------------------------- FILE UPLOAD -----------------------------------------------------------------
  if(canAddFile){
    Uploader uploader = new Uploader("DOCUMENT_UPLOAD", pageState);
    uploader.label =  I18n.get("ADD_FILE_TO_PROPOSAL");
    uploader.separator = "<br>";
    uploader.size = 40;
    uploader.treatAsAttachment = false;
    uploader.toolTip = I18n.get("DOCUMENT_UPLOAD");
    uploader.toHtml(pageContext);

  }
%></td></tr>
  <tr><td colspan="2"><%

    TextArea ta = new TextArea("ISSUE_DESCRIPTION", "<br>", 20, 10, null);
    ta.label = I18n.get("POST_MESSAGE");
    ta.maxlength = 2000;
    ta.script = "style=width:100%";
    ta.required = true;
    ta.toHtml(pageContext);

%></td></tr>
  <tr><td colspan="2"><%

    //---------------------------------------------------- CUSTOM FIELDS  -----------------------------------------------------------------
  if (IssueBricks.hasCustomField() && canSeeIssuesCF) {
    %><div class="issueCustomFields clearfix"><%
    for (int i=1; i<7; i++) {
      DesignerField customField = DesignerField.getCustomFieldInstance( "ISSUE_CUSTOM_FIELD_",i, null,false, false,  false, pageState);
      if (customField!=null){
        customField.separator="<br>";
        %><div style="display:inline-block; margin-right: 5px" fieldName="ISSUE_CUSTOM_FIELD_<%=i%>"><%customField.toHtml(pageContext); %></div><%
      }
    }
    %></div><%
  }



    %></td></tr><tr><td colspan="2"><%
    if(!noCaptcha){
      RecaptchaV2 recaptcha = new RecaptchaV2("6LeGqCITAAAAANwJVFJhjMxha7m_OqJXs7U-Cbiz", "6LeGqCITAAAAAKfLkKiLYsvcEOwBGpGe1ciygt1j");
      recaptcha.toHtml(pageContext);

    }

    %></td></tr></table><%

    ButtonBar bb = new ButtonBar();
    ButtonSubmit save = ButtonSubmit.getSaveInstance(pageState.getForm(), I18n.get("SAVE_PROPOSAL"));
    save.variationsFromForm.setCommand("SEND");
    bb.addButton(save);
    bb.toHtml(pageContext);

    tabAddTicket.end(pageContext);
  }


  // si chiude il tabSet
  if (tabSet.tabs.size()>1)
    tabSet.end(pageContext);




%>