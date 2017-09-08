<%@ page import="com.twproject.plan.PlanUtilities, com.twproject.resource.Resource, com.twproject.task.Assignment, com.twproject.task.TaskBricks, org.jblooming.agenda.Period, org.jblooming.persistence.PersistenceHome, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.*, com.twproject.utilities.TeamworkComparators, com.twproject.security.TeamworkPermissions" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);


  String resId = pageState.getEntry("resId").stringValueNullIfEmpty();
  int day= pageState.getEntry("day").intValueNoErrorCodeNoExc();

  if (JSP.ex(resId) && day > 0) {
    Date theDay=DateUtilities.intToDate(day);


    Resource resource = (Resource) PersistenceHome.findByPrimaryKey(Resource.class,resId);
  %><div><%=I18n.get("ASSIGNMENTS_OF_%%_ON_%%", resource.getDisplayName(),DateUtilities.dateToFullString(theDay))%></div>
<br>
<div style="overflow-y:auto;max-height:400px"><%

  Period period = Period.getDayPeriodInstance(theDay);

  PlanUtilities planUtilities = new PlanUtilities();

  // si sovrascrive il default: se siamo per il piano si usa quello di sistema, sul load si usa quello da checkbox
  if ("plan".equalsIgnoreCase(pageState.getEntry("forPage").stringValueNullIfEmpty()))
    planUtilities.ignoreWorklogWhenComputingLoad=I18n.isActive("CUSTOM_FEATURE_OPLOAD_IGNORE_WORKLOG");
  else
    planUtilities.ignoreWorklogWhenComputingLoad=pageState.getEntryOrDefault("OPLOAD_IGNORE_WORKLOG","NO").checkFieldValue();

  PlanUtilities.ResourceLoad loadPlan = planUtilities.getTotalLoadAndWork(resource, period);

  List<Assignment> resAssigs = new ArrayList();
  resAssigs.addAll(loadPlan.assignementsDetailLine.keySet());
  Collections.sort(resAssigs, new TeamworkComparators.AssignmentByPriority(theDay));

%><table width="100%" cellspacing="0" cellpadding="4" class="edged table">
  <tr>
    <th class="tableHead" colspan="3"><%=I18n.get("TASK")%></th>
    <th class="tableHead"><%=I18n.get("ASSIGNMENT_PRIORITY_SHORT")%></th>
    <th class="tableHead" style="text-align: right"><%=I18n.get("WORKLOG_DONE_SHORT")%></th>
    <th class="tableHead" style="text-align: right"><%=I18n.get("WORKLOG_DURATION")%></th>
  </tr>
<%
  long t0=0;
  long t1=0;
  boolean somethingScazzed=false;


  boolean hide0=I18n.isActive("CUSTOM_FEATURE_HIDE_LOAD_DETAIL_IF_0");
  PageSeed editTask= pageState.pageFromRoot("task/taskAssignmentList.jsp");
  editTask.command= Commands.FIND;

  for (Assignment ass:resAssigs){
    boolean toMuchPlan=false;
    Map<Integer,PlanUtilities.AssigLoadAndWork> assDetail = loadPlan.assignementsDetailLine.get(ass);
    if (assDetail!=null){
      PlanUtilities.AssigLoadAndWork loads= assDetail.get(day);
      if (loads!=null ){//&& (loads[0]>0 || loads[2]>0) ){
        toMuchPlan=toMuchPlan|| loads.workLoad<(loads.getTotalPlanned()) && (loads.getTotalPlanned())>0;
        t0+=loads.workLoad;
        t1+=loads.workDone;

        //editTask.mainObjectId=ass.getTask().getId();
        editTask.addClientEntry("TASK_ID",ass.getTask());


        if (loads.workLoad>1000*60 ||loads.workDone>0 || !hide0){
          ButtonLink taskLink = ButtonLink.getTextualInstance(ass.getTask().getName(), editTask);
          boolean canRead = ass.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.task_canRead);
          if (!canRead){
            taskLink.enabled=false;
            taskLink.label="----------------";
          }


          %><tr>
  <td class="columnTaskName">
    <%if (I18n.isActive("CUSTOM_FEATURE_ALWAYS_SHOW_TASK_PATH")){
    %><div class="pathSmall"><%=ass.getTask().getPath(" / ", false)%></div><%
    }%>
    <%taskLink.toHtml(pageContext);%>
  </td>
  <td class='textSmall columnTaskCode'><span><%=canRead?JSP.w(ass.getTask().getCode()):"----"%></span></td>
  <td class='textSmall'><%= ass.getRole().getCode() %></td>
  <td width="1%" style="vertical-align: middle"><%TaskBricks.assignmentPriorityToHtml(15, theDay.getTime(), ass, pageContext);%></td>
  <td align="right" title="<%=I18n.get("WORKLOG_DONE")%>"><%=DateUtilities.getMillisInHoursMinutes(loads.workDone)%></td>

  <td style="width:50px;text-align:right;" class="<%=toMuchPlan?"overPlanned":""%>"><%=DateUtilities.getMillisInHoursMinutes(loads.workLoad)%></td>
            </tr>
          <%
        }
      }
    }
    somethingScazzed=somethingScazzed|| toMuchPlan;
  }
  %><tr class="totals">
        <td colspan="4" align="right"><%=I18n.get("TOTAL")%></td>
        <td style="text-align:right;"><%=DateUtilities.getMillisInHoursMinutes(t1)%></td>
        <td style="text-align:right;"><%=DateUtilities.getMillisInHoursMinutes(t0)%></td>
      </tr>
  <%
    if (loadPlan.resourceWorkPower.get(day)!=null ){
      long wmil=loadPlan.resourceWorkPower.get(day);
      if (wmil<-1 || wmil<resource.getWorkDailyCapacity()){
        long vacMill= wmil<-1?resource.getWorkDailyCapacity():resource.getWorkDailyCapacity()-wmil;
        %><tr style="background-color:<%=pageState.getColor("NOT_AVAILABLE_BACKGROUND")%>"><td colspan="2" align="right"><%=I18n.get("PLAN_IN_VACATION_LEGENDA")%></td><td style="width:50px;text-align:right;"><%=DateUtilities.getMillisInHoursMinutes(vacMill)%></td> </tr><%
      }
    }
  %></table></div><%

    if (somethingScazzed){

      %><table><tr><td><div style="height:20px;width:30px;background-color:orange;">&nbsp;</div></td><td>
        <b><%=I18n.get("PLAN_ESTIMATION_MISMATCH")%></b></td></tr></table>

      <%


    }

  } else {
    %>Invalid data!<%
  }

%>
