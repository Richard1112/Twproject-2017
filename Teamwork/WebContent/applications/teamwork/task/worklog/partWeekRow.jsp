<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.task.Assignment, com.twproject.task.Task, com.twproject.task.TaskStatus, org.jblooming.agenda.CompanyCalendar, org.jblooming.agenda.Period, org.jblooming.agenda.Scale, org.jblooming.ontology.Pair, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jblooming.waf.view.ClientEntry, com.twproject.security.TeamworkPermissions" %>
<%
PageState pageState = PageState.getCurrentPageState(request);

  TeamworkOperator logged= (TeamworkOperator) pageState.getLoggedOperator();

  JspHelper includer = (JspHelper) JspHelper.getCurrentInstance(request);
  Scale scale=(Scale) includer.parameters.get("scale");
  Assignment ass=  (Assignment) includer.parameters.get("ass");
  Task task= ass.getTask();

  PageSeed assList = pageState.pageFromRoot("task/worklog/worklogAssignmentList.jsp");
  assList.addClientEntry("ASS_ID", ass.getId());
  assList.command=Commands.FIND;

  PageSeed editTask = pageState.pageFromRoot("/task/taskAssignmentList.jsp");
  editTask.setCommand(Commands.FIND);
  ButtonLink editTaskB = new ButtonLink("", editTask);
  editTask.addClientEntry("TASK_ID",task.getId());

  // se la custom feature è abilitata non puoi mettere wl nel passato
  long notBeyond=Long.MIN_VALUE;
  if (I18n.isActive("CUSTOM_FEATURE_DO_NOT_ACCEPT_OLD_WORLOG")){
    int days=new ClientEntry("dummy",I18n.get("CUSTOM_FEATURE_DO_NOT_ACCEPT_OLD_WORLOG")).durationInWorkingDaysNoErrorNoCatchedExc(false);
    notBeyond=days>0?System.currentTimeMillis()-days*CompanyCalendar.MILLIS_IN_DAY:notBeyond;
  }

%><tr assId="<%=ass.getId()%>" class="<%=ass.isEnabled()?"":"assDisabled"%>">
    <td class="columnTaskName" title="<%=ass.getDisplayNameFull()%>">
    <%
      if (I18n.isActive("CUSTOM_FEATURE_ALWAYS_SHOW_TASK_PATH")){
        %><div class="pathSmall"><%=task.getPath(" / ", false)%></div><%
      }

    editTaskB.label = JSP.w(task.getName());
    editTaskB.toHtmlInTextOnlyModality(pageContext);

  %></td>
  <td class='textSmall columnTaskCode'><span><%=JSP.w(task.getCode())%></span></td>
  <td class='textSmall'><%= ass.getRole().getCode() %></td>
    <td align="center">
    <span class="teamworkIcon small wlDetail" style="cursor:pointer;" onclick="openBlackPopup('<%=assList.toLinkToHref()%>','1180px','768px');" title="<%=I18n.get("HIS_WORKLOGLIST")%>" >A</span><%
    if (I18n.isActive("CUSTOM_FEATURE_SHOW_EXP_ON_TIMESHEET") && ( task.hasPermissionFor(logged,TeamworkPermissions.task_cost_canRead) ||  task.hasPermissionFor(logged,TeamworkPermissions.assignment_canCRW)|| task.hasPermissionFor(logged, TeamworkPermissions.expense_manage) || ass.getResource().equals(logged.getPerson()))){
      PageSeed exp=pageState.pageFromRoot("task/financial/taskAssignmentCosts.jsp");
      exp.mainObjectId=ass.getId();
      exp.addClientEntry("DATE",JSP.w(scale.startPointDate)+":"+JSP.w(scale.endPointDate));

  %><span class="teamworkIcon small lreq20 lreqHide" style="cursor:pointer;" onclick="openBlackPopup('<%=exp.toLinkToHref()%>','1180px','768px');" title="<%=I18n.get("ASSIG_EXPENSES")%>">$</span><%
    }

  %></td><%

    CompanyCalendar cc=new CompanyCalendar(scale.startPointTime);
      int i=1;
      while (cc.getTimeInMillis() <scale.endPointTime) { //---------------------------------------- loop on days


        // count total wl per day and wl entries
        long wlid=0; // worklog in day
        int wleid=0; // worklog entries in day
        Pair<Integer,Long> wls=ass.getWorklogDone(Period.getDayPeriodInstance(cc.getTime()));
        wleid=wls.first;
        wlid=wls.second;

        boolean isActiveToday = task.getSchedule() != null &&
          task.getSchedule().getStartDate() != null &&
          task.getSchedule().getEndDate() != null &&
          task.getSchedule().getStartDate().getTime() <= cc.getTimeInMillis() &&
          task.getSchedule().getEndDate().getTime() > cc.getTimeInMillis()&&
          TaskStatus.STATUS_ACTIVE.equals(task.getStatusOn(cc.getTime()));


        boolean disabledInThePast= cc.getTimeInMillis()<notBeyond || cc.getTimeInMillis()>System.currentTimeMillis()+CompanyCalendar.MILLIS_IN_WEEK;

        //----------------------------------------------------------------------------------------------  START DRAWING CELL ---------------------------------------------------------------------------------

        %><td nowrap class="day <%=cc.isWorkingDay()?"":"dayH"%> <%=cc.isToday()?"dayT":""%> <%=!isActiveToday?"inactive":""%> wlDayCell" day="<%=JSP.w(cc.getTime())%>" title="<%=!isActiveToday?I18n.get("ASSIGNMENT_NOT_ACTIVE_THIS_DAY"):""%>">
            <%--<input type="text" entrytype="DURATIONMILLIS" oldvalue="1" <%=disabledInThePast?"disabled":""%> value="" maxlength="5" class="cell wlTime validated" size="5"  wlid="" id="<%=ass.getId()+"-"+i++%>" minValue="0" >--%>
            <span class="wlmill" title="<%=wleid>0?wleid + " records.":""%>"><%=wleid>0?"&nbsp;"+DateUtilities.getMillisInHoursMinutes(wlid)+"&nbsp;": "" %></span><%

      //----------------------------------------------------------------------------------------------  END DRAWING CELL ---------------------------------------------------------------------------------

      %>
            <%--</div>--%>
  </td><%
      cc.add(CompanyCalendar.DATE, 1);
    }
  %><td align="right" class="totByRow">&nbsp;</td></tr>
