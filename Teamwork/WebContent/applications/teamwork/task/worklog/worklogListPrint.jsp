<%@ page import="com.twproject.resource.Resource, com.twproject.security.TeamworkPermissions, com.twproject.task.Assignment, com.twproject.task.Issue, com.twproject.task.Task, com.twproject.waf.TeamworkPopUpScreen,
                 com.twproject.worklog.Worklog, com.twproject.worklog.businessLogic.WorklogController, org.jblooming.agenda.CompanyCalendar, org.jblooming.designer.DesignerField, org.jblooming.operator.Operator,
                 org.jblooming.system.SystemConstants, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.SessionState, org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.display.Img, org.jblooming.waf.html.input.CheckField,
                 org.jblooming.waf.html.input.RadioButton,org.jblooming.waf.html.state.Form,org.jblooming.waf.settings.ApplicationState,org.jblooming.waf.settings.I18n,org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState, java.util.*, org.jblooming.security.License"%>
<%!
  public class CompByTask implements Comparator<Worklog> {

    public boolean compByRes=false;

    public int compare(Worklog t1, Worklog t2) {
     Task task2 = t2.getAssig().getTask();
      Task task1 = t1.getAssig().getTask();
      if (task1.equals(task2)) {
        Resource resource2 = t2.getAssig().getResource();
        Resource resource1 = t1.getAssig().getResource();
        if (compByRes && !resource2.equals(resource1))
           return resource1.getName().compareToIgnoreCase(resource2.getName());
        else
          return t1.getInserted().compareTo(t2.getInserted());
      } else
        return (task1.getCode()+task1.getName()).compareToIgnoreCase(task2.getCode()+task2.getName());
     }
  }
  public class CompByRes implements Comparator<Worklog> {

    public int compare(Worklog t1, Worklog t2) {
      Resource task2 = t2.getAssig().getResource();
      Resource task1 = t1.getAssig().getResource();
      if (task1.equals(task2))
        return t1.getInserted().compareTo(t2.getInserted());
      else
        return (task1.getName()).compareTo(task2.getName());
     }
  }
%>
<%

    PageState pageState = PageState.getCurrentPageState(request);
    SessionState sessionState = pageState.sessionState;


    if (!pageState.screenRunning) {
      pageState.screenRunning = true;

      final ScreenArea body = new ScreenArea(new WorklogController(), request);
      TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
      lw.register(pageState);
      pageState.perform(request, response).toHtml(pageContext);
    } else {


    PageSeed self = pageState.thisPage(request);
    self.setCommand(Commands.FIND);
    self.addClientEntries(pageState.getClientEntries());
    self.removeEntriesMatching("PRINT_");

    Form f = new Form(self);
    f.alertOnChange = false;
    pageState.setForm(f);
    f.start(pageContext);

    Operator op = pageState.getLoggedOperator();


    boolean ordByTask = "TASK".equals(pageState.getEntryOrDefault("PRINT_WORKLOG_ORDER","TASK").stringValue());
    boolean ordByRes = "RES".equals(pageState.getEntryOrDefault("PRINT_WORKLOG_ORDER","TASK").stringValue());
//    boolean ordByTask = pageState.getEntryOrDefault("PRINT_WORKLOG_ORDER_BY_TASK").checkFieldValue();
//    boolean ordByRes = pageState.getEntryOrDefault("PRINT_WORKLOG_ORDER_BY_RESOURCE").checkFieldValue();

    boolean hideRes = pageState.getEntryOrDefault("PRINT_WORKLOG_HIDE_ASSIGNEE").checkFieldValue(); //|| ordByTask;
    boolean hideIssue = pageState.getEntryOrDefault("PRINT_WORKLOG_HIDE_ISSUE").checkFieldValue();
    boolean hideCost = pageState.getEntryOrDefault("PRINT_WORKLOG_HIDE_COSTS").checkFieldValue();
    boolean hideWorklog = pageState.getEntryOrDefault("PRINT_WORKLOG_HIDE_DETAIL").checkFieldValue();

    List<Worklog> wklg = pageState.getPage().getAllElements();

    if (ordByRes && !ordByTask) {
      CompByRes c = new CompByRes();
      Collections.sort(wklg, c);
    } else if (ordByTask) {
      CompByTask c = new CompByTask();
      c.compByRes=ordByRes;
      Collections.sort(wklg, c);
    }



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
</table>

<div id="printFilter" style="visibility:visible;" class="noprint"> <%
  //filters

    Container filter = new Container("IP");
    filter.level = 2;
    filter.start(pageContext);

    {
      RadioButton showCh = new RadioButton(I18n.get("WORKLOG_ORDER_BY_TASK"),"PRINT_WORKLOG_ORDER", "TASK","", null,false,null);
      showCh.label= I18n.get("WORKLOG_ORDER_BY_TASK");
      showCh.toHtmlI18n(pageContext);
    }

    {
      RadioButton showCh = new RadioButton(I18n.get("WORKLOG_ORDER_BY_RESOURCE"),"PRINT_WORKLOG_ORDER", "RES","", null,false,null);
      showCh.label= I18n.get("WORKLOG_ORDER_BY_RESOURCE");
      showCh.toHtmlI18n(pageContext);
    }


    {
      CheckField showCh = new CheckField("PRINT_WORKLOG_HIDE_DETAIL", "", false);
      showCh.label= I18n.get("WORKLOG_HIDE_DETAIL");
      showCh.toHtmlI18n(pageContext);
    }


    {
      CheckField showCh = new CheckField("PRINT_WORKLOG_HIDE_ASSIGNEE", "", false);
      showCh.label= I18n.get("WORKLOG_HIDE_ASSIGNEE");
      showCh.toHtmlI18n(pageContext);
    }

    {
      CheckField showCh = new CheckField("PRINT_WORKLOG_HIDE_ISSUE", "", false);
      showCh.label= I18n.get("WORKLOG_HIDE_ISSUE");
      showCh.toHtmlI18n(pageContext);
    }

    {
      CheckField showCh = new CheckField("PRINT_WORKLOG_HIDE_COSTS", "", false);
      showCh.label= I18n.get("WORKLOG_HIDE_COSTS");
      if (op.hasPermissionFor(TeamworkPermissions.task_cost_canRead))
        showCh.toHtmlI18n(pageContext);
    }

    %>&nbsp;&nbsp;&nbsp;<%
    ButtonSubmit sc = new ButtonSubmit(f);
    sc.label = I18n.get("REFRESH");
    sc.additionalCssClass="small";
    sc.toHtml(pageContext);

    filter.end(pageContext);

  %></div> <%



  int totCol = 11 - (License.assertLevel(20)?0:1) - ( (hideRes ? 1 : 0) + (hideIssue ? 1 : 0) + (ordByRes||ordByTask ? 1 : 0)) + (hideCost ? -2 : 0);


/*________________________________________________________________________________________________________________________________________________________________________


  results

________________________________________________________________________________________________________________________________________________________________________

*/

    %><table border="0" width="100%" align="center" cellpadding="5" cellspacing="0" style="font-size: 70%;" class="table"><tr>
        <th class="tableHead"><%="id"%></th>

      <th class="tableHead"><%=I18n.get("WORKLOG_DATE")%></th>
      <%if (License.assertLevel(20)){%>
      <th class="tableHead"><%=I18n.get("WORKLOG_STATUS")%></th>
      <%}%>
      <%if (!ordByTask){%>
        <th class="tableHead"><%=I18n.get("ASSIGNMENT")%></th>
      <%}%>
      <%if (!hideRes && (!ordByRes || ordByTask)){%>
        <th class="tableHead"><%=I18n.get("ASSIGNEE")%></th>
      <%}%>
      <%if (!hideIssue){%>
        <th class="tableHead"><%=I18n.get("ISSUE")%></th>
      <%}%>

      <th class="tableHead"><%=I18n.get("WORKLOG_ACTION")%></th>
      <th class="tableHead"><%=I18n.get("WORKLOG_DURATION")%></th>
      <%if (op.hasPermissionFor(TeamworkPermissions.task_cost_canRead)&& !hideCost) {%>
        <th class="tableHead"><small><%=I18n.get("HOURLY_COST_FROM_ASSIGNMENT")%></small></th>
        <th class="tableHead"><%=I18n.get("COMPUTED_COST")%></th>
      <%}%>
      </tr><%

      long totWL=0;
      double totCostFromAssig=0;
      long partialWL=0;
      double partialCostFromAssig=0;
      long estimFromAssig=0;
      long totEstimFromAssig = 0;
      double inputtedHourlyCost = pageState.getEntry("HOURLY_COST").doubleValueNoErrorNoCatchedExc();
      boolean costIsFromAssig = inputtedHourlyCost==0;

      PageSeed wlw=pageState.pageInThisFolder("worklogWeek.jsp",request);

        if (wklg != null && wklg.size() > 0) {

          Task currTask = null;
          Assignment curAssignment = null;

          Set<Assignment> touchedAssigs=new HashSet();
          for (Worklog workLog : wklg) {

            if (curAssignment==null  || !curAssignment.equals(workLog.getAssig())) {
              curAssignment = workLog.getAssig();
              if (!touchedAssigs.contains(curAssignment)) {
                long totalWorklogEstimated = curAssignment.getEstimatedWorklog();
                estimFromAssig = estimFromAssig + totalWorklogEstimated;
                totEstimFromAssig = totEstimFromAssig + totalWorklogEstimated;
                touchedAssigs.add(curAssignment);
              }
            }

/*
________________________________________________________________________________________________________________________________________________________________________


task header/footer

________________________________________________________________________________________________________________________________________________________________________

*/
            if (ordByTask) {
              if (currTask ==null || !currTask.equals(workLog.getAssig().getTask())) {
                //footer
                if (currTask !=null) {
                  %><tr>
                  <td align="right" colspan="<%=totCol-2+(hideCost ? 0 : -2)%>">
                    <i><%=I18n.get("WORKLOG_PARTIAL_TOTAL")%>: (est.<%=DateUtilities.getMillisInHoursMinutes(estimFromAssig)%>)</i></td>
                  <td align="right" colspan="1"><%=DateUtilities.getMillisInHoursMinutes(partialWL)%></td>
                  <% if (op.hasPermissionFor(TeamworkPermissions.task_cost_canRead) && !hideCost) {
                    %><td align="right" colspan="2"><%=JSP.currency(partialCostFromAssig)%></td><%
                  }%>
                  </tr> <%
                  partialWL =0;
                  partialCostFromAssig=0;
                  estimFromAssig = 0;
                }
                //header
                currTask = workLog.getAssig().getTask();
                 %><tr><td colspan="<%=totCol%>" align="left"  class="tableSection">
                   <div class="childNode"><h2><%=currTask.getDisplayName()%></h2></div>
                 </td></tr><%
              }
            }
/*
________________________________________________________________________________________________________________________________________________________________________


worklog line

________________________________________________________________________________________________________________________________________________________________________

*/
            totWL = totWL + workLog.getDuration();
            partialWL = partialWL + workLog.getDuration();

            Assignment a = workLog.getAssig();
            if (!hideWorklog) {
            %><tr><%

            %><td align="center"><small><%=workLog.getId()%></small></td><%

            %><td nowrap><%=DateUtilities.dateToFullString(workLog.getInserted())%></td> <%
            %><%if (License.assertLevel(20)){%><td nowrap align="center"><%=workLog.bricks.drawStatus(pageState)%></td><%}%> <%

            if (!ordByTask) {
            %><td>
              <%if (I18n.isActive("CUSTOM_FEATURE_ALWAYS_SHOW_TASK_PATH")){
                %><div class="pathSmall"><%=a.getTask().getPath(" / ", false)%></div><%
              }%>

              <%=hideRes ? a.getTask().getName() : a.getDisplayNameWithResource()%>
            </td><%
            }

            if (!hideRes && (!ordByRes || ordByTask) ) {
              %><td><%=JSP.w(a.getResource().getDisplayName())%></td><%
            }

            if (!hideIssue) {
              Issue i = null;
              if (workLog.getIssue()!=null)
                      i = workLog.getIssue();
              if (i!=null) {
              %><td align="center"><%=JSP.encode(i.getDescription())%></td><%
              } else {
              %><td align="center" >-</td><%
              }
            }
            %> <td align="left" ><%=workLog.getAction()!=null && workLog.getAction().trim().length()>0 ? workLog.getAction() : "&nbsp;"%>
                  <div style="font-size: 80%;"><%
                    for (int i=1; i<5; i++) {
                      DesignerField dfStr = DesignerField.getCustomFieldInstance( "WORKLOG_CUSTOM_FIELD_",i, workLog ,true,true, false, pageState);
                      if (dfStr!=null){
                        dfStr.separator="&nbsp;";
                        dfStr.toHtml(pageContext);
                  %>&nbsp;&nbsp;&nbsp;<%
                      }
                    }
                  %></div>

              </td>
              <td align="right"><%=DateUtilities.getMillisInHoursMinutes(workLog.getDuration())%></td> <%
              if (op.hasPermissionFor(TeamworkPermissions.task_cost_canRead) && !hideCost ) {
                %> <td align="right"><%=costIsFromAssig ? JSP.currency(a.getHourlyCost()) : JSP.currency(inputtedHourlyCost)%></td> <%
              }
            }
              double rowCost = 0;
              if (costIsFromAssig)
                rowCost = ((double)workLog.getDuration()/ CompanyCalendar.MILLIS_IN_HOUR) * a.getHourlyCost();
              else
                rowCost = ((double)workLog.getDuration()/ CompanyCalendar.MILLIS_IN_HOUR) * inputtedHourlyCost;
              totCostFromAssig = totCostFromAssig + rowCost;
              partialCostFromAssig = partialCostFromAssig + rowCost;
             if (op.hasPermissionFor(TeamworkPermissions.task_cost_canRead) && !hideWorklog && !hideCost) {
              %> <td align="right"><%=JSP.currency(rowCost)%></td> <%
              %></tr><%
            }
          }
/*
________________________________________________________________________________________________________________________________________________________________________


last task footer

________________________________________________________________________________________________________________________________________________________________________

*/
          if (currTask !=null) {
                  %><tr>
                  <td valign="top" style="border-top:1px solid #808080;" align="right" colspan="<%=totCol-2+(hideCost ? 0 : -2)%>">
                    <i><%=I18n.get("WORKLOG_PARTIAL_TOTAL")%>: (est.<%=DateUtilities.getMillisInHoursMinutes(estimFromAssig)%>hh)</i></td>
                  <td valign="top" style="border-top:1px solid #808080;" align="right" colspan="1"><%=DateUtilities.getMillisInHoursMinutes(partialWL)%></td>
                  <% if (op.hasPermissionFor(TeamworkPermissions.task_cost_canRead) && !hideCost) {
                    %><td valign="top" style="border-top:1px solid #808080;" align="right" colspan="2"><%=JSP.currency(partialCostFromAssig)%></td><%
                  } %>
                  </tr> <%
          }

/*
________________________________________________________________________________________________________________________________________________________________________


totals

________________________________________________________________________________________________________________________________________________________________________

*/
        %>

          <tr>
            <td style="border-top:1px solid #808080;" align="right" colspan="<%=totCol-2+(hideCost ? 0 : -2)+3%>">
              <i><%=I18n.get("WORKLOG_ESTIMATED_TOTAL")%> (hh:mm):</i>&nbsp;
            <%=DateUtilities.getMillisInHoursMinutes(totEstimFromAssig)%>&nbsp;          
                <i>(dd-hh:mm):</i>
            <%=DateUtilities.getMillisInDaysWorkHoursMinutes(totEstimFromAssig)%></td>
          </tr>

          <tr>
            <td style="border-top:1px solid #808080;" align="right" colspan="<%=totCol-2+(hideCost ? 0 : -2)%>">
              <i><%=I18n.get("WORKLOG_TOTAL")%> (hh:mm):</i>
            </td>
            <td style="border-top:1px solid #808080;" align="right" colspan="1"><b><%=DateUtilities.getMillisInHoursMinutes(totWL)%></b></td>
            <% if (op.hasPermissionFor(TeamworkPermissions.task_cost_canRead) && !hideCost) {
               %><td style="border-top:1px solid #808080;" align="right" colspan="2"><b><%=JSP.currency(totCostFromAssig)%></b></td><%
            } %>

            </tr>

            <tr>
            <td  align="right" colspan="<%=totCol-2+(hideCost ? 0 : -2)%>"><i><%=I18n.get("WORKLOG_TOTAL")%> (dd-hh:mm):</i></td>
            <td  align="right" colspan="1"><b><%=DateUtilities.getMillisInDaysWorkHoursMinutes(totWL)%></b></td>
            <% if (op.hasPermissionFor(TeamworkPermissions.task_cost_canRead) && !hideCost) {
               %><td  align="center" colspan="2">&nbsp;</td><%
            } %>

            </tr> <%

      } else {
      %> <tr><td colspan="99"><%=I18n.get("NO_WORKLOGS")%></td></tr><%
      }
    %></table><%

      f.end(pageContext);
  }
%>