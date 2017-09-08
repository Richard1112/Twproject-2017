<%@ page import="com.twproject.security.TeamworkPermissions, com.twproject.task.Assignment, com.twproject.worklog.Worklog, com.twproject.worklog.businessLogic.WorklogBricks, org.jblooming.agenda.CompanyCalendar,
org.jblooming.operator.Operator, org.jblooming.utilities.DateUtilities, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.core.JspHelper,
org.jblooming.waf.html.input.ColorValueChooser, org.jblooming.waf.html.input.TextField, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.ArrayList, java.util.Hashtable, java.util.List, java.util.Map, org.jblooming.utilities.JSP, com.twproject.operator.TeamworkOperator, org.jblooming.waf.html.input.TextArea" %><%


  PageState pageState = PageState.getCurrentPageState(request);
  JspHelper includer = (JspHelper) JspHelper.getCurrentInstance(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  long minMillisInBar=  (Long) includer.parameters.get("minMillisInBar");
  boolean hideHolidays=  (Boolean) includer.parameters.get("hideHoly");
  boolean allowBulk=  (Boolean) includer.parameters.get("allowBulkOperations");
  String type=  (String) includer.parameters.get("type");

  Map<Assignment, Map<Integer,List<Worklog>>> plan = (Map<Assignment, Map<Integer,List<Worklog>>>) includer.parameters.get("plan");


  Assignment assig=  (Assignment) includer.parameters.get("assig");


%>
<tr class="rowAppr" assId="<%=assig.getId()%>" >

<td class="day" valign="top"><%if (allowBulk){%><input type="checkbox" onclick="rowSel(this)"><%}else{%>&nbsp;<%}%></td>

  <%
    if ("TASK".equals(type)){
      // make the resource approval link
      PageSeed ps= pageState.pageInThisFolder("worklogApprovalByResource.jsp",request);
      ps.addClientEntry("RES_ID", assig.getResource().getId());
      ps.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));
      ButtonLink bl = ButtonLink.getTextualInstance(assig.getResource().getDisplayName(), ps);

      %><td valign="top" class="day columnTaskName" title="<%=assig.getDisplayNameWithResource()%>"><%bl.toHtmlInTextOnlyModality(pageContext);%></td>
      <td valign="top" class='textSmall'><%= assig.getRole().getCode() %></td><%


} else {
      // make the task approval link
      PageSeed ps= pageState.pageInThisFolder("worklogApprovalByTask.jsp",request);
      ps.addClientEntry("TASK_ID", assig.getTask().getId());
      ps.command="dummy";
      ps.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));
      ButtonLink bl = ButtonLink.getTextualInstance(assig.getTask().getName(), ps);
  %>
  <td valign="top" class="day columnTaskName" title="<%=assig.getDisplayName()%>"><%bl.toHtmlInTextOnlyModality(pageContext);%></td>
  <td valign="top" class='textSmall columnTaskCode'><span><%=JSP.w(assig.getTask().getCode())%></span></td>
  <td valign="top" class='textSmall'><%= assig.getRole().getCode() %></td><%
    }

  %>
<%


  // get the week wls for row
  Map<Integer,List<Worklog>> rowWls = plan.get(assig);
  if (rowWls ==null) {
    rowWls =new Hashtable<Integer, List<Worklog>>();
  }

  // test security
  boolean canChangeStatus=assig.hasPermissionFor(logged, TeamworkPermissions.worklog_manage);



  ColorValueChooser stChooser=WorklogBricks.getStatusChooser("",false,pageState);
  stChooser.displayValue=false;
  stChooser.showOpener=true;
  stChooser.width=15;
  stChooser.height = 15;
  stChooser.onChangeScript="hidden.each(changeStatus);";


  // iterate for 7 days
  CompanyCalendar cc= new CompanyCalendar();
  cc.setTimeInMillis(minMillisInBar);
  for (int i = 1; i <= 7; i++) {
    // wich day of week?
    int nday=cc.get(CompanyCalendar.DAY_OF_WEEK);

     %><td valign="top" day="<%=nday%>" class="day cellDrop <%=cc.isToday() ?"dayT" : ""%> <%=!cc.isWorkingDay() ?"dayH" : ""%>"><%

    if (hideHolidays && cc.isWorkingDay() || !hideHolidays){
      List<Worklog> wlPerDayPerRow= rowWls.get(nday);
      if (wlPerDayPerRow==null)
        wlPerDayPerRow=new ArrayList<Worklog>();

      // iterate for the wl
      for (Worklog wl : wlPerDayPerRow) {
        boolean canWrite =wl.bricks.canWrite(logged);
        %><table width="100%" wlId="<%=wl.getId()%>" cellpadding="0" cellspacing="1" class="wldrag<%=assig.getId()%> <%=canWrite?"isDrag":""%>">
          <tr>
            <td class="dragHandler" rowspan="2" valign="top" width="10"><%if (allowBulk){%><input type="checkbox" name="ckwl_<%=wl.getId()%>"><%}else{%>&nbsp;<%}%></td>
            <td width="1%"><%
              String fieldCount=wl.getId()+"";
              pageState.addClientEntry("s_" + fieldCount,wl.getStatus()!=null?wl.getStatus().getId():"");
              stChooser.fieldName="s_" + fieldCount;
              stChooser.readOnly=!canChangeStatus;
              stChooser.toHtml(pageContext);
            %></td><td width="1%" nowrap><%
                pageState.addClientEntry("wl_" + fieldCount,DateUtilities.getMillisInHoursMinutes(wl.getDuration()));
                TextField tfDur = TextField.getDurationInMillisInstance("wl_" + fieldCount);
                tfDur.separator="";
                tfDur.label = "";
                tfDur.maxlength=5;
                tfDur.fieldSize=3;
                tfDur.minValue="0";
                tfDur.fieldClass=tfDur.fieldClass+" wlTime";
                tfDur.readOnly=!canWrite;
                tfDur.toHtml(pageContext);

                %></td><td align="right"><%

                  ButtonJS delData = new ButtonJS("removeWorklog(this,'"+wl.getId()+"');");
                  delData.confirmRequire = true;
                  delData.confirmQuestion = I18n.get("CONFIRM_DELETE_WKL");
                  delData.enabled=canWrite;
                  delData.iconChar="<small>d</small>";
                  delData.additionalCssClass="delete";
                  delData.label="";
                  delData.toHtmlInTextOnlyModality(pageContext);


                %></td>
            </tr>
            <tr>
              <td align="center" colspan="3" style="padding: 0;margin:0"><%

                pageState.addClientEntry("d" + tfDur.id,wl.getAction());
                TextArea tfDescr = new TextArea("d" + tfDur.id, "",1,1,"");
                tfDescr.label = "";
                tfDescr.maxlength=1900;
                //tfDescr.fieldSize=5;
                tfDescr.fieldClass=tfDescr.fieldClass+"formElements wlDescr";
                tfDescr.readOnly=!canWrite;
                tfDescr.script="style='font-size:10px; padding: 1px; width:100%; height:25px;'";
                tfDescr.toHtml(pageContext);


              %></td>
            </tr><%
            %>
        </table><%

      }

    } else {
      %>&nbsp;<%
    }
    %></td><%
    cc.add(CompanyCalendar.DAY_OF_MONTH, 1);    
  }
%></tr>
