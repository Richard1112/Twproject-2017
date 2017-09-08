<%@ page import="com.twproject.task.Assignment, com.twproject.task.Task, com.twproject.task.businessLogic.TaskController,
com.twproject.waf.TeamworkPopUpScreen, com.twproject.worklog.Worklog, org.jblooming.agenda.CompanyCalendar,
org.jblooming.oql.OqlQuery, org.jblooming.persistence.PersistenceHome, org.jblooming.system.SystemConstants, org.jblooming.utilities.DateUtilities,
org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.display.Img,
org.jblooming.waf.html.input.DateField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.Date, java.util.List, org.jblooming.waf.settings.I18n"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new TaskController(), request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);

    pageState.toHtml(pageContext);

  } else {



    Task task = (Task) PersistenceHome.findByPrimaryKey(Task.class, pageState.mainObjectId);

    PageSeed self = pageState.thisPage(request);
    self.mainObjectId = pageState.mainObjectId;
    Form f = new Form(self);
    pageState.setForm(f);
    f.start(pageContext);

    //Img print = new Img("printIcon.png", pageState.getI18n("PRINT_PAGE"), "20", "20");
    Img log = new Img(ApplicationState.getApplicationSetting(SystemConstants.PRINT_LOGO), "");

    %><table border="0" align="center" cellpadding="5" cellspacing="0" width="100%">
          <tr>
            <td nowrap><%log.toHtml(pageContext);%></td>
            <td width="150" align="right" class="textSmallDark">
            <a href="javascript:obj('printButton').style.visibility='hidden';window.print();window.close()">
              <div id="printButton" style="visibility:visible;" class="button icon textual">
                <span class="teamworkIcon" title="<%=I18n.get("PRINT_PAGE")%>">p</span>
              </div>
            </a>
          </td>
          </tr>
    <tr><td colspan="2"><h3><%=I18n.get("WORKLOGS_IN_PERIOD")%>: <%=task.getDisplayName()%></h3></td></tr>
      </table>
<div class="optionsBar">
    <table cellpadding="5" cellspacing="0" align="left">
       <tr>
         <td><%=I18n.get("PICK_INTERVAL")%>:</td>
         <td><%new DateField("START").toHtmlI18n(pageContext);%></td>
         <td><%new DateField("END").toHtmlI18n(pageContext);%></td>
         <td><%ButtonSubmit go = new ButtonSubmit(f); go.label="GO";go.additionalCssClass="small";go.toHtmlI18n(pageContext);%></td>
         </tr>
    </table></div><%

  Date start = pageState.getEntry("START").dateValue();
  Date end = pageState.getEntry("END").dateValue();

  Date endOfToday = (new CompanyCalendar()).setAndGetTimeToDayEnd();
  if (end!=null && end.getTime()>endOfToday.getTime())
    end = endOfToday;


  if (start!=null && end!=null && start.getTime()<end.getTime()) {

    %><table border="0" width="100%" align="center" cellpadding="5" cellspacing="0" class="table">
    <tr>
      <th class="tableHead"><%=I18n.get("TASK")%></th>
      <th class="tableHead"><%=I18n.get("ASSIGNMENT")%></th>
      <th class="tableHead"><%=I18n.get("WORKLOG_IN_HOURS")%></th>
    </tr><%

   
    String hql = " select task.id, task.ancestorIds, assignment.id, sum(worklog.duration) from "+
           Task.class.getName()+" as task, "+ Assignment.class.getName()+" as assignment, "+Worklog.class.getName() + " as worklog " +
           " where task.id = assignment.task.id and assignment.id = worklog.assig.id " +
           " and worklog.inserted >= :startDate and worklog.inserted <= :endDate"+
           " and (task=:mainTask or task.ancestorIds like :rootIdHacked)" +
           " group by task.id, task.ancestorIds, assignment.id";

    OqlQuery oqlQuery = new OqlQuery(hql);
    oqlQuery.getQuery().setEntity("mainTask",task);
    oqlQuery.getQuery().setTimestamp("startDate",start);
    oqlQuery.getQuery().setTimestamp("endDate",end);
    oqlQuery.getQuery().setString("rootIdHacked", JSP.w(task.getChildAncentorIds())+"%");

    List<Object[]> res = oqlQuery.list();



    long totalW = 0;
    for (Object[] o : res) {

      String taskId = (String) o[0];
      Task t = (Task) PersistenceHome.findByPrimaryKey(Task.class,taskId);
      String ancestorIds = (String) o[1];
      String assignmentId = (String) o[2];
      Assignment a = (Assignment) PersistenceHome.findByPrimaryKey(Assignment.class,assignmentId);
      long duration = (Long) o[3];

      %> <tr class="alternate"> <%
      %> <td><%=t.getDisplayName()%></td><%
      %> <td><%=a.getDisplayNameWithResource()%></td><%
      %> <td align="right"><%=DateUtilities.getMillisInHoursMinutes(duration)%></td> </tr><%
      totalW += duration;

    }

    %><tr><td colspan="3" align="right" style="border-top:1px solid;"><big>
        <i>(<%=I18n.get("DAYS_HOURS_MINUTES")%>&nbsp;<%=DateUtilities.getMillisInDaysWorkHoursMinutes(totalW)%>)</i>
        <b><%=DateUtilities.getMillisInHoursMinutes(totalW)%></b></big>
      </td></tr><%

    %></table> <%




    }
  f.end(pageContext);

  }
%>