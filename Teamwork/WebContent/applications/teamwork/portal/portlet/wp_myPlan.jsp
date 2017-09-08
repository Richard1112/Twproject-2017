<%@ page import="com.twproject.resource.Person, com.twproject.resource.Resource, com.twproject.worklog.WorklogPlan, org.jblooming.agenda.CompanyCalendar, org.jblooming.agenda.Period,
                org.jblooming.oql.OqlQuery, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.Bricks, org.jblooming.waf.SessionState,
                org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.Date, java.util.List" %><%
  PageState pageState = PageState.getCurrentPageState(request);

  CompanyCalendar cc= new CompanyCalendar(new Date());
  Date start=cc.setAndGetTimeToDayStart();
  cc.add(CompanyCalendar.DAY_OF_MONTH,1);
  Date end =cc.setAndGetTimeToDayEnd();

  Person resource = Person.getLoggedPerson(pageState);

  String hql="select wlp from " + WorklogPlan.class.getName() + " as wlp where wlp.assig.resource = :res and wlp.inserted>=:pst and wlp.inserted<=:pen and wlp.duration>0 order by wlp.inserted";
  OqlQuery oql = new OqlQuery(hql);
  oql.getQuery().setEntity("res", resource);
  oql.getQuery().setTimestamp("pst", start);
  oql.getQuery().setTimestamp("pen", end);
  List<WorklogPlan> wps = oql.list();

  if (JSP.ex(wps)){
    //there is something in plan

    %><div class="portletBox myPlannedWork lreq30 lreqHide"><%
    PageSeed ps = pageState.pageFromRoot("task/plan/planByResource.jsp");
    Bricks.addReferral(resource.getId(), Resource.class, ps);
    ps.addClientEntry( pageState.getEntry("FOCUS_MILLIS"));
    ps.setCommand("FIND_BY_ENTITY");


    ButtonLink sub = new ButtonLink(ps);
    sub.label=I18n.get("PLAN_BY_RESOURCE");
    sub.additionalCssClass="small";


  %><div style="float: right;"><%sub.toHtmlInTextOnlyModality(pageContext);%></div>
  <h1><%=I18n.get("PLANNED_WORK")%></h1><%

    %><table class="table" border="0"><%

    Date lastDate= Period.getDayPeriodInstance(start).getEndDate();
    boolean firstRow = true;
    for (WorklogPlan wlp:wps){
      // key break
      if (firstRow || lastDate.compareTo(wlp.getInserted())<0){
        cc.setTime(wlp.getInserted());
        lastDate=cc.setAndGetTimeToDayEnd();

        %><tr><td colspan="2"><h4 style="border-bottom: 1px solid #CCCCCC;" ><%--=I18n.get(cc.isToday()?"TODAY":"TOMORROW")--%> <%=DateUtilities.dateToFullString(wlp.getInserted())%></h4></td></tr><%
      firstRow = false;
      }
      %><tr><td valign="top"><%=DateUtilities.getMillisInHoursMinutes(wlp.getDuration())%></td>
        <td>
          <%=wlp.getAssig().getTask().getDisplayName()%>
          <%
            if (JSP.ex(wlp.getAction())){
              %><br><i><%=wlp.getAction()%></i><%
            }
          %>
        </td>
      </tr><%
    }
    %></table></div><%
  }
%>