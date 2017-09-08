<%@ page contentType="text/html;charset=UTF-8" language="java" %><%@ page
        import="com.twproject.agenda.Event,
                com.twproject.operator.TeamworkOperator,
                com.twproject.resource.Person,
                com.twproject.resource.Resource,
                com.twproject.resource.ResourceBricks,
                com.twproject.security.TeamworkPermissions,
                com.twproject.waf.TeamworkPopUpScreen,
                com.twproject.worklog.Worklog,
                org.jblooming.agenda.CompanyCalendar,
                org.jblooming.agenda.Period,
                org.jblooming.oql.OqlQuery,
                org.jblooming.oql.QueryHelper,
                org.jblooming.system.SystemConstants,
                org.jblooming.utilities.DateUtilities,
                org.jblooming.utilities.JSP,
                org.jblooming.waf.ScreenArea,
                org.jblooming.waf.constants.Fields,
                org.jblooming.waf.html.button.ButtonJS,
                org.jblooming.waf.html.button.ButtonSubmit,
                org.jblooming.waf.html.container.Container,
                org.jblooming.waf.html.display.Img,
                org.jblooming.waf.html.input.CheckField,
                org.jblooming.waf.html.input.DateField,
                org.jblooming.waf.html.input.SmartCombo,
                org.jblooming.waf.html.input.TextField,
                org.jblooming.waf.html.state.Form,
                org.jblooming.waf.settings.ApplicationState,
                org.jblooming.waf.settings.I18n,
                org.jblooming.waf.view.PageSeed,
                org.jblooming.waf.view.PageState,
                java.util.Date, java.util.List, java.util.TreeMap" %><%


  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    body.areaHtmlClass="lreq20 lreqPage";
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);

    pageState.toHtml(pageContext);
  } else {


    boolean iAmWatchingMyBusiness = true;
    String resId = pageState.getEntry("RES_ID").stringValueNullIfEmpty();
    String resTxt = pageState.getEntry("RES_ID" + SmartCombo.TEXT_FIELD_POSTFIX).stringValueNullIfEmpty();

    TeamworkOperator loggedOperator = (TeamworkOperator) pageState.getLoggedOperator();
    if (resId != null) {
      iAmWatchingMyBusiness = (ResourceBricks.myself(pageState).getId() == resId);
    } else if (resTxt==null){
      //se non c'è specificato nulla metto me stesso
      resId= loggedOperator.getId()+"";
      pageState.addClientEntry("RES_ID",resId);
    }

    boolean searchingForMissing = Fields.TRUE.equals(pageState.getEntry("ISMISSING").stringValueNullIfEmpty());

    PageSeed self = pageState.thisPage(request);
    self.setCommand("PRINT");
    Form f = new Form(self);
    pageState.setForm(f);
    f.start(pageContext);

    Img logo = new Img(ApplicationState.getApplicationSetting(SystemConstants.PRINT_LOGO), "");
%>
<table border="0" width="100%" align="center" cellpadding="5" cellspacing="0" class="noprint">
  <tr>
    <td align="left" width="90%"><%logo.toHtml(pageContext);%></td>
    <td align="right"><%

      TextField.hiddenInstanceToHtml("ISMISSING",pageState.getEntry("ISMISSING").stringValueNullIfEmpty(), pageContext);

      ButtonJS print = new ButtonJS("window.print();");
      print.label = "";
      print.toolTip = I18n.get("PRINT_PAGE");
      print.iconChar = "p";
      print.toHtmlInTextOnlyModality(pageContext);

    %></td>
  </tr>
</table>
<table width="100%" align="center"><tr><td>
<div class="optionsBar">
    <div class="filterElement"><%

                 SmartCombo assignee = ResourceBricks.getInspectableResourcesCombo("RES_ID", pageState);
                 assignee.label = I18n.get("WORKLOGS_OF_RESOURCE");
                 assignee.separator = "<br>";
                 assignee.addAllowed = true;
                 assignee.fieldSize = 30;
                 assignee.preserveOldValue = false;
                 assignee.toHtmlI18n(pageContext);
                 Container help = new Container();
                  help.status = Container.HIDDEN;
                  help.closeable=true;
                  help.draggable = true;
                  help.width = "150px";
                  help.title = I18n.get("HELP");
                  help.start(pageContext);
                  %><small><%=I18n.get("HELP_SMART_COMBO_MULTI")%></small><%
                  help.end(pageContext);
                  ButtonJS helpB =  help.getOpenerButton(true);
                  helpB.label=  "&nbsp;<small>"+I18n.get("HELP")+"</small>";
                  helpB.toHtmlInTextOnlyModality(pageContext);


             %>

    </div><div class="filterElement"><%
              Date start = pageState.getEntryOrDefault("START_WM").dateValue() != null ? pageState.getEntryOrDefault("START_WM").dateValue() : new Date();

              Date end = pageState.getEntryOrDefault("END_WM").dateValue() != null ? pageState.getEntryOrDefault("END_WM").dateValue() : new Date();
              CompanyCalendar temp = new CompanyCalendar(end);
              end = temp.setAndGetTimeToDayEnd();


              DateField startCE = new DateField("START_WM");
              startCE.separator = "<br>";
              startCE.labelstr = "START";
              DateField endCE = new DateField("END_WM");
              endCE.separator = "<br>";
              endCE.labelstr = "END";

          pageState.getEntryOrDefault("SHOW_EVERY_DAY");
          CheckField showEveryday= new CheckField(I18n.get("SHOW_EVERY_DAY"),"SHOW_EVERY_DAY", "&nbsp;", false);

       %></div>
        <div class="filterElement"><%startCE.toHtmlI18n(pageContext);%></div>
        <div class="filterElement"><%endCE.toHtmlI18n(pageContext);%></div>
        <div class="filterElement centered"><%showEveryday.toHtml(pageContext);%></div>
        <div class="filterElement centered"><%
          ButtonSubmit go = new ButtonSubmit(f);
          go.variationsFromForm.command = pageState.getCommand();
          go.label = I18n.get("REFRESH");
          go.additionalCssClass = "small";
          go.toHtml(pageContext);

          Date endOfToday = (new CompanyCalendar()).setAndGetTimeToDayEnd();

          if (end != null && end.getTime() > endOfToday.getTime())
            end = endOfToday;

    %></div>
</div><%

  if (start != null && end != null && start.getTime() < end.getTime() ) {

%><table border="0" width="100%" align="center" cellpadding="5" cellspacing="0" class="table dataTable">
  <tr style="font-weight:bold;">
    <th nowrap class="tableHead"><%=I18n.get("NAME")%></th>
    <th nowrap class="tableHead"><%=I18n.get("DAY")%></th>
    <th class="tableHead"><%=I18n.get("WORKLOG_IN_HOURS")%></th>
    <th class="tableHead"><%=I18n.get("APPOINTMENTS_LIST")%></th>
  </tr><%



    String q =  "select distinct r from  " + Person.class.getName() + " as r ";
    QueryHelper qhelp = new QueryHelper(q);
    if (JSP.ex(resId)) {
      qhelp.addOQLClause("r.id = :assignee", "assignee", resId);
    } else {
      if (JSP.ex(resTxt)) {
        qhelp.addQBEORClauses(
                resTxt,
                qhelp.getOrElement("r.name", "name", QueryHelper.TYPE_CHAR),
                qhelp.getOrElement("r.personSurname || ' ' || r.personName", "surnameName", QueryHelper.TYPE_CHAR),
                qhelp.getOrElement("r.personName || ' ' || r.personSurname", "nameSurname", QueryHelper.TYPE_CHAR)
        );
      }
    }

    List<Resource> resources = qhelp.toHql().list();

    String hql = "select sum(worklog.duration) from " + Worklog.class.getName() + " as worklog " +
            "where " +
            "worklog.assig.resource = :me and " +
            "worklog.inserted>=:currentDayBegin and worklog.inserted<=:currentDayEnd";

    int printed = 0;
    for(Resource r :resources){

      //controllo permessi
      if (!r.equals(loggedOperator.getPerson()) && !r.hasPermissionFor(loggedOperator, TeamworkPermissions.resource_manage) && !r.hasPermissionFor(loggedOperator, TeamworkPermissions.worklog_manage) )
        continue;

      TreeMap<Integer, Long> workablePlan = r.getWorkablePlan(new Period(start, end));

  %><tr <%=printed>0 ? "style=\"page-break-before: always;\"":""%> class="tableSection"><td colspan="4"><div class="childNode"><h2><%=r.getDisplayName()%></h2></div></td></tr><%
      printed++;

      CompanyCalendar cc = new CompanyCalendar();
      cc.setTime(start);
      Date currentDayBegin = cc.setAndGetTimeToDayStart();
      Date currentDayEnd = cc.setAndGetTimeToDayEnd();
      boolean showEveryDay = pageState.getEntry("SHOW_EVERY_DAY").checkFieldValue();
      while (currentDayBegin.getTime() <= end.getTime()) {

        int day= DateUtilities.dateToInt(cc.getTime());

        Long todayCap = workablePlan.get(day);

        if (todayCap != null && todayCap>0) {

          OqlQuery oql = new OqlQuery(hql);
          oql.getQuery().setEntity("me", r);
          oql.getQuery().setTimestamp("currentDayBegin", currentDayBegin);
          oql.getQuery().setTimestamp("currentDayEnd", currentDayEnd);
          Long wklgs = (Long) oql.uniqueResultNullIfEmpty();
          if (wklgs == null)
            wklgs = 0L;

          if((searchingForMissing && wklgs < todayCap) || (!searchingForMissing && wklgs > todayCap) || showEveryDay){
            // collect agenda events
            Period todayPeriod = Period.getDayPeriodInstance(cc.getTime());
            TreeMap<Period, Event> evtm = Event.getPeriodsInPeriodFor(r, todayPeriod, false);

            %>
            <tr class="alternate">
              <td><%=r.getDisplayName()%>&nbsp;</td>
              <td><%=DateUtilities.dateToFullString(currentDayBegin)%>&nbsp;</td>
              <td><%=DateUtilities.getMillisInHoursMinutes(wklgs)%>
              </td>
              <td><%
                if (evtm.size() > 0) {
              %>
                <table><%
                  for (Period key : evtm.keySet()) {
                    Event ev = evtm.get(key);
                %>
                  <tr>
                    <td><%=DateUtilities.dateToHourMinutes(key.getStartDate()) + "-" + DateUtilities.dateToHourMinutes(key.getEndDate())%>
                    </td>
                    <td><%
                      if (ev.isPersonal() && !iAmWatchingMyBusiness) {
                        %><i><%=I18n.get("IS_PERSONAL")%></i><%
                      } else {
                        %><%=ev.getSummary()%><%
                      }
                    %></td>
                  </tr>
                  <%
                    }
                  %></table>
                <%
                } else {
                %>&nbsp;<%
                }

              %></td>
            </tr>
            <%
          }
        }
        cc.add(CompanyCalendar.DAY_OF_YEAR, 1);
        currentDayBegin = cc.setAndGetTimeToDayStart();
        currentDayEnd = cc.setAndGetTimeToDayEnd();

      }
    }
  %></table></table><%

    }
    f.end(pageContext);
  }
%>
