<%@ page import="com.twproject.operator.TeamworkOperator, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.container.DivOnMouseover, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, com.twproject.security.TeamworkPermissions, com.twproject.resource.Resource, com.twproject.resource.Person, org.jblooming.waf.html.button.ButtonSupport" %><%
  PageState pageState = PageState.getCurrentPageState(request);

  Person myself = Person.getLoggedPerson(pageState);
  Resource focusedResource = myself;

/*
  ButtonJS opener = new ButtonJS("");
  opener.label=I18n.get("TIMESHEET_MENU");
  //opener.iconChar="[";
*/

  PageSeed ww = pageState.pageFromRoot("task/worklog/worklogWeek.jsp");
  ww.addClientEntry( pageState.getEntry("FOCUS_MILLIS"));
  ButtonSupport but = new ButtonLink(I18n.get("TIMESHEET_MENU"), ww);
  but.additionalCssClass="lreq10 lreqActive hasDivomo";

  DivOnMouseover domo = new DivOnMouseover(null, but, true);

  {
    but = new ButtonLink(I18n.get("TIMESHEET_MENU"), ww);
    but.additionalCssClass="lreq10 lreqLabel lreqActive";
    domo.addButton(but);
  }
  {
    PageSeed tc = pageState.pageFromRoot("task/timeCounter.jsp");
    tc.addClientEntry("FOCUS_MILLIS", System.currentTimeMillis());
    but = ButtonLink.getBlackInstance(I18n.get("TIMECOUNTER_MENU"), tc);
    but.additionalCssClass="lreq10 lreqLabel lreqActive";
    domo.addButton(but);
  }
  {
    PageSeed wo = pageState.pageFromRoot("task/worklog/worklogOverview.jsp");
    wo.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));
    but = new ButtonLink(I18n.get("TIMESHETS_OVERVIEW"), wo);
    but.additionalCssClass="lreq10 lreqLabel";
    domo.addButton(but);
  }
  if (pageState.getLoggedOperator().hasPermissionFor(TeamworkPermissions.worklog_manage) ||
    (focusedResource!=null && focusedResource.getMyself()!=null && focusedResource.getMyself().equals(pageState.getLoggedOperator()))
    ) {
    PageSeed ps = pageState.pageFromRoot("/task/worklog/approval/worklogApprovalByResource.jsp");
    ps.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));
    ps.addClientEntry("RES_ID", focusedResource.getId());
    but = new ButtonLink(I18n.get("WORKLOG_APPROVAL"), ps);
    but.additionalCssClass="lreq20 lreqLabel lreqActive";
    domo.addButton(but);

  }
  {
    PageSeed wlL = pageState.pageFromRoot("task/worklog/worklogList.jsp");
    but = new ButtonLink(I18n.get("WORKLOG_FIND_MENU"), wlL);
    but.additionalCssClass="lreq10 lreqLabel";
    domo.addButton(but);
  }
  {
    PageSeed wlL = pageState.pageFromRoot("task/financial/expenseList.jsp");
    but = new ButtonLink(I18n.get("ASSIG_EXPENSES_LIST"), wlL);
    but.additionalCssClass="lreq30 lreqLabel lreqActive";
    domo.addButton(but);
  }

  domo.additionalDropDownClass="divDark";
  domo.toHtml(pageContext);
%>
