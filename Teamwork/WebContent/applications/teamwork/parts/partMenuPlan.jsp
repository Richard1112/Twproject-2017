<%@ page import="com.twproject.operator.TeamworkOperator, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.container.DivOnMouseover, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState" %><%
  PageState pageState = PageState.getCurrentPageState(request);


/*
  ButtonJS opener = new ButtonJS("");
  opener.label=I18n.get("PLAN_MENU");
*/

  PageSeed ww = pageState.pageFromRoot("task/plan/planByResource.jsp");
  ww.setCommand("FIND_BY_ENTITY");
  ww.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));
  ButtonLink but = new ButtonLink(I18n.get("PLAN_MENU"), ww);
  but.additionalCssClass="lreq20 lreqActive hasDivomo";

  DivOnMouseover domo = new DivOnMouseover(null, but, true);
  {
    ButtonLink butpl = new ButtonLink(I18n.get("PLAN_MENU"), ww);
    butpl.additionalCssClass="lreq30 lreqActive lreqLabel";
    domo.addButton(butpl);
  }
  {
    PageSeed ol = pageState.pageFromRoot("task/plan/operatorLoad.jsp");
    ol.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));
    but = new ButtonLink(I18n.get("TASK_WORKGROUP_OPERATOR_LOAD"), ol);
    but.additionalCssClass="lreq20 lreqActive lreqLabel";
    domo.addButton(but);
  }


  {
    PageSeed ap = pageState.pageFromRoot("task/assignment/assignmentPriority.jsp");
    ap.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));
    but = new ButtonLink(I18n.get("TASK_WORKGROUP_PRIORITY"), ap);
    but.additionalCssClass ="lreq20 lreqLabel lreqActive";

    domo.addButton(but);
  }



  domo.additionalDropDownClass="divDark";
  domo.toHtml(pageContext);
%>
