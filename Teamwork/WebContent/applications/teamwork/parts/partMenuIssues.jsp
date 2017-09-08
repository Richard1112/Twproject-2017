<%@ page import="com.twproject.operator.TeamworkOperator, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.container.DivOnMouseover, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jblooming.waf.html.button.ButtonSupport" %><%
  PageState pageState = PageState.getCurrentPageState(request);
  ButtonSupport but;

  //if (request.getRequestURI().contains("issueOrganizer")||request.getRequestURI().contains("issuePlanner")||request.getRequestURI().contains("taskIssueList")) { //tolta per non usare il task come filtro di default
  if (request.getRequestURI().contains("issueOrganizer")||request.getRequestURI().contains("issuePlanner")){
     but = new ButtonJS(I18n.get("ISSUES_MENU"), "changeActionAndSubmit(contextPath+'/applications/teamwork/issue/issueList.jsp','FN')");
  } else {
    PageSeed ww = pageState.pageFromRoot("issue/issueList.jsp");
    but = new ButtonLink(I18n.get("ISSUES_MENU"), ww);

  }
  but.additionalCssClass = "hasDivomo";


  DivOnMouseover domo = new DivOnMouseover(null, but, true);
  domo.addButton(but);

  if (request.getRequestURI().contains("issueList")||request.getRequestURI().contains("issuePlanner")||request.getRequestURI().contains("taskIssueList")) {
    but = new ButtonJS(I18n.get("ISSUE_ORGANIZER"), "changeActionAndSubmit(contextPath+'/applications/teamwork/issue/issueOrganizer.jsp','FN')");
  } else {
    PageSeed ol = pageState.pageFromRoot("issue/issueOrganizer.jsp");
    but = new ButtonLink(I18n.get("ISSUE_ORGANIZER"), ol);
  }
  but.additionalCssClass="lreq10 lreqLabel lreqActive";
  domo.addButton(but);


  if (request.getRequestURI().contains("issueList")||request.getRequestURI().contains("issueOrganizer")||request.getRequestURI().contains("taskIssueList")) {
    but = new ButtonJS(I18n.get("ISSUE_PLANNER"), "changeActionAndSubmit(contextPath+'/applications/teamwork/issue/issuePlanner.jsp','FN')");
  } else {
    PageSeed ap = pageState.pageFromRoot("issue/issuePlanner.jsp");
    but = new ButtonLink(I18n.get("ISSUE_PLANNER"), ap);
  }
  but.additionalCssClass="lreq20 lreqLabel lreqActive";
  domo.addButton(but);

  domo.additionalDropDownClass="divDark";
  domo.toHtml(pageContext);
%>
