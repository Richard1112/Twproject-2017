<%@ page import="com.twproject.task.businessLogic.IssueController, com.twproject.waf.TeamworkHBFScreen, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea,
org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n,
org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState" %><%


  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;

    final ScreenArea body = new ScreenArea(new IssueController(), request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {
    PageSeed self = pageState.thisPage(request);
    self.command = Commands.FIND;
    Form form = new Form(self);
    form.alertOnChange = true;
    form.id = "issueMultiEditor";
    pageState.setForm(form);

%><script>$("#ISSUES_MENU").addClass('selected');</script>

<%---------------------------------------------- RIGHT COLUMN START ---------------------------------------------------------%>
<jsp:include page="partIssueSidebBar.jsp"/>
<%------------------------------------------------ RIGHT COLUMN END ---------------------------------------------------------%>

<%
    // this var is used to inform the page that single-user or single-task search is currently active in order to enable issue sorting
    // can be: "BY_RESOURCE", "BY_TASK", "NONE"
    String sortFlavour = "NONE";
    String resourceId = pageState.getEntry("FLT_ISSUE_ASSIGNED_TO").stringValueNullIfEmpty();
    String taskId = pageState.getEntry("FLT_ISSUE_TASK").stringValueNullIfEmpty();

    if (JSP.ex(resourceId)) {
      // enable sortFlavour for assignee
      sortFlavour = "BY_RESOURCE";
    } else if (JSP.ex(taskId)) {
      // enable sortFlavour for task
      sortFlavour = "BY_TASK";
    }

  String savedFilterName = pageState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty();


%>




<%---------------------------------------------- MAIN COLUMN START ---------------------------------------------------------%>


<div class="mainColumn">

  <h1 class="filterTitle" defaultTitle="<%=I18n.get("ISSUE_LIST")%>">
    <%=JSP.ex(savedFilterName)?I18n.get(savedFilterName):I18n.get("ISSUE_LIST")%>
  </h1>



<%---------------------------------------------- ISSUE LIST START ---------------------------------------------------------%>
  <%  new JspHelper("/applications/teamwork/issue/partIssueList.jsp").toHtml(pageContext);%>
  <%---------------------------------------------- ISSUE LIST END ---------------------------------------------------------%>

</div>
<%
}

%>
