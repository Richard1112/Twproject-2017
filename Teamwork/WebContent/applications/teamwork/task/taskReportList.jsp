<%@page import="com.teamwork.expand.ReportController"%>
<%@page import="com.twproject.operator.TeamworkOperator"%>
<%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8" %>
<%@ page import="com.twproject.task.IssueBricks,
                 com.twproject.task.Task,
                 com.twproject.waf.TeamworkHBFScreen,
                 com.twproject.waf.html.TaskHeaderBar,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.constants.Fields,
                 org.jblooming.waf.html.core.JspHelper,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new ReportController(), request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {
//     if (!JSP.ex(pageState.mainObjectId)) {
//       pageState.mainObjectId = pageState.getEntry("FLT_ISSUE_TASK").stringValueNullIfEmpty() + "";
//     } else {
//       pageState.addClientEntry("FLT_ISSUE_TASK", pageState.mainObjectId);
//       pageState.addClientEntry("FLT_TASK_ISSUE_SHOW_CHILDREN", Fields.TRUE);
//       IssueBricks.addOpenStatusFilter(pageState);
//     }


    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    //this is set by action
    Task task = (Task) pageState.attributes.get("REFERRAL_OBJECT");
    pageState.setMainObject(task);

    // Form -----------------------------------------------------------------------------------------------
    PageSeed self = pageState.thisPage(request);
    self.mainObjectId = task.getId();
    self.setCommand(Commands.FIND);
    String parId = pageState.getEntry(Fields.PARENT_ID).stringValueNullIfEmpty();
    self.addClientEntry(Fields.PARENT_ID, JSP.w(parId));

    Form form = new Form(self);
    form.encType = Form.MULTIPART_FORM_DATA;
    form.alertOnChange = false;
    form.id="reportMultiEditor";
    pageState.setForm(form); // il form è stampato dalla partReportList



  //  if ("ADDISSUE".equals(pageState.command)){
      %>
      <script>
//         $(function(){
//           $("[data-table]").one("load.dataTable",function(){
//             addIssue();
//           });
//         });
      </script>
      <%
 //   }

    //----------------------------------------------- MAIN COLUMN START ---------------------------------------------------------

%>
<div class="mainColumn">
  <%


    //-----------------HEAD BAR START
    pageState.addClientEntry("TASK_TABSET", "TASK_REPORT_TAB");
    TaskHeaderBar head = new TaskHeaderBar(task);
    //head.pathToObject.mainClass = Task.class;
    head.toHtml(pageContext);
    //-----------------HEAD BAR END

    //---------------------------------------------- REPORT LIST START ---------------------------------------------------------
    new JspHelper("/applications/teamwork/task/report/partReportList.jsp").toHtml(pageContext);
    //---------------------------------------------- REPORT LIST END ---------------------------------------------------------

    //----------------------------------------------- MAIN COLUMN END ---------------------------------------------------------

  %></div><%


    //---------------------------------------------- RIGHT COLUMN START ---------------------------------------------------------
    if (!task.isNew()) {
      JspHelper side = new JspHelper("part/partTaskSideBar.jsp");
      side.parameters.put("TASK", task);
      side.toHtml(pageContext);
    }
    //---------------------------------------------- RIGHT COLUMN END ---------------------------------------------------------
  }
%>
