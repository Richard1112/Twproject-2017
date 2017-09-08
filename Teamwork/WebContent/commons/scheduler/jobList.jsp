<%@ page import="org.jblooming.scheduler.businessLogic.JobController, org.jblooming.waf.ScreenBasic, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.DataTable, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form,
                org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jblooming.waf.constants.Fields, org.jblooming.utilities.JSP, org.jblooming.waf.html.display.DeletePreviewer"%><%

PageState pageState = PageState.getCurrentPageState(request);
pageState.getLoggedOperator().testIsAdministrator();

if (!pageState.screenRunning) {

  ScreenBasic.preparePage(new JobController(),pageContext);
  pageState.perform(request, response).toHtml(pageContext);

} else {

  PageSeed self = pageState.thisPage(request);
  self.setCommand(Commands.FIND);
  Form f = new Form(self);
  f.addKeyPressControl(13, "this.submit();", "onkeyup");
  f.start(pageContext);
  pageState.setForm(f);

  new DeletePreviewer("JOB_DEL",JobController.class, pageState);



%><script>$("#ADMINISTRATION_ROOT_MENU").addClass('selected');</script>
<%
  ButtonLink adminLink = new ButtonLink(I18n.get("ADMINISTRATION_ROOT_MENU") + " /",pageState.pageFromRoot("administration/administrationIntro.jsp"));
%>
<%adminLink.toHtmlInTextOnlyModality(pageContext);%>
<h1 class="filterTitle">
  <%=I18n.get("JOB_LIST")%>
</h1>


<%

  TextField tfCode = new TextField("TEXT", I18n.get("JOB_NAME"), "JOB_NAME", "</td><td>", 30, false);
  tfCode.addKeyPressControl(13, "obj('" + f.id + "').submit();", "onkeyup");
%>
<table class="table" cellspacing="5">
  <tr>
    <td width="10%" nowrap >
      <%tfCode.toHtml(pageContext);%>
    </td>
  </tr>
</table>
<%
    ButtonBar bb = new ButtonBar();

    PageSeed sm = new PageSeed("scheduleManager.jsp");
    ButtonLink smlist = new ButtonLink(sm);
    smlist.label = I18n.get("SCHEDULER");
    bb.addButton(smlist);

    PageSeed ad = new PageSeed("jobEditor.jsp");
    ad.setCommand(Commands.ADD);
    ButtonLink add = new ButtonLink(I18n.get("ADD"), ad);
    add.additionalCssClass="first lreq30 lreqLabel";
    bb.addButton(add);

    DataTable dataTable = new DataTable("LH", f, new JspHelper("/commons/scheduler/rowJobList.jsp"), JobController.class, pageState);
    dataTable.addHeader("name", "job.name");
    dataTable.addHeader("description", "job.description");
    dataTable.addHeader("executable", "job.executable");
    dataTable.addHeader("type");
    dataTable.addHeader("on/off", "job.enabled");
    dataTable.addHeaderFitAndCentered("<span class=\"lreq30 lreqLabel\">"+I18n.get("DELETE_SHORT")+"</span>");
    bb.addButton(dataTable.getSearchButton());

    bb.toHtml(pageContext);

    dataTable.drawPaginator(pageContext);
    dataTable.drawTable(pageContext);

    f.end(pageContext);

    pageState.setFocusedObjectDomId(tfCode.id);
%>

<script>
  function delRow(el) {
    var issueRow = $(el).closest("[jobid]");
    var issueId = issueRow.attr("jobid");
    deletePreview("JOB_DEL", issueId, function(response) {  // callback function
      if (response && response.ok){
        issueRow.fadeOut(500, function () {$(this).remove();});
      }
    });
  }
</script>

  <%
  }
%>
