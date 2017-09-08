<%@ page import="com.twproject.document.businessLogic.DocumentController, com.twproject.task.Task, com.twproject.waf.TeamworkPopUpScreen, org.jblooming.waf.ScreenArea,
org.jblooming.waf.constants.Commands, org.jblooming.waf.html.state.Form, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, com.twproject.document.TeamworkDocument" %><%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new DocumentController(), request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {
    //this is set by action
    Task task = (Task) pageState.attributes.get("REFERRAL_OBJECT");

    PageSeed ps = pageState.thisPage(request);
    ps.command = Commands.EDIT;
    ps.addClientEntry("TASK_ID", task.getId());
    ps.mainObjectId = pageState.getMainObject().getId();
    Form form = new Form(ps);
    form.alertOnChange = true;
    form.encType = Form.MULTIPART_FORM_DATA;
    form.start(pageContext);
    pageState.setForm(form);
%>
<div class="mainColumn">
<jsp:include page="/applications/teamwork/document/partDocumentableDocumentEdit.jsp"/>
</div>
 <%
    form.end(pageContext);
   }
 %>


