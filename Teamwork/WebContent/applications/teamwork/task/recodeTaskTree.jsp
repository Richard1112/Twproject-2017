<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.security.SecurityBricks, com.twproject.security.TeamworkPermissions, com.twproject.task.Task,com.twproject.task.businessLogic.TaskController, com.twproject.waf.TeamworkHBFScreen,
com.twproject.waf.html.TaskHeaderBar, org.jblooming.operator.Operator, org.jblooming.utilities.StringUtilities, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Fields,org.jblooming.waf.html.button.ButtonLink,org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.Combo, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jblooming.waf.constants.Commands, org.jblooming.system.SystemConstants, com.twproject.waf.TeamworkPopUpScreen, org.jblooming.waf.html.button.ButtonJS" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();


    Task task = Task.load(pageState.mainObjectId);
    pageState.setMainObject(task);
    task.testPermission(logged, TeamworkPermissions.task_canWrite);

    pageState.addClientEntry("newCode",task.getCode());


%><div class="mainColumn">

<h2><%=I18n.get("RECODE_TASK_TREE")%></h2>
<div class="hint"><%=I18n.get("RECODE_TASK_TREE_EXPLAIN")%></div>
<table class="table" border="0" cellpadding="5" id="recodeForm">
  <tr>
    <td><%
      TextField codePrefix=new TextField("newCode","<br>");
      codePrefix.label=I18n.get("NEW_TASK_CODE");
      codePrefix.required=true;
      codePrefix.toHtml(pageContext);

    %></td></tr>
  <tr>
    <td colspan="2" align="right">
      <%
        ButtonBar buttonBar = new ButtonBar();
        ButtonJS save = new ButtonJS(I18n.get("SAVE"),"recodeTask()");
        save.confirmRequire=true;
        save.additionalCssClass="first";
        buttonBar.addButton(save);
        buttonBar.toHtml(pageContext);
      %>
    </td>
  </tr>
</table>
<script>
  function recodeTask(){
    //console.debug("recodeTask");
    $("#newCode").clearErrorAlert();
    if (canSubmitForm($("#recodeForm"))) {
      var nc = $("#newCode");

      var request = {CM: "RECODE", OBJID:"<%=task.getId()%>",newCode: nc.val()};

      showSavingMessage();
      $.getJSON("taskAjaxController.jsp",request,function(response){
        jsonResponseHandling(response);
        if (response.ok ) {
          parent.location.href=contextPath+"/applications/teamwork/task/taskEditor.jsp?CM=ED&OBJID="+request.OBJID;
        }
        hideSavingMessage();
      })

    }
  }
</script>


<%

}

%>
