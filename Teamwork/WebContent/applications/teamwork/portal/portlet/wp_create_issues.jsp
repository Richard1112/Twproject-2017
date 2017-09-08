<%@ page import="com.twproject.resource.Person,
                 com.twproject.resource.ResourceBricks,
                 com.twproject.security.TeamworkPermissions,
                 com.twproject.task.Issue,
                 com.twproject.task.IssueBricks,
                 com.twproject.task.TaskBricks,
                 org.jblooming.operator.Operator,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.SessionState,
                 org.jblooming.waf.constants.Fields,
                 org.jblooming.waf.html.button.ButtonJS,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.container.ButtonBar,
                 org.jblooming.waf.html.input.*,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.ApplicationState,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState, com.twproject.task.IssueStatus, com.twproject.resource.Resource"
  %>
<%

  PageState pageState = PageState.getCurrentPageState(request);

%>
<div id="createIssuesExDiv" class="portletBox" style="background-color: #FBFBD4">
<script>

  function saveAndReloadIssues(button) {
    //console.debug("saveAndReloadIssues")
    if(canSubmitForm('CREATEISSUE_FORM')) {
      showSavingMessage();
      var request = {CM: "SV", OBJID: "new", setValuesAsDefault: "yes"};
      $("#issueCreateLine").fillJsonWithInputValues(request);

      $.getJSON(contextPath + "/applications/teamwork/issue/issueAjaxControllerJson.jsp", request, function (response) {
        jsonResponseHandling(response);
        if (response.ok) {
          showFeedbackMessageInDiv("OK", "<%=I18n.get("ISSUE_CORRECTLY_SAVED")%>", "createIssuesExDiv");
          //$("#ISSUE_DESCRIPTION").val("");
          $("body").trigger("issueEvent", [
            {type: "save", response: response}
          ])
        }
        hideSavingMessage();
      });
    }
  }

</script>
<%

  Operator logged = pageState.getLoggedOperator();

  PageSeed self = pageState.pagePart(request);
  Form f = new Form(self);
  f.id = "CREATEISSUE_FORM";
  f.additionalParams="novalidate";
  pageState.setForm(f);

  f.start(pageContext);


  pageState.getEntryOrDefault("ISSUE_TASK");
  pageState.getEntryOrDefault("ISSUE_STATUS", IssueStatus.getStatusOpen().getId()+"");
  pageState.getEntryOrDefault("ISSUE_GRAVITY",Issue.GRAVITY_MEDIUM);


  PageSeed ps = new PageSeed(request.getContextPath() + "/applications/teamwork/issue/issueList.jsp");
  ButtonLink sub = new ButtonLink(ps);
  sub.iconChar = "i";
  sub.label = "";


%>
<div style="float:right;padding-top: 15px">
  <%//sub.toHtmlInTextOnlyModality(pageContext);
  sub.label=I18n.get("ADD_ISSUE");
    sub.iconChar="";
  %>
</div>
<h1><%sub.toHtmlInTextOnlyModality(pageContext);%>
</h1>
<div class="issueRow issueRowFocused">


  <table id="issueCreateLine" width="100%" border="0">
    <tr>

      <td nowrap valign="top" colspan="3"><%

        TextArea ta = new TextArea(I18n.get("NOTES"), "ISSUE_DESCRIPTION", "", 70, 10, "");
        ta.script = "style='height:123px;width:100%;'";
        ta.label = "";
        ta.toolTip = I18n.get("ISSUE_DESCRIPTION");
        ta.required = true;
        ta.toHtml(pageContext);

      %></td>
    </tr>
    <tr>
      <td colspan="3"><%
        TagBox tags = new TagBox("ISSUE_TAGS", Issue.class, Person.getLoggedPerson(pageState).getArea());
        tags.label = "";
        tags.separator = "";
        tags.script = "style='width:100%'";
        tags.innerLabel = I18n.get("ADD_TAGS");
        tags.toHtml(pageContext);

      %></td>
    </tr>
    <tr>
      <td>
        <div id="createIssues"><%
          SmartCombo taskSC = TaskBricks.getTaskCombo("ISSUE_TASK", true, TeamworkPermissions.issue_canCreate, pageState);
          taskSC.fieldSize = 20;
          taskSC.separator = "<br>";
          taskSC.preserveOldValue = false;
          taskSC.addAllowed = true;
          taskSC.required = true;
          taskSC.label = I18n.get("ISSUE_TASK");
          taskSC.toHtmlI18n(pageContext);
          //inhibit submit
        %></div>
      </td>
      <td align="left" nowrap><%

        if (Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("USETYPEONISSUES"))) {


          SmartCombo sc = IssueBricks.getIssueTypeCombo("ISSUE_TYPE", null, pageState);
          sc.separator = "<br>";
          sc.label = I18n.get("ISSUE_TYPE");
          sc.fieldSize = 10;
          sc.toHtml(pageContext);

        }
      %> </td>

      <td><%
        //SmartCombo requester = ResourceBricks.getPersonCombo("ASSIGNED_BY", TeamworkPermissions.resource_canRead, false, null, pageState);
        SmartCombo requester = ResourceBricks.getResourceCombo("ASSIGNED_BY", TeamworkPermissions.resource_canRead, null, Resource.class,pageState);
        requester.label = I18n.get("ASSIGNED_BY");
        requester.fieldSize = 20;
        requester.separator = "<br>";
        requester.toHtml(pageContext);

      %></td>
    </tr>
    <tr>
      <td><%

        ColorValueChooser cvcG = IssueBricks.getGravityChooser("ISSUE_GRAVITY", "ISSGRLIN", false, false, pageState);
        cvcG.height = 30;
        cvcG.label = "";
        cvcG.label = I18n.get("ISSUE_GRAVITY");
        cvcG.separator = "<br>";
        cvcG.showOpener = true;
        cvcG.toHtml(pageContext);

      %></td>
      <td valign="top"><%


        ColorValueChooser cvc = IssueBricks.getStatusChooser("ISSUE_STATUS", "STATUSLINE", pageState);
        cvc.label = I18n.get("ISSUE_STATUS");
        cvc.height = 30;
        cvc.separator = "<br>";
        cvc.showOpener = true;
        cvc.toHtml(pageContext);


      %></td>

      <td><%

        DateField df = new DateField("ISSUE_DATE_CLOSE_BY");
        df.separator = "<br>";
        df.labelstr = I18n.get("ISSUE_DATE_CLOSE_BY");
        df.toHtml(pageContext);

      %></td>
    </tr>

  </table>
  <div class="editButton"><%

    ButtonBar bar = new ButtonBar();
    bar.buttonAreaHtmlClass="buttonArea notFix";
    ButtonJS savebutton = new ButtonJS(I18n.get("SAVE"), "saveAndReloadIssues($(this));");
    savebutton.additionalCssClass = "first";
    bar.addButton(savebutton);
    bar.toHtml(pageContext);

  %></div>
</div>
<%
  f.end(pageContext);
%></div>


