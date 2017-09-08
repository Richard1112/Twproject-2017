<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.task.Task, com.twproject.task.businessLogic.TaskController, com.twproject.waf.TeamworkPopUpScreen,org.jblooming.utilities.StringUtilities, org.jblooming.waf.ScreenArea,
org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.TextField,org.jblooming.waf.html.state.Form,org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonSupport, net.sf.json.JSONObject, java.util.Iterator, java.util.Map, com.twproject.security.TeamworkPermissions" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new TaskController(), request);
    body.areaHtmlClass="lreq20 lreqPage";
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    //this is set by action
    Task task = Task.load(pageState.mainObjectId);
    pageState.setMainObject(task);

    PageSeed ps = pageState.thisPage(request);
    ps.command = "";
    ps.mainObjectId = task.getId();
    Form form = new Form(ps);
    form.alertOnChange = true;
    form.start(pageContext);
    pageState.setForm(form);


%>
<div class="mainColumn">
<%

  //to do siamo sicuri che sia questo il permesso giusto?
  //if (task.bricks.canManageSecurity) {
  if (task.hasPermissionFor(logged, TeamworkPermissions.task_canWrite)) { // bicch 25/08/16

    /*-------------------------------------------------------------------*/
    /*  TASK PUBLIC PAGE                                                 */
    /*-------------------------------------------------------------------*/
    %><h2><%=I18n.get("ENABLE_PUBLIC_PROJECT_PAGE")%></h2><%


  /*make*/
  JSONObject jsonData = task.getJsonData();
  if (jsonData.has("publicPage")) {
    JSONObject options = jsonData.getJSONObject("publicPage");
    Iterator i = options.entrySet().iterator();
    while (i.hasNext()) {
      Map.Entry entry = (Map.Entry) i.next();
      pageState.addClientEntry(entry.getKey() + "", entry.getValue() + "");
    }
  }


  boolean isPublicTask = pageState.getEntry("MASTER_PUBLIC_TASK").checkFieldValue();
  boolean isrequiredKey = pageState.getEntry("PUBLIC_TASK_REQUIRED_KEY").checkFieldValue();
  boolean showAssig = pageState.getEntry("PUBLIC_TASK_ASSIGNEE").checkFieldValue();
  boolean publicAddProposal = pageState.getEntry("PUBLIC_TASK_ADD_ISSUES").checkFieldValue();

  CheckField isPublic = CheckField.getMasterCheckField("MASTER_PUBLIC_TASK", "PUBLIC_TASK_");
  isPublic.separator = "&nbsp";
  isPublic.selector = StringUtilities.replaceAllNoRegex(isPublic.selector, ":enabled", "");
  String script = "{ var selector=$('" + isPublic.selector + "');";
  isPublic.label = I18n.get("ENABLE_PUBLIC_PROJECT_PAGE");
  isPublic.additionalOnclickScript = isPublic.additionalOnclickScript + script +
          "if (this.checked) { " +
          "  selector.each(function(){$(this).removeAttr('disabled')});" +
          "} else { " +
          "  selector.each(function(){$(this).attr('disabled','true')}); }; " +
          "}" +
          " $('#taskPublicUrl').toggle();";

  String scriptUnCheckAll = "var selector=$('" + isPublic.selector + "');" + "selector.each(function() {" +
          "this.checked = false;var name = this.id; name = name.substring('ck_'.length,name.length); var hidden = $('input#'+name);hidden.val('" + Fields.FALSE + "');});";


  isPublic.additionalOnclickScript = isPublic.additionalOnclickScript + "if (this.checked==false) { " + scriptUnCheckAll + "$('#taskPublicKey').hide(); };";
  isPublic.selector = "";       // trick: if the selector is != null the checkfield works as a checkall - uncheckall

  CheckField publicIssues = new CheckField(I18n.get("PUBLIC_TASK_ISSUES"), "PUBLIC_TASK_ISSUES", "&nbsp;", false);

  CheckField publicIssuesCField = new CheckField(I18n.get("PUBLIC_TASK_ISSUES_CFIELDS"), "PUBLIC_TASK_ISSUES_CFIELDS", "&nbsp;", false);
  CheckField addFiles = new CheckField(I18n.get("PUBLIC_TASK_ADD_FILE"), "PUBLIC_TASK_ADD_FILE", "&nbsp;", false);

  CheckField publicWorklogTotal = new CheckField(I18n.get("PUBLIC_TASK_WORKLOG"), "PUBLIC_TASK_WORKLOG", "&nbsp;", false);

  CheckField publicCosts = new CheckField(I18n.get("PUBLIC_TASK_COSTS"), "PUBLIC_TASK_COSTS", "&nbsp;", false);

  CheckField publicAdditionaCosts = new CheckField(I18n.get("PUBLIC_TASK_ADDITIONAL_COSTS"), "PUBLIC_TASK_ADDITIONAL_COSTS", "&nbsp;", false);

  CheckField addProposal = new CheckField(I18n.get("PUBLIC_TASK_ADD_ISSUES"), "PUBLIC_TASK_ADD_ISSUES", "&nbsp;", false);
  addProposal.additionalOnclickScript = "$('#publicIssuesCF').toggle(); if(this.checked==false){$('#ck_PUBLIC_TASK_INTRANET_NO_CAPTCHA').attr('checked','');$('#ck_PUBLIC_TASK_ISSUES_CFIELDS').attr('checked','');$('#ck_PUBLIC_TASK_ISSUES_STATUS').attr('checked','');$('#ck_PUBLIC_TASK_ADD_FILE').attr('checked','')}";

  CheckField publicAssignee = new CheckField(I18n.get("PUBLIC_TASK_ASSIGNEE"), "PUBLIC_TASK_ASSIGNEE", "&nbsp;", false);
  publicAssignee.additionalOnclickScript = "$('#taskPublicAssignee').toggle();if(this.checked==false){$('#ck_PUBLIC_TASK_WORKLOG').attr('checked','')}";

  //CheckField publicChildren = new CheckField(I18n.get("PUBLIC_TASK_CHILDREN"), "PUBLIC_TASK_CHILDREN", "&nbsp;", false);

  CheckField requiredKey = new CheckField(I18n.get("PUBLIC_TASK_REQUIRED_KEY"), "PUBLIC_TASK_REQUIRED_KEY", "&nbsp;", false);
  requiredKey.additionalOnclickScript = "$('#taskPublicKey').toggle();";

  CheckField showProjectSummary = new CheckField(I18n.get("PUBLIC_TASK_SHOW_SUMMARY"), "PUBLIC_TASK_SHOW_SUMMARY", "&nbsp;", false);
  CheckField showGantt = new CheckField(I18n.get("PUBLIC_TASK_SHOW_GANTT"), "PUBLIC_TASK_SHOW_GANTT", "&nbsp;", false);


  CheckField noCaptcha = new CheckField(I18n.get("PUBLIC_TASK_INTRANET_NO_CAPTCHA"), "PUBLIC_TASK_INTRANET_NO_CAPTCHA", "&nbsp;", false);

  CheckField hideIfClosed = new CheckField(I18n.get("PUBLIC_TASK_HIDE_IF_CLOSED"), "PUBLIC_TASK_HIDE_IF_CLOSED", "&nbsp;", false);


  if (!isPublicTask) {
    //publicChildren.disabled = true;
    publicAdditionaCosts.disabled = true;
    publicIssuesCField.disabled = true;
    publicIssues.disabled = true;
    publicWorklogTotal.disabled = true;
    publicCosts.disabled = true;
    addProposal.disabled = true;
    publicAssignee.disabled = true;
    requiredKey.disabled = true;
    hideIfClosed.disabled = true;
    showGantt.disabled=true;
    showProjectSummary.disabled=true;
  }

%>
<table width="100%" border="0">
  <tr>
    <td height="60"><%isPublic.toHtml(pageContext);%></td><td>
      <div id="taskPublicUrl" <%=isPublicTask ? "" : "style=\"display:none;white-space:nowrap\""%>>
        <%
          pageState.addClientEntry("PUBLIC_TASK_URL", ApplicationState.serverURL + "/project/" + task.getId());
          TextField link = new TextField("PUBLIC_TASK_URL", "&nbsp;&nbsp;");
          link.label = I18n.get("PUBLIC_PAGE_ADDRESS");
          link.fieldSize = 40;
          link.separator = "<br>";
          link.readOnly = true;
          link.toHtml(pageContext);
        %> &nbsp;&nbsp;<a target="_blank" href="<%=ApplicationState.serverURL + "/project/" + task.getId()%>"><%=I18n.get("LINK")%> </a>
      </div>
    </td>
  </tr>
  <tr>
    <td valign="bottom" nowrap><%requiredKey.toHtml(pageContext);%></td><td valign="top">
    <div id="taskPublicKey" <%=isrequiredKey ? "" : "style=\"display:none;\""%>>
      <%
        TextField key = new TextField("PUBLIC_TASK_KEY", "<br>");
        key.label = I18n.get("INSERT_KEY") + ":";
        key.fieldSize = 40;
        key.toHtml(pageContext);
      %>
    </div>
  </td>
  </tr>

  <tr>
    <td ><%showProjectSummary.toHtml(pageContext);%></td>
    <td ><%showGantt.toHtml(pageContext);%></td>
  </tr>
  <%--<tr>
    <td colspan="2"><%publicChildren.toHtml(pageContext);%></td>
  </tr>--%>
  <tr>
    <td colspan="2"><%publicIssues.toHtml(pageContext);%></td>
  </tr>
  <tr>
    <td colspan="2"><%publicCosts.toHtml(pageContext);%></td>
  </tr>
  <tr>
    <td colspan="2"><%publicAdditionaCosts.toHtml(pageContext);%></td>
  </tr>
  <tr>
    <td colspan="2" nowrap>
      <table cellpadding="0" cellspacing="0">
        <tr>
          <td><%publicAssignee.toHtml(pageContext);%></td>
          <td>
            <div id="taskPublicAssignee" <%=showAssig ? "" : "style=\"display:none;\""%>>
              <%publicWorklogTotal.toHtml(pageContext);%>
            </div>
          </td>
        </tr>
      </table>
  </tr>

  <tr>
    <td colspan="2">
      <table cellpadding="0" cellspacing="0">
        <tr>
          <td valign="top"><%addProposal.toHtml(pageContext);%></td>
          <td>
            <div id="publicIssuesCF" <%=publicAddProposal ? "" : "style=\"display:none;\""%>>
              <%addFiles.toHtml(pageContext);%><br>
              <%publicIssuesCField.toHtml(pageContext);%><br>
              <%noCaptcha.toHtml(pageContext);%>
            </div>
          </td>
        </tr>
      </table>
  </tr>
  <tr>
    <td colspan="2"><%hideIfClosed.toHtml(pageContext);%></td>
  </tr>
</table>
<%
  ButtonBar buttonBar = new ButtonBar();
  ButtonSubmit save = ButtonSubmit.getSaveInstance(pageState);
  save.variationsFromForm.setCommand("TASK_SAVE_PUBLICPAGE");
  save.additionalCssClass="first";
  buttonBar.addButton(save);

  ButtonJS embed=new ButtonJS(I18n.get("EMBED_GANTT_BTN"),"embed()");
  embed.additionalCssClass="small";
  embed.outputModality=ButtonSupport.TEXT_ONLY;

  buttonBar.addToRight(embed);
  buttonBar.toHtml(pageContext);
%>



<script>
  function embed(){

    var key="<%=task.getKey()%>";

    var id="gnt_"+new Date().getTime();

    var ndo = createModalPopup(600, 300, null, "rgba(255,255,255, .7)").append("<h2><%=I18n.get("EMBED_GANTT_TITLE")%></h2>")
      .append("<textarea rows=6 cols=70 class='embedCode' readonly></textarea><br>"+
        "<p><%=I18n.get("EMBED_GANTT_DESCR")%></p>");

    var inpW=$("<input type='text' size='3' id='w'>");
    inpW.val(800);
    var inpH=$("<input type='text' size='3' id='h'>");
    inpH.val(600);
    ndo.append("Change the size of your gantt. Width:").append(inpW );
    ndo.append("&nbsp;&nbsp;Height:").append(inpH);
    ndo.find("input").on("click blur keyup",function(){
      var ta=ndo.find("textarea");
      var w = inpW.val();
      var h = inpH.val();
      w=w<200?200:w;
      h=h<150?150:h;
      var txt="<iframe src='<%=ApplicationState.serverURL%>/widget/"+key+"' id='"+id+"' width='"+w+"' height='"+ h+"' style='display:none;'><"+"/iframe>"+
        "<script type='text/javascript' src='<%=ApplicationState.serverURL%>/applications/teamwork/task/gantt/wdg.js' onload=\"ld('"+id+"')\"><"+"/script>";
      ta.val(txt);
    });
    inpW.click();

  }
</script>


<%
} else {
%>
<table width="100%">
  <tr>
    <td height="200px;" valign="top"><%
      if (pageState.getEntry("MASTER_PUBLIC_TASK").checkFieldValue()) {
        pageState.addClientEntry("PUBLIC_TASK_URL", ApplicationState.serverURL + "/project/" + task.getId());
        TextField link = new TextField("PUBLIC_TASK_URL", "&nbsp;&nbsp;");
        link.label = I18n.get("PUBLIC_PAGE_ADDRESS");
        link.fieldSize = 50;
        link.readOnly = true;
        link.toHtml(pageContext);
        %>&nbsp;&nbsp;&nbsp;<a target="_blank" href="<%=ApplicationState.serverURL + "/project/" + task.getId()%>"><%=I18n.get("LINK")%> </a> <%
    } else {
      %><%=I18n.get("NO_PUBLIC_PAGE_FOR_THIS_TASK")%><%
        }
      %></td>
  </tr>
</table>
<%
  }
%></div>


<%
form.end(pageContext);

}

%>
