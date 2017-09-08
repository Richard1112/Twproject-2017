<%@ page import="com.twproject.task.Issue,
                 com.twproject.task.IssueHistory,
                 com.twproject.task.Task,
                 com.twproject.task.TaskStatus,
                 com.twproject.task.businessLogic.IssueAction,
                 com.twproject.waf.TeamworkPopUpScreen,
                 net.sf.json.JSONObject,
                 org.jblooming.oql.OqlQuery,
                 org.jblooming.persistence.PersistenceHome,
                 org.jblooming.system.SystemConstants,
                 org.jblooming.utilities.HtmlSanitizer,
                 org.jblooming.utilities.JSP,
                 org.jblooming.utilities.RecaptchaV2,
                 org.jblooming.utilities.StringUtilities,
                 org.jblooming.waf.ActionUtilities,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.constants.Fields,
                 org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.core.JspHelper,
                 org.jblooming.waf.html.display.Img,
                 org.jblooming.waf.html.input.TextField,
                 org.jblooming.waf.html.input.Uploader,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.ApplicationState,
                 org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.Hashtable, java.util.Map" %>
<%@page pageEncoding="UTF-8" %><%
  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    body.areaHtmlClass="lreq20 lreqPage";

    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    lw.menu = null;
    pageState.toHtml(pageContext);
  } else {

    String taskIdentifier = pageState.getEntry("TASK_ID").stringValue();
    Task task = null;
    if (JSP.ex(taskIdentifier)) {
      task = (Task) PersistenceHome.findByPrimaryKey(Task.class, taskIdentifier);
      if (task == null) { // then try to use code or name
        String hql = "select t from " + Task.class.getName() + " as t where t.code=:filter or t.name=:filter ";
        OqlQuery oql = new OqlQuery(hql);
        oql.getQuery().setString("filter", taskIdentifier);
        task = oql.list().size() > 0 ? (Task) oql.list().get(0) : null;
      }
    }

    Img logoTw = new Img(ApplicationState.getApplicationSetting(SystemConstants.PRINT_LOGO), "");


    PageSeed self = pageState.thisPage(request);
    self.addClientEntry("TASK_ID", taskIdentifier);


    Form form = new Form(self);
    pageState.setForm(form);
    form.encType = Form.MULTIPART_FORM_DATA;
    form.start(pageContext);
%>

<script type="text/javascript">
  var RecaptchaOptions = {
    theme: 'white'
  };
</script>
<style>

  #twMainContainerPopup {
    background-color: white;
    position: absolute;
    border: 10px solid #356a8c;
    overflow: auto;
    height: 100%;
  }

  #twInnerContainerPopup {
    position: relative;
    padding: 10px 50px 30px;
   }

  #twInnerContainer {
    padding: 0;
    min-width: auto;
  }

  .message {
    margin: 0;
    padding: 50px;
    font-style: italic;
    font-weight: 200;
    font-size: 30px;
    color: #000;
  }

  #tskAddIss.tab {
    background-color: rgba(47,151,198,0.2);
  }

  #tskAddIss.tab.tabSelected {
    background-color: #fff;
  }


  #tskAddIss.tab .button.textual {
    color: #2F97C6;
    font-weight: bold;
  }

</style>
<div id="twMainContainer">
<div id="twInnerContainer" style="margin:0 auto; text-align: center"><%

  // task founded
  if (task != null) {

    /*--------------------------------------------------------------------------------------------------------------------------------------*/
    /*                                                           CONTROLLER                                                                      */
    /*--------------------------------------------------------------------------------------------------------------------------------------*/
    if ("SEND".equals(pageState.getCommand())) {
      JSONObject jsonData = task.getJsonData();
      boolean nocaptcha = false;
      if (jsonData.has("publicPage")) {
        JSONObject options = jsonData.getJSONObject("publicPage");
        nocaptcha = Fields.TRUE.equals(options.get("PUBLIC_TASK_INTRANET_NO_CAPTCHA"));

        String description = pageState.getEntryAndSetRequired("ISSUE_DESCRIPTION").stringValue();
        String requesterName = pageState.getEntryAndSetRequired("REQUESTER_NAME").stringValue();
        String requesterCompany = pageState.getEntry("REQUESTER_COMPANY").stringValue();
        String extRequesterEmail= pageState.getEntryAndSetRequired("EXTERNAL_REQUESTER").stringValue();
        String doc = pageState.getEntry("DOCUMENT_UPLOAD").stringValueNullIfEmpty();


        String requestBody=I18n.get("ASSIGNED_BY")+": "+requesterName+"\n"+
          (JSP.ex(requesterCompany)?I18n.get("COMPANY")+": "+requesterCompany+"\n":"")+
          "----------------------------------\n"+description;


        boolean isResponseCorrect = false;
        if (!nocaptcha){
          RecaptchaV2 recaptcha = new RecaptchaV2("6LeGqCITAAAAANwJVFJhjMxha7m_OqJXs7U-Cbiz", "6LeGqCITAAAAAKfLkKiLYsvcEOwBGpGe1ciygt1j");
          isResponseCorrect = recaptcha.isValid(pageState,true);
        }

        if (isResponseCorrect || nocaptcha) {

          Issue issue = new Issue();
          issue.setIdAsNew();
          issue.setArea(task.getArea());
          issue.setTask(task);
          issue.setStatusOpen();

          //todo c'è un problema con l'apice singolo nel testo della issue
          issue.setDescription(JSP.ex(requestBody) ? HtmlSanitizer.getText(requestBody) : I18n.get("ISSUE_ADDED_FROM_PUBLIC_WITH_ATTACH"));
          issue.setGravity(Issue.GRAVITY_MEDIUM);

          issue.setCreator(extRequesterEmail);

          issue.setExtRequesterEmail(extRequesterEmail);
          for (int j = 1; j < 7; j++) {
            ActionUtilities.setString(pageState.getEntry("ISSUE_CUSTOM_FIELD_" + j), issue, "customField" + j);
          }
          issue.store();
          IssueHistory history = new IssueHistory(issue);
          history.store();
          //la email all'external requester la manda questo metodo
          IssueAction.createEventIssueAddedClosed(issue, true, null, extRequesterEmail);

          pageState.removeEntry("ISSUE_DESCRIPTION");

          Map<String, String> params = new Hashtable();
          params.put("ISSUE_CODE_ID", issue.getMnemonicCode() + "");
          params.put("TICKETS_REVIEW_URL", ApplicationState.serverURL + "/TICKETS/" + StringUtilities.generateKeyForEmail(extRequesterEmail));

          //ATTENZIONE NON si deve mandare il link a video, ma solo via e-mail per essere sicuri che il link arrivi solo al proprietario della mail
          pageState.addMessageInfo(I18n.get("ISSUE_CREATED_EXTERNAL_REQUEST_MESSAGE_SUBJECT", params) + "<hr>" + I18n.get("ISSUE_EXTERNAL_REQUEST_MAIL_SENT"));


          //attached file
          if (JSP.ex(doc)) {
            issue.addFile(Uploader.save(issue, "DOCUMENT_UPLOAD", pageState));
            issue.store();
          }

        } else {
          pageState.addMessageError(I18n.get("INVALID_CAPTCHA"));
        }
      }

    }
/*--------------------------------------------------------------------------------------------------------------------------------------*/
  /*                                                           CONTROLLER  END                                                                  */
/*--------------------------------------------------------------------------------------------------------------------------------------*/


    JSONObject jsonData = task.getJsonData();

    if (jsonData.has("publicPage")) {
      JSONObject options = jsonData.getJSONObject("publicPage");
      boolean isPublic = Fields.TRUE.equals(options.get("MASTER_PUBLIC_TASK"));
      boolean canSeeIssues = Fields.TRUE.equals(options.get("PUBLIC_TASK_ISSUES"));
      boolean canSeeIssuesCF = Fields.TRUE.equals(options.get("PUBLIC_TASK_ISSUES_CFIELDS"));
      boolean canSeeCosts = Fields.TRUE.equals(options.get("PUBLIC_TASK_COSTS"));
      boolean canSeeAdditionalCosts = Fields.TRUE.equals(options.get("PUBLIC_TASK_ADDITIONAL_COSTS"));
      boolean canSeeChildren = Fields.TRUE.equals(options.get("PUBLIC_TASK_CHILDREN"));
      boolean canSeeAssignee = Fields.TRUE.equals(options.get("PUBLIC_TASK_ASSIGNEE"));
      boolean canSeeWorklog = Fields.TRUE.equals(options.get("PUBLIC_TASK_WORKLOG"));
      boolean canAddIssue = Fields.TRUE.equals(options.get("PUBLIC_TASK_ADD_ISSUES"));
      boolean canAddFile = Fields.TRUE.equals(options.get("PUBLIC_TASK_ADD_FILE"));
      boolean isRequiredKey = Fields.TRUE.equals(options.get("PUBLIC_TASK_REQUIRED_KEY"));
      boolean nocaptcha = Fields.TRUE.equals(options.get("PUBLIC_TASK_INTRANET_NO_CAPTCHA"));
      boolean hideIfClosed = Fields.TRUE.equals(options.get("PUBLIC_TASK_HIDE_IF_CLOSED"));
      boolean showGantt = Fields.TRUE.equals(options.get("PUBLIC_TASK_SHOW_GANTT"));
      boolean showSummary = Fields.TRUE.equals(options.get("PUBLIC_TASK_SHOW_SUMMARY"));

      String keyEntry = pageState.getEntry("PUBLIC_TASK_KEY_INSERT").stringValueNullIfEmpty();
      if (JSP.ex(keyEntry))
        pageState.sessionState.setAttribute("PUBLIC_TASK_KEY_INSERT", keyEntry);
      else
        keyEntry = pageState.sessionState.getAttribute("PUBLIC_TASK_KEY_INSERT") + "";

      String taskKey = options.get("PUBLIC_TASK_KEY") + "";

    /*--------------------------------------------------------------------------------------------------------------------------------------*/
    /*                                                           ASK KEY IF REQUIRED                                                                      */
    /*--------------------------------------------------------------------------------------------------------------------------------------*/


      if ((isRequiredKey && JSP.ex(taskKey) && !(taskKey).equals(keyEntry)) || (isRequiredKey && JSP.ex(taskKey) && !JSP.ex(keyEntry))) {
%>
<style>

  #twMainContainerPopup {
    background-color: transparent!important;
  }

  #twMainContainer {
    background-color: transparent;
  }

  body {
    background-color: #356a8c;
  }

  .accessWrapper {
    background-color: #fff;
    position: relative;
    padding: 20px;
    min-height: 350px;
  }

  .accessWrapper, .loginBox {
    border-radius: 10px
  }

  .loginBox h4 {
    margin-bottom: 15px
  }

  .headingLogo {
    margin: 20px 0 40px;
    color: #fff
  }

  .loginBox {
    box-shadow: 0 0 100px rgba(0, 0, 0, 0.30);
    max-width: 450px;
    margin: 0 auto;
    background-color: #fff;
    padding: 0;
    position: relative;
  }

  .loginBox:before {
    top: 2px;
    left: 2px;
    content: "";
    width: 543px;
    height: 240px;
    position: absolute;
    background: url(data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiA/Pgo8c3ZnIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgd2lkdGg9IjEwMCUiIGhlaWdodD0iMTAwJSIgdmlld0JveD0iMCAwIDEgMSIgcHJlc2VydmVBc3BlY3RSYXRpbz0ibm9uZSI+CiAgPGxpbmVhckdyYWRpZW50IGlkPSJncmFkLXVjZ2ctZ2VuZXJhdGVkIiBncmFkaWVudFVuaXRzPSJ1c2VyU3BhY2VPblVzZSIgeDE9IjAlIiB5MT0iMCUiIHgyPSIwJSIgeTI9IjEwMCUiPgogICAgPHN0b3Agb2Zmc2V0PSIwJSIgc3RvcC1jb2xvcj0iIzMzNjA4MCIgc3RvcC1vcGFjaXR5PSIwLjQ0Ii8+CiAgICA8c3RvcCBvZmZzZXQ9IjEwMCUiIHN0b3AtY29sb3I9IiMzMzYwODAiIHN0b3Atb3BhY2l0eT0iMCIvPgogIDwvbGluZWFyR3JhZGllbnQ+CiAgPHJlY3QgeD0iMCIgeT0iMCIgd2lkdGg9IjEiIGhlaWdodD0iMSIgZmlsbD0idXJsKCNncmFkLXVjZ2ctZ2VuZXJhdGVkKSIgLz4KPC9zdmc+);
    background: -moz-linear-gradient(top, rgba(51, 96, 128, 0.44) 0%, rgba(51, 96, 128, 0) 100%);
    background: -webkit-gradient(linear, left top, left bottom, color-stop(0%, rgba(51, 96, 128, 0.44)), color-stop(100%, rgba(51, 96, 128, 0)));
    background: -webkit-linear-gradient(top, rgba(51, 96, 128, 0.44) 0%, rgba(51, 96, 128, 0) 100%);
    background: -o-linear-gradient(top, rgba(51, 96, 128, 0.44) 0%, rgba(51, 96, 128, 0) 100%);
    background: -ms-linear-gradient(top, rgba(51, 96, 128, 0.44) 0%, rgba(51, 96, 128, 0) 100%);
    background: linear-gradient(to bottom, rgba(51, 96, 128, 0.44) 0%, rgba(51, 96, 128, 0) 100%);
    filter: progid:DXImageTransform.Microsoft.gradient(startColorstr = '#70336080', endColorstr = '#00336080', GradientType = 0);

    transform: rotate(34.5deg);
    -webkit-transform: rotate(34.5deg);
    -moz-transform: rotate(34.5deg);
    -o-transform: rotate(34.5deg);
    -ms-transform: rotate(34.5deg);

    transform-origin: 0 0;
    -webkit-transform-origin: 0 0;
    -ms-transform-origin: 0 0;
    z-index: 0
  }

  .button.full {
    width: 100%
  }

  .formElements {
    background-color: #eee;
  }

  #__FEEDBACKMESSAGEPLACE {
    position: fixed;
    top: 0;
    width: 100%;
  }




</style>
<div class="headingLogo"><%new Img("logo_login.png", "", "", "").toHtml(pageContext);%></div>
<div class="loginBox">
  <div class="accessWrapper">
    <h4 style="font-weight: 300; margin: 40px 0;"><%=I18n.get("TEAMWORK_PUBLIC_PAGE")%>
    </h4>
    <table align="center">
      <%

        if ((isRequiredKey && JSP.ex(taskKey) && !(taskKey).equals(keyEntry))) {
          pageState.getEntry("PUBLIC_TASK_KEY_INSERT").errorCode = I18n.get("INCORRECT_KEY");
        }
      %>
      <tr>
        <td nowrap style="text-align: left"><%
          TextField insertKey = new TextField("PUBLIC_TASK_KEY_INSERT", "<br>");
          insertKey.label = I18n.get("INSERT_KEY");
          insertKey.fieldClass = "formElements formElementsBig light";
          insertKey.fieldSize = 20;
          insertKey.toHtml(pageContext);
        %></td>
      </tr>
      <tr>
        <td height="70">
          <%
            ButtonSubmit save = ButtonSubmit.getSaveInstance(pageState.getForm(), I18n.get("PROCEED"));
            save.additionalCssClass = "first big full";
            save.toHtml(pageContext);
          %></td>
      </tr>
    </table>
  </div>
</div>
<%
  /*--------------------------------------------------------------------------------------------------------------------------------------*/
  /*                                                          END KEY                                                                   */
  /*--------------------------------------------------------------------------------------------------------------------------------------*/
} else {

  // if public cheeck if it is open/in time period and if the user ask to hide if closed
  if (isPublic && hideIfClosed) {
    isPublic = TaskStatus.STATUS_ACTIVE.equals(task.getStatus());
  }
  if (isPublic) {
%>
<div class="accessWrapper" style="margin: 0 auto; max-width: 1400px; text-align: left">
  <table border="0" cellpadding="5" cellspacing="0" class="table noprint">
    <tr>
      <td align="left" valign="middle"><h1><%=task.getDisplayName()%></h1></td>
      <td align="right" width="180"><%


        if (task!=null && JSP.ex(task.bricks.getImageUrl())){
          Img logo = new Img(task.bricks.getImageUrl(),"");
          logo.height="100";
          logo.toHtml(pageContext);
        }

        %></td>
    </tr>
  </table>

  <%

    JspHelper dcheck = new JspHelper("/applications/teamwork/task/partTaskPublic.jsp");
    dcheck.parameters.put("task", task);
    dcheck.parameters.put("canSeeCosts", canSeeCosts);
    dcheck.parameters.put("canSeeAdditionalCosts", canSeeAdditionalCosts);
    dcheck.parameters.put("canSeeChildren", canSeeChildren);
    dcheck.parameters.put("canSeeIssues", canSeeIssues);
    dcheck.parameters.put("canSeeIssuesCF", canSeeIssuesCF);
    dcheck.parameters.put("canSeeWorklog", canSeeWorklog);
    dcheck.parameters.put("canSeeAssignee", canSeeAssignee);
    dcheck.parameters.put("canAddIssue", canAddIssue);
    dcheck.parameters.put("canAddFile", canAddFile);
    dcheck.parameters.put("showGantt", showGantt);
    dcheck.parameters.put("showSummary", showSummary);
    dcheck.parameters.put("noCaptcha", nocaptcha);
    dcheck.toHtml(pageContext);

  %></div><%
} else {  //task not public
%>
<table border="0" data-bulk="true" id="multi" cellpadding="5" cellspacing="3" class="table noprint">
  <tr>
    <td class="message" align="center">
      <%logoTw.toHtml(pageContext);%><br>
      You are searching for a task which is non-existent or that you cannot access. After selecting the "enable public
      page" did you save?
    </td>
  </tr>
</table>
<% }
}
}
} else {  // task null
%>
<table border="0" id="multi" cellpadding="5" cellspacing="3" class="table noprint">
  <tr>
    <td colspan="3" class="message" align="center">
      <%logoTw.toHtml(pageContext);%><br>
      You are searching for a task which is non-existent or that you cannot access. After selecting the "enable public
      page" did you save?
    </td>
  </tr>
</table>
<%
  }
  form.end(pageContext);
%></div>

</div>
<table class="table" style="position: absolute; bottom: 10px; right:10px">
  <tr>
    <td align="right">
      <small>Page generated by Twproject - help and info on <a target="_blank" href="http://twproject.com">Twproject
        website</a>.
      </small>
      <a target="_blank" href="http://twproject.com"><%logoTw.style="width: 100px; opacity: 0.4; float: right; margin: -42px 20px 0 20px;";
        logoTw.toHtml(pageContext);%></a>
    </td>
  </tr>
</table>
<%
  }
%>
