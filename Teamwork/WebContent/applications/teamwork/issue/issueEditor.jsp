<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.resource.Person, com.twproject.resource.Resource, com.twproject.resource.ResourceBricks, com.twproject.security.TeamworkPermissions, com.twproject.task.*,
                 com.twproject.task.businessLogic.IssueController, com.twproject.waf.TeamworkPopUpScreen, com.twproject.worklog.Worklog, net.sf.json.JSONArray, org.jblooming.designer.DesignerField,
                 org.jblooming.messaging.MessagingSystem, org.jblooming.ontology.PersistentFile,
                 org.jblooming.oql.OqlQuery, org.jblooming.persistence.PersistenceHome, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields,
                 org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.core.JST, org.jblooming.waf.html.core.JspHelper,
                 org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.html.input.*, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List, java.util.Date, org.jblooming.utilities.StringUtilities" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new IssueController(), request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);

    pageState.perform(request, response).toHtml(pageContext);

  } else {
    Issue issue = (Issue) pageState.getMainObject();

    Issue lastSavedIssue=null;
    if ( "SAVE_AND_ADD_NEW".equals(pageState.command)){
      lastSavedIssue=Issue.load(pageState.getEntry("_LAST_SAVED_ISSUE").intValueNoErrorCodeNoExc()+"");
    } else if (Commands.SAVE.equals(pageState.command)){
      lastSavedIssue=issue;
    }


%><script type="text/javascript">
  {
    var closeBl=<%=Commands.SAVE.equals(pageState.command)%>;
    var lastSavedIssue=<%=lastSavedIssue==null?"undefined":lastSavedIssue.jsonify()%>;
    var isValid=<%=pageState.validEntries()%>;
    var askForNotes=<%=pageState.getEntry("ASK_FOR_COMMENT").stringValue()%>;
    var askForWorklog=<%=pageState.getEntry("ASK_FOR_WORKLOG").stringValue()%>;

    if(isValid && lastSavedIssue)
      getBlackPopupOpener().$("body").trigger("issueEvent",[{type:"save",response:{issue:lastSavedIssue, askForNotes:askForNotes, askForWorklog:askForWorklog}}]);

  if (isValid && closeBl)
      closeBlackPopup(lastSavedIssue);
  }

    </script><%


  boolean isNew = PersistenceHome.NEW_EMPTY_ID.equals(issue.getId());

  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
  Person loggedPerson = logged.getPerson();

  Task task = issue.getTask();
  if (isNew) {
    String taskId = pageState.getEntry("ISSUE_TASK").stringValueNullIfEmpty();
    if (taskId != null)
      task = (Task) PersistenceHome.findByPrimaryKey(Task.class, taskId);
  }
  boolean taskIsSet = task != null;

  Resource assignedTo = issue.getAssignedTo();
  boolean resIsSet = assignedTo != null;

  boolean resIsMe = resIsSet && assignedTo.equals(loggedPerson);

/*
________________________________________________________________________________________________________________________________________________________________________


begin draw

________________________________________________________________________________________________________________________________________________________________________

*/
  PageSeed self = pageState.thisPage(request);
  self.mainObjectId = issue.getId();
  self.setCommand(Commands.EDIT);
  self.setPopup(true);

  Form f = new Form(self);
  f.alertOnChange = true;
  f.encType = Form.MULTIPART_FORM_DATA;
  pageState.setForm(f);
  f.start(pageContext);


  boolean issuCanWrite = issue.hasPermissionFor(logged,TeamworkPermissions.issue_canWrite);

%>
<%-- ------------------------- UPLOADIZE -------------------------------------------- --%>
<input type="hidden" name="PENDING_PF" id="PENDING_PF">


<h2><%=issue.isNew() ? I18n.get("ADD_ISSUE") : ("I#"+issue.getMnemonicCode()+"#")%> <span style="font-size:16px;padding-top:14px;">
<%
  if (task != null) {
    Date startDate = task.getSchedule().getStartDate();
    Date endDate = task.getSchedule().getEndDate();
    Date now=new Date();

    boolean startInError=false;
    String startErrorMessage="";
    if (TaskStatus.STATUS_SUSPENDED.equals(task.getStatus()) && task.getPreviousesSize()>0 && startDate.before(now) ){
      startInError=true;
      startErrorMessage+=I18n.get("TASK_SHOULD_BE_ALREADY_OPEN");
    }

    boolean endInError=false;
    String endErrorMessage="";
    if (TaskStatus.STATUS_ACTIVE.equals(task.getStatus()) &&  endDate.before(now) ){
      endInError=true;
      endErrorMessage+=I18n.get("TASK_SHOULD_BE_ALREADY_CLOSED");
    }


    %>
    <%=I18n.get("ISSUE_FOR_TASK")%>: <b><%=task.getDisplayName()%></b><br>
      <span class='pathCode' >
     <%=I18n.get("START")%> <b class="<%=startInError?"warning":""%>" title="<%=startErrorMessage%>"><%=JSP.w(startDate)%></b>&nbsp;&nbsp;&nbsp;
      <%=I18n.get("END")%> <b class="<%=endInError?"warning":""%>" title="<%=endErrorMessage%>"><%=JSP.w(endDate)%></b>&nbsp;&nbsp;&nbsp;
      (<%=I18n.get("DAYS")%>: <b><%=task.getDuration()%></b>)
    </span>
    <%


} else if (issue.getAssignedTo()!=null){
%><%=I18n.get("TODO_OF")%>: <%=issue.getAssignedTo().getDisplayName()%><%
} else {
%>&nbsp;<%
  }
%></span></h2>

<%-------------------------------------------------------------------------------   EDITOR  START -------------------------------------------------------------------------------%>

<table class="table" id="issueEditorTable" issueId="<%=issue.getId()%>" border="0" cellpadding="4">
<tr>
  <td colspan="3"><div style="display:inline-block;margin-right: 30px"><%
    pageState.getEntryOrDefault("ISSUE_GRAVITY",Issue.GRAVITY_HIGH);
    ColorValueChooser cvc = IssueBricks.getGravityChooser("ISSUE_GRAVITY","ISSUE_GRAVITY",false, false, pageState);
    cvc.height = 30;
    cvc.separator = "<br>";
    cvc.label=I18n.get("ISSUE_GRAVITY");
    cvc.readOnly = !issuCanWrite;
    cvc.showOpener = true;
    cvc.toHtml(pageContext);

  %></div><div style="display:inline-block;"><%
    cvc = IssueBricks.getStatusChooser("ISSUE_STATUS","STATUS",pageState);
    cvc.readOnly = !issuCanWrite;
    cvc.displayValue = true;
    cvc.height = 20;
    //cvc.width = 20;
    cvc.label=I18n.get("ISSUE_STATUS");
    cvc.separator = "<br>";
    cvc.showOpener = true;
    cvc.toHtml(pageContext);

%>
  </td>
</tr>
<tr><td colspan="3"> <%

  TextArea taD = new TextArea("ISSUE_DESCRIPTION", "<br>", 57, 8, null);
  taD.readOnly = !issuCanWrite;
  taD.required = true;
  taD.maxlength=0;

  taD.script = " style=\"width:100%;\"";
  taD.setAutosize(200,600,30);
  taD.toHtmlI18n(pageContext);

  boolean usetypeonissues = Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("USETYPEONISSUES"));
  boolean usecodeonissues = Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("USECODEONISSUES"));
  boolean useimpactonissues = Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("USEIMPACTONISSUES"));

%></td>
</tr>
<tr>
  <td  colspan="<%=usecodeonissues?2:3%>"><%

    TagBox tags= new TagBox("ISSUE_TAGS",Issue.class,issue.getArea());
    tags.label=I18n.get("TAGS")+":";
    tags.separator="<br>";
    tags.innerLabel=I18n.get("ADD_TAGS");
    tags.script += " style='width:90%'";
    tags.toHtml(pageContext);

  %></td>
  <%    if (usecodeonissues) { %>
  <td valign="top" colspan="3">
    <%
      TextField ic= new TextField("ISSUE_CODE","<br>");
      ic.label=I18n.get("ISSUE_CODE");
      ic.readOnly=!issuCanWrite;
      ic.fieldSize=10;
      ic.maxlength=30;
      ic.toHtml(pageContext);
    %></td><% }
%>
</tr>

<tr><%
  PageSeed psET = pageState.pageFromRoot("task/taskOverview.jsp");
  psET.setCommand(Commands.EDIT);

  boolean showActiveOnly = !JSP.ex(issue.getDescription()); // in order to see only active task when creating

  SmartCombo taskSC = TaskBricks.getTaskCombo("ISSUE_TASK" , showActiveOnly, TeamworkPermissions.issue_canWrite, pageState);
  taskSC.readOnly=!issuCanWrite;
  taskSC.required=true;
  taskSC.separator = "<br>";
  taskSC.fieldSize =40;
  taskSC.addLinkToEntity(psET,  I18n.get("HIS_TASKEDITOR"));
  taskSC.onValueSelectedScript="reloadAssignee('"+issue.getId()+"');";

%><td><%taskSC.toHtmlI18n(pageContext);%></td>
  <td><%=I18n.get("ASSIGNEE")%><br><div id="resCombo"><%

    JspHelper issueResDraw= new JspHelper("/applications/teamwork/issue/issueResourceDrawer.jsp");
    issueResDraw.parameters.put("SIZE", 30);
    issueResDraw.parameters.put("ISSUE",issue);
    issueResDraw.toHtml(pageContext);

  %></div></td>
  <%
    if (usetypeonissues) {
      SmartCombo sc = IssueBricks.getIssueTypeCombo("ISSUE_TYPE", issue, pageState);
      sc.readOnly = !issuCanWrite;
      sc.label=I18n.get("ISSUE_TYPE");
      sc.separator = "<br>";
  %><td nowrap><%
    sc.toHtmlI18n(pageContext);
  %></td><%
    }

  %>
</tr>

<tr>
  <td><div style="display:inline-block;margin-right: 50px"><%
    DateField df = new DateField("ISSUE_DATE_CLOSE_BY");
    df.readOnly = !issuCanWrite;
    df.separator = "<br>";
    df.toHtmlI18n(pageContext);

  %></div><div style="display:inline-block;"><%
    TextField et= TextField.getDurationInMillisInstance("ISSUE_WORKLOG_ESTIMATED_TIME");
    et.separator="<br>";
    et.label=I18n.get("ISSUE_WORKLOG_ESTIMATED_TIME");
    et.readOnly=!issuCanWrite;
    et.fieldSize=5;
    et.toHtml(pageContext);


    if (!issue.isNew()) {
      Container needMoreTime = new Container();
      needMoreTime.closeable=true;
      needMoreTime.status=Container.HIDDEN;
      needMoreTime.draggable=true;
      needMoreTime.width="300px";
      needMoreTime.height="200px";
      needMoreTime.title=I18n.get("ISSUE_WORKLOG_DELTA_ESTIMATED_TIME_SHORT");
      needMoreTime.start(pageContext);

      TextField deltaEstDone = TextField.getDurationInMillisInstance("ISSUE_WORKLOG_DELTA_ESTIMATED_TIME");
      deltaEstDone.separator="<br>";
      deltaEstDone.toolTip=I18n.get("ISSUE_WORKLOG_DELTA_ESTIMATED_TIME");
      deltaEstDone.fieldSize=3;
      deltaEstDone.readOnly=!issuCanWrite;
      deltaEstDone.toHtmlI18n(pageContext);%><br><%=I18n.get("ISSUE_WORKLOG_DELTA_CHANGE")
  %><div class="separator"></div><%
    ButtonSubmit modifyDurAndSave = ButtonSubmit.getSaveInstance(pageState);
    modifyDurAndSave.variationsFromForm.command="SAVE_DUR";
    modifyDurAndSave.label=I18n.get("GO");
    modifyDurAndSave.toHtml(pageContext);

    needMoreTime.end(pageContext);

    ButtonJS ocb = needMoreTime.getOpenerButton(true);
    ocb.toolTip=I18n.get("ISSUE_WORKLOG_DELTA_ESTIMATED_TIME");
    ocb.label=DateUtilities.getMillisInHoursMinutes(issue.getWorklogDone());

  %>&nbsp;<small><%=I18n.get("WORKLOG_DONE")%>:</small>&nbsp;<%ocb.toHtmlInTextOnlyModality(pageContext);%><%
    }

  %></div></td>
  <td><%
    df = new DateField("ISSUE_DATE_SIGNALLED");
    df.readOnly = !issuCanWrite;
    df.separator = "<br>";
    df.toHtmlI18n(pageContext);
  %></td>

    <%
      if (useimpactonissues) {
        %><td nowrap=""><%
        SmartCombo sc = IssueBricks.getIssueImpactCombo("ISSUE_IMPACT", issue, pageState);
        sc.readOnly = !issuCanWrite;
        sc.separator = "<br>";
        sc.toHtmlI18n(pageContext);
       %></td><%
      }
    %>

</tr>
<tr>
  <td><%

    //SmartCombo assignedBy = ResourceBricks.getPersonCombo("ASSIGNED_BY", TeamworkPermissions.resource_canRead, false, null, pageState);
    SmartCombo assignedBy = ResourceBricks.getResourceCombo("ASSIGNED_BY", TeamworkPermissions.resource_canRead, null, Resource.class,pageState);
    assignedBy.readOnly = !issuCanWrite;
    assignedBy.fieldSize = 40;
    assignedBy.separator = "<br>";
    assignedBy.toHtmlI18n(pageContext);
  %></td><td colspan="2"><%

  // ------ external requester email
  if (JSP.ex(issue.getExtRequesterEmail())) {
    TextField extRequesterEmail = TextField.getEmailInstance("EXT_REQUESTER_EMAIL");
    extRequesterEmail.readOnly = true;
    extRequesterEmail.separator = "<br>";
    extRequesterEmail.toHtmlI18n(pageContext);
    %>&nbsp;<a style="font-size: 70%;" target="PPP" href="<%=ApplicationState.serverURL + "/TICKETS/" + StringUtilities.generateKeyForEmail(issue.getExtRequesterEmail())%>"><%=I18n.get("ALL_REQUESTS")%></a><%
  }
%></td>




</tr>
<%


  //---------------------------------------------------------- CUSTOM FIELDS ----------------------------------------------------------
  if (IssueBricks.hasCustomField()) {
%>
<tr><td colspan="3" valign="middle"><%

  for (int i=1; i<7; i++) {
    DesignerField dfStr = DesignerField.getCustomFieldInstance( "ISSUE_CUSTOM_FIELD_",i, issue,!issuCanWrite, false, false, pageState);
    if (dfStr!=null){
      dfStr.separator="<br>";
%><div style="float:left;margin-right: 10px"><%dfStr.toHtml(pageContext);%></div><%
    }
  }

%></td></tr><%
  }

%>

<%---------------------------------------------------------- FILES ----------------------------------------------------------%>
<%
  JSONArray files= new JSONArray();
  for (PersistentFile pf : issue.getFiles())
    files.add(pf.jsonify());
%>
<tr><td colspan="3"><h2><span class="teamworkIcon" style="font-size: 90%">n</span> <%=I18n.get("FILES")%> </h2></td></tr>
<tr><td class="filesBox" colspan="99">
  <div class="uploadizeDrop" <%=issuCanWrite ?"":"disabled"%> style="display:none;" ></div>
</td>

<%
  //---------------------------------------------------------- COMMENTS HISTORY ----------------------------------------------------------
  JSONArray issHists= new JSONArray();

  if (!isNew) {
    for (IssueHistory his:issue.getIssueHistories()){
      issHists.add(his.jsonify());
    }
%><tr>
  <td colspan="3"><h2><span class="teamworkIcon" style="font-size: 90%">Q </span><%=I18n.get("ISSUE_COMMENTS")%></h2></td>
</tr>


<tr><td colspan="3">

  <table class="table" id="issHistTable" cellpadding="4"></table></td></tr>
<tr><td colspan="3" class="commEditPlace"></td></tr>
<%
  }

//-----------------------------------------------------  WORKLOG --------------------------------------------------------
  Assignment assignment=issue.getAssignmentOnTask(loggedPerson);
  String assId="";
  if (assignment!=null && !isNew) {
    assId=assignment.getId()+"";
    ButtonJS addWL= new ButtonJS("","openWorklogEditorPopup($(this),{assId:'"+assId+"',issueId:'"+issue.getId()+"',title:'"+I18n.get("issue")+": I#"+ issue.getId()+"#'})");
    addWL.iconChar="w";

%>
<tr><td colspan="2"><h2><span class="teamworkIcon" style="font-size: 90%">A </span><%=I18n.get("WORKLOGS")%></h2></td><td align="right"><%addWL.toHtmlInTextOnlyModality(pageContext);%></td></tr>
<tr><td colspan="3">
  <table class="table" cellpadding="4" id="worklogTable" border="0">
    <tr class="totals" id="worklogTotalRow" style="display: none;"><td align="right" colspan="3"><%=I18n.get("TOTAL")%>: <span id="worklogTotal"></span></td><td></td></tr>
  </table>
</td>
</tr>



<%} else {
  String feedback = I18n.get("ISSUE_WORKLOG_NOT_POSSIBLE");
  if (!taskIsSet) {
    feedback = I18n.get("ISSUE_WORKLOG_NOT_POSSIBLE_MISSING_TASK");
  } else if (!resIsMe) {
    feedback = I18n.get("ISSUE_WORKLOG_NOT_POSSIBLE_NOT_ASSIGNEE");
  }

%><tr><td class="warning"><%=feedback%>&nbsp;</td></tr><%
  }

  JSONArray worklogs= new JSONArray();
  for (Worklog workLog :issue.getWorklogs()) {
    worklogs.add(workLog.jsonify());
  }

%>


<%--------------------------------------- SUBSCRIPTIONS ------------------------------------------------------%>
<tr><td colspan="3">
  <%=I18n.get("ISSUE_SUBSCRIBE_CLOSE")%>:&nbsp;<%

  for (MessagingSystem.Media media : MessagingSystem.activeMedia) {

%>&nbsp;<%
  String subscrField = "ISSUE_SUBSCRIBE_CLOSE_" + media.toString().toUpperCase();

  CheckField mediaCf = new CheckField(subscrField, "", false);
  mediaCf.disabled = !issuCanWrite;
  mediaCf.label = I18n.get(media.toString().toUpperCase());
  mediaCf.toHtml(pageContext);
%>&nbsp;<%
  }
%></td></tr>

<tr><td colspan="3">
  <%
    ButtonBar bb = new ButtonBar();
    bb.loggableIdentifiableSupport = issue;

    ButtonSubmit save1 = ButtonSubmit.getSaveInstance(f, I18n.get("SAVE"),false);

    ButtonJS bjsSave= new ButtonJS(I18n.get("SAVE"),"saveCommentAndIssue();");

    bjsSave.enabled = issuCanWrite;
    bjsSave.additionalCssClass="first big";
    bb.addButton(bjsSave);

    if (!isNew) {
      ButtonJS delPrev = new ButtonJS(I18n.get("DELETE"),"deleteIssuePreview('"+ issue.getId()+"');" );
      delPrev.enabled = issue.hasPermissionFor(logged,TeamworkPermissions.issue_canDelete);
      delPrev.additionalCssClass="big delete";
      bb.addButton(delPrev);
    }

    if (isNew) {
      ButtonSubmit saveAndAddNew = ButtonSubmit.getSaveInstance(f, I18n.get("SAVE_AND_CLONE"), false);
      saveAndAddNew.enabled = issuCanWrite && issue.hasPermissionFor(logged,TeamworkPermissions.issue_canCreate);
      saveAndAddNew.variationsFromForm.setCommand("SAVE_AND_ADD_NEW");
      saveAndAddNew.additionalCssClass="big";
      bb.addButton(saveAndAddNew);
    } else {
      ButtonSubmit saveAndAddNew = ButtonSubmit.getSaveInstance(f, I18n.get("CLONE"), false);
      saveAndAddNew.enabled = issuCanWrite && issue.hasPermissionFor(logged,TeamworkPermissions.issue_canCreate);
      saveAndAddNew.variationsFromForm.setCommand("CLONE");
      saveAndAddNew.additionalCssClass="big";
      bb.addButton(saveAndAddNew);
    }

    if (!isNew && issue.getTask() != null) {
      ButtonSubmit upgradeToTask = new ButtonSubmit(f);
      upgradeToTask.enabled =  issue.getTask().hasPermissionFor(logged,TeamworkPermissions.task_canCreate);
      upgradeToTask.label = I18n.get("UPGRADE_TO_TASK");
      upgradeToTask.additionalOnClickScript = "changeModalSize(600, 400); ";
      upgradeToTask.variationsFromForm.setCommand(Commands.EDIT);
      upgradeToTask.variationsFromForm.href = pageState.pageInThisFolder("issueTransform.jsp", request).href;
      upgradeToTask.additionalCssClass="big";
      bb.addButton(upgradeToTask);
    }

    bb.toHtml(pageContext);

  %>
</td></tr></table>

<%----------------------------------------------- WL and NOTES EDITOR ---------------------------------------------------------%>
<jsp:include page="partIssueNotes.jsp"/>

<%----------------------------------------------- START TEMPLATES ---------------------------------------------------------%>
<div style="display:none;" id="ietmplt">

  <%=JST.start("ISSHISTPL")%>
  <tr histId="(#=obj.id#)" class="issueEditorObjects">
    <td valign="top" nowrap width="40">
        <img class="face small" src="(#=obj.ownerAvatarUrl#)">
    </td>
    <td valign="top" >
      <div class="commentHeader"><b>(#=obj.lastModifier#)</b> - <span title="(#=(new Date(obj.creationDate)).format()#)">(#=dateToRelative(new Date(obj.creationDate))#)</span></div>
      <div style="width: 100%; max-width: 750px; overflow-x: hidden">
      (#=obj.statusName?"<i><%=I18n.get("ISSUE_STATUS")%></i>: "+obj.statusName+"&nbsp;/&nbsp;":""#)
      (#=obj.assigneeName?"<i><%=I18n.get("ISSUE_ASSIGNED_TO")%></i>: "+obj.assigneeName+"&nbsp;/&nbsp;":""#)
      (#=obj.taskName?"<i><%=I18n.get("ISSUE_TASK")%></i>: "+obj.taskName+"<br>":""#)
      (#=obj.comment#)
      </div>
    </td>
    <td valign="top" align="right" class="issHistDelBtn" style="text-align: center; width:40px;vertical-align:top"><%
      ButtonJS delData = new ButtonJS("deleteNotes($(this))");
      delData.confirmRequire = true;
      delData.enabled = issuCanWrite;
      delData.label="";
      delData.iconChar="d";
      delData.additionalCssClass="delete";
      delData.toHtmlInTextOnlyModality(pageContext);
    %></td>
  </tr>
  <%=JST.end()%>

  <%=JST.start("WLTPL")%>
  <tr wlid="(#=obj.id#)" class="issueEditorObjects">
    <td valign="top"> <img class="face small" src="(#=obj.resAvatarUrl#)"> <b>(#=obj.resName#)</b> &nbsp;<small>(#=new Date(obj.insertedMillis).format()#)</small></td>
    <td><small>(#=obj.action#)</small></td>
    <td valign="top" align="right" workdone="(#=obj.duration#)">(#=getMillisInHoursMinutes(obj.duration)#)</td>
    <td valign="top" style="text-align: center; width:40px;vertical-align:top"><%
      delData = new ButtonJS("deleteWorklog($(this))");
      delData.confirmRequire = true;
      delData.confirmQuestion = I18n.get("CONFIRM_DELETE_WKL");
      delData.enabled = issuCanWrite;
      delData.label="";
      delData.iconChar="d";
      delData.additionalCssClass="delete";
      delData.toHtmlInTextOnlyModality(pageContext);
    %></td></tr>
  <%=JST.end()%>

  <%=JST.start("FILEBOX")%>
  <div class="repoFileBox issueEditorObjects">
    <span class="button icon textual delete" ><span class="teamworkIcon" title="<%=I18n.get("DELETE")%>">d</span></span>
    <div class="fileName">
      <div class="docLabelWrapper">
        <img src="<%=ApplicationState.contextPath%>/img/mime/(#=getMime(obj.mime)#).png" style="width: 20px">(#=obj.name#)
      </div>
    </div>
  </div>
  <%=JST.end()%>
</div>
<%----------------------------------------------- END TEMPLATES ---------------------------------------------------------%>


<%
  new DeletePreviewer("ISS_DEL",IssueController.class, pageState);

  f.end(pageContext);
%>
<script type="text/javascript">

function getMime(mime){
//  return mime;
  return mime.replace(/\//g, '_');
}

var issueId="<%=issue.getId()%>";
var loggedId="<%=logged.getId()%>";

$(function(){

  <%if (assignment!=null && !isNew) {%>
    showNotesEditor();
  <%}%>

  //bind for worklog event
  registerEvent("worklogEvent.issueEditor",function(ev,data){
    if (data.type=="save")
      worklogSaved(data.response);
  });

  $("#ietmplt").loadTemplates().remove();
  $.JST.loadDecorator("ISSHISTPL",function(histRow,hist){
    //console.debug("decorator ISSHISTPL",histRow,hist);
    if(hist.taskId || hist.assigneeId || hist.statusId){
      histRow.addClass("issueHistory").find(".issHistDelBtn").html("");
    }
  });

  $.JST.loadDecorator("FILEBOX", function (box, pf) {
    box.data("pf",pf);
    //bind open image/download file
    box.click(function (ev) {
      ev.stopPropagation();
      openPersistentFile($(this).data("pf"));
    });

    //delete file
    box.find(".delete").click(function(ev){
      ev.preventDefault();
      ev.stopPropagation();
      var el=$(this);
      var fb=el.closest(".repoFileBox");
      var editor=fb.closest("[issueID]");

      el.confirm(function(){

        showSavingMessage();
        var request = {CM: "DELSCRSHT", issueId:editor.attr("issueId"), fileUID: fb.data("pf").uid,ck:box.data("pf").ck};
        $.getJSON("issueAjaxControllerJson.jsp", request, function (response) {
          jsonResponseHandling(response);
          if (response.ok) {
            el.closest(".repoFileBox").fadeOut(200, function () {
              $(this).remove();
              if(!$(".repoFileBox").length)
                $(".noFiles").show();
            });
          }
          hideSavingMessage();
        });
      },"<%=I18n.get("FLD_CONFIRM_DELETE")%>");
    });
  });

  enableUpload($("#issueEditorTable"));

  <%
  if (Commands.SAVE.equals(pageState.command) && issue.getStatus().isAskForComment() && issue.getLastIssueHistory()!= null ) {

  %>
  showNotesForStatusChange("<%=issue.getLastIssueHistory().getId()%>");
  <%}%>

  drawWorklog();
  drawHistory();

  var filesArray = <%=files%>;
  drawFiles(filesArray);

});

function drawWorklog(){
  var worklogs=<%=worklogs%>;
  //console.debug("drawWorklog",worklogs);
  var ndo=$("#worklogTable");
  for (var i=worklogs.length-1;i>=0;i--){
    ndo.prepend($.JST.createFromTemplate(worklogs[i],"WLTPL"));
  }
  updateWlTotal();
}

function drawHistory(){
  var issHists=<%=issHists%>;
  //console.debug("drawHistory",issHists);
  var ndo=$("#issHistTable");
  for (var i=issHists.length-1;i>=0;i--){
    ndo.prepend($.JST.createFromTemplate(issHists[i],"ISSHISTPL"));
  }
}

function updateWlTotal(){
  var totWL=0;
  $("[workdone]").each(function(){
    totWL+=parseInt($(this).attr("workdone"))
  });
  if (totWL<=0)
    $("#worklogTotalRow").hide();
  else
    $("#worklogTotalRow").show();
  $("#worklogTotal").html(getMillisInHoursMinutes(totWL));
}



function reloadAssignee(issueId) {
  if ($("#ISSUE_TASK").isValueChanged()) {
    showSavingMessage();
    var taskId = $("#ISSUE_TASK").val();
    var resName = $("#ASSIGNEE_txt").val();
    var resId = $("#ASSIGNEE").val();
    var request = {CM: "TASKCHANGED", TASKID: taskId, ISSUEID: issueId,SIZE:30};
    $.getJSON("issueAjaxControllerJson.jsp",request,function(response){
      jsonResponseHandling(response);
      if (response.ok) {
        //si mettono i nuovi limiti sulle date
        $("#ISSUE_DATE_CLOSE_BY").attr("minvalue",response.task.startDate).attr("maxvalue",response.task.endDate);

      }
      hideSavingMessage();
    });
  }
}

function worklogSaved(response) {
  if (response.worklog) {
    var row = $.JST.createFromTemplate(response.worklog, "WLTPL");
    $("#worklogTable").prepend(row);
    updateWlTotal();
    row.effect("highlight", { color: "#F9EFC5" }, 1500);
  }
}

function deleteWorklog(el){
  showSavingMessage();
  var row = el.closest("[wlid]");
  var request = {
    CM:"DL",
    wlId:row.attr("wlid")
  };
  $.getJSON(contextPath+"/applications/teamwork/task/worklog/worklogAjaxController.jsp",request,function(response){
    jsonResponseHandling(response);
    if (response.ok) {
      row.remove();
      updateWlTotal();

    }
    hideSavingMessage();
  });
}

function showNotesEditor(issueId , histId){
  var commEditor = $("#insertComment").clone(false);
  $(".commEditPlace").append(commEditor);
  commEditor.attr("histId","-1").removeProp("id").show();
}

function showNotesForStatusChange(histId){ //questo viene visualizzato al top della pagina
  $("#floatWindow").append($("#insertStatusNotes"));
  $("#insertStatusNotes").attr("histId",histId).fadeIn();
}

function saveStatusNotes(el) {
  var theDiv = el.closest(".notesEditor");
  var histId = theDiv.attr("histId");
  showSavingMessage();
  var request = {CM:"SVHISTNOTES",histId:histId,notes:theDiv.find("textarea").val(),issueId:issueId};
  $.getJSON("issueAjaxControllerJson.jsp", request, function(response) {
    jsonResponseHandling(response);
    if (response.ok) {
      theDiv.fadeOut();
      theDiv.find("textarea").updateOldValue();
      if (response.history){
        var row = $.JST.createFromTemplate(response.history,"ISSHISTPL");
        $("#issHistTable").append(row);
        row.effect("highlight", { color: "#F9EFC5" }, 1500);
      }
    }
    hideSavingMessage();
  });
}

function saveComment(el,callback) {
  var theDiv = el.closest(".notesEditor");
  var histId = theDiv.attr("histId");
  showSavingMessage();
  var request = {CM:"SVHISTNOTES",histId:histId,notes:theDiv.find("textarea").val(),issueId:issueId};
  $.getJSON("issueAjaxControllerJson.jsp", request, function(response) {
    jsonResponseHandling(response);
    if (response.ok) {
      theDiv.find("textarea").val("");
      if (response.history){
        var row = $.JST.createFromTemplate(response.history,"ISSHISTPL");
        $("#issHistTable").append(row);
        row.effect("highlight", { color: "#F9EFC5" }, 1500);
      }
    }
    if (typeof (callback)=="function")
      callback();
    hideSavingMessage();
  });
}

function saveCommentAndIssue(){
  //console.debug("saveCommentAndIssue");
  function submitForm(){
    <%=save1.generateJs()%>;
  }
  var ta= $("#HIS_COMMENT");
   if (ta.isValueChanged()){
     saveComment(ta,submitForm())
   } else {
     submitForm();
   }

}


function deleteNotes(el){
  var row=el.closest("[histId]");
  var request = {CM:"DLHISTNOTES",histId:row.attr("histId")};
  $.getJSON("issueAjaxControllerJson.jsp", request, function(response) {
    jsonResponseHandling(response);
    if (response.ok) {
      row.remove();
    }
    hideSavingMessage();
  });

}

function enableUpload(view) {
  //console.debug("enableUpload",view)
  view.find(".uploadizeDrop").each(function () {

    $(this).uploadize({
      url                        : "issueAjaxControllerJson.jsp",
      maxSize                    :<%=Uploader.getMaxUploadSizeInByte()%>,
//      activeElement:               $(this).get(0),
      multi                      : true,
      //showPlaceHolder:false,
      additionalRequestParameters: {CM: "DROPDOC", issueId: "<%=issue.getId()%>"},
      onLoadCallback             : function (response) {

        $(".uploadizeDrop").hide();

        $(".noFiles").hide();

        //aggiunge il file
        $(".filesBox").append($.JST.createFromTemplate(response.file, "FILEBOX"));

        //in caso di nuova issue mette da una parte i pf temporanei
        if (<%=issue.isNew()? "true" : "false" %>) {
          var tempPF = $("#PENDING_PF").val();
          var pfs = tempPF ? JSON.parse(tempPF) : [];
          pfs.push(response.file.uid);
          $("#PENDING_PF").val(JSON.stringify(pfs));
        }
      }
    });
  });
  $(document).on("drop", function () {
    $(".uploadizeDrop").show();
    $(".uploadLabel").hide();
  });
}


function drawFiles(files){
  //console.debug("drawFiles",files);
  var ndo=$(".filesBox");

  var label = $("<label/>").addClass("noFiles").html("<%= I18n.get("NO_FILES")%>").css("display","none");
  $(".filesBox").append(label);

  if(files.length)
    for (var i=0; i<files.length;i++){
      //console.debug(files[i]);
      ndo.append($.JST.createFromTemplate(files[i],"FILEBOX"));
    } else
      label.show();

}


function deleteIssuePreview(issueId){
  deletePreview("ISS_DEL", issueId, function(response){  // callback function
    if (response && response.ok){
      //console.debug("deleteIssuePreview done")
      getBlackPopupOpener().$("body").trigger("issueEvent",[{type:"delete",response:response}]);
      closeBlackPopup();
    }
  });
}

</script>

<%
  }
%>
