<%@ page import="org.jblooming.waf.html.display.DataTable, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.settings.ApplicationState, com.twproject.task.businessLogic.IssueController, org.jblooming.waf.settings.I18n, org.jblooming.waf.constants.Fields, org.jblooming.waf.view.PageState, com.twproject.task.Issue, com.twproject.task.IssueBricks, org.jblooming.utilities.JSP, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonSubmit, com.twproject.security.TeamworkPermissions, com.twproject.task.TaskBricks, com.twproject.resource.ResourceBricks, org.jblooming.waf.html.input.*, org.jblooming.waf.html.core.JST, org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.constants.Commands, com.twproject.task.IssueStatus, com.twproject.operator.TeamworkOperator" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  String issueId = pageState.getEntry("ISSUE_ID").stringValueNullIfEmpty();
  String taskId = pageState.getEntry("FLT_ISSUE_TASK").stringValueNullIfEmpty();
  String resourceId = pageState.getEntry("FLT_ISSUE_ASSIGNED_TO").stringValueNullIfEmpty();

  boolean showEmptyRow= Commands.ADD.equals(pageState.command);

%>


<%-- ------------------------- UPLOADIZE -------------------------------------------- --%>
<%
  DataTable dataTable = new DataTable("ISSUEFILTER", pageState.getForm(), new JspHelper("/applications/teamwork/issue/rowIssueList.jsp"), IssueController.class, pageState);
  dataTable.drawPageFooter=true;

  dataTable.addHeader("<input type=\"checkbox\" onclick=\"selUnselAll($(this));\">");
  dataTable.addHeader(I18n.get("ISSUE_GRAVITY_SHORT"), "");
  dataTable.addHeader(I18n.get("ISSUE_STATUS"), "issue.status");
  dataTable.addHeader(I18n.get("ISSUE_DESCRIPTION"),null,null,null,"headIssDescr");
  dataTable.addHeader(I18n.get("ISSUE_TASK"), "task.name");
  dataTable.addHeader(I18n.get("ISSUE_ASSIGNED_TO"), "resource.name");
  boolean usetypeonissues = Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("USETYPEONISSUES"));
  if (usetypeonissues) {
    dataTable.addHeader(I18n.get("ISSUE_TYPE"));
  }
  boolean useimpactonissues = Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("USEIMPACTONISSUES"));
  if (useimpactonissues) {
    dataTable.addHeader(I18n.get("ISSUE_IMPACT"),"issue.impact.id");
  }
  dataTable.addHeader(I18n.get("ISSUE_CLOSE_BY_SHORT"),"issue.shouldCloseBy");
  dataTable.addHeader(I18n.get("WORKLOG_SHORT"));
  dataTable.addHeader("");

  pageState.getForm().start(pageContext);

  new DeletePreviewer("ISS_DEL",IssueController.class, pageState);

  boolean saveFilterHidden=request.getRequestURI().indexOf("taskIssueList")>-1 || request.getRequestURI().indexOf("resourceIssueList")>-1;
%>

<%----------------------------------------------------------------------------  START FILTER ----------------------------------------------------------------------------%>
<div class="filterBar withButtons clearfix">
  <%
    new JspHelper("/applications/teamwork/issue/partIssueFilter.jsp").toHtml(pageContext);
    LoadSaveFilter lsfb = new LoadSaveFilter("ISSUEFILTER", pageState.getForm());
    lsfb.label = I18n.get("WANT_TO_SAVE_FILTER");
    lsfb.drawEditor = true;
    lsfb.drawButtons = false;
    lsfb.id="issueLSF";
  %>

  <div class="filterButtons">
    <div class="filterButtonsElement filterAdd">
      <span class="button" id="filterSelectorOpener" title="<%=I18n.get("ADD_FILTER")%>" onclick="bjs_showMenuDiv('filterSelectorBox', 'filterSelectorOpener');"><span class="teamworkIcon">f</span></span>
    </div>
    <div class="filterButtonsElement filterSearch"><%dataTable.getSearchButton().toHtml(pageContext);%></div>

    <div class="filterActions">
      <div class="filterButtonsElement filterSave" style="<%=saveFilterHidden?"display:none;":""%>"><%lsfb.toHtml(pageContext);%></div>
      <div class="filterButtonsElement filterHelp"><%
        DataTable.getQBEHelpButton(pageState).toHtmlInTextOnlyModality(pageContext);%></div>
    </div>

  </div>
</div>
<script src="<%=request.getContextPath()%>/commons/js/filterEngine.js"></script>
<%----------------------------------------------------------------------------  END FILTER ----------------------------------------------------------------------------%>

<%-- ------------------------------------------------------------- START CSS-------------------------------------------------------------- --%>
<style type="text/css">
  .table.dataTable .dataTableBody td.priority {
    position: relative;
  }

  <%for (String gr:Issue.getGravities()){%>

  .table.dataTable .dataTableBody tr.issueRow[gravity="<%=gr%>"] td.priority {
    background-color: <%=IssueBricks.getGravityColor(gr)%>;
  }
  <%}%>
</style>
<%
  // finalize color status choosers in order to enable ajaxed ones
  ColorValueChooser stc = IssueBricks.getStatusChooser("dum1", "STATUSLINE", pageState);
  stc.height = 30;
  stc.width = 120;
  stc.onChangeScript = "hidden.each(changeStatus);";
  stc.init(pageContext);

  ColorValueChooser grc = IssueBricks.getGravityChooser("dum2", "ISSGRLIN", false, false, pageState);
  grc.height = 30;
  grc.width = 120;
  grc.onChangeScript = "hidden.each(changeGravity);";
  grc.init(pageContext);

  pageState.getForm().end(pageContext);

%>

<%----------------------------------------------- WL and NOTES EDITOR ---------------------------------------------------------%>
<jsp:include page="partIssueNotes.jsp"/>

<%----------------------------------------------- START BULK OPERATIONS ---------------------------------------------------------%>
<div id="bulkOp" style="display:none;">

  <div id="bulkRowSel"></div>

  <div><%

    ButtonJS closeSel = new ButtonJS(I18n.get("ISSUE_CLOSE_ALL"), "performBulkAction('BULK_CLOSE_ISSUES');");
    closeSel.confirmRequire = true;
    closeSel.confirmQuestion = I18n.get("ISSUE_CONFIRM_CLOSE_ALL");
    closeSel.iconChar = "x";
    closeSel.toHtmlInTextOnlyModality(pageContext);

  %></div><div><%

  ButtonJS changeGravityAll = new ButtonJS("openBulkAction('changeGravityAll');");
  changeGravityAll.label = I18n.get("ISSUE_CHANGEGRAVITY_ALL");
  changeGravityAll.iconChar = "h";
  changeGravityAll.toHtmlInTextOnlyModality(pageContext);

%><br><%

  ButtonJS changeStatusAll = new ButtonJS("openBulkAction('changeStatusAll');");
  changeStatusAll.label = I18n.get("ISSUE_CHANGESTATUS_ALL");
  changeStatusAll.iconChar = "h";
  changeStatusAll.toHtmlInTextOnlyModality(pageContext);

  if (Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("USEIMPACTONISSUES"))){
    %><br><%
    ButtonJS changeImpactAll = new ButtonJS("openBulkAction('changeImpactAll');");
    changeImpactAll.label = I18n.get("ISSUE_CHANGEIMPACT_ALL");
    changeImpactAll.iconChar = "h";
    changeImpactAll.toHtmlInTextOnlyModality(pageContext);
  }


%></div><div><%
  ButtonJS changeTaskAll = new ButtonJS("openBulkAction('moveToTaskAll');");
  changeTaskAll.label = I18n.get("ISSUE_CHANGETASK_ALL");
  changeTaskAll.iconChar = "y";
  changeTaskAll.toHtmlInTextOnlyModality(pageContext);

%><br><%
  ButtonJS changeResAll = new ButtonJS("openBulkAction('moveToResAll');");
  changeResAll.label = I18n.get("ISSUE_CHANGERES_ALL");
  changeResAll.iconChar = "y";
  changeResAll.toHtmlInTextOnlyModality(pageContext);

%><br><%
  ButtonJS copyTaskAll = new ButtonJS("openBulkAction('copyToTaskAll');");
  copyTaskAll.label = I18n.get("ISSUE_COPYTOTASK_ALL");
  copyTaskAll.iconChar = "D";
  copyTaskAll.toHtmlInTextOnlyModality(pageContext);

%></div><div><%

  ButtonJS mergeAll = new ButtonJS(I18n.get("ISSUE_MERGE_ALL"), "performBulkAction('BULK_MERGE_ISSUES');");
  mergeAll.confirmRequire = true;
  mergeAll.iconChar = "q";
  mergeAll.confirmQuestion = I18n.get("ISSUE_CONFIRM_MERGE_ALL");
  mergeAll.toHtmlInTextOnlyModality(pageContext);

%><br><%
  ButtonJS changeDateAll = new ButtonJS(I18n.get("ISSUE_CHANGEDATE_ALL"), "openBulkAction('changeDateAll');");
  changeDateAll.iconChar = "m";
  changeDateAll.toHtmlInTextOnlyModality(pageContext);

%></div><div><%

  ButtonJS addCommentAll = new ButtonJS(I18n.get("ADD_COMMENT"), "openBulkAction('addCommentAll');");
  addCommentAll.iconChar="Q";
  addCommentAll.toHtmlInTextOnlyModality(pageContext);
%><br><%
  ButtonJS addTagsAll = new ButtonJS(I18n.get("ADD_TAGS"), "openBulkAction('addTagsAll');");
  addTagsAll.iconChar="B";
  addTagsAll.toHtmlInTextOnlyModality(pageContext);

%></div><div><%

  ButtonJS removeAll = new ButtonJS(I18n.get("ISSUE_REMOVE_ALL"), "performBulkAction('BULK_DEL_ISSUES');");
  removeAll.confirmRequire = true;
  removeAll.iconChar = "&#xa2;";
  removeAll.confirmQuestion = I18n.get("ISSUE_CONFIRM_REMOVE_ALL");
  removeAll.label = I18n.get("ISSUE_REMOVE_ALL");
  removeAll.toHtmlInTextOnlyModality(pageContext);

%><br><%

  ButtonSubmit bulkpr = new ButtonSubmit(pageState.getForm());
  bulkpr.additionalOnClickScript = "";
  bulkpr.variationsFromForm.command = "BULK_PRINT";
  bulkpr.additionalOnClickScript = "injectSelectedIds();";
  bulkpr.variationsFromForm.href = pageState.pageFromRoot("issue/issueListPrint.jsp").href;
  bulkpr.target = "_blank";
  bulkpr.label = I18n.get("PRINT_SELECTED");
  bulkpr.preserveFormStatus = true;
  bulkpr.iconChar = "p";
  bulkpr.toHtmlInTextOnlyModality(pageContext);

  ButtonJS p = ButtonSubmit.getBlackInstance(pageState.getForm(), pageState.pageFromRoot("issue/issueListPrint.jsp").href);
  p.label = I18n.get("PRINT");

%></div>
</div>
<%----------------------------------------------- END BULK OPERATIONS ---------------------------------------------------------%>

<%----------------------------------------------- MAIN COLUMN END ---------------------------------------------------------%>

<%------------------------------------------------ BULK BOXES START ---------------------------------------------------------%>
<div id="moveToTaskAll" style="display:none;" class="bulkData">
  <h1><%=I18n.get("ISSUE_CHANGETASK_ALL")%>
  </h1>
  <table width="100%">
    <tr>
      <td valign='top' width="330" nowrap><%
        SmartCombo tasks = TaskBricks.getTaskCombo("ISSUE_MOVE_TO_TASK", false, TeamworkPermissions.issue_canCreate, pageState);
        tasks.fieldSize = 35;
        tasks.label = I18n.get("ISSUE_MOVE_TO_TASK");
        tasks.separator = "<br>";
        tasks.required=true;
        tasks.preserveOldValue = false;
        tasks.toHtml(pageContext);
      %></td>
    </tr>
    <tr>
      <td valign='top'><%
        TextArea tak = new TextArea(I18n.get("NOTES"), "HIS_NOTES_TSK", "<br>", 35, 2, "");
        tak.preserveOldValue = false;
        tak.toHtml(pageContext);

      %></td>
    </tr>
    <tr>
      <td><br><%
        ButtonJS doMove = new ButtonJS(I18n.get("PROCEED"), "performBulkAction('BULK_MOVE_TO_TASK',$(this));");
        doMove.additionalCssClass = "first";
        doMove.toHtml(pageContext);
      %></td>
    </tr>
  </table>
</div>

<div id="copyToTaskAll" style="display:none;" class="bulkData">
  <h1><%=I18n.get("ISSUE_COPYTOTASK_ALL")%>
  </h1>
  <table width="100%">
    <tr>
      <td valign='top' width="330" nowrap><%
        tasks = TaskBricks.getTaskCombo("ISSUE_COPY_TO_TASK", false, TeamworkPermissions.issue_canCreate, pageState);
        tasks.fieldSize = 35;
        tasks.label = I18n.get("ISSUE_MOVE_TO_TASK");
        tasks.separator = "<br>";
        tasks.required=true;
        tasks.preserveOldValue = false;
        tasks.toHtml(pageContext);
      %></td>
    </tr>
    <tr>
      <td><br><%
        ButtonJS doCopy = new ButtonJS(I18n.get("PROCEED"), "performBulkAction('BULK_COPY_TO_TASK',$(this));");
        doCopy.additionalCssClass = "first";
        doCopy.toHtml(pageContext);
      %></td>
    </tr>
  </table>
</div>

<div id="moveToResAll" style="display:none;" class="bulkData">
  <h1><%=I18n.get("ISSUE_CHANGERES_ALL")%>
  </h1>
  <table width="100%">
    <tr>
      <td valign="top" width="330" nowrap><%
        //SmartCombo res = ResourceBricks.getPersonCombo("ISSUE_MOVE_TO_RES", TeamworkPermissions.assignment_manage, false, null, pageState);
        SmartCombo res = TaskBricks.getAssignableResourceCombo(null,"ISSUE_MOVE_TO_RES", false, pageState);
        res.label = I18n.get("ISSUE_MOVE_TO_RES");
        res.separator = "<br>";
        res.preserveOldValue = false;
        res.fieldSize = 35;
        res.toHtml(pageContext);
      %></td>
    </tr>
    <tr>
      <td valign='top'><%
        TextArea tar = new TextArea(I18n.get("NOTES"), "HIS_NOTES_RES", "<br>", 35, 2, "");
        tar.preserveOldValue = false;
        tar.toHtml(pageContext);

      %></td>
    </tr>
    <tr>
      <td><br><%
        doMove = new ButtonJS(I18n.get("PROCEED"), "performBulkAction('BULK_MOVE_TO_RES',$(this));");
        doMove.additionalCssClass = "first";
        doMove.toHtml(pageContext);
      %></td>
    </tr>
  </table>
</div>

<div id="changeStatusAll" style="display:none;" class="bulkData">
  <h1><%=I18n.get("ISSUE_CHANGESTATUS_ALL")%></h1>
  <table width="100%">
    <tr>
      <td valign="top" width="340" nowrap><%
        pageState.getEntryOrDefault("ISSUE_STATUS_ALL", IssueStatus.getStatusOpen().getId()+"");
        ColorValueChooser cvc = IssueBricks.getStatusChooser("ISSUE_STATUS_ALL", "STATUS", pageState);
        cvc.showOpener = true;
        cvc.label = I18n.get("ISSUE_STATUS");
        cvc.separator = "<br>";
        cvc.displayValue = true;
        cvc.preserveOldValue = false;
        cvc.height = 30;
        cvc.toHtml(pageContext);
      %></td>
    </tr>
    <tr>
      <td valign='top'><%
        TextArea tan = new TextArea(I18n.get("NOTES"), "HIS_NOTES_ST", "<br>", 35, 2, "");
        tan.preserveOldValue = false;
        tan.script="style='width:100%' ";
        tan.toHtml(pageContext);

      %></td>
    </tr>
    <tr>
      <td><br><%
        ButtonJS doChStatMove = new ButtonJS(I18n.get("PROCEED"), "performBulkAction('BULK_SET_STATUS',$(this));");
        doChStatMove.additionalCssClass = "first";
        doChStatMove.toHtml(pageContext);
      %></td>
    </tr>
  </table>
</div>

<div id="changeGravityAll" style="display:none;" class="bulkData">
  <h1><%=I18n.get("ISSUE_CHANGEGRAVITY_ALL")%>
  </h1>
  <table width="100%">
    <tr>
      <td width="340"><%
        pageState.getEntryOrDefault("ISSUE_GRAVITY_ALL", Issue.GRAVITY_BLOCK);
        ColorValueChooser cvcg = IssueBricks.getGravityChooser("ISSUE_GRAVITY_ALL", "GRAVITY", false, false, pageState);
        cvcg.label = I18n.get("ISSUE_GRAVITY");
        cvcg.showOpener = true;
        cvcg.separator = "<br>";
        cvcg.displayValue = true;
        cvcg.preserveOldValue = false;
        cvcg.height = 30;
        cvcg.toHtml(pageContext);%>
      </td>
    </tr>
    <tr>
      <td valign='top'><%
        tan = new TextArea(I18n.get("NOTES"), "HIS_NOTES_ST", "<br>", 35, 2, "");
        tan.script="style='width:100%' ";
        tan.preserveOldValue = false;
        tan.toHtml(pageContext);
      %></td>
    </tr>
    <tr>
      <td><%
        ButtonJS doChGrav = new ButtonJS(I18n.get("PROCEED"), "performBulkAction('BULK_SET_GRAVITY',$(this));");
        doChGrav.additionalCssClass = "first";
        doChGrav.toHtml(pageContext);
      %></td>

    </tr>
  </table>
</div>

<div id="changeImpactAll" style="display:none;" class="bulkData">
  <h1><%=I18n.get("ISSUE_CHANGEIMPACT_ALL")%>
  </h1>
  <table width="100%">
    <tr>
      <td><%
        SmartCombo issueImpact = IssueBricks.getIssueImpactCombo("ISSUE_IMPACT_ALL", null, pageState);
        issueImpact.separator = "<br>";
        issueImpact.label = I18n.get("ISSUE_IMPACT");
        issueImpact.fieldSize = 10;
        issueImpact.preserveOldValue=false;
        issueImpact.toHtml(pageContext);
        %>
      </td>
    </tr>
    <tr>
      <td><%
        ButtonJS doChImpact = new ButtonJS(I18n.get("PROCEED"), "performBulkAction('BULK_SET_IMPACT',$(this));");
        doChImpact.additionalCssClass = "first";
        doChImpact.toHtml(pageContext);
      %></td>

    </tr>
  </table>
</div>


<div id="changeDateAll" style="display:none;" class="bulkData">
  <h1><%=I18n.get("ISSUE_CHANGEDATE_ALL")%>
  </h1>
  <table width="100%">
    <tr>
      <td><%
        DateField newDate= new DateField("ISSUE_NEWDATE_ALL");
        newDate.separator = "<br>";
        newDate.preserveOldValue=false;
        newDate.toHtmlI18n(pageContext);
        %>
      </td>
    </tr>
    <tr>
      <td><%
        ButtonJS doChDates = new ButtonJS(I18n.get("PROCEED"), "performBulkAction('BULK_SET_NEW_DATE',$(this));");
        doChDates.additionalCssClass = "first";
        doChDates.toHtml(pageContext);
      %></td>

    </tr>
  </table>
</div>

<div id="addCommentAll" style="display:none;" class="bulkData">
  <h1><%=I18n.get("ADD_COMMENT")%>
  </h1>
  <table width="100%">
    <tr>
      <td valign='top'><%
        tan = new TextArea(I18n.get("NOTES"), "HIS_NOTES_ST", "<br>", 35, 4, "");
        tan.script="style='width:100%' ";
        tan.preserveOldValue = false;
        tan.toHtml(pageContext);
      %></td>
    </tr>
    <tr>
      <td><%
        ButtonJS doAddComm = new ButtonJS(I18n.get("PROCEED"), "performBulkAction('BULK_ADD_COMMENT',$(this));");
        doAddComm.additionalCssClass = "first";
        doAddComm.toHtml(pageContext);
      %></td>

    </tr>
  </table>
</div>

<div id="addTagsAll" style="display:none;" class="bulkData">
  <h1><%=I18n.get("ADD_TAGS")%>
  </h1>
  <table width="100%">
    <tr>
      <td valign='top'><%
        TagBox tags = new TagBox("ISSUE_TAGS", Issue.class,logged.getPerson().getArea());
        tags.label = "";
        tags.toolTip = I18n.get("TAGS_COMMA_SEPARATED");
        tags.separator = "<br>";
        tags.preserveOldValue=false;
        tags.script = "style='width:100%;'";
        tags.toHtml(pageContext);
      %></td>
    </tr>
    <tr><td>
      <%
        new CheckField("REPLACE_EXISTING","&nbsp;",false).toHtmlI18n(pageContext);
      %></td>
    </tr>
    <tr>
      <td><%
        ButtonJS doAddTag = new ButtonJS(I18n.get("PROCEED"), "performBulkAction('BULK_ADD_TAGS',$(this));");
        doAddTag.additionalCssClass = "first";
        doAddTag.toHtml(pageContext);
      %></td>

    </tr>
  </table>
</div>


<%------------------------------------------------ BULK BOXES END ---------------------------------------------------------%>

<div style="display: none;" id="issueTemplates">
  <%=JST.start("ISSHISTPL")%>
  <tr histId="(#=obj.id#)" class="alternate">
    <td valign="top" nowrap width="40">
      <img class="face small" src="(#=obj.ownerAvatarUrl#)">
    </td>
    <td valign="top">
      <div class="commentHeader"><b>(#=obj.lastModifier#)</b> - (#=dateToRelative(new Date(obj.creationDate))#)</div>
      (#=obj.oldStatusName?"<%=I18n.get("ISSUE_STATUS")%>: "+obj.statusName+"<br>":""#)
      (#=obj.assigneeName?"<%=I18n.get("ISSUE_ASSIGNED_TO")%>: "+obj.assigneeName+"<br>":""#)
      (#=obj.taskName?"<%=I18n.get("ISSUE_TASK")%>: "+obj.taskName+"<br>":""#)
      (#=obj.comment#)
    </td>
    <td valign="top" align="right" class="issHistDelBtn"><%
      ButtonJS delData = new ButtonJS("deleteNotes($(this))");
      delData.confirmRequire = true;
      delData.label = "";
      delData.iconChar = "d";
      delData.toHtmlInTextOnlyModality(pageContext);
    %></td>
  </tr>
  <%=JST.end()%>

</div>
<%----------------------------------------------- START TEMPLATES ---------------------------------------------------------%>
<div style="display: none;" id="iltmplt">
  <%=JST.start("FILEBOX")%>
  <div class="repoFileBox" >
    <span class="button textual" title="<%=I18n.get("PREVIEW_DOWNLOAD")%>">(#=obj.name#)</span><span class="del teamworkIcon" title="<%=I18n.get("DELETE")%>">d</span>
  </div>
  <%=JST.end()%>
</div>
<%----------------------------------------------- END TEMPLATES ---------------------------------------------------------%>


<%--------------------------------------------------------------- START JAVASCRIPT CONTROLLER ----------------------------------------------------------------%>

<script>
var sortEnabled = false;
var sortFlavour = "";
var _showEmptyRow=<%=showEmptyRow%>;

//WARINING template must be loaded before rows
$("#iltmplt").loadTemplates().remove();
$.JST.loadDecorator("FILEBOX", function (box, pf) {
  //console.debug(box,pf)
  box.data("pf",pf);
  //bind open image/download file
  box.click(function (ev) {
    ev.stopPropagation();
    openPersistentFile($(this).data("pf"));
  });

  //console.debug("box.data",box.data("pf"))
  //delete file
  box.find(".del").click(function(ev){
    ev.stopPropagation();
    var el = $(this);
    var fb = el.closest(".repoFileBox");
    //console.debug("fb",fb.data("pf"));

    var editor=fb.closest("[issueID]");
    fb.confirm(function(){
      var box=$(this);
      showSavingMessage();
      var request = {CM: "DELSCRSHT", issueId:editor.attr("issueId"), fileUID: box.data("pf").uid,ck:box.data("pf").ck};
      $.getJSON(contextPath+"/applications/teamwork/issue/issueAjaxControllerJson.jsp", request, function (response) {
        jsonResponseHandling(response);
        if (response.ok) {
          var filesBox=box.closest(".filesBox");
          if (filesBox.find(".repoFileBox").size()<=1)
            filesBox.find(".filesIcon").hide();
          box.fadeOut(200, function () {$(this).remove()});
        }
        hideSavingMessage();
      });
    },"<%=I18n.get("FLD_CONFIRM_DELETE")%>");

  });
});


$(function () {

  //an issue_id has been passed as CE?
  if ("<%=JSP.w(issueId)%>" != "")
    openIssueEditorInBlack("<%=issueId%>");

  //issue update events from black popup editor
  registerEvent("issueEvent", function (e, data) {
    //console.debug("issueEvent",data)
    if (data.type == "delete")
      $("[issueId=" + data.response.deletedId + "].issueRow").remove();
    else if (data.type == "save") {

      refreshIssueRow(data.response.issue.id, 400, false, function(){

      if(data.response.askForNotes){
          showNotesForStatusChange(data.response.askForNotes);
      }

      if(data.response.askForWorklog){
        btnShowWorklog($("tr[issueid="+data.response.issue.id+"]").find("[name=ISSUE_STATUS]").parent(".cvcComponent"));
      }

      });
    }
  });


  //bind duplicate issue
  $("body").on("keydown", function(event) {
    switch (event.keyCode) {
      case 68: //left arrow
        if (event.ctrlKey || event.metaKey){
          event.preventDefault();
          duplicateCurrentIssue();
        }
    }
  });


  //bind for worklog Saved event
  registerEvent("worklogEvent.partIssueList",function(ev,data){
    //console.debug("worklog Saved",ev,data);
    if (data.type=="save" && ! $(".issueRow.inEdit[issueId="+data.response.worklog.issueId+"]").find(":input[oldValue]").isValueChanged()) { // se ci sono dati cambiati non si rinfresca
      refreshIssueRow(data.response.worklog.issueId, 100, true);
    }
  });


  //inject the table search
  createTableFilterElement($("#headIssDescr"),".dataTable [issueId]",".issueDescTd,.issueTaskCell");

});


function dataTableCallback(num) {
  //console.debug("dataTableCallback", num);
  if ($("#FLT_ISSUE_ASSIGNED_TO").val()) {
    sortEnabled = true;
    sortFlavour = "BY_RESOURCE";
  } else if ($("#FLT_ISSUE_TASK").val()) {
    sortEnabled = true;
    sortFlavour = "BY_TASK";
  }


  var multi = $("#ISSUEFILTER .dataTableBody");

  multi.find("[issueId]").each(function () {
    activateView($(this));
  });

  if (sortEnabled) {
    var sortableHelper = function (e, ui) {
      ui.children().each(function () {
        $(this).width($(this).width());
      });
      var clone = ui.clone();
      clone.css({position: "absolute", marginLeft: 35});
      return clone;
    };

    multi.sortable({
      handle:  ".dragHandler",
      axis:    "y",
      helper:  sortableHelper,
      opacity: 0.9,
      update:  function (ev, ui) {
        var sortedEl = ui;
        var issueRow = sortedEl.item;

        //if (ev.ctrlKey) { // se droppo con ctrl copia la riga
        //  multi.sortable("cancel");
        //} else {
        updateOrderAndGravity(issueRow);
        //}
      },
      start:   function (e, ui) {
        ui.placeholder.height(ui.helper.outerHeight());
      },
      end:     function () {}
    }).unselectable();
  }

  if(_showEmptyRow){
    addIssue();
    _showEmptyRow=false;
  }
}



function moveIssueToTop(el) {
  //console.debug("moveIssueToTop",issueRow);
  var issueRow = el.closest("[issueid]");
  issueRow.fadeOut(300, function () {
    $("#ISSUEFILTER .dataTableBody").prepend($(this));
    updateOrderAndGravity($(this));
    $(this).show();
  })
}

function moveIssueToBottom(el) {
  var issueRow = el.closest("[issueid]");
  issueRow.fadeOut(300, function () {
    $("#ISSUEFILTER .dataTableBody").append($(this));
    updateOrderAndGravity($(this));
    $(this).show();
  })
}

function updateOrderAndGravity(issueRow) {
  var multi = $("#ISSUEFILTER .dataTableBody");
  //var sortedEl=ui;
  //var issueRow = sortedEl.item;

  //compute new gravity
  var prevIssue = issueRow.prev();
  var nextIssue = issueRow.next();
  var oldG = issueRow.attr("gravity");
  var newG = oldG;

  var prevG = prevIssue.attr("gravity");
  var nextG = nextIssue.attr("gravity");

  //move to bottom
  if (nextG == undefined) {
    newG = prevG > oldG ? oldG : prevG;

    //move to top
  } else if (prevG == undefined) {
    newG = nextG < oldG ? oldG : nextG;

    //move in the middle
  } else {
    //not compatible place
    if (!(nextG < oldG && prevG > oldG)) {
      newG = nextG;
    }
    // else do nothing
  }

  //change gravity graphically
  issueRow.attr("gravity", newG);

  //update position only for strips with newG gravity. Order is always by gravity,position
  var sortedIds = [];
  multi.children().filter("[gravity=" + newG + "]").each(function (i) {
    var row = $(this);
    var issueId = row.attr("issueId");
    sortedIds.push(issueId);
    //if new issue set pending order
    if (issueId.indexOf("new_")>=0)
      row.data("TEMP_ORDER",i);
  });

  var request = {CM: "SORT_ISSUES", SORT_FLAVOUR: sortFlavour, issues: sortedIds.join(",")};
  //if gravity changed send new gravity mand issue id
  if (oldG != newG) {
    request["issueId"] = issueRow.attr("issueId");
    request["newGravity"] = newG;
  }

  showSavingMessage();
  $.getJSON(contextPath+"/applications/teamwork/issue/issueAjaxControllerJson.jsp", request, function (response) {
    jsonResponseHandling(response);
    if (response.ok) {
      //do nothing
    }
    hideSavingMessage();
  });
}


function editIssue(el) {

  if(!hideWorklogEditorIfYouCan())
    return;

    //console.debug("editIssue",el) ;
  var issueRow = $(el).closest("[issueId]");
  var issueId = issueRow.attr("issueId");

  if(issueRow[0].isInEdit)
    return;

  issueRow[0].isInEdit = true;

  var request = {CM: "EDIT", EDIT: sortEnabled ? "yes" : "no", ISSUEID: issueId};
  $.post(contextPath+"/applications/teamwork/issue/issueAjaxController.jsp", request, function (response) {

    issueRow[0].isInEdit = false;

    var editor = $(response);

    var wst = $(window).scrollTop();

    var issuePH = $("<div/>").css("height", 800);

    activateEditor(editor);
    editor.click();
    $("body").append(issuePH);

    issueRow.replaceWith(editor);

    setTimeout(function(){
      issuePH.remove();
//      $(window).scrollTop(wst)
    },300);

    //give focus to selected field
    var fieldToFocus;
    if (el.size() > 0)
      fieldToFocus = $(el).attr("fieldName");

    var inp = editor.find("[name=" + fieldToFocus + "]");
    if ("ISSUE_GRAVITY"==fieldToFocus) {
      inp.next().click();
    } else {
      if (inp.size() <= 0)
        inp = editor.find("[name=ISSUE_DESCRIPTION]");
      inp.focus();
    }
  });
}

function refreshIssueRow(issueId, delay, flash, callback) {
  //console.debug("refreshIssueRow ",issueId, delay, flash);
  if (!delay)
    delay = 1;

  $("body").oneTime(delay, "refreshIssueRow", function () {
    var issueRow = $(".issueRow[issueId=" + issueId + "]");
    var request = {CM: "REFRESHROW", DRAGENABLED: sortEnabled ? "yes" : "no", ISSUEID: issueId};
    $.get(contextPath+"/applications/teamwork/issue/issueAjaxController.jsp", request, function (response) {
      var view = $(response);
      activateView(view);
      issueRow.replaceWith(view);



      if (flash)
        view.effect("highlight", { color: "#F9EFC5" }, 1500);

      if( typeof(callback) == 'function')
        callback();
    });



  });
}

function activateView(view) {
  //console.debug("activateView",view, view.find("[fieldName]").length);
  view.find(".twLink").activateTeamworkLinks(true).emoticonize();

  view.find("[fieldName]").click(function (ev) {
    ev.stopPropagation();
    var el = $(this);
    editIssue(el);
    return false;
  });

  view.click(function(){
    $(".issueRowFocused").removeClass("issueRowFocused");
    $(this).addClass("issueRowFocused");
  });
  enableUpload(view);
}


//bind and decorate editor
function activateEditor(editor) {
  editor.find(":input[oldvalue]").updateOldValue().on("keyup blur change", function () {
    //console.debug("keyup blur change")
    if ($(this).isValueChanged()) {
      $("#issueButtonBar").show();
      $(window).resize();
      editor.find(".btnSave").removeClass("disabled");
    }
  });

  //enlarge textarea
  editor.find("textarea.autosize").oneTime(10, enlargeTextArea);

  editor.find("textarea.autosize").keydown(function (ev) {

    //add key events to save the issue with the cmd or ctrl and or s keys
    if (ev.ctrlKey || ev.metaKey)
      switch (ev.keyCode) {

        case 13: //return
        case 83: //s
          saveIssue($(this));
          ev.preventDefault();
          return false;
          break;

        default:
          break;
      }
    ;


    //add key events to close the issue editor with the ESC key
    if (ev.keyCode == 27) {
      closeEditor($(this));
      ev.preventDefault();
      return false;
    }


  });

  editor.click(function () {
    $(".issueRowFocused").removeClass("issueRowFocused");
    $(this).addClass("issueRowFocused");
  });

  enableUpload(editor);

}



function enableUpload(view) {
  view.find(".uploadizeDrop").each(function () {
        $(this).uploadize({
          url:                         contextPath+"/applications/teamwork/issue/issueAjaxControllerJson.jsp",
          maxSize:<%=Uploader.getMaxUploadSizeInByte()%>,
          activeElement:               $(this).closest(".issueRow").get(0),
          multi:                       true,
          showPlaceHolder:             false,
          additionalRequestParameters: {CM: "DROPDOC", issueId: view.attr("issueId")},
          onLoadCallback:              function (response) {
            $(this).hide();
            view.find(".filesBox").append( $.JST.createFromTemplate(response.file, "FILEBOX"));
            view.find(".filesIcon").show();

            //in caso di nuova issue mette da una parte i pf temporanei
            if (view.attr("issueId").indexOf("new_") >= 0) {
              var tempPF = $("#PENDING_PF").val();
              var pfs = view.data("PENDING_PF") ? view.data("PENDING_PF") : [];
              pfs.push(response.file.uid);
              view.data("PENDING_PF", pfs);
            }

          },
          resolvePasteElement: function(event){
            var el=$(":focus").closest("[issueId]").find(".isUploadizer");
            return el;
          }

        });
      }
  );
  view.bind("drop", function () {
    $(this).find(".uploadizeDrop").show();
    $(this).find(".uploadLabel").hide();
  });
}


function closeEditor(el) {
  closeEditorIfYouCan(el.closest(".inEdit"));
}


function closeEditorIfYouCan(editor) {
  var issueId = editor.attr("issueId");
  var ret = true;
  if (editor.size() > 0) {
    if (editor.find(":input[oldValue]").isValueChanged()) {
      if (confirm("<%=I18n.get("LOOSE_CHANGES")%>")) {
        if (editor.attr("issueId").startsWith("new"))
          editor.remove();
        else
          refreshIssueRow(editor.attr("issueId"));
      } else {
        ret = false;
      }
    } else {
      if (issueId.startsWith("new"))
        editor.remove();
      else
        refreshIssueRow(issueId);
    }
  }
  return ret;
}


function saveIssue(button) {
  if (!button.is(".disabled"))
    saveRows(button.closest("tr.issueRow.inEdit"))
}


function saveAllIssues(button) {
  saveRows($("tr.issueRow.inEdit"))
}

function saveRows(jQueryRows) {

  var request = {CM: "SVISS"};
  var issues = [];
  jQueryRows.each(function () {
    var editor = $(this);
    //console.debug(editor)
    if (editor.find(":input").isValueChanged()) {

      if (canSubmitForm(editor)) {

        var issueId = editor.attr("issueId");
        var issue = {OBJID: issueId};
        // extract every input in tr
        editor.fillJsonWithInputValues(issue);

        //controllo se ci solo dei PF pending
        if (editor.data("PENDING_PF"))
          issue["PENDING_PF"] = JSON.stringify(editor.data("PENDING_PF"));

        //controllo se c'è un sort pending
        if (editor.data("TEMP_ORDER"))
          issue["TEMP_ORDER"] = editor.data("TEMP_ORDER");
        issue["SORT_FLAVOUR"] = sortFlavour;

        issues.push(issue);
      }
    }
  });

  if (issues.length) {
    showSavingMessage();

    request.issues = JSON.stringify(issues);

    $.post(contextPath+"/applications/teamwork/issue/issueAjaxControllerJson.jsp", request, function (response) {
      jsonResponseHandling(response);
      if (response.ok ) {
        $(".notesEditor").fadeOut();
        if (response.oldIdNewId) {
          for (var oldId in response.oldIdNewId) {
            var newId = response.oldIdNewId[oldId];
            //update issueId in case of new to make refreshIssueRow effective
            $("tr.issueRow[issueId=" + oldId + "]").attr("issueId", newId);
            refreshIssueRow(newId, 0, true);
          }
        }
        if (response.rowErrors){
          var errMessages=[];
          for (var oldId in response.rowErrors) {
            var issueRow = $("tr.issueRow[issueId=" + oldId + "]");
            jsonResponseHandling({clientEntryErrors:response.rowErrors[oldId]}, issueRow);
            if (errMessages.indexOf(response.rowErrors[oldId][0].error)<0)
              errMessages.push(response.rowErrors[oldId][0].error);
          }
          showFeedbackMessage("ERROR",errMessages.join("<br>"),"<%=I18n.get("ERROR_SAVING")%>")
        }
      }
      hideSavingMessage();
      $("#issueButtonBar").hide();
      $.buttonBar.manageButtonBar();
    }, "json");
  }
}

function changeGravity(){
  var el = $(this);
  el.closest("tr").attr("gravity",el.val());
}

function changeStatus() {
  //console.debug("changeStatus")
  var el = $(this);
  var issueRow = el.closest("[issueId]");
  var issueId = issueRow.attr("issueId");
  if (el.isValueChanged() && !issueId.startsWith("new")) { // if new issue do not save immediately
    showSavingMessage();

    var request = {CM: "CHISSSTS", OBJID: issueId};
    request[el.prop("name")] = el.val();
    $.getJSON(contextPath+"/applications/teamwork/issue/issueAjaxControllerJson.jsp", request, function (response) {
      jsonResponseHandling(response);
      if (response.ok) {
        el.updateOldValue();
        // eventually shows wl insertion
        if (response.assId) {
          var issueText = "<%=I18n.get("CLOSED_ISSUE_PREFILLED_ACTION")%>" + (issueRow.find("[name=ISSUE_DESCRIPTION],[fieldname=ISSUE_DESCRIPTION]").text());
          issueText = issueText.substr(0, Math.min(255, issueText.length));
          btnShowWorklog(el.parent());
        } else {
          hideWorklogEditorIfYouCan();
        }
        //eventually show notes
        if (response.histId) {
          showNotesForStatusChange(response.histId);
        } else {
          $(".notesEditor").fadeOut();
        }
      }
      hideSavingMessage();
    });
  }
}


function addIssue() {
  $('.paginatorNotFound').hide();
  showSavingMessage();
  var request = {CM: "ADD", DRAGENABLED: sortEnabled ? "yes" : "no", ISSUE_TASK: "<%=JSP.w(taskId)%>",ASSIGNEE:"<%=JSP.w(resourceId)%>"};
  $.post(contextPath+"/applications/teamwork/issue/issueAjaxController.jsp", request, function (response) {
    var editor = $(response);
    activateEditor(editor);

    $("#ISSUEFILTER .dataTableHead").show();
    $("#ISSUEFILTER .dataTableBody").prepend(editor);
    editor.find("textarea").focus();

    $(".issueRowFocused").removeClass("issueRowFocused");
    editor.addClass("issueRowFocused");

    hideSavingMessage();
  });
}


function delRow(el) {
  var issueRow = $(el).closest("[issueId]");
  var issueId = issueRow.attr("issueId");
  var isNew = issueId.startsWith("new");
  var taEmpty = issueRow.find("#ISSUE_DESCRIPTION").val() == '';
  if (isNew ) {
    if (taEmpty) {
      issueRow.fadeOut(500, function () {$(this).remove();});
    } else {
      el.confirm(function(){issueRow.fadeOut(500, function () {$(this).remove();});},"<%=I18n.get("FLD_CONFIRM_DELETE")%>")
    }
  } else {
    deletePreview("ISS_DEL", issueId, function(response) {  // callback function
      if (response && response.ok){
        issueRow.fadeOut(500, function () {$(this).remove();});
      }
    });
  }
}


function duplicateCurrentIssue(){
  var issueRow=$(".issueRowFocused");
  if (issueRow.length>0){
    clone(issueRow.find("td:first"),true);
  }
}


function clone(el,full) {
  var issueRow = $(el).closest("[issueId]");
  var issueId = issueRow.attr("issueId");

  if (issueRow.is(".inEdit")) { // if in edit duplicate the html
    var editor = issueRow.clone();
    editor.attr("issueId", "new_" + new Date().getTime());
    editor.find(".filesBox").empty();
    if (full) {
      editor.find("[name=ISSUE_DESCRIPTION]").val(issueRow.find("[name=ISSUE_DESCRIPTION]").val());
    }
    activateEditor(editor);
    issueRow.after(editor);
    editor.find("textarea").focus();
    editor.click();
  } else {

    showSavingMessage();
    var request = {CM: "CLONE", DRAGENABLED: sortEnabled ? "yes" : "no", ISSUEID: issueId, FULL: full ? "yes" : "no"};
    $.post(contextPath+"/applications/teamwork/issue/issueAjaxController.jsp", request, function (response) {
      var editor = $(response);
      activateEditor(editor);
      issueRow.after(editor);
      editor.find("textarea").focus();
      hideSavingMessage();
    });
  }
}


function showNotesForStatusChange(histId){ //questo viene visualizzato al top della pagina
  $("#floatWindow").append($("#insertStatusNotes"));
  $("#insertStatusNotes").attr("histId",histId).fadeIn();
}


function saveStatusNotes(el) {
  var theDiv = el.closest(".notesEditor");
  var histId = theDiv.attr("histId");
  showSavingMessage();
  var request = {CM:"SVHISTNOTES",histId:histId,notes:theDiv.find("textarea").val()};
  $.getJSON(contextPath+"/applications/teamwork/issue/issueAjaxControllerJson.jsp", request, function(response) {
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



function btnShowWorklog(el) {
  var issueRow = el.closest("[issueId]");
  var issueText;
  if (issueRow.is(".inEdit"))
    issueText = issueRow.find("[name=ISSUE_DESCRIPTION]").val();
  else
    issueText = issueRow.find(".issueDesc").text();

  openWorklogEditorCloseToElement(el,{assId:issueRow.attr("assId"),text: issueText, issueId:issueRow.attr("issueId"),title:"<%=I18n.get("ISSUE")%>: I#"+issueRow.attr("issueId")+"#"});
}


function reloadAssignee(el) {
  //console.debug("reloadAssignee",el);
  var editor = el.closest("[issueId]");
  var issueId = editor.attr("issueId");
  var taskField = editor.find("#ISSUE_TASK");
  if (taskField.isValueChanged()) {
    var taskId = taskField.val();
    var resName = editor.find("[name=ASSIGNEE_txt]").val();
    var resId = editor.find("[name=ASSIGNEE]").val();
    var request = {CM: "TASKCHANGED", TASKID: taskId, ISSUEID: issueId};
    showSavingMessage();

    $.getJSON(contextPath+"/applications/teamwork/issue/issueAjaxControllerJson.jsp", request, function (response) {
      jsonResponseHandling(response);
      if (response.ok && response.task) {
        //si mettono i nuovi limiti sulle date
        editor.find("#ISSUE_DATE_CLOSE_BY").attr("minvalue",response.task.startDate).attr("maxvalue",response.task.endDate);
      }
      hideSavingMessage();
    });
  }
}


<%-- --------------------------------------------------- BULK OPERATIONS  ----------------------------------------------
     serialize checked ids, inject an hidden on for and submit page -> refresh gratis --%>


function getCheckedIds() {
  var ret = [];
  $("#ISSUEFILTER .selector:checked").each(function () {
    ret.push($(this).closest("[issueId]").attr("issueId"));
  });
  return ret;
}


function injectSelectedIds() {
  var form = $("#issueMultiEditor");
  var ids = getCheckedIds();
  if (ids.length > 0) {
    // inject hidden ids field on form
    var hid = $("<input type='hidden' name='issueIds'>");
    hid.val(ids.join(","));
    form.append(hid);
  }
}


function performBulkAction(command, el) {
  //console.debug("performBulkAction",command);
  var ids = getCheckedIds();
  if (ids.length > 0) {
    var request = {CM:command,issueIds:ids.join(",")};
    if (el) {
      var bulkDiv = el.closest(".bulkData");
      bulkDiv.fillJsonWithInputValues(request);
    }

    showSavingMessage();
    $.getJSON(contextPath+"/applications/teamwork/issue/issueAjaxControllerJson.jsp", request, function(response) {
      jsonResponseHandling(response);
      if (response.ok) {
        closeBlackPopup(response);
        dataTableRefresh("ISSUEFILTER");
      }
      hideSavingMessage();
    });
  }
}


function addComment(el){
  showComments(el,true);
}

function showComments(el,add){
  var row=el.closest("[issueId]");
  var issueId = row.attr("issueId");
  openBlackPopup(contextPath+"/applications/teamwork/issue/issueComments.jsp?issueId="+ issueId+  (add?"&CM=ADD":"")   +"&_="+(new Date().getTime()),800,600,function(){
    refreshIssueRow(issueId,20,true);
  });
}

function drawFiles(issueId,files){
  //console.debug("drawFiles",issueId,files);
  var ndo=$("[issueId="+issueId+"] .filesBox");
  if (files.length>0) {
    ndo.find(".filesIcon").show();
  }
  //var icon = files.length ? $("<span/>").html("n ").addClass("teamworkIcon") : "";
  //ndo.append(icon);
  for (var i=0; i<files.length;i++) {
    ndo.append($.JST.createFromTemplate(files[i], "FILEBOX"));
  }
}

</script>
<%-- ------------------------------------------------------------- END JAVASCRIPT CONTROLLER -------------------------------------------------------------- --%>

<%
  //---------------------------------  INIZIO TABELLA ----------------------------------------------
  dataTable.drawPaginator(pageContext);
%><div alertonchange=true><%
  dataTable.drawTable(pageContext);
%></div><%

  dataTable.drawPaginatorPagesOnly(pageContext);
//---------------------------------  FINE TABELLA ----------------------------------------------


  ButtonBar bb = new ButtonBar();
  bb.id = "issueButtonBar";
  ButtonJS save = new ButtonJS(I18n.get("ISSUE_SAVE_ALL"), "saveAllIssues();");
  save.id = "saveAllBtn";
  save.additionalCssClass = "first big";
  bb.addButton(save);
  bb.toHtml(pageContext);
%>
