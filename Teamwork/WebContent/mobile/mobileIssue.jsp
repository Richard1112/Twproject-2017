<%@ page
        import="com.twproject.security.TeamworkPermissions, com.twproject.task.IssueBricks, org.jblooming.security.Permission, org.jblooming.waf.html.core.JST, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.input.ColorValueChooser, java.util.HashSet, java.util.Set" %>
<%----------------------------------------------------------  PAGES DEFINITION  ---------------------------------------------------------%>
<%-- -----------------------------------  issues filter page -------------------------------------- --%>
<div data-role="page" id="issueFilter" title="<%=I18n.get("ISSUES")%>">
  <div data-role="content">

    <div class="searchBox">
      <input type="text" placeHolder="<%=I18n.get("SEARCH")%> ..." onblur="issueSearch('SEARCH',$(this).val());"
             onkeypress="if(event.keyCode==13)this.blur();" class="search">
    </div>

    <div class="buttonList">

      <button onclick="issueSearch('PF_MY_OPEN_ISSUES',$(this).html());"
              class="full first"><%=I18n.get("PF_MY_OPEN_ISSUES")%>
      </button>
      <button onclick="issueSearch('PF_MY_OPEN_SEVERE_ISSUES',$(this).html());"
              class="full"><%=I18n.get("PF_MY_OPEN_SEVERE_ISSUES")%>
      </button>
      <button onclick="issueSearch('PF_MY_INSERTED_ISSUES',$(this).html());"
              class="full"><%=I18n.get("PF_MY_INSERTED_ISSUES")%>
      </button>
      <button onclick="issueSearch('PF_ISSUES_OPENED_RECENTLY',$(this).html());"
              class="full"><%=I18n.get("PF_ISSUES_OPENED_RECENTLY")%>
      </button>
      <button onclick="issueSearch('PF_ISSUES_CLOSED_RECENTLY',$(this).html());"
              class="full"><%=I18n.get("PF_ISSUES_CLOSED_RECENTLY")%>
      </button>
    </div>
  </div>


  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell left inputBox col6">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
    <div class="groupCell right inputBox col6">
      <button onmousedown="addIssue();" class="first addRootIssue"><span class="teamworkIcon big">P</span></button>
    </div>
  </div>

</div>


<%-- -----------------------------------  issue list page -------------------------------------- --%>
<div data-role="page" id="issueList" class="issueList">
  <div data-role="content" class="scroll">
    <div id="issueListPlace"></div>
  </div>


  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell left inputBox col6">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
    <div class="groupCell right inputBox col6">
      <button onmousedown="addIssue();" class="first addRootIssue"><span class="teamworkIcon big">P</span></button>
    </div>
  </div>


</div>

<%-- -----------------------------------  issue view page -------------------------------------- --%>
<div data-role="page" id="issueView" title="<%=I18n.get("ISSUES")%>">
  <div data-role="content">
    <div id="issueViewPlace"></div>


  </div>


  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell col4">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
    <div class="groupCell col4">
      <button class="first" onmousedown="closeIssue($(this));" style="(#=obj.canWriteIssue && obj.isOpen?'':'display:none'#)"><%=I18n.get("CLOSE")%>
      </button>
    </div>
    <div class="groupCell col4">
      <button class="button icon delete pullRight" msg="<%=I18n.get("FLD_CONFIRM_DELETE")%>" onmousedown="deleteIssue($(this));" style="display:(#=obj.canCreateIssue?'':'none'#)"><span class="icon">d</span>
      </button>
    </div>
  </div>


</div>


<%-- -----------------------------------  issue editor page -------------------------------------- --%>
<div data-role="page" class="editor noFooter" id="issueEditor" title="<%=I18n.get("ISSUE")%>">
  <div data-role="header" data-position="fixed">
    <div data-role="button" onmousedown="backPage();" class="close"></div>
    <div data-role="title"></div>
  </div>
  <div data-role="content" class="scroll">
    <div id="issueEditorPlace"></div>
  </div>


  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell left col6">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
    <div class="groupCell right col6">
      <button class="save" onmousedown="saveIssue($(this));"><%=I18n.get("SAVE")%></button>
    </div>
  </div>
</div>

<%----------------------------------------------------------  TEMPLATES  ---------------------------------------------------------%>
<div class="_mobileTemplates">
  <%-- ---------------------------------  ISSUE_ROW TEMPLATE ------------------------------------------------------------------------------------------------------------------- --%>

  <%=JST.start("ISSUE_ROW")%>

  <div data-role="swiper" class="listRow clearfix" issueId="(#=id#)" taskId=(#=obj.taskId#) assId=(#=obj.assId#) title="(#=obj.gravityName#)" onclick="editIssue($(this));">

    <div class="issueGravityColor" style="background-color:(#=obj.gravityColor#);"></div>
    <div class="issueStatusColorDiv">
      <div class="issueStatusColor" style="background-color:(#=obj.statusColor#);"></div>
    </div>

    <div class="issueContent">

      <div class="issueDescription">(#=htmlLinearize(obj.description,80)#)</div>
      <div class="issueMeta">
        <div class="issueTaskName" style="(#=obj.taskName?'':'display:none'#)">(#=obj.taskName#)</div>
      </div>
    </div>
    <div class="issueTools">
      <img class="face" src="(#=obj.assigneeAvatarUrl#)">
    </div>

    <%-- SWIPE BOX --%>
    <div class="swipeBox">


      <%--todo: add actions--%>
      <div class="swipeButton addWorklog"><span class="teamworkIcon" onclick="addWorklogFromIssue($(this));">w</span></div>
      <div class="swipeButton cloneIssue"><span class="teamworkIcon" onclick="cloneIssue($(this));">D</span></div>
      <div class="swipeButton delete"><span class="teamworkIcon delete" onclick="deleteIssue($(this));">d</span></div>

    </div>

  </div>
  <%=JST.end()%>

 <%-- ---------------------------------  DOCUMENT_ROW TEMPLATE ------------------------------------------------------------------------------------------------------------------- --%>
    <%=JST.start("DOCUMENT_ISSUE_ROW")%>
    <div class="documentRow" fileUID="(#=obj.uid#)" issueId="(#=obj.issueId#)" ck="(#=obj.ck#)">
      <a href="(#=obj.url#)" target="_blank"><img src="(#=contextPath+'/img/mime/'+obj.img#)" style="height:20px;">&nbsp;<span title="<%=I18n.get("PREVIEW_DOWNLOAD")%>">(#=obj.name#)</span>
      </a>
      <div class="delete"><span class="teamworkIcon delete" onclick="deleteIssueDocument($(this));">d</span></div>
    </div>
</div>
<%=JST.end()%>

  <%-- ---------------------------------  ISSUE_EDITOR TEMPLATE --------------------------------------------------------------------------- --%>
  <%=JST.start("ISSUE_EDITOR")%>
  <div class="editor" issueId="(#=obj.id#)">

    <div class="groupRow">

      <div class="groupCell inputBox touchEl col12">
        <%
          pageState.addClientEntry("ISSUE_DESCRIPTION", "(#=obj.description#)");
        %>
        <label><%=I18n.get("ISSUE_DESCRIPTION")%>
        </label>
        <textarea size="60" id="ISSUE_DESCRIPTION" name="ISSUE_DESCRIPTION"
                  class="description">(#=obj.description#)</textarea>


      </div>
    </div>
    <div class="groupRow">

      <div class="groupCell inputBox touchEl col12">
        <%

          pageState.addClientEntry("ISSUE_TASK", "(#=obj.taskId#)");
          pageState.addClientEntry("ISSUE_TASK_txt", "(#=obj.taskName#)");

          Set<Permission> perms = new HashSet();
          perms.add(TeamworkPermissions.issue_canCreate);
          perms.add(TeamworkPermissions.issue_canWrite);

          boolean showActiveOnly = true; //!JSP.ex(issue.getDescription()); // in order to see only active task when creating
          SmartCombo taskCombo = TaskBricks.getTaskCombo("ISSUE_TASK", showActiveOnly, perms, null, pageState);

          taskCombo.label = I18n.get("TASK");
          taskCombo.separator = "";
          taskCombo.fieldSize = 20;
          taskCombo.required = true;
          taskCombo.onValueSelectedScript = "reloadAssignee($(this));";
          taskCombo.toHtml(pageContext);

        %>
      </div>
    </div>
    <div class="groupRow">
      <div class="groupCell inputBox touchEl col12"><%

        pageState.addClientEntry("ASSIGNEE", "(#=obj.assigneeId#)");
        pageState.addClientEntry("ASSIGNEE_txt", "(#=obj.assigneeName#)");

      %><label for="ASSIGNEE_txt"><%=I18n.get("ASSIGNEE")%>
      </label><%
        JspHelper issueResDraw = new JspHelper("/applications/teamwork/issue/issueResourceDrawer.jsp");
        issueResDraw.parameters.put("SIZE", 30);
        issueResDraw.toHtml(pageContext);
 %>
      </div>
    </div>
    <div class="groupRow">
      <div class="groupCell inputBox touchEl col6">
        <label for="ISSUE_DATE_CLOSE_BY"><%=I18n.get("ISSUE_DATE_CLOSE_BY")%>
        </label>

        <input type="text" id="ISSUE_DATE_CLOSE_BY" name="ISSUE_DATE_CLOSE_BY"
               value="(#=obj.shouldCloseBy?new Date(obj.shouldCloseBy).format():''#)" readonly class="noFocus"
               onmousedown="issueOpenCalendar($('.teamworkIcon.calendarOpener'))">
        <span class="teamworkIcon calendarOpener" onmousedown="issueOpenCalendar($(this))">m</span>
      </div>
      <div class="groupCell inputBox touchEl col6">
        <%
          pageState.addClientEntry("ISSUE_WORKLOG_ESTIMATED_TIME", "(#=getMillisInHoursMinutes(obj.estimatedDuration)#)");
          TextField duration = TextField.getTimeInstance("ISSUE_WORKLOG_ESTIMATED_TIME");
          duration.label="ISSUE_WORKLOG_ESTIMATED_TIME";
          duration.fieldSize=30;
          duration.separator="";
          duration.toHtmlI18n(pageContext);
        %>
      </div>
    </div>

    <div class="groupRow">

      <div class="groupCell inputBox touchEl col6">
        <%
          pageState.addClientEntry("ISSUE_GRAVITY", "(#=obj.gravityId#)");

          ColorValueChooser gravityChooser = IssueBricks.getGravityChooser("ISSUE_GRAVITY", "ISSUE_GRAVITY", false, false, pageState);
          gravityChooser.separator = "";
          gravityChooser.height = 30;
          gravityChooser.showOpener = true;
          gravityChooser.displayValue = true;

          gravityChooser.label = I18n.get("ISSUE_GRAVITY");
          gravityChooser.toHtml(pageContext);
        %>

      </div>
      <div class="groupCell inputBox touchEl col6">
        <%
         pageState.addClientEntry("ISSUE_STATUS", "(#=obj.statusId#)");

          ColorValueChooser statusChooser = IssueBricks.getStatusChooser("ISSUE_STATUS", "ISSUE_STATUS", pageState);
          statusChooser.separator = "";
          statusChooser.height = 30;
          statusChooser.showOpener = true;
          statusChooser.displayValue = true;

          statusChooser.label = I18n.get("ISSUE_STATUS");
          statusChooser.toHtml(pageContext);
        %>
      </div>
    </div>

    <div class="issueDocumentsPlace">
      <div class="groupRow">
        <div class="groupCell inputBox col12">
          <label for="PENDING_PF"><%=I18n.get("ATTACHMENT")%></label>
          <div style="padding: 15px"> <div id="holder" (#=obj.readOnly?"disabled":""#)  class="uploadizeDrop picture" ></div></div>
        <input type="hidden" name="PENDING_PF" id="PENDING_PF">
      </div>
    </div>
  </div>
  </div>
  <%=JST.end()%>


  <%-- ---------------------------------  PAGE TITLE template -------------------------------------------------------------- --%>
  <%=JST.start("PAGE_TITLE_ISSUE_TASK")%>
  ##task## <%=I18n.get("ISSUES")%>
  <%=JST.end()%>

</div>

<%----------------------------------------------------------  DECORATORS  ---------------------------------------------------------%>
<script>

  $.JST.loadDecorator("ISSUE_ROW", function(domEl, issue) {
   var swipeBox =  domEl.find(".swipeBox");
   if(!issue.isMine){
     domEl.find(".swipeBox .addWorklog .teamworkIcon").attr("onclick","").css({"opacity": 0.3, color:"#fff"});//this is required to remove the swipe funcion
   }

   if(!issue.canCreateIssue){
     domEl.find(".swipeBox .cloneIssue .teamworkIcon").attr("onclick","").css({"opacity": 0.3, color:"#fff"});//this is required to remove the swipe funcion
   }

   if(!issue.canDeleteIssue){
     domEl.find(".swipeBox .delete .teamworkIcon").attr("onclick","").css({"opacity": 0.3, color:"#fff"});//this is required to remove the swipe funcion
   }
 });

  $.JST.loadDecorator("ISSUE_EDITOR", function (domEl, issue) {

    var ndo = domEl.find(".issueDocumentsPlace");
    if (!issue.canWriteIssue || issue.id == -1) {
      ndo.find("#holder").remove();
    } else {
      domEl.find("#holder").uploadize({
        fieldName: "attachment",
        url: "../mobile/ajax/mobileAjaxController.jsp",
        maxSize:<%=Uploader.getMaxUploadSizeInByte()%>,
        multi: false,
        showPlaceHolder: true,
        additionalRequestParameters: {CM: "ISSUEATTACH", issueId: issue.id},
        onLoadCallback: function (response) {
          if (response.ok) {
            currentPage.find(".issueDocumentsPlace").append($.JST.createFromTemplate(response.attachment, "DOCUMENT_ISSUE_ROW"));
            updateDocumentListinIssueCache(response.attachment);
            var updatedIssue = getIssueById(issue.id)
            var task = getTaskById(issue.taskId);
            if (task) {
              updateIssueListinTaskCache(updatedIssue);
            }

          }
        },
        fileAreaSelector: "#holder"
      });

    }

    var ndo = domEl.find(".issueDocumentsPlace");
    if (issue.documents) {
      for (var i = 0; i < issue.documents.length; i++) {

        var doc = issue.documents[i];
        doc.issueId = issue.id;

        var docRow = $.JST.createFromTemplate(issue.documents[i], "DOCUMENT_ISSUE_ROW");
        ndo.append(docRow);

        updateApplicationCacheElement(applicationCache.documents, issue.documents[i]);
        updateDocumentListinIssueCache(issue.documents[i]);
      }
    }

  });


</script>


<%------------------------------------------------------------------------  ISSUE PAGES FUNCTIONS  ---------------------------------------------------------%>
<script>


function issueFilterEnter(event, data, fromPage, isBack) {
  var page = $(this);
  if(!applicationCache.user.canCreateIssue){
    page.find(".addRootIssue").remove();
  }
}
function issueListEnter(event, data, fromPage, isBack) {

  var ndo = $("#issueListPlace");
  if (isBack){
    updateListWithCache("ISSUE_ROW", ndo, applicationCache.issues)
    enableSwipe();
  }

  var page = $(this);
  if ("SEARCH" == data.searchType)
    page.find("[data-role=title]").html("\"" + data.label + "\"");
  else
    page.find("[data-role=title]").html(data.label);


  var filter = {"CM": "ISSUESEARCH", "SCHTYPE": data.searchType, "SEARCH": data.label};
  filter.TASKID = data.taskId;

  callController(filter, function (response) {
    ndo.empty();
    var issues = response.issues;
    if (issues && issues.length > 0) {
      for (var i = 0; i < issues.length; i++)  {
        var issue = issues[i];
        var template = $.JST.createFromTemplate(issue, "ISSUE_ROW", true);
        updateApplicationCacheElement(applicationCache.issues, issue);

        ndo.append(template);
      }


      //enable swipe action on list row
      enableSwipe();

    } else {
      ndo.append($.JST.createFromTemplate({}, "NO_ELEMENT_FOUND"));
    }
  });

  if(!applicationCache.user.canCreateIssue){
    page.find(".addRootIssue").remove();
  }

}


function issueEditorEnter(event, data, fromPage, isBack) {
  var page = $(this);
  var edit = false;
  var issue ={};

  if (data && data.issueId) {
    edit = true;
    var issueId = data.issueId;
    issue = getIssueById(issueId);

  } else if(data) { //caso del save and clone ci sono i dati ma la issue è vuota

    issue = data;
    issue.id = -1;

  }else{
    issue.id = -1;
  }

  page.find("#issueEditorPlace").empty().append($.JST.createFromTemplate(issue, "ISSUE_EDITOR"));

  if (edit) {
    cvc_redraw(page.find(".cvcComponent"));
    cvc_redraw(page.find("[cvctype=ISSUE_STATUS]"));
  } else {
    cvc_selectFirstElement(page.find(".cvcComponent"));
  }
}


function issueSearch(searchType, label) {
  if (label.length)
    goToPage("issueList", {"searchType": searchType, "label": label});
}


function viewIssuesForTask(el) {
  goToPage("issueList", {"searchType": "TASK", "taskId": el.closest("[taskId]").attr("taskId")});
}


function viewIssue(el) {
  goToPage("issueView", {"issueId": el.attr("issueId")});
}


function editIssue(el) {
  goToPage("issueEditor", {"issueId": el.attr("issueId")});
}


function getIssueById(id) {
  for (i in applicationCache.issues) {
    if (applicationCache.issues[i].id == id)
      return applicationCache.issues[i];
  }
  return false;
}


function closeIssue(el) {
  var id = el.closest("[issueId]").attr("issueId");
  var command = {"CM": "CLOSEISSUE", "ID": id};
  callController(command, function (response) {
    var issue = response.issue;

    //update cache
    updateApplicationCacheElement(applicationCache.issues, issue);

    if (response.askWL) {
      $("#issueViewPlace").empty().append($.JST.createFromTemplate(issue, "ISSUE_VIEW", true));
      $("#issueViewPlace #wlEd").attr("assId", response.assId).show();
    } else {
      backPage();
    }
  });
}


function addIssue() {
  goToPage("issueEditor");
}


function addWorklogFromIssue(el){

  var div = el.closest("div[assId]");
  var assId = div.attr("assId");
  var issueid = div.attr("issueid");
  var taskName = div.find("div.issueTaskName").html();
  goToPage("worklogEditor",{"assId":assId,"taskName": taskName , issueId: issueid ,"wlDoneOn": (new Date()).toInt() });

}

function reloadAssignee(taskIdField) {
  //console.debug("reloadAssignee",hiddenAssigId,hiddenAssigId.isValueChanged());
  if (taskIdField.isValueChanged()) {
    showSavingMessage();
    var taskId = taskIdField.val();
    var assigneeName = $("#ASSIGNEE_txt").val();
    var assigneeId = $("#ASSIGNEE").val();
    var issueId = "";//todo
    var request = {CM: "TASKCHANGED", TASKID: taskId, ISSUEID: issueId, SIZE: 30};
    $.post("../applications/teamwork/issue/issueAjaxControllerJson.jsp", request, function (response) {
      hideSavingMessage();
    });
  }
}


function cloneIssue(el) {
  var div = el.closest("div[assId]");
  var issueid = div.attr("issueid");
    var clonedIssue = getIssueById(issueid)
    clonedIssue.id="";
    clonedIssue.description="";
    clonedIssue.documents="";
    goToPage("issueEditor",clonedIssue)
}


function saveIssue(el) {

  var ed = currentPage.find("#issueEditorPlace");
  ed.find(":input").clearErrorAlert();
  if (canSubmitForm("issueEditorPlace")) {
    var issueId = ed.find("div.editor").attr("issueId");
    var request = {"CM": "SVISSUE"};
    if (issueId != -1)
      request["OBJID"] = issueId;

    ed.fillJsonWithInputValues(request);
    request["ASSIGNED_BY"] = applicationCache.user.person.id;

    callController(request, function (response) {
      if (response.ok) {

        updateApplicationCacheElement(applicationCache.issues, response.issue);
        updateIssueListinTaskCache(response.issue);

        if(response.issue.askForWorklog){
          goToPage("worklogEditor",{"assId":response.issue.assId,"taskName": response.issue.taskName , issueId: response.issue.id ,"wlDoneOn": (new Date()).toInt() });
        }else{
          backPage();
        }
      }
    });
  }
}

function deleteIssue(el) {
  el.confirm(function () {
    var ed = el.closest("[issueId]");
    var issueId = ed.attr("issueId");
    var issue = getIssueById(issueId);
    var task = getTaskById(issue.taskId);
    var filter = { "CM": "DLISSUE", "ISSUEID": issueId};

    callController(filter, function (response) {
      //update cache
      delete applicationCache.issues[issueId];
      if (task && task.issues) {

        for (var i = 0; i < task.issues.length; i++) {
          if (task.issues[i].id == issueId) {
            task.issues.splice(i, 1);
            break;
          }

        }
        updateApplicationCacheElement(applicationCache.tasks,task)

      }
      //remove issue row
      currentPage.find("[issueId=" + issueId + "]").remove();
      var accordionIssue = currentPage.find("#issuesSize");
      if(accordionIssue){
        accordionIssue.html("("+task.issues.length+")");
      }
      //backPage();
    });
  });
}


function deleteIssueDocument(el) {
  el.confirm(function () {
    var ed = el.closest("[fileUID]");
    var docId = ed.attr("fileUID");
    var issueId = ed.attr("issueId");
    var issue;
    if (issueId) {
      var issue = getIssueById(issueId);
    }


    var filter = { "CM": "DELSCRSHT", "fileUID": docId, "issueId": issueId};

    callController(filter, function (response) {
        for (var i = 0; i < issue.documents.length; i++)  {
          if (issue.documents[i].uid == docId) {
            issue.documents.splice(i, 1);
            updateApplicationCacheElement(applicationCache.issues, issue);
            break;
          }
        }


      var task = getTaskById(issue.taskId);
      if(task){
        updateIssueListinTaskCache(issue);
      }
      //remove issue row
      el.closest(".documentRow").remove();
    });
  });


}


function issueOpenCalendar(el) {
  var inp = el.closest("[data-role=page]").find("#ISSUE_DATE_CLOSE_BY");
  setTimeout(function () {
    inp.dateField({
      inputField: inp,
      centerOnScreen: true,
      useYears: 1,
      useMonths: 3,
      width: "100%"
    });
  }, 100)
}

</script>
