<%@ page
        import="com.twproject.security.SecurityBricks" %>
<%----------------------------------------------------------  PAGES DEFINITION  ---------------------------------------------------------%>

<%-- -----------------------------------  task filter page -------------------------------------- --%>
<div data-role="page" id="taskFilter" title="<%=I18n.get("TASKS")%>">
  <div data-role="content" class="scroll">
    <div class="searchBox">
      <input type="text" placeHolder="<%=I18n.get("SEARCH")%> ..."
             onblur="taskFreeSearch({'NAME_DESCRIPTION_NOTE_CODE':$(this).val()},'<%=I18n.get("TASKS")%>: '+$(this).val());"
             onkeypress="if(event.keyCode==13)this.blur();" class="search"><br>
    </div>

    <div class="buttonList">
      <button onclick="taskSearch('PF_MY_OPEN_TASK',$(this).html());" class="full first"><%=I18n.get("PF_MY_OPEN_TASK")%> </button>
      <button onclick="taskSearch('PF_MY_OPEN_PROJECT',$(this).html());" class="full"><%=I18n.get("PF_MY_OPEN_PROJECT")%></button>
      <button onclick="taskSearch('PF_MY_OVERDUE_TASK',$(this).html());"class="full"><%=I18n.get("PF_MY_OVERDUE_TASK")%></button>
      <button onclick="taskSearch('PF_MY_FORTHCOMING_MILESTONES',$(this).html());" class="full"><%=I18n.get("PF_MY_FORTHCOMING_MILESTONES")%></button>
      <button onclick="taskSearch('PF_OVERDUE_TASK',$(this).html());" class="full"><%=I18n.get("PF_OVERDUE_TASK")%></button>
    </div>
  </div>

  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell left inputBox col6">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
    <div class="groupCell right inputBox col6">
      <button onmousedown="addTask();" class="first addRootProject"><span class="teamworkIcon big">P</span></button>
    </div>
  </div>
</div>

<%-- -----------------------------------  task list page ----------------------------------------%>
<div data-role="page" id="taskList">
  <div data-role="content" class="scroll">
    <div id="taskListPlace"></div>
  </div>

  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell left inputBox col6">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
    <div class="groupCell right inputBox col6">
      <button onmousedown="addTask();" class="first addRootProject"><span class="teamworkIcon big">P</span></button>
    </div>
  </div>
</div>

<%-------------------------------------  task editor page --------------------------------------%>
<div data-role="page" class="editor" id="taskEditor" title="<%=I18n.get("TASK_GENERAL_TAB")%>">
  <div data-role="header" data-position="fixed">
    <div data-role="button" onmousedown="backPage();" class="close"></div>
    <div data-role="title"></div>
  </div>

  <div data-role="content" class="scroll">
    <div id="taskEditorPlace"></div>
  </div>


  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell left inputBox col6">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
    <div class="groupCell right inputBox col6">
      <button onmousedown="saveTask($(this));" class="save saveTask" ><%=I18n.get("SAVE")%></button>
      <button onmousedown="saveTaskStatus($(this));" style="display: none" class="save saveTaskStatus"><%=I18n.get("SAVE_STATUS")%></button>
    </div>
  </div>


</div>


<%-------------------------------------  Assignment editor page --------------------------------------%>
<div data-role="page" class="editor" id="assigEditor" title="<%=I18n.get("ASSIGNMENT")%>">
  <div data-role="header" data-position="fixed">
    <div data-role="button" onmousedown="backPage();" class="close"></div>
    <div data-role="title"></div>
  </div>

  <div data-role="content" class="scroll">
    <div id="assigEditorPlace"></div>
  </div>


  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell left inputBox col6">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
    <div class="groupCell right inputBox col6">
      <button onmousedown="saveAssig($(this));" class="save"><%=I18n.get("SAVE")%></button>
    </div>
  </div>


</div>


<%-------------------------------------  task view page -------------------------------------- --%>
<div data-role="page" id="taskView" title="<%=I18n.get("TASK")%>">
  <div data-role="content" class="scroll">
    <div id="taskViewPlace"></div>
  </div>


  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell left inputBox col6">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
    <div class="groupCell right inputBox col6">
      <button onmousedown="editTask($(this));" class="edit"><%=I18n.get("EDIT")%></button>
    </div>
  </div>
</div>


<%----------------------------------------------------------  TEMPLATES  ---------------------------------------------------------%>
<div class="_mobileTemplates">
  <%-- ---------------------------------  TASK_ROW TEMPLATE ------------------------------------------------------------------------------------------------------------------- --%>
  <%=JST.start("TASK_ROW")%>
  <div data-role="swiper" class="listRow" taskId="(#=id#)" assId="(#=obj.myAssId#)" onclick="viewTask($(this));">

    <div class="taskShort">
      <div class="pathSmall" style="(#=obj.path?'display:block':'display:none'#)">((#=obj.path#))</div>

      <div class="titleWrap">
        <div style="width: 40px"><span class="taskStatusColor" style="background-color:(#=statusColor#);"></span></div>
        <div style="display:table-cell;">
          <h2>(#=name#)
            <small style="(#=obj.code?'display:table-cell':'display:none'#)">((#=obj.code#))</small>
          </h2>

          <div class="taskMeta taskDates">
            <span><%=I18n.get("END")%>: (#=endDate#)</span>
            <%--<br><span><%=I18n.get("TASK_REMAINING_SHORT")%>: #todo 4</span>--%>
            <%--<span id="progressPlace" valign="top"></span>--%>
          </div>
        </div>
      </div>
    </div>

    <%-- SWIPE BOX --%>

    <div class="swipeBox">

      <%--todo: add actions--%>
      <div class="swipeButton"><span class="teamworkIcon" onclick="addExpensesFromTask($(this));">$</span></div>
      <div class="swipeButton"><span class="teamworkIcon" onclick="addWorklogFromTask($(this));">w</span></div>
      <div class="swipeButton"><span class="teamworkIcon play" onclick="startTimeCounter($(this));">a</span></div>
    </div>

  </div>
  <%=JST.end()%>

  <%-- ---------------------------------  TASK_VIEW TEMPLATE ------------------------------------------------------------------------------------------------------------------- --%>
  <%=JST.start("TASK_VIEW")%>
  <div class="editor task" taskId="(#=id#)" taskCode="(#=obj.code#)">


    <div class="taskHeader">
      <div class="pathSmall" style="(#=obj.path?'display:block':'display:none'#)">((#=obj.path#))</div>
      <div class="titleWrap">
        <div style="width: 40px"><span class="taskStatusColor" style="background-color:(#=statusColor#);"></span>
        </div>
        <div style="display:table-cell">
          <h2>(#=name#)
            <small style="(#=obj.code?'display:table-cell':'display:none'#)">((#=obj.code#))</small>
          </h2>
        </div>
      </div>
      <div class="taskMeta taskDates">
        <span class="taskStart"><%=I18n.get("START")%>: (#=startDate#)</span>
        <span class="taskEnd"><%=I18n.get("END")%>: (#=endDate#)</span>
        <%--<br><span><%=I18n.get("TASK_REMAINING_SHORT")%>: #todo 4</span>--%>
        <span id="progressPlace" valign="top"></span>
      </div>
    </div>
    <div id="taskDesc(#=obj.id#)" class="taskDesc" style="(#=obj.description?'':'display:none'#); max-height: 150px; overflow: hidden" onclick="fit_content('taskDesc(#=obj.id#)')">
      <div style="(#=obj.description?'':'display:none'#)">
        <label><%=I18n.get("DESCRIPTION")%>:</label>

        <p>(#=obj.description#)</p>
      </div>

      <div style="(#=obj.notes?'':'display:none'#)">
        <label><%=I18n.get("NOTES")%>:</label>
        (#=obj.notes#)
      </div>

      <div style="(#=obj.tags?'':'display:none'#)">
        <label><%=I18n.get("TAGS")%>:</label>
        <span>(#=obj.tags#)</span>
      </div>
    </div>

    <%-- ACCORDION --%>

    <div class="accordion">
      <h3 class="touchEl"><%=I18n.get("ASSIGNMENTS")%> <span class="small" id="assigSize"
                                                             style="(#=obj.assignments && obj.assignments.length>0?'':'display:none'#);">
        ((#=obj.assignments.length#))</span></h3>

      <div class="assigPlace addAssig">
        <div class="buttonGroup addAssig">
          <button onclick="addAssig()" class="full first"><%=I18n.get("ADD_ASSIGNMENT")%></button>
        </div>
      </div>

      <h3 class="touchEl"><%=I18n.get("ISSUES")%> <span class="small" id="issuesSize"
                                                        style="(#=obj.issues && obj.issues.length>0?'':'display:none'#);">
             ((#=obj.issues?obj.issues.length:""#))</span>
      </h3>

      <div class="issuesPlace">
        <div class="buttonGroup addIssue">
          <button onclick="addTaskIssue()" class="full first"><%=I18n.get("ADD_ISSUE")%></button>
        </div>
      </div>

      <h3 class="touchEl"><%=I18n.get("DOCUMENTS")%> <span class="small" id="documentSize" style="(#=obj.documents && obj.documents.length>0?'':'display:none'#)">((#=obj.documents?obj.documents.length:""#))</span></h3>

      <div class="documentsPlace">
        <div class="groupRow">
          <div class="groupCell inputBox col12">
            <label for="PENDING_PF"><%=I18n.get("ATTACHMENT")%></label>
            <div style="padding: 15px">
              <div id="holder" (#=obj.readOnly?"disabled":""#) class="uploadizeDrop picture" ></div>
          </div>
          <input type="hidden" name="PENDING_PF" id="PENDING_PF">
        </div>
      </div>
    </div>

    <h3 class="touchEl">Subtask<%--<%=I18n.get("SUBTASK")%>--%>
      <span class="small" id="subtaskSize" style="(#=obj.children && obj.children.length>0?'':'display:none'#);">
            ((#=obj.children?children.length:""#))</span></h3>

    <div class="childrenPlace">
      <div class="buttonGroup addChild">
        <button onclick="addTask($(this))" class="full first"><%=I18n.get("ADD_CHILD_TASK")%></button>
      </div>
    </div>
  </div>

  <div class="entityLog" style="padding: 0 10px"><%=I18n.get("LAST_MODIFIED_BY")%> (#=lastModifier#) - (#=dateToRelative(lastModified)#)</div>
  <%
    String to = "<a h" +
            "=\"mailto:" + twEmail + "?subject=TASK #(#=obj.id#)# ATTACHMENT\">" + twEmail + "</a>";
    String sendMessage = I18n.get("ATTACH_DOC_TO_TASK_EMAIL", to);
    if (pop3isConf) {
  %>
  <div class=" entityLog" style="padding: 0 10px"><%=sendMessage%> TASK #(#=obj.id#)# ATTACHMENT</div>
  <%
    }
  %>
</div>
<%=JST.end()%>

<%-- ---------------------------------  CHILD_ROW TEMPLATE ------------------------------------------------------------------------------------------------------------------- --%>
<%=JST.start("CHILD_TASK_ROW")%>
<div class="childRow" taskId="(#=id#)" onclick="viewTask($(this))">
  <table class="">
    <tr>
      <td style="width:5px">
        <div class="taskStatusColor" style="background-color:(#=statusColor#);"></div>
      </td>
      <td>
        (#=name#)
      </td>
    </tr>
  </table>
</div>
<%=JST.end()%>

<%-- ---------------------------------  DOCUMENT_ROW TEMPLATE ------------------------------------------------------------------------------------------------------------------- --%>
<%=JST.start("DOCUMENT_TASK_ROW")%>
<div class="documentRow" docId="(#=obj.id#)" taskId="(#=obj.taskId#)">
  <a onclick="viewDocument($(this))"   docId="(#=obj.id#)" target="_blank"><img src="(#=contextPath+'/img/mime/'+obj.img#)"
                                                                                style="height:20px;">&nbsp;<span
          title="<%=I18n.get("PREVIEW_DOWNLOAD")%>">(#=obj.name#)</span>
  </a>

  <div class="delete"><span class="teamworkIcon delete" onclick="deleteDocument($(this));">d</span></div>
</div>
</div>
<%=JST.end()%>

<%-- ---------------------------------  ASSIG_TASK_ROW TEMPLATE ------------------------------------------------------------------------------------------------------------------- --%>
<%=JST.start("ASSIG_TASK_ROW")%>
<div data-role="swiper" class="assigRow" assId="(#=obj.id#)" taskId="(#=obj.taskId#)" resourceId="(#=obj.resId#)">
  <table class="" width="100%">
    <tr>
      <td>
        <img src="(#=obj.resAvatarUrl#)" class="face">
      </td>
      <td>
        <div style="width:190px" class="ellipsis">(#=resName#)<br><span class="small">(#=roleCode#)</span></div>
      </td>
      <td align="right" valign="top" class="small (#=done>estimated?'warning':''#)"><%=I18n.get("WORKLOG_SHORT")%>
        (#=getMillisInHoursMinutes(done)#) / (#=getMillisInHoursMinutes(estimated)#)
      </td>
    </tr>
  </table>
  <%--<div class="swipeBox">--%>
  <%--&lt;%&ndash;todo: add actions&ndash;%&gt;--%>
  <%--<div class="swipeButton delete"><span class="teamworkIcon delete" onclick="deleteAssig($(this));">d</span></div>--%>
  <%--</div>--%>

</div>
<%=JST.end()%>
<%-- ---------------------------------  ISSUE_TASK_ROW TEMPLATE ------------------------------------------------------------------------------------------------------------------- --%>

<%=JST.start("ISSUE_TASK_ROW")%>

<div data-role="swiper" class="listRow clearfix" issueId="(#=id#)" title="(#=obj.gravityName#)">

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
    <img src="(#=obj.assigneeAvatarUrl#)" class="face">
    <%--<div style="position: absolute; bottom: 10px; right:10px" onclick="addIssue($(this));" ><span class="teamworkIcon" style="font-size: 150%">e</span></div>--%>
  </div>

  <%-- SWIPE BOX --%>
  <div class="swipeBox">
    <%--todo: add actions--%>
    <div class="swipeButton delete"><span class="teamworkIcon delete" onclick="deleteIssue($(this));">d</span></div>
  </div>

</div>
<%=JST.end()%>

<%-- ---------------------------------  TASK_EDITOR TEMPLATE ------------------------------------------------------------------------------------------------------------------- --%>
<%=JST.start("TASK_EDITOR")%>
<div class="editor" taskId="(#=obj.id#)" parentId="(#=obj.parent#)">
  <div class="groupRow">
    <div class="groupCell inputBox touchEl col12">
      <%

        pageState.addClientEntry("PARENT_ID", "(#=obj.parent#)");
        TextField.hiddenInstanceToHtml("PARENT_ID", pageContext);

        pageState.addClientEntry("TASK_CODE", "(#=obj.code#)");
        TextField taskCode = new TextField("TASK_CODE", "");
        taskCode.label = "CODE";
        taskCode.fieldSize = 30;
        taskCode.toHtmlI18n(pageContext);
      %>
    </div>
  </div>
  <div class="groupRow">
    <div class="groupCell inputBox touchEl col12">
      <%
        pageState.addClientEntry("TASK_NAME", "(#=obj.name#)");
        TextField taskName = new TextField("TASK_NAME", "");
        taskName.label = "NAME";
        taskName.fieldSize = 30;
        taskName.required = true;
        taskName.toHtmlI18n(pageContext);
      %>
    </div>
  </div>
  <div class="groupRow">
    <div class="groupCell inputBox touchEl col12">
      <label><%=I18n.get("DESCRIPTION")%>
      </label>
      <textarea size="60" class="description"  id="DESCRIPTION" name="DESCRIPTION" onfocus="fit_content('DESCRIPTION')" onblur="fit_content('DESCRIPTION',true)">(#=obj.description#)</textarea>
    </div>
  </div>
  <div class="groupRow">
    <div class="groupCell inputBox touchEl col6">
      <label for="START"><%=I18n.get("START")%>
      </label>
      <input type="text" id="START" name="START" class="calendarInput (#=obj.canWriteTask?'':'disabled'#)"
             value="(#=obj.startDate?obj.startDate:currentDate.format()#)" style="width: 100%" class="noFocus"
             onclick="this.blur(); taskOpenCalendar($(this))">
      <span class="teamworkIcon calendarOpener" onclick="taskOpenCalendar($(this))">m</span>
    </div>
    <div class="groupCell inputBox touchEl col6">
      <label for="END"><%=I18n.get("END")%>
      </label>
      <input type="text" id="END" name="END" class="calendarInput (#=obj.canWriteTask?'':'disabled'#)"
             value="(#=obj.endDate?obj.endDate:currentDate.format()#)" style="width: 100%" class="noFocus"
             onclick="this.blur(); taskOpenCalendar($(this))">
      <span class="teamworkIcon calendarOpener" onclick="taskOpenCalendar($(this))">m</span>
    </div>
    <div class="groupCell inputBox touchEl col6">

      <label for="STARTISMILESTONE" class="checkbox checkbox--material">
        <input id="STARTISMILESTONE" name="STARTISMILESTONE" type="checkbox"
               class="checkbox__input checkbox-skin__input" (#=obj.startIsMilestone ?'checked':''#) >
        <div class="checkbox__checkmark checkbox-skin__checkmark"></div>
        <%=I18n.get("ISMILESTONE")%>
      </label>

    </div>
    <div class="groupCell inputBox touchEl col6">

      <label for="ENDISMILESTONE" class="checkbox checkbox-skin">
        <input id="ENDISMILESTONE" name="ENDISMILESTONE" type="checkbox"  class="checkbox__input checkbox-skin__input (#=obj.canWriteTask?'':'disabled'#)"
        (#=obj.endIsMilestone ?'checked':''#) >
        <div class="checkbox__checkmark checkbox-skin__checkmark"></div>
        <%=I18n.get("ISMILESTONE")%>
      </label>
    </div>
  </div>

  <div class="groupRow">
    <div class="groupCell inputBox touchEl col6">
      <%
        pageState.addClientEntry("TASK_DURATION", "(#=obj.duration#)");
        TextField taskDays = TextField.getTimeInstance("TASK_DURATION");
        taskDays.label = "DURATION";
        taskDays.type = "number";
        taskDays.fieldSize = 30;
        taskDays.separator = "";
        taskDays.toHtmlI18n(pageContext);
      %>
    </div>
    <div class="groupCell inputBox touchEl col6">
      <%
        pageState.addClientEntry("STATUS", "(#=obj.status#)");
        ColorValueChooser taskStatusChooser = TaskBricks.getStatusChooser("STATUS", "STATUS", false, false);
        taskStatusChooser.separator = "";
        taskStatusChooser.height = 30;
        taskStatusChooser.showOpener = true;
        taskStatusChooser.displayValue = true;
        taskStatusChooser.label = I18n.get("STATUS");
        taskStatusChooser.toHtml(pageContext);
      %>
    </div>
  </div>
  <div class="groupRow">
    <div class="groupCell inputBox touchEl col12">
      <%
        pageState.addClientEntry("TASK_TYPE", "(#=obj.typeId#)");
        pageState.addClientEntry("TASK_TYPE_txt", "(#=obj.type#)");
        SmartCombo taskTypeCombo = TaskBricks.getTaskTypeCombo("TASK_TYPE", pageState);
        taskTypeCombo.label = I18n.get("TYPE");
        taskTypeCombo.separator = "";
        taskTypeCombo.fieldSize = 20;
        //taskTypeCombo.onValueSelectedScript = "reloadAssignee($(this));";
        taskTypeCombo.toHtml(pageContext);
      %>
    </div>
  </div>
  <div class="groupRow">
    <div class="groupCell inputBox touchEl col6">
      <%
        pageState.addClientEntry("RELEVANCE", "(#=obj.relevance#)");
        TextField taskRelevance = TextField.getTimeInstance("RELEVANCE");
        taskRelevance.label = "RELEVANCE";
        taskRelevance.type = "number";
        taskRelevance.fieldSize = 30;
        taskRelevance.separator = "";
        taskRelevance.toHtmlI18n(pageContext);
      %>
    </div>
    <div class="groupCell inputBox touchEl col6">
      <%
        pageState.addClientEntry("PROGRESS", "(#=obj.progress#)");
        TextField taskProgress = TextField.getTimeInstance("PROGRESS");
        taskProgress.label = "PROGRESS";
        taskProgress.type = "number";
        taskProgress.fieldSize = 30;
        taskProgress.separator = "";
        taskProgress.toHtmlI18n(pageContext);
      %>
    </div>
  </div>

</div>
<%=JST.end()%>

<%-- ---------------------------------  ASSIG_EDITOR TEMPLATE ------------------------------------------------------------------------------------------------------------------- --%>
<%=JST.start("ASSIG_EDITOR")%>
<div class="editor" taskId="(#=obj.taskId#)" assigId="(#=obj.id#)">
  <div class="groupRow">
    <div class="groupCell inputBox touchEl col12">
      <%
        pageState.addClientEntry("ASSIGNEE", "(#=obj.resId#)");
        pageState.addClientEntry("ASSIGNEE_txt", "(#=obj.resName#)");

        SmartCombo resCombo = TaskBricks.getAssignableResourceCombo(null, "ASSIGNEE", false, pageState);

        resCombo.fieldSize = 30;
        resCombo.label = I18n.get("ASSIGNEE");
        resCombo.linkToEntity = null;
        resCombo.required = true;
        resCombo.onValueSelectedScript = "updateResourceLoad($(this));";
        resCombo.toHtml(pageContext);

      %>
    </div>
  </div>
  <div class="groupRow">
    <div class="groupCell inputBox touchEl col12">
      <%
        pageState.addClientEntry("ASSIG_ROLE", "(#=obj.roleId#)");
        pageState.addClientEntry("ASSIG_ROLE_txt", "(#=obj.role#)");
        SmartCombo roles = SecurityBricks.getRoleComboForAssignments("ASSIG_ROLE", null, false, pageState);
        roles.required = true;
        roles.fieldSize = 20;
        roles.toHtmlI18n(pageContext);
      %>
    </div>
  </div>
  <div class="groupRow">
    <div class="groupCell inputBox touchEl col12">
      <%
        pageState.addClientEntry("ESTIMATE_WORKLOG", "(#=getMillisInHoursMinutes(obj.estimated)#)");
        TextField estim = TextField.getDurationInMillisInstance("ESTIMATE_WORKLOG");
        estim.label = "ASSIG_SET_EST";
        estim.toHtmlI18n(pageContext);
      %>
    </div>
  </div>
</div>
<%=JST.end()%>
</div>
<script>

<%----------------------------------------------------------  DECORATORS  ---------------------------------------------------------%>

$.JST.loadDecorator("TASK_ROW", function (domEl, task) {
  domEl.find("#progressPlace").append($.JST.createFromTemplate({width: 50, perc: task.progress}, "PERCENTILE"));

  if (task.endIsMilestone == true)
    domEl.find(".taskDates").addClass("isMilestone");

  if (parseInt(task.end) < new Date().getTime())
    domEl.find(".taskDates").addClass("warning");


  if (!task.myAssId) {
    domEl.find(".swipeBox .teamworkIcon").attr("onclick","").css({"opacity": 0.3, color:"#fff"});//this is required to remove the swipe funcion
  }

});

$.JST.loadDecorator("TASK_EDITOR", function (domEl, task) {

  //dobbiamo discriminare il new dall'edit
  var isNewRoot = task.id==-1 && task.parent == "undefined";
  var isNewChild = task.id==-1 && (task.parent != "undefined");
  var isEdit = task.id!=-1;
  var canEdit = false;
  var canEditStatus = false;

  //console.debug("isNewRoot", isNewRoot)
  //console.debug("isNewChild", isNewChild)
  //console.debug("isEdit", isEdit)

  if(isNewRoot){
var user =applicationCache.user;
    canEdit = user.canCreateProject
    canEditStatus = true;

  }else if(isNewChild){
    var parent = getTaskById(task.parent)
    canEdit = parent.canAddTask;
    canEditStatus = true;

  }else if(isEdit){
    canEdit = task.canWriteTask;
    canEditStatus = task.canChangeTaskStatus;
  }

  //console.debug("canEdit", canEdit)
  //console.debug("canEditStatus", canEditStatus)

  if (!canEdit){
    domEl.find("#TASK_CODE").attr("readonly","true").addClass("disabled");
    domEl.find("#TASK_NAME").attr("readonly","true").addClass("disabled");
    domEl.find("#TASK_DURATION").attr("readonly","true").addClass("disabled");
    domEl.find("#TASK_TYPE").parents(".touchEl").css({"pointer-events": "none"}).addClass("disabled");
    domEl.find("#RELEVANCE").attr("readonly","true").addClass("disabled");
    domEl.find("#PROGRESS").attr("readonly","true").addClass("disabled");
    domEl.find("#ENDISMILESTONE").attr("disabled","true").addClass("disabled");
    domEl.find("#STARTISMILESTONE").attr("disabled","true").addClass("disabled");
    domEl.find("#DESCRIPTION").attr("readonly","true").addClass("disabled");
    domEl.find("#START").attr("readonly","true").addClass("disabled");
    domEl.find("#END").attr("readonly","true").addClass("disabled");
  }

  if (!canEditStatus){
    domEl.find(".cvcColorSquare").parents(".touchEl").css({"pointer-events": "none"}).addClass("disabled");
  }

  if (!task.myAssId) {
    domEl.find(".swipeBox .teamworkIcon").attr("onclick","").css({"opacity": 0.3, color:"#fff"});//this is required to remove the swipe funcion
  }

});


$.JST.loadDecorator("TASK_VIEW", function (domEl, task) {
  //console.debug("loadDecorator:\"TASK_VIEW\"",task);

  domEl.find("#progressPlace").append($.JST.createFromTemplate({width: 50, perc: task.progress}, "PERCENTILE"));

  if (task.startIsMilestone == true)
    domEl.find(".taskStart").addClass("isMilestone");

  if (task.endIsMilestone == true)
    domEl.find(".taskEnd").addClass("isMilestone");

  if (parseInt(task.end) < new Date().getTime())
    domEl.find(".taskDates").addClass("warning");

  //add assigs
  var ndo = domEl.find(".assigPlace");
  if (task.assignments) {
    for (var i = 0; i < task.assignments.length; i++) {
      var assigRow = $.JST.createFromTemplate(task.assignments[i], "ASSIG_TASK_ROW");
      if (task.canAssign)
        assigRow.bind("click", function () {
          editAssig($(this));
        });
      ndo.append(assigRow);

    }
  }

  if (!task.canAssign) {
    ndo.find("div.addAssig").remove()
  }

  //add issue
  var ndo = domEl.find(".issuesPlace");
  if (task.issues) {
    for (var i = 0; i < task.issues.length; i++) {

      var issueRow = $.JST.createFromTemplate(task.issues[i], "ISSUE_TASK_ROW");
      issueRow.bind("click", function () {
        editIssue($(this));
      });
      ndo.append(issueRow);

      if(!task.canDeleteIssue){
        ndo.find(".swipeBox .delete .teamworkIcon").attr("onclick","").css({"opacity": 0.3, color:"#fff"});//this is required to remove the swipe funcion
      }
      updateApplicationCacheElement(applicationCache.issues, task.issues[i]);
    }

  }
  if (!task.canAddIssue) {
    ndo.find("div.addIssue").remove()
  }

  if (!task.canAddDocument) {
    ndo.find("#holder").remove();
  } else {
    domEl.find("#holder").uploadize({
      fieldName: "attachment",
      url: "../mobile/ajax/mobileAjaxController.jsp",
      maxSize:<%=Uploader.getMaxUploadSizeInByte()%>,
      multi: false,
      showPlaceHolder: true,
      additionalRequestParameters: {CM: "TASKATTACH", taskId: task.id},
      onLoadCallback: function (response) {
        if (response.ok) {
          currentPage.find(".documentsPlace").append($.JST.createFromTemplate(response.attachment, "DOCUMENT_TASK_ROW"));
          var doc = response.taskDoc;
          doc.taskId = task.id;
          updateDocumentListinTaskCache(doc);
        }
      },
      fileAreaSelector: "#holder"
    });

  }

  var ndo = domEl.find(".documentsPlace");
  if (task.documents) {
    for (var i = 0; i < task.documents.length; i++) {

      var doc = task.documents[i];
      doc.taskId = task.id;

      var docRow = $.JST.createFromTemplate(task.documents[i], "DOCUMENT_TASK_ROW");
      ndo.append(docRow);

      if(!task.canDeleteDocument){
        ndo.find(".delete").attr("onclick","").css({"opacity": 0.3, color:"#fff"});//this is required to remove the swipe funcion
      }

      updateApplicationCacheElement(applicationCache.documents, task.documents[i]);
      updateDocumentListinTaskCache(task.documents[i]);
    }
  }
  //add children
  ndo = domEl.find(".childrenPlace");
  if (task.children) {
    for (var i = 0; i < task.children.length; i++) {
      ndo.append($.JST.createFromTemplate(task.children[i], "CHILD_TASK_ROW"));
    }
  }
  if (!task.canWriteTask && !task.canChangeTaskStatus) {
    $("#taskView").find("button.edit").hide()
  } else {
    $("#taskView").find("button.edit").show()
  }

  if(!task.canAddTask){
    ndo.find("div.addChild").remove()
  }

});

$.JST.loadDecorator("ISSUE_TASK_ROW", function(domEl, issue) {
  var swipeBox =  domEl.find(".swipeBox");
  if(!issue.canCreateIssue){
    domEl.find(".swipeBox .delete .teamworkIcon").attr("onclick","").css({"opacity": 0.3, color:"#fff"});//this is required to remove the swipe funcion
  }
});

//$.JST.loadDecorator("ASSIG_TASK_ROW", function(domEl, assig) {
//  var swipeBox =  domEl.find(".swipeBox");
//
//  if(!assig.canCreate){ //todo
//    swipeBox.find(".delete").remove()
//  }
//});
<%----------------------------------------------------------  TASK PAGES FUNCTIONS  ---------------------------------------------------------%>

function taskFreeSearch(searchParameters, label) {

  var search_value = label.replace("Projects: ", "");
  if(!search_value || search_value.length == 0)
    return false;

  var params = {"searchType": "FREE", "label": label};
  for (var p in searchParameters)
    params[p] = searchParameters[p];

  goToPage("taskList", params);
}

function taskSearch(filterName, label) {
  goToPage("taskList", {"<%=Fields.FLD_FILTER_NAME%>": filterName, "label": label});
}


function viewTask(el, event) {
  goToPage("taskView", {"taskId": el.attr("taskId")});
}

function taskFilterEnter(event, data, fromPage, isBack) {
  var page = $(this);
  if(!applicationCache.user.canCreateProject){
    page.find(".addRootProject").remove();
  }
}

function taskListEnter(event, data, fromPage, isBack) {

  var ndo = $("#taskListPlace");
  if (isBack)
    updateListWithCache("TASK_ROW", ndo, applicationCache.tasks);

  var page = $(this);
  page.find("[data-role=title]").html(data.label);

  ndo.empty();

  var filter = {"CM": "TASKSEARCH"};
  $.extend(filter, data);

  if (data.searchType)
    filter.SCHTYPE = data.searchType;

  callController(filter, function (response) {
    if (response.tasks && response.tasks.length > 0) {
      for (var i = 0; i < response.tasks.length; i++) {
        var task = response.tasks[i];
        var template = $.JST.createFromTemplate(task, "TASK_ROW");

        if (applicationCache.countedAssignment && applicationCache.countedAssignment == task.myAssId) {
          template.find("span.play").removeClass("play").addClass("stop").html("s").attr("onclick", "showTimeCounterEditor()");
        }
        ndo.append(template);
        updateApplicationCacheElement(applicationCache.tasks, task);
      }
      //enable swipe action on list row
      enableSwipe();

    } else {
      ndo.append($.JST.createFromTemplate({}, "NO_ELEMENT_FOUND"));
    }

  });
  if(!applicationCache.user.canCreateProject || (data.FLNM && data.FLNM=="PF_MY_OPEN_PROJECT")){
    page.find(".addRootProject").remove();
  }
}


function taskViewEnter(event, data, fromPage, isBack) {

  var taskId = data.taskId;
  var task = getTaskById(taskId);

  if (task && task.loadComplete) { // load from cache
    ndo = $("#taskViewPlace").empty().append($.JST.createFromTemplate(task, "TASK_VIEW"));
    $(".accordion").accordion({
      heightStyle: "content",
      animate: 200,
      collapsible: true,
      active: false,
      activate: function (event, ui) {
        enableSwipe();
        refreshIscroll();
      }

    });

  } else {
    var filter = {"CM": "LOADTASK", "ID": taskId};
    callController(filter, function (response) {
      ndo = $("#taskViewPlace").empty();
      ndo.append($.JST.createFromTemplate(response.task, "TASK_VIEW"));
      $(".accordion").accordion({
        heightStyle: "content",
        animate: 200,
        collapsible: true,
        active: false,
        activate: function (event, ui) {
          enableSwipe();
          refreshIscroll();
        }
      });
      //update cache
      updateApplicationCacheElement(applicationCache.tasks, response.task);
    });
  }
  //enableSwipe();

}

function taskEditorEnter(event, data, fromPage, isBack) {
  var edit = false;
  var page= $(this)

  if (data.taskId) {
    edit = true;
    var taskId = data.taskId;
    var task = getTaskById(taskId);
  } else {
    var task = {id: -1}
    if (data.parent) {
      task.parent = data.parent;
    }else{
      task.parent = -1;
    }
  }

  var taskEditor = $(this).find("#taskEditorPlace");
  taskEditor.empty().append($.JST.createFromTemplate(task, "TASK_EDITOR"));
  if (edit) {
    cvc_redraw(taskEditor.find(".cvcComponent"));
  } else {
    cvc_selectFirstElement(taskEditor.find(".cvcComponent"));
  }


  //bind dateField on dates, duration
  taskEditor.find("#START,#END,#TASK_DURATION").click(function () {
    var input = $(this);
    if (input.is("[entrytype=DATE]")) {
      input.dateField({
        inputField: input,
        callback: function (d) {
          $(this).blur();
        }
      });
    }
  }).blur(function () {
            var inp = $(this);
            if (inp.validateField()) {
              resynchDates(inp.attr("id"), "START", "startIsMilestone", "TASK_DURATION", "END", "endIsMilestone");
            }
          });

  taskEditor.find("#startIsMilestone,#endIsMilestone").click(function () {
    var inp = $(this);
    resynchDates(inp.attr("id"), "START", "startIsMilestone", "TASK_DURATION", "END", "endIsMilestone");
  });

  if(task.canChangeTaskStatus && !task.canWriteTask){
    page.find(".saveTask").hide();
    page.find(".saveTaskStatus").show();
  }
}

function taskEditorLeave(event, data, fromPage, isBack) {
  $(this).find("#taskEditorPlace").empty();
}


function assigEditorEnter(event, data, fromPage, isBack) {
  var taskId = data.OBJID;
  var task = getTaskById(taskId);
  var assig = {id: -1, taskId: taskId}
  if (data.assigId) {
    for (var i = 0; i < task.assignments.length; i++) {
      if (task.assignments[i].id == data.assigId)
        assig = task.assignments[i];
    }
  }

  var assigEditor = $(this).find("#assigEditorPlace");
  assigEditor.empty().append($.JST.createFromTemplate(assig, "ASSIG_EDITOR"));

}

function assigEditorLeave(event, data, fromPage, isBack) {
  $(this).find("#assigEditorPlace").empty();
}


function editTask(el) {
  goToPage("taskEditor", {"taskId": $("div.editor.task").attr("taskId")});
}

function addTask() {
  var parent = currentPage.find("div.editor.task").attr("taskId");
  goToPage("taskEditor", {"parent": parent},true);
}

function addAssig() {
  goToPage("assigEditor", {"OBJID": currentPage.find("div.editor.task").attr("taskId")});
}

function addTaskIssue() {
  var task = getTaskById(currentPage.find("div.editor.task").attr("taskId"))
  goToPage("issueEditor", {"taskId": task.id, "taskName": task.name});
}

function editAssig(el) {
  goToPage("assigEditor", {"OBJID": el.attr("taskId"), "assigId": el.attr("assId")});
}

function getTaskById(taskId) {
  for (i in applicationCache.tasks) {
    if (applicationCache.tasks[i].id == taskId)
      return applicationCache.tasks[i];
  }
  return false;
}


function saveTask(el) {

  var ed = el.closest("#taskEditor").find("#taskEditorPlace");
  ed.find(":input").clearErrorAlert();
  if (canSubmitForm("taskEditorPlace")) {
    var taskid = ed.find("div.editor").attr("taskId");
    var request = {"CM": "SAVEPROJECT"};
    if (taskid != -1)
      request["OBJID"] = taskid;
    ed.fillJsonWithInputValues(request);
    callController(request, function (response) {
      if (response.ok) {
        updateApplicationCacheElement(applicationCache.tasks, response.task);
        if (response.task.parentId) {
          updateSubTaskListinTaskCache(response.task);
        }
        goToPage("taskView", {"taskId": response.task.id});
      }
    });
  }

}function saveTaskStatus(el) {

  var ed = el.closest("#taskEditor").find("#taskEditorPlace");
  ed.find(":input").clearErrorAlert();
  if (canSubmitForm("taskEditorPlace")) {
    var taskid = ed.find("div.editor").attr("taskId");
    var request = {"CM": "SAVETASKSTATUS"};
    request["OBJID"] = taskid;
    request["NEW_STATUS"] = ed.find("#STATUS").val();
    callController(request, function (response) {
      if (response.ok) {
        updateApplicationCacheElement(applicationCache.tasks, response.task);
        if (response.task.parentId) {
          updateSubTaskListinTaskCache(response.task);
        }
        goToPage("taskView", {"taskId": response.task.id});
      }
    });
  }

}


function saveAssig(el) {

  var ed = el.closest("#assigEditor").find("#assigEditorPlace");
  ed.find(":input").clearErrorAlert();
  if (canSubmitForm("assigEditorPlace")) {
    var assigId = ed.find("div.editor").attr("assigId");
    var taskId = ed.find("div.editor").attr("taskId");
    var request = {"CM": "SAVEASSIGNMENT"};
    if (assigId != -1)
      request["OBJID"] = assigId;
    request["TASK_ID"] = taskId;
    ed.fillJsonWithInputValues(request);

    callController(request, function (response) {
      if (response.ok) {
        updateApplicationCacheElement(applicationCache.tasks, response.task);
        backPage();
      }
    });
  }
}


function addWorklogFromTask(el) {

  var div = el.closest("div[assId]");
  var assId = div.attr("assId");
  var taskId = div.attr("taskId");

  goToPage("worklogEditor", {"assId": assId, "taskName": applicationCache.tasks[taskId].name, "wlDoneOn": (new Date()).toInt() });

}

function addExpensesFromTask(el) {

  var div = el.closest("div[assId]");
  var assId = div.attr("assId");
  var taskId = div.attr("taskId");

  goToPage("expenseEditor", {"assId": assId, "taskName": applicationCache.tasks[taskId].name });

}

function taskOpenCalendar(el) {
  var inp = el.closest(".touchEl").find(".calendarInput");
  if (inp.is(".componentOverlayTF"))
    return;
  var config = {
    inputField: inp,
    centerOnScreen: true,
    useYears: 1,
    useMonths: 3,
    showToday: true,
    //notAfterMillis: new Date().getTime() + 3600000 * 24,
    width: "100%"
  };
  inp.dateField(config);
}

</script>
