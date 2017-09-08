<%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8" import="com.twproject.resource.Resource, com.twproject.security.TeamworkPermissions, com.twproject.task.Task, com.twproject.task.TaskBricks, org.jblooming.operator.Operator, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.core.JST, org.jblooming.waf.html.input.ColorValueChooser, org.jblooming.waf.html.input.SmartCombo, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, org.jblooming.waf.view.RestState, org.jblooming.waf.html.input.TextArea" %>
<%
  PageState realPageState = PageState.getCurrentPageState(request);

  //ATTENZIONE nel caso del widget, il logged operator non c'è per cui per construire i combo occorre usare la restState se c'è per creare i componenti, ma la pageState per tutto il resto
  RestState restState= (RestState) realPageState.getAttribute("restState");
  restState=restState==null?realPageState:restState;

  Operator logged = restState.getLoggedOperator();


  //horrible trick to setup right values in combo template. Works thanks to changes on smartCombo to preserve filled values
  restState.addClientEntry("resourceId", "(#=getResource(obj.assig).id#)");
  restState.addClientEntry("resourceId_txt", "(#=getResource(obj.assig).name#)");

  Task task= (Task) restState.getMainObject();
  //initialize smartcombo
  SmartCombo resCombo;
  if (logged!=null) {
    resCombo = TaskBricks.getAssignableResourceCombo(task, "resourceId", false, restState);
    resCombo.label = "";
    resCombo.separator = "";
    resCombo.script = "style=\"width:310px\" ";
    resCombo.linkToEntity = null;
  } else {
    resCombo=new SmartCombo("resourceId",Resource.class,"name");
    resCombo.readOnly=true;
  }

  //aggiungo un bottone aggiungi risorsa se ho i diritti
  if (logged!=null && logged.hasPermissionFor(TeamworkPermissions.resource_canCreate))
    resCombo.addEntityButton= new ButtonJS(I18n.get("ADD_RESOURCE"),"createNewResource($(this));");

  resCombo.onValueSelectedScript="updateResourceLoad($(this));";

  ColorValueChooser cvc;
  if (task==null)
    cvc=TaskBricks.getStatusChooser("status","STATUS",false,false);
  else
    cvc = task.bricks.getStatusChooser("status");
  cvc.showOpener = true;
  cvc.width = 120;
  cvc.height = 26;
  cvc.label = I18n.get("STATUS");
  cvc.separator = "<br>";
  cvc.preserveOldValue = true;
  cvc.init(pageContext);


  SmartCombo taskTypeCombo;
  if (task==null)
    taskTypeCombo = TaskBricks.getTaskTypeCombo("type", restState);
  else
    taskTypeCombo = TaskBricks.getTaskTypeCombo("type",task, restState);

  taskTypeCombo.separator = "<br>";
  taskTypeCombo.fieldSize = 15;



%>
<div id="gantEditorTemplates" style="display:none;">
  <%=JST.start("GANTBUTTONS")%>
  <div class="ganttButtonBar noprint">
    <div class="buttons">


      <button onclick="$('#workSpace').trigger('undo.gantt');return false;" class="button textual icon requireCanWrite" title="<%=I18n.get("UNDO")%>"><span class="teamworkIcon">&#39;</span></button>
      <button onclick="$('#workSpace').trigger('redo.gantt');return false;" class="button textual icon requireCanWrite" title="<%=I18n.get("REDO")%>"><span class="teamworkIcon">&middot;</span></button>
      <span class="ganttButtonSeparator requireCanWrite requireCanAdd"></span>
      <button onclick="$('#workSpace').trigger('addAboveCurrentTask.gantt');return false;" class="button textual icon requireCanWrite requireCanAdd" title="<%=I18n.get("INSERT_ABOVE")%>"><span class="teamworkIcon">l</span></button>
      <button onclick="$('#workSpace').trigger('addBelowCurrentTask.gantt');return false;" class="button textual icon requireCanWrite requireCanAdd" title="<%=I18n.get("INSERT_BELOW")%>"><span class="teamworkIcon">X</span></button>
      <span class="ganttButtonSeparator requireCanWrite requireCanInOutdent"></span>
      <button onclick="$('#workSpace').trigger('outdentCurrentTask.gantt');return false;" class="button textual icon requireCanWrite requireCanInOutdent" title="<%=I18n.get("UNINDENT")%>"><span class="teamworkIcon">.</span></button>
      <button onclick="$('#workSpace').trigger('indentCurrentTask.gantt');return false;" class="button textual icon requireCanWrite requireCanInOutdent" title="<%=I18n.get("INDENT")%>"><span class="teamworkIcon">:</span></button>
      <span class="ganttButtonSeparator requireCanWrite requireCanMoveUpDown"></span>
      <button onclick="$('#workSpace').trigger('moveUpCurrentTask.gantt');return false;" class="button textual icon requireCanWrite requireCanMoveUpDown" title="<%=I18n.get("MOVE_UP")%>"><span class="teamworkIcon">k</span></button>
      <button onclick="$('#workSpace').trigger('moveDownCurrentTask.gantt');return false;" class="button textual icon requireCanWrite requireCanMoveUpDown" title="<%=I18n.get("MOVE_DOWN")%>"><span class="teamworkIcon">j</span></button>

      <span class="ganttButtonSeparator requireCanDelete"></span>
      <button onclick="$('#workSpace').trigger('deleteFocused.gantt');return false;" class="button textual icon delete requireCanWrite" title="<%=I18n.get("DELETE")%>"><span class="teamworkIcon">&cent;</span></button>

      <span class="ganttButtonSeparator requireCanAddIssue"></span>
      <button onclick="$('#workSpace').trigger('addIssue.gantt');return false;" class="button textual icon requireCanAddIssue" title="<%=I18n.get("ADD_ISSUE")%>"><span class="teamworkIcon">i</span></button>


      <span class="ganttButtonSeparator"></span>
      <button onclick="$('#workSpace').trigger('expandAll.gantt');return false;" class="button textual icon " title="<%=I18n.get("EXPAND_ALL")%>"><span class="teamworkIcon">6</span></button>
      <button onclick="$('#workSpace').trigger('collapseAll.gantt'); return false;" class="button textual icon " title="<%=I18n.get("COLLAPSE_ALL")%>"><span class="teamworkIcon">5</span></button>

      <span class="ganttButtonSeparator"></span>
      <button onclick="$('#workSpace').trigger('zoomMinus.gantt'); return false;" class="button textual icon " title="<%=I18n.get("ZOOM_OUT")%>"><span class="teamworkIcon">)</span></button>
      <button onclick="$('#workSpace').trigger('zoomPlus.gantt');return false;" class="button textual icon " title="<%=I18n.get("ZOOM_IN")%>"><span class="teamworkIcon">(</span></button>

      <span class="ganttButtonSeparator"></span>
      <button onclick="print();return false;" class="button textual icon " title="<%=I18n.get("PRINT")%>"><span class="teamworkIcon">p</span></button>
      <span class="ganttButtonSeparator"></span>
      <button onclick="ge.gantt.showCriticalPath=!ge.gantt.showCriticalPath; ge.redraw();return false;" class="button textual icon requireCanSeeCriticalPath" title="<%=I18n.get("CRITICAL_PATH")%>"><span class="teamworkIcon">&pound;</span></button>
      <span class="ganttButtonSeparator requireCanSeeCriticalPath"></span>
      <button onclick="ge.splitter.resize(.1);return false;" class="button textual icon" ><span class="teamworkIcon">F</span></button>
      <button onclick="ge.splitter.resize(50);return false;" class="button textual icon" ><span class="teamworkIcon">O</span></button>
      <button onclick="ge.splitter.resize(100);return false;" class="button textual icon"><span class="teamworkIcon">R</span></button>
      <span class="ganttButtonSeparator"></span>
      <button onclick="$('#workSpace').trigger('fullScreen.gantt');return false;" class="button textual icon" title="<%=I18n.get("FULLSCREEN")%>" id="fullscrbtn"><span class="teamworkIcon">@</span></button>
      <button onclick="ge.element.toggleClass('colorByStatus' );return false;" class="button textual icon""><span class="teamworkIcon">§</span></button>

      <div id="saveGanttButton" onclick="saveGanttOnServer($(this));return false;" class="button first requireCanWrite disabled" title="<%=I18n.get("SAVE")%>"><%=I18n.get("SAVE")%></div>
    </div></div>
  <%=JST.end()%>
  <%=JST.start("TASKSEDITHEAD")%>
  <table class="gdfTable" cellspacing="0" cellpadding="0">
    <thead>
    <tr style="height:40px">
      <th class="gdfColHeader" style="width:35px; border-right: none"></th>
      <th class="gdfColHeader" style="width:25px;"></th>
      <th class="gdfColHeader gdfResizable" style="width:100px;"><%=I18n.get("CODE")%></th>
      <th class="gdfColHeader gdfResizable" style="width:300px;"><%=I18n.get("NAME")%></th>
      <th class="gdfColHeader"  align="center" style="width:17px;" title="<%=I18n.get("START_IS_MILESTONE")%>"><span class="teamworkIcon" style="font-size: 8px;">^</span></th>
      <th class="gdfColHeader gdfResizable" style="width:80px;"><%=I18n.get("START")%></th>
      <th class="gdfColHeader"  align="center" style="width:17px;" title="<%=I18n.get("END_IS_MILESTONE")%>"><span class="teamworkIcon" style="font-size: 8px;">^</span></th>
      <th class="gdfColHeader gdfResizable" style="width:80px;"><%=I18n.get("END")%></th>
      <th class="gdfColHeader gdfResizable" style="width:50px;"><%=I18n.get("GANTT_DURATION")%></th>
      <th class="gdfColHeader gdfResizable" style="width:20px;">%</th>
      <th class="gdfColHeader gdfResizable requireCanSeeDep" style="width:50px;"><%=I18n.get("DEPENDS")%></th>
      <th class="gdfColHeader gdfResizable" style="width:1000px; text-align: left; padding-left: 10px;"><%=I18n.get("ASSIGNEES")%></th>
    </tr>
    </thead>
  </table>
  <%=JST.end()%>

  <%=JST.start("TASKROW")%>
  <tr taskId="(#=obj.id#)" class="taskEditRow (#=obj.isParent()?'isParent':''#) (#=obj.collapsed?'collapsed':''#)" level="(#=level#)">
    <th class="gdfCell edit" align="right" style="cursor:pointer;"><span class="taskRowIndex">(#=obj.getRow()+1#)</span> <span class="teamworkIcon" style="font-size:12px;" >e</span></th>
    <td class="gdfCell noClip" align="center"><div class="taskStatus cvcColorSquare" status="(#=obj.status#)"></div></td>
    <td class="gdfCell"><input type="text" name="code" value="(#=obj.code?obj.code:''#)" placeholder="<%=I18n.get("TASK_CODE")%>"></td>
    <td class="gdfCell indentCell" style="padding-left:(#=obj.level*10+18#)px;">
      <div class="exp-controller" align="center"></div>
      <input type="text" name="name" value="(#=obj.name#)" placeholder="<%=I18n.get("TASK_NAME")%>">
    </td>
    <td class="gdfCell" align="center"><input type="checkbox" name="startIsMilestone"></td>
    <td class="gdfCell"><input type="text" name="start"  value="" class="date"></td>
    <td class="gdfCell" align="center"><input type="checkbox" name="endIsMilestone"></td>
    <td class="gdfCell"><input type="text" name="end" value="" class="date"></td>
    <td class="gdfCell"><input type="text" name="duration" autocomplete="off" value="(#=obj.duration#)"></td>
    <td class="gdfCell"><input type="text" name="progress" class="validated" entrytype="PERCENTILE" autocomplete="off" value="(#=obj.progress?obj.progress:''#)" (#=obj.progressByWorklog?"readOnly":""#)></td>
    <td class="gdfCell requireCanSeeDep"><input type="text" name="depends" autocomplete="off" value="(#=obj.depends#)" (#=obj.hasExternalDep?"readonly":""#)></td>
    <td class="gdfCell taskAssigs">(#=obj.getAssigsString()#)</td>
  </tr>
  <%=JST.end()%>

  <%=JST.start("TASKEMPTYROW")%>
  <tr class="taskEditRow emptyRow" >
    <th class="gdfCell" align="right"></th>
    <td class="gdfCell noClip" align="center"></td>
    <td class="gdfCell"></td>
    <td class="gdfCell"></td>
    <td class="gdfCell"></td>
    <td class="gdfCell"></td>
    <td class="gdfCell"></td>
    <td class="gdfCell"></td>
    <td class="gdfCell"></td>
    <td class="gdfCell"></td>
    <td class="gdfCell requireCanSeeDep"></td>
    <td class="gdfCell"></td>
  </tr>
  <%=JST.end()%>

  <%=JST.start("TASKBAR")%>
  <div class="taskBox taskBoxDiv" taskId="(#=obj.id#)" >
    <div class="layout (#=obj.hasExternalDep?'extDep':''#)">
      <div class="taskStatus" status="(#=obj.status#)"></div>
      <div class="taskProgress" style="width:(#=obj.progress>100?100:obj.progress#)%; background-color:(#=obj.progress>100?'red':'rgb(153,255,51);'#);"></div>
      <div class="milestone (#=obj.startIsMilestone?'active':''#)" ></div>
      <%--<div class="taskLabel">(#=obj.name#)</div>--%>
      <div class="taskLabel"></div>
      <div class="milestone end (#=obj.endIsMilestone?'active':''#)" ></div>
    </div>
  </div>
  <%=JST.end()%>

  <%-- ---------------------------------  CHANGE STATUS ------------------------------------------------------------------------------------------------------------------- --%>
  <%=JST.start("CHANGE_STATUS")%>
  <div class="taskStatusBox">
    <div class="taskStatus cvcColorSquare" status="STATUS_ACTIVE" title="<%=I18n.get("STATUS_ACTIVE")%>"></div>
    <div class="taskStatus cvcColorSquare" status="STATUS_DONE" title="<%=I18n.get("STATUS_DONE")%>"></div>
    <div class="taskStatus cvcColorSquare" status="STATUS_FAILED" title="<%=I18n.get("STATUS_FAILED")%>"></div>
    <div class="taskStatus cvcColorSquare" status="STATUS_SUSPENDED" title="<%=I18n.get("STATUS_SUSPENDED")%>"></div>
    <div class="taskStatus cvcColorSquare" status="STATUS_UNDEFINED" title="<%=I18n.get("STATUS_UNDEFINED")%>"></div>
  </div>
  <%=JST.end()%>

  <%-- ---------------------------------  TASK EDITOR ------------------------------------------------------------------------------------------------------------------- --%>
  <%=JST.start("TASK_EDITOR")%>
  <div class="ganttTaskEditor">
    <h2 class="taskData"><%=I18n.get("GANTT_TASK_EDITOR")%></h2>
    <table  cellspacing="1" cellpadding="5" width="100%" class="taskData table" border="0">
      <tr>
        <td width="200" style="height: 80px"  valign="top">
          <label for="code"><%=I18n.get("TASK_CODE")%></label><br>
          <input type="text" name="code" id="code" value="" size=15 class="formElements" autocomplete='off' maxlength=255 style='width:100%' oldvalue="1">
        </td>
        <td colspan="3" valign="top"><label for="name" class="required"><%=I18n.get("TASK_NAME")%></label><br><input type="text" name="name" id="name"class="formElements" autocomplete='off' maxlength=255 style='width:100%' value="" required="true" oldvalue="1"></td>
      </tr>


      <tr class="dateRow">
        <td nowrap="">
          <div style="position:relative">
            <label for="start"><%=I18n.get("START")%></label>&nbsp;&nbsp;&nbsp;&nbsp;
            <input type="checkbox" id="startIsMilestone" name="startIsMilestone" value="yes"> &nbsp;<label for="startIsMilestone"><%=I18n.get("ISMILESTONE")%></label>&nbsp;
            <br><input type="text" name="start" id="start" size="8" class="formElements dateField validated date" autocomplete="off" maxlength="255" value="" oldvalue="1" entrytype="DATE">
            <span title="<%=I18n.get("CALENDAR")%>" id="starts_inputDate" class="teamworkIcon openCalendar" onclick="$(this).dateField({inputField:$(this).prevAll(':input:first'),isSearchField:false});">m</span>          </div>
        </td>
        <td nowrap="">
          <label for="end"><%=I18n.get("END")%></label>&nbsp;&nbsp;&nbsp;&nbsp;
          <input type="checkbox" id="endIsMilestone" name="endIsMilestone" value="yes"> &nbsp;<label for="endIsMilestone"><%=I18n.get("ISMILESTONE")%></label>&nbsp;
          <br><input type="text" name="end" id="end" size="8" class="formElements dateField validated date" autocomplete="off" maxlength="255" value="" oldvalue="1" entrytype="DATE">
          <span title="<%=I18n.get("CALENDAR")%>" id="ends_inputDate" class="teamworkIcon openCalendar" onclick="$(this).dateField({inputField:$(this).prevAll(':input:first'),isSearchField:false});">m</span>
        </td>
        <td nowrap="" >
          <label for="duration" class=" "><%=I18n.get("DAYS")%></label><br>
          <input type="text" name="duration" id="duration" size="4" class="formElements validated durationdays" title="<%=I18n.get("HELP_DURATION")%>" autocomplete="off" maxlength="255" value="" oldvalue="1" entrytype="DURATIONDAYS">&nbsp;
        </td>
        <td><%cvc.toHtml(pageContext);%></td>
      </tr>

      <tr>
        <td nowrap><%taskTypeCombo.toHtmlI18n(pageContext);%></td>
        <td valign="top">
          <label for="relevance" class=" "><%=I18n.get("RELEVANCE")%></label><br>
          <input type="text" name="relevance" id="relevance" size="7" class="formElements validated percentile" autocomplete="off" maxlength="255" value="" oldvalue="1" entrytype="PERCENTILE">
        </td>

        <td valign="top" nowrap colspan="2">
          <label><%=I18n.get("PROGRESS")%></label>
          <input type="checkbox" id="progressByWorklog" name="progressByWorklog" value="yes" onClick="$('#progress').attr('readonly',$(this).is(':checked'))"> &nbsp;<label for="progressByWorklog"><%= I18n.get("PROGRESS_BY_WORKLOG")%></label><br>
          <input type="text" name="progress" id="progress" size="7" class="formElements validated percentile" autocomplete="off" maxlength="255" value="" oldvalue="1" entrytype="PERCENTILE">
        </td>
      </tr>

      <tr>
        <td colspan="5">
          <label for="description"><%=I18n.get("DESCRIPTION")%></label><br>
          <textarea rows="3" cols="30" id="description" name="description" class="formElements" style="width:100%" oldvalue="1"></textarea>
        </td>
      </tr>
    </table>
    <br>
    <h2><%=I18n.get("ASSIGNMENTS")%></h2>
    <table class="table dataTable">
      <tr>
        <th class="tableHead" style="width:340px;"><%=I18n.get("NAME")%></th>
        <th class="tableHead" style="width:200px;"><%=I18n.get("ROLE")%></th>
        <th class="tableHead" style="width:120px;"><%=I18n.get("WORKLOG_ESTIMATED_SHORT")%></th>
        <th class="tableHead" style="width:50px; text-align: center" id="addAssig" align="center"><span class="button textual"><span class="teamworkIcon edit">P</span></span></th>
      </tr>
    </table>
    <div class="assigsTableWrapper">
      <table id="assigsTable" class="table dataTable"></table>
    </div>

    <div class="buttonArea" style="padding: 10px 0">
      <span id="saveButton" class="button first" onClick="$(this).trigger('saveFullEditor.gantt');"><%=I18n.get("SAVE")%></span>
      <%--<span id="taskFullEditor" class="button" onClick="ge.element.trigger('openExternalEditor.gantt');"><%=I18n.get("FULL_EDIT")%></span>--%>
    </div>
  </div>
  <%=JST.end()%>


  <%-- ---------------------------------  ASSIGNMENT ROW ------------------------------------------------------------------------------------------------------------------- --%>
  <%=JST.start("ASSIGNMENT_ROW")%>
  <tr taskId="(#=obj.task.id#)" assId="(#=obj.assig.id#)" class="assigEditRow" >
    <%--<td><select name="resourceId"  class="formElements" (#=obj.assig.id.indexOf("tmp_")==0?"":"disabled"#) ></select></td>--%>
    <td style="width:340px;"><%resCombo.toHtml(pageContext);%></td>
    <td style="width:200px;"><select type="select" name="roleId"  class="formElements" style="width:200px;"></select></td>
    <td style="width:70px;"><input type="text" name="effort" value="(#=getMillisInHoursMinutes(obj.assig.effort)#)"  onchange="updateResourceLoad($(this));" size="5" class="formElements"></td>
    <td style="width:50px;" class="workloadInfo" align="center"></td>
    <td style="width:50px;" align="center"><span class="button icon textual delAssig del delete"><span class="teamworkIcon">d</span></span></td>
  </tr>
  <%=JST.end()%>
</div>


<%---------------------------------------------  GRID EDITOR TASK ROW MENU ---------------------------------------------------------------------------%>
<div id="taskRowMenu" class="divomo" style=" position: absolute;left:-10000px; top:-10000px;">
  <div class="divomoArrow" ></div>
  <div>
    <%new ButtonJS(I18n.get("EDIT"),"ge.element.trigger('openFullEditor.gantt')").toHtmlInTextOnlyModality(pageContext);%>
    <%new ButtonJS(I18n.get("ASSIGNMENTS"),"ge.element.trigger('openAssignmentEditor.gantt')").toHtmlInTextOnlyModality(pageContext);%>
    <hr>
    <%new ButtonJS(I18n.get("ADD_ISSUE"),"ge.element.trigger('addIssue.gantt')").toHtmlInTextOnlyModality(pageContext);%>
    <hr>
    <%new ButtonJS(I18n.get("FULL_EDIT"),"ge.element.trigger('openExternalEditor.gantt')").toHtmlInTextOnlyModality(pageContext);%>
  </div>
</div>




<%---------------------------------------------  LOG CHANGES ---------------------------------------------------------------------------%>
<div id="LOG_CHANGES_CONTAINER" class="microEdit mbBalloon" onclick="event.stopPropagation();return false;">
  <div style="bottom: -14px; left: 10px;" class="arrow s"></div>
  <div style="bottom: -12px; left: 10px;border-color:#fff" class="arrow border s "></div>
  <h3><%=I18n.get("LOG_GANTT_CHANGES")%></h3>

  <%
    TextArea ta = new TextArea("LOG_CHANGES", "", 25, 3, null);
    ta.label = "";
    ta.maxlength=2000;
    ta.script = "style='width:100%;'";
    ta.toHtml(pageContext);
  %>
</div>


<script type="text/javascript">
  function getResource(assig){
    var ret={id:0,name:""};
    if (assig && assig.resourceId){
      ret=ge.getResource(assig.resourceId);
    }
    return ret;
  }
  $.JST.loadDecorator("ASSIGNMENT_ROW", function(assigTr, taskAssig){
    var roleEl = assigTr.find("[name=roleId]");
    for(var i in taskAssig.task.master.roles){
      var role = taskAssig.task.master.roles[i];
      var optr = $("<option>");
      optr.val(role.id).html(role.name);
      if(taskAssig.assig.roleId == role.id)
        optr.attr("selected", "true");
      roleEl.append(optr);
    }

    if(taskAssig.task.master.permissions.canWrite && taskAssig.task.canWrite){
      assigTr.find(".delAssig").click(function(){
        var tr = $(this).closest("[assId]").fadeOut(200, function(){$(this).remove()});
      });
    }

  });


  function loadI18n(){
    GanttMaster.messages = {
      "CANNOT_WRITE":"<%=I18n.get("NO_RIGHTS_FOR_CHANGING_FOLLOWING_TASK")%>",
      "CHANGE_OUT_OF_SCOPE":"<%=I18n.get("NO_RIGHTS_FOR_UPDATE_PARENTS_OUT_OF_EDITOR_SCOPE")%>",
      "START_IS_MILESTONE":"<%=I18n.get("START_IS_MILESTONE")%>",
      "END_IS_MILESTONE":"<%=I18n.get("END_IS_MILESTONE")%>",
      "TASK_HAS_CONSTRAINTS":"<%=I18n.get("TASK_HAS_CONSTRAINTS")%>",
      "GANTT_ERROR_DEPENDS_ON_OPEN_TASK":"<%=I18n.get("GANTT_ERROR_DEPENDS_ON_OPEN_TASK")%>",
      "GANTT_ERROR_DESCENDANT_OF_CLOSED_TASK":"<%=I18n.get("GANTT_ERROR_DESCENDANT_OF_CLOSED_TASK")%>",
      "TASK_HAS_EXTERNAL_DEPS":"<%=I18n.get("TASK_HAS_EXTERNAL_DEPS")%>",
      "GANNT_ERROR_LOADING_DATA_TASK_REMOVED":"<%=I18n.get("GANNT_ERROR_LOADING_DATA_TASK_REMOVED")%>",
      "CIRCULAR_REFERENCE":"<%=I18n.get("CIRCULAR_REFERENCE")%>",
      "CANNOT_DEPENDS_ON_ANCESTORS":"<%=I18n.get("CANNOT_DEPENDS_ON_ANCESTORS")%>",
      "INVALID_DATE_FORMAT":"<%=I18n.get("INVALID_DATA")%>",
      "GANTT_ERROR_LOADING_DATA_TASK_REMOVED":"<%=I18n.get("GANTT_ERROR_LOADING_DATA_TASK_REMOVED")%>",
      "CANNOT_CLOSE_TASK_IF_OPEN_ISSUE":"<%=I18n.get("CANNOT_CLOSE_TASK_IF_OPEN_ISSUE")%>",
      "TASK_MOVE_INCONSISTENT_LEVEL":"<%=I18n.get("TASK_MOVE_INCONSISTENT_LEVEL")%>",
      "GANTT_QUARTER_SHORT":"<%=I18n.get("GANTT_QUARTER_SHORT")%>",
      "GANTT_SEMESTER_SHORT":"<%=I18n.get("GANTT_SEMESTER_SHORT")%>",
      "CANNOT_MOVE_TASK":"<%=I18n.get("CANNOT_MOVE_TASK")%>",
      "PLEASE_SAVE_PROJECT":"<%=I18n.get("PLEASE_SAVE_PROJECT")%>"
    };
  }



  <%------------------------------------------- ADD RESOURCE MANAGEMENT ---------------------------------------------------%>
  function createNewResource(el){
    var row = el.closest("tr[taskid]");
    var name=row.find("[name=resourceId_txt]").val();
    var url= contextPath + "/applications/teamwork/resource/resourceNew.jsp?CM=ADD&name="+encodeURI(name);

    openBlackPopup(url,700,320,function(response) {
      //fillare lo smart combo
      if (response && response.resId && response.resName) {
        //fillare lo smart combo e chiudere l'editor
        row.find("[name=resourceId]").val(response.resId);
        row.find("[name=resourceId_txt]").val(response.resName).focus().blur();

        //se è stata creato un login si comunicano i dati
        if (response.loginCreatedMessage)
          showFeedbackMessage("INFO",response.loginCreatedMessage);

      }
    })
  }

  <%------------------------------------------- ADD RESOURCE MANAGEMENT ---------------------------------------------------%>
  function taskTypeAddedCallback(taskType){
    //console.debug("taskTypeAddedCallback",taskType)
    if (taskType) {
      $("#type").val(taskType.id);
      $("#type_txt").val(taskType.name).blur();
    }

  }
</script>
