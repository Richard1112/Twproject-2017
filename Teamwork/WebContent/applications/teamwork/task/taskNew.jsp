<%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8" %><%@ page import="com.twproject.operator.TeamworkOperator,
                 com.twproject.resource.Person,
                 com.twproject.security.RoleTeamwork,
                 com.twproject.security.SecurityBricks,
                 com.twproject.security.TeamworkPermissions,
                 com.twproject.task.Task,
                 com.twproject.task.TaskBricks,
                 com.twproject.task.businessLogic.TaskController,
                 com.twproject.waf.TeamworkPopUpScreen,
                 org.jblooming.designer.DesignerField, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.constants.Fields,
                 org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.container.ButtonBar,
                 org.jblooming.waf.html.core.JST, org.jblooming.waf.html.input.*, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jblooming.waf.html.button.ButtonSupport"%><%

  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new TaskController(), request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {

    Task task = (Task) pageState.getMainObject();

    // Form -----------------------------------------------------------------------------------------------
    PageSeed self = pageState.thisPage(request);
    self.mainObjectId = task.getId();
    String parId = pageState.getEntry(Fields.PARENT_ID).stringValueNullIfEmpty();
    self.addClientEntry(Fields.PARENT_ID, JSP.w(parId));

    Form f = new Form(self);
    f.id = "mainForm";
    f.encType = Form.MULTIPART_FORM_DATA;
    f.alertOnChange = true;
    pageState.setForm(f);
    f.start(pageContext);

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    Person loggedPerson = logged.getPerson();

    String addType=pageState.getEntry("ADD_TYPE").stringValueNullIfEmpty();
    addType=JSP.ex(addType)?addType:"ADD_TASK";

%>

<h1><%=I18n.get(addType)%></h1>

<table border="0" class="table" cellpadding="5">

<tr>
  <td valign="top" style="height: 80px"><% TextField tf = new TextField("TASK_CODE", "<br>");
    tf.fieldSize = 15;
    boolean canWrite = task.hasPermissionFor(logged,TeamworkPermissions.task_canWrite);
    tf.readOnly = !canWrite;
    tf.script="style='width:100%' onChange=\"$(this).removeAttr('invalid')\"";
    tf.toHtmlI18n(pageContext); %>
    <small><%=task.isNew() ? "" : "(id:&nbsp;" + task.getId() + ")"%></small>
  </td>
  <td colspan="3" valign="top"><%
    TextField tfn = new TextField("TASK_NAME", "<br>");
    tfn.fieldSize = 50;
    tfn.readOnly = !canWrite;
    tfn.required = true;
    tfn.script="style='width:100%'";
    tfn.toHtmlI18n(pageContext);
  %></td>
</tr>

  <tr class="dateRow">
    <td nowrap>
      <div style="position:relative"><%

        DateDurationInput ddi = new DateDurationInput("START", "STARTISMILESTONE", "END", "ENDISMILESTONE", "TASK_DURATION","datesChanged($(this));", pageState);

      %><%=JSP.makeTag("label", "for=\"START\"", I18n.get("START"))%>&nbsp;&nbsp;&nbsp;&nbsp;<%
        CheckField cbx = ddi.startMilestoneField;
        cbx.disabled = !canWrite;
        cbx.disabled = !canWrite;
        cbx.label = I18n.get("ISMILESTONE");
        ddi.drawStartMiles(pageContext);

        DateField df = ddi.startField;
        df.readOnly = !canWrite || task.getPreviousesSize() > 0; //task.getChildrenSize()>0 ||
        df.labelstr = "";
        df.separator = "<br>";
        ddi.drawStart(pageContext);
      %></div>
    </td>
    <td nowrap>
      <%=JSP.makeTag("label", "for=\"END\"", I18n.get("END"))%>&nbsp;&nbsp;&nbsp;&nbsp;<%
      CheckField cby = ddi.endMilestoneField;
      cby.disabled = !canWrite;
      cby.label = I18n.get("ISMILESTONE");
      ddi.drawEndMiles(pageContext);

      df = ddi.endField;
      df.readOnly = !canWrite;
      df.labelstr = "";
      df.separator = "<br>";
      ddi.drawEnd(pageContext);
    %></td>
    <td nowrap><%

      TextField td = ddi.durationField;
      td.separator = "<br>";
      td.readOnly = !canWrite || (task.isStartIsMilestone() && task.isEndIsMilestone()); //|| task.getChildrenSize()>0
      td.label = I18n.get("DAYS");
      td.toolTip = I18n.get("HELP_DURATION");
      ddi.drawDuration(pageContext);
    %>&nbsp;
    </td>

    <td valign="top">
      <div style="margin:0; position: relative;">
        <%

          ColorValueChooser cvc = task.bricks.getStatusChooser("STATUS");
          cvc.showOpener = true;
          cvc.width = 120;
          cvc.height = 26;
          cvc.readOnly = !canWrite;
          cvc.label = I18n.get(cvc.fieldName);
          cvc.separator = "<br>";
          cvc.preserveOldValue = false;
          cvc.toHtml(pageContext);

        %>

      </div>
    </td>
  </tr>
  <tr>
    <td nowrap><%
      SmartCombo sc = TaskBricks.getTaskTypeCombo("TASK_TYPE", task, pageState);
      sc.separator = "<br>";
      sc.fieldSize = 15;
      sc.readOnly = !canWrite;

      sc.toHtmlI18n(pageContext);
    %></td>

    <td valign="top">
      <%
        tf = TextField.getPercentileInstance("RELEVANCE");
        tf.separator = "<br>";
        tf.readOnly = !canWrite;
        tf.fieldSize = 7;

        tf.toHtmlI18n(pageContext); %>&nbsp;<%


    %></td>

    <td valign="top" nowrap colspan="2">
      <label><%=I18n.get("PROGRESS")%>&nbsp;&nbsp;&nbsp;&nbsp;</label>
      <%
        CheckField pbw = new CheckField("PROGRESS_BY_WORKLOG", "", false);
        pbw.label = I18n.get("PROGRESS_BY_WORKLOG");
        pbw.toolTip = I18n.get("HELP_PROGRESS_BY_WORKLOG");
        pbw.disabled = !canWrite;
        if (!pbw.disabled) {
          pbw.additionalOnclickScript = "$(this).attr('checked') ? disableProgress(this) : enableProgress(this)";
        }

        pbw.toHtmlI18n(pageContext);

        tf = TextField.getPercentileInstance("PROGRESS");
        tf.separator = "<br>";
        tf.readOnly = !canWrite || task.isProgressByWorklog();
        tf.fieldSize = 7;
        tf.label = "";
        tf.toHtmlI18n(pageContext);

      %>
      <script type="text/javascript">
        function enableProgress(o) {
          $('#<%=tf.id%>').removeAttr('readonly');
          $('#<%=tf.id%>').css('backgroundColor', '#ffffff');
        }
        function disableProgress(o) {
          $('#<%=tf.id%>').attr('readonly', 'readonly');
          $('#<%=tf.id%>').css('backgroundColor', '#f3f3f3');
        }
      </script>
    </td>
  </tr>
  <tr>
    <td colspan="4"><label><%=I18n.get("TASK_DESCRIPTION")%></label>
      <%
        TextArea ta = new TextArea("DESCRIPTION", "", 22, 5, null);
        ta.readOnly = !canWrite;
        ta.label = "";
        ta.script = "style='width:100%;height:60px;'";
        ta.maxlength = 2000;
        ta.setAutosize(60, 500, 20);
        ta.toHtml(pageContext);


        if (I18n.isActive("CUSTOM_FEATURE_USE_PROJECT_COLOR")){
          ColorChooser colorChooser = new ColorChooser("COLOR");
          colorChooser.label=I18n.get("COLOR");
          colorChooser.separator="<br>";
          colorChooser.showTextField=false;

      %><div style="float: left; width: 10%;"><% colorChooser.toHtml(pageContext);%> </div><%
        }
      %>

      <div style="float: right; width: <%=I18n.isActive("CUSTOM_FEATURE_USE_PROJECT_COLOR")?90:100%>%;">
        <%
          TagBox tags= new TagBox("TASK_TAGS",Task.class,task.getArea());
          tags.label=I18n.get("TAGS");
          tags.separator="<br>";
          tags.fieldSize=36;
          tags.script="style='width:100%'";
          tags.toHtml(pageContext);

        %></div>

    </td>
  </tr>
</table>
<%
  //---------------------------------------------------- CUSTOM FIELDS ------------------------------------------------------------------------
  if (TaskBricks.hasCustomField() ){
    %><div class="clearfix" style="margin: 10px 0"><%
    for (int i=1; i<7; i++) {
      DesignerField dfStr = DesignerField.getCustomFieldInstance( "TASK_CUSTOM_FIELD_",i, task,!canWrite, false, false, pageState);
      if (dfStr!=null){
        dfStr.separator="<br>";
        %><table style="float:left;"><tr><td><%dfStr.toHtml(pageContext);%></td></tr></table><%
      }
    }
    %></div><%
  }

  f.end(pageContext);

  //---------------------------------------------------- ASSIGNMENTS ------------------------------------------------------------------------
  // sono fuori dalla form perchè sono gestiti separatamente con un json

  SmartCombo bulkRoles = SecurityBricks.getRoleComboForAssignments("bkRoleId", task, false, pageState);
  bulkRoles.fieldSize = 30;
  bulkRoles.label="";
  bulkRoles.separator="";
  bulkRoles.preserveOldValue=false;

  ButtonSupport workgroup = new ButtonJS ("openWorkGroup();");
  workgroup.toolTip = I18n.get("BULK_ASSIGNMENT");
  workgroup.label = "";
  workgroup.iconChar = "r";
  workgroup.additionalCssClass="first small lreq20";


  if (task.hasPermissionFor(logged,TeamworkPermissions.assignment_canCRW)){
%><h2><%=I18n.get("ASSIGNMENTS")%><span style="float: right"><%workgroup.toHtml(pageContext);%></span></h2>

  <table class="table dataTable fixHead fixFoot" assigs=true id="assigTable">
    <thead class="dataTableHead">
    <tr>
      <th class="tableHead"><input type="checkbox" id="ck_chall" class="selector lreq20" onclick="selUnselAll($(this));" title="<%=I18n.get("SELECT_DESELECT_ALL")%>"></th>
      <th class="tableHead"><%=I18n.get("NAME")%></th>
      <th class="tableHead"><%=I18n.get("ROLE")%></th>
      <th class="tableHead" colspan="2"><%=I18n.get("WORKLOG_ESTIMATED_SHORT")%></th>
      <th class="tableHead" style="text-align: center"><span class="teamworkIcon edit" style="cursor: pointer" onclick="addAssigRow({});">P</span></th>
    </tr>
    </thead>
    <tbody class="dataTableBody" id="assigRowsPlace"></tbody>
    <tfoot>
      <tr><td id="bulkPlace" colspan="99"></td></tr>
    </tfoot>

  </table>
<div id="bulkOp" style="display:none;">
  <div id="bulkRowSel"></div>
  <div><%=I18n.get("ASSIGN_THIS_ROLE")%> &nbsp;&nbsp; <%bulkRoles.toHtml(pageContext);%> <span class="button small lreq20" onclick="assignRoleToSelected($(this));"><%=I18n.get("PROCEED")%></span></div>
</div>
<%
  }

  ButtonBar buttonBar = new ButtonBar();

  ButtonJS save = new ButtonJS(I18n.get("SAVE"),"createNewProject($(this));");
  save.additionalCssClass="first big";
  save.enabled = canWrite;
  buttonBar.addButton(save);

  pageState.setFocusedObjectDomId(tfn.id);

  buttonBar.loggableIdentifiableSupport = task;
  buttonBar.toHtml(pageContext);


  pageState.addClientEntry("resourceId","##resId##");
  pageState.addClientEntry("resourceId_txt","##resName##");
  SmartCombo resCombo = task.bricks.getAssignableResourceCombo("resourceId", false, pageState);
  resCombo.fieldSize = 30;
  resCombo.label = "";
  resCombo.linkToEntity=null;
  resCombo.onValueSelectedScript="updateResourceLoad($(this));";
  resCombo.separator="";
  //aggiungo un bottone aggiungi risorsa se ho i diritti
  if (pageState.getLoggedOperator().hasPermissionFor(TeamworkPermissions.resource_canCreate))
    resCombo.addEntityButton= new ButtonJS(I18n.get("ADD_RESOURCE"),"createNewResource($(this))");
  resCombo.script="style=width:100%";



  pageState.addClientEntry("roleId","##roleId##");
  pageState.addClientEntry("roleId_txt","##roleName##");
  SmartCombo roles = SecurityBricks.getRoleComboForAssignments("roleId", task, false, pageState);
  roles.fieldSize = 30;
  roles.label="";
  roles.separator="";
  roles.script=" style='width:100%;' role";



  RoleTeamwork projectManagerRole = TaskBricks.getProjectManagerRole(loggedPerson.getArea());
  RoleTeamwork customerRole = TaskBricks.getCustomerRole(loggedPerson.getArea());
  RoleTeamwork workerRole = TaskBricks.getWorkerRole(loggedPerson.getArea());
%>
<div id="gantEditorTemplates" style="display:none;">
<%-- ---------------------------------  ASSIGNMENT ROW ------------------------------------------------------------------------------------------------------------------- --%>
<%=JST.start("ASSIGNMENT_ROW")%>
<tr class="assigRow" >
  <td><input type="checkbox" class="selector lreq20" onclick="refreshBulk($(this));"></td>
  <td><%resCombo.toHtml(pageContext);%></td>
  <td><%roles.toHtml(pageContext);%></td>
  <td><input type="text" name="WL"  size="5" class="formElements validated durationmillis" onchange="updateResourceLoad($(this))" entryType="DURATIONMILLIS"></td>
  <td class="workloadInfo" align="center"></td>
  <td align="center"><span class="teamworkIcon delAssig delete" style="cursor: pointer" onclick="$(this).closest('tr').remove();refreshBulk($('#ck_chall'));">d</span></td>
</tr>
<%=JST.end()%>
</div>

<jsp:include page="plan/workloadUtilities.js.jsp"></jsp:include>
<script>
  $(function(){
    $("#gantEditorTemplates").loadTemplates().remove();
    var isRoot=!$("[name=PARENT_ID]").val();
    if ($("table[assigs]").size()>0 && isRoot){
      <%if (projectManagerRole!=null){%>addAssigRow({resId: "<%=loggedPerson.getId()%>", resName: "<%=loggedPerson.getName()%>", roleId: "<%=projectManagerRole.getId()%>", roleName: "<%=projectManagerRole.getName()%>"});<%}%>
      <%--if (projectManagerRole!=null){%>addAssigRow({resId: "", resName: "", roleId: "<%=projectManagerRole.getId()%>", roleName: "<%=projectManagerRole.getName()%>"});<%}--%>
      <%if (customerRole!=null){%>addAssigRow({resId: "", resName: "", roleId: "<%=customerRole.getId()%>", roleName: "<%=customerRole.getName()%>"});<%}%>
      <%if (workerRole!=null){%>addAssigRow({resId: "", resName: "", roleId: "<%=workerRole.getId()%>", roleName: "<%=workerRole.getName()%>"});<%}%>
    } else {
      <%if (workerRole!=null){%>addAssigRow({resId: "", resName: "", roleId: "<%=workerRole.getId()%>", roleName: "<%=workerRole.getName()%>"});<%}%>
      addAssigRow({resId: "", resName: "", roleId: "", roleName: ""});
    }

    //mostra carico primo utente
    datesChanged();
  });


  function addAssigRow(data){
    //console.debug("addAssigRow",data)
    var row = $.JST.createFromTemplate(data, "ASSIGNMENT_ROW");
    $("#assigRowsPlace").append(row);
    return row;
  }

  function createNewProject(el){
    //console.debug("createNewProject");

    //if name filled -> role mandatory
    $("table[assigs] .assigRow").each(function () {
      var row = $(this);
      if (row.find("input[name=resourceId]").val()) {
        row.find("input[name=roleId],input[name=roleId_txt]").attr("required", true);
      } else {
        row.find("input[name=roleId],input[name=roleId_txt]").removeAttr("required").clearErrorAlert();
      }
    });

    if (canSubmitForm("mainForm") && canSubmitForm("assigTable")) {
      var form=$("#mainForm");

      var data = {};
      form.fillJsonWithInputValues(data);

      data.CM = "NEWTASK";

      //recover assignemnts
      var assigs = [];

      $("table[assigs] .assigRow").each(function () {
        var row = $(this);
        var ass = {
          resId:  row.find("input[name=resourceId]").val(),
          roleId: row.find("input[name=roleId]").val(),
          estWl:  row.find("input[name=WL]").val()
        };
        if (ass.resId && ass.roleId)
          assigs.push(ass);
      });

      data.assigs = JSON.stringify(assigs);
      //console.debug(data);
      $.getJSON(contextPath+"/applications/teamwork/task/taskAjaxController.jsp",data,function(response){
        jsonResponseHandling(response);
        if (response.ok) {
          form.find(":input").updateOldValue(); // per non avere il messagio di leave
          //decide dove andare
         //if ("<%=addType%>"=="ADD_TASKTREE"){
         //   top.location.href = contextPath + "/applications/teamwork/task/gantt/gantt.jsp?CM=ED&OBJID=" + response.taskId;
         // } else {
            top.location.href = contextPath + "/applications/teamwork/task/taskOverview.jsp?CM=ED&OBJID=" + response.taskId;
         // }

        } else {
          showFeedbackMessage("ERROR","<%=I18n.get("ERROR_SAVING")%>");
        }
        hideSavingMessage();

      });

    }
  }


  function updateResourceLoad(el){
    var row = el.closest("tr");
    computeResourceLoad(el,getCurrentTaskPeriod(),row.find("[name=resourceId]").val(), millisFromString(row.find("[name=WL]").val(),-1));
  }

  <%------------------------------------------- ADD RESOURCE MANAGEMENT ---------------------------------------------------%>
  function createNewResource(el){
    var row = el.closest(".assigRow");
    var name=row.find("[name=resourceId_txt]").val();
    var url= contextPath + "/applications/teamwork/resource/resourceNew.jsp?CM=ADD&name="+encodeURI(name);

    openBlackPopup(url,700,320,function(response) {
      //fillare lo smart combo
      if (response && response.resId && response.resName) {
        row.find("[name=resourceId]").val(response.resId);
        row.find("[name=resourceId_txt]").val(response.resName).focus().blur();

        //se è stata creato un login si comunicano i dati
        if (response.loginCreatedMessage)
          showFeedbackMessage("INFO",response.loginCreatedMessage);

      }
    })
  }

  function assignRoleToSelected(el){
    var roleId=$("#bkRoleId").val();
    var roleTxt=$("#bkRoleId_txt").val();
    if (roleId && roleTxt) {
      $("table[assigs] tr.assigRow.selected").each(function () {
        var tr=$(this);
        //console.debug(tr.find("#roleId"))
        tr.find("#roleId").val(roleId);
        tr.find("#roleId_txt").val(roleTxt);
        tr.removeClass("selected").find(":checkbox").prop("checked",false);
      });
      refreshBulk($("#ck_chall"));
    }
  }

  function openWorkGroup() {
    //WG_IDS, WG_CAND_IDS
    var wgIds=[];
    $(":input[name=resourceId]").each(function () {
      var inp = $(this);
      if (inp.val()){
        wgIds.push(inp.val());
      }
    });
    var url = contextPath+"/applications/teamwork/workgroup/workgroupPopup.jsp?" +
      "useCallbackFunction=yes&" +
      "enableTags=yes&" +
      "title="+encodeURIComponent("<%=I18n.get("BULK_ASSIGNMENT")%>")+"&" +
      "WG_IDS="+encodeURIComponent(wgIds.join())+"&" +
      "PERM_REQUIRED=<%=TeamworkPermissions.resource_manage%>";
    openBlackPopup(url,700, 600,wgCallback);
  }

  function wgCallback(data) {
    //console.debug("wgCallback",data);
    if (data) {
      //si eliminano le righe vuote
      $(":input[name=resourceId]").each(function () {
        var inp = $(this);
        if (!inp.val()) {
          inp.closest("tr").remove();
        }
      });

      var row;
      for (var resId in data) {
        //si deve controllare se c'è già
        var found=false;
        $(":input[name=resourceId]").each(function () {
          var inp = $(this);
          if (resId == inp.val()) {
            found=true;
            return false;
          }
        });

        //se la risorsa non c'è già la aggiungo
        if (!found){
          row=addAssigRow({resId:resId,resName:data[resId]});
          row.find(":checkbox").click();
          updateResourceLoad(row.children().eq(0))
        }
      }
    }
  }



  function taskTypeAddedCallback(taskType){
    //console.debug("taskTypeAddedCallback",taskType)
    if (taskType) {
      $("#TASK_TYPE").val(taskType.id);
      $("#TASK_TYPE_txt").val(taskType.name).blur();
    }

  }


  <%---------------- OPERATOR WORK LOAD MANAGEMENT --------------------------------------%>
  var lastPeriodChecked;

  function  getCurrentTaskPeriod(){
    var ret={startMillis:Date.parseString($("#START").val()).setHours(0,0,0),endMillis:Date.parseString($("#END").val()).setHours(23,59,59)};
    lastPeriodChecked=ret;
    return ret;
  }


  function datesChanged(el) {
    //console.debug("datesChanged");
    var ret = {startMillis: Date.parseString($("#START").val()).setHours(0, 0, 0), endMillis: Date.parseString($("#END").val()).setHours(23, 59, 59)};
    if (!lastPeriodChecked || ret.startMillis != lastPeriodChecked.startMillis || ret.endMillis != lastPeriodChecked.endMillis) {
      $("tr.assigRow [name=resourceId]").each(function () {
        var inp = $(this);
        if (inp.val()) {
          updateResourceLoad(inp);
        }
      })
    }
  }


</script>

  <%


  }
  %>
