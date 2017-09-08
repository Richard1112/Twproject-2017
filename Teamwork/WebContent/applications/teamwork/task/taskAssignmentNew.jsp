<%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8" %><%@ page import="com.twproject.operator.TeamworkOperator,
                com.twproject.resource.Person,
                 com.twproject.security.RoleTeamwork,
                 com.twproject.security.SecurityBricks,
                 com.twproject.security.TeamworkPermissions,
                 com.twproject.task.Assignment,
                 com.twproject.task.Task,
                 com.twproject.task.TaskBricks,
                 com.twproject.waf.TeamworkPopUpScreen,
                 net.sf.json.JSONArray, net.sf.json.JSONObject, org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.html.button.ButtonJS,
                 org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.core.JST,
                 org.jblooming.waf.html.input.SmartCombo, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, org.jblooming.waf.html.display.Hint, org.jblooming.agenda.Period, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.view.PageSeed, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonLink"%><%

PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    //final ScreenArea body = new ScreenArea(new TaskController(), request);
    final ScreenArea body = new ScreenArea( request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {

    Task task = Task.load(pageState.getEntry("TASK_ID").intValueNoErrorCodeNoExc()+"");

    if (task==null)
      return;

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    Person loggedPerson = logged.getPerson();


    if (!task.hasPermissionFor(logged,TeamworkPermissions.assignment_canCRW)){
      return;
    }


//---------------------------------------------------- ASSIGNMENTS ------------------------------------------------------------------------


  ButtonJS add=new ButtonJS("addAssigRow({})");
  add.additionalCssClass="edit";
  add.iconChar="P";


  ButtonSupport workgroup = new ButtonJS ("openWorkGroup();");
  workgroup.toolTip = I18n.get("BULK_ASSIGNMENT");
  workgroup.label = "";
  workgroup.iconChar = "r";
  workgroup.additionalCssClass="first small lreq20";




%><h1><%=I18n.get("ADD_ASSIGNMENTS")%></h1>
<h3><%=task.getDisplayName()%><span style="float: right"><%workgroup.toHtml(pageContext);%></span></h3>
<%

  pageState.addClientEntry("resourceId","##resId##");
  pageState.addClientEntry("resourceId_txt","##resName##");
  SmartCombo resCombo = task.bricks.getAssignableResourceCombo("resourceId", false, pageState);
  resCombo.fieldSize = 30;
  resCombo.label = "";
  resCombo.linkToEntity=null;
  resCombo.separator="";
  resCombo.script="style=width:100%";
  resCombo.onValueSelectedScript="updateResourceLoad($(this));";
  //aggiungo un bottone aggiungi risorsa se ho i diritti
  if (pageState.getLoggedOperator().hasPermissionFor(TeamworkPermissions.resource_canCreate))
    resCombo.addEntityButton= new ButtonJS(I18n.get("ADD_RESOURCE"),"createNewResource($(this))");

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

  Hint.HintWriter hintWriter = new Hint.HintWriter();
  hintWriter.addHint("HINT_FIRST_ASSIG_ROLE", "#roleId_txt", 400, 250,true, pageState);
  hintWriter.toHtml(pageContext);

  //si controlla se ci sono già delle assegnazioni in modo da proporre delle righe già pre-fillate
  boolean okPM=false;
  boolean okCUST=false;
  boolean okWK=false;
  boolean youAreAlready=false;

  for (Assignment ass: task.getAssignments()) {
    if (ass.getRole().equals(projectManagerRole))
      okPM = true;
    else if (ass.getRole().equals(customerRole))
      okCUST = true;
    else if (ass.getRole().equals(workerRole))
      okWK = true;

    if (ass.getResource().equals(loggedPerson))
      youAreAlready=true;
  }

  JSONArray assigsToAdd = new JSONArray();
  if(!okPM && projectManagerRole!=null){
    JSONObject a= new JSONObject();
    a.element("roleId",projectManagerRole.getId());
    a.element("roleName",projectManagerRole.getDisplayName());
    if (!youAreAlready){
      a.element("resId",loggedPerson.getId());
      a.element("resName",loggedPerson.getDisplayName());
      youAreAlready=true;
    }
    assigsToAdd.add(a);
  }
  if(!okWK && workerRole!=null){
    JSONObject a= new JSONObject();
    a.element("roleId",workerRole.getId());
    a.element("roleName",workerRole.getDisplayName());
    if (!youAreAlready){
      a.element("resId",loggedPerson.getId());
      a.element("resName",loggedPerson.getDisplayName());
      youAreAlready=true;
    }
    assigsToAdd.add(a);
  }
  if(!okCUST && customerRole!=null){
    JSONObject a= new JSONObject();
    a.element("roleId",customerRole.getId());
    a.element("roleName",customerRole.getDisplayName());
    a.element("resId","");
    a.element("resName","");
    assigsToAdd.add(a);
  }

  for (int i=assigsToAdd.size(); i<3;i++) {
    JSONObject a= new JSONObject();
    a.element("roleId","");
    a.element("roleName","");
    a.element("resId","");
    a.element("resName","");
    assigsToAdd.add(a);
  }

  Period period = new Period(task.getSchedule().getStartDate(),task.getSchedule().getEndDate());


  SmartCombo bulkRoles = SecurityBricks.getRoleComboForAssignments("bkRoleId", task, false, pageState);
  bulkRoles.fieldSize = 30;
  bulkRoles.label="";
  bulkRoles.separator="";
  bulkRoles.preserveOldValue=false;

%>


<table class="table dataTable fixHead fixFoot" assigs=true alertonchange=true>
  <thead class="dataTableHead">
  <tr>
    <th class="tableHead"><input type="checkbox" id="ck_chall" class="selector lreq20" onclick="selUnselAll($(this));" title="<%=I18n.get("SELECT_DESELECT_ALL")%>"></th>
    <th class="tableHead"><%=I18n.get("NAME")%></th>
    <th class="tableHead" id="rolesColumn"><%=I18n.get("ROLE")%></th>
    <th class="tableHead" colspan="2"><%=I18n.get("WORKLOG_ESTIMATED_SHORT")%></th>
    <th class="tableHead" style="text-align: center"><%add.toHtmlInTextOnlyModality(pageContext);%></th>
  </tr>
  </thead>
  <tbody class="dataTableBody" id="assigRowsPlace"></tbody>
  <tfoot>
  <tr><td id="bulkPlace" colspan="99"></td></tr>
  </tfoot>

</table>

<div id="bulkOp" style="display:none;">
  <table class="tableaaa" cellspacing="10"><tr><td id="bulkRowSel"></td><td><%=I18n.get("ASSIGN_THIS_ROLE")%> </td><td> <%bulkRoles.toHtml(pageContext);%></td><td> <span class="button small lreq20" onclick="assignRoleToSelected($(this));"><%=I18n.get("PROCEED")%></span></td></tr></table>
</div>
<%

  ButtonBar buttonBar = new ButtonBar();

  ButtonJS save = new ButtonJS(I18n.get("SAVE"),"createAssignments($(this));");
  save.additionalCssClass="first";
  save.enabled =  task.hasPermissionFor(logged,TeamworkPermissions.assignment_canCRW);
  buttonBar.addButton(save);

  buttonBar.toHtml(pageContext);

%>

<div id="templates" style="display:none;">
<%-- ---------------------------------  ASSIGNMENT ROW ------------------------------------------------------------------------------------------------------------------- --%>
<%=JST.start("ASSIGNMENT_ROW")%>
<tr class="assigRow" >
  <td><input type="checkbox" class="selector lreq20" onclick="refreshBulk($(this));"></td>
  <td><%resCombo.toHtml(pageContext);%></td>
  <td><%roles.toHtml(pageContext);%></td>
  <td><input type="text" name="WL"  size="5" class="formElements validated durationmillis" onchange="updateResourceLoad($(this))" entryType="DURATIONMILLIS" style="width: 100%"></td>
  <td class="workloadInfo" align="center"></td>
  <td align="center"><span class="teamworkIcon delAssig delete" style="cursor: pointer" onclick="$(this).closest('tr').remove();refreshBulk($('#ck_chall'));">d</span></td>
</tr>
<%=JST.end()%>
</div>

<jsp:include page="plan/workloadUtilities.js.jsp"></jsp:include>

<script>
  $(function(){
    $("#templates").loadTemplates().remove();
    var assigsToAdd=<%=assigsToAdd%>;
    for (var i=0;i<assigsToAdd.length;i++){
      addAssigRow(assigsToAdd[i]);
    }
  });

  function addAssigRow(data){
    //console.debug("addAssigRow",data)
    var row = $.JST.createFromTemplate(data, "ASSIGNMENT_ROW");
    $("#assigRowsPlace").append(row);
    return row;
  }

  function createAssignments(el){
    //console.debug("createAssignments");
    if (canSubmitForm($("table[assigs]"))) {
      var data = {CM :"NEWASSIGS",taskId:"<%=task.getId()%>"};

      //recover assignemnts
      var assigs = [];

      $(".assigRow").each(function () {
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
          $("table[assigs] :input").updateOldValue(); // per non avere il messagio di leave
          //decide dove andare
          //top.location.href = contextPath + "/applications/teamwork/task/taskAssignmentList.jsp?CM=FN&TASK_ID=" + response.taskId;
          top.location.reload(true);
        }
        hideSavingMessage();
      });
    }
  }


  function updateResourceLoad(el){
    var row = el.closest("tr");
    computeResourceLoad(el,<%=period.jsonify()%>,row.find("[name=resourceId]").val(), millisFromString(row.find("[name=WL]").val()),-1);
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
    var url = "../workgroup/workgroupPopup.jsp?" +
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

</script>
  <%
  }
  %>
