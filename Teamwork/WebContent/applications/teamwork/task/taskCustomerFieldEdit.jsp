<%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8" %><%@ page import="com.twproject.operator.TeamworkOperator,
                com.twproject.resource.Person,
                 com.twproject.security.RoleTeamwork,
                 com.twproject.security.SecurityBricks,
                 com.twproject.security.TeamworkPermissions,
                 com.twproject.task.Assignment,
                 com.twproject.task.Task,
                 com.twproject.task.TaskCustomerFieldRelation,
                 com.twproject.task.TaskBricks,
                 com.twproject.waf.TeamworkPopUpScreen,
                 net.sf.json.JSONArray, net.sf.json.JSONObject, org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.html.button.ButtonJS,
                 org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.core.JST,
                 org.jblooming.waf.html.input.SmartCombo, 
                 org.jblooming.waf.html.input.SQLCombo, 
                 org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, org.jblooming.waf.html.display.Hint, org.jblooming.agenda.Period"%><%

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

//---------------------------------------------------- ASSIGNMENTS ------------------------------------------------------------------------

  ButtonJS add=new ButtonJS("addNewRow({})");
  add.additionalCssClass="edit";
  add.iconChar="P";


%><h1>ADD CUSTOMER FIELD</h1>
<h3><%=task.getDisplayName()%></h3>


  <table class="table dataTable" assigs=true alertonchange=true>
    <tr>
      <th class="tableHead">Name</th>
      <th class="tableHead">Value</th>
      <th class="tableHead">Type</th>
      <th class="tableHead">Description</th>
      <th class="tableHead" style="text-align: center"><%add.toHtmlInTextOnlyModality(pageContext);%></th>
    </tr>
    </table><%

  

  ButtonBar buttonBar = new ButtonBar();

  ButtonJS save = new ButtonJS(I18n.get("SAVE"),"createAssignments($(this));");
  save.additionalCssClass="first";
  save.enabled = task.hasPermissionFor(logged,TeamworkPermissions.assignment_canCRW);
  buttonBar.addButton(save);

  //buttonBar.loggableIdentifiableSupport = task;
  buttonBar.toHtml(pageContext);


  pageState.addClientEntry("fieldId","##fieldId##");
  
  SQLCombo resCombo = task.bricks.getNoAddCombo(task,"fieldId", pageState);
  resCombo.fieldSize = 10;
  resCombo.label = "";
  resCombo.separator="";
  resCombo.onValueSelectedScript="updateResourceLoad($(this));";
  //aggiungo un bottone aggiungi risorsa se ho i diritti
  
  //resCombo.addEntityButton= new ButtonJS(I18n.get("ADD_RESOURCE"),"createNewResource($(this))");


  JSONArray assigsToAdd = new JSONArray();
  if(task!=null){
    for (TaskCustomerFieldRelation rl : task.bricks.hasCustomField(task.getId())){
    	assigsToAdd.add(rl.jsonify());
    }
  }


  Period period = new Period(task.getSchedule().getStartDate(),task.getSchedule().getEndDate());

%>

<div id="templates" style="display:none;">
<%-- ---------------------------------  ASSIGNMENT ROW ------------------------------------------------------------------------------------------------------------------- --%>
<%=JST.start("EXIT_ROW")%>
<tr class="assigRow" >
  <td><b>(#=obj.name#)</b><input type="hidden" name="fieldId" value="(#=obj.fieldId#)"/><input type="hidden" name="rlId" value="(#=obj.id#)"/></td>
  <td><input type="text" name="vl" class="formElements validated" value="(#=obj.value#)"/></td>
  <td><b>(#=obj.type#)</b></td>
  <td><b>(#=obj.description#)</b></td>
  <td align="center"><span class="teamworkIcon delAssig delete" style="cursor: pointer" onclick="$(this).closest('tr').remove();">d</span></td>
</tr>
<%=JST.end()%>

<%=JST.start("NEW_ROW")%>
<tr class="assigRow" >
  <td><%resCombo.toHtml(pageContext);%><input type="hidden" name="rlId" value=""/></td>
  <td><input type="text" name="vl" class="formElements validated" value=""/></td>
  <td><b id="type"></b></td>
  <td><b id="description"></b></td>
  <td align="center"><span class="teamworkIcon delAssig delete" style="cursor: pointer" onclick="$(this).closest('tr').remove();">d</span></td>
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
    $("table[assigs] tr:last").after($.JST.createFromTemplate(data,"EXIT_ROW"))
  }
  
  function addNewRow(data){
    $("table[assigs] tr:last").after($.JST.createFromTemplate(data,"NEW_ROW"))
  }

  function createAssignments(el){
    //console.debug("createAssignments");
    if (canSubmitForm($("table[assigs]"))) {
    	
      var data = {CM :"NEWCF",taskId:"<%=task.getId()%>"};

      //recover assignemnts
      var assigs = [];

      $(".assigRow").each(function () {
    	  
        var row = $(this);
        var ass = {
          fieldId:  row.find("input[name=fieldId]").val(),
          rlId:  row.find("input[name=rlId]").val(),
          vl: row.find("input[name=vl]").val()
        };
          assigs.push(ass);
      });

      data.assigs = JSON.stringify(assigs);
      //console.debug(data);
      $.getJSON(contextPath+"/applications/teamwork/task/cfAjaxController.jsp",data,function(response){
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
    
  }



</script>

  <%


  }
  %>
