<%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8" %><%@ page import="com.twproject.operator.TeamworkOperator,
                com.twproject.resource.Person,
                 com.twproject.security.RoleTeamwork,
                 com.twproject.security.SecurityBricks,
                 com.twproject.security.TeamworkPermissions,
                 com.twproject.task.Assignment,
                 com.twproject.task.Task,
                 com.twproject.task.TaskAudit,
                 com.twproject.task.TaskAuditReview,
                 com.twproject.task.TaskAuditSubjectBricks,
                 com.twproject.task.TaskAuditLog,
                 java.util.List,
                 org.jblooming.waf.html.display.Img,
                 org.jblooming.utilities.DateUtilities,
                 com.twproject.task.TaskCustomerFieldRelation,
                 com.twproject.task.TaskBricks,
                 org.jblooming.waf.view.ClientEntry,
                 com.twproject.resource.ResourceBricks,
                 com.twproject.waf.TeamworkPopUpScreen,
                 net.sf.json.JSONArray, net.sf.json.JSONObject, org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.html.input.TextArea,
                 org.jblooming.waf.html.input.Combo,
                 org.jblooming.waf.html.input.CheckField,
                 org.jblooming.waf.html.button.ButtonJS,
                 org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.core.JST,
                 org.jblooming.waf.html.input.SmartCombo, 
                 org.jblooming.waf.html.input.SQLCombo, 
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.settings.ApplicationState,
                 org.jblooming.waf.html.button.ButtonSupport,
                 org.jblooming.system.SystemConstants,
                 org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, org.jblooming.waf.html.display.Hint, org.jblooming.agenda.Period"%><%PageState pageState = PageState.getCurrentPageState(request);
if (!pageState.screenRunning) {
	pageState.screenRunning = true;
	//final ScreenArea body = new ScreenArea(new TaskController(), request);
	final ScreenArea body = new ScreenArea(request);
	TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
	lw.register(pageState);
	pageState.perform(request, response);
	pageState.toHtml(pageContext);
} else {

	Task task = Task.load(pageState.getEntry("TASK_ID").intValueNoErrorCodeNoExc() + "");

	if (task == null)
		return;

	TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();%><h3><%=I18n.get("SUBMIT_AUDIT") %></h3>
<%
	ButtonJS save = new ButtonJS(I18n.get("SUBMIT"), "submitAudit($(this));");
	save.additionalCssClass = "first";
	//save.enabled = task.hasPermissionFor(logged, TeamworkPermissions.assignment_canCRW);

	ButtonJS reset = new ButtonJS(I18n.get("RESET"), "reset();");
	reset.additionalCssClass = "second";
	//reset.enabled = task.hasPermissionFor(logged, TeamworkPermissions.assignment_canCRW);


	SmartCombo titleCombo = TaskAuditSubjectBricks.getSubjectCombo("title", pageState);
	titleCombo.fieldSize = 10;
	titleCombo.maxRowToFetch=10;
	titleCombo.label = I18n.get("AUDIT_TITLE");
	titleCombo.linkToEntity = null;
	titleCombo.separator = ":";
	titleCombo.required=true;
	titleCombo.searchAll=false;
	titleCombo.onValueSelectedScript = "updateSubjectLoad($(this));";
	
	TextArea taa = new TextArea("content", "", 25, 8, null);
	taa.maxlength = 2000;
	//taa.script = "style='width:100%;'";
	taa.label = I18n.get("AUDIT_CONTENT");
	taa.separator = ":<br/>";
	taa.required = true;

	SmartCombo resCombo = ResourceBricks.getResourceCombo1("reviewer", pageState, task.getId().toString());
	//resCombo.fixedParams.put("purelyMyself", logged.getPerson());
	//SmartCombo resCombo = task.bricks.getAssignableResourceCombo("reviewer", false, pageState);
	resCombo.fieldSize = 10;
	resCombo.label = I18n.get("AUDIT_REVIEWER");
	resCombo.linkToEntity = null;
	resCombo.separator = ":";
	//resCombo.required=true;
	resCombo.onValueSelectedScript = "updateResourceLoad($(this));";
	
	//if (pageState.getLoggedOperator().hasPermissionFor(TeamworkPermissions.resource_canCreate))
		//resCombo.addEntityButton = new ButtonJS(I18n.get("ADD_RESOURCE"), "createNewResource($(this))");

	Combo a = TaskBricks.getAuditTypeCombo("type", "AuditType");
	a.cvl.addChoose(pageState);
	a.label = I18n.get("AUDIT_TYPE");
	a.separator = ":";
	a.required = true;

	String cammond = pageState.getCommand();
	String auditId = pageState.getEntry("AUDIT_ID").stringValueNullIfEmpty();
	TaskAudit audit = new TaskAudit();
	if (auditId != null){
		audit = TaskAudit.load(auditId);
	}
	
	if ("R_AUDIT".equals(cammond)) {
		taa.readOnly = true;
		
		if (audit == null)
			return;
		
		ClientEntry cleTitle = new ClientEntry("title", audit.getTitle().getIntId());
		pageState.addClientEntry(cleTitle);
		//titleCombo.setValue(cleTitle);
		titleCombo.readOnly = true;
		
		ClientEntry cle = new ClientEntry("content", audit.getContent());
		pageState.addClientEntry(cle);
		taa.setValue(cle);

		ClientEntry cleType = new ClientEntry("type", audit.getType().getIntId());
		pageState.addClientEntry(cleType);
	} else if ("S_AUDIT".equals(cammond)) {
		taa.readOnly = false;
		if (audit == null)
			return;
		ClientEntry cleTitle = new ClientEntry("title", audit.getTitle().getIntId());
		
		pageState.addClientEntry(cleTitle);
		//titleCombo.setValue(cleTitle);
		//titleCombo.disabled = false;
		
		ClientEntry cle = new ClientEntry("content", audit.getContent());
		pageState.addClientEntry(cle);
		taa.setValue(cle);
		
		//ClientEntry cleRev = new ClientEntry("reviewer", audit.getReviewers().get(0).getIntId());
		//pageState.addClientEntry(cleRev);
		//titleCombo.setValue(cleTitle);
		//resCombo.disabled = true;
		
		ClientEntry cleType = new ClientEntry("type", audit.getType().getIntId());
		pageState.addClientEntry(cleType);
		//a.disabled = true;
	} else if ("V_AUDIT".equals(cammond)) {
		taa.readOnly = true;
		if (audit == null)
			return;
		ClientEntry cleTitle = new ClientEntry("title", audit.getTitle().getIntId());
		pageState.addClientEntry(cleTitle);
		//titleCombo.setValue(cleTitle);
		titleCombo.disabled = true;
		
		ClientEntry cle = new ClientEntry("content", audit.getContent());
		pageState.addClientEntry(cle);
		taa.setValue(cle);
		
		//ClientEntry cleRev = new ClientEntry("reviewer", audit.getReviewers().get(0).getIntId());
		//pageState.addClientEntry(cleRev);
		//titleCombo.setValue(cleTitle);
		resCombo.disabled = true;
		
		ClientEntry cleType = new ClientEntry("type", audit.getType().getIntId());
		pageState.addClientEntry(cleType);
		a.disabled = true;

	}
	
	PageSeed printFreeze = new PageSeed("/applications/teamwork/task/taskAuditPrint.jsp");
	printFreeze.mainObjectId = task.getId();
	printFreeze.setCommand("ED");
	printFreeze.addClientEntry("AUDIT_ID", audit.getId());
	printFreeze.addClientEntry("TASK_ID", task.getId());
	
	PageSeed redirTo = pageState.pageFromRoot("task/taskOverview.jsp");
	redirTo.mainObjectId = task.getId();
	redirTo.setCommand("ED");
	redirTo.command = "CREATE_SNAPSHOT";
	
	ButtonLink freeze = ButtonLink.getPDFFreezeButton(printFreeze, redirTo, "audit_" + audit.getId());
	String spa = ApplicationState.getApplicationSetting(SystemConstants.FLD_REPOSITORY_URL);
	freeze.enabled = org.jblooming.utilities.JSP.ex(spa);
	freeze.label = I18n.get("TASK_FREEZE");
	
%>
<div style="float:left;">
<table class="table dataTable" assigs=true alertonchange=true>
	<tr>
		<td>
			<%
			titleCombo.toHtml(pageContext);
			%> <input type="hidden" name="mainId"
			value="<%=auditId%>" />
		</td>
		<td rowspan="5">
		
		</td>
	</tr>
	<tr>
		<td>
			<%
				taa.toHtml(pageContext);
			%>
		</td>
	</tr>
	<tr>
		<td>
			<%
				resCombo.toHtml(pageContext);
			%>
			<div class="clearfix" style="min-height:25px;">
			<%
				if(audit.getReviewers()!=null && !"R_AUDIT".equals(cammond)){
				for (int i=0; i<audit.getReviewers().size();i++){
					TaskAuditReview tar = audit.getReviewers().get(i);
				%>
					<div id="res_<%=tar.getReviewer().getIntId() %>" class="workgroupElement">
				    <%=tar.getReviewer().getName() %>
				    <%if (i< audit.getReviewers().size()-1){ %>,<%} 
				    if ("S_AUDIT".equals(cammond)){
				    %>
				    <span class="teamworkIcon edit" style="cursor: pointer" onclick="deleteUser('<%=tar.getReviewer().getIntId() %>');" title="删除">x</span>
				    <%} %>
				    <input type="hidden" name="reviewers" value="<%=tar.getReviewer().getIntId() %>"/>
				    </div>
				<%}
				}
			%>
			</div>
		</td>
	</tr>
	<tr>
		<td>
			<%
				a.toHtml(pageContext);
			%>
		</td>
	</tr>
	<tr>
		<td>
		<br />
			<%if (!"V_AUDIT".equals(cammond)){ %>
			<%save.toHtml(pageContext);%> &nbsp;
			<%reset.toHtml(pageContext);%>
			<%}  if ("V_AUDIT".equals(cammond)){%>
			 &nbsp;<%freeze.toHtml(pageContext);
			 %>
			<%} %>
		</td>
	</tr>
</table>
</div>
<div style="float:right;">
<%
			Img img = audit.getTitle().bricks.getAvatarImage("");
			//img.script = "class='face " + (JSP.ex(size) ? size : "") + "'";
			img.script = "style='width:450px;height:auto;'";
			img.id = "PIC";
			img.toHtml(pageContext);
		%>
</div>


<script type="text/javascript">
	$(function() {
		muteAlertOnChange = true;
	});
	
	function updateSubjectLoad(obj){
		var row = obj.closest("tr");
	    var id = row.find("[name=title]").val();
	    var data = {CM :"GETPIC",subjectId:id};
	    $.getJSON(contextPath+"/applications/teamwork/task/taskAuditAjaxController.jsp",data,function(response){
	        jsonResponseHandling(response);
	        if (response.ok) {
	        	var imgUrl = response.imageUrl;
	        	$("#PIC").prop("src",imgUrl);
	         
	        }
	      });
	}
	function editAudit(obj) {
		$("input[name=mainId]").val(obj.find("input[name=auditId]").val());
		$("textarea[name=title]")
				.val(obj.find("input[name=titleHd]").val());
		$("textarea[name=content]")
				.val(obj.find("input[name=contentHd]").val());
		$("input[name=reviewer_txt]").val(
				obj.find("input[name=reviewerHd]").val());
		$("input[name=reviewer]").val(
				obj.find("input[name=reviewerIdHd]").val());
		$("select[name=type]").val(obj.find("input[name=typeHd]").val());
	}
	function reset() {
		$("input[name=mainId]").val("");
		$("textarea[name=title]").val("");
		$("textarea[name=content]").val("");
		$("input[name=reviewer]").val("");
		$("select[name=type]").val("");

	}

	function createPdf(el){
		var auditId =$("table[assigs]").find("input[name=mainId]").val();
		var data = {CM :"CTPDF",
				url:contextPath+"/applications/teamwork/task/taskAuditPrint.jsp?AUDIT_ID="+auditId,
				auditId:auditId
			};
		
		
		$.getJSON(contextPath+"/applications/teamwork/task/taskAuditAjaxController.jsp",data,function(response){
	        jsonResponseHandling(response);
	        if (response.ok) {
	          //$("table[assigs] :input").updateOldValue(); // per non avere il messagio di leave
	          //decide dove andare
	          //top.location.href = contextPath + "/applications/teamwork/task/taskAssignmentList.jsp?CM=FN&TASK_ID=" + response.taskId;
	         // top.location.reload(true);
	         //closeBlackPopup();
	        }
	        //hideSavingMessage();
	      });
	}

	function submitAudit(el) {
		//console.debug("createAssignments");
		if (canSubmitForm($("table[assigs]"))) {
<%if ("R_AUDIT".equals(cammond)){%>
      var data = {CM :"READC",taskId:"<%=task.getId()%>"};
<%} else{%>
var data = {CM :"NEWADC",taskId:"<%=task.getId()%>"};

<%} %>

      //recover assignemnts
      var reviewers = [];


        //assigs.push(ass);
      $("input[name=reviewers]").each(function(){
    	  var reviewer={id:$(this).val()};
    	  reviewers.push(reviewer);
      });
      
      if (reviewers.length==0){
    	  $("#reviewer_txt").addClass("formElementsError");
    	  return;
      }
      var ass = {
              title:  $("table[assigs]").find("input[name=title]").val(),
              content:  $("table[assigs]").find("textarea[name=content]").val(),
              reviewer:  $("table[assigs]").find("input[name=reviewer]").val(),
              auditType: $("table[assigs]").find("select[name=type]").val(),
              auditId: $("table[assigs]").find("input[name=mainId]").val(),
              reviewers: reviewers
            };
      data.ass = JSON.stringify(ass);
      //console.debug(data);
      $.getJSON(contextPath+"/applications/teamwork/task/taskAuditAjaxController.jsp",data,function(response){
        jsonResponseHandling(response);
        if (response.ok) {
          //$("table[assigs] :input").updateOldValue(); // per non avere il messagio di leave
          //decide dove andare
          //top.location.href = contextPath + "/applications/teamwork/task/taskAssignmentList.jsp?CM=FN&TASK_ID=" + response.taskId;
         // top.location.reload(true);
         closeBlackPopup();
        }
        //hideSavingMessage();
      });
    }
  }


  function updateResourceLoad(el){
    var row = el.closest("tr");
    var id = row.find("[name=reviewer]").val();
    if ($(".clearfix").find("div[id='res_"+id+"']").length<=0 && id!=''){
	    var em = '<div id="res_'+row.find("[name=reviewer]").val()+'" class="workgroupElement">';
	    em += row.find("[name=reviewer_txt]").val();
	    em += '<span class="teamworkIcon edit" style="cursor: pointer" onclick="deleteUser('+row.find("[name=reviewer]").val()+');" title="删除">x</span>';
	    em += '<input type="hidden" name="reviewers" value="'+row.find("[name=reviewer]").val()+'"/>';
	    em += '</div>';
	    $(".clearfix").append(em);
    }
    $("#reviewer_txt").val("");
  }
  function deleteUser(id) {
	  $("#res_"+id).remove();
  }

</script>

  <%


  }
  %>
