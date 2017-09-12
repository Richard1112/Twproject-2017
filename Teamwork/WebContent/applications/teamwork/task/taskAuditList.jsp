<%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8" %><%@ page import="com.twproject.operator.TeamworkOperator,
                com.twproject.resource.Person,
                 com.twproject.security.RoleTeamwork,
                 com.twproject.security.SecurityBricks,
                 com.twproject.security.TeamworkPermissions,
                 com.twproject.task.Assignment,
                 com.twproject.task.Task,
                 com.twproject.task.TaskAudit,
                 com.twproject.task.TaskAuditReview,
                 com.twproject.task.TaskAuditLog,
                 java.util.List,
                 org.jblooming.waf.settings.ApplicationState,
                 org.jblooming.system.SystemConstants,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonSupport,
                 org.jblooming.utilities.DateUtilities,
                 com.twproject.task.TaskCustomerFieldRelation,
                 com.twproject.task.TaskBricks,
                 com.twproject.task.businessLogic.TaskAuditController,
                 com.twproject.waf.TeamworkPopUpScreen,
                 net.sf.json.JSONArray, net.sf.json.JSONObject, org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.html.input.TextArea,
                 org.jblooming.waf.html.input.TextField,
                 org.jblooming.waf.html.input.Combo,
                 org.jblooming.waf.html.display.DataTable,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.html.core.JspHelper,
                 org.jblooming.waf.html.input.LoadSaveFilter,
                 org.jblooming.waf.html.button.ButtonJS,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.view.ClientEntry,
                 org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.core.JST,
                 org.jblooming.waf.html.input.SmartCombo, 
                 org.jblooming.waf.html.input.SQLCombo, 
                 org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, org.jblooming.waf.html.display.Hint, org.jblooming.agenda.Period"%><%

PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    //final ScreenArea body = new ScreenArea(new TaskController(), request);
    final ScreenArea body = new ScreenArea(new TaskAuditController(), request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {

	  
    Task task = Task.load(pageState.getEntry("TASK_ID").intValueNoErrorCodeNoExc()+"");
    if (task==null)
      return;

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    String command = pageState.getCommand();
    pageState.addClientEntry("TASKId", task.getId());
    PageSeed self = pageState.thisPage(request);
    if(Commands.AUDIT.equals(command)){
    	self.setCommand(Commands.FINDMR);
    	pageState.setCommand(Commands.AUDIT);
    } else{
    	self.setCommand(Commands.FIND);
    	pageState.setCommand(Commands.S_AUDIT);
    }
    Form f = new Form(self);
    f.id = "AUDIT_LIST";
    f.alertOnChange = false;

    pageState.setForm(f);
    f.start(pageContext);
%>
<%

  PageSeed psAddAssignment2 = pageState.pageFromRoot("task/taskAuditSubmit.jsp");
  psAddAssignment2.addClientEntry("TASK_ID",task.getId());
  psAddAssignment2.command = Commands.ADD;
  
  ButtonSupport jtp2 = ButtonLink.getBlackInstance("",550,800, psAddAssignment2,"onCloseCallBack");
  jtp2.toolTip=I18n.get("SUBMIT_AUDIT");
  jtp2.label=I18n.get("SUBMIT_AUDIT");
  jtp2.width="140px";
  jtp2.iconChar="P";
  jtp2.additionalCssClass = "small" ;
  
  DataTable dataTable= new DataTable("ADTLST",f, new JspHelper("/applications/teamwork/task/rowAuditList.jsp"), TaskAuditController.class,pageState );
  dataTable.addHeader(I18n.get("AUDIT_TITLE"),"",null);
  dataTable.addHeader(I18n.get("AUDIT_STATUS"), "",null);
  dataTable.addHeader(I18n.get("AUDIT_REPORTER"),"",null);
  dataTable.addHeader(I18n.get("AUDIT_REVIEWER"),"",null);
  dataTable.addHeader(I18n.get("AUDIT_CREATION"), "",null);

  dataTable.addHeader("");
  dataTable.tableClass="table";


  
  String isClosed = pageState.getEntry("ISCLOSED").stringValueNullIfEmpty();
  
%>


<h4>
	<%=I18n.get("AUDIT_LIST") %>
</h4>
<%if(Commands.S_AUDIT.equals(command)){
	jtp2.toHtml(pageContext);
	}

PageSeed printFreeze = new PageSeed("/applications/teamwork/task/taskAuditPrint.jsp");
printFreeze.mainObjectId = "TASK_ID0";
printFreeze.setCommand("ED");
printFreeze.addClientEntry("AUDIT_ID", "AUDIT_ID1");
printFreeze.addClientEntry("TASK_ID", "TASK_ID1");

PageSeed redirTo = pageState.pageFromRoot("task/taskOverview.jsp");
redirTo.mainObjectId = "TASK_ID2";
redirTo.setCommand("ED");
redirTo.command = "CREATE_SNAPSHOT";

ButtonLink freeze = ButtonLink.getPDFFreezeButton(printFreeze, redirTo, "audit_" + "AUDIT_ID2");
String spa = ApplicationState.getApplicationSetting(SystemConstants.FLD_REPOSITORY_URL);
freeze.enabled = org.jblooming.utilities.JSP.ex(spa);
freeze.label = I18n.get("TASK_FREEZE");
freeze.style="display:none";
freeze.id="createPdf";
freeze.toHtml(pageContext);
	%>

<div class="filterBar clearfix">
<div class="filterActiveElements">
<%
TextField taskId = new TextField("TASKID", "<br>");
taskId.type="hidden";
ClientEntry cle= new ClientEntry("TASKID", task.getId().toString());
pageState.addClientEntry(cle);
taskId.setValue(cle);
taskId.toHtml(pageContext);
if (JSP.ex(isClosed)) {
	TextField isClosedTf = new TextField("ISCLOSED", "<br>");
	isClosedTf.type="hidden";
	ClientEntry cle1= new ClientEntry("ISCLOSED", isClosed);
	pageState.addClientEntry(cle1);
	isClosedTf.setValue(cle1);
	isClosedTf.toHtml(pageContext);
}
if(Commands.S_AUDIT.equals(command)){
	TextField reportId = new TextField("REPORTID", "<br>");
	reportId.type="hidden";
	ClientEntry cle1= new ClientEntry("REPORTID", logged.getPerson().getId().toString());
	pageState.addClientEntry(cle1);
	reportId.setValue(cle1);
	reportId.toHtml(pageContext);
}
if(Commands.AUDIT.equals(command)){
	TextField reviewerId = new TextField("REVIEWERID", "<br>");
	reviewerId.type="hidden";
	ClientEntry cle1= new ClientEntry("REVIEWERID", logged.getPerson().getId().toString());
	pageState.addClientEntry(cle1);
	reviewerId.setValue(cle1);
	reviewerId.toHtml(pageContext);
}
%>
</div>


  <%

      //---------------------------------  INIZIO TABELLA ----------------------------------------------
        dataTable.drawTable(pageContext);
      //---------------------------------  FINE TABELLA ----------------------------------------------

      dataTable.drawPaginatorPagesOnly(pageContext);
        //new JspHelper("taskListNothingFound.jsp").toHtml(pageContext);
    %>
<% //if (Commands.S_AUDIT.equals(command)) {jtp2.toHtml(pageContext);} %>
</div>
<%

f.end(pageContext);
%>

<script type="text/javascript">
  $(function(){
	  //dataTableSearchClick('ADTLST');
  });
  function onCloseCallBack(){
	  <%
	  if(Commands.S_AUDIT.equals(command)){%>
	  dataTableRefresh('ADTLST', true, '<%=Commands.FIND%>');
	  <%}
	  else{ %>
	  dataTableRefresh('ADTLST', true, '<%=Commands.FINDMR%>');
	  <%}%>
  }

  function editAudit(obj){
	  <%if (Commands.S_AUDIT.equals(command)){%>
	  var auditId = obj.find("input[name=auditId]").val();
	  var url= contextPath + "/applications/teamwork/task/taskAuditSubmit.jsp?CM=E_AUDIT&AUDIT_ID="+auditId+"&TASK_ID="+<%=task.getId()%>;
	  openBlackPopup(url,800,550,function(response) {
	        onCloseCallBack();
	      });
	  <%} else if (Commands.AUDIT.equals(command)) { %>
	  var auditId = obj.find("input[name=auditId]").val();
	  var url= contextPath + "/applications/teamwork/task/taskAudit.jsp?CM=V_AUDIT&AUDIT_ID="+auditId+"&TASK_ID="+<%=task.getId()%>;
	  openBlackPopup(url,800,550,function(response) {
	        onCloseCallBack();
	      });
	  <%}%>
}
  function editAuditV(obj){
	  
	  var auditId = obj.find("input[name=auditId]").val();
	  var url= contextPath + "/applications/teamwork/task/taskAuditSubmit.jsp?CM=V_AUDIT&AUDIT_ID="+auditId+"&TASK_ID="+<%=task.getId()%>;
	  openBlackPopup(url,800,550,function(response) {
	        //onCloseCallBack();
	      });
	
  }
  function callBack(obj){
	var auditId = obj.find("input[name=auditId]").val();
	var data = {CM :"CALLBACK",auditId:auditId};
	$.getJSON(contextPath+ "/applications/teamwork/task/taskAuditAjaxController.jsp",
					data, function(response) {
						jsonResponseHandling(response);
						if (response.ok) {
							//closeBlackPopup(response);
						}
				        onCloseCallBack();
						hideSavingMessage();
					});
  }
  function closeAudit(obj){
		var auditId = obj.find("input[name=auditId]").val();
		var taskId = obj.find("input[name=taskId]").val();
		var data = {CM :"CLOSE",auditId:auditId};
		if (confirm("确定要关闭吗？")){
			$.getJSON(contextPath+ "/applications/teamwork/task/taskAuditAjaxController.jsp",
					data, function(response) {
						jsonResponseHandling(response);
						if (response.ok) {
							//closeBlackPopup(response);
						}
						onCloseCallBack();
						var hr = $("#createPdf").attr("href");
						
						for(var i=0;i<3;i++){
							hr = hr.replace("TASK_ID"+i, taskId);
							hr = hr.replace("AUDIT_ID"+i, auditId);
						}
						$("#createPdf").attr("href", hr);
						var ht = $("#createPdf").html();
						ht = "<p>"+ht+"</p>";
						$("#createPdf").html(ht);
						$("a#createPdf>p").trigger("click");
						hideSavingMessage();
			});
		}
	  }
  function reSubmit(obj){
	  var auditId = obj.find("input[name=auditId]").val();
	  var url= contextPath + "/applications/teamwork/task/taskAuditSubmit.jsp?CM=R_AUDIT&AUDIT_ID="+auditId+"&TASK_ID="+<%=task.getId()%>;
	  openBlackPopup(url,800,550,function(response) {
	        if (response) {
	        	onCloseCallBack();
	        }
	        onCloseCallBack();
	      });
	  
  }
  function goToHistory(el){
	var auditId = el.find("input[name=auditId]").val();
	var url= contextPath + "/applications/teamwork/task/taskAuditHisList.jsp?CM=SHOW&AUDIT_ID="+auditId+"&TASK_ID="+<%=task.getId()%>;
		openBlackPopup(url,800,400,function(response) {
	      if (response) {
	          showFeedbackMessage("INFO",response.loginCreatedMessage);
	      }
	    });
  }



</script>

  <%


  }
  %>
