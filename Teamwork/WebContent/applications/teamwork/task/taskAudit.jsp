<%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8"%><%@ page
	import="com.twproject.operator.TeamworkOperator,
                com.twproject.resource.Person,
                 com.twproject.security.RoleTeamwork,
                 com.twproject.security.SecurityBricks,
                 com.twproject.security.TeamworkPermissions,
                 com.twproject.task.Assignment,
                 com.twproject.task.Task,
                 com.twproject.task.TaskCustomerFieldRelation,
                 com.twproject.task.TaskBricks,
                 com.twproject.task.TaskAudit,
                 com.twproject.task.TaskAuditLog,
                 java.util.List,
                 org.jblooming.utilities.DateUtilities,
                 com.twproject.waf.TeamworkPopUpScreen,
                 net.sf.json.JSONArray, net.sf.json.JSONObject, org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.html.input.TextArea,
                 org.jblooming.waf.view.ClientEntry,
                 org.jblooming.waf.html.input.TextField,
                 org.jblooming.waf.html.input.Combo,
                 org.jblooming.waf.html.input.ComboBox,
                 org.jblooming.waf.html.button.ButtonJS,
                 org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.core.JST,
                 org.jblooming.waf.html.input.SmartCombo, 
                 org.jblooming.waf.html.input.SQLCombo, 
                 org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, org.jblooming.waf.html.display.Hint, org.jblooming.agenda.Period"%>
<%

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

    //if (task==null)
  //    return;
    
    
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    
    String auditId=pageState.getEntry("AUDIT_ID").stringValueNullIfEmpty();

    TaskAudit audit = TaskAudit.load(auditId);
    if (audit==null)
        return;
%><h3>
	<%=I18n.get("TASK_AUDIT") %>
</h3>
<%

  ButtonJS pass = new ButtonJS(I18n.get("PASS"),"submitAudit($(this),'PASS');");
  pass.additionalCssClass="first";
  pass.confirmRequire=false;
  //pass.enabled = task.hasPermissionFor(logged,TeamworkPermissions.assignment_canCRW);
  

  ButtonJS reject = new ButtonJS(I18n.get("REJECT"),"submitAudit($(this),'REJECT');");
  reject.additionalCssClass="second";
  reject.enabled = true;
  reject.confirmRequire=false;


  TextArea taa = new TextArea("content", "", 30, 5, null);
  taa.readOnly = true;
  taa.maxlength=2000;
  taa.label = I18n.get("AUDIT_CONTENT");
  taa.separator=":<br/>";
  ClientEntry cle= new ClientEntry("content", audit.getContent());
  pageState.addClientEntry(cle);
  taa.setValue(cle);
  
  TextArea suj = new TextArea("sujection", "", 30, 5, null);
  suj.maxlength=2000;
  suj.label = I18n.get("AUDIT_SUJECTION");
  suj.separator=":<br/>";
  
  TextField tfn = new TextField("TASK_NAME", "<br>");
  tfn.fieldSize = 25;
  tfn.readOnly = true;
  tfn.fieldClass = "formElements bold";
  tfn.label = I18n.get("AUDIT_TASKNAME");
  tfn.separator=":<br/>";
  cle= new ClientEntry("TASK_NAME", audit.getTask().getName());
  pageState.addClientEntry(cle);
  tfn.setValue(cle);
  
  TextField tfn2 = new TextField("AUDIT_REPORTER", "<br>");
  tfn2.fieldSize = 25;
  tfn2.readOnly = true;
  tfn2.fieldClass = "formElements bold";
  tfn2.label = I18n.get("AUDIT_REPORTER");
  tfn2.separator=":<br/>";
  cle= new ClientEntry("AUDIT_REPORTER", audit.getReportor().getMyself().getFullname()+" ("+ DateUtilities.dateToString(audit.getLastModified(),"yyyy-MM-dd")+")");
  pageState.addClientEntry(cle);
  tfn2.setValue(cle);
  
  
%>

<table class="table dataTable" assigs=true alertonchange=true>
	<tr>
		<td>
			<%taa.toHtmlI18n(pageContext); %> <input type="hidden" name="mainId" value="<%=auditId %>" />
		</td>
	
		<td>
			<%suj.toHtmlI18n(pageContext); %>
		</td>
	</tr>
	<tr>
		<td>
			<%tfn.toHtmlI18n(pageContext);  %>
		</td>
	
		<td>
			<%tfn2.toHtmlI18n(pageContext); %>
		</td>
	</tr>
	<tr>
		<td colspan="2">
			<%pass.toHtml(pageContext); %>&nbsp;
			<%reject.toHtml(pageContext); %>
		</td>
	</tr>
</table>
<hr>
<h4>
	<%=I18n.get("AUDIT_HIS") %>
</h4>

<table class="table dataTable" name="adlist">
	<tr>
		<th class="tableHead"><%=I18n.get("AUDIT_REPORTER")%></th>
		<th class="tableHead"><%=I18n.get("AUDIT_REVIEWER")%></th>
		<th class="tableHead"><%=I18n.get("AUDIT_DATE2")%></th>
		<th class="tableHead"><%=I18n.get("AUDIT_SUJECTION")%></th>
		<th class="tableHead"><%=I18n.get("AUDIT_STAT")%></th>
	</tr>
	<%
		List<TaskAuditLog> logList = audit.getLogs();
		if (logList != null) {
			for (TaskAuditLog log : logList) {
	%>
	<tr class="listRow">
		<td><b><%=log.getSubmituser().getName()%></b></td>
		<td><b>
		<%if (log.getAuditStatus().getIntValue()==3||log.getAuditStatus().getIntValue()==2){%>
			<%=log.getAudituser()!=null?log.getAudituser().getName():""%>
		<%} else{%>
			<%=audit.listReviewers()%>
		<%}%>
		</b></td>
		<td><b><%=DateUtilities.dateToString(log.getAuditdate(), "yyyy-MM-dd")%></b></td>
		<td><b><%=log.getContent()==null?"":log.getContent()%></b></td>
		<td><b><%=log.getAuditStatus().getDescription()%></b></td>
	</tr>
		<%
			}
		}
	%>
	</table>
<script type="text/javascript">
  $(function(){
	  muteAlertOnChange = true;
  });

  function submitAudit(el,cm){
    //console.debug("createAssignments");
    //if (canSubmitForm($("table[assigs]"))) {
      var data = {CM :cm};
			var ass = {
				auditId : $("table[assigs]").find("input[name=mainId]").val(),
				sujection : $("table[assigs]").find("textarea[name=sujection]").val()
			};

			data.ass = JSON.stringify(ass);
			//console.debug(data);
			$.getJSON(contextPath+ "/applications/teamwork/task/taskAuditAjaxController.jsp",
							data, function(response) {
								jsonResponseHandling(response);
								if (response.ok) {
									closeBlackPopup(response);
								}
								hideSavingMessage();
							});
		}
	//}
</script>

<%
	}
%>
