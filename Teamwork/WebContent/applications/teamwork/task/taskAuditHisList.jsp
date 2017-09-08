<%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8" %><%@ page import="com.twproject.operator.TeamworkOperator,
                com.twproject.resource.Person,
                 com.twproject.security.RoleTeamwork,
                 com.twproject.security.SecurityBricks,
                 com.twproject.security.TeamworkPermissions,
                 com.twproject.task.Assignment,
                 com.twproject.task.Task,
                 com.twproject.task.TaskAudit,
                 com.twproject.task.TaskAuditLog,
                 java.util.List,
                 java.lang.Object,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonSupport,
                 org.jblooming.utilities.DateUtilities,
                 com.twproject.task.TaskCustomerFieldRelation,
                 com.twproject.task.TaskBricks,
                 com.twproject.waf.TeamworkPopUpScreen,
                 net.sf.json.JSONArray, net.sf.json.JSONObject, org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.html.input.TextArea,
                 org.jblooming.waf.html.input.Combo,
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


    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    String auditId=pageState.getEntry("AUDIT_ID").stringValueNullIfEmpty();
%>



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
				if (auditId != null) {
					TaskAudit audit = TaskAudit.load(auditId);
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
		<td><b><%=DateUtilities.dateToString(log.getAuditdate(), "yyyy-MM-dd HH:mm:ss")%></b></td>
		<td><b><%=log.getContent()==null?"":log.getContent()%></b></td>
		<td><b><%=log.getAuditStatus().getDescription()%></b></td>
	</tr>
	<%
		}
					}
				}
	%>
</table>

<script>
	$(function() {

	});
	function editAudit(obj) {

	}
	function goToHistory(el) {
		var url = contextPath
				+ "/applications/teamwork/task/taskAuditHis.jsp?CM=SHOW";

		openBlackPopup(url, 700, 320, function(response) {
			//fillare lo smart combo
			if (response && response.resId && response.resName) {
				if (response.loginCreatedMessage)
					showFeedbackMessage("INFO", response.loginCreatedMessage);
			}
		})
	}
</script>

  <%


  }
  %>
