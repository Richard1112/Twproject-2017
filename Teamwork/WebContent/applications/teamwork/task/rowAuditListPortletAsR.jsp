<%@ page import="com.opnlb.website.forum.ForumEntry, 
com.twproject.task.TaskAudit,
com.twproject.task.TaskAuditReview,
org.jblooming.waf.constants.Commands,
com.twproject.operator.TeamworkOperator, com.twproject.resource.Resource, com.twproject.task.Assignment, com.twproject.task.IssueBricks, com.twproject.task.Task, com.twproject.waf.html.StatusIcon, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.display.PercentileDisplay, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List, org.jblooming.waf.html.button.ButtonJS, com.twproject.task.TaskStatus, org.jblooming.waf.html.input.ColorValueChooser, java.util.HashSet, java.util.Set, com.twproject.security.TeamworkPermissions, org.jblooming.waf.html.button.AHref" %><%


  JspHelper rowDrawer = (JspHelper) JspIncluderSupport.getCurrentInstance(request);
TaskAudit audit=  (TaskAudit)rowDrawer.parameters.get("ROW_OBJ");

Task task = audit.getTask();
  PageState pageState = PageState.getCurrentPageState(request);

String command = pageState.getCommand();
  TeamworkOperator logged= (TeamworkOperator) pageState.getLoggedOperator();
 
  ButtonLink bEdit = ButtonLink.getEditInstance("taskOverview.jsp", task, request);
%>

		<tr class="listRow">
		<td ><b><%=audit.getId().toString() %></b>
			</td>
		<td title="<%=audit.getTitle().getDescription() %>"><b>
		<a href="javascript:void(0);" onclick="editAudit($(this).closest('tr'));" ><%=audit.getTitle().getDescription().substring(0, audit.getTitle().getDescription().length()>25?25:audit.getTitle().getDescription().length()) %></a>
		</b><input type="hidden" name="auditId"
			value="<%=audit.getId() %>" /><input type="hidden" name="titleHd"
			value="<%=audit.getTitle().getIntId() %>" /><input type="hidden" name="contentHd"
			value="<%=audit.getContent() %>" /><input type="hidden" name="taskNameHd"
			value="<%=audit.getTask().getName() %>" /><input type="hidden" name="taskId"
			value="<%=audit.getTask().getId().toString() %>" /></td>
		<td title="<%=audit.getTask().getName() %>"><b>
		<a href="<%=bEdit.pageSeed.toLinkToHref()%>" class="button textual bolder taskName" ><%=audit.getTask().getName().substring(0, audit.getTask().getName().length()>25?25:audit.getTask().getName().length()) %></a></b></td>
		<td title="<%=audit.selectStatusForDisplay() %>"><b><%=audit.getAuditStatus().getDescription() %></b><input type="hidden" name="statusHd"
			value="<%=audit.getAuditStatus().getIntValue() %>" /></td>
		<td><b><%=audit.getReportor().getName() %></b><input type="hidden"
			name="reviewerIdHd" value="<%=audit.getReportor().getId() %>" /><input type="hidden"
			name="reviewerHd" value="<%=audit.getReportor().getName() %>" /></td>
		<td><b><%=audit.listReviewers() %></b></td>
		<td><b><%=DateUtilities.dateToString(audit.getCreationDate(),"yyyy-MM-dd HH:mm:ss") %></b><input type="hidden"
			name="creationHd" value="<%=DateUtilities.dateToString(audit.getCreationDate(),"yyyy-MM-dd HH:mm:ss") %>" /></td>
		<td align="center">
		<%
		if (Commands.FIND.equals(command)&&audit.getIsClosed()==1){%>
			<%if(audit.getAuditStatus().getIntValue()==2||audit.getAuditStatus().getIntValue()==0){ 
				if (task.hasPermissionFor(logged,TeamworkPermissions.task_audit_canCreate)){
			%>
				<span class="teamworkIcon edit"
				style="cursor: pointer" onclick="editAuditR($(this).closest('tr'));" title="<%=I18n.get("A_EDIT") %>">e</span>
			<%}
			}
			if(audit.getAuditStatus().getIntValue()==1){ %>
			<%}
			if(audit.getAuditStatus().getIntValue()>=3 || audit.getAuditStatus().getIntValue()==2){%>
			<span class="teamworkIcon edit"
				style="cursor: pointer" onclick="closeAudit($(this).closest('tr'));" title="<%=I18n.get("A_CLOSE") %>">x</span>
				
			<%}%>
		<%} else if (Commands.FINDMR.equals(command)&&audit.getIsClosed()==1) {
			if (task.hasPermissionFor(logged,TeamworkPermissions.task_audit_canAudit)){
			TaskAuditReview tv = audit.getCurrentReviewer(audit.getReviewers(),logged.getPerson().getId().toString(),audit.getIntId());
			%>
			<%if(tv.getAuditStatus().getIntValue()==1 && audit.getAuditStatus().getIntValue()!=2){ %><span class="teamworkIcon edit"
			style="cursor: pointer" onclick="editAudit($(this).closest('tr'));" title="<%=I18n.get("A_AUDIT") %>">a</span>
			<%}
			}
			//if (audit.getAuditStatus().getIntValue()>=3){
			//	if (task.hasPermissionFor(logged,TeamworkPermissions.task_audit_canRCreate)){
			if (audit.showReSubmitBtn(logged)){
			%>
			<span class="teamworkIcon edit"
			style="cursor: pointer" onclick="reSubmit($(this).closest('tr'));" title="<%=I18n.get("A_RESUBMIT") %>">s</span>
			<%}//}%>
		<%}%>
			<span class="teamworkIcon edit"
			style="cursor: pointer" onclick="goToHistory($(this).closest('tr'));" title="<%=I18n.get("A_HISTORY") %>">A</span>
		</td>
	</tr>
