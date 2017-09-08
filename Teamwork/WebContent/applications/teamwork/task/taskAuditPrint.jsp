<%@ page import="com.twproject.security.TeamworkPermissions, com.twproject.task.Task, com.twproject.task.businessLogic.TaskAuditController, com.twproject.waf.TeamworkPopUpScreen,
                 com.twproject.waf.html.TaskPrintDrawer, org.jblooming.system.SystemConstants, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.display.Img, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState,
                  org.jblooming.waf.settings.I18n, org.jblooming.waf.view.ClientEntry, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState,
                  java.util.Date,com.twproject.task.TaskAudit,com.twproject.task.TaskAuditLog,java.util.List" %><%

  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new TaskAuditController(), request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);

  } else {

	String taskId=pageState.getEntry("TASK_ID").stringValueNullIfEmpty();
    Task task = Task.load(taskId);


    ClientEntry printTaskDesc = pageState.getEntryOrDefault("PRINT_TASK_DESCENDANTS", Fields.FALSE);
    boolean printDesc = printTaskDesc.checkFieldValue();

    boolean printASB = pageState.getEntryOrDefault("PRINT_TASK_ASSIGS_SUBTASKS", Fields.TRUE).checkFieldValue();
    pageState.getEntryOrDefault("PRINT_TASK_WORKLOG_DETAIL", Fields.FALSE);

    PageSeed self = pageState.thisPage(request);
    self.setCommand(Commands.EDIT);
    self.mainObjectId = pageState.mainObjectId;
    Form f = new Form(self);
    pageState.setForm(f);
    f.start(pageContext);

    Img logo = new Img(ApplicationState.getApplicationSetting(SystemConstants.PRINT_LOGO), "");
    if (task!=null && JSP.ex(task.bricks.getImageUrl())){
      logo.imageUrl=task.bricks.getImageUrl();
      logo.height="130";
    }



%>
<table border="0" width="100%" align="center" cellpadding="5" cellspacing="0" class="noprint">
  <tr>
    <td align="left" width="100%"><%logo.toHtml(pageContext);%></td>
    <td align="right"><%

      ButtonJS print = new ButtonJS("window.print();");
      print.label = "";
      print.toolTip = I18n.get("PRINT_PAGE");
      print.iconChar = "p";
      print.toHtmlInTextOnlyModality(pageContext);

    %></td>
  </tr>
</table>

<div id="printFilter" style="visibility:visible;" class="noprint">
  <%

        CheckField pas = new CheckField("PRINT_TASK_ASSIGS_SUBTASKS", "&nbsp;", false);
        pas.preserveOldValue = false;
        pas.toHtmlI18n(pageContext);

        CheckField wkt = new CheckField("PRINT_TASK_DESCENDANTS", "&nbsp;", false);
        wkt.preserveOldValue = false;
        wkt.toHtmlI18n(pageContext);

        CheckField ptwd = new CheckField("PRINT_TASK_WORKLOG_DETAIL", "&nbsp;", false);
        ptwd.preserveOldValue = false;
        ptwd.toHtmlI18n(pageContext);

        CheckField ptdia = new CheckField("PRINT_TASK_DIARY_DETAIL", "&nbsp;", false);
        ptdia.preserveOldValue = false;
        ptdia.toHtmlI18n(pageContext);

        CheckField ptl = new CheckField("PRINT_TASK_LOGS", "&nbsp;", false);
        ptl.preserveOldValue = false;
        ptl.toHtmlI18n(pageContext);

        %>&nbsp;&nbsp;&nbsp;<%
        ButtonSubmit sc = new ButtonSubmit(f);
        sc.label = I18n.get("REFRESH");
        sc.additionalCssClass="small";
        sc.toHtml(pageContext);

      %>
</div>
<%

  TaskPrintDrawer tpd = new TaskPrintDrawer(task);

  tpd.canSeeCosts=  task.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.task_cost_canRead);
  tpd.recurseOnChildren = printDesc;
  tpd.pageBreak = false;
  tpd.toHtml(pageContext);

  if (printASB) {

%>
<table border="0" width="100%" align="center" cellpadding="5" cellspacing="0" class="table dataTable">
  <tr class="totals">
    <td align="right" width="50%"><%=I18n.get("TOTAL_WORKLOG_ESTIMATED_ENTIRE_PROJECT")%>
      :&nbsp;<%=DateUtilities.getMillisInHoursMinutes(tpd.totalEstimatedWorklog)%>
    </td>
    <td align="right"><%=I18n.get("TOTAL_WORKLOG_DONE_ENTIRE_PROJECT")%>
      :&nbsp;<%=DateUtilities.getMillisInHoursMinutes(tpd.totalWorklogDone)%>
    </td>
  </tr>
</table>
<%

  }

%><br>
<%
String auditId=pageState.getEntry("AUDIT_ID").stringValueNullIfEmpty();
if (auditId!=null &&!"".equals(auditId)){
	TaskAudit audit = TaskAudit.load(auditId);

if (audit != null){
%>
<h4>
	<%=I18n.get("MY_AUDIT") %>
</h4>
<table class="table dataTable"  border="0" width="100%" align="center" cellpadding="5" cellspacing="0">
	<tr>
		<th class="tableHead"><%=I18n.get("AUDIT_TITLE") %></th>
		<th class="tableHead"><%=I18n.get("TASK") %>
		</td>
		<th class="tableHead"><%=I18n.get("AUDIT_STATUS") %></th>
		<th class="tableHead"><%=I18n.get("AUDIT_REPORTER") %></th>
		<th class="tableHead"><%=I18n.get("AUDIT_REVIEWER") %></th>
		<th class="tableHead"><%=I18n.get("AUDIT_CREATION") %></th>
	</tr>
	<tr>
		<td><%=audit.getTitle().getDescription() %></td>
		<td><%=audit.getTask().getName() %></td>
		<td><%=audit.selectStatusForDisplay() %></td>
		<td><%=audit.getReportor().getName() %></td>
		<td><%=audit.listReviewers() %></td>
		<td><%=DateUtilities.dateToString(audit.getCreationDate(),"yyyy-MM-dd HH:mm:ss") %>
		</td>
	</tr>
</table>
<h4>
	<%=I18n.get("AUDIT_HIS") %>
</h4>
<table class="table dataTable" name="adlist"  border="0" width="100%" align="center" cellpadding="5" cellspacing="0">
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
		<td><%=log.getSubmituser().getName()%></td>
		<td>
			<%if (log.getAuditStatus().getIntValue()==3||log.getAuditStatus().getIntValue()==2){%>
			<%=log.getAudituser()!=null?log.getAudituser().getName():""%> <%} else{%>
			<%=audit.listReviewers()%> <%}%>
		</td>
		<td><%=DateUtilities.dateToString(log.getAuditdate(), "yyyy-MM-dd HH:mm:ss")%></td>
		<td><%=log.getContent()==null?"":log.getContent()%></td>
		<td><%=log.getAuditStatus().getDescription()%></td>
	</tr>
	<%
		}
		
		}
	%>
</table>
<%}
}%>
<br>
<p align="right"><i><%=I18n.get("PRINTED_ON", JSP.timeStamp(new Date()))%>
</i></p><%

    f.end(pageContext);

  }
%>