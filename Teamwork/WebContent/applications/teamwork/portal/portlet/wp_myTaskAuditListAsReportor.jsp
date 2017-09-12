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
                 org.jblooming.waf.html.input.CheckField,
                 org.jblooming.waf.html.input.Combo,
                 org.jblooming.waf.html.display.DataTable,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.html.core.JspHelper,
                 org.jblooming.waf.html.input.LoadSaveFilter,
                 org.jblooming.waf.html.button.ButtonJS,
                 org.jblooming.utilities.JSP,
                 org.jblooming.system.SystemConstants,
                 org.jblooming.waf.settings.ApplicationState,
                 org.jblooming.waf.view.ClientEntry,
                 org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.core.JST,
                 org.jblooming.waf.html.input.SmartCombo, 
                 org.jblooming.waf.html.input.SQLCombo, 
                 org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, org.jblooming.waf.html.display.Hint, org.jblooming.agenda.Period"%><%

PageState pageState = PageState.getCurrentPageState(request);

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    String command = pageState.getCommand();
    PageSeed self = pageState.thisPage(request);
    self.setCommand(Commands.FIND);
    Form f = new Form(self);
    f.id = "AUDIT_LIST";
    f.alertOnChange = false;

    pageState.setForm(f);
    f.start(pageContext);
%>
<%

  
	DataTable dataTable= new DataTable("ADTLST",f, new JspHelper("/applications/teamwork/task/rowAuditListPortletAsR.jsp"), TaskAuditController.class,pageState );
dataTable.addHeader("ID","",null);
dataTable.addHeader(I18n.get("AUDIT_TITLE"),"",null);
	dataTable.addHeader(I18n.get("TASK"), "",null);
	dataTable.addHeader(I18n.get("AUDIT_STATUS"), "",null);
	dataTable.addHeader(I18n.get("AUDIT_REPORTER"),"",null);
	dataTable.addHeader(I18n.get("AUDIT_REVIEWER"),"",null);
	dataTable.addHeader(I18n.get("AUDIT_CREATION"), "",null);

	dataTable.addHeader("");
	dataTable.tableClass="table";


  
  String isClosed = pageState.getEntry("ISCLOSED").stringValueNullIfEmpty();
  
  ButtonJS bs = new ButtonJS();
  bs.onClickScript = "$('#myAudits').toggle()";
  bs.iconChar="g";
  bs.label="";
  bs.additionalCssClass="ruzzol";
  bs.toolTip=I18n.get("FILTER");
  
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



<div class="portletBox" id="myAuditListAsReportor">
<div style="float:right;padding-top: 5px">
  <%bs.toHtmlInTextOnlyModality(pageContext);%>
  </div>
<h1>
	<%=I18n.get("MY_AUDIT") %>
</h1>
<div id="myAudits" class="filterActiveElements portletParams" style="display:none">
<%

	CheckField showAlsoDep= new CheckField("IFCLOSED","",false);
	showAlsoDep.label=I18n.get("IFCLOSED");
	showAlsoDep.script="onchange=\"changeClose(this)\"";
    showAlsoDep.toHtmlI18n(pageContext);
    
	TextField isclosed = new TextField("ISCLOSED", "<br>");
	isclosed.type="hidden";
	isclosed.toHtml(pageContext);

	TextField reportId = new TextField("REPORTID", "<br>");
	//reportId.type="hidden";
	ClientEntry cle1= new ClientEntry("REPORTID", logged.getPerson().getId().toString());
	pageState.addClientEntry(cle1);
	reportId.setValue(cle1);
	reportId.toHtml(pageContext);
	
	//dataTable.drawPaginatorToolOnly(pageContext);
%>
</div>
<%
      //---------------------------------  INIZIO TABELLA ----------------------------------------------
        dataTable.drawTable(pageContext);
      //---------------------------------  FINE TABELLA ----------------------------------------------

      dataTable.drawPaginatorPagesOnly(pageContext);
        //new JspHelper("taskListNothingFound.jsp").toHtml(pageContext);
    %>

</div>
<%
        f.end(pageContext);
%>

<script>
  $(function(){
	  $("#ISCLOSED").val("1");
	  dataTableRefresh('ADTLST', true, 'FN');
  });

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
						dataTableRefresh('ADTLST', true, 'FN');
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
  function goToHistory(el){
	  
	var auditId = el.find("input[name=auditId]").val();
    var url= contextPath + "/applications/teamwork/task/taskAuditHisList.jsp?CM=SHOW&AUDIT_ID="+auditId;

    openBlackPopup(url,800,400,function(response) {
      if (response) {
        if (response.ok)
          showFeedbackMessage("INFO",response.loginCreatedMessage);
      }
    })
  }
  function changeClose(obj){
	  if (obj.checked){
		  $("#ISCLOSED").val("");
	  } else{
		  $("#ISCLOSED").val("1");
	  }
	  $(".portletParams").hide();
	  dataTableRefresh('ADTLST', true, 'FN');
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
							dataTableRefresh('ADTLST', true, 'FN');
						});
	  }
  function editAudit(obj){

	  var auditId = obj.find("input[name=auditId]").val();
	  var taskId = obj.find("input[name=taskId]").val();
	  var url= contextPath + "/applications/teamwork/task/taskAuditSubmit.jsp?CM=V_AUDIT&AUDIT_ID="+auditId+"&TASK_ID="+taskId;
	  openBlackPopup(url,800,550,function(response) {
		  dataTableRefresh('ADTLST', true, 'FN');
	      });
	 
  }
  function editAuditR(obj){

	  var auditId = obj.find("input[name=auditId]").val();
	  var taskId = obj.find("input[name=taskId]").val();
	  var url= contextPath + "/applications/teamwork/task/taskAuditSubmit.jsp?CM=E_AUDIT&AUDIT_ID="+auditId+"&TASK_ID="+taskId;
	  openBlackPopup(url,800,550,function(response) {
		  dataTableRefresh('ADTLST', true, 'FN');
	      });
	 
  }
</script>

