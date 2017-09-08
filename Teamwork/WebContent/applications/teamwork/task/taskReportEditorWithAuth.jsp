<%@page import="org.jblooming.waf.constants.OperatorConstants"%>
<%@page import="org.jblooming.waf.html.input.Uploader" pageEncoding="UTF-8"%>
<%@page import="org.jblooming.waf.html.button.ButtonSubmit"%>
<%@page import="com.twproject.operator.TeamworkOperator"%>
<%@page import="org.jblooming.waf.html.container.ButtonBar"%>
<%@page import="org.jblooming.waf.settings.I18n"%>
<%@page import="com.teamwork.expand.TaskServiceBricks"%>
<%@page import="org.jblooming.waf.html.input.TinyMCE"%>
<%@page import="org.jblooming.waf.html.button.ButtonJS"%>
<%@page import="com.teamwork.expand.TaskReportBricks"%>
<%@page import="org.jblooming.waf.html.input.SmartCombo"%>
<%@page import="com.teamwork.expand.ReportController"%>
<%@page import="org.jblooming.waf.html.input.TextField"%>
<%@ page import="com.twproject.document.businessLogic.DocumentController, com.twproject.task.Task, com.twproject.waf.TeamworkPopUpScreen, org.jblooming.waf.ScreenArea,
org.jblooming.waf.constants.Commands, org.jblooming.waf.html.state.Form, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, com.twproject.document.TeamworkDocument" %><%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new ReportController(), request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {
    //this is set by action
    Task task = (Task) pageState.attributes.get("REFERRAL_OBJECT");
    PageSeed ps = pageState.thisPage(request);
    ps.command = "AUTH";
    ps.addClientEntry("TASK_ID", task.getId());
    ps.mainObjectId = pageState.getMainObject().getId();
    Form form = new Form(ps);
    form.alertOnChange = true;
    form.encType = Form.MULTIPART_FORM_DATA;
    form.start(pageContext);
    pageState.setForm(form);
%>
<h1> <%=I18n.get("REPORT") %></h1>
<div class="mainColumn">
<table class="table" border="0" cellpadding="5">
  <tr>
  <td>
    <%
    SmartCombo reportTypeCombo = TaskReportBricks.getReportTypeCombo("REPORT_TYPE_ID", pageState);
    reportTypeCombo.fieldSize = 18;
    reportTypeCombo.label = I18n.get("REPORT_TYPE");
    reportTypeCombo.linkToEntity=null;
    reportTypeCombo.separator="";
    reportTypeCombo.required=true;
    reportTypeCombo.onValueSelectedScript="reportTypeChangeSelected()";
    reportTypeCombo.toHtml(pageContext);
    %>
  </td>
  <td>
   <%
   SmartCombo supCombo = TaskServiceBricks.getDepartCombo("REPORT_DEPART_ID", pageState, "DEPARTMENT");
   supCombo.fieldSize = 18;
   supCombo.label =  I18n.get("Report Department");
   supCombo.linkToEntity=null;
   supCombo.separator="";
   //supCombo.required=true;
   supCombo.addEntityButton=new ButtonJS(I18n.get("ADD_DEPARTMENT"),"createNewResource($(this));");
   supCombo.toHtml(pageContext);
   %>
 </td>
</tr>

<tr>
<td>
<input type="hidden" value="/<%=task.getCode()%>/" id="DOCUMENT_PATH_hidden_ROOT" />
 <input type="hidden" value="<%=pageState.getEntry("DOCUMENT_PATH").stringValueNullIfEmpty() %>" id="DOCUMENT_PATH_hidden" name="DOCUMENT_PATH"/>
 <%=I18n.get("DOCUMENT_PATH") %>:<span id="span_show_id"><%=pageState.getEntry("DOCUMENT_PATH").stringValueNullIfEmpty() %></span>
 <span class="button small" style="display: none;" id="ADD_FOLDER_id" onclick="toggleAddFolder($(this));"><%=I18n.get("ADD_FOLDER")%></span>
 <span id="mkdirArea" style="display: none;">
  <%
       TextField mdf = new TextField("DIR_NAME", "&nbsp;");
       //mdf.label = I18n.get("NAME");
       mdf.label = "";
       mdf.id="DIR_NAME_ID";
       mdf.fieldSize = 15;
       mdf.script="style=' margin: 0 5px'";
       mdf.toHtml(pageContext);

//        ButtonSubmit md = new ButtonSubmit(pageState.getForm());
//        md.variationsFromForm.setCommand("MKDIR");
//        md.label = I18n.get("CREATE");
//        md.additionalCssClass = "small";
//        md.alertOnChange=false;
//        md.toHtml(pageContext);
  %>
  <input type="button"   class="button small" value="<%=I18n.get("CREATE")%>" onclick="toggleCreateFolder($(this));">
  </span>
</td>
<td>
<% 
        Uploader  uploader = new Uploader("DOCUMENT_UPLOAD", pageState);
       // uploader.label = I18n.get("DOCUMENT_UPLOAD");
        uploader.separator = "";
        uploader.size = 60;
        uploader.treatAsAttachment = false;
     //   uploader.readOnly = !canWrite;
        uploader.required = false;
        uploader.toolTip = I18n.get("DOCUMENT_UPLOAD");
        uploader.toHtml(pageContext);
        %>
</td>
</tr>
<tr>
	<td>
	 <%  
	 TextField tf = new TextField("PASSWORD","",OperatorConstants.FLD_PWD,"",20,false);
     tf.innerLabel=I18n.get(OperatorConstants.FLD_PWD);
     tf.fieldClass="formElements formElementsBig light";
     tf.tabIndex=2;
     tf.required=true;
     tf.toHtml(pageContext);
     %>
	
	</td>
</tr>

 </table>
 <%

  ButtonBar buttonBar = new ButtonBar();

  TeamworkOperator loggedOperator = (TeamworkOperator) pageState.getLoggedOperator();
  //boolean canWrite = documentable.hasPermissionFor(loggedOperator, TeamworkPermissions.document_canWrite);

  ButtonSubmit save = ButtonSubmit.getSaveInstance(pageState.getForm(), I18n.get("SAVE_REPORT"),false);
  save.variationsFromForm.setCommand("AUTH");
  save.additionalCssClass="first big";
 // save.enabled = document.getParent()==null && document.isEnabled(loggedOperator) && canWrite;
  buttonBar.addToRight(save);

//   if (!document.isNew() ) {

//     if (document.getParent()==null && isUpload) {
//       ButtonSubmit addVersion = new ButtonSubmit(pageState.getForm());
//       addVersion.variationsFromForm.setCommand("ADD_VERSION");
//       addVersion.label = I18n.get("ADD_VERSION");
//       addVersion.enabled = document.isEnabled(loggedOperator) && documentable.hasPermissionFor(loggedOperator, TeamworkPermissions.document_canCreate);
//       addVersion.additionalCssClass="big";
//       buttonBar.addToRight(addVersion);
//     }

//     new DeletePreviewer("DOC_DEL",ReportController.class, pageState);

//     ButtonJS delB = new ButtonJS(I18n.get("DOCUMENT_DELETE"),"deleteDocumentPreview("+document.getId()+");");
//     delB.enabled = documentable.hasPermissionFor(loggedOperator, TeamworkPermissions.document_canDelete);
//     delB.additionalCssClass="big delete";


//     buttonBar.addToRight(delB);


//     buttonBar.loggableIdentifiableSupport=document;
//   }

  buttonBar.toHtml(pageContext);

//  pageState.setMainObject(documentable);

%>
</div>
 <%
    form.end(pageContext);
   }
 %>
<script type="text/javascript">
$(function(){
    var closeBl=<%="AUTH".equals(pageState.command)%>;
    var isValid=<%=pageState.validEntries()%>;
    if (isValid && closeBl) {
      window.parent.$("body").oneTime(100, "doclrl", function () { window.parent.location.reload() });
    }
  });
  
  
<%------------------------------------------- ADD RESOURCE MANAGEMENT ---------------------------------------------------%>
function createNewResource(el){
  var row = el.closest(".assigRow");
  var name="";
  var url= contextPath + "/applications/teamwork/task/report/departmentNew.jsp?CM=ADD&isCompany=true&name="+encodeURI(name);

  openBlackPopup(url,700,320,function(response) {
    //fillare lo smart combo
    if (response && response.resId && response.resName) {
      row.find("[name=resourceId]").val(response.resId);
      row.find("[name=resourceId_txt]").val(response.resName).focus().blur();

      //se Ã¨ stata creato un login si comunicano i dati
      if (response.loginCreatedMessage)
        showFeedbackMessage("INFO",response.loginCreatedMessage);

    }
  })
}



function toggleAddFolder(el){
    var mkda=$('#mkdirArea');
    if (mkda.is(":visible")){
      mkda.hide();
      el.html("<%=I18n.get("ADD_FOLDER")%>");
    } else {
      mkda.show();
      mkda.find(":input").focus();
      el.html("<%=I18n.get("RESET")%>");
    }
  }
  
function reportTypeChangeSelected(){
	 var valText=$("#REPORT_TYPE_ID_txt").val();
	 if(valText!=null&&valText.length>0){
		 var now = new Date(); 
		 if(valText.indexOf("MONTH")!=-1||valText=="月报"){
			 var nowMonth = now.getMonth();
			 var nowYear = now.getFullYear(); //当前年 
			 var days=now.getDate();
			 //var weekd=getWeekNumber(nowYear,nowMonth,days);
			 $("#DOCUMENT_PATH_hidden").val($("#DOCUMENT_PATH_hidden_ROOT").val()+valText+"/"+nowYear+"/"+nowMonth+"月"+"/");
			  $("#span_show_id").text($("#DOCUMENT_PATH_hidden").val());
			  $("#ADD_FOLDER_id").hide();
		 }else if(valText.indexOf("WEEK")!=-1||valText=="周报"){
			 var nowMonth = now.getMonth();
			 var nowYear = now.getFullYear(); //当前年 
			 var days=now.getDate();
			 var weekd=getWeekNumber();
			 $("#DOCUMENT_PATH_hidden").val($("#DOCUMENT_PATH_hidden_ROOT").val()+valText+"/"+nowYear+"/第"+weekd+"周"+"/");
			 $("#span_show_id").text($("#DOCUMENT_PATH_hidden").val());
			 $("#ADD_FOLDER_id").hide();
		 }else{
			 var nowMonth = now.getMonth();
			 var nowYear = now.getFullYear(); //当前年 
			 $("#DOCUMENT_PATH_hidden").val($("#DOCUMENT_PATH_hidden_ROOT").val()+valText+"/");
			 $("#span_show_id").text($("#DOCUMENT_PATH_hidden").val());
			 $("#ADD_FOLDER_id").show();
		 }
	 }
}

function getWeekNumber(){
	 var totalDays = 0;
	    now = new Date();
	    years = now.getYear()
	    if (years < 1000)
	        years += 1900
	    var days = new Array(12);
	    days[0] = 31;
	    days[2] = 31;
	    days[3] = 30;
	    days[4] = 31;
	    days[5] = 30;
	    days[6] = 31;
	    days[7] = 31;
	    days[8] = 30;
	    days[9] = 31;
	    days[10] = 30;
	    days[11] = 31;
	     
	    //判断是否为闰年，针对2月的天数进行计算
	    if (Math.round(now.getYear() / 4) == now.getYear() / 4) {
	        days[1] = 29
	    } else {
	        days[1] = 28
	    }
	 
	    if (now.getMonth() == 0) {
	        totalDays = totalDays + now.getDate();
	    } else {
	        var curMonth = now.getMonth();
	        for (var count = 1; count <= curMonth; count++) {
	            totalDays = totalDays + days[count - 1];
	        }
	        totalDays = totalDays + now.getDate();
	    }
	    //得到第几周
	    var week = Math.round(totalDays / 7);
	    return week;
}
  
function toggleCreateFolder(el){
	$("#DOCUMENT_PATH_hidden").val($("#DOCUMENT_PATH_hidden").val()+$("#DIR_NAME_ID").val()+"/");
    $("#span_show_id").text($("#DOCUMENT_PATH_hidden").val());
    $("#DIR_NAME_ID").val("");
    var mkda=$('#mkdirArea');
    mkda.hide();
    $("#ADD_FOLDER_id").html("<%=I18n.get("ADD_FOLDER")%>");
  }
</script>

