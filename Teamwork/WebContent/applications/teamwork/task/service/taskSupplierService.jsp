<%@page import="com.teamwork.expand.TaskService"%>
<%@page import="org.jblooming.waf.html.core.JST"%>
<%@page import="org.jblooming.waf.constants.Fields"%>
<%@page import="org.jblooming.waf.html.input.DateField"%>
<%@page import="org.jblooming.utilities.JSP"%>
<%@page import="com.twproject.resource.Person"%>
<%@page import="com.twproject.task.Task"%>
<%@page import="com.teamwork.expand.TaskServiceBricks"%>
<%@page import="org.jblooming.waf.html.input.SmartCombo"%>
<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.security.TeamworkPermissions, com.twproject.waf.TeamworkPopUpScreen, org.jblooming.company.DepartmentType, org.jblooming.ontology.businessLogic.DeleteHelper, org.jblooming.oql.OqlQuery, org.jblooming.persistence.PersistenceHome, org.jblooming.persistence.exceptions.PersistenceException, org.jblooming.security.Area, org.jblooming.waf.ScreenArea, org.jblooming.waf.SessionState, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.ClientEntry, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List, java.util.Map, java.util.Set, org.jblooming.waf.html.button.ButtonJS" %>
<%
PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {
	  
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea( request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    if (Commands.SAVE.equals(pageState.command)) {
    	//System.out.println("SAVE OR UPDATE");
    	
    	Map<String, ClientEntry> map = pageState.getClientEntries().getEntriesStartingWithStripped("TOTAL_NUM_");
        for (String id : map.keySet()) {
        	TaskService t = (TaskService) PersistenceHome.findByPrimaryKey(TaskService.class, id);
        	t.setResourceId(pageState.getEntry("SUPPLIER_"+id).stringValue());
        	t.setServiceTypeId(pageState.getEntry("SERVICE_TYPE_"+id).stringValue());
        	t.setServiceId(pageState.getEntry("SERVICE_CONTENT_"+id).stringValue());
        	t.setTotalNum(pageState.getEntry("TOTAL_NUM_"+id).stringValue());
        	t.setServiceDate(pageState.getEntry("SERVICE_DATE_"+id).dateValue());
        	t.store();
        }
        Map<String, ClientEntry> newMap = pageState.getClientEntries().getEntriesStartingWithStripped("NEW_TOTAL_NUM_");
        for (String id : newMap.keySet()) {
        	TaskService nt = new TaskService();
        	nt.setResourceId(pageState.getEntry("NEW_SUPPLIER_"+id).stringValue());
        	nt.setServiceTypeId(pageState.getEntry("NEW_SERVICE_TYPE_"+id).stringValue());
        	nt.setServiceId(pageState.getEntry("NEW_SERVICE_CONTENT_"+id).stringValue());
        	nt.setTotalNum(pageState.getEntry("NEW_TOTAL_NUM_"+id).stringValue());
        	nt.setServiceDate(pageState.getEntry("NEW_SERVICE_DATE_"+id).dateValue());
        	nt.setTaskId(pageState.getEntry("TASK_ID").stringValue());
        	nt.store();
        }
       
    }
    pageState.toHtml(pageContext);

  } else {
	    Task task = Task.load(pageState.getEntry("TASK_ID").intValueNoErrorCodeNoExc()+"");
	    PageSeed self = pageState.thisPage(request);
	    self.mainObjectId = task.getId();
	    self.addClientEntry("TASK_ID",task.getId());
	//  String parId = pageState.getEntry(Fields.PARENT_ID).stringValueNullIfEmpty();
	//  self.addClientEntry(Fields.PARENT_ID, JSP.w(parId));
	    self.command=Commands.SAVE;
	    Form f = new Form(self);
	    f.id = "mainForm";
	    f.encType = Form.MULTIPART_FORM_DATA;
	    f.alertOnChange = true;
	    pageState.setForm(f);
	    f.start(pageContext);
	
	    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
	    Person loggedPerson = logged.getPerson();
	
// 	    String addType=pageState.getEntry("ADD_TYPE").stringValueNullIfEmpty();
// 	    addType=JSP.ex(addType)?addType:"ADD_TASK";
	 if (task.hasPermissionFor(logged,TeamworkPermissions.project_canCreate)){
	  %>
	  <h2><%=I18n.get("SupplierService")%></h2>
    <table class="table dataTable" assigs id="assigTable">
    <tr>
      <th class="tableHead"><%=I18n.get("SUPPLIER")%></th>
      <th class="tableHead"><%=I18n.get("SERVICE_TYPE")%></th>
      <th class="tableHead"><%=I18n.get("SERVICE_CONTENT")%></th>
      <th class="tableHead"><%=I18n.get("TOTAL_NUM")%></th>
      <th class="tableHead"><%=I18n.get("SERVICE_DATE")%></th>
      <th class="tableHead"><%=I18n.get("SERVICE_ITEM")%></th>
      <th class="tableHead" style="text-align: center"><span class="teamworkIcon edit" style="cursor: pointer" onclick="addTaskService({});">P</span></th>
    </tr>
    <%
    String hql = "from " + TaskService.class.getName() + " as tt where tt.taskId='"+pageState.getEntry("TASK_ID").stringValue()+"' ";
    OqlQuery oql = new OqlQuery(hql);
    List<TaskService> tts = oql.list();
    for (TaskService tt : tts) {
     %> <tr class="assigRow"  objid="<%=tt.getId()%>">
      <%
      SmartCombo supCombo = TaskServiceBricks.getSupplierCombo("SUPPLIER_"+tt.getId(), pageState, "SUPPLIER");
      supCombo.fieldSize = 18;
      supCombo.label = "";
      supCombo.linkToEntity=null;
      supCombo.separator="";
      supCombo.required=true;
      supCombo.addEntityButton=new ButtonJS(I18n.get("ADD_SUPPLIER"),"createNewResource($(this));");
      pageState.addClientEntry("SUPPLIER_"+tt.getId(), tt.getResourceId());
      %><td><%supCombo.toHtml(pageContext);%></td><%

      SmartCombo serviceType = TaskServiceBricks.getServiceTypeCombo("SERVICE_TYPE_"+tt.getId(), pageState);
      serviceType.fieldSize = 18;
      //serviceType.initialSelectedCode="'"+tt.getServiceTypeId()+"'";
      serviceType.label="";
      serviceType.required=true;
      serviceType.separator="";
      pageState.addClientEntry("SERVICE_TYPE_"+tt.getId(),tt.getServiceTypeId());
      %><td><%serviceType.toHtml(pageContext);%></td>
     <%

     SmartCombo serviceContent = TaskServiceBricks.getServiceContentCombo("SERVICE_CONTENT_"+tt.getId(), pageState);
     serviceContent.fieldSize = 18;
     serviceContent.label="";
     serviceContent.required=true;
    // serviceContent.initialSelectedCode="'"+tt.getServiceId()+"'";
     serviceContent.separator="";
     pageState.addClientEntry("SERVICE_CONTENT_"+tt.getId(),tt.getServiceId());
      %><td><%serviceContent.toHtml(pageContext);%></td>
  
      <td>
      <input type="text" name="TOTAL_NUM_<%=tt.getId()%>" autocomplete="off"  required="true" value="<%=tt.getTotalNum()%>"  size="5" class="formElements validated"  entryType="DURATIONMILLIS">
      </td>
       <%

       DateField dfL = new DateField("SERVICE_DATE_"+tt.getId());
       dfL.labelstr="";
       dfL.separator="";
       dfL.required=true;
       pageState.addClientEntry("SERVICE_DATE_"+tt.getId(), tt.getServiceDate());
       //dfL.
      %><td><%dfL.toHtml(pageContext);%></td>
      
      <td><a href="javascript:void(0)"
			style="cursor: pointer" onclick="taskServiceItem($(this),'<%=tt.getId()%>','<%=pageState.getEntry("TASK_ID")%>','<%=tt.getServiceTypeId()%>','<%=tt.getServiceId()%>')" title="<%=I18n.get("SERVICE_ITEM")%>"><%=tt.getSubTotalNum() %></span></td>
    <td align="center"><%
    ButtonJS delLink = new ButtonJS("delRow($(this))");
    delLink.iconChar = "d";
    delLink.label = "";
    delLink.additionalCssClass = "delete";
    delLink.toHtmlInTextOnlyModality(pageContext);
    %></td></tr><%

    }

%>
    </table>
    <%

  }
  boolean canWrite = task.hasPermissionFor(logged,TeamworkPermissions.task_canWrite);
  ButtonBar buttonBar = new ButtonBar();
  ButtonSubmit save = ButtonSubmit.getSaveInstance(f, I18n.get("SAVE"));
  save.additionalCssClass="first";
  save.enabled = canWrite;
  buttonBar.addButton(save);

  buttonBar.loggableIdentifiableSupport = task;
  buttonBar.toHtml(pageContext);

 
%>

	  
<%
f.end(pageContext);
new DeletePreviewer("OBJ_DEL_2", TaskService.class.getName(), pageState);
}
%>


<script>
$(function(){
    //$("#gantEditorTemplates").loadTemplates().remove();
     //    addTaskService({supplierId: "", resName: "", roleId: "", roleName: ""});
//    }


  });


  function addTaskService(data){
	var index=$("#assigTable tr").length;
	 var newHref = 'partTaskSupplierService.jsp?rowLine=' + index;

	  $.get(newHref, function (ret) {
	    //console.debug("get",ret)
	    $("table[assigs] tr:last").after(ret);
	  });
	
	
	//var data={NEW_TOTAL_NUM:"NEW_TOTAL_NUM_"+index,NEW_SUPPLIER:"NEW_SUPPLIER_"+index,NEW_SERVICE_TYPE:"NEW_SERVICE_TYPE_"+index,NEW_SERVICE_CONTENT:"NEW_SERVICE_CONTENT_"+index,NEW_SERVICE_DATE:"NEW_SERVICE_DATE_"+index};
   // $("table[assigs] tr:last").after($.JST.createFromTemplate(data,"SUPPLIER_SERVICE_ROW"))
  }
  
  
  function saveSupplierService(el){
	  $("#mainForm").submit();
  }

  

  <%------------------------------------------- ADD RESOURCE MANAGEMENT ---------------------------------------------------%>
  function createNewResource(el){
    var row = el.closest(".assigRow");
    var name="";
    var url= contextPath + "/applications/teamwork/task/service/supplierNew.jsp?CM=ADD&isCompany=true&name="+encodeURI(name);

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

  function taskServiceItem(el,objId,taskId,typeId,serviceId){
	    var row = el.closest(".assigRow");
	    var name="";
	    var url= contextPath + "/applications/teamwork/task/service/taskSupplierServiceItem.jsp?CM=EDIT&TASK_ID="+taskId+"&SERVICE_ID="+objId+"&SERVICE_TYPE_ID="+typeId+"&SERVICE_CONTENT_ID="+serviceId;

	    openBlackPopup(url,700,500,function(response) {
	    	 $("#mainForm").submit();

	    })
	  }




  
  function delRow(el) {
	    var issueRow = $(el).closest("[objid]");
	    var issueId = issueRow.attr("objid");
	    deletePreview("OBJ_DEL_2", issueId, function(response) {  // callback function
	      if (response && response.ok){
	        issueRow.fadeOut(500, function () {$(this).remove();});
	      }
	    });
	  }

</script>