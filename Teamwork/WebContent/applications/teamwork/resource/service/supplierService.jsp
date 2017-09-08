<%@page import="com.twproject.resource.Resource"%>
<%@page import="org.jblooming.waf.html.input.TextArea"%>
<%@page import="com.teamwork.expand.SupplierService"%>
<%@page import="com.twproject.resource.Company"%>
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
        	SupplierService t = (SupplierService) PersistenceHome.findByPrimaryKey(SupplierService.class, id);
        	//t.setResourceId(pageState.getEntry("SUPPLIER_"+id).stringValue());
        	t.setServiceTypeId(pageState.getEntry("SERVICE_TYPE_"+id).stringValue());
        	t.setServiceId(pageState.getEntry("SERVICE_CONTENT_"+id).stringValue());
        	t.setTotalNum(pageState.getEntry("TOTAL_NUM_"+id).stringValue());
        	t.setServiceDate(pageState.getEntry("SERVICE_DATE_"+id).dateValue());
        	t.setDescription(pageState.getEntry("DESCRIPTION_"+id).stringValue());
        	t.store();
        }
        Map<String, ClientEntry> newMap = pageState.getClientEntries().getEntriesStartingWithStripped("NEW_TOTAL_NUM_");
        for (String id : newMap.keySet()) {
        	SupplierService nt = new SupplierService();
        	//nt.setResourceId(pageState.getEntry("NEW_SUPPLIER_"+id).stringValue());
        	nt.setServiceTypeId(pageState.getEntry("NEW_SERVICE_TYPE_"+id).stringValue());
        	nt.setServiceId(pageState.getEntry("NEW_SERVICE_CONTENT_"+id).stringValue());
        	nt.setTotalNum(pageState.getEntry("NEW_TOTAL_NUM_"+id).stringValue());
        	nt.setServiceDate(pageState.getEntry("NEW_SERVICE_DATE_"+id).dateValue());
        	nt.setDescription(pageState.getEntry("NEW_DESCRIPTION_"+id).stringValue());
        	nt.setResourceId(pageState.getEntry("RESOURCE_ID").stringValue());
        	nt.store();
        }
       
    }
    pageState.toHtml(pageContext);

  } else {
	    // Resource company = Company.load(pageState.getEntry("RESOURCE_ID").intValueNoErrorCodeNoExc()+"");
	  //  System.out.println(pageState.getMainObjectId());
	    Resource company = Company.load(pageState.getMainObjectId());
	   // Resource company = (Resource) pageState.getMainObject();
	    PageSeed self = pageState.thisPage(request);
	    self.mainObjectId = company.getId();
	    self.addClientEntry("RESOURCE_ID",company.getId());
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
	 if (company.hasPermissionFor(logged,TeamworkPermissions.resource_canCreate)){
	  %>
	  <h2><%=I18n.get("ServiceContent")%></h2>
    <table class="table dataTable" assigs id="assigTable">
    <tr>
      <th class="tableHead"><%=I18n.get("SERVICE_TYPE")%></th>
      <th class="tableHead"><%=I18n.get("SERVICE_CONTENT")%></th>
      <th class="tableHead"><%=I18n.get("DESCRIPTION")%></th>
      <th class="tableHead"><%=I18n.get("TOTAL_NUM")%></th>
      <th class="tableHead"><%=I18n.get("SERVICE_DATE")%></th>
      <th class="tableHead" style="text-align: center"><span class="teamworkIcon edit" style="cursor: pointer" onclick="addSupplierService({});">P</span></th>
    </tr>
    <%
    String hql = "from " + SupplierService.class.getName() + " as tt where tt.resourceId='"+pageState.getEntry("RESOURCE_ID").stringValue()+"' ";
    OqlQuery oql = new OqlQuery(hql);
    List<SupplierService> tts = oql.list();
    for (SupplierService tt : tts) {
     %> <tr class="assigRow"  objid="<%=tt.getId()%>">
     <%
      SmartCombo serviceType = TaskServiceBricks.getServiceTypeCombo("SERVICE_TYPE_"+tt.getId(), pageState);
      serviceType.fieldSize = 18;
      //serviceType.initialSelectedCode="'"+tt.getServiceTypeId()+"'";
      serviceType.label="";
      serviceType.separator="";
      serviceType.required=true;
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
  
  
   <%

   
   TextArea ta = new TextArea("","DESCRIPTION_"+tt.getId(),"",20,3,"");
   ta.script = "style='width:100%;height:80px'";
   ta.maxlength=2000;
 
  // ta.readOnly = !canWrite;
   //ta.toHtml(pageContext);
   pageState.addClientEntry("DESCRIPTION_"+tt.getId(),tt.getDescription());
    %><td><%ta.toHtml(pageContext);%></td>
  
 
      <td>
      <input type="text" name="TOTAL_NUM_<%=tt.getId()%>" value="<%=tt.getTotalNum()%>" autocomplete="off"  required="true"  size="5" class="formElements validated"  entryType="DURATIONMILLIS">
      </td>
       <%
       DateField dfL = new DateField("SERVICE_DATE_"+tt.getId());
       dfL.labelstr="";
       dfL.separator="";
       dfL.required=true;
       pageState.addClientEntry("SERVICE_DATE_"+tt.getId(), tt.getServiceDate());
       //dfL.
      %><td><%dfL.toHtml(pageContext);%></td>
      
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
  //boolean canWrite = task.hasPermissionFor(logged,TeamworkPermissions.task_canWrite);
  ButtonBar buttonBar = new ButtonBar();
  ButtonSubmit save = ButtonSubmit.getSaveInstance(f, I18n.get("SAVE"));
  save.additionalCssClass="first";
//  save.enabled = canWrite;
  buttonBar.addButton(save);

 // buttonBar.loggableIdentifiableSupport = company;
  buttonBar.toHtml(pageContext);

 
%>

	  
<%
f.end(pageContext);
new DeletePreviewer("OBJ_DEL", SupplierService.class.getName(), pageState);
}
%>


<script>
$(function(){
    //$("#gantEditorTemplates").loadTemplates().remove();
     //    addTaskService({supplierId: "", resName: "", roleId: "", roleName: ""});
//    }


  });


  function addSupplierService(data){
	var index=$("#assigTable tr").length;
	 var newHref = 'partSupplierService.jsp?rowLine=' + index;

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

  function taskServiceItem(el,objId,taskId){
	    var row = el.closest(".assigRow");
	    var name="";
	    var url= contextPath + "/applications/teamwork/task/service/taskSupplierServiceItem.jsp?CM=EDIT&TASK_ID="+taskId+"&SERVICE_ID="+objId;

	    openBlackPopup(url,700,500,function(response) {
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




  
  function delRow(el) {
	    var issueRow = $(el).closest("[objid]");
	    var issueId = issueRow.attr("objid");
	    deletePreview("OBJ_DEL", issueId, function(response) {  // callback function
	      if (response && response.ok){
	        issueRow.fadeOut(500, function () {$(this).remove();});
	      }
	    });
	  }

</script>