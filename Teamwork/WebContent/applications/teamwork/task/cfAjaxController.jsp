<%@ page import="java.util.ArrayList,com.twproject.task.TaskCustomerFieldRelation,com.twproject.task.TaskCustomerField,com.twproject.document.TeamworkDocument, com.twproject.document.businessLogic.DocumentAction, com.twproject.operator.TeamworkOperator, com.twproject.resource.Resource, com.twproject.resource.ResourceBricks, com.twproject.security.RoleTeamwork, com.twproject.security.TeamworkPermissions, com.twproject.task.Assignment, com.twproject.task.Task, com.twproject.task.businessLogic.AssignmentAction, com.twproject.task.businessLogic.TaskAction, com.twproject.task.financial.Cost, com.twproject.task.financial.CostClassification, net.sf.json.JSONArray, net.sf.json.JSONObject, org.jblooming.agenda.Period, org.jblooming.designer.DesignerField, org.jblooming.ontology.PersistentFile, org.jblooming.ontology.SerializedMap, org.jblooming.persistence.PersistenceHome, org.jblooming.utilities.JSP, org.jblooming.utilities.NumberUtilities, org.jblooming.waf.ActionUtilities, org.jblooming.waf.JSONHelper, org.jblooming.waf.html.input.Uploader, org.jblooming.waf.view.ClientEntry, org.jblooming.waf.view.PageState, org.jblooming.waf.view.RestState, java.util.Date, com.twproject.task.TaskDataHistory, java.util.List, org.jblooming.waf.settings.I18n, java.util.HashSet, java.util.Set, com.twproject.task.TaskBricks, org.jblooming.oql.QueryHelper, org.jblooming.agenda.CompanyCalendar, org.jblooming.messaging.MessagingSystem, org.jblooming.waf.constants.Fields, org.jblooming.tracer.Tracer, org.jblooming.waf.constants.OperatorConstants, net.wimpi.pim.util.Base64, java.io.InputStream, java.io.ByteArrayInputStream, org.jblooming.utilities.StringUtilities, java.io.File, org.jblooming.waf.settings.ApplicationState, java.io.FileOutputStream, org.jblooming.utilities.file.FileUtilities" %>
<%


  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  JSONHelper jsonHelper = new JSONHelper();
  JSONObject json = jsonHelper.json;

  try {
    Resource loggedRes = logged.getPerson();

    // --------------------------- CREATE NEW TASK and/or CREATE NEW ASSIGNMENTS --------------------------------
    TaskAction taskAction = new TaskAction(pageState);
      // ---------------------------------------- customer field  ----------------------------------------
    if ("NEWCF".equals(pageState.command)) {
        pageState.initializeEntries("cell");
        Task task = Task.load(pageState.getEntry("taskId").intValueNoErrorCodeNoExc() + "");
        if (task != null) {
        	//delete data
        	JSONArray assigs = JSONArray.fromObject(pageState.getEntry("assigs").stringValueNullIfEmpty());
        	List<Integer> rlids = new ArrayList<Integer>();
        	for (Object o : assigs) {
        		JSONObject ass = (JSONObject) o;
        		 if(!"".equals(ass.getString("rlId"))){
        		rlids.add(Integer.parseInt(ass.getString("rlId")));
        		 }
        	}
        	TaskCustomerFieldRelation.deleteAll(task.getId(), rlids);
        	
        	

            for (Object o : assigs) {
              JSONObject ass = (JSONObject) o;
        	  TaskCustomerField fd = TaskCustomerField.load(ass.getString("fieldId"));
        	  
          	
          	String rlId = ass.getString("rlId");
          	 TaskCustomerFieldRelation fr = null;
        	  if(!"".equals(rlId)){
        		fr = TaskCustomerFieldRelation.load(rlId);
        		fr.setValue(ass.getString("vl"));
        	  }
        	 
        	  if (fr==null){
        		  fr = new TaskCustomerFieldRelation(task, fd, ass.getString("vl"));
        		  fr.setIdAsNew();
        	  }
        	
        	  
        	  fr.store();
            }
        }

        // --------------------------- save customer field value --------------------------------
      } else if ("SVCF".equals(pageState.command)) {
          pageState.initializeEntries("cell");
          	JSONArray assigs = JSONArray.fromObject(pageState.getEntry("assigs").stringValueNullIfEmpty());
          	for (Object o : assigs) {
                JSONObject ass = (JSONObject) o;
          	  
            	//save data
            	String rlId = ass.getString("rlId");
            	 TaskCustomerFieldRelation fr = null;
          	  if(!"".equals(rlId)){
          		fr = TaskCustomerFieldRelation.load(rlId);
          		fr.setValue(ass.getString("vl"));
          	  }
          	 
          	  
          	  fr.store();
          	}
          // --------------------------- CHANGE TASK STATUS --------------------------------
        } else if ("CHTSKSTS".equals(pageState.command)) {
      Set<Task> tasks = taskAction.cmdChangeStatus();
      json.element("ok", tasks.size()>0);

    }

  } catch (Throwable t) {
    jsonHelper.error(t);
  }

  jsonHelper.close(pageContext);

%>
