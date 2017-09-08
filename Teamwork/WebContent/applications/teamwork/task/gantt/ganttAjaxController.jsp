<%@ page
  import="com.twproject.operator.TeamworkOperator, com.twproject.security.RoleTeamwork, com.twproject.security.TeamworkPermissions, com.twproject.task.*, com.twproject.task.businessLogic.AssignmentAction, com.twproject.task.businessLogic.TaskAction, net.sf.json.JSONArray, net.sf.json.JSONObject, org.jblooming.agenda.CompanyCalendar, org.jblooming.messaging.SomethingHappened, org.jblooming.ontology.Pair, org.jblooming.ontology.businessLogic.DeleteHelper, org.jblooming.persistence.PersistenceHome, org.jblooming.persistence.hibernate.PersistenceContext, org.jblooming.security.Area, org.jblooming.utilities.HashTable, org.jblooming.utilities.JSP, org.jblooming.utilities.StringUtilities, org.jblooming.waf.JSONHelper, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.constants.ObjectEditorConstants, org.jblooming.waf.html.input.SmartCombo, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.ClientEntries, org.jblooming.waf.view.PageState, java.io.Serializable, java.util.*, org.jblooming.tracer.Tracer, com.twproject.task.businessLogic.TaskGanttAction, com.twproject.resource.businessLogic.ResourceAction, org.jblooming.waf.view.RestState, com.twproject.resource.Person, com.twproject.resource.Company, org.jblooming.ontology.Identifiable, com.twproject.resource.Resource" %>
<%!

%>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();


  JSONHelper jsonHelper = new JSONHelper();
  JSONObject json = jsonHelper.json;
  try {


    // ------------------------------------------------ NEW PROJECT -------------------------------------------------------------
    if ("NEWPROJECT".equals(pageState.command)) {
      new TaskGanttAction(pageState).cmdNewProject(pageState, json);

      // ------------------------------------------------ LOAD PROJECT -------------------------------------------------------------
    } else if ("LOADPROJECT".equals(pageState.command)) {
      String taskId = pageState.getEntry("taskId").stringValueNullIfEmpty() + "";
      new TaskGanttAction(pageState).cmdLoadProject(taskId, pageState, json, false);


      // ------------------------------------------------ UPDATE TASK CODE -------------------------------------------------------------
    } else if ("UPDTSKCODE".equals(pageState.command)) {
      Task task = Task.load(pageState.getEntry("taskId").intValueNoErrorCodeNoExc() + "");
      if (task != null && task.hasPermissionFor(logged, TeamworkPermissions.task_canWrite)) {
        //check if the new code is unique

        String newCode = pageState.getEntry("code").stringValueNullIfEmpty();
        task.setCode(newCode);
        // test code uniqueness
        if (Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("USEUNIQUECODES")) && JSP.ex(task.getCode()) && !task.isUnique("code")) {
          pageState.getEntry("code").errorCode = pageState.getI18n("KEY_MUST_BE_UNIQUE");
        } else {
          task.store(); //completely unusefull as is always stored by dirtiness 
        }
      }

      // ------------------------------------------------ UPDATE TASK NAME -------------------------------------------------------------
    } else if ("UPDTSKNAME".equals(pageState.command)) {
      Task task = Task.load(pageState.getEntry("taskId").intValueNoErrorCodeNoExc() + "");
      if (task != null && task.hasPermissionFor(logged, TeamworkPermissions.task_canWrite)) {
        String newName = pageState.getEntryAndSetRequired("name").stringValue();
        task.setName(newName);
        task.store();
      }

        // ------------------------------------------------ SAVE PROJECT -------------------------------------------------------------
    } else if ("SVPROJECT".equals(pageState.command)) {
      new TaskGanttAction(pageState).cmdSaveGantt(json, false);
    } else if ( "SVTASKLIST".equals(pageState.command)) {
      new TaskGanttAction(pageState).cmdSaveTaskList(json);

      // ------------------------------------------------ IMPORT -------------------------------------------------------------
    } else if ("IMPORTPROJECT".equals(pageState.command)) { // da export singolo progetto
      new TaskGanttAction(pageState).cmdSaveGantt(json, true);
    } else if ("IMPORTPROJECTLIST".equals(pageState.command)) { // da export task list. Ci sono n task che si mettono tutti sotto un unico prj
      new TaskGanttAction(pageState).cmdImportProjectList(json);

    } else if ( "IMPORTMULTIPROJECTS".equals(pageState.command)) { //da gantt service
      new TaskGanttAction(pageState).cmdSaveMultiGantt(json, true);

    }

  } catch (Throwable t) {
    jsonHelper.error(t);
  }

  jsonHelper.close(pageContext);


%>