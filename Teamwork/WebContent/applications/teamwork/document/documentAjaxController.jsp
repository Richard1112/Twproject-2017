<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.resource.Resource, com.twproject.security.TeamworkPermissions, com.twproject.task.Task, net.sf.json.JSONObject, org.jblooming.designer.DesignerData, org.jblooming.security.Securable, org.jblooming.security.SecurityException, org.jblooming.waf.JSONHelper, org.jblooming.waf.view.PageState, org.jblooming.ontology.Identifiable, com.twproject.document.businessLogic.DocumentAction, com.twproject.resource.Person, org.jblooming.ontology.businessLogic.DeleteHelper" %>
<%


  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  JSONHelper jsonHelper = new JSONHelper();
  JSONObject json = jsonHelper.json;

  try {
    Resource loggedRes = logged.getPerson();

    // --------------------------- EMPTY a CUSTOM FORM --------------------------------
    if ("EMPCF".equals(pageState.command)) {
      DesignerData designerData = DesignerData.load(pageState.getEntry("desDatId").intValueNoErrorCodeNoExc() + "");
      if (designerData != null) {
        Securable sec = null;
        if (Task.class.getName().equals(designerData.getReferenceClassName())) {
          sec = Task.load(designerData.getReferenceId());
        } else if (Person.class.getName().equals(designerData.getReferenceClassName())) {
          sec = Person.load(designerData.getReferenceId());
        } else if (Resource.class.getName().equals(designerData.getReferenceClassName())) {
          sec = Resource.load(designerData.getReferenceId());
        }
        if (sec != null ) {
          SecurityException.riseExceptionIfNoPermission(sec.hasPermissionFor(logged, TeamworkPermissions.document_canWrite),TeamworkPermissions.document_canWrite,(Identifiable)sec);
          //designerData.remove();
          DeleteHelper.cmdDelete(designerData,pageState);
        }

      }

      // --------------------------- BULK MOVE TO TASK --------------------------------
    } else if ("BULK_MOVE_TO_TASK".equals(pageState.command)) {
      new DocumentAction(pageState).cmdBulkMoveToTask();

      // --------------------------- BULK MOVE TO RESOURCE --------------------------------
    } else if ("BULK_MOVE_TO_RES".equals(pageState.command)) {
      new DocumentAction(pageState).cmdBulkMoveToResource();

     // --------------------------- BULK MOVE ADD TAGS --------------------------------
    } else if ("BULK_ADD_TAGS".equals(pageState.command)) {
      new DocumentAction(pageState).cmdBulkAddTags();

    }


      } catch (Throwable t) {
    jsonHelper.error(t);
  }

  jsonHelper.close(pageContext);

%>