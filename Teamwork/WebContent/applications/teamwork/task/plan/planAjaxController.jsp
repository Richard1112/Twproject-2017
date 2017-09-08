<%@ page import="com.twproject.plan.businessLogic.PlanControllerAction,
                 org.jblooming.waf.JSONHelper,
                 org.jblooming.waf.view.PageState, com.twproject.resource.Resource, com.twproject.plan.PlanUtilities, org.jblooming.agenda.Period, net.sf.json.JSONObject, org.jblooming.security.License, com.twproject.resource.Person" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  JSONHelper jsonHelper = new JSONHelper();
  JSONObject json = jsonHelper.json;
  try {
    
    if ("SV_CELL".equals(pageState.command)) {
      PlanControllerAction.ajaxSaveCell(jsonHelper.json, pageState);

    } else if ("UPD_EST".equals(pageState.command)) {
      PlanControllerAction.updateEstimation(jsonHelper.json, pageState);

    } else if ("RECONCILE".equals(pageState.command)) {
      PlanControllerAction.reconcilePlan(jsonHelper.json, pageState);

    } else if ("FILLPLAN0".equals(pageState.command)) {
      PlanControllerAction.fillPlanWith0(jsonHelper.json, pageState);

    } else if ("REFRESHASS".equals(pageState.command)) {
      PlanControllerAction.refreshAss(jsonHelper.json, pageState);


    // get resource load for dates
    } else if ("GETOPLOAD".equals(pageState.command)) {
      Resource res = Resource.load(pageState.getEntry("resId").intValueNoErrorCodeNoExc()+"");
      long startMillis=pageState.getEntry("startMillis").longValueNoErrorNoCatchedExc();
      long endMillis=pageState.getEntry("endMillis").longValueNoErrorNoCatchedExc();
      long simulatedLoad=pageState.getEntry("simulatedLoad").longValueNoErrorNoCatchedExc();
      long ignoredId=pageState.getEntry("assId").longValueNoErrorNoCatchedExc();
      if (res!=null && res instanceof Person && startMillis>0 && endMillis>0 && License.assertLevel(20)){

        // non ci interessa il passato ma solo da oggi alla fine del task
        startMillis=startMillis<System.currentTimeMillis()?System.currentTimeMillis():startMillis;

        PlanUtilities planUtilities = new PlanUtilities();
        Period period = new Period(startMillis, endMillis);
        PlanUtilities.ResourceLoad totalLoadAndWork = planUtilities.getTotalLoadAndWork(res, period,simulatedLoad,ignoredId);
        json.element("stats",totalLoadAndWork.getStats(period));
      }

    //--------------------- NOTES MANAGEMENT ----------------------------------------------------------
    } else if ("GETNOTES".equals(pageState.command)) {
      PlanControllerAction.getNotes(jsonHelper.json, pageState);
    } else if ("SAVENOTES".equals(pageState.command)) {
      PlanControllerAction.saveNotes(jsonHelper.json, pageState);
    } else if ("DELNOTES".equals(pageState.command)) {
      PlanControllerAction.saveNotes(jsonHelper.json, pageState);

    }
  } catch (Throwable t) {
    jsonHelper.error(t);
  }
  jsonHelper.close(pageContext);

%>