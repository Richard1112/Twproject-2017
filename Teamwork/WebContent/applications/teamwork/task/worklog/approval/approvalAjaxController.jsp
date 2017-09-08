<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.security.TeamworkPermissions, com.twproject.worklog.Worklog, com.twproject.worklog.WorklogStatus, net.sf.json.JSONObject,
org.jblooming.agenda.CompanyCalendar, org.jblooming.persistence.PersistenceHome, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.ActionUtilities, org.jblooming.waf.JSONHelper, org.jblooming.waf.view.PageState, org.jblooming.ontology.businessLogic.DeleteHelper" %><%



  PageState pageState = PageState.getCurrentPageState(request);

  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();


  JSONHelper jsonHelper = new JSONHelper();
  JSONObject json = jsonHelper.json;
  try {


    if ("REMOVEWL".equals(pageState.command)) {
      String wlId = pageState.getEntry("wlId").stringValueNullIfEmpty();
      if (JSP.ex(wlId)) {
        Worklog wl = (Worklog) PersistenceHome.findByPrimaryKey(Worklog.class, wlId);
        wl.bricks.testWritePermission(logged);
        //wl.remove();
        DeleteHelper.cmdDelete(wl,pageState);
      }
    } else if ("SAVEWL".equals(pageState.command)){
      String wlId = pageState.getEntry("wlId").stringValueNullIfEmpty();
      long dur=pageState.getEntry("dur").durationInWorkingMillis(true);
      if (JSP.ex(wlId) && dur>0) {
        Worklog wl = (Worklog) PersistenceHome.findByPrimaryKey(Worklog.class, wlId);
        wl.bricks.testWritePermission(logged);
        wl.setDuration(dur);
        wl.store();
        jsonHelper.json.element("normValue",DateUtilities.getMillisInHoursMinutes(dur) );
        //out.println("normValue='"+ DateUtilities.getMillisInHoursMinutes(dur)+"'");
      }
    } else if ("SAVEDESCR".equals(pageState.command)){
      String wlId = pageState.getEntry("wlId").stringValueNullIfEmpty();
      if (JSP.ex(wlId)) {
        Worklog wl = (Worklog) PersistenceHome.findByPrimaryKey(Worklog.class, wlId);
        wl.bricks.testWritePermission(logged);
        ActionUtilities.setString(pageState.getEntry("descr"),wl,"action");
        wl.store();
      }
    } else if ("SAVESTATUS".equals(pageState.command)){
      pageState.initializeEntries("row");
      String wlId = pageState.getEntry("wlId").stringValueNullIfEmpty();
      if (JSP.ex(wlId) && pageState.getEntry("sts").intValueNoErrorCodeNoExc()>=0) {
        Worklog wl = (Worklog) PersistenceHome.findByPrimaryKey(Worklog.class, wlId);
        wl.testPermission(logged, TeamworkPermissions.worklog_manage); // si cambia status ci vuole i permessi veri
        WorklogStatus wls = (WorklogStatus) PersistenceHome.findByPrimaryKey(WorklogStatus.class, pageState.getEntry("sts").intValueNoErrorCodeNoExc());
        wl.setStatus(wls);
        wl.store();
      }
    } else if ("CHANGEDAY".equals(pageState.command)){
      String wlId = pageState.getEntry("wlId").stringValueNullIfEmpty();
      int day=pageState.getEntry("day").intValueNoErrorCodeNoExc();
      if (JSP.ex(wlId)) {
        Worklog wl = (Worklog) PersistenceHome.findByPrimaryKey(Worklog.class, wlId);
        wl.bricks.testWritePermission(logged);
        CompanyCalendar cc= new CompanyCalendar(wl.getInserted());
        cc.set(CompanyCalendar.DAY_OF_WEEK,day);
        wl.setInserted(cc.getTime());
        wl.store();
      }

    }
  } catch (Throwable t) {
    jsonHelper.error(t);
  }


  jsonHelper.close(pageContext);


%>
