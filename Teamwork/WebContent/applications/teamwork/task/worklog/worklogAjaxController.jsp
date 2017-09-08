<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.security.TeamworkPermissions, com.twproject.task.Assignment, com.twproject.worklog.Worklog, com.twproject.worklog.businessLogic.WorklogAction, net.sf.json.JSONObject, org.jblooming.agenda.CompanyCalendar, org.jblooming.agenda.Period, org.jblooming.agenda.Scale, org.jblooming.designer.DesignerField, org.jblooming.oql.OqlQuery, org.jblooming.persistence.PersistenceHome, org.jblooming.persistence.exceptions.FindException, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.JSONHelper, org.jblooming.waf.SessionState, org.jblooming.waf.constants.Commands, org.jblooming.waf.exceptions.ActionException, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.ClientEntry, org.jblooming.waf.view.PageState, java.text.ParseException, java.util.Date, java.util.List, com.twproject.task.businessLogic.IssueAction, com.twproject.worklog.WorklogStatus, com.twproject.resource.Person, org.jblooming.oql.QueryHelper, com.twproject.worklog.businessLogic.WorklogBricks, org.jblooming.waf.html.table.ListHeader, net.sf.json.JSONArray, com.twproject.plan.PlanUtilities, java.util.TreeMap, com.twproject.task.businessLogic.TimeCounterAction, com.twproject.task.Issue, org.jblooming.persistence.hibernate.PersistenceContext, org.jblooming.ontology.businessLogic.DeleteHelper" %><%!

  // this is use for update the single cell
  void computeVariableUpdates(Assignment ass, Date when, JSONObject ret) throws FindException {
    Period p = Period.getDayPeriodInstance(when);
    String hql = "select wl from " + Worklog.class.getName() + " as wl where wl.assig.id=:assid and wl.inserted between :sd and :ed)";

    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setString("assid", ass.getId() + "");
    oql.getQuery().setTimestamp("sd", p.getStartDate());
    oql.getQuery().setTimestamp("ed", p.getEndDate());
    List<Worklog> wls = oql.list();
    long totMillis = 0;
    int quant = 0;

    for (Worklog wl : wls) {
      quant++;
      totMillis += wl.getDuration();
    }

    ret.element("totMillis", totMillis);
    ret.element("numWl", quant);
  }

%><%


  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();


  //---  HTML PART-------------------------------------------------------------- ADD ROW

  if ("ADDROW".equals(pageState.command)) {
    String assId = pageState.getEntry("assId").stringValueNullIfEmpty();
    long minMillisInBar = pageState.getEntry("minMillisInBar").longValueNoErrorNoCatchedExc();
    if (JSP.ex(assId)) {
      Assignment ass = (Assignment) PersistenceHome.findByPrimaryKey(Assignment.class, assId);
      ass.testPermission(logged, TeamworkPermissions.task_canRead); // this is almost a fake test as the

      JspHelper rowDrawer = new JspHelper("partWeekRow.jsp");
      rowDrawer.parameters.put("ass", ass);
      rowDrawer.parameters.put("minMillisInBar", minMillisInBar);

      rowDrawer.parameters.put("scale", Scale.getScaleAndSynch(Scale.ScaleType.valueOf(pageState.getEntry("scaleType").stringValueNullIfEmpty()), minMillisInBar, true, SessionState.getLocale()));

      rowDrawer.toHtml(pageContext);
    }



    //----- JSON PART -----------------------------------------------------------------

  } else {


    JSONHelper jsonHelper = new JSONHelper();
    JSONObject json = jsonHelper.json;
    try {


      if ("LOADDETAIL".equals(pageState.command)) {
        String assId = pageState.getEntry("assId").stringValueNullIfEmpty();
        //long focusedMillis = pageState.getEntry("millis").longValueNoErrorNoCatchedExc();
        Date focusedDate = pageState.getEntry("date").dateValueNoErrorNoCatchedExc();
        if (JSP.ex(assId) && focusedDate!=null) {
          long focusedMillis= focusedDate.getTime();

          // se la custom feature è abilitata non puoi better wl nel passato
          long notBeyond = Long.MIN_VALUE;
          if (I18n.isActive("CUSTOM_FEATURE_DO_NOT_ACCEPT_OLD_WORLOG")) {
            int days = new ClientEntry("dummy", I18n.get("CUSTOM_FEATURE_DO_NOT_ACCEPT_OLD_WORLOG")).durationInWorkingDaysNoErrorNoCatchedExc(false);
            notBeyond = days > 0 ? System.currentTimeMillis() - days * CompanyCalendar.MILLIS_IN_DAY : notBeyond;
          }
          boolean disabledInThePast = focusedMillis < notBeyond || focusedMillis > System.currentTimeMillis() + CompanyCalendar.MILLIS_IN_WEEK;

          Period p = Period.getDayPeriodInstance(new Date(focusedMillis));
          String hql = "select wl from " + Worklog.class.getName() + " as wl where wl.assig.id=:assid and wl.inserted between :sd and :ed)";
          OqlQuery oql = new OqlQuery(hql);
          oql.getQuery().setString("assid", assId);
          oql.getQuery().setTimestamp("sd", p.getStartDate());
          oql.getQuery().setTimestamp("ed", p.getEndDate());
          List<Worklog> wls = oql.list();
          JSONArray worklogs = new JSONArray();
          if (JSP.ex(wls)) {
            for (Worklog w : wls) {
              JSONObject jwl = w.jsonify();
              jwl.element("canWrite", !disabledInThePast && w.bricks.canWrite(logged));
              worklogs.add(jwl);
            }
            json.element("worklogs", worklogs);
          }

        }


        //---------------------------------------------------------------------- SAVE WORKLOG
      } else if (Commands.SAVE.equals(pageState.command)) {
        Worklog worklog = new WorklogAction(pageState).cmdSave();

        if (worklog != null) {
          json.element("worklog", worklog.jsonify());
          // creates javascript for recompute total millis for the cell
          computeVariableUpdates(worklog.getAssig(), worklog.getInserted(), json);
          json.element("wlId", worklog.getId());
          json.element("worklog", worklog.jsonify());
        }


        //---------------------------------------------------------------------- DELETE WORKLOG
      } else if (Commands.DELETE.equals(pageState.command)) {
        WorklogAction action = new WorklogAction(pageState);
        action.cmdDelete();


      } else if ("UPDDESCR".equals(pageState.command)) {
        String wlId = pageState.getEntry("wlId").stringValueNullIfEmpty();
        if (JSP.ex(wlId)) {
          Worklog wl = (Worklog) PersistenceHome.findByPrimaryKey(Worklog.class, wlId);
          if (wl != null) {
            wl.bricks.testWritePermission(logged);
            String action = pageState.getEntry("descr").stringValueNullIfEmpty();
            wl.setAction(action);
            wl.bricks.testWritePermission(logged);
            wl.store();
            json.element("worklog", wl.jsonify());
          }
        }

      } else if ("UPDDATE".equals(pageState.command)) {
        String wlId = pageState.getEntry("wlId").stringValueNullIfEmpty();
        if (JSP.ex(wlId)) {
          Worklog wl = (Worklog) PersistenceHome.findByPrimaryKey(Worklog.class, wlId);
          if (wl != null) {
            wl.bricks.testWritePermission(logged);
            Date oldDate = wl.getInserted();
            CompanyCalendar cc = new CompanyCalendar(oldDate);
            Date newDate = pageState.getEntry("newdate").dateValueNoErrorNoCatchedExc();
            if (newDate != null) {
              cc.setTime(newDate);
              cc.setAndGetTimeToDayStart();
              cc.add(CompanyCalendar.MILLISECOND, CompanyCalendar.getMillisFromMidnight(oldDate));
              wl.setInserted(cc.getTime());

              wl.bricks.testWritePermission(logged);
              wl.store();
            }
          }
        }

      } else if ("REMOVEWL".equals(pageState.command)) {
        String wlId = pageState.getEntry("wlId").stringValueNullIfEmpty();
        if (JSP.ex(wlId)) {
          Worklog wl = Worklog.load(wlId);
          wl.bricks.testWritePermission(logged);
          Assignment ass = wl.getAssig();
          Date when = wl.getInserted();
          //wl.remove();
          DeleteHelper.cmdDelete(wl,pageState);
          computeVariableUpdates(ass, when, json);
          json.element("deletedId", wlId);
        }

        // ritorna lista dei worklog inseriti per il focused day dal logged
      } else if ("LISTWL1DAY".equals(pageState.command)) {
        long focusMillis = pageState.getEntry("FOCUS_MILLIS").longValueNoErrorNoCatchedExc();
        focusMillis = focusMillis == 0 ? System.currentTimeMillis() : focusMillis;

        Person focusedResource = Person.getLoggedPerson(pageState);

        Date focusedDate = new Date(focusMillis);
        QueryHelper qhelp = WorklogBricks.getWorklogForDay(focusedResource, focusedDate);
        ListHeader.orderAction(qhelp, "WORKLH", pageState, "worklog.inserted");
        List<Worklog> wklg = qhelp.toHql().list();

        JSONArray worklogs = new JSONArray();
        if (wklg.size() > 0) {
          for (Worklog workLog : wklg) {
            worklogs.add(workLog.jsonify());
          }
        }
        json.element("worklogs", worklogs);

        Long wb = focusedResource.getWorkablePlan(Period.getDayPeriodInstance(focusedDate)).get(DateUtilities.dateToInt(focusedDate));
        json.element("workable", wb);


      } else if ("STOPCOUNTER".equals(pageState.command)) {
        pageState.mainObjectId = pageState.getEntry("assId").intValueNoErrorCodeNoExc() + "";
        TimeCounterAction tcc = new TimeCounterAction(pageState);
        Worklog worklog = tcc.cmdStop(pageState);
        if (worklog != null) {
          json.element("worklog", worklog.jsonify());
          pageState.getEntryOrDefault("wpTCSAssignment", worklog.getAssig().getId() + "");
        }

      } else if ("STARTCOUNTER".equals(pageState.command)) {
        pageState.mainObjectId = pageState.getEntry("assId").intValueNoErrorCodeNoExc() + "";
        TimeCounterAction tcc = new TimeCounterAction(pageState);
        Worklog worklog = tcc.cmdStart(pageState);
        if (worklog != null) {
          json.element("worklog", worklog.jsonify());
        }

        Assignment a = Assignment.load(pageState.mainObjectId);
        if (a != null && a.getResource().equals(logged.getPerson())) {
          json.element("assignment", a.jsonify());
          pageState.getEntryOrDefault("wpTCSAssignment", a.getId() + "");
        }


      } else if ("WLCHSTS".equals(pageState.command)) {
        pageState.initializeEntries("row");
        String wlId = pageState.getEntry("wlId").stringValueNullIfEmpty();
        int stsId = pageState.getEntry("stsId").intValueNoErrorCodeNoExc();
        if (JSP.ex(wlId)) {
          Worklog wl = Worklog.load(wlId);
          wl.bricks.testWritePermission(logged);

          if (stsId > 0) {
            wl.setStatus(WorklogStatus.load(stsId + ""));
          } else {
            wl.setStatus(null);
          }
          wl.store();
        }


      }
    } catch (Throwable t) {
      jsonHelper.error(t);
    }
    jsonHelper.close(pageContext);
  }
%>