package com.twproject.plan.businessLogic;

import com.twproject.operator.TeamworkOperator;
import com.twproject.plan.PlanUtilities;
import com.twproject.security.TeamworkPermissions;
import com.twproject.task.Assignment;
import com.twproject.worklog.WorklogPlan;
import net.sf.json.JSONObject;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.agenda.Period;
import org.jblooming.agenda.Scale;
import org.jblooming.operator.Operator;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.ActionSupport;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.constants.SecurityConstants;
import org.jblooming.waf.view.PageState;
import org.hibernate.Query;
import org.jblooming.waf.view.RestState;

import java.io.Serializable;
import java.text.ParseException;
import java.util.Date;
import java.util.Map;
import java.util.List;
import java.util.TreeMap;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 8-set-2006 : 12.01.56
 */
public class PlanControllerAction extends ActionSupport {

  public PlanControllerAction(PageState pageState) {
    super(pageState);
  }


  public static void ajaxSaveCell(JSONObject json, RestState pageState) throws ParseException, PersistenceException, org.jblooming.security.SecurityException {
    pageState.initializeEntries("table");
    Serializable assId = pageState.getEntry("assId").stringValueNullIfEmpty();
    int day = pageState.getEntry("day").intValueNoErrorCodeNoExc();


    String ret = "";
    boolean wpPassed = JSP.ex(pageState.getEntry("wp"));

    long wp = 0;
    try {
      wp = pageState.getEntry("wp").durationInWorkingMillis(true);
      wp=wp>CompanyCalendar.MILLIS_IN_DAY?0:wp;
    } catch (Exception e) {
    }
    if (assId != null && day > 0) {

      Date dDay = DateUtilities.intToDate(day);

      Operator logged = pageState.getLoggedOperator();
      Assignment assig = Assignment.load(assId);
      if (!assig.hasPermissionFor(logged, TeamworkPermissions.resource_manage))
        throw new org.jblooming.security.SecurityException(SecurityConstants.I18N_PERMISSION_LACKING);

      //String oql = "delete from " + WorklogPlan.class.getName() + " where assig=:ass and inserted=:time";
      String oql = "select wp from " + WorklogPlan.class.getName() + " as wp where wp.assig=:ass and wp.inserted=:time";
      OqlQuery q = new OqlQuery(oql);
      q.getQuery().setEntity("ass", assig);
      q.getQuery().setTimestamp("time", dDay);

      //int a = q.getQuery().executeUpdate(); // this may cause deadlock
      List<WorklogPlan> wps=q.getQuery().list();

      // create a new plan entry if estimation >0
      // if (wp > 0) { // romeved by bicch on 22/10/12
      // create a new plan entry if estimation is specified

      /*if (wpPassed) {
        WorklogPlan wpl = new WorklogPlan();
        wpl.setAssig(assig);
        wpl.setDuration(wp);
        wpl.setInserted(dDay);
        wpl.store();
      }*/

      if (wpPassed) {
        if (JSP.ex(wps)){
          //update first and remove the rest
          WorklogPlan wpl = wps.get(0);
          wpl.setDuration(wp);
          wpl.store();

          for (int i=1;i<wps.size();i++){
            wps.get(i).remove();
            Tracer.platformLogger.info("Duplicated WorklogPlan for assig Id:"+assId+" day:"+day+" has been removed.");
          }
        } else {
          //create it
          WorklogPlan wpl = new WorklogPlan();
          wpl.setAssig(assig);
          wpl.setDuration(wp);
          wpl.setInserted(dDay);
          wpl.store();
        }

      } else {  //no plan remove all
        for (WorklogPlan wpl:wps)
          wpl.remove();
      }


      long focusMillis = pageState.getEntry("focusMillis").longValueNoErrorNoCatchedExc();
      focusMillis=focusMillis==0?System.currentTimeMillis():focusMillis;
      Scale scale = Scale.getScaleAndSynch(Scale.ScaleType.valueOf(pageState.getEntry("scaleType").stringValueNullIfEmpty()), focusMillis, true, pageState.getLoggedOperator().getLocale());

      PlanUtilities.ResourceLoad workPlan = new PlanUtilities().getTotalLoadAndWork(assig.getResource(), scale.getPeriod());

      json.element("ok", true);
      json.element("value", (wp > 0 ? DateUtilities.getMillisInHoursMinutes(wp) : wpPassed ? "0:00" : ""));

      json.element("estimatedWorklog", assig.getEstimatedWorklog());
      json.element("totalLoadAndWork", workPlan.totalLoadAndWork.subMap(DateUtilities.dateToInt(scale.startPointDate) - 1, DateUtilities.dateToInt(scale.endPointDate) + 1));

      CompanyCalendar cc = new CompanyCalendar(System.currentTimeMillis());
      long planned = assig.getWorklogPlanned(cc.setAndGetTimeToDayStart());

      json.element("totPlanned",planned);

    }
  }


  public static void updateEstimation(JSONObject json, RestState pageState) throws PersistenceException, org.jblooming.security.SecurityException, ActionException, ParseException {
    pageState.initializeEntries("table");
    Serializable assId = pageState.getEntry("assId").stringValueNullIfEmpty();

    Operator logged = pageState.getLoggedOperator();
    Assignment assig = Assignment.load(assId);
    //if (!assig.hasPermissionFor(logged, TeamworkPermissions.assignment_manage))
    if (!assig.hasPermissionFor(logged, TeamworkPermissions.assignment_canCRW))
      throw new org.jblooming.security.SecurityException(SecurityConstants.I18N_PERMISSION_LACKING);

    long estim = pageState.getEntry("estim").durationInWorkingMillis(true);
    assig.setEstimatedWorklog(estim);
    assig.store();

    long focusMillis = pageState.getEntry("focusMillis").longValueNoErrorNoCatchedExc();
    focusMillis=focusMillis==0?System.currentTimeMillis():focusMillis;
    Scale scale = Scale.getScaleAndSynch(Scale.ScaleType.valueOf(pageState.getEntry("scaleType").stringValueNullIfEmpty()), focusMillis, true, pageState.getLoggedOperator().getLocale());

    PlanUtilities.ResourceLoad workPlan = new PlanUtilities().getTotalLoadAndWork(assig.getResource(), scale.getPeriod());
    json.element("totalLoadAndWork", workPlan.totalLoadAndWork.subMap(DateUtilities.dateToInt(scale.startPointDate) - 1, DateUtilities.dateToInt(scale.endPointDate) + 1));

    CompanyCalendar cc = new CompanyCalendar(System.currentTimeMillis());
    long planned = assig.getWorklogPlanned(cc.setAndGetTimeToDayStart());


    json.element("totPlanned",planned);
    json.element("estimatedWorklog", assig.getEstimatedWorklog());

  }

  public static void reconcilePlan(JSONObject json, RestState pageState) throws PersistenceException, org.jblooming.security.SecurityException, ParseException {
    pageState.initializeEntries("table");
    Serializable assId = pageState.getEntry("assId").stringValueNullIfEmpty();

    Operator logged = pageState.getLoggedOperator();
    Assignment assig = Assignment.load(assId);
    if (!assig.hasPermissionFor(logged, TeamworkPermissions.resource_manage) )
      throw new org.jblooming.security.SecurityException(SecurityConstants.I18N_PERMISSION_LACKING);


    //reconcile plan starting from a millis and going back in the past
    long fromMillis = pageState.getEntry("fromMillis").longValueNoErrorNoCatchedExc();

    //if no millis start from now
    if (fromMillis <= 0)
      fromMillis = System.currentTimeMillis();

    //remove the plan
    Query query = new OqlQuery("delete from " + WorklogPlan.class.getName() + " as pl where pl.assig=:ass and inserted<=:start").getQuery();
    query.setEntity("ass", assig);
    query.setTimestamp("start", new Date(fromMillis));
    query.executeUpdate();


    //select work done group by day
    TreeMap<Integer, Long> doneByDay = assig.getWorklogDoneByDay(new Period(0, fromMillis));


    //create the new plan
    for (Integer day : doneByDay.keySet()) {
      WorklogPlan wpl = new WorklogPlan();
      wpl.setAssig(assig);
      wpl.setDuration(doneByDay.get(day));
      wpl.setInserted(DateUtilities.intToDate(day));
      wpl.store();
    }


    //recompute load
    long focusMillis = pageState.getEntry("focusMillis").longValueNoErrorNoCatchedExc();
    focusMillis=focusMillis==0?System.currentTimeMillis():focusMillis;
    Scale scale = Scale.getScaleAndSynch(Scale.ScaleType.valueOf(pageState.getEntry("scaleType").stringValueNullIfEmpty()), focusMillis, true, pageState.getLoggedOperator().getLocale());

    PlanUtilities.ResourceLoad workPlan = new PlanUtilities().getTotalLoadAndWork(assig.getResource(), scale.getPeriod());
    json.element("totalLoadAndWork", workPlan.totalLoadAndWork.subMap(DateUtilities.dateToInt(scale.startPointDate) - 1, DateUtilities.dateToInt(scale.endPointDate) + 1));
    json.element("assignementsDetailLine", workPlan.assignementsDetailLine.get(assig));

    CompanyCalendar cc = new CompanyCalendar(System.currentTimeMillis());

    long planned = assig.getWorklogPlanned(cc.setAndGetTimeToDayStart());

    json.element("totPlanned",planned);
    json.element("estimatedWorklog", assig.getEstimatedWorklog());

  }

  public static void fillPlanWith0(JSONObject json, RestState pageState) throws PersistenceException, org.jblooming.security.SecurityException, ActionException, ParseException {
    pageState.initializeEntries("table");

    Serializable assId = pageState.getEntry("assId").stringValueNullIfEmpty();

    Operator logged = pageState.getLoggedOperator();
    Assignment assig = Assignment.load(assId);
    if (!assig.hasPermissionFor(logged, TeamworkPermissions.resource_manage) )
      throw new org.jblooming.security.SecurityException(SecurityConstants.I18N_PERMISSION_LACKING);


    long focusMillis = pageState.getEntry("focusMillis").longValueNoErrorNoCatchedExc();

    if (focusMillis > 0) {
      // get the current scale factor
      Scale scale = Scale.getScaleAndSynch(Scale.ScaleType.valueOf(pageState.getEntry("scaleType").stringValue()), focusMillis, true, pageState.getLoggedOperator().getLocale());

      CompanyCalendar cc = new CompanyCalendar(scale.startPointDate);

      Period taskPeriod = assig.getTask().getSchedule();

      Map<Integer, Assignment.PlannedWork> workPlanned = assig.getWorklogPlan(scale.getPeriod(), true);

      while (cc.getTimeInMillis() <= scale.endPointTime) {
        if (cc.isWorkingDay() && taskPeriod.contains(cc.getTime()) && !workPlanned.containsKey(DateUtilities.dateToInt(cc.getTime()))) {
          WorklogPlan wpl = new WorklogPlan();
          wpl.setAssig(assig);
          wpl.setDuration(0);
          wpl.setInserted(cc.getTime());
          wpl.store();
        }

        cc.add(CompanyCalendar.DATE, 1);
      }

      //recompute load
      PlanUtilities.ResourceLoad workPlan = new PlanUtilities().getTotalLoadAndWork(assig.getResource(), scale.getPeriod());
      json.element("totalLoadAndWork", workPlan.totalLoadAndWork.subMap(DateUtilities.dateToInt(scale.startPointDate) - 1, DateUtilities.dateToInt(scale.endPointDate) + 1));

    }
  }

  public static void refreshAss(JSONObject json, RestState pageState) throws PersistenceException, org.jblooming.security.SecurityException {
    Serializable assId = pageState.getEntry("assId").stringValueNullIfEmpty();

    Operator logged = pageState.getLoggedOperator();
    Assignment assig = Assignment.load(assId);
    if (!assig.hasPermissionFor(logged, TeamworkPermissions.resource_manage)) {
      json.element("ok",false);
      return;
      //throw new org.jblooming.security.SecurityException(SecurityConstants.I18N_PERMISSION_LACKING);
    }

    long focusMillis = pageState.getEntry("focusMillis").longValueNoErrorNoCatchedExc();

    if (focusMillis > 0) {
      // get the current scale factor
      Scale scale = Scale.getScaleAndSynch(Scale.ScaleType.valueOf(pageState.getEntry("scaleType").stringValueNullIfEmpty()), focusMillis, true, pageState.getLoggedOperator().getLocale());

      //recompute load
      PlanUtilities.ResourceLoad workPlan = new PlanUtilities().getTotalLoadAndWork(assig.getResource(), scale.getPeriod());
      json.element("totalLoadAndWork", workPlan.totalLoadAndWork.subMap(DateUtilities.dateToInt(scale.startPointDate) - 1, DateUtilities.dateToInt(scale.endPointDate) + 1));
      json.element("totalWorkDone", assig.getWorklogDone());

    }
  }


  public static void getNotes(JSONObject json, RestState pageState) throws ParseException, PersistenceException, org.jblooming.security.SecurityException {
    pageState.initializeEntries("table");

    Serializable assId = pageState.getEntry("assId").stringValueNullIfEmpty();
    int day = pageState.getEntry("day").intValueNoErrorCodeNoExc();

    if (assId != null && day > 0) {

      Date dDay = DateUtilities.intToDate(day);

      TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
      Assignment assig = Assignment.load(assId);
      boolean canManage = assig.hasPermissionFor(logged, TeamworkPermissions.resource_manage);
      if (!(canManage || assig.getResource().equals(logged.getPerson()) )){
        throw new org.jblooming.security.SecurityException(SecurityConstants.I18N_PERMISSION_LACKING);
      }

      String oql = "select plan from " + WorklogPlan.class.getName() + " as plan where plan.assig=:ass and plan.inserted=:time";
      Query q = new OqlQuery(oql).getQuery();
      q.setEntity("ass", assig);
      q.setTimestamp("time", dDay);
      q.setMaxResults(1);

      List <WorklogPlan> plan=q.list();

      String notes="";
      if (JSP.ex(plan)) {
        notes=plan.get(0).getAction();
      }

      json.element("notes",notes);
      json.element("canWrite",canManage);

    }
  }

  public static void saveNotes(JSONObject json, RestState pageState) throws ParseException, PersistenceException, org.jblooming.security.SecurityException {
    pageState.initializeEntries("table");

    Serializable assId = pageState.getEntry("assId").stringValueNullIfEmpty();
    int day = pageState.getEntry("day").intValueNoErrorCodeNoExc();

    if (assId != null && day > 0) {

      Date dDay = DateUtilities.intToDate(day);

      Operator logged = pageState.getLoggedOperator();
      Assignment assig = Assignment.load(assId);
      if (!assig.hasPermissionFor(logged, TeamworkPermissions.resource_manage))
        throw new org.jblooming.security.SecurityException(SecurityConstants.I18N_PERMISSION_LACKING);

      String oql = "select plan from " + WorklogPlan.class.getName() + " as plan where plan.assig=:ass and plan.inserted=:time";
      Query q = new OqlQuery(oql).getQuery();
      q.setEntity("ass", assig);
      q.setTimestamp("time", dDay);
      q.setMaxResults(1);

      List <WorklogPlan> plan=q.list();

      if (JSP.ex(plan)) {
        WorklogPlan worklogPlan = plan.get(0);
        worklogPlan.setAction(pageState.getEntry("notes").stringValueNullIfEmpty());
        worklogPlan.store();
      }
    }
  }



}
