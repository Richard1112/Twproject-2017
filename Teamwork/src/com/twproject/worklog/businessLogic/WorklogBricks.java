package com.twproject.worklog.businessLogic;

import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Resource;
import com.twproject.security.TeamworkPermissions;
import com.twproject.task.Assignment;
import com.twproject.worklog.Worklog;
import com.twproject.worklog.WorklogStatus;
import com.twproject.worklog.WorklogSupport;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.operator.Operator;
import org.jblooming.oql.OqlQuery;
import org.jblooming.oql.QueryHelper;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.security.SecurityException;
import org.jblooming.utilities.CodeValueList;
import org.jblooming.waf.Bricks;
import org.jblooming.waf.constants.SecurityConstants;
import org.jblooming.waf.html.input.ColorValueChooser;
import org.jblooming.waf.html.input.Combo;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageState;

import java.util.Date;
import java.util.List;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Apr 17, 2008
 * Time: 1:01:26 PM
 */
public class WorklogBricks extends Bricks {

  Worklog mainObject;

  public WorklogBricks(Worklog wl) {
    this.mainObject = wl;
  }


  public static QueryHelper getWorklogForDay(Resource resource, Date day) {

    String hql = "select worklog from " + Worklog.class.getName() + " as worklog";
    QueryHelper qhelp = new QueryHelper(hql);
    qhelp.addOQLClause("worklog.assig.resource=:resource", "resource", resource);

    CompanyCalendar cc = new CompanyCalendar(day);

    qhelp.addOQLClause("worklog.inserted >= :startDate", "startDate", cc.setAndGetTimeToDayStart());
    qhelp.addOQLClause("worklog.inserted <= :endDate", "endDate", cc.setAndGetTimeToDayEnd());

    return qhelp;
  }

  public static long getTotalWorklogForDay(Resource resource, Date day) throws PersistenceException {

    String hql = "select sum(worklog.duration) from " + Worklog.class.getName() + " as worklog";
    QueryHelper qhelp = new QueryHelper(hql);
    qhelp.addOQLClause("worklog.assig.resource=:resource", "resource", resource);

    CompanyCalendar cc = new CompanyCalendar(day);

    qhelp.addOQLClause("worklog.inserted >= :startDate", "startDate", cc.setAndGetTimeToDayStart());
    qhelp.addOQLClause("worklog.inserted <= :endDate", "endDate", cc.setAndGetTimeToDayEnd());

    return (Long) qhelp.toHql().uniqueResultNullIfEmpty();
  }


  public static ColorValueChooser getStatusChooser(String fieldName, boolean showChoose, PageState pageState) throws PersistenceException {
    return getStatusChooser(fieldName, showChoose, false, pageState);
  }

  public static ColorValueChooser getStatusChooser(String fieldName, boolean showChoose, boolean multiple, PageState pageState) throws PersistenceException {

    ColorValueChooser ccv = new ColorValueChooser(fieldName, "WLSTATUS");
    ccv.showOpener=true;
    ccv.multiSelect = multiple;
    if (showChoose)
      ccv.addCodeColorValue("", "#DEDEDE", pageState.getI18n("EDITOR_CHOOSE"));

    ccv.addCodeColorValue("0", "#FFCC33", pageState.getI18n("WORKLOG_STATUS_NONE"));

    List<WorklogStatus> wls = new OqlQuery("select wl from " + WorklogStatus.class.getName() + " as wl order by wl.intValue").list();
    for (WorklogStatus i : wls) {
      ccv.addCodeColorValue(i.getId() + "", i.getColor() != null ? i.getColor() : "DEDEDE", i.getName());
    }

    return ccv;
  }

  /**
   * si fà un controllo completa su status, date e permessi. Da usare quando può essere un'azione fatta da un operatore sui suoi wl (potrebbe non avere permesso di manage).
   * è più lenta del controllo stecchito con hasPermissionFor
   *
   * @param loggedOperator
   * @return
   */
  public boolean canWrite(TeamworkOperator loggedOperator) {
    try {
      return testWrite(loggedOperator);
    } catch (SecurityException se){
      return false;
    }
  }

  /**
   * si fà un controllo completa su status, date e permessi. Da usare quando può essere un'azione fatta da un operatore sui suoi wl (potrebbe non avere permesso di manage).
   *
   * @param loggedOperator
   * @throws SecurityException
   */
  public void testWritePermission(TeamworkOperator loggedOperator) throws SecurityException {
    testWrite (loggedOperator);

  }


  // questo serve per dare una eccezione informata e/o ritornare true
  private boolean testWrite(TeamworkOperator loggedOperator) throws SecurityException {
    if (mainObject.hasPermissionFor(loggedOperator, TeamworkPermissions.worklog_manage))
      return true;

    //é un wl tuo e non hai permessi "manage"
    Assignment assig = mainObject.getAssig();
    if (assig != null && assig.getResource().equals(loggedOperator.getPerson())) {

      //se c'è lo status settato esce
      if (mainObject.getStatus() != null)
        throw new SecurityException(I18n.get("CANNOT_CHANGE_APPROVED_WORK"));


      // se la custom feature è abilitata non puoi mettere wl nel passato
      long notBeyond = Long.MIN_VALUE;
      if (I18n.isActive("CUSTOM_FEATURE_DO_NOT_ACCEPT_OLD_WORLOG")) {
        int days = new ClientEntry("dummy", I18n.get("CUSTOM_FEATURE_DO_NOT_ACCEPT_OLD_WORLOG")).durationInWorkingDaysNoErrorNoCatchedExc(false);
        notBeyond = days > 0 ? System.currentTimeMillis() - days * CompanyCalendar.MILLIS_IN_DAY : notBeyond;
      }

      boolean timeOk = mainObject.getInserted() == null || mainObject.getInserted().getTime() >= notBeyond && mainObject.getInserted().getTime()< System.currentTimeMillis() + CompanyCalendar.MILLIS_IN_WEEK ;
      if (!timeOk)
        throw new SecurityException(I18n.get("CANNOT_SAVE_IN_THE_PAST"));
      return true;
    }
    return false;
  }


  public String drawStatus(PageState pageState) {
    return WorklogBricks.drawStatus(mainObject.getStatus(), pageState);
  }


  public static String drawStatus(WorklogStatus wls, PageState pageState) {
    return "<span class=\"teamworkIcon\" style=\"font-size:110%; color:" + (wls != null ? wls.getColor() : "#FFCC33") + ";\" " +
      "title=\"" + (wls == null ? I18n.get("WORKLOG_STATUS_NONE") : wls.getName()) + "\">&copy;</span>";
  }

}
