package com.twproject.agenda;

import com.twproject.operator.TeamworkOperator;
import com.twproject.security.TeamworkPermissions;
import net.fortuna.ical4j.data.ParserException;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.agenda.EventType;
import org.jblooming.agenda.Period;
import org.jblooming.oql.QueryHelper;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.security.Area;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.html.input.SmartCombo;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import java.io.IOException;
import java.util.*;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 30-mag-2006 : 16.13.36
 */
public class AgendaBricks {

  static public ArrayList<TreeMap<Period, Event>> getListOfOverlappingDayPeriods(List<Event> totResult, Period todayPeriod) {

    TreeMap<Period, Event> totPeriodi = new TreeMap<Period, Event>();
    // iterata su tutti gli eventi del giorno da db, prende i periods per tutti gli eventi intersecati con oggi
    for (Event event : totResult) {
      List<Period> periods = event.getSchedule().getPeriods(todayPeriod, true,event.getExceptions());
      if (periods != null && periods.size() > 0) {
        for (Period period : periods) {
          period.setId(event.getId() + "" + period.getStartDate() + period.getEndDate());
          totPeriodi.put(period, event);
        }
      }
    }

    ArrayList<TreeMap<Period, Event>> columns = new ArrayList<TreeMap<Period, Event>>();
    ArrayList<Long> maxPerCol = new ArrayList();

    for (Period period : totPeriodi.keySet()) {
      boolean placed = false;

      //loop to find where to place
      for (int column = 0; column < columns.size(); column++) {
        long maxEnd = maxPerCol.get(column);

        // there is space?
        if (maxEnd <= period.getValidityStartTime()) {
          columns.get(column).put(period, totPeriodi.get(period));
          maxPerCol.set(column, period.getValidityEndTime());
          placed = true;
          break;
        }
      }

      if (!placed) {
        maxPerCol.add(new Long(period.getValidityEndTime()));
        TreeMap<Period, Event> map = new TreeMap<Period, Event>();
        map.put(period, totPeriodi.get(period));
        columns.add(map);
      }

    }
    return columns;
  }

  public static List<Event> getICalEventsInPeriod(List<Event> list, Period period) {
    List<Event> filterList = new ArrayList<Event>();
    for (Event e : list) {
      if ( e.getSchedule()!=null && !( e.getSchedule().getEndDate().before(period.getStartDate()) || e.getSchedule().getStartDate().after(period.getEndDate())))
        filterList.add(e);
    }
    return filterList;
  }


  public static SmartCombo getAgendaTypesCombo(String fieldName,PageState pageState) throws PersistenceException {
    //String sqlSelect = "select tt.id, tt.stringValue || ' ' || tt.description from " + TaskType.class.getName() + " as tt ";
    String hql = "select e.id, e.description from " + EventType.class.getName() + " as e";
    QueryHelper queryHelperForFiltering = new QueryHelper(hql);

    TeamworkOperator operator = (TeamworkOperator) pageState.getLoggedOperator();
    Set<Area> areas = operator.getAreasForPermissionPlusMine(TeamworkPermissions.task_canRead);
    queryHelperForFiltering.addOrQueryClause("e.area in (:areas) or e.area is null");
    queryHelperForFiltering.addParameter("areas", areas);

    String baseFilter = " (e.description like :" + SmartCombo.FILTER_PARAM_NAME + ") ";

    queryHelperForFiltering.addOQLClause(baseFilter);

    queryHelperForFiltering.addToHqlString(" order by e.description");

    String whereForId = "where e.id = :" + SmartCombo.FILTER_PARAM_NAME;

    SmartCombo evTySc = new SmartCombo(fieldName, hql, null, whereForId);
    evTySc.searchAll = true;
    evTySc.label=I18n.get(fieldName);
    evTySc.queryHelperForFiltering = queryHelperForFiltering;
    evTySc.separator = "</td><td>";
    evTySc.fieldSize = 20;

    return evTySc;
  }

  public static PageSeed getAgendaView(PageState pageState) {
    String agendaType = (String) pageState.sessionState.getAttribute("AGENDA_TYPE");
    PageSeed newPs = null;
    if ("MONTH".equals(agendaType)) {
      newPs = pageState.pageFromRoot("agenda/agendaMonth.jsp");
    } else {
      newPs = pageState.pageFromRoot("agenda/agendaWeekDay.jsp");
      newPs.addClientEntry("AGENDA_TYPE",agendaType);
      newPs.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));
    }
    return newPs;
  }


  /**
   *
   * @param pageState
   * @param period
   * @return una lista contenente quelli di default in sessione + eventualmente quelli da link esterno
   * controlla da quanto tempo sono stati aggiornati ed eventualmente li ricarica da esterno
   */
  public static List<Event> getIcalExternalEvents(PageState pageState,Period period ){
    List<Event> ret= new ArrayList<Event>();

    long lastRefresh=0;
    if (pageState.sessionState.getAttributes().containsKey("EXTERNAL_CAL_REFRESH_TIME"))
      lastRefresh=(Long)pageState.sessionState.getAttribute("EXTERNAL_CAL_REFRESH_TIME");

    if (lastRefresh<System.currentTimeMillis()- CompanyCalendar.MILLIS_IN_MINUTE*5) {
      pageState.sessionState.setAttribute("EXTERNAL_CAL_REFRESH_TIME", System.currentTimeMillis());

      //reload additional external iCal
      pageState.sessionState.getAttributes().remove("EXTERNAL_CAL_EVENTS");
      String external_cal_links = (String) pageState.sessionState.getAttribute("EXTERNAL_CAL_LINKS");
      if (JSP.ex(external_cal_links)) {
        try {
          List<Event> external_cal_events = IcalUtilities.getEventsFromURLs(external_cal_links);
          if (JSP.ex(external_cal_events))
            pageState.sessionState.setAttribute("EXTERNAL_CAL_EVENTS", external_cal_events);

        } catch (Throwable e) {
        }
      }

      //reload default iCal
      pageState.sessionState.getAttributes().remove("DEFAULT_CAL_EVENTS");
      String default_ext_linkd = pageState.getLoggedOperator().getOption("DEFAULT_EXT_CALENDAR");
      if (JSP.ex(default_ext_linkd)) {
        try {
          List<Event> default_cal_events = IcalUtilities.getEventsFromURLs(default_ext_linkd);
          if (JSP.ex(default_cal_events))
            pageState.sessionState.setAttribute("DEFAULT_CAL_EVENTS", default_cal_events);

        } catch (Throwable e) {
        }
      }
    }

    List<Event> sessionExternaICalEvents = (List<Event>) pageState.sessionState.getAttribute("EXTERNAL_CAL_EVENTS");
    List<Event> sessionDefaultICalEvents = (List<Event>) pageState.sessionState.getAttribute("DEFAULT_CAL_EVENTS");

    if (JSP.ex(sessionExternaICalEvents))
      ret.addAll(AgendaBricks.getICalEventsInPeriod(sessionExternaICalEvents, period));

    if (JSP.ex(sessionDefaultICalEvents))
      ret.addAll(AgendaBricks.getICalEventsInPeriod(sessionDefaultICalEvents, period));

    return ret;
  }

}
