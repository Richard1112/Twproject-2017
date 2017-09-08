package org.jblooming.waf.html.input;

import net.sf.json.JSONObject;
import org.jblooming.agenda.*;
import org.jblooming.utilities.CodeValueList;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.html.core.HtmlBootstrap;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.RestState;

import javax.servlet.jsp.PageContext;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;

public class ScheduleComposer extends JspHelper implements HtmlBootstrap {

  public boolean isMinute = false;
  public boolean isSingle = true;
  public boolean isDaily = true;
  public boolean isWeekly = true;
  public boolean isMonthly = true;
  public boolean isYearly = true;
  public String height = "155";
  public boolean durationInWorkingDays = false;
  public boolean durationInTime = true;
  public boolean addJavaScript = false;
  public String divWidth = "560px";

  public boolean readOnly=false;

  public static final String INITIALIZE = "IN";
  public static final String DRAW_INPUT = "DI";

  public ScheduleComposer(String id) {
    this.id = id;
    urlToInclude = "/commons/scheduler/partScheduleComposer.jsp";
  }



  public String getDiscriminator() {
    return this.getClass().getName();
  }

  public boolean validate(PageState pageState) {
    return true;
  }

  public void toHtml(PageContext pageContext) {
    pageContext.getRequest().setAttribute(ACTION, DRAW_INPUT);
    super.toHtml(pageContext);
  }


  /*
  Metodi per fare il make di uno schedule
  */
  public static void make(String fieldName, ScheduleSupport schedule, RestState pageState) {
    pageState.addClientEntry(fieldName,schedule.jsonify().toString());
  }


  public static ScheduleSupport getSchedule(String fieldName, RestState pageState) {
    JSONObject json= JSONObject.fromObject(pageState.getEntry(fieldName).stringValueNullIfEmpty());
    return ScheduleSupport.getInstancefromJSON(json);
  }

  public static CodeValueList getMonths(RestState pageState) {
    CodeValueList months = new CodeValueList();
    Locale locale = SessionState.getLocale();
    CompanyCalendar cal = new CompanyCalendar(locale);
    cal.setTime(new Date());
    cal.set(Calendar.MONTH, Calendar.JANUARY);
    for (int i = 0; i < 12; i++) {
      months.add(cal.get(Calendar.MONTH)+"", DateUtilities.dateToString(cal.getTime(), "MMMM"));
      cal.add(CompanyCalendar.MONTH, 1);
    }
    return months;
  }

  public static CodeValueList getDaysOfWeek(RestState pageState) {
    CodeValueList daysOfWeek = new CodeValueList();
    Locale locale = SessionState.getLocale();
    CompanyCalendar cal = new CompanyCalendar(locale);
    cal.setTime(new Date());
    cal.set(Calendar.DAY_OF_WEEK, cal.getFirstDayOfWeek());
    cal.setMillisFromMidnight(1);
    for (int i = 0; i < 7; i++) {
      daysOfWeek.add(cal.get(CompanyCalendar.DAY_OF_WEEK) + "", DateUtilities.dateToString(cal.getTime(), "EEEE"));
      cal.add(CompanyCalendar.DAY_OF_WEEK, 1);
    }
    return daysOfWeek;
  }

  public static CodeValueList getWeeksOfMonth(RestState pageState) {
    CodeValueList weeksOfMonth = new CodeValueList();
    weeksOfMonth.add("1", I18n.get("FIRST"));
    weeksOfMonth.add("2", I18n.get("SECOND"));
    weeksOfMonth.add("3", I18n.get("THIRD"));
    weeksOfMonth.add("4", I18n.get("FOURTH"));
    weeksOfMonth.add("5", I18n.get("LAST"));
    return weeksOfMonth;
  }


}