package org.jblooming.agenda;

import org.jblooming.system.SystemConstants;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.constants.OperatorConstants;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.ontology.SerializedList;

import java.util.*;


public class CompanyCalendar extends GregorianCalendar {

  public static final long MILLIS_IN_MINUTE = 1000 * 60;
  public static final long MILLIS_IN_HOUR = MILLIS_IN_MINUTE * 60;
  public static final long MILLIS_IN_3_HOUR = MILLIS_IN_HOUR * 3;
  public static final long MILLIS_IN_6_HOUR = MILLIS_IN_3_HOUR * 2;
  public static final long MILLIS_IN_12_HOUR = MILLIS_IN_6_HOUR * 2;
  public static final long MILLIS_IN_DAY = MILLIS_IN_HOUR * 24;
  public static final long MILLIS_IN_WEEK = MILLIS_IN_DAY * 7;
  public static final long MILLIS_IN_2_WEEK = MILLIS_IN_WEEK * 2;
  public static final long MILLIS_IN_MONTH = (long) (MILLIS_IN_DAY * 30.4375);
  public static final long MILLIS_IN_3_MONTH = MILLIS_IN_MONTH * 3;
  public static final long MILLIS_IN_YEAR = (long) (MILLIS_IN_DAY * 365.25);
  public static final long MILLIS_IN_2_YEAR = MILLIS_IN_YEAR * 2;
  public static final long MILLIS_IN_5_YEAR = MILLIS_IN_YEAR * 5;

  public static int ALLDAYS[] = {Calendar.MONDAY, Calendar.TUESDAY, Calendar.WEDNESDAY,
          Calendar.THURSDAY, Calendar.FRIDAY, Calendar.SATURDAY};

  /**
   * Set of MM_dd format dates
   */
  private static SerializedList<String> holyDays = null;

  public static int WORKING_HOUR_BEGIN = 8 * 60 * 60 * 1000;
  public static int WORKING_HOUR_END = 20 * 60 * 60 * 1000;
  public static int WORKING_HOUR_TOTAL = 8 * 60 * 60 * 1000;
  /**
   * first millisecond of day: always 0
   */
  public final static int MIN_TIME = 0;
  /**
   * last millisecond of day: always 0
   */
  public final static int MAX_TIME = 24 * 60 * 60 * 1000 - 1;
  /**
   * is the minimal date the system can handle
   * this works between 970 ad and 2969 ad (SPQR)
   * had to do it in this way as Long.MAX_VALUE does not work
   */
  public final static Date MIN_DATE = new Date(-1000L * 3600L * 24L * 365L * 1000L);
  /**
   * is the max date the system can handle
   */
  public final static Date MAX_DATE = new Date(1000L * 3600L * 24L * 365L * 1000L);

  /**
   * this variables are reset when the application read values from global.properties
   */
  public static long MILLIS_IN_WORKING_DAY = MILLIS_IN_HOUR * 8;
  public static boolean SATURDAY_IS_WORKING_DAY = false;
  public static boolean SUNDAY_IS_WORKING_DAY = false;
  public static boolean FRIDAY_IS_WORKING_DAY = true;
  public static int WORKING_DAYS_PER_WEEK=5;

  public CompanyCalendar() {
    this(SessionState.getLocale());
    setTimeZone(SessionState.getTimeZone()); // robik fecit 2004/02/27
  }

  public CompanyCalendar(Locale l) {
    super(l);
  }

  public CompanyCalendar(long millis) {
    this(new Date(millis));
  }
  public CompanyCalendar(Date date) {
    this();
    setTime(date);
  }

  public CompanyCalendar(Date date, Locale l) {
    this(l);
    setTime(date);
  }

public static void setup(){
  String val = ApplicationState.getApplicationSetting(SystemConstants.FRIDAY_IS_WORKING_DAY);
  FRIDAY_IS_WORKING_DAY = !JSP.ex(val) || Fields.TRUE.equals(val);
  SATURDAY_IS_WORKING_DAY = Fields.TRUE.equals(ApplicationState.getApplicationSetting(SystemConstants.SATURDAY_IS_WORKING_DAY));
  SUNDAY_IS_WORKING_DAY = Fields.TRUE.equals(ApplicationState.getApplicationSetting(SystemConstants.SUNDAY_IS_WORKING_DAY));
  try {
    MILLIS_IN_WORKING_DAY = (int) DateUtilities.millisFromHourMinuteSmart(ApplicationState.getApplicationSetting(OperatorConstants.FLD_WORKING_HOUR_TOTAL));
  } catch (NumberFormatException e) {
    Tracer.platformLogger.warn("you wrote horrible things in FLD_WORKING_HOUR_TOTAL in global.properties: look at this " +
            ApplicationState.getApplicationSetting(OperatorConstants.FLD_WORKING_HOUR_TOTAL));
  }
  WORKING_DAYS_PER_WEEK=7-(FRIDAY_IS_WORKING_DAY?0:1) - (SATURDAY_IS_WORKING_DAY?0:1) - (SUNDAY_IS_WORKING_DAY?0:1); 
}

  /**
   * Is used to get the  start working time
   *
   * @return The number of millis elapsed from midnight.
   *         If the operator do not have specific setting the general one is returned.
   */
  public int getWorkingTimeStart() {
    return CompanyCalendar.WORKING_HOUR_BEGIN;
  }

  /**
   * Is used to get the  end working time
   *
   * @return The number of millis elapsed from midnight.
   *         If the operator do not have specific setting the general one is returned.
   */
  public int getWorkingTimeEnd() {
    return WORKING_HOUR_END;
  }


  public Date setAndGetTimeToDayStart() {
    this.set(Calendar.HOUR_OF_DAY, 0);
    this.set(Calendar.MINUTE, 0);
    this.set(Calendar.SECOND, 0);
    this.set(Calendar.MILLISECOND, 0);
    return this.getTime();
  }

  public Date setAndGetTimeToDayEnd() {
    this.set(Calendar.HOUR_OF_DAY, 0);
    this.set(Calendar.MINUTE, 0);
    this.set(Calendar.SECOND, 0);
    this.set(Calendar.MILLISECOND, 0);

    //why at 999 it goes to next day ?
    this.set(Calendar.MILLISECOND, 998);
    this.set(Calendar.SECOND, 59);
    this.set(Calendar.MINUTE, 59);
    this.set(Calendar.HOUR_OF_DAY, 23);

    return this.getTime();
  }


  public boolean isWorkingDay() {
    return !(isSaturday() && !SATURDAY_IS_WORKING_DAY) &&
            !(isSunday() && !SUNDAY_IS_WORKING_DAY) &&
            !(isFriday() && !FRIDAY_IS_WORKING_DAY) &&
            !isHolyDay();
  }


  public boolean isHolyDay() {
    return isFixedHolyDay()|| isVariableHolyDay();
  }

  public boolean isFixedHolyDay() {
    return getHolyDays().contains(DateUtilities.dateToString(getTime(), "MM_dd")) ;
  }

  public boolean isVariableHolyDay() {
    return getHolyDays().contains(DateUtilities.dateToString(getTime(), "yyyy_MM_dd"));
  }


  public boolean isSaturday() {
    return (get(DAY_OF_WEEK) == SATURDAY);
  }

  public boolean isSunday() {
    return (get(DAY_OF_WEEK) == SUNDAY);
  }
  public boolean isFriday() {
    return (get(DAY_OF_WEEK) == FRIDAY);
  }

  public void moveToClosestWorkingDay() {
    moveToClosestWorkingDay(true);
  }

  public void moveToClosestWorkingDay(boolean forward) {
    while (!isWorkingDay()) {
      add(CompanyCalendar.DAY_OF_YEAR, forward ? 1 : -1);
    }
  }

  public void addWorkingDays(int wd) {
    boolean forward = wd >= 0;
    int interval = forward ? 1 : -1;
    for (int i = 0; i < Math.abs(wd); i++) {
      add(CompanyCalendar.DAY_OF_YEAR, interval);
      moveToClosestWorkingDay(forward);
    }
  }

  public void setHoliday(boolean isFixed) {
    String oc ="";
    if (isFixed)
      oc = DateUtilities.dateToString(getTime(), "MM_dd");
    else
      oc = DateUtilities.dateToString(getTime(), "yyyy_MM_dd");
    if (!getHolyDays().contains(oc))
      getHolyDays().add(oc);
  }

  public void removeHoliday() {
    getHolyDays().remove(DateUtilities.dateToString(getTime(), "MM_dd"));
    getHolyDays().remove(DateUtilities.dateToString(getTime(), "yyyy_MM_dd"));
  }

  /**
   * >0
   */
  public static int getWorkingDaysCountInPeriod(Period period, boolean onlyWorkingDaysComputation) {
    int days = 1;
    int watchDog = 5000;
    if (period.getEnd() != null && period.getStart() != null) {

      //if distance between start and end if >watchDog days returns a rough estimate
      int deltaInDays = (int)((period.getEnd().getTime() - period.getStart().getTime()) / CompanyCalendar.MILLIS_IN_DAY);
      if (deltaInDays > watchDog) {
        //compute weeks
        int weeks = (deltaInDays / 7);
        int years=(deltaInDays / 365);

        int fixHoly=0;
        for (String s:holyDays)
          fixHoly+=(s.length()>5?0:1);

        days= deltaInDays- years*fixHoly-weeks*((CompanyCalendar.FRIDAY_IS_WORKING_DAY?0:1) + (CompanyCalendar.SATURDAY_IS_WORKING_DAY?0:1) + (CompanyCalendar.SUNDAY_IS_WORKING_DAY?0:1) );
        Tracer.platformLogger.error("getWorkingDaysCountInPeriod for a period with more then "+watchDog+" days -> forcing return a rough estimate "+days+". Period id=" + period.getId() + "; time interval " + period);

      } else {

        CompanyCalendar cc = new CompanyCalendar();
        cc.setTime(period.getEnd());
        Date end = cc.setAndGetTimeToDayStart();
        cc.setTime(period.getStart());
        cc.setAndGetTimeToDayStart();
        while (cc.getTime().before(end)) {
          // only workingDays are added
          if (!onlyWorkingDaysComputation || (onlyWorkingDaysComputation && cc.isWorkingDay()))
            days++;

          cc.add(CompanyCalendar.DAY_OF_YEAR, 1);
        }
      }
    } else
      days = Integer.MAX_VALUE;
    return days;
  }

  public static int getWorkingDaysCountInPeriod(Period period) {
    return getWorkingDaysCountInPeriod(period, true);
  }


  /**
   * signed: may return negative, 0 , positive results
   */
  public static int getDistanceInWorkingDays(Date date1, Date date2) {
    Period absPeriod = new Period(Math.min(date1.getTime(), date2.getTime()), Math.max(date1.getTime(), date2.getTime()));
    int interval = getWorkingDaysCountInPeriod(absPeriod, true);
    return interval * (date1.getTime() > date2.getTime() ? -1 : (date1.getTime() < date2.getTime() ? 1 : 0));
  }

  public boolean isToday() {
    return isSameDay(new Date());
  }

  public boolean isSameDay(Date date) {
    Calendar cal = new CompanyCalendar();
    cal.setTime(date);
    if ((this.get(Calendar.YEAR) == cal.get(Calendar.YEAR)) && (this.get(Calendar.DAY_OF_YEAR) == cal.get(Calendar.DAY_OF_YEAR)))
      return true;
    else
      return false;
  }

  public static int getMillisFromMidnight(Date aTime) {
    Calendar cal = new CompanyCalendar();
    long endTime = aTime.getTime();
    cal.setTime(aTime);
    cal.set(Calendar.HOUR_OF_DAY, 0);
    cal.set(MINUTE,0);
    cal.set(SECOND,0);
    cal.set(MILLISECOND,0);
    return new Long(endTime - cal.getTime().getTime()).intValue();
  }

  public int getMillisFromMidnight() {
    final Calendar cal = new CompanyCalendar();
    cal.setTime(getTime());
    cal.set(HOUR_OF_DAY, 0);
    cal.set(MINUTE,0);
    cal.set(SECOND,0);
    cal.set(MILLISECOND,0);
    return new Long(this.getTimeInMillis() - cal.getTimeInMillis()).intValue();
  }

  public void setMillisFromMidnight(int lotOfMillis) {
    set(HOUR_OF_DAY, 0);
    set(MINUTE,0);
    set(SECOND,0);
    set(MILLISECOND,0);
    add(MILLISECOND, lotOfMillis);
  }

  /**
   * @return the easter date for the current's year
   * Gregorian
   */
  public Date getEaster() {
    int a, b, c, p, q, r;
    int year = this.get(Calendar.YEAR);
    a = year % 19;
    b = (year / 100);
    c = year % 100;
    p = ((19 * a + b - (b / 4) - ((b - ((b + 8) / 25) + 1) / 3) + 15) % 30);
    q = ((32 + 2 * ((b % 4) + (c / 4)) - p - (c % 4)) % 7);
    r = (p + q - 7 * ((a + 11 * p + 22 * q) / 451) + 114);
    CompanyCalendar cc = new CompanyCalendar();
    cc.set(Calendar.YEAR, year);
    cc.set(Calendar.MONTH, (r / 31) - 1);
    cc.set(Calendar.DATE, (r % 31) + 1);
    return cc.getTime();
  }

  public Date getDayAfterEaster() {
    if (this.getEaster() != null) {
      Calendar cc = new CompanyCalendar();
      cc.setTime(this.getEaster());
      cc.add(Calendar.DATE, 1);

      return cc.getTime();
    } else
      return null;
  }

  public boolean isEaster() {
    if (this.getEaster() != null)
      return isSameDay(this.getEaster());
    else
      return false;
  }


  public boolean isDayAfterEaster() {
    if (this.getDayAfterEaster() != null) {
      return isSameDay(this.getDayAfterEaster());
    } else
      return false;
  }

  public static SerializedList<String> getHolyDays() {
    if (holyDays == null) {
      holyDays = SerializedList.deserialize(ApplicationState.getApplicationSetting("HOLIDAYS_LIST"));
    }

    return holyDays;
  }

  /**
   * put holidays in ApplicationSettings and dump it on file
   */
  public void storeHolidays() {
    if (holyDays != null) {
      ApplicationState.applicationSettings.put("HOLIDAYS_LIST", holyDays.serialize());
      ApplicationState.dumpApplicationSettings();
    }
  }

  public static Date getFirstDayOfYear (int currentYear) {
    CompanyCalendar cal = new CompanyCalendar();
    cal.set(Calendar.YEAR, currentYear );
    cal.set(Calendar.MONTH, Calendar.JANUARY );
    cal.set(Calendar.DAY_OF_YEAR, 1 );
    return cal.setAndGetTimeToDayStart();
  }

  public static Date getLastDayOfYear(int currentYear) {
    CompanyCalendar cal = new CompanyCalendar();
    cal.set(Calendar.YEAR, currentYear );
    cal.set(Calendar.MONTH, Calendar.DECEMBER );
    cal.set(Calendar.DAY_OF_YEAR, 365 );
    return cal.setAndGetTimeToDayEnd();
  }

  /**
   * changes calenday date moving forward until it meets dayOfWeek
   * @param dayOfWeek
   */
  public void moveToNextDay(int dayOfWeek) {
    int dow = get(CompanyCalendar.DAY_OF_WEEK);
    if (dow!=dayOfWeek){
      add(CompanyCalendar.DAY_OF_YEAR,1);
      moveToNextDay(dayOfWeek);
    }
}


  public String format(String format){
    return DateUtilities.dateToString(getTime(),format);
  }

  public int getDateInt(){
    return DateUtilities.dateToInt(getTime());
  }

}
