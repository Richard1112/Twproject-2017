package org.jblooming.utilities;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import sun.text.resources.FormatData;
import sun.util.resources.LocaleData;

import java.lang.reflect.Method;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * This class contains some methods to manage dates
 *
 * @author Silvia Chelazzi & Roberto Bicchierai & Pietro Polsinelli
 * @since JDK 1.4
 *        <p/>
 *        it uses the standard date format:
 *        G  Era designator  Text  AD
 *        y  Year  Year  1996; 96
 *        M  Month in year  Month  July; Jul; 07
 *        w  Week in year  Number  27
 *        W  Week in month  Number  2
 *        D  Day in year  Number  189
 *        d  Day in month  Number  10
 *        F  Day of week in month  Number  2
 *        E  Day in week  Text  Tuesday; Tue
 *        a  Am/pm marker  Text  PM
 *        H  Hour in day (0-23)  Number  0
 *        k  Hour in day (1-24)  Number  24
 *        K  Hour in am/pm (0-11)  Number  0
 *        h  Hour in am/pm (1-12)  Number  12
 *        m  Minute in hour  Number  30
 *        s  Second in minute  Number  55
 *        S  Millisecond  Number  978
 *        z  Time zone  General time zone  Pacific Standard Time; PST; GMT-08:00
 *        Z  Time zone  RFC 822 time zone  -0800
 */
public class DateUtilities {

  public static String DAY_SHORT_CODE = "d.";


  public static int TIME_FULL = 0;
  public static int TIME_LONG = 1;
  public static int TIME_MEDIUM = 2;
  public static int TIME_SHORT = 3;
  public static int DATE_FULL = 4;
  public static int DATE_LONG = 5;
  public static int DATE_MEDIUM = 6;
  public static int DATE_SHORT = 7;

  public static int DATE_DEFAULT = DATE_SHORT;
  public static int TIME_DEFAULT = TIME_SHORT;


  /**
   * Method dateToString
   * returns a String representing the passed date in the default format.
   *
   * @param date a  Date
   * @return a String
   */
  public static String dateToString(Date date) {
    return dateToString(date, null);
  }

  public static String dateToFullString(Date date) {
    return dateToString(date, DateUtilities.getFormat(DateUtilities.DATE_FULL));
  }


  /**
   * Method dateToString
   * returns a String representing the passed date. The format is used to parse.
   *
   * @param date   a  Date
   * @param format a  String if null the deafault is used
   * @return a String
   */
  public static String dateToString(Date date, String format) {
    if (date == null)
      return "";
    if (!JSP.ex(format))
      format = DateUtilities.getFormat(DATE_DEFAULT);
    SimpleDateFormat sdf = new SimpleDateFormat(format, SessionState.getLocale());
    sdf.setTimeZone(SessionState.getTimeZone());
    return sdf.format(date);
  }

  public static String dateAndHourToString(Date date) {
    if (date == null)
      return "";
    return dateToString(date) + " " + dateToHourMinutes(date);
  }

  public static String dateAndHourToFullString(Date date) {
    if (date == null)
      return "";
    return dateToString(date, DateUtilities.getFormat(DateUtilities.DATE_FULL)) + " " + dateToHourMinutes(date);
  }

  public static String dateToHourMinutes(Date date) {
    return dateToString(date, DateUtilities.getFormat(TIME_DEFAULT));
  }


  public static String getMillisInHoursMinutes(Number millis) {
    return getMillisInHoursMinutes(millis,"0","00");
  }
  public static String getMillisInHoursMinutes(Number millis,String hourPattern,String minutesPattern) {
    int hour = (int) (millis.longValue() / CompanyCalendar.MILLIS_IN_HOUR);
    int min = (int) ((millis.longValue() % CompanyCalendar.MILLIS_IN_HOUR) / CompanyCalendar.MILLIS_IN_MINUTE);
    min = Math.abs(min);
    DecimalFormat df = (DecimalFormat) DecimalFormat.getInstance();
    df.applyPattern(hourPattern);
    String ret = df.format(hour) + ":";
    df.applyPattern(minutesPattern);
    ret+= df.format(min);
    return ret;
  }

  public static String getMillisInHoursMinutesSeconds(Number millis) {
    int hour = (int) (millis.longValue() / CompanyCalendar.MILLIS_IN_HOUR);
    long dif = (millis.longValue() % CompanyCalendar.MILLIS_IN_HOUR);
    int min = (int) (dif / CompanyCalendar.MILLIS_IN_MINUTE);
    int sec = (int) ((dif % CompanyCalendar.MILLIS_IN_MINUTE) / 1000);
    DecimalFormat df = (DecimalFormat) DecimalFormat.getInstance();
    df.applyPattern("00");
    return hour + ":" + df.format(min) + ":" + df.format(sec);
  }

  public static String getMillisInHours(Number millis) {
    int hour = (int) (millis.longValue() / CompanyCalendar.MILLIS_IN_HOUR);
    return hour + "";
  }

  public static String getMillisInDays(Number millis) {
    int days = (int) (millis.longValue() / CompanyCalendar.MILLIS_IN_DAY);
    return days + "";
  }

  public static String getMillisInDaysHoursMinutes(Number millis) {

    int days = (int) (millis.longValue() / CompanyCalendar.MILLIS_IN_DAY);
    int hour = (int) ((millis.longValue() % CompanyCalendar.MILLIS_IN_DAY) / CompanyCalendar.MILLIS_IN_HOUR);
    int min = (int) ((millis.longValue() % CompanyCalendar.MILLIS_IN_HOUR) / CompanyCalendar.MILLIS_IN_MINUTE);
    min = Math.abs(min); // wtf? why abs?
    DecimalFormat df = (DecimalFormat) DecimalFormat.getInstance();
    df.applyPattern("00");
    return (days > 0 ? days + "D  " : "") + hour + ":" + df.format(min);
  }


  public static String getMillisInDaysWorkHoursMinutes(Number millis) {


    int days = (int) (millis.longValue() / (CompanyCalendar.MILLIS_IN_WORKING_DAY>=0?CompanyCalendar.MILLIS_IN_WORKING_DAY:CompanyCalendar.MILLIS_IN_HOUR*8));
    long hour = millis.longValue() % CompanyCalendar.MILLIS_IN_WORKING_DAY;
    DecimalFormat df = (DecimalFormat) DecimalFormat.getInstance();
    df.applyPattern("00");

    return days + DAY_SHORT_CODE + getMillisInHoursMinutes(hour);//hour + ":" + df.format(min);
  }


  /**
   * @param date
   * @return a string with relative time e.g.: "just now" "some minutes ago" "last week" etc.
   */
  public static String dateToRelative(Date date) {
    if (date == null)
      return "";

    String ret = "";
    double diff = System.currentTimeMillis() - date.getTime();


    if (diff < -CompanyCalendar.MILLIS_IN_YEAR * 2)
      ret = I18n.get("DATEREL_IN_%%_YEARS", Math.round (-diff / CompanyCalendar.MILLIS_IN_YEAR) + "");

    else if (diff < -CompanyCalendar.MILLIS_IN_MONTH * 2)
      ret = I18n.get("DATEREL_IN_%%_MONTHS", Math.round (-diff / CompanyCalendar.MILLIS_IN_MONTH) + "");

    else if (diff < -CompanyCalendar.MILLIS_IN_WEEK * 2)
      ret = I18n.get("DATEREL_IN_%%_WEEKS", Math.round (-diff / CompanyCalendar.MILLIS_IN_WEEK) + "");

    else if (diff < -CompanyCalendar.MILLIS_IN_DAY * 2)
      ret = I18n.get("DATEREL_IN_%%_DAYS", Math.round (-diff / CompanyCalendar.MILLIS_IN_DAY) + "");

    else if (diff < -CompanyCalendar.MILLIS_IN_HOUR * 2)
      ret = I18n.get("DATEREL_IN_%%_HOURS", Math.round (-diff / CompanyCalendar.MILLIS_IN_HOUR) + "");

    else if (diff < -CompanyCalendar.MILLIS_IN_MINUTE * 60)
      ret = I18n.get("DATEREL_IN_ABOUT_1_HOUR");

    else if (diff < -CompanyCalendar.MILLIS_IN_MINUTE * 35)
      ret = I18n.get("DATEREL_IN_ABOUT_HALF_HOUR");

    else if (diff < -CompanyCalendar.MILLIS_IN_MINUTE * 15)
      ret = I18n.get("DATEREL_IN_SOME_MINUTES");

    else if (diff < -CompanyCalendar.MILLIS_IN_MINUTE * 5)
      ret = I18n.get("DATEREL_IN_FEW_MINUTES");

    else if (diff <= CompanyCalendar.MILLIS_IN_MINUTE)
      ret = I18n.get("DATEREL_JUSTNOW");

    else if (diff <= CompanyCalendar.MILLIS_IN_MINUTE * 5)
      ret = I18n.get("DATEREL_FEW_MINUTES_AGO");

    else if (diff <= CompanyCalendar.MILLIS_IN_MINUTE * 15)
      ret = I18n.get("DATEREL_SOME_MINUTES_AGO");

    else if (diff <= CompanyCalendar.MILLIS_IN_MINUTE * 35)
      ret = I18n.get("DATEREL_ABOUT_HALF_HOUR_AGO");

    else if (diff <= CompanyCalendar.MILLIS_IN_MINUTE * 75)
      ret = I18n.get("DATEREL_ABOUT_1_HOUR_AGO");

    else if (diff <= CompanyCalendar.MILLIS_IN_HOUR * 5)
      ret = I18n.get("DATEREL_FEW_HOURS_AGO");

    else if (diff <= CompanyCalendar.MILLIS_IN_HOUR * 24)
      ret = I18n.get("DATEREL_%%_HOURS_AGO", Math.round (diff / CompanyCalendar.MILLIS_IN_HOUR) + "");

    else if (diff <= CompanyCalendar.MILLIS_IN_DAY * 7)
      ret = I18n.get("DATEREL_%%_DAYS_AGO", Math.round (diff / CompanyCalendar.MILLIS_IN_DAY) + "");

    else if (diff <= CompanyCalendar.MILLIS_IN_WEEK * 5)
      ret = I18n.get("DATEREL_%%_WEEKS_AGO", Math.round (diff / CompanyCalendar.MILLIS_IN_WEEK) + "");

    else if (diff <= CompanyCalendar.MILLIS_IN_MONTH * 12)
      ret = I18n.get("DATEREL_%%_MONTHS_AGO", Math.round (diff / CompanyCalendar.MILLIS_IN_MONTH) + "");

    else
      ret = I18n.get("DATEREL_%%_YEARS_AGO", Math.round (diff / CompanyCalendar.MILLIS_IN_YEAR) + "");

    return ret;
  }


  public static Date dateFromString(String s) throws ParseException {
    return dateFromString(s, null);
  }

  /**
   * Method dateFromString
   * returns a date parsing the passed string in the specified format.
   *
   * @param dateString a  String
   * @return a Date
   * @throws ParseException
   */

  public static Date dateFromString(String dateString, String format) throws ParseException {

    if (dateString == null)
      return null;

    if (!JSP.ex(format))
      format = DateUtilities.getFormat(DATE_DEFAULT);

    CompanyCalendar cc = new CompanyCalendar(SessionState.getLocale());
    cc.setAndGetTimeToDayStart();

    cc.setTimeZone(SessionState.getTimeZone());

    String dateUpper = dateString.toUpperCase();
    if (shortDatesAliases.get(shortDates.NOW).contains(dateUpper)) {
      return new Date();

    } else if (shortDatesAliases.get(shortDates.TODAY).contains(dateUpper)) {
      return cc.setAndGetTimeToDayStart();

    } else if (shortDatesAliases.get(shortDates.YESTERDAY).contains(dateUpper)) {
      cc.add(CompanyCalendar.DATE, -1);
      return cc.getTime();

    } else if (shortDatesAliases.get(shortDates.TOMORROW).contains(dateUpper)) {
      cc.add(CompanyCalendar.DATE, 1);
      return cc.getTime();

    } else if (shortDatesAliases.get(shortDates.THISWEEKSTART).contains(dateUpper)) {
      cc.set(CompanyCalendar.DAY_OF_WEEK, cc.getFirstDayOfWeek());
      return cc.getTime();

    } else if (shortDatesAliases.get(shortDates.LASTWEEKSTART).contains(dateUpper)) {
      cc.set(CompanyCalendar.DAY_OF_WEEK, cc.getFirstDayOfWeek());
      cc.add(CompanyCalendar.WEEK_OF_YEAR, -1);
      return cc.getTime();

    } else if (shortDatesAliases.get(shortDates.NEXTWEEKSTART).contains(dateUpper)) {
      cc.set(CompanyCalendar.DAY_OF_WEEK, cc.getFirstDayOfWeek());
      cc.add(CompanyCalendar.WEEK_OF_YEAR, 1);
      return cc.getTime();

    } else if (shortDatesAliases.get(shortDates.THISMONTHSTART).contains(dateUpper)) {
      cc.set(CompanyCalendar.DAY_OF_MONTH, 1);
      return cc.getTime();

    } else if (shortDatesAliases.get(shortDates.LASTMONTHSTART).contains(dateUpper)) {
      cc.set(CompanyCalendar.DAY_OF_MONTH, 1);
      cc.add(CompanyCalendar.MONTH, -1);
      return cc.getTime();

    } else if (shortDatesAliases.get(shortDates.NEXTMONTHSTART).contains(dateUpper)) {
      cc.set(CompanyCalendar.DAY_OF_MONTH, 1);
      cc.add(CompanyCalendar.MONTH, 1);
      return cc.getTime();

    } else if (shortDatesAliases.get(shortDates.THISQUARTERSTART).contains(dateUpper)) {
      cc.set(CompanyCalendar.DAY_OF_MONTH, 1);
      cc.set(CompanyCalendar.MONTH, ((int) (cc.get(CompanyCalendar.MONTH) / 3)) * 3);
      return cc.getTime();

    } else if (shortDatesAliases.get(shortDates.LASTQUARTERSTART).contains(dateUpper)) {
      cc.set(CompanyCalendar.DAY_OF_MONTH, 1);
      cc.set(CompanyCalendar.MONTH, ((int) (cc.get(CompanyCalendar.MONTH) / 3)) * 3);
      cc.add(CompanyCalendar.MONTH, -3);
      return cc.getTime();

    } else if (shortDatesAliases.get(shortDates.NEXTQUARTERSTART).contains(dateUpper)) {
      cc.set(CompanyCalendar.DAY_OF_MONTH, 1);
      cc.set(CompanyCalendar.MONTH, ((int) (cc.get(CompanyCalendar.MONTH) / 3)) * 3);
      cc.add(CompanyCalendar.MONTH, 3);
      return cc.getTime();

    } else if (Pattern.matches("^-?[0-9]+[DWMY]$", dateUpper)) {
      String lastOne = dateUpper.substring(dateUpper.length() - 1);
      int val = Integer.parseInt(dateUpper.substring(0, dateUpper.length() - 1));
      int field = CompanyCalendar.DAY_OF_MONTH;
      if (lastOne.equals("W"))
        field = CompanyCalendar.WEEK_OF_YEAR;
      else if (lastOne.equals("M"))
        field = CompanyCalendar.MONTH;
      else if (lastOne.equals("Y"))
        field = CompanyCalendar.YEAR;

      cc.add(field, val);
      return cc.getTime();

      // check for calendarWeek CW32
    } else {
      for (String pat : shortDatesAliases.get(shortDates.CALENDARWEEK)) {
        if (dateUpper.startsWith(pat) && dateUpper.length() > pat.length()) {
          int val = Integer.parseInt(dateUpper.substring(pat.length(), dateUpper.length()));
          cc.set(CompanyCalendar.DAY_OF_WEEK, cc.getFirstDayOfWeek());
          cc.set(CompanyCalendar.WEEK_OF_YEAR, val);
          return cc.getTime();
        }
      }
    }


    SimpleDateFormat sdf = new SimpleDateFormat(format, SessionState.getLocale());
    sdf.setLenient(true);
    sdf.setTimeZone(SessionState.getTimeZone());

    cc.setTime(sdf.parse(dateString));
    int y = cc.get(CompanyCalendar.YEAR);
    if (y < 49)
      cc.add(CompanyCalendar.YEAR, 2000);
    else if (y < 100)
      cc.add(CompanyCalendar.YEAR, 1900);

    return cc.getTime();
  }


  public static enum shortDates {
    LASTQUARTERSTART,
    LASTMONTHSTART,
    THISMONTHSTART,
    LASTWEEKSTART,
    THISWEEKSTART,
    YESTERDAY,
    TODAY,
    NOW,
    TOMORROW,
    NEXTWEEKSTART,
    NEXTMONTHSTART,
    THISQUARTERSTART,
    NEXTQUARTERSTART,
    CALENDARWEEK
  }

  public static Map<shortDates, List<String>> shortDatesAliases = getShortDatesAliases();

  private static Map<shortDates, List<String>> getShortDatesAliases() {
    Map<shortDates, List<String>> sas = new HashTable();

    sas.put(shortDates.NOW, CollectionUtilities.toList("N", shortDates.NOW.toString()));

    sas.put(shortDates.TODAY, CollectionUtilities.toList("T", shortDates.TODAY.toString()));
    sas.put(shortDates.YESTERDAY, CollectionUtilities.toList("Y", shortDates.YESTERDAY.toString()));
    sas.put(shortDates.TOMORROW, CollectionUtilities.toList("TO", shortDates.TOMORROW.toString()));

    sas.put(shortDates.THISWEEKSTART, CollectionUtilities.toList("W", "TW", "WEEK", "THISWEEK", "WEEKSTART", shortDates.THISWEEKSTART.toString()));
    sas.put(shortDates.LASTWEEKSTART, CollectionUtilities.toList("LW", "LASTWEEK", shortDates.LASTWEEKSTART.toString()));
    sas.put(shortDates.NEXTWEEKSTART, CollectionUtilities.toList("NW", "NEXTWEEK", shortDates.NEXTWEEKSTART.toString()));

    sas.put(shortDates.THISMONTHSTART, CollectionUtilities.toList("M", "TM", "MONTH", "THISMONTH", "MONTHSTART", shortDates.THISMONTHSTART.toString()));
    sas.put(shortDates.LASTMONTHSTART, CollectionUtilities.toList("LM", "LASTMONTH", shortDates.LASTMONTHSTART.toString()));
    sas.put(shortDates.NEXTMONTHSTART, CollectionUtilities.toList("NM", "NEXTMONTH", shortDates.NEXTMONTHSTART.toString()));

    sas.put(shortDates.THISQUARTERSTART, CollectionUtilities.toList("Q", "TQ", "QUARTER", "THISQUARTER", "QUARTERSTART", shortDates.THISQUARTERSTART.toString()));
    sas.put(shortDates.LASTQUARTERSTART, CollectionUtilities.toList("LQ", "LASTQUARTER", shortDates.LASTQUARTERSTART.toString()));
    sas.put(shortDates.NEXTQUARTERSTART, CollectionUtilities.toList("NQ", "NEXTQUARTER", shortDates.NEXTQUARTERSTART.toString()));

    sas.put(shortDates.CALENDARWEEK, CollectionUtilities.toList("CW", "WEEKOFYEAR", "WY", shortDates.CALENDARWEEK.toString()));

    return sas;
  }

  public static String qbeIntervalFromString(String dateQbeString) {
    CompanyCalendar cc = new CompanyCalendar();
    String dateUpper = dateQbeString.toUpperCase();
    String ret = "";

    if (shortDatesAliases.get(shortDates.THISWEEKSTART).contains(dateUpper)) {
      cc.setAndGetTimeToDayStart();
      cc.set(CompanyCalendar.DAY_OF_WEEK, cc.getFirstDayOfWeek());
      ret = dateToString(cc.getTime()) + ":";
      cc.add(CompanyCalendar.DATE, 6);
      return ret + dateToString(cc.setAndGetTimeToDayEnd());

    } else if (shortDatesAliases.get(shortDates.LASTWEEKSTART).contains(dateUpper)) {
      cc.setAndGetTimeToDayStart();
      cc.set(CompanyCalendar.DAY_OF_WEEK, cc.getFirstDayOfWeek());
      cc.add(CompanyCalendar.DATE, -7);
      ret = dateToString(cc.getTime()) + ":";
      cc.add(CompanyCalendar.DATE, 6);
      return ret + dateToString(cc.setAndGetTimeToDayEnd());

    } else if (shortDatesAliases.get(shortDates.NEXTWEEKSTART).contains(dateUpper)) {
      cc.setAndGetTimeToDayStart();
      cc.set(CompanyCalendar.DAY_OF_WEEK, cc.getFirstDayOfWeek());
      cc.add(CompanyCalendar.DATE, 7);
      ret = dateToString(cc.getTime()) + ":";
      cc.add(CompanyCalendar.DATE, 6);
      return ret + dateToString(cc.setAndGetTimeToDayEnd());

    } else if (shortDatesAliases.get(shortDates.THISMONTHSTART).contains(dateUpper)) {
      cc.setAndGetTimeToDayStart();
      cc.set(CompanyCalendar.DAY_OF_MONTH, 1);
      ret = dateToString(cc.getTime()) + ":";
      cc.add(CompanyCalendar.MONTH, 1);
      cc.add(CompanyCalendar.DAY_OF_MONTH, -1);
      return ret + dateToString(cc.setAndGetTimeToDayEnd());

    } else if (shortDatesAliases.get(shortDates.LASTMONTHSTART).contains(dateUpper)) {
      cc.setAndGetTimeToDayStart();
      cc.set(CompanyCalendar.DAY_OF_MONTH, 1);
      cc.add(CompanyCalendar.MONTH, -1);
      ret = dateToString(cc.getTime()) + ":";
      cc.add(CompanyCalendar.MONTH, 1);
      cc.add(CompanyCalendar.DAY_OF_MONTH, -1);
      return ret + dateToString(cc.setAndGetTimeToDayEnd());

    } else if (shortDatesAliases.get(shortDates.NEXTMONTHSTART).contains(dateUpper)) {
      cc.setAndGetTimeToDayStart();
      cc.set(CompanyCalendar.DAY_OF_MONTH, 1);
      cc.add(CompanyCalendar.MONTH, 1);
      ret = dateToString(cc.getTime()) + ":";
      cc.add(CompanyCalendar.MONTH, 1);
      cc.add(CompanyCalendar.DAY_OF_MONTH, -1);
      return ret + dateToString(cc.setAndGetTimeToDayEnd());

    } else if (shortDatesAliases.get(shortDates.THISQUARTERSTART).contains(dateUpper)) {
      cc.setAndGetTimeToDayStart();
      cc.set(CompanyCalendar.DAY_OF_MONTH, 1);
      cc.set(CompanyCalendar.MONTH, ((int) (cc.get(CompanyCalendar.MONTH) / 3)) * 3);
      ret = dateToString(cc.getTime()) + ":";
      cc.add(CompanyCalendar.MONTH, 3);
      cc.add(CompanyCalendar.DAY_OF_MONTH, -1);
      return ret + dateToString(cc.setAndGetTimeToDayEnd());

    } else if (shortDatesAliases.get(shortDates.LASTQUARTERSTART).contains(dateUpper)) {
      cc.setAndGetTimeToDayStart();
      cc.set(CompanyCalendar.DAY_OF_MONTH, 1);
      cc.set(CompanyCalendar.MONTH, ((int) (cc.get(CompanyCalendar.MONTH) / 3)) * 3);
      cc.add(CompanyCalendar.MONTH, -3);
      ret = dateToString(cc.getTime()) + ":";
      cc.add(CompanyCalendar.MONTH, 3);
      cc.add(CompanyCalendar.DAY_OF_MONTH, -1);
      return ret + dateToString(cc.setAndGetTimeToDayEnd());

    } else if (shortDatesAliases.get(shortDates.NEXTQUARTERSTART).contains(dateUpper)) {
      cc.setAndGetTimeToDayStart();
      cc.set(CompanyCalendar.DAY_OF_MONTH, 1);
      cc.set(CompanyCalendar.MONTH, ((int) (cc.get(CompanyCalendar.MONTH) / 3)) * 3);
      cc.add(CompanyCalendar.MONTH, 3);
      ret = dateToString(cc.getTime()) + ":";
      cc.add(CompanyCalendar.MONTH, 3);
      cc.add(CompanyCalendar.DAY_OF_MONTH, -1);
      return ret + dateToString(cc.setAndGetTimeToDayEnd());

      // check for calendarWeek CW32
    } else {
      for (String pat : shortDatesAliases.get(shortDates.CALENDARWEEK)) {
        if (dateUpper.startsWith(pat) && dateUpper.length() > pat.length()) {
          int val = Integer.parseInt(dateUpper.substring(pat.length(), dateUpper.length()));
          cc.setAndGetTimeToDayStart();
          cc.set(CompanyCalendar.DAY_OF_WEEK, cc.getFirstDayOfWeek());
          cc.set(CompanyCalendar.WEEK_OF_YEAR, val);
          ret = dateToString(cc.getTime()) + ":";
          cc.add(CompanyCalendar.DATE, 6);
          return ret + dateToString(cc.setAndGetTimeToDayEnd());
        }
      }
    }

    return dateQbeString;
  }


  /**
   * @param value
   * @return a long value representing in millisecond the string. All this format are valid: "12:58" "13.75"  "63635676000" (this is already in milliseconds)
   * @throws NumberFormatException
   */
  public static long millisFromHourMinuteSmart(String value) throws NumberFormatException {
    long result = 0;
    if (value == null)
      throw new NumberFormatException("Null value not allowed");

    value = value.replace(',', '.'); // Double.parseDouble always parse from 123.45 not from 123,45

    int semiColSeparator = value.indexOf(":");
    int dotSeparator = value.indexOf(".");

    if (semiColSeparator < 0 && dotSeparator < 0 && value.length() > 5) {
      return Long.parseLong(value);
    } else {


      if (dotSeparator > -1) {
        double d = 0;
        //try {
        d = Double.parseDouble(value);
        //  d= NumberFormat.getInstance().parse(value).doubleValue();
        //} catch (ParseException e) {
        //  throw new NumberFormatException (e.getMessage());
        //}
        result = (long) (d * CompanyCalendar.MILLIS_IN_HOUR);

      } else {

        long hour = 0;
        long minute = 0;
        if (semiColSeparator == -1)
          hour = Long.parseLong(value);
        else {
          hour = JSP.ex(value.substring(0, semiColSeparator)) ? Long.parseLong(value.substring(0, semiColSeparator)) : 0;
          minute = Long.parseLong(value.substring(semiColSeparator + 1));
        }
        result = hour * CompanyCalendar.MILLIS_IN_HOUR + minute * CompanyCalendar.MILLIS_IN_MINUTE;
      }
      return result;
    }
  }


  /**
   * @param string              "3y 4d", "4D:08:10", "12M/3d", "2H4D", "3M4d,2h", "12:30", "11", "3", "1.5", "2m/3D", "12/3d", "1234"
   *                            by default 2 means 2 hours 1.5 means 1:30
   * @param considerWorkingdays if true day length is from global.properties CompanyCalendar.MILLIS_IN_WORKING_DAY  otherwise in 24
   * @return milliseconds. 0 if invalid string
   */
  public static long millisFromString(String string, boolean considerWorkingdays) throws NumberFormatException {
    if (string == null)
      throw new NumberFormatException("Null value not allowed");

    //String ps = "(\\d+[Yy])|(\\d+[M])|(\\d+[Ww])|(\\d+[Dd])|(\\d+[Hh])|(\\d+[m])|(\\d+[Ss])|(\\d+:\\d+)|(:\\d+)|(\\d*[\\.,]\\d+)|(\\d+)";
    String ps = "([0-9\\.,]+[Yy])|([0-9\\.,]+[M])|([0-9\\.,]+[Ww])|([0-9\\.,]+[Dd])|([0-9\\.,]+[Hh])|([0-9\\.,]+[m])|([0-9\\.,]+[Ss])|(\\d+:\\d+)|(:\\d+)|(\\d*[\\.,]\\d+)|(\\d+)";


    Pattern pattern = Pattern.compile(ps);
    Matcher matcher = pattern.matcher(string);
    double totMillis = 0;

    if (!matcher.find())
      throw new NumberFormatException("Invalid format: \"" + string + "\". Pattern allowed:\"" + ps + "\"");
    matcher.reset();

    while (matcher.find()) {
      int gcount = matcher.groupCount();
      for (int i = 1; i <= gcount; i++) {
        String match = matcher.group(i);
        if (match != null) {
          //long number = 0;
          double number = 0;
          try {
            number = DecimalFormat.getInstance().parse(match.replace(',','.')).doubleValue();
          } catch (ParseException e) {
          }
          if (i == 1) { // years
            totMillis = totMillis + number * (considerWorkingdays ? CompanyCalendar.MILLIS_IN_WORKING_DAY * CompanyCalendar.WORKING_DAYS_PER_WEEK * 52 : CompanyCalendar.MILLIS_IN_YEAR);
          } else if (i == 2) { // months
            totMillis = totMillis + number * (considerWorkingdays ? CompanyCalendar.MILLIS_IN_WORKING_DAY * CompanyCalendar.WORKING_DAYS_PER_WEEK * 4 : CompanyCalendar.MILLIS_IN_MONTH);
          } else if (i == 3) { // weeks
            totMillis = totMillis + number * (considerWorkingdays ? CompanyCalendar.MILLIS_IN_WORKING_DAY * CompanyCalendar.WORKING_DAYS_PER_WEEK : CompanyCalendar.MILLIS_IN_WEEK);
          } else if (i == 4) { // days
            totMillis = totMillis + number * (considerWorkingdays ? CompanyCalendar.MILLIS_IN_WORKING_DAY : CompanyCalendar.MILLIS_IN_DAY);
          } else if (i == 5) { // hours
            totMillis = totMillis + number * CompanyCalendar.MILLIS_IN_HOUR;
          } else if (i == 6) { // minutes
            totMillis = totMillis + number * CompanyCalendar.MILLIS_IN_MINUTE;
          } else if (i == 7) { // seconds
            totMillis = totMillis + number * 1000;
          } else if (i == 8) { // hour:minutes
            totMillis = totMillis + DateUtilities.millisFromHourMinuteSmart(match);
          } else if (i == 9) { // :minutes
            totMillis = totMillis + DateUtilities.millisFromHourMinuteSmart(match);
          } else if (i == 10) { // hour.minutes
            totMillis = totMillis + DateUtilities.millisFromHourMinuteSmart(match);
          } else if (i == 11) { // hours
            totMillis = totMillis + number * CompanyCalendar.MILLIS_IN_HOUR;
          }
          break;
        }
      }
    }
    return (long)totMillis;
  }


  /**
   * @param string              "3y 4d", "4D:08:10", "12M/3d", "2H4D", "3M4d,2h", "11", "3", "1.5", "2m/3D", "12/3d", "1234"
   *                            by default 2 means 2 hours 1.5 means 1:30
   * @param considerWorkingdays if true day length is from global.properties CompanyCalendar.MILLIS_IN_WORKING_DAY  otherwise in 24
   * @return milliseconds. 0 if invalid string
   */
  public static int daysFromString(String string, boolean considerWorkingdays) throws NumberFormatException {
    if (string == null)
      throw new NumberFormatException("Null value not allowed");
    string = string.replace(',', '.');
    //String ps = "(\\d+[Yy])|(\\d+[Mm])|(\\d+[Ww])|(\\d+[Dd])|(\\d*[\\.,]\\d+)|(\\d+)";
    String ps = "([0-9\\.,]+[Yy])|([0-9\\.,]+[Mm])|([0-9\\.,]+[Ww])|([0-9\\.,]+[Dd])|(\\d*[\\.,]\\d+)|(\\d+)";


    Pattern pattern = Pattern.compile(ps);
    Matcher matcher = pattern.matcher(string);
    double totDays = 0;

    if (!matcher.find())
      throw new NumberFormatException("Invalid format: \"" + string + "\". Pattern allowed:\"" + ps + "\"");
    matcher.reset();

    while (matcher.find()) {
      int gcount = matcher.groupCount();
      for (int i = 1; i <= gcount; i++) {
        String match = matcher.group(i);
        if (match != null) {
          //int number=0;
          double number=0.0;
           try {
             number = DecimalFormat.getInstance().parse(match.replace(',','.')).doubleValue();
           } catch (ParseException e) {
          }
          if (i == 1) { // years
            totDays = totDays + number * (considerWorkingdays ? CompanyCalendar.WORKING_DAYS_PER_WEEK * 52 : 365);
          } else if (i == 2) { // months
            totDays = totDays + number * (considerWorkingdays ? CompanyCalendar.WORKING_DAYS_PER_WEEK * 4 : 30);
          } else if (i == 3) { // weeks
            totDays = totDays + number * (considerWorkingdays ? CompanyCalendar.WORKING_DAYS_PER_WEEK : 7);
          } else if (i == 4) { // days with D
            totDays = totDays + number;
          } else if (i == 5) { // days decimal
            totDays = totDays + number;
          } else if (i == 6) { // days
            totDays = totDays + number;
          }
          break;
        }
      }
    }
    return (int)totDays;
  }


  public static int dateToInt(Date date) {
    return Integer.parseInt(dateToString(date, "yyyyMMdd"));
  }

  public static Date intToDate(int dateint)  {
    try {
      return dateFromString(StringUtils.right("00000000" + dateint,8) , "yyyyMMdd");
    } catch (ParseException e) {
      throw  new PlatformRuntimeException(e);
    }
  }


  public static String getFormat(int field) {
    String ret = null;
    SessionState ss = SessionState.getCurrentSessionState();
    if (ss != null)
      ret = ss.getLocalizedDateFormat(field);
    else
      ret = ApplicationState.getSystemLocalizedDateFormat(field);
    return ret;
  }


  public static String[] getLocalizedDateFormats(Locale locale) {


    String[] localizedDateFormats=new String[]{"HH:mm:ss z","HH:mm:ss z","HH:mm:ss","HH:mm","EEEE d MMMM yyyy","dd MMMM yyyy","dd-MMM-yyyy","dd/MM/yyyy","{1} {0}"};

    //nella 1.7 i formati sono in un solo array, nella 1.8 sono su due array
    if (System.getProperty("java.version").startsWith("1.7")) {
      Method getBundle = null;
      try {
        getBundle = ResourceBundle.class.getMethod("getBundle", String.class, Locale.class);
      ResourceBundle data = (ResourceBundle) getBundle.invoke(LocaleData.class,FormatData.class.getName(),locale);
      localizedDateFormats = data.getStringArray("DateTimePatterns");
      } catch (Exception e) {
      }

    } else  if (System.getProperty("java.version").startsWith("1.8")) {
      try {
      Method getBundle = LocaleData.class.getMethod("getBundle", String.class, Locale.class);
      ResourceBundle data = (ResourceBundle) getBundle.invoke(LocaleData.class,FormatData.class.getName(), locale);
      localizedDateFormats=(String[]) ArrayUtils.addAll(data.getStringArray("TimePatterns"), data.getStringArray("DatePatterns"));
      } catch (Exception e) {
      }
    }

    //String[] localizedDateFormats = LocaleData.getLocaleElements(locale).getStringArray("DateTimePatterns"); // for jdk 5

    // hack here for transforming short year from yy to yyyy
    localizedDateFormats[7] = localizedDateFormats[7].replaceAll("y{2,4}", "yyyy");

    return localizedDateFormats;
  }

}

