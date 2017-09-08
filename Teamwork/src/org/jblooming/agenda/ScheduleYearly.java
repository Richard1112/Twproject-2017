package org.jblooming.agenda;

import net.sf.json.JSONObject;

import java.util.*;

import org.jblooming.utilities.DateUtilities;
import org.jblooming.waf.settings.I18n;

public class ScheduleYearly extends ScheduleSupport implements Schedule {
  private int dayOfWeek;
  private int weekOfMonth;

  public ScheduleYearly() {
  }

  public ScheduleYearly(Date start, int duration, int freq, int rep,Date endRecurr) {
    this(0, 0, start, duration, freq, rep,endRecurr);
  }

  public ScheduleYearly(int dayOfWeek, int weekOfMonth, Date start, int duration, int freq, int rep,Date endRecurr) {
    this.dayOfWeek = dayOfWeek;
    this.weekOfMonth = weekOfMonth;
    this.setStart(start);
    this.setDuration(duration);
    this.setRepeat(rep);
    this.setFreq(freq > 0 ? freq : 1);
    this.endRecurr=endRecurr;
    recalculateFields();
  }

  protected int getDayOfWeek() {
    return dayOfWeek;
  }

  protected void setDayOfWeek(int dayOfWeek) {
    this.dayOfWeek = dayOfWeek;
  }

  protected int getWeekOfMonth() {
    return weekOfMonth;
  }

  protected void setWeekOfMonth(int weekOfMonth) {
    this.weekOfMonth = weekOfMonth;
  }

  public int getDayInWeek() {
    return dayOfWeek;
  }

  public void setDayInWeek(int dayOfWeek) {
    this.dayOfWeek = dayOfWeek;
    recalculateFields();
  }

  public int getWeekInMonth() {
    return weekOfMonth;
  }

  public void setWeekInMonth(int weekOfMonth) {
    this.weekOfMonth = weekOfMonth;
    recalculateFields();
  }

  public void recalculateFields() {

    CompanyCalendar cal = new CompanyCalendar();
    cal.setTimeInMillis(this.getValidityStartTime());
    this.setStartTime(cal.getMillisFromMidnight());

    //if endRecurr is passed it is used to calculate number of repetitions
    if (endRecurr!=null)
      calculateRepetitions();


    // first check if start is congruent with
    adjustToDay(cal);
    this.setStart(cal.getTime());

    if (this.getRepeat() > 0) {
      int freq = (this.getFreq() > 0 ? this.getFreq() : 1);
      int val = (this.getRepeat() - 1) * freq;
      cal.add(CompanyCalendar.YEAR, val);
      adjustToDay(cal);
    } else {
      cal.setTime(CompanyCalendar.MAX_DATE);
      cal.set(Calendar.HOUR_OF_DAY, 0);
      cal.clear(Calendar.MINUTE);
      cal.clear(Calendar.SECOND);
      cal.clear(Calendar.MILLISECOND);
      cal.add(Calendar.MILLISECOND, this.getStartTime());
    }
    cal.add(Calendar.MILLISECOND, (int) this.getDuration());
    this.setEnd(cal.getTime());
  }


  public long getNextFireTimeAfter(long afterTime) {

    long returnTime = Long.MAX_VALUE;
    if (afterTime <= getEnd().getTime()) {
      if (afterTime > getStart().getTime()) {

        long lstart = getStart().getTime();
        CompanyCalendar cal = new CompanyCalendar();
        cal.setTimeInMillis(lstart);
        int startYear = cal.get(CompanyCalendar.YEAR);
        cal.setTimeInMillis(afterTime);
        long distInYear = cal.get(CompanyCalendar.YEAR) - startYear;

        int rep = 1;
        int freq = (this.getFreq() > 0 ? this.getFreq() : 1);
        if (distInYear > 0) {
          rep = (int) distInYear / freq;
          if ((distInYear % freq) != 0)
            rep++;
        }
        if (this.getRepeat() > 0 && rep > (this.getRepeat() - 1))
          rep = this.getRepeat() - 1;
        cal.setTime(getStart());
        cal.add(Calendar.YEAR, rep * freq);

        adjustToDay(cal);
        if (cal.getTime().getTime() <= afterTime) {
          cal.add(Calendar.YEAR, freq);
          adjustToDay(cal);         
        }
        returnTime = cal.getTimeInMillis();
      } else {
        returnTime = getStart().getTime();
      }
    }
    return returnTime;
  }

  private void adjustToDay(CompanyCalendar cal) {
    if (weekOfMonth > 0 && dayOfWeek > 0) {
      if (weekOfMonth == 5) {
        cal.set(Calendar.DAY_OF_WEEK, dayOfWeek);
        cal.set(Calendar.DAY_OF_WEEK_IN_MONTH, -1);
      } else {
        cal.set(Calendar.DAY_OF_WEEK, dayOfWeek);
        cal.set(Calendar.DAY_OF_WEEK_IN_MONTH, weekOfMonth);
      }
    }
  }

  public long getPreviousFireTimeBefore(long beforeTime) {
    long returnTime = Long.MIN_VALUE;
    if (beforeTime > getStart().getTime()) {
      if (beforeTime > getEnd().getTime())
        beforeTime = getEnd().getTime();
      long lstart = getStart().getTime();
      CompanyCalendar cal = new CompanyCalendar();
      cal.setTimeInMillis(lstart);


      int startYear = cal.get(CompanyCalendar.YEAR);
      cal.setTimeInMillis(beforeTime);
      long distInYear = cal.get(CompanyCalendar.YEAR) - startYear;


      int freq = (this.getFreq() > 0 ? this.getFreq() : 1);
      int rep = (int) distInYear / freq;
      if (this.getRepeat() == 0 || rep <= (this.getRepeat() - 1)) {
        cal.setTime(getStart());
        cal.add(Calendar.YEAR, rep * freq);
        adjustToDay(cal);
        if (cal.getTime().getTime() > beforeTime) {
          cal.add(Calendar.YEAR, -freq);
          adjustToDay(cal);
        }

        returnTime = cal.getTimeInMillis();
      }
    }
    return returnTime;
  }


  private void calculateRepetitions() {
    double howManyDays=Math.round((endRecurr.getTime()-getValidityStartDate().getTime())/CompanyCalendar.MILLIS_IN_YEAR);
    int repeat = (int) (howManyDays / getFrequency());
    setRepeat(repeat<0?1:repeat);
    endRecurr=null; // si annulla in modo da non dover ricalcolare un'altra volta
  }


  public String getName() {
    return "yearly";
  }

  public String getScheduleDescription(String useSeparator) {
    String result = "";
    result = I18n.get("SCHEDULE_YEARLY_CONTENT_%%...",
            DateUtilities.dateAndHourToString(getStartDate())+useSeparator,
            DateUtilities.dateAndHourToString(getEndDate())
    );
    return result;
  }


  public JSONObject jsonify() {
    JSONObject ret = super.jsonify();
    ret.element("type","yearly");
    ret.element("dayOfWeek",getDayOfWeek());
    ret.element("weekOfMonth",getWeekOfMonth());
    return ret;
  }

  public static ScheduleYearly fromJSON(JSONObject json){
    ScheduleYearly sd;
    if (json.get("dayOfWeek")!=null && json.get("weekOfMonth")!=null){
      sd= new ScheduleYearly(json.getInt("dayOfWeek"),json.getInt("weekOfMonth"),new Date(json.getLong("startMillis")), json.getInt("duration"), json.getInt("freq"), json.getInt("repeat"),null);
    } else {
      sd = new ScheduleYearly(new Date(json.getLong("startMillis")), json.getInt("duration"), json.getInt("freq"), json.getInt("repeat"),null);
    }

    return sd;
  }


}

