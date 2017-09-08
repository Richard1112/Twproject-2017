package org.jblooming.agenda;

import org.jblooming.utilities.DateUtilities;
import org.jblooming.waf.settings.I18n;

import java.util.*;

import net.sf.json.JSONObject;


public class ScheduleMinute extends ScheduleSupport implements Schedule {

  private boolean onlyWorkingDays = false;    // whether the event is repeated only on working days
  
  public ScheduleMinute() {
  }

  public ScheduleMinute(Date start, int duration) {
    this (start,duration,1,1);
  }

  /**
   * @param start
   * @param duration in millis
   * @param freq in minutes
   * @param rep if 0 infinite
   */
  public ScheduleMinute(Date start, int duration, int freq, int rep) {
    this.setStart(start);
    this.setDuration(duration);
    this.setFreq((freq > 0 ? freq : 1));
    this.setRepeat(rep);
    recalculateFields();
  }

  public ScheduleMinute(Date start,int startTime, int duration) {
    this (start,startTime,duration,1,1);
  }

  public ScheduleMinute(Date start, int startTime, int duration, int freq, int rep) {
    this.setStartTime(startTime);
    this.setStart(start);
    CompanyCalendar cal = new CompanyCalendar();
    cal.setTimeInMillis(this.getValidityStartTime());
    cal.setMillisFromMidnight(startTime);
    this.setStart(cal.getTime());
    this.setDuration(duration);
    this.setFreq((freq > 0 ? freq : 1));
    this.setRepeat(rep);
    recalculateFields();
  }

  public void recalculateFields() {
    CompanyCalendar cal = new CompanyCalendar();
    cal.setTimeInMillis(this.getValidityStartTime());
    this.setStartTime(cal.getMillisFromMidnight());

    if (this.getRepeat() > 0) {
      int freq = (this.getFreq()>0?this.getFreq() :1);
      int val = ((this.getRepeat() - 1) * freq);
      cal.add(Calendar.MINUTE, val);
    } else {
      this.setRepeat(0);
      cal = new CompanyCalendar();
      cal.setTime(CompanyCalendar.MAX_DATE);
      cal.set(Calendar.HOUR_OF_DAY, 0);
      cal.clear(Calendar.MINUTE);
      cal.clear(Calendar.SECOND);
      cal.clear(Calendar.MILLISECOND);
      cal.add(Calendar.MILLISECOND, this.getStartTime());
    }
    cal.add(Calendar.MILLISECOND, (int)this.getDuration());
    this.setEnd(cal.getTime());
  }

  public long getNextFireTimeAfter(long afterTime) {
    long returnTime = Long.MAX_VALUE;
    if (afterTime <= getEnd().getTime()) {
        if (afterTime > getStart().getTime()) {
        CompanyCalendar cal = new CompanyCalendar();
        long lstart = getStart().getTime();
        cal.setTimeInMillis(lstart);
        TimeZone timeZone = cal.getTimeZone();
        int offset = (timeZone.getOffset(lstart) - timeZone.getOffset(afterTime)); // questo server per calcolare i millisecondi effettivi in caso di ora legale
        long distInMillisec = afterTime - lstart - offset;
        double ddistInMinutes = (double) distInMillisec / (double) CompanyCalendar.MILLIS_IN_MINUTE;
        int distInMinutes = (int) ddistInMinutes;
        int freq = (this.getFreq()>0?this.getFreq() :1);

        int rest = freq - (int) distInMinutes % freq;

        cal.setTime(getStart());
        cal.add(Calendar.MINUTE, (int) distInMinutes + rest);

        if (isOnlyWorkingDays()) {
          while (!cal.isWorkingDay()) {
            cal.add(Calendar.DATE, 1);
          }
        }

        if (cal.getTime().getTime() <= getEnd().getTime())
          returnTime = cal.getTime().getTime() + offset;
      } else
        returnTime = getStart().getTime();
    }
    return returnTime;
  }


  public long getPreviousFireTimeBefore(long beforeTime) {
    long returnTime = Long.MIN_VALUE;
    if (beforeTime > getStart().getTime()) {
      if(beforeTime > getEnd().getTime())
        beforeTime = getEnd().getTime();
        CompanyCalendar cal = new CompanyCalendar();
        long lstart = getStart().getTime();
        cal.setTimeInMillis(lstart);
        TimeZone timeZone = cal.getTimeZone();
        int ofset = (timeZone.getOffset(lstart) - timeZone.getOffset(beforeTime)); // questo serve per calcolare i millisecondi effettivi in caso di ora legale
        long distInMillisec = beforeTime - lstart - ofset;
        double ddistInMinute = (double) distInMillisec / (double) CompanyCalendar.MILLIS_IN_MINUTE;
        int distInMinute = (int) ddistInMinute;
        int freq = (this.getFreq()>0?this.getFreq() :1);
        int rest = (int) distInMinute % freq;

        cal.setTime(getStart());
        cal.add(Calendar.MINUTE, (int) distInMinute - rest);
        returnTime = cal.getTimeInMillis();
    }
    return returnTime;
  }


  public String getName() {
      return "minutes";
  }

  public String toString() {
    return DateUtilities.dateToString(getStart(), "yyyy MM dd HH:mm:ss") + " - " + DateUtilities.dateToString(getEnd(), "yyyy MM dd HH:mm:ss");
  }


  protected boolean isOnlyWorkingDays() {
    return onlyWorkingDays;
  }

  protected void setOnlyWorkingDays(boolean onlyWorkingDays) {
    this.onlyWorkingDays = onlyWorkingDays;
  }


  public String getScheduleDescription(String useSeparator) {
    String result = "";
    result = I18n.get("SCHEDULE_MINUTE_CONTENT_%%...",
            DateUtilities.dateAndHourToString(getStartDate())+useSeparator,
            DateUtilities.dateAndHourToString(getEndDate())+useSeparator,
            getFrequency() + ""+useSeparator,
            getRepetitions() + ""
    );
    return result;
  }


  public JSONObject jsonify() {
    JSONObject ret = super.jsonify();
    ret.element("type","minute");
    ret.element("onlyWorkingDays", isOnlyWorkingDays());
    return ret;
  }


  public static ScheduleMinute fromJSON(JSONObject json){
    ScheduleMinute sm = new ScheduleMinute(new Date(json.getLong("startMillis")), json.getInt("duration"), json.getInt("freq"), json.getInt("repeat"));
    sm.setOnlyWorkingDays(json.getBoolean("onlyWorkingDays"));
    return sm;
  }


}

