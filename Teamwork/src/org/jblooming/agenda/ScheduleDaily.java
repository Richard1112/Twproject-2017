package org.jblooming.agenda;

import net.sf.json.JSONObject;

import java.util.*;

import org.jblooming.waf.settings.I18n;
import org.jblooming.utilities.DateUtilities;


public class ScheduleDaily extends ScheduleSupport implements Schedule {

  private boolean onlyWorkingDays = false;    // whether the event is repeated only on working days  

  public ScheduleDaily() {
  }

  public ScheduleDaily(Date start, int duration) {
    this (start,duration,1,1);
  }


  public ScheduleDaily(Date start, int duration, int freq, int rep) {
    this ( start,  duration,  freq > 0 ? freq : 1,  rep,false, null);
  }


  public ScheduleDaily(Date start, int duration, int freq, int rep, boolean onlyWorkingDays, Date endRecur) {
    this.setStart(start);
    this.setDuration(duration);
    this.setFreq((freq > 0 ? freq : 1));
    this.setRepeat(rep);
    this.setOnlyWorkingDays(onlyWorkingDays);
    this.endRecurr=endRecur;
    recalculateFields();
  }

  public void recalculateFields() {
    CompanyCalendar cal = new CompanyCalendar();
    cal.setTimeInMillis(this.getValidityStartTime());
    this.setStartTime(cal.getMillisFromMidnight());
    //if endRecurr is passed it is used to calculate number of repetitions
    if (endRecurr!=null)
      calculateRepetitions();


    if (this.getRepeat() > 0) {
      int freq = (this.getFreq()>0?this.getFreq() :1);
      int val = ((this.getRepeat() - 1) * freq);
      cal.add(Calendar.DATE, val);
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
        int ofset = (timeZone.getOffset(lstart) - timeZone.getOffset(afterTime)); // questo serve per calcolare i millisecondi effettivi in caso di ora legale
        long distInMillisec = afterTime - lstart - ofset;
        double ddistInDays = (double) distInMillisec / (double) CompanyCalendar.MILLIS_IN_DAY;
        int distInDays = (int) ddistInDays;
        int freq = (this.getFrequency()>0?this.getFreq() :1);
        int rest = freq - (int) distInDays % freq;

        cal.setTime(getStart());
        cal.add(Calendar.DATE, (int) distInDays + rest);

        if (isOnlyWorkingDays()) {
          while (!cal.isWorkingDay()) {
            cal.add(Calendar.DATE, 1);
          }
        }
        if (cal.getTime().getTime() <= getEnd().getTime())
          returnTime = cal.getTime().getTime();
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
      int ofset = (timeZone.getOffset(lstart) - timeZone.getOffset(beforeTime)); // questo server per calcolare i millisecondi effettivi in caso di ora legale
      long distInMillisec = beforeTime - lstart - ofset;
      double ddistInDays = (double) distInMillisec / (double) CompanyCalendar.MILLIS_IN_DAY;
      int distInDays = (int) ddistInDays;
      int freq = (this.getFreq()>0?this.getFreq() :1);
      int rest = (int) distInDays % freq;
      cal.setTime(getStart());
      cal.add(Calendar.DATE, (int) distInDays - rest);
      // onlyWorkingDays  non è possibile sapere quale giorno è il prec. es: tutti i giorni non lav.
      // sab-> lun dom->lun se chiedo il prev. di lunedì quale ritorno?

      returnTime = cal.getTimeInMillis();
    }
    return returnTime;
  }


  public String getName() {
      return "daily";
  }


  protected boolean isOnlyWorkingDays() {
    return onlyWorkingDays;
  }


  protected void setOnlyWorkingDays(boolean onlyWorkingDays) {
    this.onlyWorkingDays = onlyWorkingDays;
  }


  private void calculateRepetitions() {
    double howManyDays=Math.round((endRecurr.getTime()-getValidityStartDate().getTime())/CompanyCalendar.MILLIS_IN_DAY);
    int repeat = (int) (howManyDays / getFrequency());
    setRepeat(repeat<0?1:repeat);
    endRecurr=null; // si annulla in modo da non dover ricalcolare un'altra volta
  }


  public String getScheduleDescription(String useSeparator) {
    String result = "";
    result = I18n.get("SCHEDULE_DAILY_CONTENT_%%...",
            DateUtilities.dateAndHourToString(getStartDate())+useSeparator,
            DateUtilities.dateAndHourToString(getEndDate())+useSeparator,
            getFrequency() + ""+useSeparator,
            getRepetitions() + "");
    return result;
  }
  

  public JSONObject jsonify() {
    JSONObject ret = super.jsonify();
    ret.element("type","daily");
    ret.element("onlyWorkingDays", isOnlyWorkingDays());
    return ret;
  }

  public static ScheduleDaily fromJSON(JSONObject json){
    ScheduleDaily sd = new ScheduleDaily(new Date(json.getLong("startMillis")), json.getInt("duration"), json.getInt("freq"), json.getInt("repeat"));
    sd.setOnlyWorkingDays(json.getBoolean("onlyWorkingDays"));
    return sd;
  }

}

