package org.jblooming.agenda;

import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.waf.settings.I18n;

import java.util.*;

import net.sf.json.JSONObject;

public class ScheduleMonthly extends ScheduleSupport implements Schedule {
  private int dayOfWeek;
  private int weekOfMonth;

  private ScheduleMonthly() {
  }

  public ScheduleMonthly(int dayOfWeek, int weekOfMonth, Date start, int duration) {
    this(dayOfWeek, weekOfMonth, start, duration, 1, 0);
  }

  public ScheduleMonthly(int dayOfWeek, int weekOfMonth, Date start, int duration, int freq, int rep) {
    this.dayOfWeek = dayOfWeek;
    this.weekOfMonth = weekOfMonth;
    this.setStart(start);
    this.setDuration(duration);
    this.setFreq((freq > 0 ? freq : 1));
    this.setRepeat(rep);
    recalculateFields();
  }

  public ScheduleMonthly(Date start, int duration, int freq, int rep) {
    this(start,duration,freq,rep,null);
  }
  public ScheduleMonthly(Date start, int duration, int freq, int rep,Date endRecurr) {
    this.setStart(start);
    this.setDuration(duration);
    this.setFreq((freq > 0 ? freq : 1));
    this.setRepeat(rep);
    this.endRecurr=endRecurr;
    recalculateFields();
  }

  public ScheduleMonthly(int dayOfWeek, int weekOfMonth, Date start, int startTime, int duration) {
    this(dayOfWeek, weekOfMonth, start, startTime, duration, 1, 0,null);
  }

  public ScheduleMonthly(int dayOfWeek, int weekOfMonth, Date start, int startTime, int duration, int freq, int rep,Date endRecurr) {
    this.dayOfWeek = dayOfWeek;
    this.weekOfMonth = weekOfMonth;
    this.setStartTime(startTime);
    this.setStart(start);
    CompanyCalendar cal = new CompanyCalendar();
    cal.setTimeInMillis(this.getValidityStartTime());
    cal.setMillisFromMidnight(startTime);
    this.setStart(cal.getTime());
    this.setDuration(duration);
    this.setFreq((freq > 0 ? freq : 1));
    this.setRepeat(rep);
    this.endRecurr=endRecurr;
    recalculateFields();
  }

  public ScheduleMonthly(Date start, int startTime, int duration, int freq, int rep) {
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


  public void recalculateFields() {
    CompanyCalendar cal = new CompanyCalendar();
    cal.setTimeInMillis(this.getValidityStartTime());
    long startTime = cal.getTimeInMillis();

    if (endRecurr!=null)
      calculateRepetitions();

    int freq = (this.getFreq()>0?this.getFreq() :1);
     if (weekOfMonth > 0 && dayOfWeek>0) {
      startTime = getDayInWeekInMonth(cal);
    }
    cal.setTimeInMillis(startTime);
    this.setStart(cal.getTime());
    this.setStartTime(cal.getMillisFromMidnight());
    if (this.getRepeat() > 0) {
      int val = ((this.getRepeat() - 1) * freq);
      cal.add(CompanyCalendar.MONTH, val);
      long endTime = cal.getTimeInMillis();
      if (weekOfMonth > 0 && dayOfWeek > 0) {
        endTime = getDayInWeekInMonth(cal);
      }
      cal.setTimeInMillis(endTime);
    } else {
      this.setRepeat(0);
      cal.setTime(CompanyCalendar.MAX_DATE);
      cal.set(Calendar.HOUR_OF_DAY, 0);
      cal.clear(Calendar.MINUTE);
      cal.clear(Calendar.SECOND);
      cal.clear(Calendar.MILLISECOND);
      cal.add(Calendar.MILLISECOND, getStartTimeInMillis());
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
        TimeZone timeZone = cal.getTimeZone();
        int ofset = (timeZone.getOffset(lstart) - timeZone.getOffset(afterTime)); // questo server per calcolare i millisecondi effettivi in caso di ora legale
        long distInMillisec = afterTime - lstart - ofset;
        long distInDays = distInMillisec / CompanyCalendar.MILLIS_IN_DAY;
        long distInYear = distInDays / 365;      //non sbaglia il conto fino a che non sono passati 1460 anni
        if (distInYear > 0) {
          cal.add(Calendar.YEAR, (int) distInYear);
          distInDays -= (distInYear*365);
        }
        double distInMonth = (int) distInDays / 29;  //in un anno dividendo per 29 i giorni ottengo sempre il numero corretto di mesi
        int rep = 1;
        int freq = (this.getFreq()>0?this.getFreq() :1);
        if(distInMonth > 0 ) {
          rep = (int) distInMonth / freq;
          if ((distInMonth % freq) != 0)
            rep++;
        }
        if (this.getRepeat() > 0 && rep > (this.getRepeat() - 1))
          rep = this.getRepeat() - 1;
        cal.add(Calendar.MONTH, rep * freq);
        long next;
        if (weekOfMonth > 0 && dayOfWeek > 0) {
          next = getDayInWeekInMonth(cal);
          cal.setTimeInMillis(next);
          int watchDog = 1; // not more than 10 occurrences for event
          while (next < afterTime && watchDog < 10) {
            cal.add(Calendar.MONTH, freq);
            next = getDayInWeekInMonth(cal);
            cal.setTimeInMillis(next);
            watchDog++;
          }
          if (watchDog>=10) {            
            Tracer.platformLogger.warn("watchDog barking on ScheduleMonthly.getNextFireTimeAfter");
          }
          returnTime = next;
        } else {
          next = getDayInMonthAfter(cal,afterTime);
          int watchDog = 1; // not more than 10 occurrences for event
          while (next < afterTime && watchDog < 10) {
            cal.add(Calendar.MONTH, freq);
            next = getDayInMonthAfter(cal,afterTime);
            watchDog++;
          }
          if (watchDog>=10)
            Tracer.platformLogger.warn("watchDog barking on ScheduleMonthly.getNextFireTimeAfter");
          returnTime = next;
        }
      } else {
        returnTime = getStart().getTime();
      }
    }
    return returnTime;
  }

  public long getPreviousFireTimeBefore(long beforeTime) {
    long returnTime = Long.MIN_VALUE;
    if (beforeTime > getStart().getTime()) {
      if(beforeTime > getEnd().getTime())
        beforeTime = getEnd().getTime();
      long lstart = getStart().getTime();
      CompanyCalendar cal = new CompanyCalendar();
      cal.setTimeInMillis(lstart);
      TimeZone timeZone = cal.getTimeZone();
      int ofset = (timeZone.getOffset(lstart) - timeZone.getOffset(beforeTime)); // questo server per calcolare i millisecondi effettivi in caso di ora legale
      long distInMillisec = beforeTime - lstart - ofset;
      long distInDays = distInMillisec / CompanyCalendar.MILLIS_IN_DAY;
      long distInYear = distInDays / 365;      //non sbaglia il conto fino a che non sono passati 1460 anni
      if (distInYear > 0) {
        cal.add(Calendar.YEAR, (int) distInYear);
        distInDays -= (distInYear*365);
      }
      int distInMonth = (int) distInDays / 29;  //in un anno dividendo per 29 i giorni ottengo sempre il numero corretto di mesi
      int freq = (this.getFreq()>0?this.getFreq() :1);
      int rep = distInMonth / freq;
      if (this.getRepeat() == 0 || rep <= (this.getRepeat() - 1)) {
        cal.add(Calendar.MONTH, rep * freq);
        long before;
        if (weekOfMonth > 0 && dayOfWeek > 0) {
          before = getDayInWeekInMonth(cal);
          if(before < beforeTime)
            returnTime = before;
        } else {
          before = getDayInMonthBefore(cal,beforeTime);
          if(before < beforeTime)
            returnTime = before;
        }
      }
    }
    return returnTime;
  }


  private long getDayInMonthBefore(CompanyCalendar cal, long beforeTime){
    return  cal.getTimeInMillis();
  }

  private long getDayInMonthAfter(CompanyCalendar cal, long afterTime){
    return cal.getTimeInMillis();
  }

 /*

 E' stata scelta questa soluzione perchè il CompanyCalendar ha un comportamento assurdo!!!!

 */
  private long getDayInWeekInMonth(CompanyCalendar cal) {
    cal.set(Calendar.DAY_OF_MONTH,1);
    int count = 0;
    int month = cal.get(CompanyCalendar.MONTH);
    long lastDay = 0;
    for(int i = 1; i<32 ; i++) {
      if(month != cal.get(CompanyCalendar.MONTH))
        break;
      if(cal.get(CompanyCalendar.DAY_OF_WEEK) == dayOfWeek) {
        count++;
        lastDay = cal.getTimeInMillis();
        if(count == weekOfMonth)
          break;
      }
      cal.add(CompanyCalendar.DAY_OF_MONTH, 1);
    }
    return lastDay;
  }


  private void calculateRepetitions() {
    double howManyDays=Math.round((endRecurr.getTime()-getValidityStartDate().getTime())/CompanyCalendar.MILLIS_IN_MONTH);
    int repeat = (int) (howManyDays / getFrequency());
    setRepeat(repeat<0?1:repeat);
    endRecurr=null; // si annulla in modo da non dover ricalcolare un'altra volta
  }



  public String getName() {
    return "monthly";
  }

  public String getScheduleDescription(String useSeparator) {
    String result = "";
    result = I18n.get("SCHEDULE_MONTHLY_CONTENT_%%...",
            DateUtilities.dateAndHourToString(getStartDate())+useSeparator,
            DateUtilities.dateAndHourToString(getEndDate()));
    return result;
  }


  public JSONObject jsonify() {
    JSONObject ret = super.jsonify();
    ret.element("type","monthly");
    ret.element("dayOfWeek",getDayOfWeek());
    ret.element("weekOfMonth",getWeekOfMonth());
    return ret;
  }

  public static ScheduleMonthly fromJSON(JSONObject json){
    ScheduleMonthly sd;
    if (json.get("dayOfWeek")!=null && json.get("weekOfMonth")!=null){
      sd= new ScheduleMonthly(json.getInt("dayOfWeek"),json.getInt("weekOfMonth"),new Date(json.getLong("startMillis")), json.getInt("duration"), json.getInt("freq"), json.getInt("repeat"));
    } else {
      sd = new ScheduleMonthly(new Date(json.getLong("startMillis")), json.getInt("duration"), json.getInt("freq"), json.getInt("repeat"));
    }
    return sd;
  }
  


}

