package org.jblooming.agenda;

import net.sf.json.JSONObject;
import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.JSP;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;


public abstract class ScheduleSupport extends IdentifiableSupport implements Schedule, Comparable {

  private Date start;
  private Date end;
  private int startTime; // Milliseconds elapsed from midnight
  private long duration;   // Duration in milliseconds
  private int freq = 1;                         // how many days the event is repeated
  private int repeat = 1;                       // number of times the event is repeated

  protected Date endRecurr=null; //not persisted. is used whe you want to set the end of recurrencies instead the number. If it is set can override the "repeat" field



  public ScheduleSupport() {
  }

  /**
   * @return the start date. Note this method returns null if the date is null
   *         see getValidityStartDate() on each subclass
   */
  public Date getStartDate() {
    return getStart();
  }


  /**
   * @return the end date. Note this method returns null if the date is null
   *         see getValidityEndDate() on each subclass
   */
  public Date getEndDate() {
    return getEnd();
  }

  /**
   * Set the start date and recompute the fields
   *
   * @param start
   */
  public void setStartDate(Date start) {
    setStart(start);
    recalculateFields();
  }

  /**
   * @return millis elapsed from hour midnight (00:00:00.000)
   */
  public int getStartTimeInMillis() {
    return startTime;
  }

  /**
   * set the start time in millis from hour 00:00:00.000 and recompute fields
   *
   * @param startTime
   */
  public void setStartTimeInMillis(int startTime) {
    this.startTime = startTime;
    if (start != null) {
      CompanyCalendar cc = new CompanyCalendar();
      cc.setTime(start);
      cc.setMillisFromMidnight(this.startTime);
      this.start = cc.getTime();
    }
    recalculateFields();
  }

  /**
   * @return the event duration
   */
  public long getDurationInMillis() {
    return duration;
  }

  /**
   * set the event duration in millis and recompute fields
   *
   * @param duration
   */
  public void setDurationInMillis(long duration) {
    this.duration = duration;
    recalculateFields();
  }


  /**
   * @return true if there is an overlapping betwheen periods
   */
  //  public boolean overlap(Period p) {
  //    return !(this.getStartDate().after(p.getEndDate()) || this.getEndDate().before(p.getStartDate()));
  //  }
  public boolean overlap(Period p) {
    long pf = getPreviousFireTimeBefore(p.getStartDate().getTime());
    pf = pf + getDurationInMillis();
    if (pf > p.getStartDate().getTime()) {
      return true;
    } else {
      pf = getNextFireTimeAfter(p.getStartDate().getTime());
      return pf < p.getEndDate().getTime();
    }
  }

  /**
   * @return the start date. If null return CompanyCalendar.MIN_DATE
   *         If you need to get null use getStartDate()
   */
  public Date getValidityStartDate() {
    if (this.getStartDate() == null)
      return CompanyCalendar.MIN_DATE;
    return this.getStartDate();
  }

  /**
   * @return the start date. If null return CompanyCalendar.MAX_DATE
   *         If you need to get null use getEndDate()
   */
  public Date getValidityEndDate() {
    if (this.getEndDate() == null)
      return CompanyCalendar.MAX_DATE;
    return this.getEndDate();
  }

  /**
   * @return the start. If null return CompanyCalendar.MIN_DATE
   *         If you need to get null use getEndDate()
   */
  public long getValidityStartTime() {
    if (this.getStartDate() == null)
      return CompanyCalendar.MIN_DATE.getTime();
    return this.getStartDate().getTime();
  }

  public long getValidityEndTime() {
    if (this.getEndDate() == null)
      return CompanyCalendar.MAX_DATE.getTime();
    return this.getEndDate().getTime();
  }

  /**
   * Used to get the smaller period that wraps the whole recurrent set.
   * In case of not recurrent a new period with same ends is returned.
   */
  public Period getPeriod() {
    return new Period(this.getStartDate(), this.getEndDate());
  }


  public Date getNextFireDate() {
    long nextFireTime = getNextFireTime();
    if (nextFireTime > 0)
      return new Date(nextFireTime);
    else
      return null;
  }

  public long getNextFireTime() {
    Date afterTime = new Date();
    Date pot = getNextDateAfter(afterTime);
    if (pot == null || (end != null && pot.after(end)))
      return 0;
    else
      return pot.getTime();
  }


  public Date getNextDateAfter(Date afterTime) {
    long nextFireTime = getNextFireTimeAfter(afterTime.getTime());
    if (nextFireTime > 0 && nextFireTime < Long.MAX_VALUE)
      return new Date(nextFireTime);
    else
      return null;
  }

  public Date getPreviousDateBefore(Date beforeTime) {
    long previousFireTime = getPreviousFireTimeBefore(beforeTime.getTime());
    if (previousFireTime > 0)
      return new Date(previousFireTime);
    else
      return null;
  }

  public int getFrequency() {
    return getFreq();
  }

  public void setFrequency(int freq) {
    this.freq = freq;
    recalculateFields();
  }

  public int getRepetitions() {
    return repeat;
  }

  public void setRepetitions(int repeat) {
    this.repeat = repeat;
    recalculateFields();
  }


  public abstract String getScheduleDescription(String useSeparator);
  
  protected abstract void recalculateFields();

  /**
   * @param p
   * @param trim
   * @return re
   * @deprecated
   */
  public List<Period> getPeriods(Period p, boolean trim) {
    return getPeriods(p, trim,null);
  }

  public List<Period> getPeriods(Period p, boolean trim, String exceptions) {
    List<Period> ret = new ArrayList<Period>();
    long requiredStart = p.getValidityStartTime();
    long requiredEnd = p.getValidityEndTime();
    if (!(this.getValidityEndTime() < requiredStart || this.getValidityStartTime() > requiredEnd)) {


      // find the previous
      long st = this.getPreviousFireTimeBefore(requiredStart);
      long ps = st+ this.getDurationInMillis();
      if (ps > requiredStart) {

        //è tra quelli esclusi?
        if (!JSP.w(exceptions).contains(st+"")) {
          if (trim) {
            Period tp = new Period(requiredStart, Math.min(ps, requiredEnd));
            tp.trimmed=true;
            ret.add(tp);
          } else
            ret.add(new Period(ps - this.getDurationInMillis(), ps));
        }
      }

      //long nextFire = this.getNextFireTimeAfter(requiredStart - 1);// 8/6/2010 - commented out in order to avoid hits the same appointment when this.start==p.start on recurrent events
      long nextFire = requiredStart;
      if (requiredStart != getStart().getTime())
        nextFire = this.getNextFireTimeAfter(requiredStart);
      int watchDog = 0; // not more than 1000 occurrences for event
      while (nextFire <= requiredEnd && watchDog < 1000) {

        long eventStart = nextFire;
        long eventDuration = this.getDurationInMillis();
        //è tra quelli esclusi?
        if (!JSP.w(exceptions).contains(eventStart+"")) {
          if (trim && (nextFire + eventDuration > requiredEnd)) {
            Period tp = new Period(eventStart, requiredEnd);
            tp.trimmed=true;
            ret.add(tp);
          } else {
            ret.add(new Period(eventStart, eventStart + eventDuration));
          }
        }
        nextFire = this.getNextFireTimeAfter(nextFire + eventDuration + 1);

        watchDog++;
      }
      if (watchDog >= 1000)
        Tracer.platformLogger.warn("watchDog barking on ScheduleSupport.getPeriods period id = " + getId());

    }


    return ret;
  }


  /**
   * @param date
   * @return true if the date is include in a fire period
   */
  public boolean contains(Date date) {
    boolean ret = false;
    if (date!=null) {
      long st = this.getPreviousFireTimeBefore(date.getTime() + 1);
      if (st + getDurationInMillis() >= date.getTime()) // FIXED on 24/09/2008 by robicch
        ret = true;
    }
    return ret;
  }


  /*
  * This method return the validity period.
  * The period is trimmed to the starting time and the ending time.
  * There is no test for working days validity (eg. for example if  the validity period starts on sat at 8.00
  * and you choos working days only, the validity period start on sat at 8.00 NOT on mon 8.00)
  */
  public Period getValidityPeriod() {
    return new Period(getValidityStartDate(), getValidityEndDate());
  }

  protected Date getStart() {
    return start;
  }

  protected void setStart(Date start) {
    this.start = start;
  }

  protected Date getEnd() {
    return end;
  }

  protected void setEnd(Date end) {
    this.end = end;
  }

  protected int getStartTime() {
    return startTime;
  }

  protected void setStartTime(int startTime) {
    this.startTime = startTime;
  }

  protected long getDuration() {
    return duration;
  }

  protected void setDuration(long duration) {
    this.duration = duration;
  }

  protected int getFreq() {
    return freq;
  }

  protected void setFreq(int freq) {
    this.freq = freq;
  }

  protected int getRepeat() {
    return repeat;
  }

  protected void setRepeat(int repeat) {
    this.repeat = repeat;
  }

  public int compareTo(Object o) {
    int retVal = 0;
    ScheduleSupport scheduleSupport = ((ScheduleSupport) o);
    if (getValidityStartTime() < scheduleSupport.getValidityStartTime())
      retVal = -1;
    else if (getValidityStartTime() > scheduleSupport.getValidityStartTime())
      retVal = 1;

    else if (this.getId() != null && !PersistenceHome.NEW_EMPTY_ID.equals(getId())) {
      retVal = (this.getId() + "").compareTo(scheduleSupport.getId() + "");
    } else {
      retVal = this.getEndDate().compareTo(scheduleSupport.getEndDate()); // commented on 11/5/2007
      //retVal = (this==scheduleSupport ? 0 : 1); // two non persistent period with identical timind are equals only when are == (at least this is required in agendaweek) 
    }


    return retVal;
  }

  public JSONObject jsonify() {
    JSONObject ret = super.jsonify();
    ret.element("id", getId());
    ret.element("type", getName());
    ret.element("startMillis", getStartDate().getTime());
    ret.element("endMillis", getEndDate().getTime());
    ret.element("duration", getDuration());

    ret.element("freq", getFrequency());
    ret.element("repeat", getRepetitions());
    return ret;
  }

  public static ScheduleSupport getInstancefromJSON(JSONObject json) {
    String type = json.getString("type");

    ScheduleSupport schedule = null;
    if (type != null) {
      if ("period".equals(type)) {
        schedule = Period.fromJSON(json);
      } else if ("minute".equals(type)) {
        schedule = ScheduleMinute.fromJSON(json);
      } else if ("daily".equals(type)) {
        schedule = ScheduleDaily.fromJSON(json);
      } else if ("weekly".equals(type)) {
        schedule = ScheduleWeekly.fromJSON(json);
      } else if ("monthly".equals(type)) {
        schedule = ScheduleMonthly.fromJSON(json);
      } else if ("yearly".equals(type)) {
        schedule = ScheduleYearly.fromJSON(json);
      }
    }
    return schedule;
  }

}
