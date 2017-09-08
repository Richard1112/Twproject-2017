package org.jblooming.agenda;

import org.jblooming.utilities.DateUtilities;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.waf.settings.I18n;

import java.util.Date;
import java.util.Locale;

import net.sf.json.JSONObject;

import javax.persistence.Transient;


public class Period extends ScheduleSupport implements Schedule, Comparable {

  protected boolean trimmed=false; //used to know if this period comes from a getPeriods, and has been trimmed


  /**
   * @deprecated use Period(long start, long end)
   */
  protected Period() {
  }

  public Period(long start, long end) {
    this(new Date(start), new Date(end));
  }

  public Period(Date start, long duration) {
    CompanyCalendar st = new CompanyCalendar();
    st.setTime(start);
    st.add(CompanyCalendar.MILLISECOND, (int) duration);
    buildInstance(start, st.getTime());
  }

  public Period(Date start, Date end) {
    buildInstance(start, end);
  }


  private void buildInstance(Date start, Date end) {
    this.setStart(start);
    this.setEnd(end);
    recomputeDuration();
  }

  public void recomputeDuration() {
    CompanyCalendar st = new CompanyCalendar();
    if (this.getStart() != null) {
      st.setTime(this.getStart());
      this.setStartTime(st.getMillisFromMidnight());
    } else {
      this.setStartTime(0);
    }
    this.setDuration(getValidityEndTime() - getValidityStartTime());
  }

  /**
   * @param aDate
   * @return a day period for the passed date
   */
  public static Period getDayPeriodInstance(Date aDate) {
    CompanyCalendar cc = new CompanyCalendar();
    cc.setTime(aDate);
    return new Period(cc.setAndGetTimeToDayStart(), cc.setAndGetTimeToDayEnd());
  }

  /**
   * @param aDate
   * @return a week period for the passed date
   */
  public static Period getWeekPeriodInstance(Date aDate, Locale locale) {
    CompanyCalendar cc = new CompanyCalendar(locale);
    cc.setTime(aDate);
    cc.set(CompanyCalendar.DAY_OF_WEEK,cc.getFirstDayOfWeek());
    long st=cc.setAndGetTimeToDayStart().getTime();
    cc.add(CompanyCalendar.WEEK_OF_YEAR,1);
    long en=cc.setAndGetTimeToDayStart().getTime()-1;
    return new Period(st,en);
  }


  /**
   * @param aDate
   * @return a month period for the passed date
   */
  public static Period getMonthPeriodInstance(Date aDate) {
    CompanyCalendar cc = new CompanyCalendar();
    cc.setTime(aDate);
    cc.set(CompanyCalendar.DAY_OF_MONTH,1);
    long st=cc.setAndGetTimeToDayStart().getTime();
    cc.add(CompanyCalendar.MONTH,1);
    long en=cc.setAndGetTimeToDayStart().getTime()-1;
    return new Period(st,en);
  }


  /**
   * create a new Period as intersection with the passed one
   *
   * @param p the intersecting Period
   * @return null if no intersection
   */
  public Period intersection(Period p) {
    Period result = null;
    if (!((p.getValidityEndTime() < this.getValidityStartTime()) || (p.getValidityStartTime() > this.getValidityEndTime())))
    {
      result = new Period(Math.max(this.getValidityStartTime(), p.getValidityStartTime()), Math.min(this.getValidityEndTime(), p.getValidityEndTime()));
    }
    return result;
  }

  public Period union(Period p) {
    return new Period(Math.min(this.getValidityStartTime(), p.getValidityStartTime()), Math.max(this.getValidityEndTime(), p.getValidityEndTime()));
  }

  public long getNextFireTimeAfter(long afterTime) {
    //if (getValidityStartTime() > afterTime)  // R&P 4/4/2015 altrimenti un evento di un giorno intero non overlaps
    if (getValidityStartTime() >= afterTime)
      return getValidityStartTime();
    else
      return Long.MAX_VALUE;
  }

  public long getPreviousFireTimeBefore(long beforeTime) {
    if (getValidityStartTime() < beforeTime)
      return getValidityStartTime();
    else
      return Long.MIN_VALUE;
  }

  //this is not valid in daylight saving transitions
  public int lenghtInDays() {
    // return (new Long(getDurationInMillis() / (CompanyCalendar.MILLIS_IN_DAY))).intValue(); if I use the closest int should be fine
    return (int)(Math.rint( (double)getDurationInMillis() / (CompanyCalendar.MILLIS_IN_DAY)));

  }

  public String toString() {
    return DateUtilities.dateToString(getStart(), "yyyy MM dd HH:mm:ss") + " - " + DateUtilities.dateToString(getEnd(), "yyyy MM dd HH:mm:ss");
  }


  public int getFrequency() {
    return 0;
  }


  public int getRepetitions() {
    // a Period is repeated always once
    return 1;
  }

  protected void recalculateFields() {
    //calculate the end date basing on start and duration
    setEndDate(new Date(getStartDate().getTime()+getDuration()));
  }

  public String getName() {
    return "period";
  }

  public void setEndDate(Date date) {
    setEnd(date);
  }


  public int hashCode() {
    int result = 0;
    if (PersistenceHome.NEW_EMPTY_ID.equals(id) || id == null)
      result = (this.getValidityStartTime() + "").hashCode();
    else
      result = (id + "").hashCode();
    return result;
  }


  public String getScheduleDescription(String useSeparator) {
    String result = "";

    if (DateUtilities.dateToInt(getStartDate())==DateUtilities.dateToInt(getEndDate())){
      result = I18n.get("SCHEDULE_PERIOD_CONTENT_ONE_DAY_%%...",
              DateUtilities.dateToFullString(getStartDate())+useSeparator,
              DateUtilities.dateToHourMinutes(getStartDate()),
              DateUtilities.dateToHourMinutes(getEndDate()));

    } else {
      result = I18n.get("SCHEDULE_PERIOD_CONTENT_%%...",
              DateUtilities.dateAndHourToFullString(getStartDate()),
              DateUtilities.dateAndHourToFullString(getEndDate()));
    }

    return result;
  }

  public JSONObject jsonify() {
    JSONObject ret = super.jsonify();
    ret.element("type","period");
    ret.element("trimmed",isTrimmed());
    return ret;
  }


  public static Period fromJSON(JSONObject json){
    return new Period(json.getLong("startMillis"),json.getLong("endMillis"));
  }

  @Transient
  public boolean isTrimmed(){
    return trimmed;
  }
}
