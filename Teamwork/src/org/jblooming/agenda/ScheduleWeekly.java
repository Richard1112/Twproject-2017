package org.jblooming.agenda;


import net.sf.json.JSONObject;
import net.sf.json.JSONArray;

import java.util.*;

import org.jblooming.waf.settings.I18n;
import org.jblooming.utilities.DateUtilities;

public class ScheduleWeekly extends ScheduleSupport implements Schedule {
  private int dayArray[];                     //specifies the days the event is repeated {Calendar.Monday,... and so on....

  public ScheduleWeekly() {
  }

  public ScheduleWeekly(int days[], Date start, int duration) {
    this(days, start, duration, 1, 1);
  }

  public ScheduleWeekly(int days[], Date start, int duration, int freq, int repetition) {
    this(days,start, duration, freq, repetition,null);
  }
  public ScheduleWeekly(int days[], Date start, int duration, int freq, int repetition,Date endRecurr) {
    this(days, start, new CompanyCalendar(start).getMillisFromMidnight(), duration, freq, repetition,endRecurr);
  }

  public ScheduleWeekly(int days[], Date start, int startTime, int duration) {
    this(days, start, startTime, duration, 1, 1);
  }

  public ScheduleWeekly(int days[], Date start, int startTime, int duration, int freq, int repetition) {
    this(days, start, startTime, duration, freq, repetition,null);
  }
  public ScheduleWeekly(int days[], Date start, int startTime, int duration, int freq, int repetition,Date endRecurr) {

    // added by bicch and chelazz 17/12/2008
    if (start.before(new Date(0)))
      start = new Date(0);

    this.setStartTime(startTime);
    this.setStart(start);
    this.endRecurr=endRecurr;
    CompanyCalendar cal = new CompanyCalendar();
    cal.setTimeInMillis(this.getValidityStartTime());
    cal.setMillisFromMidnight(startTime);
    this.setStart(cal.getTime());

    buildSchedule(days, duration, freq, repetition);

  }

  private void buildSchedule(int[] days, int duration, int freq, int repetition) {

    CompanyCalendar cc = new CompanyCalendar();
    cc.set(CompanyCalendar.DAY_OF_WEEK, cc.getFirstDayOfWeek());
    List daysL = new ArrayList();

    for (int i = 0; i < 7; i++) {
      int d = cc.get(CompanyCalendar.DAY_OF_WEEK);
      for (int j = 0; j < days.length; j++) {
        if (d == days[j]) {
          daysL.add(d);
          break;
        }
      }
      cc.add(CompanyCalendar.DAY_OF_YEAR, 1);
    }

    int dayArrayLocal[] = new int[daysL.size()];
    int j = 0;
    for (Object o : daysL) {
      dayArrayLocal[j] = (Integer) o;
      j++;
    }

    this.dayArray = dayArrayLocal;
    this.setDuration(duration);
    this.setFreq((freq > 0 ? freq : 1));
    this.setRepeat(repetition != -1 ? repetition : 0);
    recalculateFields();
  }


  private int[] getDayArray() {
    return dayArray;
  }

  private void setDayArray(int[] dayArray) {
    this.dayArray = dayArray;
  }

  public int[] getDays() {
    return dayArray;
  }

  public void setDays(int[] dayArray) {
    this.dayArray = dayArray;
    recalculateFields();
  }

  public void recalculateFields() {
    int eventsFired = 0;

    CompanyCalendar cal = new CompanyCalendar();
    cal.setTimeInMillis(this.getValidityStartTime());

    if (endRecurr!=null)
      calculateRepetitions();

    boolean found = false;
    int freq = (this.getFreq() > 0 ? this.getFreq() : 1);
    //begin first week match
    while (!found) {
      if (dayArray != null && dayArray.length>0) {
        for (int day : dayArray) {

          if (getRepeat() > 0 && eventsFired >= getRepeat())
            break;

          cal.set(Calendar.DAY_OF_WEEK, day);
          if (!found && cal.getTime().getTime() >= getStart().getTime()) {
            setStart(cal.getTime());
            found = true;
          }
          if (found)
            eventsFired++;
        }
      } else {
        setDayArray(new int[]{cal.get(Calendar.DAY_OF_WEEK)});
        found=true;
      }
      if (!found)
        cal.add(Calendar.WEEK_OF_YEAR, freq);
    }

    setStartTime(cal.getMillisFromMidnight());
    //end first week match

    //begin big leap
    if (getRepeat() - eventsFired > 0) {

      int numberOfDaysSelected = getDays().length;
      int eventsToFireInWeeks = (int) Math.ceil(((double) getRepeat() - (double) eventsFired) / (double) numberOfDaysSelected);
      cal.add(Calendar.WEEK_OF_YEAR, eventsToFireInWeeks * freq);
      cal.set(Calendar.DAY_OF_WEEK, dayArray.length > 0 ? dayArray[0] : cal.getFirstDayOfWeek());
      eventsFired = eventsFired + (Math.max(eventsToFireInWeeks - 1, 0) * numberOfDaysSelected);
    } else if (getRepeat() == 0) {
      cal.setTime(CompanyCalendar.MAX_DATE);
    }
    //end big leap

    cal.set(Calendar.HOUR_OF_DAY, 0);
    cal.set(Calendar.MINUTE,0);
    cal.set(Calendar.SECOND,0);
    cal.set(Calendar.MILLISECOND,0);

    cal.add(Calendar.MILLISECOND, getStartTimeInMillis());

    cal.add(Calendar.MILLISECOND, (int) this.getDuration());

    //begin eventual last week
    int dayPos = 0;
    while (eventsFired < getRepeat()) {
      if (dayPos >= dayArray.length)
        dayPos = 0;

      cal.set(Calendar.DAY_OF_WEEK, dayArray[dayPos]);
      eventsFired++;
      dayPos++;
    }
    //end eventual last week

    setEnd(cal.getTime());
  }


  public long getNextFireTimeAfter(long afterTime) {

    long returnTime = Long.MAX_VALUE;
    if (afterTime <= getEnd().getTime()) {

      if (afterTime > getStart().getTime()) {

        long lstart = getStart().getTime();
        CompanyCalendar cal = new CompanyCalendar();
        cal.setTimeInMillis(lstart);

        int posOfWeekOfStart = 0;
        for (int i = 0; i < dayArray.length; i++) {
          if (dayArray[i] == cal.get(CompanyCalendar.DAY_OF_WEEK)) {
            posOfWeekOfStart = i + 1;
            break;
          }
        }
        if (posOfWeekOfStart >= dayArray.length)
          posOfWeekOfStart = 0;

        TimeZone timeZone = cal.getTimeZone();
        int ofset = (timeZone.getOffset(lstart) - timeZone.getOffset(afterTime)); // questo serve per calcolare i millisecondi effettivi in caso di ora legale
        long distInMillisec = afterTime - lstart - ofset;
        long distInDays = distInMillisec / CompanyCalendar.MILLIS_IN_DAY;
        long distInWeek = (long) Math.floor(distInDays / 7.0);

        int freq = (this.getFreq() > 0 ? this.getFreq() : 1);
        int rep = ((int) distInWeek / freq);

        //setting ouselves on the first "good" week (i.e. one of those not empty because of frequency) before
        cal.add(Calendar.WEEK_OF_YEAR, rep * freq);

        //check whether there is a week day greater than afterTime
        boolean foundMatchingDay = false;
        for (int i = 0; i < dayArray.length; i++) {
          cal.set(CompanyCalendar.DAY_OF_WEEK, dayArray[i]);
          if (cal.getTime().getTime() >= afterTime) {
            foundMatchingDay = true;
            returnTime = cal.getTime().getTime();
            break;
          }
        }

        if (!foundMatchingDay) {

          cal.add(Calendar.WEEK_OF_YEAR, freq);
          for (int i = 0; i < dayArray.length; i++) {
            cal.set(CompanyCalendar.DAY_OF_WEEK, dayArray[i]);
            if (cal.getTime().getTime() >= afterTime) {
              returnTime = cal.getTime().getTime();
              break;
            }
          }
        }

      } else {
        returnTime = getStart().getTime();
      }
    }
    return returnTime;
  }

  /*
  public long getPreviousFireTimeBefore(long beforeTime) {
    long returnTime = Long.MIN_VALUE;
    if (beforeTime > getStart().getTime()) {
      if (beforeTime <= getEnd().getTime()) {

        long lstart = getStart().getTime();
        CompanyCalendar cal = new CompanyCalendar();
        cal.setTimeInMillis(lstart);
        TimeZone timeZone = cal.getTimeZone();
        int ofset = (timeZone.getOffset(lstart) - timeZone.getOffset(beforeTime)); // questo server per calcolare i millisecondi effettivi in caso di ora legale
        long distInMillisec = beforeTime - lstart - ofset;
        double ddistInDays = (double) distInMillisec / (double) CompanyCalendar.MILLIS_IN_DAY;
        int distInDays = (int) ddistInDays;
        long distInWeek = (distInDays / 7);
        int freq = (this.getFreq() > 0 ? this.getFreq() : 1);
        int rep = ((int) distInWeek / freq) / Math.max(dayArray.length,1);
        if (this.getRepeat() == 0 || rep <= (this.getRepeat() - 1)) {
          cal.setTime(getStart());
          cal.add(Calendar.WEEK_OF_YEAR, rep * freq);
          long next = cal.getTimeInMillis();
          for (int i = 0; i < dayArray.length; i++) {
            cal.set(Calendar.DAY_OF_WEEK, dayArray[i]);
            if (cal.getTimeInMillis() > next && beforeTime >= cal.getTimeInMillis())
              next = cal.getTimeInMillis();
          }
          returnTime = next;
        }
      } else {
        returnTime = getEnd().getTime() - getDuration();
      }
    }
    return returnTime;
  }*/

  public long getPreviousFireTimeBefore(long beforeTime) {
    long returnTime = Long.MIN_VALUE;
    if (beforeTime > getStart().getTime()) {
      if (beforeTime <= getEnd().getTime()) {

        CompanyCalendar cal = new CompanyCalendar(getStart());
        TimeZone timeZone = cal.getTimeZone();
        int ofset = (timeZone.getOffset(getStart().getTime()) - timeZone.getOffset(beforeTime)); // questo server per calcolare i millisecondi effettivi in caso di ora legale

        cal.set(CompanyCalendar.DAY_OF_WEEK, cal.getFirstDayOfWeek());
        cal.setAndGetTimeToDayStart();
        long monThis = cal.getTimeInMillis();


        cal.setTimeInMillis(beforeTime);
        cal.set(CompanyCalendar.DAY_OF_WEEK, cal.getFirstDayOfWeek());
        cal.setAndGetTimeToDayStart();
        long monBefor = cal.getTimeInMillis();

        long distInMillisec = monBefor - monThis - ofset;

        double ddistInDays = (double) distInMillisec / (double) CompanyCalendar.MILLIS_IN_DAY;

        int distInDays = (int) ddistInDays;
        long distInWeek = (distInDays / 7);
        int freq = (this.getFreq() > 0 ? this.getFreq() : 1);
        int jumpInWeeks = ((int) distInWeek / freq);

        // zomp in the correct week
        cal.setTimeInMillis(monThis);
        cal.set(CompanyCalendar.DAY_OF_WEEK, cal.getFirstDayOfWeek());
        cal.add(Calendar.WEEK_OF_YEAR, jumpInWeeks * freq);
        long next = Long.MIN_VALUE;
        cal.setMillisFromMidnight(getStartTimeInMillis());
        for (int i = 0; i < dayArray.length; i++) {
          cal.set(Calendar.DAY_OF_WEEK, dayArray[i]);
          if (cal.getTimeInMillis() < beforeTime && cal.getTimeInMillis()>next)
            next = cal.getTimeInMillis();
        }
        if (next== Long.MIN_VALUE){
          //zomp back 1 freq
          cal.add(Calendar.WEEK_OF_YEAR, - freq);
          for (int i = 0; i < dayArray.length; i++) {
            cal.set(Calendar.DAY_OF_WEEK, dayArray[i]);
            if (cal.getTimeInMillis() < beforeTime && cal.getTimeInMillis()>next)
              next = cal.getTimeInMillis();
          }

        }
        returnTime = next;
        //}
      } else {
        returnTime = getEnd().getTime() - getDuration();
      }
    }
    return returnTime;
  }


  public String getScheduleDescription(String useSeparator) {
    String result = "";
    CompanyCalendar cal = new CompanyCalendar();
    cal.setTime(new Date());
    String week = "";
    for (int i : getDays()) {
      cal.set(Calendar.DAY_OF_WEEK, i);
      week = week + " " + DateUtilities.dateToString(cal.getTime(), "EEEE");
    }
    result = I18n.get("SCHEDULE_WEEKLY_CONTENT_%%...",
            DateUtilities.dateAndHourToFullString(getStartDate()),
            DateUtilities.dateAndHourToFullString(getEndDate()),
            week,
            getFrequency() + "",
            getRepetitions() + ""
    );
    return result;
  }



  ///todo è da implementare tenendo conto dei giorni della settimana
  private void calculateRepetitions() {
    double howManyDays=Math.round((endRecurr.getTime()-getValidityStartDate().getTime())/CompanyCalendar.MILLIS_IN_WEEK);
    int repeat = (int) (howManyDays / getFrequency());
    setRepeat(repeat<0?1:repeat);
    endRecurr=null; // si annulla in modo da non dover ricalcolare un'altra volta
  }


  public JSONObject jsonify() {
    JSONObject ret = super.jsonify();
    ret.element("type","weekly");
    ret.element("days",getDays());

    return ret;
  }

  public static ScheduleWeekly fromJSON(JSONObject json){
    JSONArray daysOb = json.getJSONArray("days");
    int[] days= new int[daysOb.size()];
    for (int i=0; i<daysOb.size(); i++ ){
      days[i]=daysOb.getInt(i);
    }

    ScheduleWeekly sd = new ScheduleWeekly(days,new Date(json.getLong("startMillis")), json.getInt("duration"), json.getInt("freq"), json.getInt("repeat"));
    return sd;
  }


  public String getName() {
    return "weekly";
  }


}

