package org.jblooming.agenda;

import org.jblooming.utilities.DateUtilities;

import java.util.Date;
import java.util.Locale;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 28-set-2005 : 9.54.16
 */
public class Scale {


  public int fieldToLoop = CompanyCalendar.MONTH;
  public int fieldIncrement = 1;
  public ScaleType scaleName = null;
  public String tickLabelDateFormat = "MMM";
  public String tickLabelDateFormatCompact="MM";
  public String superTickLabelScaleFormat = "yyyy";

  public long millisInPeriod = 0;

  public long startPointTime = new Date().getTime();
  public long endPointTime = new Date().getTime();

  public Date startPointDate = new Date();
  public Date endPointDate = new Date();

  private CompanyCalendar calS;
  private CompanyCalendar calE;

  public long millisInTick;


  public enum ScaleType {
    SCALE_1HOUR,SCALE_3HOUR, SCALE_6HOUR,SCALE_12HOUR, SCALE_1DAY, SCALE_1WEEK, SCALE_2WEEK, SCALE_3WEEK, SCALE_4WEEK, SCALE_1MONTH, SCALE_3MONTH, SCALE_1YEAR, SCALE_2YEAR,SCALE_5YEAR}

  /**
   * Private method use getScaleAndSynch
   *
   * @param scaleDescription
   * @param fieldToLoop
   * @param fieldIncrement
   * @param tickLabelDateFormat
   * @param tickLabelDateFormatCompact
   * @param superTickLabelScaleFormat
   * @param millisInPeriod
   * @param locale
   */
  private Scale(ScaleType scaleDescription, int fieldToLoop, int fieldIncrement, String tickLabelDateFormat, String tickLabelDateFormatCompact, String superTickLabelScaleFormat, long millisInPeriod, Locale locale) {

    this.calE = new CompanyCalendar(locale);
    this.calS = new CompanyCalendar(locale);
    this.fieldToLoop = fieldToLoop;
    this.fieldIncrement = fieldIncrement;
    this.tickLabelDateFormat = tickLabelDateFormat;
    this.superTickLabelScaleFormat = superTickLabelScaleFormat;
    this.scaleName = scaleDescription;
    this.millisInPeriod = millisInPeriod;
  }


  /**
   * @param scaleLevel          is the level require
   * @param currentTimeInMillis is the current time
   * @param synchExtremes       if true set the extremes to the beginning and ending millisedond for the period.
   * @param locale
   * @return the scale
   *         e.g: in month scale if the
   *         current millis is the day 4 of may, if synchExtremes=true return  start=1 may end=31 may. If currentTimeInMillis=false return start=19 april end=19 may
   */
  public static Scale getScaleAndSynch(ScaleType scaleLevel, long currentTimeInMillis, boolean synchExtremes, Locale locale) {
    Scale scale = getInstance(scaleLevel, locale);
    scale.resynch(currentTimeInMillis, synchExtremes);
    return scale;
  }


  public static Scale getScaleForPeriod(long min, long max, Locale locale) {
    Scale scale = getBestFittingScale(max - min, locale);
    scale.fitScaleToPeriod(min, max);
    return scale;
  }


  private void fitScaleToPeriod(long min, long max) {
    calS.setTimeInMillis(min);
    calE.setTimeInMillis(max);

    if (millisInPeriod >= CompanyCalendar.MILLIS_IN_HOUR) { //"SCALE_1HOUR"  wed 4 may 2005 9:45:30    -->  4 may 2005 9:00:00 --- wed 4 may 2005 10:00:00
      calS.set(CompanyCalendar.MILLISECOND, 0);
      calS.set(CompanyCalendar.SECOND, 0);
      calS.set(CompanyCalendar.MINUTE, 0);

      //calE.setTimeInMillis(calS.getTimeInMillis());
      calE.set(CompanyCalendar.MILLISECOND, 0);
      calE.set(CompanyCalendar.SECOND, 0);
      calE.set(CompanyCalendar.MINUTE, 0);

      calE.add(CompanyCalendar.HOUR_OF_DAY, 1);
      calE.add(CompanyCalendar.MILLISECOND, -1);
    }

    if (millisInPeriod >= CompanyCalendar.MILLIS_IN_3_HOUR) { //SCALE_3HOUR  wed 4 may 2005 9:45:30 --> wed 4 may 2005 9:00:00 --- wed 4 may 2005 12:00
      int sex = (calS.get(CompanyCalendar.HOUR_OF_DAY) / 3) * 3;
      calS.add(CompanyCalendar.HOUR_OF_DAY, sex - calS.get(CompanyCalendar.HOUR_OF_DAY));

      sex = (calE.get(CompanyCalendar.HOUR_OF_DAY) / 3) * 3;
      calE.add(CompanyCalendar.HOUR_OF_DAY, sex - calE.get(CompanyCalendar.HOUR_OF_DAY));
    }

    if (millisInPeriod >= CompanyCalendar.MILLIS_IN_6_HOUR) { //SCALE_6HOUR  wed 4 may 2005 9:45:30 --> wed 4 may 2005 6:00 --- wed 4 may 2005 12:00
      int sex = (calS.get(CompanyCalendar.HOUR_OF_DAY) / 6) * 6;
      calS.add(CompanyCalendar.HOUR_OF_DAY, sex - calS.get(CompanyCalendar.HOUR_OF_DAY));

      sex = (calE.get(CompanyCalendar.HOUR_OF_DAY) / 6) * 6;
      calE.add(CompanyCalendar.HOUR_OF_DAY, sex - calE.get(CompanyCalendar.HOUR_OF_DAY));

    }

    if (millisInPeriod >= CompanyCalendar.MILLIS_IN_12_HOUR) { //SCALE_12HOUR  wed 4 may 2005 9:45:30 --> wed 4 may 2005 0:00 --- wed 4 may 2005 12:00
      int sex = (calS.get(CompanyCalendar.HOUR_OF_DAY) / 12) * 12;
      calS.add(CompanyCalendar.HOUR_OF_DAY, sex - calS.get(CompanyCalendar.HOUR_OF_DAY));

      sex = (calE.get(CompanyCalendar.HOUR_OF_DAY) / 12) * 12;
      calE.add(CompanyCalendar.HOUR_OF_DAY, sex - calE.get(CompanyCalendar.HOUR_OF_DAY));

    }


    if (millisInPeriod >= CompanyCalendar.MILLIS_IN_DAY) { // SCALE_1DAY   -->  wed 4 may 2005 00:00 --- wed 4 may 2005 23:59:59.9999
      calS.setAndGetTimeToDayStart();
      calE.setAndGetTimeToDayEnd();
    }


    if (millisInPeriod >= CompanyCalendar.MILLIS_IN_WEEK) { // SCALE_1WEEK    -->  mon* 2 may 2005 00:00 --- sun* 8 may 2005 23:59:59.9999  *=according to settings
      calS.set(CompanyCalendar.DAY_OF_WEEK, calS.getFirstDayOfWeek());
      calE.set(CompanyCalendar.DAY_OF_WEEK, calE.getFirstDayOfWeek());
      calE.add(CompanyCalendar.DAY_OF_WEEK, 6);
    }

    if (millisInPeriod >= CompanyCalendar.MILLIS_IN_2_WEEK && (calE.getTimeInMillis()-calS.getTimeInMillis())<CompanyCalendar.MILLIS_IN_2_WEEK) { // SCALE_2WEEK -->  mon* 25 apr 2005 00:00 --- sun* 15 may 2005 23:59:59.9999  *=according to settings
      calS.add(CompanyCalendar.DATE, -7);
      calE.add(CompanyCalendar.DATE, 7);
    }

    if (millisInPeriod >= CompanyCalendar.MILLIS_IN_WEEK*3 && (calE.getTimeInMillis()-calS.getTimeInMillis())<CompanyCalendar.MILLIS_IN_WEEK*3) { // SCALE_3WEEK -->  mon* 25 apr 2005 00:00 --- sun* 15 may 2005 23:59:59.9999  *=according to settings
      calS.add(CompanyCalendar.DATE, -7);
      calE.add(CompanyCalendar.DATE, 7);
    }

    if (millisInPeriod >= CompanyCalendar.MILLIS_IN_WEEK*4 && (calE.getTimeInMillis()-calS.getTimeInMillis())<CompanyCalendar.MILLIS_IN_WEEK*4) { // SCALE_4WEEK -->  mon* 25 apr 2005 00:00 --- sun* 15 may 2005 23:59:59.9999  *=according to settings
      calS.add(CompanyCalendar.DATE, -7);
      calE.add(CompanyCalendar.DATE, 7);
    }

    if (millisInPeriod >= CompanyCalendar.MILLIS_IN_MONTH) { // SCALE_1MONTH
      calS.set(CompanyCalendar.DAY_OF_MONTH, 1);
      calE.set(CompanyCalendar.DAY_OF_MONTH, 1);
      calE.add(CompanyCalendar.MONTH, 1);
      calE.add(CompanyCalendar.DATE, -1);
    }

    if (millisInPeriod >= CompanyCalendar.MILLIS_IN_3_MONTH && (calE.getTimeInMillis()-calS.getTimeInMillis())<CompanyCalendar.MILLIS_IN_3_MONTH) { // SCALE_3MONTH
      int sex = calS.get(CompanyCalendar.MONTH) / 3;
      calS.set(CompanyCalendar.MONTH, sex * 3 + 1);
      sex = calE.get(CompanyCalendar.MONTH) / 3;
      calE.set(CompanyCalendar.MONTH, sex * 3 + 1);

    }

    if (millisInPeriod >= CompanyCalendar.MILLIS_IN_YEAR) { // SCALE_1YEAR
      calS.set(CompanyCalendar.MONTH, CompanyCalendar.JANUARY);
      calE.set(CompanyCalendar.MONTH, CompanyCalendar.DECEMBER);
      calE.set(CompanyCalendar.DAY_OF_MONTH, 31);
    }

    if (millisInPeriod >= CompanyCalendar.MILLIS_IN_2_YEAR && (calE.getTimeInMillis()-calS.getTimeInMillis())<CompanyCalendar.MILLIS_IN_2_YEAR) { // SCALE_2YEAR
      calS.add(CompanyCalendar.YEAR, -1);
      calE.add(CompanyCalendar.YEAR, 1);
    }

    if (millisInPeriod >= CompanyCalendar.MILLIS_IN_5_YEAR&& (calE.getTimeInMillis()-calS.getTimeInMillis())<CompanyCalendar.MILLIS_IN_5_YEAR) { // SCALE_5YEAR
      calS.add(CompanyCalendar.YEAR, -2);
      calE.add(CompanyCalendar.YEAR, 3);
    }

    startPointTime = calS.getTimeInMillis();
    startPointDate = calS.getTime();
    endPointTime = calE.getTimeInMillis();
    endPointDate = calE.getTime();

  }



  public static ScaleType getNextScale(ScaleType scale) {
    return getNextScale(scale, null);
  }

  public static ScaleType getNextScale(ScaleType scale, ScaleType maxScale) {
    int pos = scale.ordinal();
    int posm = ScaleType.values().length-1;
    if (maxScale != null)
      posm = maxScale.ordinal();
    return ScaleType.values()[(pos < posm ? pos+1 :posm)];
  }

  public static ScaleType getPrevScale(ScaleType scale) {
    return getPrevScale(scale, null);
  }

  public static ScaleType getPrevScale(ScaleType scale, ScaleType minScale) {
    int pos = scale.ordinal();
    int posm = 0;
    if (minScale != null)
      posm = minScale.ordinal();
    return ScaleType.values()[(pos > posm ? pos - 1 : posm)];
  }

  //  -------------------------------------------------   PRIVATE MOTHODS -------------------------------------------------

  private static Scale getInstance(ScaleType scaleLevel, Locale locale) {
    Scale scale = null;
    if (ScaleType.SCALE_1HOUR.equals(scaleLevel)) scale = new Scale(ScaleType.SCALE_1HOUR, CompanyCalendar.MINUTE, 5, "mm", "mm", "HH:mm", CompanyCalendar.MILLIS_IN_HOUR, locale);
    else if (ScaleType.SCALE_3HOUR.equals(scaleLevel)) scale = new Scale(ScaleType.SCALE_3HOUR, CompanyCalendar.MINUTE, 15, "HH:mm", "HH:mm", "EEEE dd MMMM yyyy", CompanyCalendar.MILLIS_IN_3_HOUR, locale);
    else if (ScaleType.SCALE_6HOUR.equals(scaleLevel)) scale = new Scale(ScaleType.SCALE_6HOUR, CompanyCalendar.MINUTE, 30, "HH:mm", "HH", "EEEE dd MMMM yyyy", CompanyCalendar.MILLIS_IN_6_HOUR, locale);
    else if (ScaleType.SCALE_12HOUR.equals(scaleLevel)) scale = new Scale(ScaleType.SCALE_12HOUR, CompanyCalendar.HOUR_OF_DAY, 1, "HH:mm", "HH", "EEEE dd MMMM yyyy", CompanyCalendar.MILLIS_IN_12_HOUR, locale);
    else if (ScaleType.SCALE_1DAY.equals(scaleLevel)) scale = new Scale(ScaleType.SCALE_1DAY, CompanyCalendar.HOUR_OF_DAY, 2, "HH:mm", "HH", "EEEE dd MMMM yyyy", CompanyCalendar.MILLIS_IN_DAY, locale);
    else if (ScaleType.SCALE_1WEEK.equals(scaleLevel)) scale = new Scale(ScaleType.SCALE_1WEEK, CompanyCalendar.DAY_OF_WEEK, 1, "EEE dd", "E dd", "MMMM yyyy", CompanyCalendar.MILLIS_IN_WEEK, locale);
    else if (ScaleType.SCALE_2WEEK.equals(scaleLevel)) scale = new Scale(ScaleType.SCALE_2WEEK, CompanyCalendar.DAY_OF_WEEK, 1, "dd", "dd", "MMMM yyyy", CompanyCalendar.MILLIS_IN_2_WEEK, locale);

    else if (ScaleType.SCALE_3WEEK.equals(scaleLevel)) scale = new Scale(ScaleType.SCALE_3WEEK, CompanyCalendar.DAY_OF_WEEK, 1, "dd", "dd", "MMMM yyyy", CompanyCalendar.MILLIS_IN_WEEK*3, locale);
    else if (ScaleType.SCALE_4WEEK.equals(scaleLevel)) scale = new Scale(ScaleType.SCALE_4WEEK, CompanyCalendar.DAY_OF_WEEK, 1, "dd", "dd", "MMMM yyyy", CompanyCalendar.MILLIS_IN_WEEK*4, locale);

    else if (ScaleType.SCALE_1MONTH.equals(scaleLevel)) scale = new Scale(ScaleType.SCALE_1MONTH, CompanyCalendar.DAY_OF_MONTH, 1, "dd", "dd", "MMMM yyyy", CompanyCalendar.MILLIS_IN_MONTH, locale);
    else if (ScaleType.SCALE_3MONTH.equals(scaleLevel)) scale = new Scale(ScaleType.SCALE_3MONTH, CompanyCalendar.WEEK_OF_MONTH, 1, "dd MMM", "dd MM", "yyyy", CompanyCalendar.MILLIS_IN_3_MONTH, locale);
    else if (ScaleType.SCALE_1YEAR.equals(scaleLevel)) scale = new Scale(ScaleType.SCALE_1YEAR, CompanyCalendar.MONTH, 1, "MMM yyyy", "MM", "yyyy", CompanyCalendar.MILLIS_IN_YEAR, locale);
    else if (ScaleType.SCALE_2YEAR.equals(scaleLevel)) scale =  new Scale(ScaleType.SCALE_2YEAR, CompanyCalendar.MONTH, 3, "MM yyyy", "MM", "yyyy", CompanyCalendar.MILLIS_IN_2_YEAR, locale);
    else scale = new Scale(ScaleType.SCALE_5YEAR, CompanyCalendar.YEAR, 1, "MM yyyy", "MM", "yyyy", CompanyCalendar.MILLIS_IN_5_YEAR, locale); //(ScaleType.SCALE_2YEAR.equals(scaleLevel))

    return scale;
  }

  private static Scale getBestFittingScale(long millisInPeriod, Locale locale) {
    Scale scale = null;
    if (millisInPeriod <= CompanyCalendar.MILLIS_IN_HOUR) scale = getInstance(ScaleType.SCALE_1HOUR, locale);
    else if (millisInPeriod <= CompanyCalendar.MILLIS_IN_3_HOUR) scale = getInstance(ScaleType.SCALE_3HOUR, locale);
    else if (millisInPeriod <= CompanyCalendar.MILLIS_IN_6_HOUR) scale = getInstance(ScaleType.SCALE_6HOUR, locale);
    else if (millisInPeriod <= CompanyCalendar.MILLIS_IN_12_HOUR) scale = getInstance(ScaleType.SCALE_12HOUR, locale);
    else if (millisInPeriod <= CompanyCalendar.MILLIS_IN_DAY) scale = getInstance(ScaleType.SCALE_1DAY, locale);
    else if (millisInPeriod <= CompanyCalendar.MILLIS_IN_WEEK) scale = getInstance(ScaleType.SCALE_1WEEK, locale);
    else if (millisInPeriod <= CompanyCalendar.MILLIS_IN_2_WEEK) scale = getInstance(ScaleType.SCALE_2WEEK, locale);
    else if (millisInPeriod <= CompanyCalendar.MILLIS_IN_MONTH) scale = getInstance(ScaleType.SCALE_1MONTH, locale);
    else if (millisInPeriod <= CompanyCalendar.MILLIS_IN_3_MONTH) scale = getInstance(ScaleType.SCALE_3MONTH, locale);
    else if (millisInPeriod <= CompanyCalendar.MILLIS_IN_YEAR) scale = getInstance(ScaleType.SCALE_1YEAR, locale);
    else if (millisInPeriod <= (CompanyCalendar.MILLIS_IN_YEAR *2 )) scale = getInstance(ScaleType.SCALE_2YEAR, locale);
    else scale = getInstance(ScaleType.SCALE_5YEAR, locale);

    return scale;
  }

  public long getMillisInTick() {
    if (ScaleType.SCALE_1HOUR.equals(this.scaleName))
      return CompanyCalendar.MILLIS_IN_MINUTE*this.fieldIncrement;
    else if (ScaleType.SCALE_3HOUR.equals(this.scaleName))
      return CompanyCalendar.MILLIS_IN_MINUTE*this.fieldIncrement;
    else if (ScaleType.SCALE_6HOUR.equals(this.scaleName))
      return CompanyCalendar.MILLIS_IN_MINUTE*this.fieldIncrement;
    else if (ScaleType.SCALE_12HOUR.equals(this.scaleName))
       return CompanyCalendar.MILLIS_IN_12_HOUR*this.fieldIncrement;
    else if (ScaleType.SCALE_1DAY.equals(this.scaleName))
      return CompanyCalendar.MILLIS_IN_12_HOUR*this.fieldIncrement;
    else if (ScaleType.SCALE_1WEEK.equals(this.scaleName))
      return CompanyCalendar.MILLIS_IN_DAY*this.fieldIncrement;
    else if (ScaleType.SCALE_2WEEK.equals(this.scaleName))
      return CompanyCalendar.MILLIS_IN_DAY*this.fieldIncrement;
    else if (ScaleType.SCALE_3WEEK.equals(this.scaleName))
      return CompanyCalendar.MILLIS_IN_DAY*this.fieldIncrement;
    else if (ScaleType.SCALE_4WEEK.equals(this.scaleName))
      return CompanyCalendar.MILLIS_IN_DAY*this.fieldIncrement;
    else if (ScaleType.SCALE_1MONTH.equals(this.scaleName))
      return CompanyCalendar.MILLIS_IN_DAY*this.fieldIncrement;
    else if (ScaleType.SCALE_3MONTH.equals(this.scaleName))
      return CompanyCalendar.MILLIS_IN_WEEK*this.fieldIncrement;
    else if (ScaleType.SCALE_1YEAR.equals(this.scaleName))
      return CompanyCalendar.MILLIS_IN_MONTH*this.fieldIncrement;
    else if (ScaleType.SCALE_2YEAR.equals(this.scaleName))
      return CompanyCalendar.MILLIS_IN_MONTH*this.fieldIncrement;
    else
      return CompanyCalendar.MILLIS_IN_YEAR*this.fieldIncrement;
  }


  private void resynch(long millis, boolean synchExtremes) {
    if (synchExtremes)
      synchExtremes(millis);
    else {
      moveExtremes(millis);
    }
  }


  private void synchExtremes(long millis) {
    long posS = 0;
    long posE = 0;
    calS.setTimeInMillis(millis);
    calE.setTimeInMillis(millis);

    if (millisInPeriod >= CompanyCalendar.MILLIS_IN_HOUR) { //"SCALE_1HOUR"  wed 4 may 2005 9:45:30    -->  4 may 2005 9:00:00 --- wed 4 may 2005 10:00:00
      calS.set(CompanyCalendar.MILLISECOND, 0);
      calS.set(CompanyCalendar.SECOND, 0);
      calS.set(CompanyCalendar.MINUTE, 0);
      calE.setTimeInMillis(calS.getTimeInMillis());
      calE.add(CompanyCalendar.HOUR_OF_DAY, 1);
      calE.add(CompanyCalendar.MILLISECOND, -1);
    }

    if (millisInPeriod >= CompanyCalendar.MILLIS_IN_3_HOUR) { //SCALE_3HOUR  wed 4 may 2005 9:45:30 --> wed 4 may 2005 9:00:00 --- wed 4 may 2005 12:00
      int sex = (calS.get(CompanyCalendar.HOUR_OF_DAY) / 3) * 3;
      calS.add(CompanyCalendar.HOUR_OF_DAY, sex - calS.get(CompanyCalendar.HOUR_OF_DAY));
      calE.setTimeInMillis(calS.getTimeInMillis());
      calE.add(CompanyCalendar.HOUR_OF_DAY, 3);
      calE.add(CompanyCalendar.MILLISECOND, -1);
    }

    if (millisInPeriod >= CompanyCalendar.MILLIS_IN_6_HOUR) { //SCALE_6HOUR  wed 4 may 2005 9:45:30 --> wed 4 may 2005 6:00 --- wed 4 may 2005 12:00
      int sex = (calS.get(CompanyCalendar.HOUR_OF_DAY) / 6) * 6;
      calS.add(CompanyCalendar.HOUR_OF_DAY, sex - calS.get(CompanyCalendar.HOUR_OF_DAY));
      calE.setTimeInMillis(calS.getTimeInMillis());
      calE.add(CompanyCalendar.HOUR_OF_DAY, 6);
      calE.add(CompanyCalendar.MILLISECOND, -1);
    }

    if (millisInPeriod >= CompanyCalendar.MILLIS_IN_12_HOUR) { //SCALE_12HOUR  wed 4 may 2005 9:45:30 --> wed 4 may 2005 0:00 --- wed 4 may 2005 12:00
      int sex = (calS.get(CompanyCalendar.HOUR_OF_DAY) / 12) * 12;
      calS.add(CompanyCalendar.HOUR_OF_DAY, sex - calS.get(CompanyCalendar.HOUR_OF_DAY));
      calE.setTimeInMillis(calS.getTimeInMillis());
      calE.add(CompanyCalendar.HOUR_OF_DAY, 12);
      calE.add(CompanyCalendar.MILLISECOND, -1);
    }


    if (millisInPeriod >= CompanyCalendar.MILLIS_IN_DAY) { // SCALE_1DAY   -->  wed 4 may 2005 00:00 --- wed 4 may 2005 23:59:59.9999
      calS.setAndGetTimeToDayStart();
      calE.setAndGetTimeToDayEnd();
      // store the day start,end
      posS = calS.getTimeInMillis();
      posE = calE.getTimeInMillis();
    }


    if (millisInPeriod >= CompanyCalendar.MILLIS_IN_WEEK) { // SCALE_1WEEK    -->  mon* 2 may 2005 00:00 --- sun* 8 may 2005 23:59:59.9999  *=according to settings
      calS.set(CompanyCalendar.DAY_OF_WEEK, calE.getFirstDayOfWeek());
      calE.set(CompanyCalendar.DAY_OF_WEEK, calE.getFirstDayOfWeek());
      calE.add(CompanyCalendar.DAY_OF_WEEK, 6);

    }

    if (millisInPeriod >= CompanyCalendar.MILLIS_IN_2_WEEK) { // SCALE_2WEEK -->  mon* 25 apr 2005 00:00 --- sun* 15 may 2005 23:59:59.9999  *=according to settings
      calE.add(CompanyCalendar.DATE, 7);
    }

    if (millisInPeriod >= CompanyCalendar.MILLIS_IN_WEEK*3) { // SCALE_3WEEK -->  mon* 25 apr 2005 00:00 --- sun* 15 may 2005 23:59:59.9999  *=according to settings
      calE.add(CompanyCalendar.DATE, 7);
    }

    if (millisInPeriod >= CompanyCalendar.MILLIS_IN_WEEK*4) { // SCALE_4WEEK -->  mon* 25 apr 2005 00:00 --- sun* 15 may 2005 23:59:59.9999  *=according to settings
      calE.add(CompanyCalendar.DATE, 7);
    }

    if (millisInPeriod >= CompanyCalendar.MILLIS_IN_MONTH) { // SCALE_1MONTH
      //reposition to start day
      calS.setTimeInMillis(posS);
      calE.setTimeInMillis(posS);
      calS.set(CompanyCalendar.DAY_OF_MONTH, 1);
      calE.set(CompanyCalendar.DAY_OF_MONTH, 1);
      calE.add(CompanyCalendar.MONTH, 1);
      calE.add(CompanyCalendar.MILLISECOND, -1);
    }

    if (millisInPeriod >= CompanyCalendar.MILLIS_IN_3_MONTH) { // SCALE_3MONTH
      int sex = calS.get(CompanyCalendar.MONTH) / 3;
      calS.set(CompanyCalendar.MONTH, sex * 3 + 1);
      calE.set(CompanyCalendar.DAY_OF_MONTH, 1);
      calE.set(CompanyCalendar.MONTH, sex * 3 + 4);
      calE.add(CompanyCalendar.DAY_OF_MONTH, -1);
    }

    if (millisInPeriod >= CompanyCalendar.MILLIS_IN_YEAR) { // SCALE_1YEAR
      calS.set(CompanyCalendar.MONTH, CompanyCalendar.JANUARY);
      calS.set(CompanyCalendar.DAY_OF_MONTH, 1);
      calE.set(CompanyCalendar.MONTH, CompanyCalendar.DECEMBER);
      calE.set(CompanyCalendar.DAY_OF_MONTH, 31);
    }

    if (millisInPeriod >= CompanyCalendar.MILLIS_IN_2_YEAR) { // SCALE_2YEAR
      calS.add(CompanyCalendar.YEAR, -1);
      calE.add(CompanyCalendar.YEAR, 1);
    }

    if (millisInPeriod >= CompanyCalendar.MILLIS_IN_5_YEAR) { // SCALE_5YEAR
      calS.add(CompanyCalendar.YEAR, -2);
      calE.add(CompanyCalendar.YEAR, 3);
    }

    startPointTime = calS.getTimeInMillis();
    startPointDate = calS.getTime();
    endPointTime = calE.getTimeInMillis();
    endPointDate = calE.getTime();


  }


  private void moveExtremes(long millis) {
    calS.setTimeInMillis(millis - millisInPeriod / 2);
    //calE.setTimeInMillis(millis + millisInPeriod / 2);

    // in any case resynch to the tick
    resetLesserFields(calS, fieldToLoop);
    calE.setTimeInMillis(calS.getTimeInMillis() + millisInPeriod);

    startPointTime = calS.getTimeInMillis();
    startPointDate = calS.getTime();
    endPointTime = calE.getTimeInMillis();
    endPointDate = calE.getTime();
  }


  private void resetLesserFields(CompanyCalendar cc, int field) {

    // ever reset millis and seconds
    cc.set(CompanyCalendar.MILLISECOND, 0);
    cc.set(CompanyCalendar.SECOND, 0);


    // normalize the calendar to the less field step. E.g.: if the field is MINUTE and  the increment is 15   9.34 -> 9.30
    cc.set(field, (cc.get(field)/fieldIncrement)*fieldIncrement );

    if (CompanyCalendar.MINUTE == field) {

    } else if (CompanyCalendar.HOUR_OF_DAY == field) {
      cc.set(CompanyCalendar.MINUTE, 0);

    } else if (CompanyCalendar.DAY_OF_WEEK == field) {
      cc.set(CompanyCalendar.MINUTE, 0);
      cc.set(CompanyCalendar.HOUR_OF_DAY, 0);

    } else if (CompanyCalendar.DAY_OF_MONTH == field) {
      cc.set(CompanyCalendar.MINUTE, 0);
      cc.set(CompanyCalendar.HOUR_OF_DAY, 0);

    } else if (CompanyCalendar.WEEK_OF_MONTH == field) {
      cc.set(CompanyCalendar.MINUTE, 0);
      cc.set(CompanyCalendar.HOUR_OF_DAY, 0);

    } else if (CompanyCalendar.MONTH == field) {
      cc.set(CompanyCalendar.MINUTE, 0);
      cc.set(CompanyCalendar.HOUR_OF_DAY, 0);
      cc.set(CompanyCalendar.DAY_OF_MONTH, 1);


    }
  }

  public Period getPeriod() {
    return new Period(startPointTime,endPointTime);
  }

  public int getSuperScaleField(){
    if (scaleName.equals(ScaleType.SCALE_1HOUR) )
      return CompanyCalendar.HOUR_OF_DAY ;
    else if (scaleName.equals(ScaleType.SCALE_3HOUR) ||
        scaleName.equals(ScaleType.SCALE_6HOUR) ||
        scaleName.equals(ScaleType.SCALE_12HOUR) )
      return CompanyCalendar.DAY_OF_MONTH;
    else if (scaleName.equals(ScaleType.SCALE_1DAY) ||
            scaleName.equals(ScaleType.SCALE_1WEEK ) ||
            scaleName.equals(ScaleType.SCALE_2WEEK ) ||
            scaleName.equals(ScaleType.SCALE_3WEEK ) ||
            scaleName.equals(ScaleType.SCALE_4WEEK ) )
      return CompanyCalendar.MONTH ;
    else if (scaleName.equals(ScaleType.SCALE_1MONTH) ||
            scaleName.equals(ScaleType.SCALE_3MONTH ) ||
            scaleName.equals(ScaleType.SCALE_1YEAR ) )
      return CompanyCalendar.YEAR ;
    else
      return CompanyCalendar.YEAR;

  }


  public String getJSIncrementField(){
    switch (fieldToLoop) {
      case CompanyCalendar.YEAR: return "y";
      case CompanyCalendar.MONTH: return"M";
      case CompanyCalendar.WEEK_OF_YEAR: return"w";
      case CompanyCalendar.DATE: return"d";
      case CompanyCalendar.HOUR: return"h";
      case CompanyCalendar.MINUTE: return"m";
      case CompanyCalendar.SECOND: return"s";
    }
    return "M";
  }


}
