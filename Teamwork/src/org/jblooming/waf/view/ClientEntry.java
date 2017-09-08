package org.jblooming.waf.view;

import org.jblooming.ontology.Hidrator;
import org.jblooming.ontology.SerializedList;
import org.jblooming.ontology.SerializedMap;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.NumberUtilities;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.utilities.math.MathParse;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.constants.FieldErrorConstants;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.settings.I18n;
import org.jblooming.agenda.CompanyCalendar;
import org.apache.commons.validator.UrlValidator;
import org.apache.commons.validator.EmailValidator;


import java.text.*;
import java.util.*;


public class ClientEntry {

  public String name;
  private String value;
  public String errorCode = null;

  public boolean required = false;

  private boolean fillErrorCodeOnError = true;

  public String suggestedValue;

  //mark the businessLogic so that PageState.saveEntriesInDefaults will persist it in options if called on same pageState
  public boolean persistInOptions = false;


  public ClientEntry(String name, int value) {
    this(name, value + "");
  }


  public ClientEntry(String name, String value) {

    this.name = name;
    this.setValue(value);

    this.suggestedValue = suggestedValue;
  }

  public ClientEntry getNewInstance() {
    return new ClientEntry(name, ((value != null) ? new String(value) : null));
  }

  public String stringValueNullIfEmpty() {
    String ret = null;
    if (!(value != null && value.trim().length() == 0))
      ret = value;
    return ret;
  }

  public String stringValue() throws ActionException {
    if (required && !isFilled()) {
      if (fillErrorCodeOnError)
        errorCode = FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY;
      throw new ActionException(FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY+" "+name);
    }
    return value;
  }

  @Deprecated
  public SerializedList listValue() throws ActionException {
    String value = stringValue();
    return SerializedList.deserialize(value);
  }

  @Deprecated
  public List listValue(Hidrator hidrator) throws ActionException {
    String value = stringValue();
    List returnValues = new ArrayList();
    List<String> values = SerializedList.deserialize(value);
    for (String s : values) {
      returnValues.add(hidrator.hidrate(s));
    }
    return returnValues;
  }

  @Deprecated
  public SerializedMap mapValue() throws ActionException {
    String value = stringValue();
    return SerializedMap.deserialize(value);
  }

  public int intValue() throws ActionException, ParseException {
    int result = 0;
    if (required && !isFilled()) {
      if (fillErrorCodeOnError)
        errorCode = FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY;
      throw new ActionException(FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY+" "+name);
    }

    if (isFilled())
      try {
        result = Integer.parseInt(value);
      } catch (NumberFormatException e) {
        result = (int) processEquation();
      }
    else
      throw new ParseException("Null value not allowed", 0);

    //computed value is copied to ce
    value = result + "";

    return result;
  }

  public int intValueNoErrorCodeNoExc() {
    fillErrorCodeOnError = false;
    try {
      return intValue();
    } catch (ParseException e) {
    } catch (ActionException e) {
    }
    return 0;
  }


  public long longValueNoErrorNoCatchedExc() {
    fillErrorCodeOnError = false;
    long ret = 0;
    try {
      return longValue();
    } catch (ActionException e) {
    } catch (ParseException e) {
    }
    return ret;
  }

  public long longValue() throws ActionException, ParseException {
    long result = 0L;
    if (required && !isFilled()) {
      if (fillErrorCodeOnError)
        errorCode = FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY;
      throw new ActionException(FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY+" "+name);
    }
    if (isFilled())
      try {
        result = Long.parseLong(value);
      } catch (NumberFormatException e) {
        result = (long) processEquation();
      }
    else
      throw new ParseException("Null value not allowed", 0);

    //computed value is copied to ce
    value = result + "";

    return result;
  }

  public double doubleValue() throws ActionException, ParseException {
    return doubleValue(NumberUtilities.DEFAULT_DECIMAL_PLACES);
  }

  public double doubleValue(int decimalPlaces) throws ActionException, ParseException {
    double result = 0.0;
    if (required && !isFilled()) {
      if (fillErrorCodeOnError)
        errorCode = FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY;
      throw new ActionException(FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY+" "+name);
    }
    if (isFilled()) {
      ParsePosition pp = new ParsePosition(0);
      NumberFormat instance = NumberFormat.getInstance(SessionState.getLocale());
      //result=Double.parseDouble(value);
      Number num = instance.parse(value, pp);
      if (num == null) {
        result = processEquation();
      } else {
        result = num.doubleValue();
      }
      if (pp.getIndex() < value.length()) {
        result = processEquation();
      }
    } else
      throw new ParseException("Null value not allowed", 0);

    //computed value is copied to ce
    value = NumberUtilities.decimalNoGrouping(result, decimalPlaces);

    return result;
  }

  public double doubleValueNoErrorNoCatchedExc() {
    fillErrorCodeOnError = false;
    try {
      return doubleValue();
    } catch (ActionException e) {
    } catch (ParseException e) {
    }
    return 0;
  }

  public double currencyValue() throws ActionException, ParseException {
    double result = 0.0;
    if (required && !isFilled()) {
      if (fillErrorCodeOnError)
        errorCode = FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY;
      throw new ActionException(FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY+" "+name);
    }
    if (isFilled()) {
      try {
        result=NumberUtilities.parseCurrency(value);
      } catch (NumberFormatException e) {
        result = processEquation();
      }
    } else
      throw new ParseException("Null value not allowed", 0);

    //computed value is copied to ce
    value = NumberUtilities.currency(result);

    return result;
  }

  public double currencyValueNoErrorNoCatchedExc() {
    fillErrorCodeOnError = false;
    try {
      return currencyValue();
    } catch (ActionException e) {
    } catch (ParseException e) {
    }
    return 0;
  }


  private double processEquation() throws ParseException {
    double result;

    String reworkedValue = StringUtilities.replaceAllNoRegex(value, new DecimalFormatSymbols().getGroupingSeparator() + "", "");
    reworkedValue = StringUtilities.replaceAllNoRegex(reworkedValue, new DecimalFormatSymbols().getDecimalSeparator() + "", ".");

    try {
      result = MathParse.parse(reworkedValue);
    } catch (ParseException e1) {
      if (fillErrorCodeOnError)
        errorCode = FieldErrorConstants.ERR_FIELD_MUST_BE_NUMERIC;
      throw e1;
    }
    return result;
  }


  public Date dateValue() throws ActionException, ParseException {
    return dateValue(null,null);
  }

  public Date dateValue(String format, Locale locale) throws ActionException, ParseException {
    return dateValue(format,locale,null);
  }

  public Date dateValue(String format, Locale locale,TimeZone tz) throws ActionException, ParseException {
    Date result = null;
    if (required && !isFilled()) {
      if (fillErrorCodeOnError)
        errorCode = FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY;
      throw new ActionException(FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY+" "+name);
    }
    if (isFilled())
      try {        
        result = DateUtilities.dateFromString(value,format);
      } catch (ParseException e) {
        if (fillErrorCodeOnError)
          errorCode = FieldErrorConstants.ERR_FIELD_MUST_BE_DATELIKE;
        throw e;
      }
    return result;
  }


  public Date dateValueNoErrorNoCatchedExc() {
    return dateValueNoErrorNoCatchedExc(null,null);
  }
  
  public Date dateValueNoErrorNoCatchedExc(String format, Locale locale) {
    return dateValueNoErrorNoCatchedExc(format, locale,null);
  }
  
  public Date dateValueNoErrorNoCatchedExc(String format, Locale locale, TimeZone tz) {
    fillErrorCodeOnError = false;
    try {
      return dateValue( format,  locale,tz);
    } catch (ActionException e) {
    } catch (ParseException e) {
    }
    return null;
  }

  public long timeValueInMillis() throws ActionException, ParseException {
    long result = 0;
    if (required && !isFilled()) {
      if (fillErrorCodeOnError)
        errorCode = FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY;
      throw new ActionException(FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY+" "+name);
    }

    if (value != null && value.trim().length() > 0) {

      try {
        result = DateUtilities.millisFromHourMinuteSmart(value);

      } catch (NumberFormatException e) {
        try {
          result= (long) (processEquation()* CompanyCalendar.MILLIS_IN_HOUR);
        } catch (ParseException e1) {
          if (fillErrorCodeOnError)
            errorCode = FieldErrorConstants.ERR_FIELD_MUST_BE_TIME;
          throw new ParseException(FieldErrorConstants.ERR_FIELD_MUST_BE_TIME+" "+name, 0);
        }      
      }


    } else
      throw new ParseException("Null value not allowed: "+name, 0);

    return result;
  }

  /**
   * @param considerWorkingDay if true a day is 8 hours otherwise is 24h
   * @return
   */
  public long durationInWorkingMillisNoErrorNoCatchedExc(boolean considerWorkingDay) {
    long ret=0;
    try {
      ret=durationInWorkingMillis(considerWorkingDay);
    } catch (Throwable e) {}
    return ret;
  }
  public long durationInWorkingMillis(boolean considerWorkingDay) throws ActionException, ParseException {
    long result = 0;
    if (required && !isFilled()) {
      if (fillErrorCodeOnError)
        errorCode = FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY;
      throw new ActionException(FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY+" "+name);
    }

    if (value != null && value.trim().length() > 0) {

      try {
        result = DateUtilities.millisFromString(value,considerWorkingDay);

      } catch (NumberFormatException e) {
        try {
          result= (long) (processEquation()* CompanyCalendar.MILLIS_IN_HOUR);
        } catch (ParseException e1) {
          if (fillErrorCodeOnError)
            errorCode = FieldErrorConstants.ERR_FIELD_MUST_BE_TIME;
          throw new ParseException(FieldErrorConstants.ERR_FIELD_MUST_BE_TIME+" "+name, 0);
        }
      }


    } else
      throw new ParseException("Null value not allowed: "+name, 0);

    return result;
  }

  /**
   * @param considerWorkingDay if true a week is 5 days (depending on config) 7 otherwise
   * @return
   * @throws ActionException
   * @throws ParseException
   */
  public int durationInWorkingDaysNoErrorNoCatchedExc(boolean considerWorkingDay)  {
      fillErrorCodeOnError = false;
      int ret=0;
      try {
        ret=durationInWorkingDays(considerWorkingDay);
      } catch (Throwable e) {}
      return ret;
  }

  public int durationInWorkingDays(boolean considerWorkingDay) throws ActionException, ParseException {
    int result = 0;
    if (required && !isFilled()) {
      if (fillErrorCodeOnError)
        errorCode = FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY;
      throw new ActionException(FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY+" "+name);
    }

    if (value != null && value.trim().length() > 0) {

      try {
        result = DateUtilities.daysFromString(value,considerWorkingDay);

      } catch (NumberFormatException e) {
        try {
          result= (int) (processEquation());
        } catch (ParseException e1) {
          if (fillErrorCodeOnError)
            errorCode = FieldErrorConstants.ERR_FIELD_MUST_BE_NUMERIC;
          throw new ParseException(FieldErrorConstants.ERR_FIELD_MUST_BE_NUMERIC+" "+name, 0);
        }
      }


    } else
      throw new ParseException("Null value not allowed: "+name, 0);

    return result;
  }

  public boolean checkFieldValue() {
    return Fields.TRUE.equalsIgnoreCase(value) || "true".equalsIgnoreCase(value);
  }

  public String checkFieldHtmlValue() {
    if (checkFieldValue())
      return Fields.TRUE;
    else
      return Fields.FALSE;
  }


  public String emailValue() throws ActionException {
    if (required && !isFilled()) {
      if (fillErrorCodeOnError)
        errorCode = FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY;
      throw new ActionException(FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY + " " + name);
    } else if (isFilled() && !EmailValidator.getInstance().isValid(value)) {
      errorCode = FieldErrorConstants.ERR_FIELD_MUST_BE_EMAIL;
      throw new ActionException(FieldErrorConstants.ERR_FIELD_MUST_BE_EMAIL + " " + name);
    }
    return value;
  }

  public String urlValue() throws ActionException {
    if (required && !isFilled()) {
      if (fillErrorCodeOnError)
        errorCode = FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY;
      throw new ActionException(FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY + " " + name);
    } else if (isFilled()) {

      if (!value.startsWith("http://") && !value.startsWith("https://"))
        value = "http://"+value;
      if (!new UrlValidator(UrlValidator.ALLOW_ALL_SCHEMES).isValid(value)) {
        errorCode = I18n.get(FieldErrorConstants.ERR_FIELD_MUST_BE_URL);
        throw new ActionException(FieldErrorConstants.ERR_FIELD_MUST_BE_URL + " " + name);
      }
    }
    return value;
  }


  public void setValue(String value) {
    this.value = value;
  }

  public boolean isFilled() {
    return value != null && value.trim().length() > 0;
  }

//  ---------------------------------------------------------------------------  deprecated methods ---------------------------------------------------------------

  public String toString(){
    return name+":"+stringValueNullIfEmpty();
  }

}
