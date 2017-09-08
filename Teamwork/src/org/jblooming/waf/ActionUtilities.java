package org.jblooming.waf;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.oql.QueryHelper;
import org.jblooming.ontology.Identifiable;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.ontology.PersistentText;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.hibernate.HibernateFactory;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.input.Uploader;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.RestState;

import java.lang.reflect.Field;
import java.text.ParseException;
import java.util.Date;


/**
 * If ce.value arrives empty and is not required, the value is reset
 * <p/>
 * if you want to check whether the update was attempted, check the return value
 * <p/>
 * in case of error, the client businessLogic error code is reset with the current value
 *
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class ActionUtilities {


  public static boolean setString(ClientEntry ce, Object mainObject, String propertyName) {
    return setString(ce, mainObject,propertyName,false);
  }

  public static boolean setString(ClientEntry ce, Object mainObject, String propertyName,boolean toUpperCase) {

    String value = null;
    boolean ok = true;

    try {
      if(toUpperCase)
        value = ce.stringValue().toUpperCase();
      else
      value = ce.stringValue();
      setFieldValue(propertyName, mainObject, value);
    } catch (ActionException e) {
      ok = false;
    }
    return ok;
  }

  public static boolean setDate(ClientEntry ce, Object mainObject, String propertyName) {

    Date date = null;
    boolean ok = true;

    try {
      date = ce.dateValue();
      setFieldValue(propertyName, mainObject, date);
    } catch (ActionException e) {
      ok = false;
    } catch (ParseException e) {
      ok = false;
    }
    return ok;
  }

  public static boolean setBoolean(ClientEntry ce, Object mainObject, String propertyName) {
    boolean value;
    boolean ok = true;
    value = ce.checkFieldValue();
    setFieldValue(propertyName, mainObject, value);
    return ok;
  }

  public static boolean setInt(ClientEntry ce, Object mainObject, String propertyName) {
    int value;

    boolean ok = true;

    try {
      value = ce.intValue();
      setFieldValue(propertyName, mainObject, value);
    } catch (ActionException e) {
      ok = false;
    } catch (ParseException e) {
      ok = false;
    }

    if (!ok && !ce.required && ce.stringValueNullIfEmpty() == null)
      setFieldValue(propertyName, mainObject, 0);

    return ok;
  }


  public static boolean setDouble(ClientEntry ce, Object mainObject, String propertyName) {

    double value;
    boolean ok = true;

    try {
      value = ce.doubleValue();
      setFieldValue(propertyName, mainObject, value);
    } catch (ActionException e) {
      ok = false;
    } catch (ParseException e) {
      ok = false;
    }

    if (!ok && !ce.required && ce.stringValueNullIfEmpty() == null)
      setFieldValue(propertyName, mainObject, 0d);

    return ok;
  }

  public static boolean setLong(ClientEntry ce, Object mainObject, String propertyName) {

    long value;
    boolean ok = true;

    try {
      value = ce.longValue();
      setFieldValue(propertyName, mainObject, value);
    } catch (ActionException e) {
      ok = false;
    } catch (ParseException e) {
      ok = false;
    }

    if (!ok && !ce.required && ce.stringValueNullIfEmpty() == null)
      setFieldValue(propertyName, mainObject, 0);

    return ok;

  }

  public static boolean setTime(ClientEntry ce, Object mainObject, String propertyName) {
    long value;
    boolean ok = true;

    try {
      value = ce.timeValueInMillis();
      setFieldValue(propertyName, mainObject, value);
    } catch (ActionException e) {
      ok = false;
    } catch (ParseException e) {
      ok = false;
    }

    if (!ok && !ce.required && ce.stringValueNullIfEmpty() == null)
      setFieldValue(propertyName, mainObject, 0);

    return ok;
  }

  private static void setFieldValue(String propertyName, Object mainObject, Object value) {
    try {
      ReflectionUtilities.setFieldValue(propertyName, mainObject, value);
    } catch (Exception e) {
      throw new PlatformRuntimeException(e);
    }
  }

//   public static boolean setObject(ClientEntry ce, Object mainObject, String propertyName) throws FindByPrimaryKeyException {
//   }

  public static boolean setCurrency(ClientEntry ce, Object mainObject, String propertyName) {
    double value;
    boolean ok = true;

    try {
      value = ce.currencyValue();
      setFieldValue(propertyName, mainObject, value);
    } catch (ActionException e) {
      ok = false;
    } catch (ParseException e) {
      ok = false;
    }

    if (!ok && !ce.required && ce.stringValueNullIfEmpty() == null)
      setFieldValue(propertyName, mainObject, 0);

    return ok;
  }

  public static boolean setDurationInMillis(ClientEntry ce, boolean considerWorkingdays, Object mainObject, String propertyName) {
    long value;
    boolean ok = true;

    try {
      value = ce.durationInWorkingMillis(considerWorkingdays);
      setFieldValue(propertyName, mainObject, value);
    } catch (ActionException e) {
      ok = false;
    } catch (ParseException e) {
      ok = false;
    }

    if (!ok && !ce.required && ce.stringValueNullIfEmpty() == null)
      setFieldValue(propertyName, mainObject, 0);

    return ok;
  }


  public static boolean setDurationInDays(ClientEntry ce, boolean considerWorkingdays, Object mainObject, String propertyName) {
    int value;
    boolean ok = true;

    try {
      value = ce.durationInWorkingDays(considerWorkingdays);
      setFieldValue(propertyName, mainObject, value);
    } catch (ActionException e) {
      ok = false;
    } catch (ParseException e) {
      ok = false;
    }

    if (!ok && !ce.required && ce.stringValueNullIfEmpty() == null)
      setFieldValue(propertyName, mainObject, 0);

    return ok;
  }

  public static boolean setIdentifiable(ClientEntry ce, Object mainObject, String propertyName, PersistenceContext pc) throws FindByPrimaryKeyException {
    Object value = null;
    boolean ok = true;

    try {
      String entityId = ce.stringValue();
      if (entityId != null && entityId.trim().length() > 0) {
        Field field = ReflectionUtilities.getField(propertyName, ReflectionUtilities.getUnderlyingObjectClass(mainObject));
        if (field == null)
          throw new PlatformRuntimeException("propertyName '" + propertyName + "' not found on " + mainObject.getClass());
        if (pc != null)
          value = HibernateFactory.findByPrimaryKey(field.getType(), entityId, pc);
        else
          value = HibernateFactory.findByPrimaryKey(field.getType(), entityId);
      }

      try {
        ReflectionUtilities.setFieldValue(propertyName, mainObject, value);
      } catch (Exception e) {
        throw new PlatformRuntimeException(e);
      }

    } catch (ActionException e) {
      ok = false;
    }

    return ok;
  }

  public static boolean setIdentifiable(ClientEntry ce, Object mainObject, String propertyName) throws FindByPrimaryKeyException {
    return setIdentifiable(ce, mainObject, propertyName, null);

  }

  public static boolean setText(ClientEntry ce, Object mainObject, String propertyName) {

    Object value = null;
    boolean ok = true;

    try {
      String text = ce.stringValue();

      Field field = ReflectionUtilities.getField(propertyName, ReflectionUtilities.getUnderlyingObjectClass(mainObject));
      if (field == null)
        throw new PlatformRuntimeException("propertyName '" + propertyName + "' not found on " + mainObject.getClass());
      field.setAccessible(true);
      PersistentText pt = (PersistentText) field.get(mainObject);
      if (pt == null)
        pt = new PersistentText();

      pt.setText(text);
      try {
        ReflectionUtilities.setFieldValue(propertyName, mainObject, pt);
      } catch (Exception e) {
        throw new PlatformRuntimeException(e);
      }
    } catch (IllegalAccessException e1) {
      throw new PlatformRuntimeException(e1);
    } catch (ActionException e1) {
      throw new PlatformRuntimeException(e1);
    }
    return ok;

  }

  /**
   * somethingSearched = somethingSearched || ActionUtilities.addQBEClause("ISSUE_DESCRIPTION","issue.description", "description",qhelp,QueryHelper.TYPE_CLOB,pageState);
   */
  public static boolean addQBEClauseForJoinedEntity(String fieldName, String objAliasDotPropertyName, String alias, QueryHelper qhelp, String qbeType, RestState pageState) {
    String filter = pageState.getEntry(fieldName).stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addQBEClauseForJoinedEntity(objAliasDotPropertyName, alias, filter, qbeType);
      pageState.getEntry(fieldName).errorCode = qhelp.getImproperUseOfQBEErrorCode();
      return true;
    } else
      return false;
  }
  public static boolean addQBEClause(String fieldName, String objAliasDotPropertyName, String alias, QueryHelper qhelp, String qbeType, RestState pageState) {
    String filter = pageState.getEntry(fieldName).stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addQBEClause(objAliasDotPropertyName, alias, filter, qbeType);
      pageState.getEntry(fieldName).errorCode = qhelp.getImproperUseOfQBEErrorCode();
      return true;
    } else
      return false;
  }

  public static boolean addOQLClause(String fieldName, String objAliasDotPropertyName, String alias, QueryHelper qhelp, String qbeType, RestState pageState) {
    boolean ret = false;
    if (QueryHelper.TYPE_CHAR.equals(qbeType)) {
      String filter = pageState.getEntry(fieldName).stringValueNullIfEmpty();
      if (filter != null) {
        qhelp.addOQLClause(objAliasDotPropertyName + "=:" + alias, alias, filter);
        pageState.getEntry(fieldName).errorCode = qhelp.getImproperUseOfQBEErrorCode();
        ret = true;
      }
    } else if (QueryHelper.TYPE_LONG.equals(qbeType) ) {
      long filter = pageState.getEntry(fieldName).longValueNoErrorNoCatchedExc();
      if (filter != 0) {
        qhelp.addOQLClause(objAliasDotPropertyName + "=:" + alias, alias, filter);
        pageState.getEntry(fieldName).errorCode = qhelp.getImproperUseOfQBEErrorCode();
        ret = true;
      }
    } else if ( QueryHelper.TYPE_INT.equals(qbeType)) {
      int filter = pageState.getEntry(fieldName).intValueNoErrorCodeNoExc();
      if (filter != 0) {
        qhelp.addOQLClause(objAliasDotPropertyName + "=:" + alias, alias, filter);
        pageState.getEntry(fieldName).errorCode = qhelp.getImproperUseOfQBEErrorCode();
        ret = true;
      }
    } else if (QueryHelper.TYPE_DOUBLE.equals(qbeType) || QueryHelper.TYPE_FLOAT.equals(qbeType)) {
      double filter = pageState.getEntry(fieldName).doubleValueNoErrorNoCatchedExc();
      if (filter != 0) {
        qhelp.addOQLClause(objAliasDotPropertyName + "=:" + alias, alias, filter);
        pageState.getEntry(fieldName).errorCode = qhelp.getImproperUseOfQBEErrorCode();
        ret = true;
      }
    } else if (QueryHelper.TYPE_DATE.equals(qbeType) ) {
      Date filter = pageState.getEntry(fieldName).dateValueNoErrorNoCatchedExc();
      if (filter != null) {
        qhelp.addOQLClause(objAliasDotPropertyName + "=:" + alias, alias, filter);
        pageState.getEntry(fieldName).errorCode = qhelp.getImproperUseOfQBEErrorCode();
        ret = true;
      }
    } else if (QueryHelper.TYPE_CLOB.equals(qbeType)) {
      throw new PlatformRuntimeException("Cannot use QueryHelper.TYPE_CLOB for equality.");
    }


    return ret;
  }


}


