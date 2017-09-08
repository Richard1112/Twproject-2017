package org.jblooming.persistence.objectEditor;

import org.hibernate.mapping.PersistentClass;
import org.hibernate.mapping.SimpleValue;
import org.hibernate.type.DoubleType;
import org.hibernate.type.IntegerType;
import org.hibernate.type.LongType;
import org.hibernate.type.StringType;
import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.ontology.*;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.hibernate.HibernateFactory;
import org.jblooming.persistence.hibernate.HibernateUtilities;
import org.jblooming.utilities.CodeValueList;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.waf.constants.FieldErrorConstants;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.input.Combo;
import org.jblooming.waf.html.input.SmartCombo;
import org.jblooming.waf.html.input.Uploader;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.RestState;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.util.Date;
import java.util.List;


/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 12-apr-2005 : 13.37.15
 */
public class FieldFeature {
  /**
   * exact name of the property of the object considered; will be used by reflection
   */
  public String propertyName;

  /**
   * name of client entry (html input). As default is the same of property
   */
  public String fieldName;
  public String label;
  public boolean required = false;
  public boolean readOnly = false;
  public String initialValue;

  public SmartCombo smartCombo;
  //used to help object action determine real class of identificable which is property
  public Class smartComboClass;

  public boolean noSortable = false;
  public String mask = null;

  public Combo comboElement; //aggiunto da amelie
  
  public String separator = null;

  public int fieldSize = -1;

   public String format;
  /**
   * method used for callback when displaying field content
   */
  public String toStringCallbackMethod;

  public PageSeed pageSeed;

  public boolean boolAsCombo = false;

  public boolean transformToUpperCase = false;


  /**
   * if false the filed is not used for searching
   */
  public boolean usedForSearch = true;
  public boolean usedComboForSearch = false;
  public String blank = null;
  public boolean useEmptyForAll = false;

  public FieldFeature(String propertyName, String label) {
    this.fieldName = propertyName;
    this.propertyName = propertyName;
    this.label = label;
  }

  public static FieldFeature getLookupInstance(String lookupField, String label, Class lookupClass) {
    return getLookupInstance(lookupField, label, lookupClass, false);
  }

  public static FieldFeature getLookupInstance(String lookupField, String label, Class lookupClass, boolean toUpperCase) {
    return getLookupInstance(lookupField, label, null, lookupClass, toUpperCase);
  }

  public static FieldFeature getLookupInstance(String lookupField, String label, String whereForFiltering, Class lookupClass, boolean toUpperCase) {

    FieldFeature ff = new FieldFeature(lookupField, label);
    try {
      Object o = lookupClass.newInstance();
      if (o instanceof Lookup) {
        String hql = "select p.id, p.description from " + lookupClass.getName() + " as p";
        if (whereForFiltering==null) {
          whereForFiltering = "where p.description like :" + SmartCombo.FILTER_PARAM_NAME + " order by p.description";
          if (toUpperCase) {
            whereForFiltering = "where upper(p.description) like :" + SmartCombo.FILTER_PARAM_NAME + " order by p.description";
          }
        } 

        String whereForId = "where p.id = :" + SmartCombo.FILTER_PARAM_NAME;
        SmartCombo lookup = new SmartCombo(lookupField, hql, whereForFiltering, whereForId);
        if (toUpperCase)
          lookup.convertToUpper = true;
        ff.smartCombo = lookup;

      } else
        throw new PlatformRuntimeException("FieldFeature accepts LookupSupport extensions only");

    } catch (InstantiationException e) {
      throw new PlatformRuntimeException(e);
    } catch (IllegalAccessException e) {
      throw new PlatformRuntimeException(e);
    }
    return ff;
  }

  public static FieldFeature getIdentifiableInstance(String lookupField, String alias, Class identifiableClass, String[] comboDisplayProperties) {
    return getIdentifiableInstance(lookupField, alias, identifiableClass, comboDisplayProperties, false);
  }

  public static FieldFeature getIdentifiableInstance(String lookupField, String alias, Class identifiableClass, String[] comboDisplayProperties, boolean toUpper) {

    FieldFeature ff = new FieldFeature(lookupField, alias);
    ff.smartComboClass = identifiableClass;
    try {
      Object o = identifiableClass.newInstance();

      if (o instanceof Identifiable) {
        String hql = "select p.id";
        for (String cdp : comboDisplayProperties) {
          hql += ", p." + cdp;
        }
        hql += " from " + identifiableClass.getName() + " as p";
        String whereForFiltering = "where p." + comboDisplayProperties[0] + " like :" + SmartCombo.FILTER_PARAM_NAME + " order by p." + comboDisplayProperties[0];
        if (toUpper)
          whereForFiltering = "where upper(p." + comboDisplayProperties[0] + ") like :" + SmartCombo.FILTER_PARAM_NAME + " order by p." + comboDisplayProperties[0];

        String whereForId = "where p.id = :" + SmartCombo.FILTER_PARAM_NAME;
        SmartCombo lookup = new SmartCombo(lookupField, hql, whereForFiltering, whereForId);
        if(toUpper)
          lookup.convertToUpper = true;
        ff.smartCombo = lookup;

      } else
        throw new PlatformRuntimeException("FieldFeature accepts Identifiable extensions only");

    } catch (InstantiationException e) {
      throw new PlatformRuntimeException(e);
    } catch (IllegalAccessException e) {
      throw new PlatformRuntimeException(e);
    }
    return ff;
  }

  public static FieldFeature getComboInstance(String lookupField, String alias, Enum enums) {
    FieldFeature ff = new FieldFeature(lookupField, alias);
    Object [] obj = enums.getDeclaringClass().getEnumConstants();
    CodeValueList cvl = new CodeValueList();
    cvl.add("", "&nbsp;");
    for (int i = 0; i < obj.length; i++) {
      Object og = obj[i];
      cvl.add(og.toString(), og.toString());
    }
    Combo enumC = new Combo(ff.fieldName, null, "", 10, null, cvl, "");
    ff.comboElement = enumC;
    return ff;
  }

  public static FieldFeature getComboInstance(String lookupField, String alias, CodeValueList cvl) {
    FieldFeature ff = new FieldFeature(lookupField, alias);
    Combo cvlC = new Combo(ff.fieldName, null, "", 10, null, cvl, "");
    ff.comboElement = cvlC;
    return ff;
  }

  public static FieldFeature getLinkInstance(String propertyName, String alias, PageSeed pageSeed) {
    FieldFeature ff = new FieldFeature(propertyName, alias);
    ff.pageSeed = pageSeed;
    return ff;
  }

}
