package org.jblooming.designer;

import com.opnlb.fulltext.Indexable;
import org.apache.lucene.analysis.core.StopAnalyzer;
import org.hibernate.search.annotations.*;
import org.hibernate.search.bridge.builtin.IntegerBridge;
import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.ontology.Identifiable;
import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.ontology.LoggableIdentifiableSupport;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.FindException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.tracer.Tracer;
import org.jblooming.uidgen.Counter;
import org.jblooming.uidgen.CounterHome;
import org.jblooming.utilities.*;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.input.SQLCombo;
import org.jblooming.waf.html.input.SmartCombo;
import org.jblooming.waf.html.input.Uploader;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.RestState;

import javax.persistence.Transient;
import java.io.Serializable;
import java.text.ParseException;
import java.util.*;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
@Indexed(index = "fulltext")
public class DesignerData extends LoggableIdentifiableSupport implements Indexable {

  private Serializable referenceId;
  private String referenceClassName;


  @DocumentId
  @FieldBridge(impl = IntegerBridge.class)
  public Serializable getId() {
    return super.getId();
  }

  /**
   * the form to which this data refers to, as several forms (here called designers) can have the same reference
   */
  private String designerName;


  private Map<String, String> valueMap = new HashTable();


  public Serializable getReferenceId() {
    return referenceId;
  }

  public void setReferenceId(Serializable referenceId) {
    this.referenceId = referenceId;
  }

  public String getReferenceClassName() {
    return referenceClassName;
  }

  public void setReferenceClassName(String referenceClassName) {
    this.referenceClassName = referenceClassName;
  }

  public Map<String, String> getValueMap() {
    return valueMap;
  }

  public void setValueMap(Map<String, String> valueMap) {
    this.valueMap = valueMap;
  }

  public String getDesignerName() {
    return designerName;
  }

  public void setDesignerName(String designerName) {
    this.designerName = designerName;
  }

  public static List<DesignerData> getAllInstances(Serializable referenceId, String referenceClassName) {
    String hql = "select dd from " + DesignerData.class.getName() + " as dd where dd.referenceId=:refid and dd.referenceClassName=:rcn";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setString("refid", referenceId + "");
    oql.getQuery().setString("rcn", referenceClassName);
    List<DesignerData>  designerDatas=oql.getQuery().list();
    return designerDatas;
  }

  public static DesignerData getInstance(String designerName, Serializable referenceId, String referenceClassName) {

    DesignerData designerData = null;
    String hql = "select dd from " + DesignerData.class.getName() + " as dd where dd.referenceId=:refid and dd.referenceClassName=:rcn and dd.designerName=:dn";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setString("refid", referenceId + "");
    oql.getQuery().setString("rcn", referenceClassName);
    oql.getQuery().setString("dn", designerName);
    designerData = (DesignerData) oql.uniqueResultNullIfEmpty();

    if (designerData == null) {
      designerData = new DesignerData();
      designerData.setIdAsNew();
      designerData.setReferenceClassName(referenceClassName);
      designerData.setReferenceId(referenceId + "");
      designerData.setDesignerName(designerName);
    }
    return designerData;
  }


  public void removeDetail(Detail detail) {
    for (String keyToRemove : new HashSet<String>(getValueMap().keySet())) {
      if (keyToRemove.startsWith(detail.name + "_")) {
        getValueMap().remove(keyToRemove);
      }
    }
  }

  @Transient
    public String getAbstractForIndexing() {
    String afi="";
    for (String v: getValueMap().values()){
       if (JSP.ex(v))
         afi=afi+JSP.w(v)+"\n";
    }
    return afi;
  }

  @Transient
  @Fields({
          @Field(name = "fullcontent", analyzer = @Analyzer(impl = StopAnalyzer.class)),
          @Field(name = "content")
  })
  private String getContentForIndexing() {
    return getAbstractForIndexing();
  }




  /**
   * @param pageState
   * @param df
   * @param ceName
   * @param propertyNameInDesignerData
   * @return true if the field is filled
   * @throws org.jblooming.persistence.exceptions.PersistenceException
   */
  @Transient
  public boolean putValue(RestState pageState, DesignerField df,  String ceName, String propertyNameInDesignerData) throws PersistenceException {

    boolean isFilled = false;

    // if is a loggable
    if (df instanceof DesignerLoggable && !this.isNew() && (DesignerLoggable.LogType.CREATION_DATE.equals(((DesignerLoggable) df).type) ||
            (DesignerLoggable.LogType.CREATOR.equals(((DesignerLoggable) df).type)))) {
      return false;

      // if designer field is readonly
    } else if(df.readOnly) {
      if(this.getValueMap().containsKey(propertyNameInDesignerData))
        return true;
    }

    ClientEntry ce = pageState.getEntry(ceName);
    ce.required = df.required;


    try {
      if (String.class.getName().equals(df.kind)  //Tipo stringa!!!
          || SQLCombo.class.getName().equals(df.kind)  // SQL Combo
          || SmartCombo.class.getName().equals(df.kind)  // SmartCombo
              ) {
        try {
          if (df instanceof DesignerLoggable && DesignerLoggable.LogType.LAST_MODIFIER.equals(((DesignerLoggable) df).type)) {
            this.getValueMap().put(propertyNameInDesignerData, pageState.getLoggedOperator().getFullname());
            pageState.addClientEntry(ceName,pageState.getLoggedOperator().getFullname());
          } else {
            String value = ce.stringValue();
            if (value != null && !"".equals(value)) {
              this.getValueMap().put(propertyNameInDesignerData, value);
              isFilled = true;
            } else
              this.getValueMap().remove(propertyNameInDesignerData);
          }
        } catch (ActionException e) {
        }

      } else if (ReflectionUtilities.extendsOrImplements(Class.forName(df.kind), Date.class)) { //Tipo data!!!
        try {
          if (df instanceof DesignerLoggable && DesignerLoggable.LogType.LAST_MODIFIED.equals(((DesignerLoggable) df).type)) {
            this.getValueMap().put(propertyNameInDesignerData, DateUtilities.dateToString(new Date(), "yyyy-MM-dd-HH-mm-ss"));
            pageState.addClientEntry(ceName,new Date());
          } else {
            Date value = ce.dateValue();
            String persistentValue = DateUtilities.dateToString(value, "yyyy-MM-dd-HH-mm-ss");
            if (value != null) {
              this.getValueMap().put(propertyNameInDesignerData, persistentValue);
              isFilled = true;
            } else
              this.getValueMap().remove(propertyNameInDesignerData);
          }
        } catch (ActionException e) {
        } catch (ParseException e) {
        }

      } else if (Integer.class.getName().equals(df.kind)) { //Tipo intero!!!
        try {
          String value = ce.stringValue();
          if (value != null) {
            int value2 = ce.intValue();
            this.getValueMap().put(propertyNameInDesignerData, value2 + "");
            isFilled = true;
          } else
            this.getValueMap().remove(propertyNameInDesignerData);
        } catch (ActionException e) {
        } catch (ParseException e) {
        }

      } else if (Long.class.getName().equals(df.kind)) {  //Tipo long
        try {
          String value = ce.stringValue();
          if (value != null) {
            long value2 = ce.longValue();
            this.getValueMap().put(propertyNameInDesignerData, value2 + "");
            isFilled = true;
          } else
            this.getValueMap().remove(propertyNameInDesignerData);
        } catch (ActionException e) {
        } catch (ParseException e) {}

      } else if (Double.class.getName().equals(df.kind)) {  //Tipo double
        try {
          String value = ce.stringValue();
          if (value != null && !"".equals(value)) {
            double value2 = ce.doubleValue(df.getDecimalPlaces());
            this.getValueMap().put(propertyNameInDesignerData, value2 + "");
            isFilled = true;
          } else
            this.getValueMap().remove(propertyNameInDesignerData);
        } catch (ActionException e) {
        } catch (ParseException e) {}

      } else if (Currency.class.getName().equals(df.kind)) {  //Tipo valuta /Type currency
        try {
          String value = ce.stringValue();
          if (JSP.ex(value)) {
            double value2 = ce.currencyValue();
            this.getValueMap().put(propertyNameInDesignerData, value2 + "");
            isFilled = true;
          } else
            this.getValueMap().remove(propertyNameInDesignerData);
        } catch (ActionException e) {
        } catch (ParseException e) {}

      } else if (Counter.class.getName().equals(df.kind)) {

        String value = ce.stringValueNullIfEmpty();
        if (!JSP.ex(value)) {
          int valueI = CounterHome.next(propertyNameInDesignerData);
          this.getValueMap().put(propertyNameInDesignerData, valueI + "");
          isFilled = true;
          ce.setValue(valueI + "");
        }

      } else if ((Boolean.class.getName().equals(df.kind))) { //Tipo booleano
        try {
          String value = ce.stringValue();
          if (value != null && !"".equals(value)) {
            this.getValueMap().put(propertyNameInDesignerData, value);
            isFilled = true;
          } else
            this.getValueMap().remove(propertyNameInDesignerData);
        } catch (ActionException e) {}

      } else if (ReflectionUtilities.extendsOrImplements(Class.forName(df.kind), PersistentFile.class)) { //Tipo PersistentFile
        try {

          PersistentFile persistentFile = PersistentFile.deserialize(this.getValueMap().get(df.name));
          if (persistentFile == null)
            persistentFile = new PersistentFile(0, null,PersistentFile.DEFAULT_STORAGE_TYPE);

          PersistentFile pf = Uploader.save(this, persistentFile, ceName, pageState);

          if (pf != null)
            this.getValueMap().put(propertyNameInDesignerData, pf.serialize());
          else
            this.getValueMap().remove(propertyNameInDesignerData);

        } catch (ApplicationException e) {
          Tracer.platformLogger.error(e);
        } catch (ActionException e) {}

      } else if (ReflectionUtilities.extendsOrImplements(Class.forName(df.kind), Identifiable.class)) { //Tipo Identifiable

        try {
          String value = ce.stringValue();
          if (value != null && !"".equals(value)) {
            this.getValueMap().put(propertyNameInDesignerData, value);
            isFilled = true;
          } else
            this.getValueMap().remove(propertyNameInDesignerData);
        } catch (ActionException e) {}

      } else if (ReflectionUtilities.extendsOrImplements(Class.forName(df.kind), CodeValue.class)) { //Tipo CodeValue
        try {
          String value = ce.stringValue();
          if (value != null && !"".equals(value)) {
            this.getValueMap().put(propertyNameInDesignerData, value);
            isFilled = true;
          } else
            this.getValueMap().remove(propertyNameInDesignerData);
        } catch (ActionException e) {}
      }

    } catch (ClassNotFoundException e) {
      throw new PlatformRuntimeException(e);
    }
    return isFilled;
  }

  public static DesignerData load(String mainObjectId) throws FindByPrimaryKeyException {
    return (DesignerData) PersistenceHome.findByPrimaryKey(DesignerData.class, mainObjectId);
  }


  public static boolean hasFormFilled (Identifiable obj, String designerName)  {
    String hql = "select count(dd.id) from " + DesignerData.class.getName() + " as dd where dd.referenceId=:refid and dd.referenceClassName=:rcn";

    if (JSP.ex(designerName))
      hql+=" and dd.designerName=:dn";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setString("refid", obj.getId() + "");
    oql.getQuery().setString("rcn", obj.getClass().getName());
    if (JSP.ex(designerName))
      oql.getQuery().setString("dn", designerName);

    Long ret=(Long)oql.uniqueResultNullIfEmpty();

    return ret>0;
  }


}
