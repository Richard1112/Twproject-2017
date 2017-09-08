package com.twproject.task.financial;

import com.twproject.worklog.WorklogStatus;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.designer.DesignerField;
import org.jblooming.ontology.LoggableIdentifiableSupport;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.RemoveException;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;
import net.sf.json.JSONObject;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.ClientEntry;

import javax.persistence.Transient;
import java.util.Date;

public class Cost extends LoggableIdentifiableSupport {

  private String description;
  private CostAggregator costCenter;
  private CostClassification classification;
  private double realCost;
  private double estimatedCost;

  //6.0.60011
  private WorklogStatus status;
  private PersistentFile attachment;


  private String customField1;
  private String customField2;
  private String customField3;
  private String customField4;


  public Cost() {
  }

  public String getName() {
    return getDescription();
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public CostAggregator getCostCenter() {
    return costCenter;
  }

  public void setCostCenter(CostAggregator costCenter) {
    this.costCenter = costCenter;
  }

  public CostClassification getClassification() {
    return classification;
  }

  public void setClassification(CostClassification classification) {
    this.classification = classification;
  }

  public double getRealCost() {
    return realCost;
  }

  public double getEstimatedCost() {
    return estimatedCost;
  }

  public void setRealCost(double realCost) {
    this.realCost = realCost;
  }

  public void setEstimatedCost(double estimatedCost) {
    this.estimatedCost = estimatedCost;
  }

  public Cost getClonedInstance() {
    Cost clone = new Cost();
    clone.setIdAsNew();
    clone.setDescription(getDescription());
    clone.setEstimatedCost(getEstimatedCost());
    clone.setRealCost(getRealCost());
    clone.setCostCenter(getCostCenter());
    return clone;
  }

  public String getCustomField1() {
    return customField1;
  }

  public void setCustomField1(String customField1) {
    this.customField1 = customField1;
  }

  public String getCustomField2() {
    return customField2;
  }

  public void setCustomField2(String customField2) {
    this.customField2 = customField2;
  }

  public String getCustomField3() {
    return customField3;
  }

  public void setCustomField3(String customField3) {
    this.customField3 = customField3;
  }

  public String getCustomField4() {
    return customField4;
  }

  public void setCustomField4(String customField4) {
    this.customField4 = customField4;
  }

  public WorklogStatus getStatus() {
    return status;
  }

  public void setStatus(WorklogStatus status) {
    this.status = status;
  }

  public PersistentFile getAttachment() {
    return attachment;
  }

  public void setAttachment(PersistentFile attachment) {
    this.attachment = attachment;
  }


  public static Cost load(String id) throws FindByPrimaryKeyException {
    return (Cost) PersistenceHome.findByPrimaryKey(Cost.class,id);
  }

  public JSONObject jsonify() {
    JSONObject jso= super.jsonify();
    jso.element("id",getId());
    if (getStatus()!=null)
      jso.element("status",getStatus().jsonify());
    if (getCreationDate()!=null) {
      jso.element("creationMillis", getCreationDate().getTime());
    }
    jso.element("description",getDescription());
    if (getEstimatedCost()>0)
      jso.element("estimatedCost", getEstimatedCost());
    if (getRealCost()>0)
      jso.element("realCost", getRealCost());
    jso.element("classificationId",getClassification()==null?"": getClassification().getId());
    jso.element("classificationName",getClassification()==null?"": getClassification().getDisplayName());

    jso.element("customField1", getCustomField1());
    jso.element("customField2", getCustomField2());
    jso.element("customField3", getCustomField3());
    jso.element("customField4", getCustomField4());


    if (getAttachment()!=null)
      jso.element("attachment", getAttachment().jsonify());

    return jso;
  }


  public static boolean hasCustomField() {
    return DesignerField.hasCustomField("COST_CUSTOM_FIELD_", 4);
  }

  public void remove(org.jblooming.persistence.hibernate.PersistenceContext pc) throws RemoveException {
    if (getAttachment()!=null)
      getAttachment().delete();
    super.remove(pc);
  }


  @Transient
  public boolean isLockedByDateOrStatus(){
    boolean ret=false;
    if (getStatus()!=null)
      return true;
    // se la custom feature è abilitata non puoi mettere costi nel passato
    long notBeyond=Long.MIN_VALUE;
    if (I18n.isActive("CUSTOM_FEATURE_DO_NOT_ACCEPT_OLD_COSTS")){
      int days=new ClientEntry("dummy",I18n.get("CUSTOM_FEATURE_DO_NOT_ACCEPT_OLD_COSTS")).durationInWorkingDaysNoErrorNoCatchedExc(false);
      notBeyond=days>0?System.currentTimeMillis()-days* CompanyCalendar.MILLIS_IN_DAY:notBeyond;
    }
    ret=getCreationDate()==null?false:getCreationDate().getTime()<notBeyond;


    return ret;
  }


/*
  @Transient
  public static boolean isLockedByDateOrStatus(Cost cost, Date date){

    date=date==null?new Date():date;
    // se la custom feature è abilitata non puoi mettere costi nel passato
    long notBeyond=Long.MIN_VALUE;
    if (I18n.isActive("CUSTOM_FEATURE_DO_NOT_ACCEPT_OLD_COSTS")){
      int days=new ClientEntry("dummy",I18n.get("CUSTOM_FEATURE_DO_NOT_ACCEPT_OLD_COSTS")).durationInWorkingDaysNoErrorNoCatchedExc(false);
      notBeyond=days>0?date.getTime()-days* CompanyCalendar.MILLIS_IN_DAY:notBeyond;
    }


    boolean ret=false;
    if (cost==null || cost.isNew()){

    }
    return ret;
    if (getStatus()!=null)
      return true;
    // se la custom feature è abilitata non puoi mettere costi nel passato
    long notBeyond=Long.MIN_VALUE;
    if (I18n.isActive("CUSTOM_FEATURE_DO_NOT_ACCEPT_OLD_COSTS")){
      int days=new ClientEntry("dummy",I18n.get("CUSTOM_FEATURE_DO_NOT_ACCEPT_OLD_COSTS")).durationInWorkingDaysNoErrorNoCatchedExc(false);
      notBeyond=days>0?System.currentTimeMillis()-days* CompanyCalendar.MILLIS_IN_DAY:notBeyond;
    }
    ret=getCreationDate()==null?false:getCreationDate().getTime()<notBeyond;


    return ret;
  }
*/

}
