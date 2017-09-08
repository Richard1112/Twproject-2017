package org.jblooming.logging;

import net.sf.json.JSONObject;
import org.hibernate.annotations.Index;
import org.hibernate.annotations.Type;
import org.jblooming.ontology.IdentifiableSupport;

import javax.persistence.*;
import java.io.Serializable;
import java.util.Date;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
@Entity
@Table(name = "_deletelog")
public class DeleteLog extends IdentifiableSupport {

  private String entityId;
  private String entityClass;
  private String entityName;
  private Date deletedOn;
  private String deletedBy;
  private JSONObject jsonData =new JSONObject();


  public DeleteLog() { }


  @Id
  @Type(type = "int")
  @GeneratedValue(strategy = GenerationType.AUTO)
  public Serializable getId() {
      return super.getId();
  }


  @Index(name = "idx_dellog_entid")
  public String getEntityId() {
    return entityId;
  }

  public void setEntityId(String entityId) {
    this.entityId = entityId;
  }

  @Index(name = "idx_dellog_entclass")
  public String getEntityClass() {
    return entityClass;
  }

  public void setEntityClass(String entityClass) {
    this.entityClass = entityClass;
  }

  public String getEntityName() {
    return entityName;
  }

  public void setEntityName(String entityName) {
    this.entityName = entityName;
  }

  @Index(name = "idx_dellog_deleted")
  public Date getDeletedOn() {
    return deletedOn;
  }

  public void setDeletedOn(Date deletedOn) {
    this.deletedOn = deletedOn;
  }

  public String getDeletedBy() {
    return deletedBy;
  }

  public void setDeletedBy(String deletedBy) {
    this.deletedBy = deletedBy;
  }


  @Type(type = "org.jblooming.ontology.JSONObjectType")
  public JSONObject getJsonData(){
    return this.jsonData;
  }

  public void setJsonData(JSONObject jsonData){
    this.jsonData = jsonData;
  }
}