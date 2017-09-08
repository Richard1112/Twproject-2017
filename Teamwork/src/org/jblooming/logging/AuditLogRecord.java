package org.jblooming.logging;

import org.hibernate.annotations.Index;
import org.hibernate.annotations.Type;
import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.operator.Operator;

import javax.persistence.*;
import java.io.Serializable;
import java.util.Date;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
@Entity
@Table(name = "_auditlog")
public class AuditLogRecord extends IdentifiableSupport {

  private String message;
  private String entityId;
  private String entityClass;
  private String fullName;
  private Date created;
  private String data;

  AuditLogRecord() {
  }

  public AuditLogRecord(String message,
                        Serializable entityId,
                        String entityClass,
                        Operator operator) {
    this.message = message;
    this.entityId = entityId!=null ? entityId.toString() : "null";
    this.entityClass = entityClass;
    this.fullName = operator.getFullname();
    this.created = new Date();
  }

  @Id
  @Type(type = "int")
  @GeneratedValue(strategy = GenerationType.AUTO)
  public Serializable getId() {
      return super.getId();
  }

  
  public String getMessage() {
    return message;
  }

  public void setMessage(String message) {
    this.message = message;
  }

  @Index(name = "idx_auditlog_entid")
  public String getEntityId() {
    return entityId;
  }

  public void setEntityId(String entityId) {
    this.entityId = entityId;
  }

  @Index(name = "idx_auditlog_entclass")
  public String getEntityClass() {
    return entityClass;
  }

  public void setEntityClass(String entityClass) {
    this.entityClass = entityClass;
  }

  public String getFullName() {
    return fullName;
  }

  public void setFullName(String fullName) {
    this.fullName = fullName;
  }

  @Index(name = "idx_auditlog_created")
  public Date getCreated() {
    return created;
  }

  public void setCreated(Date created) {
    this.created = created;
  }

  @Lob
  @Type(type = "org.hibernate.type.TextType")
  public String getData() {
    return data;
  }

  public void setData(String data) {
    this.data = data;
  }
}