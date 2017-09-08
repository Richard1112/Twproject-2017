package com.twproject.task;

import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.security.SecurableWithArea;
import org.jblooming.security.Area;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.oql.OqlQuery;
import org.jblooming.PlatformRuntimeException;
import org.hibernate.annotations.Type;
import org.hibernate.annotations.ForeignKey;
import org.hibernate.annotations.Index;
import org.hibernate.search.annotations.DocumentId;
import org.hibernate.search.annotations.FieldBridge;
import org.hibernate.search.bridge.builtin.StringBridge;
import org.hibernate.*;

import javax.persistence.*;
import java.io.Serializable;
import java.util.List;

@Entity
@Table(name = "twk_issue_status")
public class IssueStatus extends IdentifiableSupport {

  private String description;
  private boolean behavesAsOpen;
  private boolean behavesAsClosed;
  private boolean askForComment;
  private boolean askForWorklog;
  private int orderBy;
  private String color;


  @Id
  @Type(type = "int")
  @GeneratedValue(strategy = GenerationType.AUTO)
  public Serializable getId() {
    return super.getId();
  }


  public boolean isBehavesAsOpen() {
    return behavesAsOpen;
  }

  public void setBehavesAsOpen(boolean behavesAsOpen) {
    this.behavesAsOpen = behavesAsOpen;
  }

  public boolean isBehavesAsClosed() {
    return behavesAsClosed;
  }

  public void setBehavesAsClosed(boolean behavesAsClosed) {
    this.behavesAsClosed = behavesAsClosed;
  }

  public boolean isAskForComment() {
    return askForComment;
  }

  public void setAskForComment(boolean askForComment) {
    this.askForComment = askForComment;
  }

  public boolean isAskForWorklog() {
    return askForWorklog;
  }

  public void setAskForWorklog(boolean askForWorklog) {
    this.askForWorklog = askForWorklog;
  }

  public int getOrderBy() {
    return orderBy;
  }

  public void setOrderBy(int orderBy) {
    this.orderBy = orderBy;
  }

  public static IssueStatus load(Serializable id) throws FindByPrimaryKeyException {
    return (IssueStatus) PersistenceHome.findByPrimaryKey(IssueStatus.class, id);
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  @Transient
  public static IssueStatus getStatusClose() {
    org.hibernate.Query query = new OqlQuery("select iss from " + IssueStatus.class.getName() + " as iss where iss.behavesAsClosed=true order by iss.orderBy").getQuery();
    query.setMaxResults(1);

    IssueStatus status = (IssueStatus) query.uniqueResult();
    if (status==null)
      throw new PlatformRuntimeException("There is no issue status that behaves as 'closed'");
    return status;
  }

  @Transient
  public static IssueStatus getStatusOpen() {
    org.hibernate.Query query = new OqlQuery("select iss from " + IssueStatus.class.getName() + " as iss where iss.behavesAsOpen=true order by iss.orderBy").getQuery();
    query.setMaxResults(1);

    IssueStatus status = (IssueStatus) query.uniqueResult();
    if (status==null)
      throw new PlatformRuntimeException("There is no issue status that behaves as 'open'");
    return status;
  }

  @Transient
  public static List<IssueStatus> getStatusesAsOpen() {
    org.hibernate.Query query = new OqlQuery("select iss from " + IssueStatus.class.getName() + " as iss where iss.behavesAsOpen=true order by iss.orderBy").getQuery();
    List<IssueStatus> statuses =  query.list();
    return statuses ;
  }

  @Transient
  public static List<IssueStatus> getStatusesAsClose() {
    org.hibernate.Query query = new OqlQuery("select iss from " + IssueStatus.class.getName() + " as iss where iss.behavesAsClosed=true order by iss.orderBy").getQuery();
    List<IssueStatus> statuses =  query.list();
    return statuses ;
  }

  public String getColor() {
    return color;
  }

  public void setColor(String color) {
    this.color = color;
  }
  
}
