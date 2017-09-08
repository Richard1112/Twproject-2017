package com.twproject.task;

import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.ResourceBricks;
import net.sf.json.JSONArray;
import net.sf.json.JSONObject;
import org.jblooming.ontology.LoggableIdentifiableSupport;
import org.jblooming.ontology.SecuredLoggableSupport;
import org.jblooming.operator.Operator;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.PersistenceHome;
import org.hibernate.annotations.Type;
import org.hibernate.annotations.ForeignKey;
import org.hibernate.annotations.Index;
import com.twproject.resource.Resource;
import org.jblooming.security.Securable;
import org.jblooming.utilities.HttpUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;

import javax.persistence.*;
import java.io.Serializable;

@Entity
@Table(name = "twk_issue_history")
public class IssueHistory extends SecuredLoggableSupport {

  private Operator owner;

  private Issue issue;
  private String comment;
  private IssueStatus status;
  private IssueStatus oldStatus;
  private Resource assignee;
  private Task task;

  private String extRequesterEmail;

  // da usare per creare le note (NON mette i dati dello stato))
  public IssueHistory() {
  }

  // da usare solo per creare la storia
  public IssueHistory(Issue issue) {
    setIssue(issue);
    setTask(issue.getTask());
    setOldStatus(issue.getStatus());
    setStatus(issue.getStatus());
    setAssignee(issue.getAssignedTo());
  }

  @Id
  @Type(type = "int")
  @GeneratedValue(strategy = GenerationType.AUTO)
  public Serializable getId() {
    return super.getId();
  }


  /**
   * ATTENZIONE questo può essere null se il commento è stato inserito da un utente anonimo tramite la pagina dei ticket
   * @return
   */
  @ManyToOne(targetEntity = TeamworkOperator.class, fetch = FetchType.LAZY)
  @ForeignKey(name = "fk_issuehist_owner")
  @Index(name = "idx_issuehist_owner")
  @JoinColumn(name = "ownerx")
  public Operator getOwner() {
    return owner;
  }

  public void setOwner(Operator owner) {
    this.owner = owner;
  }



  @Lob
  @Column(name = "commentx")
  @Type(type = "org.hibernate.type.TextType")
  public String getComment() {
    return comment;
  }

  public void setComment(String comment) {
    this.comment = comment;
  }

  @ManyToOne(targetEntity = IssueStatus.class)
  @ForeignKey(name = "fk_issuehist_isstat")
  @Index(name = "idx_issuehist_isstat")
  @JoinColumn(name="statusx")
  public IssueStatus getStatus() {
    return status;
  }

  public void setStatus(IssueStatus status) {
    this.status = status;
  }

  @ManyToOne(targetEntity = IssueStatus.class)
  @ForeignKey(name = "fk_issuehist_isoldstat")
  @Index(name = "idx_issuehist_isoldstat")
  @JoinColumn(name="oldstatus")
  public IssueStatus getOldStatus() {
    return oldStatus;
  }

  public void setOldStatus(IssueStatus oldStatus) {
    this.oldStatus = oldStatus;
  }
  

  @ManyToOne(targetEntity = Resource.class)
  @ForeignKey(name = "fk_issuehist_res")
  @Index(name = "idx_issuehist_res")
  public Resource getAssignee() {
    return assignee;
  }

  public void setAssignee(Resource assignee) {
    this.assignee = assignee;
  }

  @ManyToOne(targetEntity = Task.class)
  @ForeignKey(name = "fk_issuehist_task")
  @Index(name = "idx_issuehist_task")
  public Task getTask() {
    return task;
  }

  public void setTask(Task task) {
    this.task = task;
  }

  @ManyToOne(targetEntity = Issue.class)
  @ForeignKey(name = "fk_issuehist_issue")
  @Index(name = "idx_issuehist_issue")
  public Issue getIssue() {
    return issue;
  }

  public void setIssue(Issue issue) {
    this.issue = issue;
  }

  public boolean somethingChanged(Issue issue) {
    boolean changed = false;
    if (getTask()!=null && !getTask().equals(issue.getTask())  || getTask()==null && issue.getTask()!=null){
      setTask(issue.getTask());
      changed=true;
    }else {
      setTask(null);
    }
    
    if (getOldStatus()!=null && !getOldStatus().equals(issue.getStatus())  || getOldStatus()==null && issue.getStatus()!=null){
      setStatus(issue.getStatus());
      changed=true;
    }else{
      setStatus(null);
    }

    if (getAssignee()!=null && !getAssignee().equals(issue.getAssignedTo())  || getAssignee()==null && issue.getAssignedTo()!=null){
      setAssignee(issue.getAssignedTo());
      changed=true;
    }else{
      setAssignee(null);
    }

    if (changed)
      issue.setLastIssueHistory(this);
    else
      issue.setLastIssueHistory(null);

    return changed;
  }

  public boolean testChangedAndStore() throws StoreException {
    // save the hisorty if needed
    boolean ret = somethingChanged(issue);
    if (ret){
      this.store();
    }
    return  ret;
  }

  public static IssueHistory load(Serializable mainObjectId) throws FindByPrimaryKeyException {
    return (IssueHistory) PersistenceHome.findByPrimaryKey(IssueHistory.class, mainObjectId);
  }


  @Column (length = 60)
  @Index(name = "idx_issue_extemail")
  public String getExtRequesterEmail() {
    return extRequesterEmail;
  }

  public void setExtRequesterEmail(String extRequesterEmail) {
    this.extRequesterEmail = extRequesterEmail;
  }


  public JSONObject jsonify() {
    JSONObject jso =super.jsonify();

    jso.element("id", getId());
    jso.element("comment", JSP.encode(getComment()));

    Task task = getTask();
    if (task != null) {
      jso.element("taskId", task.getId());
      //jso.element("taskName", task.getDisplayName());
      jso.element("taskName", task.getName());
      jso.element("taskCode", task.getCode());
      jso.element("tcodid", task.getMnemonicCode());
      jso.element("taskOrder", task.getName());
    }

    Resource assignee = getAssignee();
    if (assignee != null) {
      jso.element("assigneeId", assignee.getId());
      jso.element("assigneeName", assignee.getDisplayName());
      jso.element("rcodid", assignee.getMnemonicCode());
      jso.element("assigneeAvatarUrl", assignee.bricks.getAvatarImageUrl());
    }


    if (getStatus() != null) {
      jso.element("statusId", getStatus().getId());
      jso.element("statusName", getStatus().getDescription());
      jso.element("statusColor", getStatus().getColor());
      jso.element("statusOrder", getStatus().getOrderBy());
      jso.element("isOpen", getStatus().isBehavesAsOpen());
    }

    if (getOldStatus() != null) {
      jso.element("oldStatusId", getOldStatus().getId());
      jso.element("oldStatusName", getOldStatus().getDescription());
      jso.element("oldStatusColor", getOldStatus().getColor());
      jso.element("oldStatusOrder", getOldStatus().getOrderBy());
      jso.element("oldSisOpen", getOldStatus().isBehavesAsOpen());
    }

    jso.element("lastModified",getLastModified().getTime());
    jso.element("lastModifier",getLastModifier());

    jso.element("creationDate",getCreationDate().getTime());
    jso.element("creator",getCreator());

    jso.element("ownerId",getOwner()==null?"-1":getOwner().getId());

    //gestione del gravatar per i commenti da utente esterno
    if (getOwner()!=null)
      jso.element("ownerAvatarUrl",((TeamworkOperator)getOwner()).getPerson().bricks.getAvatarImageUrl());
    else if (JSP.ex(getExtRequesterEmail()))
      jso.element("ownerAvatarUrl", ResourceBricks.getGravatarUrl(getExtRequesterEmail(),50));

    jso.element("extRequesterEmail",getExtRequesterEmail());

    return jso;
  }



}
