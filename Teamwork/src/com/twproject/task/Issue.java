package com.twproject.task;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.hibernate.annotations.Type;
import org.hibernate.search.annotations.DocumentId;
import org.hibernate.search.annotations.FieldBridge;
import org.hibernate.search.annotations.Indexed;
import org.hibernate.search.bridge.builtin.StringBridge;
import org.jblooming.messaging.MailHelper;
import org.jblooming.messaging.Message;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.messaging.SomethingHappened;
import org.jblooming.ontology.HasDenormalizedFields;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.operator.Operator;
import org.jblooming.operator.User;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.FindException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.RemoveException;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.security.Permission;
import org.jblooming.security.PermissionCache;
import org.jblooming.security.Securable;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageSeed;

import com.opnlb.fulltext.Indexable;
import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Person;
import com.twproject.resource.Resource;
import com.twproject.security.RoleTeamwork;
import com.twproject.worklog.Worklog;

import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

/**
 * @author Pietro Polsinelli ppolsinelli@twproject.com
 */

@Entity
@Table(name = "twk_issue")
@Indexed(index = "fulltext")
public class Issue extends IssuePersistent implements Securable, Indexable,HasDenormalizedFields, PermissionCache.PermissionCacheEnabled {

  public IssueBricks bricks = new IssueBricks(this);


  public static final String GRAVITY_LOW = "01_GRAVITY_LOW";
  public static final String GRAVITY_MEDIUM = "02_GRAVITY_MEDIUM";
  public static final String GRAVITY_HIGH = "03_GRAVITY_HIGH";
  public static final String GRAVITY_CRITICAL = "04_GRAVITY_CRITICAL";
  public static final String GRAVITY_BLOCK = "05_GRAVITY_BLOCK";

  /**
   * this is a transient field filled by the saving action when a history record is created
   */
  private IssueHistory lastIssueHistory = null;


  public enum Event {
    ISSUE_CLOSE,ISSUE_ASSIGNED
  }


  public Issue() {
  }

  @Override
@Id
  @Type(type = "string")
  // @Column(length = 15)  // this should be used but all tw4.0 existing installations have 255
  @DocumentId
  @FieldBridge(impl = StringBridge.class)
  public Serializable getId() {
    return super.getId();
  }

  @Override
@Transient
  public String getName() {
    return JSP.encode(getDescription());
  }

  @Transient
  public static String[] getGravities() {
    String grvs[] = {Issue.GRAVITY_BLOCK, Issue.GRAVITY_CRITICAL, Issue.GRAVITY_HIGH, Issue.GRAVITY_MEDIUM, Issue.GRAVITY_LOW};
    return grvs;
  }

  @Transient
  public void updateWorklogDone() {

    long wl = 0;
    if (!isNew()) {
      String hql = "select sum(wklg.duration) from " + Worklog.class.getName() + " as wklg where wklg.issue = :issue";
      OqlQuery oql = new OqlQuery(hql);
      oql.getQuery().setEntity("issue", this);
      try {
        wl = (Long) oql.uniqueResult();
      } catch (FindException e) {
      }
    }
    setWorklogDone(wl);
  }

  @Override
@Transient
  public String getDisplayName() {
    return JSP.encode(getDescription());
  }

  public void addWorklogInMemory(Worklog wl) {
    worklogs.add(wl);
  }

  public void removeWorklogInMemory(Worklog wkl) {
    worklogs.remove(wkl);
  }

  @Transient
  public Assignment getAssignmentOnTask() {
    Assignment a = null;
    if (getAssignedTo() != null && getTask() != null) {
      a=getTask().getFirstAssignmentsForResource(getAssignedTo());
    }
    return a;
  }

  @Transient
  public Assignment getAssignmentOnTask(Resource resource) {
    Assignment a = null;
    if (resource != null && getTask() != null) {
      a=getTask().getFirstAssignmentsForResource(resource);
    }
    return a;
  }

  @Override
public boolean hasPermissionFor(User u, Permission p) {
    return PermissionCache.hasPermissionFor(u,this,p);
  }

  @Override
public boolean hasPermissionForUnCached(User u, Permission p) {
    boolean ret = false;

    //-------------------------------------------------------- TO DO-----------------------------------
    Person loggedPerson = ((TeamworkOperator) u).getPerson();
    if (getTask() == null) { //is a t_odo  special treatement ONLY the assignee has permissions (no admins, no area managers)
      if (getAssignedTo()==null){
        ret=super.hasPermissionFor(u, p);
      } else {
        ret = loggedPerson.equals(getAssignedTo());
      }
    //------------------------------------------------------------ ISSUE ---------------------------------------
    } else { // is an issue: almost standard security (assignee can read/change status)

      // si controlla, l'admin, l'owner, o i permessi globali
      ret=super.hasPermissionFor(u, p);

      //se è assegnata a me
      if (!ret && RoleTeamwork.getMyselfRole().hasPermissionFor(p))
        ret= loggedPerson.equals(getAssignedTo());


      //se è assegnata a una risorsa che gestisco
      if (!ret && RoleTeamwork.getManagerRole().hasPermissionFor(p))
        ret= getAssignedTo().hasPermissionFor(u,p);


      // altrimenti si controlla il task
      if (!ret)
        ret =  getTask().hasPermissionFor(u, p);



    }
    return ret;
  }


  public void setStatusClosed() {
    setIssueStatus(IssueStatus.getStatusClose());
  }


  public void setStatusOpen() {
    setIssueStatus(IssueStatus.getStatusOpen());
  }

  public static Issue load(String mainObjectId) throws FindByPrimaryKeyException {
    return (Issue) PersistenceHome.findByPrimaryKey(Issue.class, mainObjectId);
  }


  public void setIssueStatus(IssueStatus status) {
    IssueStatus oldSts = getStatus();
    if (oldSts !=null && !oldSts.equals(status) || status!=null && !status.equals(oldSts)){
      super.setLastStatusChangeDate(new Date());
      super.setStatus(status);
    }
  }


  @Transient
  public static List<IssueStatus> getStatusList() throws FindException {
    return new OqlQuery("select iss from " + IssueStatus.class.getName() + " as iss order by iss.orderBy").list();
  }

  @Transient
  public IssueHistory getLastIssueHistory() {
    return lastIssueHistory;
  }

  public void setLastIssueHistory(IssueHistory lastIssueHistory) {
    this.lastIssueHistory = lastIssueHistory;
  }

  @Transient
  public static int getGravityScore(String aGravity){
    if (!JSP.ex(aGravity) || aGravity.length()<2)
      return 0;
    return Integer.parseInt(aGravity.charAt(1)+"");  // aaargh! what f..... beautiful hack!  01_GRAVITY_LOW , 02_GRAVITY_MEDIUM
  }


  @Override
public JSONObject jsonify() {
    JSONObject jso =super.jsonify();
    jso.element("id", getId());
    jso.element("description", JSP.encode(getDescription()));
    jso.element("code", getCode());
    jso.element("estimatedDuration", getEstimatedDuration());
    jso.element("typeId", getType() == null ? "" : getType().getId());
    jso.element("type", getType() == null ? "" : getType().getDescription());
    jso.element("tags", JSP.encode(getTags()));
    jso.element("icodid", getMnemonicCode());

    jso.element("orderByTask",getOrderFactor());
    jso.element("orderByRes",getOrderFactorByResource());

    Task task = getTask();
    if (task == null) {
      jso.element("taskId", "-1");
      jso.element("taskName", I18n.get("NO_TASK"));
      jso.element("taskCode", "");

      jso.element("taskOrder", "");
    } else {
      jso.element("taskId", task.getId());
      jso.element("taskName", getTask().getName());
      jso.element("taskCode", getTask().getCode());
      jso.element("tcodid", task.getMnemonicCode());
      jso.element("taskOrder", task.getName());
      if (I18n.isActive("CUSTOM_FEATURE_ALWAYS_SHOW_TASK_PATH")){
        jso.element("taskPath", task.getPath(" / ",false));
      }

    }

    Resource resource = getAssignedTo();
    if (resource == null) {
      jso.element("assigneeId", "-1");
      jso.element("assigneeName", I18n.get("UNASSIGNED"));
      jso.element("assigneeAvatarUrl", ApplicationState.contextPath + "/img/noPhoto_TW.jpg");
    } else {
      jso.element("assigneeId", resource.getId());
      jso.element("assigneeName", resource.getDisplayName());
      jso.element("rcodid", resource.getMnemonicCode());
      jso.element("assigneeAvatarUrl", resource.bricks.getAvatarImageUrl());
    }

    resource = getAssignedBy();
    if (resource != null) {
      jso.element("assignedById", resource.getId());
      jso.element("assignedByName", resource.getDisplayName());
      jso.element("assigendByCodid", resource.getMnemonicCode());
      jso.element("assignedByAvatarUrl", resource.bricks.getAvatarImageUrl());
    }

    jso.element("extRequesterEmail", getExtRequesterEmail());

    Assignment ass = getAssignmentOnTask();
    if (ass != null)
      jso.element("assignmentId", ass.getId());


    if (getShouldCloseBy() != null) {
      jso.element("shouldCloseBy", getShouldCloseBy().getTime());
    }

    if (getStatus() == null) {
      jso.element("statusId", "-1");
      jso.element("statusName", I18n.get("NO_STATUS"));
      jso.element("statusColor", "#ffffff");
      jso.element("statusOrder", "");
    } else {
      jso.element("statusId", getStatus().getId());
      jso.element("statusName", getStatus().getDescription());
      jso.element("statusColor", getStatus().getColor());
      jso.element("statusOrder", getStatus().getOrderBy());
      jso.element("isOpen", getStatus().isBehavesAsOpen());
    }

    if (getGravity() == null) {
      jso.element("gravityId", "-1");
      jso.element("gravityName", I18n.get("NO_GRAVITY"));
      jso.element("gravityColor", "#ffffff");
      jso.element("gravityOrder", 99);
    } else {
      jso.element("gravityId", getGravity());
      jso.element("gravityName", I18n.get(getGravity()));
      jso.element("gravityColor", IssueBricks.getGravityColor(getGravity()));
      jso.element("gravityOrder", IssueBricks.getGravityOrder(getGravity()));
    }


    jso.element("lastModified", getLastModified().getTime());
    jso.element("lastModifier", getLastModifier());


    // ----------------- documents
    JSONArray files= new JSONArray();
    for (PersistentFile pf:getFiles()){
      JSONObject jsonObject = pf.jsonify();
      jsonObject.element("issueId", getId());
      files.add(jsonObject);
    }
    jso.element("documents", files);

    return jso;
  }



  @Transient
  public List<IssueHistory> getComments(){
    ArrayList<IssueHistory> iHs = new ArrayList<>();
    for (IssueHistory ih:getIssueHistories())
      if (JSP.ex(ih.getComment()))
        iHs.add(ih);
    return iHs;
  }


  @Transient
  public IssueHistory addComment(String comment) {
    IssueHistory iH = new IssueHistory();
    iH.setIssue(this);
    iH.setComment(comment);
    return iH;
  }


  public void generateCommentMessage(Operator from,Operator to, IssueHistory issueHistory,MessagingSystem.Media media) throws PersistenceException {
    if (to != null) {
      String language = to.getLanguage();
      Message message = new Message();
      message.setFromOperator(from);
      message.setToOperator(to);
      message.setDefaultExpires();
      message.setMedia(media.toString());
      message.setSubject(I18n.getLabel("NEW_COMMENT_ON_ISSUE", language) + ": " + "I#" + getMnemonicCode() + "#");

      PageSeed ps = new PageSeed(ApplicationState.serverURL + "/i/" + getId());
      ButtonLink edit = new ButtonLink(ps);
      edit.label = "I#" + getMnemonicCode() + "#";
      message.setLink(edit.toPlainLink());

      message.setMessageBody(JSP.makeTag("I","",JSP.convertLineFeedToBR(JSP.limWr(this.getDescription(), 50))) +"<br>"+issueHistory.getComment());
      message.setReceived(new Date());
      message.store();
    }
  }

  public void generateCommentMessageForExternalResource(IssueHistory issueHistory) throws PersistenceException {
    if (JSP.ex(getExtRequesterEmail())) {
      String subject=I18n.get("NEW_COMMENT_ON_ISSUE") + ": " + "I#" + getMnemonicCode() + "#";
      String message=I18n.get("NEW_COMMENT_ON_ISSUE") + ": "+JSP.makeTag("I","",JSP.limWr(this.getDescription(), 50)) +"<br><blockquote>"+ JSP.encode(issueHistory.getComment())+"</blockquote>";
      Map<String, String> params = new Hashtable();
      params.put("TICKETS_REVIEW_URL", ApplicationState.serverURL + "/TICKETS/" + StringUtilities.generateKeyForEmail(getExtRequesterEmail()));
      message+="<br><br>"+ I18n.get("ISSUE_EXTERNAL_REQUEST_LINK", params);
      MailHelper.sendHtmlMailInSeparateThread(null,getExtRequesterEmail(), subject,message);
    }
  }


  /**
   *  Invia una mail al richiedente esterno
   * @param justAdded
   * @param hasBeenClosed
   * @param justAssigned
   */
  @Transient
  public void generateMessageForExternalRequester(boolean justAdded, boolean hasBeenClosed, boolean justAssigned)  {
    if (JSP.ex(getExtRequesterEmail())) {
      Map<String, String> params = new Hashtable();
      params.put("ISSUE_CODE_ID", getMnemonicCode() + "");
      params.put("ISSUE_DESCRIPTION", JSP.encode(getDescription()));
      params.put("TICKETS_REVIEW_URL", ApplicationState.serverURL + "/TICKETS/" + StringUtilities.generateKeyForEmail(getExtRequesterEmail()));
      String message="";
      String subject="";
      if (justAdded) {
        //bisogna mandare al requester una mail con il link alle sue issue
        subject = I18n.get("ISSUE_CREATED_EXTERNAL_REQUEST_MESSAGE_SUBJECT", params);
        message = I18n.get("ISSUE_CREATED_EXTERNAL_REQUEST_MESSAGE_BODY", params);
      } else if (hasBeenClosed) {
        subject = I18n.get("ISSUE_CLOSED_EXTERNAL_REQUEST_MESSAGE_SUBJECT", params);
        message = I18n.get("ISSUE_CLOSED_EXTERNAL_REQUEST_MESSAGE_BODY", params);
      } else if(justAssigned){
        subject = I18n.get("ISSUE_JUST_ASSIGNED_MESSAGE_SUBJECT", params);
        message = I18n.get("ISSUE_JUST_ASSIGNED_MESSAGE_BODY", params);
      } else {
        subject = I18n.get("ISSUE_UPDATED_EXTERNAL_REQUEST_MESSAGE_SUBJECT", params);
        message = I18n.get("ISSUE_UPDATED_EXTERNAL_REQUEST_MESSAGE_BODY", params);
      }

      message+="<br>"+ I18n.get("ISSUE_EXTERNAL_REQUEST_LINK", params);

      //gli si dice chi ha preso in carico la mail? no!
      MailHelper.sendHtmlMailInSeparateThread(null,getExtRequesterEmail(), subject,message);
    }
  }



  public void riseCommentAddedEvent(TeamworkOperator sender,String comment) throws StoreException {
    // rise event on task
    if (getTask() != null) {
      SomethingHappened change = new SomethingHappened();
      change.setIdAsNew();
      change.setEventType(Task.Event.TASK_DIARY_CHANGE + "");
      change.setWhoCausedTheEvent(sender);
      change.getMessageParams().put("SUBJECT_REPLACEMENT", I18n.get("NEW_COMMENT_ON_ISSUE") + ": " + "I#" + getId() + "#");
      change.setMessageTemplate("TASK_ISSUE_ADD_COMMENT_MESSAGE_TEMPLATE");
      change.getMessageParams().put("issueCode", "I#" + getId() + "#");
      change.getMessageParams().put("task", getTask().getDisplayName());
      change.getMessageParams().put("comment", JSP.encode(JSP.limWr(comment, 500)));

      PageSeed ps = new PageSeed(ApplicationState.serverURL + "/i/" + getId());
      ButtonLink edit = new ButtonLink(ps);
      edit.label = "I#" + getMnemonicCode() + "#";
      change.setLink(edit.toPlainLink());
      change.setIdentifiable(getTask());
      change.store();
    }
  }


  @Override
public void remove(org.jblooming.persistence.hibernate.PersistenceContext pc) throws RemoveException {
    for (PersistentFile pf:getFiles()){
      pf.delete();
    }

    Task t = getTask();
    super.remove(pc);
    if (t != null) {
      t.markAsDirty();
    }
  }


  @Override
public void store(org.jblooming.persistence.hibernate.PersistenceContext pc) throws StoreException {
    Task t = getTask();
    super.store(pc);
    if (t != null) {
      t.markAsDirty();
    }
  }


  @Override
  public void recomputeDenormalizedFields(PersistenceContext pc) {
    updateWorklogDone();
  }


  @Override
public Issue clone(){
    Issue clonedIssue = new Issue();
    clonedIssue.setIdAsNew();
    clonedIssue.setArea(getArea());
    clonedIssue.setAssignedBy(getAssignedBy());
    clonedIssue.setAssignedTo(getAssignedTo());
    //clonedIssue.setDateSignalled(getDateSignalled());
    clonedIssue.setDateSignalled(new Date()); // RB è una nuova issue non deve mantenere la vecchia data
    clonedIssue.setDescription(getDescription());
    clonedIssue.setEstimatedDuration(getEstimatedDuration());
    clonedIssue.setGravity(getGravity());
    clonedIssue.setImpact(getImpact());
    clonedIssue.setShouldCloseBy(getShouldCloseBy());
    clonedIssue.setOrderFactor(getOrderFactor());
    clonedIssue.setOrderFactorByResource(getOrderFactor());
    clonedIssue.setOwner(getOwner());
    clonedIssue.setIssueStatus(getStatus());
    clonedIssue.setTask(getTask());
    clonedIssue.setType(getType());
    clonedIssue.setJsonData(getJsonData());

    return clonedIssue;
  }

}
