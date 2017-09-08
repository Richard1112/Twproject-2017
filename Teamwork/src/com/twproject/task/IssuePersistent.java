package com.twproject.task;

import com.opnlb.fulltext.IndexingMachine;
import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Resource;
import com.twproject.worklog.Worklog;
import net.sf.json.JSONArray;
import net.sf.json.JSONObject;
import org.apache.lucene.analysis.core.StopAnalyzer;
import org.hibernate.annotations.ForeignKey;
import org.hibernate.annotations.Index;
import org.hibernate.annotations.Type;
import org.hibernate.search.annotations.*;
import org.jblooming.classification.Taggable;
import org.jblooming.logging.Auditable;
import org.jblooming.logging.Sniffable;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.ontology.SecuredSupportWithArea;
import org.jblooming.operator.Operator;
import org.jblooming.persistence.exceptions.RemoveException;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.security.Area;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.settings.ApplicationState;

import javax.persistence.*;
import java.util.*;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
@MappedSuperclass
public abstract class IssuePersistent extends SecuredSupportWithArea implements Auditable, Sniffable, Taggable {

  private Operator owner;
  private IssueStatus status;
  private String gravity;

  private IssueType type;

  private IssueImpact impact;
  private double orderFactor;
  private double orderFactorByResource;

  private String description;
  private Date shouldCloseBy;
  private Date dateSignalled;
  private Task task;
  private Resource assignedTo;
  private Resource assignedBy;


  private JSONObject jsonData =new JSONObject();

  private Date lastStatusChangeDate;
  private long estimatedDuration=0;
  private long worklogDone=0;

  private String tags;

  private String customField1;
  private String customField2;
  private String customField3;
  private String customField4;
  private String customField5;
  private String customField6;

  private String code;
  private String extRequesterEmail;

  //read only
  protected Set<Worklog> worklogs = new HashSet<Worklog>();

  //read only
  private List<IssueHistory> issueHistories = new LinkedList<IssueHistory>();


  public IssuePersistent() {
  }

  //robicch 26/6/2017 la cancellazione della issue non deve fare il cascade
  //@OneToMany(cascade = CascadeType.REMOVE, targetEntity = Worklog.class)

  @OneToMany(targetEntity = Worklog.class)
  @JoinColumn(name = "issue")
  @OrderBy("inserted desc")
  public Set<Worklog> getWorklogs() {
    return worklogs;
  }

  private void setWorklogs(Set<Worklog> worklogs) {
    this.worklogs = worklogs;
  }

  @ManyToOne(targetEntity = IssueStatus.class)
  @ForeignKey(name = "fk_issue_issStatus")
  @Index(name = "idx_issue_issStatus")
  @JoinColumn(name = "statusx")
  public IssueStatus getStatus() {
    return status;
  }


  //wrapped by issue.setIssueStatus()
  protected void setStatus(IssueStatus status) {
    this.status = status;
  }

  @Lob
  @Column(name = "descriptionx")
  @Type(type = "org.hibernate.type.TextType")
  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public String getGravity() {
    return gravity;
  }

  public void setGravity(String gravity) {
    this.gravity = gravity;
  }

  public Date getShouldCloseBy() {
    return shouldCloseBy;
  }

  public void setShouldCloseBy(Date shouldCloseBy) {
    this.shouldCloseBy = shouldCloseBy;
  }

  @ManyToOne(targetEntity = Task.class, fetch = FetchType.LAZY)
  @ForeignKey(name = "fk_issue_task")
  @Index(name = "idx_issue_task")
  public Task getTask() {
    return task;
  }

  public void setTask(Task task) {
    this.task = task;
  }

  @ManyToOne(targetEntity = Resource.class, fetch = FetchType.LAZY)
  @ForeignKey(name = "fk_issue_worker")
  @Index(name = "idx_issue_worker")
  public Resource getAssignedTo() {
    return assignedTo;
  }

  public void setAssignedTo(Resource assignedTo) {
    this.assignedTo = assignedTo;
  }

  @ManyToOne(targetEntity = TeamworkOperator.class, fetch = FetchType.LAZY)
  @ForeignKey(name = "fk_issue_owner")
  @Index(name = "idx_issue_owner")
  @JoinColumn(name = "ownerx")
  public Operator getOwner() {
    return owner;
  }

  public void setOwner(Operator owner) {
    this.owner = owner;
  }

  @Type(type = "org.jblooming.ontology.JSONObjectType")
  public JSONObject getJsonData(){
    return this.jsonData;
  }

  public void setJsonData(JSONObject jsonData){
    this.jsonData = jsonData;
  }

  @Transient
  public List<PersistentFile> getFiles(){
    List<PersistentFile> ret= new ArrayList<PersistentFile>();

    if (getJsonData().has("__files__")) {
      for (Object o : getJsonData().getJSONArray("__files__"))
      ret.add(PersistentFile.deserialize(o.toString()));
    }
    return Collections.unmodifiableList(ret);
  }

  @Transient
  public void addFile(PersistentFile newFile){
    if (!getJsonData().has("__files__")) {
      getJsonData().element("__files__", new JSONArray());
    }
    JSONArray files=getJsonData().getJSONArray("__files__");

    if (!files.contains(newFile.serialize()))
      files.add(newFile.serialize());
  }

  @Transient
  public void removeFile(PersistentFile file) {
    if (getJsonData().has("__files__")) {
      getJsonData().getJSONArray("__files__").remove(file.serialize());
    }
  }

  @ManyToOne(targetEntity = IssueImpact.class)
  @ForeignKey(name = "fk_issue_impact")
  @Index(name = "idx_issue_impact")
  @JoinColumn(name = "impact")
  public IssueImpact getImpact() {
    return impact;
  }

  public void setImpact(IssueImpact impact) {
    this.impact = impact;
  }

  @ManyToOne(targetEntity = IssueType.class)
  @ForeignKey(name = "fk_issue_type")
  @Index(name = "idx_issue_type")
  @JoinColumn(name = "type")
  public IssueType getType() {
    return type;
  }

  public void setType(IssueType type) {
    this.type = type;
  }

  public Date getLastStatusChangeDate() {
    return lastStatusChangeDate;
  }

  //should never used by hand. Called whene issue.setIssueStatus()
  protected void setLastStatusChangeDate(Date lastStatusChangeDate) {
    this.lastStatusChangeDate = lastStatusChangeDate;
  }

  @ManyToOne(targetEntity = Resource.class)
  @ForeignKey(name = "fk_issue_assigner")
  @Index(name = "idx_issue_assigner")
  public Resource getAssignedBy() {
    return assignedBy;
  }

  public void setAssignedBy(Resource assignedBy) {
    this.assignedBy = assignedBy;
  }

  @Column(name = "estimatedDuration")
  public long getEstimatedDuration() {
    return estimatedDuration;
  }

  public void setEstimatedDuration(long estimatedDuration) {
    this.estimatedDuration = estimatedDuration;
  }

  public double getOrderFactor() {
    return orderFactor;
  }

  public void setOrderFactor(double orderFactor) {
    this.orderFactor = orderFactor;
  }


  public Date getDateSignalled() {
    return dateSignalled;
  }

  public void setDateSignalled(Date dateSignalled) {
    this.dateSignalled = dateSignalled;
  }

  @Column(length = 1024)
  @Field(name = "content", analyze = Analyze.YES ,index = org.hibernate.search.annotations.Index.YES, store = Store.NO)
  @Boost(2)
  public String getTags() {
    return tags;
  }

  public void setTags(String tags) {
    this.tags = tags;
  }


  @Transient
  public String getAbstractForIndexing() {
    String afi =
            ("I#"+(JSP.ex(getCode())?getCode():getId())+"#\n "+ getDescription() +"\n"+
            (getStatus()!=null?getStatus().getDescription():"")+ " "+getGravity()+" "+
            (getTask() != null ? getTask().getDisplayName() + "\n" : "") + " "+
            (getAssignedTo() != null ? getAssignedTo().getDisplayName() + "\n" : "") +
            (getShouldCloseBy() != null ? JSP.w(getShouldCloseBy()) : "") + (getEstimatedDuration()!=0? DateUtilities.getMillisInHoursMinutes(getEstimatedDuration()):"")+" \n"+
            JSP.w(getTags(), "\n") + " " +
            JSP.w(getCustomField1()) + " " +
            JSP.w(getCustomField2()) + " " +
            JSP.w(getCustomField3()) + " " +
            JSP.w(getCustomField4()) + " " +
            JSP.w(getCustomField5()) + " " +
            JSP.w(getCustomField6())).trim();

    for (IssueHistory ih : getIssueHistories()) {
      if (JSP.ex(ih.getComment()))
        afi = afi + JSP.w(ih.getComment(), "\n");
    }
    return afi;
  }

  @Transient
  @Fields({
          @Field(name = "fullcontent", analyzer = @Analyzer(impl = StopAnalyzer.class)),
          @Field(name = "content")
  })
  private String getContentForIndexing() {

    for (PersistentFile pf:getFiles()){
      IndexingMachine.addToBeIndexed(this, getArea().getId(),pf);
    }
    return getAbstractForIndexing();
  }

  @Column(name = "orderfactorbyres")
  public double getOrderFactorByResource() {
    return orderFactorByResource;
  }

  public void setOrderFactorByResource(double orderFactorByResource) {
    this.orderFactorByResource = orderFactorByResource;
  }

  @ManyToOne(targetEntity = Area.class)
  @JoinColumn(name = "areax")
  @ForeignKey(name = "fk_issue_area")
  @Index(name = "idx_issue_area")
  public Area getArea() {
    return super.getArea();
  }

  public void setArea(Area area) {
    super.setArea(area);
  }


  @Transient
  public void setArea(TeamworkOperator logged) {
    if (getTask() != null)
      setArea(getTask().getArea());
    else if (getAssignedTo() != null)
      setArea(getAssignedTo().getArea());
    else
      setArea(logged.getPerson().getArea());
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

  public String getCustomField5() {
    return customField5;
  }

  public void setCustomField5(String customField5) {
    this.customField5 = customField5;
  }

  public String getCustomField6() {
    return customField6;
  }

  public void setCustomField6(String customField6) {
    this.customField6 = customField6;
  }

  @OneToMany(cascade = CascadeType.REMOVE, targetEntity = IssueHistory.class)
  @JoinColumn(name = "issue")
  @OrderBy("creationDate")
  public List<IssueHistory> getIssueHistories() {
    return issueHistories;
  }

  public void setIssueHistories(List<IssueHistory> issueHistories) {
    this.issueHistories = issueHistories;
  }

  @Column (length = 30,name = "codex")
  @Index(name = "idx_issue_code")
  public String getCode() {
    return code;
  }

  public void setCode(String code) {
    this.code = code;
  }

  @Transient
  public String getMnemonicCode(){
    String codeOrId=getId()+"";
    if (org.jblooming.waf.constants.Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("USECODEONISSUES")) && JSP.ex(getCode()) && isUnique("code")) {
      codeOrId=getCode();
    }
    return codeOrId;
  }

  @Column(name="worklogDone")
  public long getWorklogDone() {
    return worklogDone;
  }

  public void setWorklogDone(long worklogDone) {
    this.worklogDone = worklogDone;
  }


  @Column (length = 60)
  @Index(name = "idx_issue_extemail")
  public String getExtRequesterEmail() {
    return extRequesterEmail;
  }

  public void setExtRequesterEmail(String extRequesterEmail) {
    this.extRequesterEmail = extRequesterEmail;
  }



}