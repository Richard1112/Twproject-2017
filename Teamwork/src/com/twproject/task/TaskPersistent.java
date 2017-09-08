/*
 * Created by IntelliJ IDEA.
 * User: ppolsinelli
 * Date: 10-gen-2006
 * Time: 9.53.59
 */
package com.twproject.task;

import com.teamwork.expand.TaskReport;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import javax.persistence.Transient;

import org.apache.lucene.analysis.core.StopAnalyzer;
import org.hibernate.search.annotations.Analyzer;
import org.hibernate.search.annotations.Boost;
import org.hibernate.search.annotations.Field;
import org.hibernate.search.annotations.Fields;
import org.jblooming.agenda.Period;
import org.jblooming.classification.Taggable;
import org.jblooming.designer.DesignerData;
import org.jblooming.ontology.Documentable;
import org.jblooming.ontology.SecuredNodeWithAreaSupport;
import org.jblooming.ontology.SerializedMap;
import org.jblooming.oql.OqlQuery;
import org.jblooming.oql.QueryHelper;
import org.jblooming.persistence.exceptions.FindException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.settings.I18n;

import com.twproject.document.TeamworkDocument;
import com.twproject.forum.TeamworkForumEntry;
import com.twproject.task.financial.Cost;
import com.twproject.task.process.TaskProcess;

import net.sf.json.JSONObject;

public abstract class TaskPersistent extends SecuredNodeWithAreaSupport implements Taggable, Documentable {

  private String code;
  private String name;
  private TaskType type;
  private String description;
  private String status;
  /**
   * the actual task relevance (for the task manager)
   */
  private int relevance = 0;
  /**
   * degree of completion of this task
   */
  private double progress = 0;
  protected Set<Assignment> assignments = new HashSet<>();
  private Set<Issue> issues = new HashSet<>();

  private boolean progressByWorklog = false;

  private long totalWorklogDone;
  private long totalWorklogEstimated;

  private double totalCostsDone;
  private double totalCostsEstimated;

  private long totalEstimatedFromIssues;
  private int totalIssues;
  private int totalIssuesOpen;
  private int totalIssuesScoreOpen;
  private int totalIssuesScoreClosed;


  private Period schedule;
  private Set<TaskScheduleHistory> scheduleHistory = new HashSet<>();

  private int duration;

  private Set<TaskStatusHistory> statusHistory = new HashSet<>();
  private Set<TaskDependency> previouses = new HashSet<>();
  private Set<TaskDependency> nexts = new HashSet<>();
  private Set<TeamworkDocument> documents = new HashSet<>();
  
  private Set<TaskReport> reportSet = new HashSet<TaskReport>();


  private TeamworkForumEntry forumEntry;// = new TeamworkForumEntry();
  private String externalCode;
  private String costCenter;
  private String notes;
  private String orderFactor;
  private boolean startIsMilestone = false;
  private boolean endIsMilestone = false;

  /**
   * BEGIN FINANTIAL PROPERTIES
   */
  private double forecasted;
  private Set<Cost> costs = new TreeSet<>();

  private String customField1;
  private String customField2;
  private String customField3;
  private String customField4;
  private String customField5;
  private String customField6;


  private String budgetCustomField1;
  private String budgetCustomField2;
  private String budgetCustomField3;
  private String budgetCustomField4;


  // workflow inverse   
  private TaskProcess taskProcess;

  private String tags;
  private String color;

  @Deprecated
//  private SerializedMap<String, String> options = new SerializedMap<String, String>();
  private JSONObject jsonData =new JSONObject();


  public Iterator getIssuesIterator() {
    return issues.iterator();
  }

  public boolean issuesContains(Issue issue) {
    return issues.contains(issue);
  }

  public int issuesSize() {
    return issues.size();
  }

  /**
   * task manager sets it by hand
   */
  public void setTaskProgress(double progress) {
  }

  /**
   *
   */
  public double getTaskProgress() {
    return 0;
  }

  public Period getSchedule() {
    return schedule;
  }

  public void setSchedule(Period schedule) {
    this.schedule = schedule;
  }

  public double getProgress() {
    return progress;
  }

  public void setProgress(double progress) {
    this.progress = progress;
  }

  public Set getDocuments() {
    return documents;
  }

  private void setDocuments(Set documents) {
    this.documents = documents;
  }

  public void addDocument(TeamworkDocument document) {
    documents.add(document);
  }

  public void removeDocument(TeamworkDocument document) {
    documents.remove(document);
  }

  public Iterator<TeamworkDocument> getDocumentsIterator() {
    return documents.iterator();
  }

  public Set<Assignment> getAssignments() {
    return assignments;
  }

  private void setAssignments(Set<Assignment> assignments) {
    this.assignments = assignments;
  }

  public Iterator<Assignment> getAssignementsIterator() {
    return getAssignments().iterator();
  }

  public List<Assignment> getAssignementsSortedByRole() throws FindException {
    String hql = "from " + Assignment.class.getName() + " as assig where assig.task = :task order by assig.role.name, assig.resource.name";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setEntity("task", this);
    return oql.list();
  }


  public List<Assignment> getActiveAssignements(boolean activeOnly) throws PersistenceException {
    String hql = "from " + Assignment.class.getName() + " as assig where assig.task = :task ";
    QueryHelper qhelp = new QueryHelper(hql);
    if (activeOnly) {
      qhelp.addOQLClause("assig.enabled = :active ");
      qhelp.setParameter("active", activeOnly);
    }
    qhelp.setParameter("task", this);
    return qhelp.toHql().list();
  }


  public TaskProcess getTaskProcess() {
    return taskProcess;
  }

  public void setTaskProcess(TaskProcess dontUseMe) {
    taskProcess = dontUseMe;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public String getCode() {
    return code;
  }

  public void setCode(String code) {
    this.code = code;
  }

  public TaskType getType() {
    return type;
  }

  public void setType(TaskType type) {
    this.type = type;
  }

  public String getStatus() {
    return status;
  }

  public void setStatus(String status) {
    this.status = status;
  }

  public int getRelevance() {
    return relevance;
  }

  public void setRelevance(int relevance) {
    this.relevance = relevance;
  }

  public Set<Issue> getIssues() {
    return issues;
  }

  private void setIssues(Set<Issue> issues) {
    this.issues = issues;
  }

  public Set<TaskScheduleHistory> getScheduleHistory() {
    return scheduleHistory;
  }

  private void setScheduleHistory(Set<TaskScheduleHistory> scheduleHistory) {
    this.scheduleHistory = scheduleHistory;
  }

  public void addScheduleHistory(TaskScheduleHistory history) {
    scheduleHistory.add(history);
  }

  public void removeScheduleHistory(TaskScheduleHistory history) {
    scheduleHistory.remove(history);
  }

  public Iterator<TaskScheduleHistory> getTaskScheduleHistoryIterator() {
    return scheduleHistory.iterator();
  }

  public boolean scheduleHistoryContains(TaskScheduleHistory history) {
    return scheduleHistory.contains(history);
  }

  public int scheduleHistorySize() {
    return scheduleHistory.size();
  }

  public Set<TaskStatusHistory> getStatusHistory() {
    return statusHistory;
  }

  private void setStatusHistory(Set<TaskStatusHistory> statusHistory) {
    this.statusHistory = statusHistory;
  }

  public void addStatusHistory(TaskStatusHistory history) {
    statusHistory.add(history);
  }

  public void removeStatusHistory(TaskStatusHistory history) {
    statusHistory.remove(history);
  }

  public Iterator<TaskStatusHistory> getStatusHistoryIterator() {
    return statusHistory.iterator();
  }

  public boolean statusHistoryContains(TaskStatusHistory history) {
    return statusHistory.contains(history);
  }

  public int statusHistorySize() {
    return statusHistory.size();
  }


  public void setName(String name) {
    this.name = name;
  }

  @Field(name = "content")
  @Boost(3)
  public String getName() {
    return name;
  }


  //  protected Set<TaskDependency> getPreviouses() {

  public Set<TaskDependency> getPreviouses() {
    return previouses;
  }

  public void setPreviouses(Set<TaskDependency> previouses) {
    this.previouses = previouses;
  }

  public Iterator<TaskDependency> getPreviousesModifiableIterator() {
    return new HashSet(previouses).iterator();
  }

  public TeamworkForumEntry getForumEntry() {
//    if (forumEntry == null)
//      forumEntry = new TeamworkForumEntry();
    return forumEntry;
  }

  public void setForumEntry(TeamworkForumEntry forumEntry) {
    this.forumEntry = forumEntry;
  }

  public String getExternalCode() {
    return externalCode;
  }

  public void setExternalCode(String externalCode) {
    this.externalCode = externalCode;
  }

  public String getCostCenter() {
    return costCenter;
  }

  public void setCostCenter(String costCenter) {
    this.costCenter = costCenter;
  }

  public boolean isStartIsMilestone() {
    return startIsMilestone;
  }

  public void setStartIsMilestone(boolean startIsMilestone) {
    this.startIsMilestone = startIsMilestone;
  }

  public boolean isEndIsMilestone() {
    return endIsMilestone;
  }

  public void setEndIsMilestone(boolean endIsMilestone) {
    this.endIsMilestone = endIsMilestone;
  }

  public int getAssignementsSize() {
    return getAssignments().size();
  }

  public String getNotes() {
    return notes;
  }

  public void setNotes(String notes) {
    this.notes = notes;
  }

  public Set<TaskDependency> getNexts() {
    return nexts;
  }


  public void setNexts(Set<TaskDependency> nexts) {
    this.nexts = nexts;
  }

  public int getPreviousesSize() {
    return previouses.size();
  }

  public int getNextsSize() {
    return nexts.size();
  }

  public double getForecasted() {
    return forecasted;
  }

  public void setForecasted(double forecasted) {
    this.forecasted = forecasted;
  }


  public Set<Cost> getCosts() {
    return costs;
  }

  public void setCosts(Set<Cost> costs) {
    this.costs = costs;
  }

  public boolean isProgressByWorklog() {
    return progressByWorklog;
  }

  public void setProgressByWorklog(boolean progressByWorklog) {
    this.progressByWorklog = progressByWorklog;
  }


  public int getDuration() {
    return duration;
  }

  public void setDuration(int durationInWorkingDays) {
    this.duration = durationInWorkingDays;
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

  @Field(name = "content")
  @Boost(2)
  public String getTags() {
    return tags;
  }

  public void setTags(String tags) {
    this.tags = tags;
  }


  public String getAbstractForIndexing() {

    String logContent = "";

    //get changes log
    Set<TaskStatusHistory> tshs = getStatusHistory();
    if (tshs.size()>0){
      //logContent+= I18n.get("STATUS_CHANGE_HISTORY")+"\n";
      for (TaskStatusHistory tsh : tshs) {
        if (JSP.ex(tsh.getChangeLog()))
          logContent = logContent + JSP.w(tsh.getChangeLog()) + "\n";

      }
    }

    Set<TaskScheduleHistory> tsdh = getScheduleHistory();
    if (tsdh.size()>0){
      //logContent+= I18n.get("DATE_CHANGES")+"\n";
      for (TaskScheduleHistory tsh : tsdh) {
        if (JSP.ex(tsh.getChangeLog())){
          logContent = logContent +JSP.w(tsh.getChangeLog()) + "\n";
        }
      }
    }

    //recover designer data
    List<DesignerData> datas = DesignerData.getAllInstances(getId(), Task.class.getName());
    StringBuffer bb = new StringBuffer();
    for (DesignerData dd : datas) {
      for (String val : dd.getValueMap().values()) {
        if (JSP.ex(val))
          bb.append(JSP.w(val) + " ");
      }
    }


    return JSP.w(
            //" T#" + getMnemonicCode() +"# "+ require hours
            "T#" + (JSP.ex(getCode()) ? getCode() : getId()) + "# " +
                    getName() + "\n" +
                    (JSP.ex(getDescription()) ? JSP.w(getDescription()) + "\n" : "") +
                    (JSP.ex(getTags()) ? JSP.w(getTags()) + "\n" : "") +
                    (JSP.ex(getNotes()) ? JSP.w(getNotes()) + "\n" : "") +
                    JSP.w(getCustomField1()) + " " +
                    JSP.w(getCustomField2()) + " " +
                    JSP.w(getCustomField3()) + " " +
                    JSP.w(getCustomField4()) + " " +
                    JSP.w(getCustomField5()) + " " +
                    JSP.w(getCustomField6()) +
                    (JSP.ex(logContent) ? "\n" + logContent : "") +
                    (bb.length() > 0 ? "\n" + JSP.w(bb.toString()) : "")
    );
  }

  @Fields({
          @Field(name = "fullcontent", analyzer = @Analyzer(impl = StopAnalyzer.class)),
          @Field(name = "content")
  })
  private String getContentForIndexing() {
    return getAbstractForIndexing();
  }

//  public SerializedMap<String, String> getOptions() {
//    return options;
//  }
//  public void setOptions(SerializedMap<String, String> options) {
//    this.options = options;
//  }


  public JSONObject getJsonData(){
    return this.jsonData;
  }
  public void setJsonData(JSONObject jsonData){
    this.jsonData = jsonData;
  }



  @Transient
  public String getMnemonicCode() {
    String codeOrId = getId() + "";
    if (JSP.ex(getCode()) && isUnique("code")) {
      codeOrId = getCode();
    }
    return codeOrId;
  }

  public long getTotalWorklogDone() {
    return totalWorklogDone;
  }

  public void setTotalWorklogDone(long totalWorklogDone) {
    this.totalWorklogDone = totalWorklogDone;
  }

  public long getTotalWorklogEstimated() {
    return totalWorklogEstimated;
  }

  public void setTotalWorklogEstimated(long totalWorklogEstimated) {
    this.totalWorklogEstimated = totalWorklogEstimated;
  }

  public long getTotalEstimatedFromIssues() {
    return totalEstimatedFromIssues;
  }

  public void setTotalEstimatedFromIssues(long totalEstimatedFromIssues) {
    this.totalEstimatedFromIssues = totalEstimatedFromIssues;
  }

  public double getTotalCostsDone() {
    return totalCostsDone;
  }

  public void setTotalCostsDone(double totalCostsDone) {
    this.totalCostsDone = totalCostsDone;
  }

  public double getTotalCostsEstimated() {
    return totalCostsEstimated;
  }

  public void setTotalCostsEstimated(double totalCostsEstimated) {
    this.totalCostsEstimated = totalCostsEstimated;
  }

  public int getTotalIssues() {
    return totalIssues;
  }

  public void setTotalIssues(int totalIssues) {
    this.totalIssues = totalIssues;
  }

  public int getTotalIssuesOpen() {
    return totalIssuesOpen;
  }

  public void setTotalIssuesOpen(int totalIssuesOpen) {
    this.totalIssuesOpen = totalIssuesOpen;
  }

  public int getTotalIssuesScoreOpen() {
    return totalIssuesScoreOpen;
  }

  public void setTotalIssuesScoreOpen(int totalIssuesScoreOpen) {
    this.totalIssuesScoreOpen = totalIssuesScoreOpen;
  }

  public int getTotalIssuesScoreClosed() {
    return totalIssuesScoreClosed;
  }

  public void setTotalIssuesScoreClosed(int totalIssuesScoreClosed) {
    this.totalIssuesScoreClosed = totalIssuesScoreClosed;
  }

  public String getOrderFactor() {
    return orderFactor;
  }

  public void setOrderFactor(String orderFactor) {
    this.orderFactor = orderFactor;
  }

  public String getBudgetCustomField1() {
    return budgetCustomField1;
  }

  public void setBudgetCustomField1(String budgetCustomField1) {
    this.budgetCustomField1 = budgetCustomField1;
  }

  public String getBudgetCustomField2() {
    return budgetCustomField2;
  }

  public void setBudgetCustomField2(String budgetCustomField2) {
    this.budgetCustomField2 = budgetCustomField2;
  }

  public String getBudgetCustomField3() {
    return budgetCustomField3;
  }

  public void setBudgetCustomField3(String budgetCustomField3) {
    this.budgetCustomField3 = budgetCustomField3;
  }

  public String getBudgetCustomField4() {
    return budgetCustomField4;
  }

  public void setBudgetCustomField4(String budgetCustomField4) {
    this.budgetCustomField4 = budgetCustomField4;
  }

  public String getColor() {
    return color;
  }

  public Set<TaskReport> getReportSet() {
	return reportSet;
}

public void setReportSet(Set<TaskReport> reportSet) {
	this.reportSet = reportSet;
}

public void setColor(String color) {
    this.color = color;
  }
}