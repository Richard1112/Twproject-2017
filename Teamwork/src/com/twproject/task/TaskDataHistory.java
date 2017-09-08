package com.twproject.task;

import com.twproject.resource.Person;
import net.sf.json.JSONObject;
import org.hibernate.annotations.Index;
import org.hibernate.annotations.Type;
import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.ontology.Pair;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.ReflectionUtilities;

import javax.persistence.*;
import java.io.Serializable;
import java.util.Date;
import java.util.Set;

@Entity
@Table(name = "twk_task_data_hist")
public class TaskDataHistory extends IdentifiableSupport {

  private String taskId = "";
  private Date createdOn;

  private Date startDate;
  private Date endDate;
  private int duration = 0;
  private String status = "";
  private double progress = 0;
  private int teamSize = 0;

  private int totalDocuments = 0;

  private int totalDescendant = 0;
  private int totalDescendantClosed = 0;

  private long totalWorklogDone = 0;
  private long totalWorklogEstimated = 0;

  private double forecasted = 0;
  private double totalCostsDone = 0;
  private double totalCostsEstimated = 0;

  private long totalEstimatedFromIssues = 0;
  private int totalIssues = 0;
  private int totalIssuesOpen = 0;
  private int totalIssuesScoreOpen = 0;
  private int totalIssuesScoreClosed = 0;


  public TaskDataHistory() {
  }

  @Id
  @Type(type = "int")
  @GeneratedValue(strategy = GenerationType.AUTO)
  public Serializable getId() {
    return super.getId();
  }

  @Transient
  public String getName() {
    return id + " task:" + getTaskId() + " createdOn:" + DateUtilities.dateAndHourToString(getCreatedOn());
  }


  @Index(name = "idx_tskdathist_tskid")
  public String getTaskId() {
    return taskId;
  }

  public void setTaskId(String taskId) {
    this.taskId = taskId;
  }

  @Index(name = "idx_tskdathist_createdon")
  public Date getCreatedOn() {
    return createdOn;
  }

  public void setCreatedOn(Date createdOn) {
    this.createdOn = createdOn;
  }

  public Date getStartDate() {
    return startDate;
  }

  public void setStartDate(Date startDate) {
    this.startDate = startDate;
  }

  public Date getEndDate() {
    return endDate;
  }

  public void setEndDate(Date endDate) {
    this.endDate = endDate;
  }

  public int getDuration() {
    return duration;
  }

  public void setDuration(int duration) {
    this.duration = duration;
  }

  public String getStatus() {
    return status;
  }

  public void setStatus(String status) {
    this.status = status;
  }

  public double getProgress() {
    return progress;
  }

  public void setProgress(double progress) {
    this.progress = progress;
  }

  public int getTeamSize() {
    return teamSize;
  }

  public void setTeamSize(int teamSize) {
    this.teamSize = teamSize;
  }

  public int getTotalDocuments() {
    return totalDocuments;
  }

  public void setTotalDocuments(int totalDocuments) {
    this.totalDocuments = totalDocuments;
  }

  public int getTotalDescendant() {
    return totalDescendant;
  }

  public void setTotalDescendant(int totalDescendant) {
    this.totalDescendant = totalDescendant;
  }

  public int getTotalDescendantClosed() {
    return totalDescendantClosed;
  }

  public void setTotalDescendantClosed(int totalDescendantClosed) {
    this.totalDescendantClosed = totalDescendantClosed;
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

  public double getForecasted() {
    return forecasted;
  }

  public void setForecasted(double forecasted) {
    this.forecasted = forecasted;
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

  public long getTotalEstimatedFromIssues() {
    return totalEstimatedFromIssues;
  }

  public void setTotalEstimatedFromIssues(long totalEstimatedFromIssues) {
    this.totalEstimatedFromIssues = totalEstimatedFromIssues;
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


  public JSONObject jsonify() {
    JSONObject jso = ReflectionUtilities.jsonify(this, "taskId", "duration", "status", "progress", "teamSize", "totalDocuments", "totalDescendant", "totalDescendantClosed", "totalWorklogDone",
      "totalWorklogEstimated", "forecasted", "totalCostsDone", "totalCostsEstimated", "totalEstimatedFromIssues", "totalIssues", "totalIssuesOpen", "totalIssuesScoreOpen", "totalIssuesScoreClosed");
    jso.element("createdOn",getCreatedOn().getTime());
    jso.element("startDate",getStartDate().getTime());
    jso.element("endDate",getEndDate().getTime());
    return jso;
  }

}