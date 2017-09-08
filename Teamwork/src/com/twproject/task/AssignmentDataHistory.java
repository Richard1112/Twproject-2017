package com.twproject.task;

import net.sf.json.JSONObject;
import org.hibernate.annotations.Index;
import org.hibernate.annotations.Type;
import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.ReflectionUtilities;

import javax.persistence.*;
import java.io.Serializable;
import java.util.Date;

@Entity
@Table(name = "twk_assignment_data_hist")
public class AssignmentDataHistory extends IdentifiableSupport {

  private String assignmentId = "";
  private Date createdOn;

  private long worklogDone;
  private long estimatedWorklog;

  private double hourlyCost;
  private double budget;

  private double costDone = 0;  //non includono i costi del wl che si estraggono dal wldone*hourlycost
  private double costEstimated = 0; //non includono i costi del wl


  public AssignmentDataHistory() {
  }

  @Id
  @Type(type = "int")
  @GeneratedValue(strategy = GenerationType.AUTO)
  public Serializable getId() {
    return super.getId();
  }

  @Transient
  public String getName() {
    return id + " assignment:" + getAssignmentId() + " createdOn:" + DateUtilities.dateAndHourToString(getCreatedOn());
  }


  @Index(name = "idx_assdathist_assid")
  public String getAssignmentId() {
    return assignmentId;
  }

  public void setAssignmentId(String assignmentId) {
    this.assignmentId = assignmentId;
  }

  @Index(name = "idx_tskdathist_createdon")
  public Date getCreatedOn() {
    return createdOn;
  }

  public void setCreatedOn(Date createdOn) {
    this.createdOn = createdOn;
  }


  public long getWorklogDone() {
    return worklogDone;
  }

  public void setWorklogDone(long worklogDone) {
    this.worklogDone = worklogDone;
  }

  public long getEstimatedWorklog() {
    return estimatedWorklog;
  }

  public void setEstimatedWorklog(long estimatedWorklog) {
    this.estimatedWorklog = estimatedWorklog;
  }

  public double getHourlyCost() {
    return hourlyCost;
  }

  public void setHourlyCost(double hourlyCost) {
    this.hourlyCost = hourlyCost;
  }

  public double getBudget() {
    return budget;
  }

  public void setBudget(double budget) {
    this.budget = budget;
  }

  public double getCostDone() {
    return costDone;
  }

  public void setCostDone(double costDone) {
    this.costDone = costDone;
  }

  public double getCostEstimated() {
    return costEstimated;
  }

  public void setCostEstimated(double costEstimated) {
    this.costEstimated = costEstimated;
  }


  public JSONObject jsonify() {
    JSONObject jso = ReflectionUtilities.jsonify(this, "assignmentId", "worklogDone", "estimatedWorklog", "hourlyCost", "budget", "costDone", "costEstimated");
    jso.element("createdOn",getCreatedOn().getTime());
    return jso;
  }
}