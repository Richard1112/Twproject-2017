package com.twproject.task;

import org.jblooming.ontology.LoggableIdentifiableSupport;
import org.jblooming.ontology.SerializedMap;
import org.jblooming.agenda.Period;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class TaskStatusHistory extends LoggableIdentifiableSupport {

  private Task task;
  private String fromStatus;
  private String toStatus;
  private String changeLog;


  private Period schedule;


  public String getChangeLog() {
    return changeLog;
  }

  public void setChangeLog(String changeLog) {
    this.changeLog = changeLog;
  }

  public Task getTask() {
    return task;
  }

  public void setTask(Task task) {
    this.task = task;
  }

  public String getFromStatus() {
    return fromStatus;
  }

  public void setFromStatus(String fromStatus) {
    this.fromStatus = fromStatus;
  }

  public String getToStatus() {
    return toStatus;
  }

  public void setToStatus(String toStatus) {
    this.toStatus = toStatus;
  }
  
}
