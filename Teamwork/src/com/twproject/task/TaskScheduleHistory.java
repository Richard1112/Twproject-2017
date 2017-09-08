package com.twproject.task;

import org.jblooming.agenda.Period;
import org.jblooming.ontology.LoggableIdentifiableSupport;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class TaskScheduleHistory extends LoggableIdentifiableSupport {

  private Task task;
  private Period schedule;
  private String changeLog;


  public Period getSchedule() {
    return schedule;
  }

  public void setSchedule(Period schedule) {
    this.schedule = schedule;
  }

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
}
