package com.twproject.task;

import org.jblooming.agenda.Period;
import org.jblooming.agenda.Schedule;

import java.util.Date;

/**
 * Created by Open Lab
 * info@open-lab.com
 * Date: Nov 6, 2006
 * Time: 9:43:00 AM
 */
public class TaskScheduleCandidate {
  public Task task;
  public Date start;
  public Date end;
  public int duration;
  public TaskScheduleHistory taskScheduleHistory;
  boolean visited = false;
  boolean modifiedByHand;
  public boolean toBeShifted = false;
  public Schedule oldSchedule=null;


  public TaskScheduleCandidate(Task task) {
    this.task = task;
    this.oldSchedule=task.getSchedule()==null?null:task.getSchedule().getPeriod();
  }


}
