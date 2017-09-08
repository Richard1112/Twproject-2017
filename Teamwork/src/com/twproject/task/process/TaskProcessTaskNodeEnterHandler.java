package com.twproject.task.process;

import com.twproject.task.TaskStatus;
import com.twproject.task.Task;
import com.twproject.task.TaskScheduleCandidate;
import com.twproject.rank.Hit;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.ontology.Pair;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.messaging.SomethingHappened;
import org.jbpm.graph.exe.ExecutionContext;

import java.util.HashSet;
import java.util.Date;
import java.util.Hashtable;
import java.util.ArrayList;

public class TaskProcessTaskNodeEnterHandler extends TeamworkTaskNodeEventHandler {


  public void doTheRealAction (ExecutionContext executionContext) {    
    try {

      if (!TaskStatus.STATUS_ACTIVE.equals(taskOnStep.getStatus())){
        // set task status to Active
        taskOnStep.changeStatusPersistAndPropagate(TaskStatus.STATUS_ACTIVE, "", new HashSet<Task>(), loggedOperator, new ArrayList<Pair<String, String[]>>());
          for (SomethingHappened sh : taskOnStep.happenings) {
             sh.store();
          }
      }

      // change dates
      Hashtable<Task, TaskScheduleCandidate> taskCandidates = new Hashtable<Task, TaskScheduleCandidate>();

      StringBuffer log = new StringBuffer();
      Date start= new Date();
      CompanyCalendar cc= new CompanyCalendar(start);
      start=cc.setAndGetTimeToDayStart();
      cc.addWorkingDays(taskOnStep.getDuration()-1);
      Date end=cc.getTime();
      boolean invalidClientEntries = !Task.analyzeScheduleChangesRun(start, taskOnStep.getDuration(), end, taskOnStep, log, loggedOperator, taskCandidates);

      if (!invalidClientEntries) {
        for (TaskScheduleCandidate tsc : taskCandidates.values()) {
          tsc.taskScheduleHistory = tsc.task.changeSchedule(tsc.start, tsc.duration, tsc.end, "Change from process", loggedOperator);
        }
      }

      Hit.getInstanceAndStore(taskOnStep, loggedOperator, .3);

    } catch (Throwable t) {
      throw new PlatformRuntimeException(t);
    }

  }
}


