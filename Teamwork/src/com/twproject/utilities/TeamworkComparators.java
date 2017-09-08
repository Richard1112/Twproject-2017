package com.twproject.utilities;

import com.twproject.agenda.Event;
import com.twproject.meeting.DiscussionPoint;
import com.twproject.resource.Company;
import com.twproject.resource.Resource;
import com.twproject.task.Assignment;
import com.twproject.task.AssignmentPriority;
import com.twproject.task.Task;
import org.jblooming.anagraphicalData.AnagraphicalData;
import org.jblooming.remoteFile.Document;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.settings.I18n;

import java.util.Collections;
import java.util.Comparator;
import java.util.Date;

/**
 * Created by rbicchierai on 27/05/2015.
 */
public class TeamworkComparators {


  //----------------------- Resource by class and displayName
  public static class ResourceComparator implements Comparator<Resource> {
    public int compare(Resource o1, Resource o2) {
        // questo comparator deve tenere conto che ci possano essere Person e Company e proxate
        return ((o1.getClass().getName().contains("Person")?"A":"B")+o1.getDisplayName()).compareToIgnoreCase((o2.getClass().getName().contains("Person")?"A":"B")+o2.getDisplayName());
    }
  }


  //----------------------- AnagraphicalData by orderBy
  public static class AnagraphicalDataComparator implements Comparator<AnagraphicalData> {
    public int compare(AnagraphicalData d1, AnagraphicalData d2) {
      return d1.getOrderFactor() - d2.getOrderFactor();
    }
  }


  //--------------------- Assignment comparator by priority at a specific date
  public static class AssignmentByPriority implements Comparator<Assignment> {
    long finalWhen = 0;
    boolean thenOrderByCode=false;


    public AssignmentByPriority(Date date) {
      if (I18n.isActive("CUSTOM_FEATURE_ORDER_TASK_BY_CODE"))
        thenOrderByCode=true;
      finalWhen = date.getTime();
    }

    public int compare(Assignment a1, Assignment a2) {
      int result = a2.getPriorityAtTime(finalWhen) - a1.getPriorityAtTime(finalWhen);
      if (result == 0) {
        if (thenOrderByCode)
          result = (JSP.w(a1.getTask().getCode()) + JSP.w(a1.getTask().getName()) + JSP.w(a1.getRole().getCode())).compareToIgnoreCase(JSP.w(a2.getTask().getCode()) + JSP.w(a2.getTask().getName()) + JSP.w(a2.getRole().getCode()));
        else
          result = (JSP.w(a1.getTask().getName()) + JSP.w(a1.getTask().getCode()) + JSP.w(a1.getRole().getCode())).compareToIgnoreCase(JSP.w(a2.getTask().getName()) + JSP.w(a2.getTask().getCode()) + JSP.w(a2.getRole().getCode()));

      }
      return result;
    }
  }



  //--------------------------- Assignment by  task.end
  public static class AssignmentComparatorByTaskEnd implements Comparator<Assignment> {
    public int compare(Assignment o1, Assignment o2) {
      Long v1=o1.getTask().getSchedule()==null?Long.MIN_VALUE:o1.getTask().getSchedule().getStartDate().getTime();
      Long v2=o2.getTask().getSchedule()==null?Long.MIN_VALUE:o2.getTask().getSchedule().getStartDate().getTime();
      return v2.compareTo(v1);
    }
  }


  //--------------------------- Assignment by  task name, task code, role code
  public static class AssignmentComparatorByTask implements Comparator<Assignment> {
    public int compare(Assignment o1, Assignment o2) {
      return (JSP.w(o1.getTask().getName()) + JSP.w(o1.getTask().getCode()) + JSP.w(o1.getRole().getCode())).compareToIgnoreCase(JSP.w(o2.getTask().getName()) + JSP.w(o2.getTask().getCode()) + JSP.w(o2.getRole().getCode()));
    }
  }

  //--------------------------- Assignment by  task code, task name,  role code
  public static class AssignmentComparatorByTaskCode implements Comparator<Assignment> {
    public int compare(Assignment o1, Assignment o2) {
      return (JSP.w(o1.getTask().getCode()) + JSP.w(o1.getTask().getName()) + JSP.w(o1.getRole().getCode())).compareToIgnoreCase(JSP.w(o2.getTask().getCode()) + JSP.w(o2.getTask().getName()) + JSP.w(o2.getRole().getCode()));
    }
  }


  //--------------------------- Assignment by  resource name, role code
  public static class AssignmentComparatorByResource implements Comparator<Assignment> {
    public int compare(Assignment o1, Assignment o2) {
      return (JSP.w(o1.getResource().getName()) + JSP.w(o1.getRole().getCode())).compareToIgnoreCase(JSP.w(o2.getResource().getName()) + JSP.w(o2.getRole().getCode()));
    }
  }

  //--------------------------- AssignmentPriority by  cutPoint
  public static class AssignmentPriorityComparator implements Comparator<AssignmentPriority> {
    public int compare(AssignmentPriority rd1, AssignmentPriority rd2) {
      return (int) (rd2.getCutPoint() - rd1.getCutPoint());
    }
  }



  //--------------------------------- Task by Name, Code
  public static class TaskNameCodeComparator implements Comparator<Task> {
    boolean orderTaskByCode=false;

    public TaskNameCodeComparator() {
      if (I18n.isActive("CUSTOM_FEATURE_ORDER_TASK_BY_CODE"))
        orderTaskByCode=true;
    }

    public int compare(Task t1, Task t2) {
      if (orderTaskByCode)
        return (JSP.w(t1.getCode()+ t1.getName())).compareToIgnoreCase(JSP.w(t2.getCode()+ t2.getName()));
      else
        return (t1.getName() + JSP.w(t1.getCode())).compareToIgnoreCase(t2.getName() + JSP.w(t2.getCode()));
    }

  }

  //--------------------------------- Task by Manual Order, name, code
  public static class TaskManualOrderComparator implements Comparator<Task> {
    public int compare(Task t1, Task t2) {
      int ret=(t1.getOrderFactor()==null?"x":t1.getOrderFactor()).compareTo(t2.getOrderFactor()==null?"x":t2.getOrderFactor());
      if (ret==0)
        ret=new TaskNameCodeComparator().compare(t1, t2);
      return ret;
    }
  }

  /**
   * use Task by start date, name, code
   */
  public static class TaskByDateComparator implements Comparator<Task> {

    public int compare(Task t1, Task t2) {
      int result = 0;
      result = new Long(t1.getSchedule()==null?Long.MIN_VALUE:t1.getSchedule().getStartDate().getTime()).compareTo(new Long(t2.getSchedule()==null?Long.MIN_VALUE:t2.getSchedule().getStartDate().getTime()));
      if (result == 0)
        result = new TaskNameCodeComparator().compare(t1, t2);
      return result;
    }
  }







  // ---------------------- compare Events by Period
  public static class EventPeriodComparator implements Comparator<Event> {
    public int compare(Event p1, Event p2) {
      int retVal = 0;
      if (p1.getSchedule().getValidityStartTime() > p2.getSchedule().getValidityStartTime())
        retVal = 1;
      if (p2.getSchedule().getValidityStartTime() > p1.getSchedule().getValidityStartTime())
        retVal = -1;
      return retVal;
    }
  }


  // ----------------------------------- Document by name and version
  public static Comparator<Document> documentNameVersionComparator = new Comparator<Document>() {
    public int compare(Document o1, Document o2) {
      return (o1.getName().toUpperCase() + o1.getVersion()).compareTo(o2.getName().toUpperCase() + o2.getVersion());
    }
  };


  // ---------------------  DiscussionPoin by orderBy
  public static class DiscussionPointComparator implements Comparator<DiscussionPoint> {
    public int compare(DiscussionPoint o1, DiscussionPoint o2) {
      return o1.getOrderBy() - o2.getOrderBy();
    }
  }



}



