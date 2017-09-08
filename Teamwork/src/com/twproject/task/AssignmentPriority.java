package com.twproject.task;

import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.waf.settings.I18n;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class AssignmentPriority extends IdentifiableSupport {

  private Assignment assignment;
  private long cutPoint;
  private int priority=PRIORITY_LOW;

  public static final int PRIORITY_LOW = 30;
  public static final int PRIORITY_MEDIUM = 50;
  public static final int PRIORITY_HIGH = 70;


  public Assignment getAssignment() {
    return assignment;
  }

  public void setAssignment(Assignment assignment) {
    this.assignment = assignment;
  }

  public long getCutPoint() {
    return cutPoint;
  }

  public void setCutPoint(long cutPoint) {
    this.cutPoint = cutPoint;
  }

  public int getPriority() {
    return priority;
  }

  public void setPriority(int priority) {
    this.priority = priority;
  }


  public static String getPriorityColor(int priority) {
    if (priority == PRIORITY_MEDIUM)
      return "#FFAE00";
    else if (priority == PRIORITY_HIGH)
      return "#e50000";
    else
      return "#a0a0a0";
  }

  public static String getPriorityDescription(int priority) {
    if (priority == PRIORITY_MEDIUM)
      return I18n.get("PRIORITY_MEDIUM_SHORT");
    else if (priority == PRIORITY_HIGH)
      return I18n.get("PRIORITY_HIGH_SHORT");
    else
      return I18n.get("PRIORITY_LOW_SHORT");
  }

}
