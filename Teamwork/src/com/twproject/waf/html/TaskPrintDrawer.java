package com.twproject.waf.html;

import org.jblooming.waf.html.core.JspHelper;

import com.twproject.task.Task;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class TaskPrintDrawer extends JspHelper {

  public Task task;
  public boolean recurseOnChildren = true;
  public boolean pageBreak = true;
  public long totalEstimatedWorklog;
  public long totalWorklogDone;
  public boolean canSeeCosts;


  public TaskPrintDrawer(Task task) {
    super();
    this.urlToInclude = "/applications/teamwork/task/partTaskEditPrintDraw.jsp";
    this.task = task;
  }

}