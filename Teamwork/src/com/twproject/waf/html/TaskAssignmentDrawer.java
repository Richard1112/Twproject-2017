package com.twproject.waf.html;

import com.twproject.task.Assignment;
import com.twproject.task.Task;
import org.jblooming.waf.html.core.JspHelper;

import javax.servlet.jsp.PageContext;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class TaskAssignmentDrawer extends JspHelper {
  public boolean recurseOnChildren = true;
  public boolean showWorload=false;
  public Task task;
  public Assignment currentAssignment;


  public Draw_Modality modality=Draw_Modality.DRAW_TASK;
  private boolean inited=false;

  public static enum Draw_Modality {
    DRAW_TASK, DRAW_RESOURCE;
  }

  public TaskAssignmentDrawer(Task task) {
    super();
    this.urlToInclude = "/applications/teamwork/task/assignment/partAssignmentDrawer.jsp";
    this.task = task;
  }

   public void init(PageContext pageContext)  {
     if (!inited){
       inited=true;
       pageContext.getRequest().setAttribute(ACTION, "INIT");
       super.toHtml(pageContext);
     }

   }
   public void drawTask(PageContext pageContext)  {
     pageContext.getRequest().setAttribute(ACTION, "TASKPART");
     super.toHtml(pageContext);
  }

  public void drawAssignment(Assignment assignment, PageContext pageContext) {
      currentAssignment = assignment;
      pageContext.getRequest().setAttribute(ACTION, "ASSIGPART");
      super.toHtml(pageContext);
  }


  /**
   * @deprecated
   */
  public void toHtml(PageContext pageContext) {
    throw new RuntimeException("Call task and assig");
  }


}
