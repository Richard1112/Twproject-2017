package com.twproject.waf.html;

import com.twproject.security.TeamworkPermissions;
import com.twproject.task.Task;
import com.twproject.utilities.TeamworkComparators;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.display.PathToObject;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageSeed;

public class TaskHeaderBar extends JspHelper {


  public PathToObject pathToObject;
  public Task task;

  public TaskHeaderBar(Task task) {
    super();
    this.task=task;
    this.urlToInclude = "/applications/teamwork/task/part/partTaskHeadBar.jsp";

    this.pathToObject = new PathToObject(task);
    this.pathToObject.comparator = new TeamworkComparators.TaskManualOrderComparator();
    this.pathToObject.canClick = TeamworkPermissions.task_canRead;

    PageSeed back = new PageSeed(ApplicationState.contextPath+"/applications/teamwork/task/taskList.jsp");
    this.pathToObject.rootDestination =new ButtonLink(I18n.get("TASK_LIST")+" /", back);

  }

}