package com.twproject.webService;

import com.twproject.task.Task;

import java.util.List;
import java.util.ArrayList;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Jan 7, 2008
 * Time: 3:49:41 PM
 */
public class TaskWS {

  private Task target;
  String name;

  public TaskWS() {
  }

  protected TaskWS(Task t) {
    this.target = t;
    this.name = t.getName();
  }

  public String getName() {
    return name;
  }

  public double getProgress() {
    return target.getProgress();
  }

  public TaskWS getParent() {
    if (target.getParent() != null) {
      TaskWS tws = new TaskWS(target.getParent());
      return tws;
    } else
      return null;
  }

  public List<TaskWS> getChildren() {
    List<Task> chs = target.getChildrenSorted();
    List<TaskWS> chWS = new ArrayList();
    for (Task ch : chs) {
      chWS.add(TeamworkWS.getTaskWSInstance(ch.getId()+""));
    }
    return chWS;
  }


}

