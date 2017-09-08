package com.twproject.webService;

import com.twproject.task.Task;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.PlatformRuntimeException;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Jan 7, 2008
 * Time: 4:08:22 PM
 */
public class TeamworkWS {

  public static TaskWS getTaskWSInstance(String taskId) {

    try {
      Task t = (Task) PersistenceHome.findByPrimaryKey(Task.class,taskId);
      TaskWS tws = new TaskWS(t);
      return tws;
    } catch (FindByPrimaryKeyException e) {
      throw new PlatformRuntimeException(e);
    }

  }

  public static String gimmeFive() {
    return "5";
  }

}
