package com.twproject.task;

import org.jblooming.ApplicationException;
import org.jblooming.ontology.LoggableIdentifiableSupport;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */
public class TaskDependency extends LoggableIdentifiableSupport {


  private Task task;

  /**
   * this must be a brother of task
   */
  private Task depends;
  private int lag;


  public Task getTask() {
    return task;
  }

  public void setTask(Task task) {
    this.task = task;
  }


   public Task getDepends() {
    return depends;
  }

  private void setDepends(Task depends) {
    this.depends = depends;
  }

  public void setDependency(Task depends) throws ApplicationException {
    Task parent = task.getParent();
    //if (parent!=null && !parent.equals(depends.getParent()))
    //  throw new ApplicationException("Dependencies are defined only between brothers");
    this.depends = depends;
  }


  public int getLag() {
    return lag;
  }

  public void setLag(int lag) {
    this.lag = lag;
  }

}
