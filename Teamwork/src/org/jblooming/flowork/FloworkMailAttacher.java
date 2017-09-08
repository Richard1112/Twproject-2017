package org.jblooming.flowork;

import org.jbpm.taskmgmt.exe.TaskInstance;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.ontology.LoggableIdentifiableSupport;

public class FloworkMailAttacher extends LoggableIdentifiableSupport {

  TaskInstance taskInstance;
  String mailText;

  public FloworkMailAttacher() {
  }


  public FloworkMailAttacher(TaskInstance taskInstance, String mailText) {
    this.taskInstance = taskInstance;
    this.mailText = mailText;
  }

  public TaskInstance getTaskInstance() {
    return taskInstance;
  }

  public void setTaskInstance(TaskInstance taskInstance) {
    this.taskInstance = taskInstance;
  }

  public String getMailText() {
    return mailText;
  }

  public void setMailText(String mailText) {
    this.mailText = mailText;
  }

  public static FloworkMailAttacher getInstanceFromTask(TaskInstance taskInstance) {
    return (FloworkMailAttacher) PersistenceHome.findUniqueNullIfEmpty(FloworkMailAttacher.class,"taskInstance",taskInstance);
  }
}
