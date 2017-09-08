package com.twproject.messaging;

import com.twproject.task.*;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.messaging.SomethingHappened;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.persistence.exceptions.FindException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.scheduler.ExecutableSupport;
import org.jblooming.scheduler.JobLogData;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageSeed;

import java.util.Date;
import java.util.List;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class ExpiredTaskFinder extends ExecutableSupport {

  public JobLogData run(JobLogData jobLogData) throws Exception {
    PersistenceContext pc = null;

    Date lastRun = new Date(this.secondLastExecutionTime);

    try {
      pc = PersistenceContext.getDefaultPersistenceContext();

      findTasks();

      findIssues();

      pc.commitAndClose();

      jobLogData.notes = jobLogData.notes + getClass().getName() + " executed on " + DateUtilities.dateAndHourToString(new Date());
    } catch (Throwable e) {
      Tracer.platformLogger.error(getClass().getName() + " error", e);
      jobLogData.successfull = false;

      if (pc != null) {
        pc.rollbackAndClose();
      }
    }

    return jobLogData;
  }

  private void findTasks() throws FindException, StoreException {
    CompanyCalendar cc=new CompanyCalendar();

    String hql = "from " + Task.class.getName() + " as task " +
            "where task.schedule.end between :lastMonth and :thisMorning and task.status=:statusActive";

    OqlQuery query = new OqlQuery(hql);
    query.getQuery().setTimestamp("lastMonth", new Date(System.currentTimeMillis() - CompanyCalendar.MILLIS_IN_MONTH));
    query.getQuery().setTimestamp("thisMorning", cc.setAndGetTimeToDayStart());
    query.getQuery().setString("statusActive", TaskStatus.STATUS_ACTIVE);
    List<Task> tasks = query.list();


    //generate events
    for (Task task : tasks) {
      storeTaskEvent(task);

      //se il task ha task dipendenti si notificano anche quelli
      if (JSP.ex(task.getNexts())){
        for (TaskDependency tDep:task.getNexts()){
          Task next = tDep.getTask();
          // se quelli dopo sono sospesi e dovevano già essere aperti si notifica
          if (TaskStatus.STATUS_SUSPENDED.equals(next.getStatus()) && next.getSchedule().getStartDate().before(new Date())){
            storeTaskDependantEvent(next,task);
          }

        }
      }


    }
  }

  private void storeTaskEvent(Task task) throws StoreException {
    SomethingHappened change = new SomethingHappened();
    change.setIdAsNew();
    change.setIdentifiable(task);
    change.setEventType(Task.Event.TASK_EXPIRED+"");
    change.getMessageParams().put("SUBJECT",JSP.limWr(task.getDisplayName(),30));
    change.setMessageTemplate(Task.Event.TASK_EXPIRED+"_MESSAGE_TEMPLATE");

    change.getMessageParams().put("task", task.getDisplayName());
    change.getMessageParams().put("end", JSP.w(task.getSchedule().getEndDate()));

    PageSeed ps = new PageSeed(ApplicationState.serverURL + "/applications/teamwork/task/taskOverview.jsp");
    ps.setCommand(Commands.EDIT);
    ps.setMainObjectId(task.getId());
    ButtonLink edit = new ButtonLink(ps);
    edit.label = task.getDisplayName();
    change.setLink(edit.toPlainLink());
    change.store();

  }

  private void storeTaskDependantEvent(Task task,Task dependsOn) throws StoreException {
    SomethingHappened change = new SomethingHappened();
    change.setIdAsNew();
    change.setIdentifiable(task);
    change.setEventType(Task.Event.TASK_EXPIRED+"");
    change.getMessageParams().put("SUBJECT",JSP.limWr(task.getDisplayName(),30));
    change.setMessageTemplate(Task.Event.TASK_EXPIRED+"_DEPENDANT_MESSAGE_TEMPLATE");

    change.getMessageParams().put("task", task.getDisplayName());
    change.getMessageParams().put("start", JSP.w(task.getSchedule().getStartDate()));
    change.getMessageParams().put("dependsOn", JSP.w(dependsOn.getDisplayName()));

    PageSeed ps = new PageSeed(ApplicationState.serverURL + "/applications/teamwork/task/taskOverview.jsp");
    ps.setCommand(Commands.EDIT);
    ps.setMainObjectId(task.getId());
    ButtonLink edit = new ButtonLink(ps);
    edit.label = task.getDisplayName();
    change.setLink(edit.toPlainLink());
    change.store();

  }



  private void findIssues() throws FindException, StoreException {
    CompanyCalendar cc=new CompanyCalendar();

    String hql = "from " + Issue.class.getName() + " as issue " +
            //"where issue.shouldCloseBy between :lastMonth and :thisMorning and issue.task.status=:statusActive and issue.status.behavesAsOpen=true";
            "where issue.shouldCloseBy between :lastMonth and :thisEvening and issue.task.status=:statusActive and issue.status.behavesAsOpen=true";

    OqlQuery query = new OqlQuery(hql);
    query.getQuery().setTimestamp("lastMonth", new Date(System.currentTimeMillis() - CompanyCalendar.MILLIS_IN_MONTH));
    //query.getQuery().setTimestamp("thisMorning", cc.setAndGetTimeToDayStart());
    query.getQuery().setTimestamp("thisEvening", cc.setAndGetTimeToDayEnd());
    query.getQuery().setString("statusActive", TaskStatus.STATUS_ACTIVE);
    List<Issue> issues = query.list();


    //generate events
    for (Issue issue : issues) {
      storeIssueEvent(issue);
    }

  }

  private void storeIssueEvent(Issue issue) throws StoreException {
    SomethingHappened change = new SomethingHappened();
    change.setIdAsNew();
    change.setIdentifiable(issue.getTask());
    change.setEventType(Task.Event.TASK_EXPIRED+"");
    change.getMessageParams().put("SUBJECT_REPLACEMENT", "EVENT_ISSUE_EXPIRED");
    change.setMessageTemplate("ISSUE_EXPIRED_MESSAGE_TEMPLATE");

    change.getMessageParams().put("issue", JSP.limWr(issue.getDisplayName(),1000));
    change.getMessageParams().put("subject", "I#"+issue.getId()+"#");
    change.getMessageParams().put("issueName", "I#"+issue.getId()+"#");
    change.getMessageParams().put("task", issue.getTask().getDisplayName());
    change.getMessageParams().put("end", JSP.w(issue.getShouldCloseBy()));

    ButtonLink edit = IssueBricks.getPopoupLinkToEditor(issue.getId());
    edit.label = "I#"+issue.getId()+"#";
    change.setLink(edit.toPlainLink());
    change.store();
  }

}