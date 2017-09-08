package com.twproject.messaging;

import com.twproject.resource.Resource;
import com.twproject.task.*;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.messaging.SomethingHappened;
import org.jblooming.oql.QueryHelper;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.scheduler.ExecutableSupport;
import org.jblooming.scheduler.JobLogData;
import org.jblooming.scheduler.Parameter;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;

import java.util.Date;
import java.util.List;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Apr 16, 2007
 * Time: 2:37:36 PM
 */
public class UnassignedIssueNotifier extends ExecutableSupport {

  @Parameter("72")
  public int lag_in_hours = 72;
  @Parameter("")
  public TaskType taskType;

  public JobLogData run(JobLogData jobLogData) throws Exception {


    PersistenceContext pc = null;

    try {

      if(taskType!=null && lag_in_hours>=0){

      pc = PersistenceContext.getDefaultPersistenceContext();

      CompanyCalendar now = new CompanyCalendar(new Date());
      now.add(CompanyCalendar.HOUR, -lag_in_hours);

      String hql = "select task from " + Task.class.getName() + " as task ";
      QueryHelper qhelp = new QueryHelper(hql);
      qhelp.addOQLClause("task.type.id=:typex", "typex", taskType.getId());
      qhelp.addOQLClause("task.status =:statusx", "statusx", TaskStatus.STATUS_ACTIVE);


      List<Task> taskByType = qhelp.toHql().list();

      for (Task t : taskByType) {

        hql = "select issue from " + Issue.class.getName() + " as issue where issue.assignedTo = null  ";
        qhelp = new QueryHelper(hql);
        List<Issue> unassignedIssues = qhelp.toHql().list();

//        Resource pm = null;
//        String customerRoleName = ApplicationState.getApplicationSetting("DEFAULT_PROJECT_MANAGER_ROLE_NAME", "Project Manager");
//        for (Assignment ass : t.getAssignments()) {
//          if (customerRoleName.equalsIgnoreCase(ass.getRole().getName())) {
//            pm = ass.getResource();
//            break;
//          }
//        }

        for (Issue i : unassignedIssues) {
          if (i.getStatus().isBehavesAsOpen() && i.getCreationDate().before(now.getTime())) {
            createEventIssueUnissigned(i);
          }
        }

      }

      pc.commitAndClose();
      }

    } catch (Throwable e) {

      Tracer.platformLogger.error(getClass().getName() + " error", e);
      Tracer.emailLogger.error(getClass().getName() + " error", e);
      if (pc != null) {
        pc.rollbackAndClose();
      }

    } finally {


    }

    return jobLogData;
  }

  public static void createEventIssueUnissigned(Issue issue) throws StoreException {
    if (issue.getTask() != null) {

      ButtonLink edit = IssueBricks.getPopoupLinkToEditor(issue.getId());
      edit.label = "I#" + issue.getMnemonicCode() + "#";//JSP.limWr(issue.getDisplayName(), 30);

      String language = ApplicationState.SYSTEM_LOCALE.getLanguage();

      SomethingHappened changeEvent = new SomethingHappened();
      changeEvent.setIdAsNew();
      changeEvent.setEventType(Task.Event.TASK_ISSUE_ADDED.toString());
      changeEvent.getMessageParams().put("SUBJECT", JSP.limWr(issue.getTask().getDisplayName(), 30) + " - I#" + issue.getId() + "#");
      changeEvent.getMessageParams().put("SUBJECT_REPLACEMENT", "ISSUE_UNISSIGNED_SBJ_TEMPLATE");
      changeEvent.setMessageTemplate("ISSUE_UNISSIGNED_MESSAGE_TEMPLATE");
      changeEvent.getMessageParams().put("issue", JSP.limWr(issue.getDisplayName(), 1000));
      changeEvent.getMessageParams().put("task", issue.getTask().getDisplayName());
      changeEvent.getMessageParams().put("delay", DateUtilities.dateToRelative(issue.getCreationDate()));

      changeEvent.setLink(edit.toPlainLink());
      changeEvent.setIdentifiable(issue.getTask());
      changeEvent.store();

    }

  }

}


