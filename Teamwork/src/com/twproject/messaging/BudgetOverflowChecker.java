package com.twproject.messaging;

import com.twproject.task.Task;
import com.twproject.worklog.Worklog;
import org.jblooming.messaging.SomethingHappened;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.scheduler.ExecutableSupport;
import org.jblooming.scheduler.JobLogData;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.view.PageSeed;

import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.HashSet;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class BudgetOverflowChecker extends ExecutableSupport {

  public JobLogData run(JobLogData jobLogData) throws Exception {
    PersistenceContext pc = null;


    Set<Task> taskSeeds = new HashSet<Task>();

    Date examineAfter = new Date(this.secondLastExecutionTime);


    //if someone add a budget it will not be detected until a wl or a cost will change


    // which task are to be examinated? those where there is a worklog inserted after second last execution time
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();
      String hql = "select distinct wl.assig.task from " + Worklog.class.getName() + " as wl where wl.inserted>=:after";
      OqlQuery query = new OqlQuery(hql);
      query.getQuery().setTimestamp("after", examineAfter);
      List<Task> lts = query.list();
      for (Task task : lts) {
        taskSeeds.add(task);
      }

      // plus task where there are cost just modified/inseted
      hql = "select task from " + Task.class.getName() + " as task join task.costs as cost where cost.lastModified>=:after";
      query = new OqlQuery(hql);
      query.getQuery().setTimestamp("after", examineAfter);
      lts = query.list();
      for (Task task : lts) {
        taskSeeds.add(task);
      }


      // put all parents in the set in order to propagate check
      Set<Task> tasks = new HashSet<Task>();
      for (Task task : taskSeeds) {
        tasks.addAll(task.getAncestors());
      }


      //loop for all candidate task
      for (Task task : tasks) {
        double budget = task.getForecasted();
        if (budget > 0) {
          double cost = task.getTotalCostsDone();
          if (cost > budget) {
            // generate the overflow event
            storeEvent(task, cost, budget);

          }
        }
      }


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

  private void storeEvent(Task task, double cost, double budget) throws StoreException {
    SomethingHappened change = new SomethingHappened();
    change.setIdAsNew();
    change.setIdentifiable(task);
    change.setEventType(Task.Event.TASK_BUDGET_OVERFLOW + "");
    change.setMessageTemplate(Task.Event.TASK_BUDGET_OVERFLOW + "_MESSAGE_TEMPLATE");
    change.getMessageParams().put("SUBJECT",JSP.limWr(task.getDisplayName(),30));    
    change.getMessageParams().put("task", task.getDisplayName());
    change.getMessageParams().put("cost", JSP.w(cost));
    change.getMessageParams().put("budget", JSP.w(budget));

    PageSeed ps = new PageSeed(ApplicationState.serverURL + "/applications/teamwork/task/taskOverview.jsp");
    ps.setCommand(Commands.EDIT);
    ps.setMainObjectId(task.getId());
    ButtonLink edit = new ButtonLink(ps);
    edit.label = task.getDisplayName();
    change.setLink(edit.toPlainLink());
    change.store();
  }
}