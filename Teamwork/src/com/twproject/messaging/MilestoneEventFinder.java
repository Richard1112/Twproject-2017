package com.twproject.messaging;

import com.twproject.task.Task;
import org.jblooming.agenda.CompanyCalendar;
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

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class MilestoneEventFinder extends ExecutableSupport {

  public JobLogData run(JobLogData jobLogData) throws Exception {
    PersistenceContext pc = null;

    CompanyCalendar cc = new CompanyCalendar();
    cc.setAndGetTimeToDayStart();
    long now = cc.getTime().getTime();

    try {

      pc = PersistenceContext.getDefaultPersistenceContext();

      String hql = "from " + Task.class.getName() + " as task " +
              "where task.schedule is not null and " +
              "(task.startIsMilestone=:truth1 and task.schedule.start>:now1 and  task.schedule.start<:nowPlusDelta1)" +
              " or " +
              "(task.endIsMilestone=:truth2 and task.schedule.end>:now2 and task.schedule.end<:nowPlusDelta2)";

      OqlQuery query = new OqlQuery(hql);
      query.getQuery().setBoolean("truth1", Boolean.TRUE);
      query.getQuery().setBoolean("truth2", Boolean.TRUE);
      query.getQuery().setTimestamp("now1", cc.getTime());
      query.getQuery().setTimestamp("now2", cc.getTime());

      cc.add(CompanyCalendar.DAY_OF_YEAR, Integer.parseInt(ApplicationState.getApplicationSetting("MILESTONE_ALERT_DELTA", "2")));

      long nowPlusDelta = cc.getTime().getTime();

      query.getQuery().setTimestamp("nowPlusDelta1", cc.getTime());
      query.getQuery().setTimestamp("nowPlusDelta2", cc.getTime());
      List<Task> tasks = query.list();

      //generate events
      for (Task task : tasks) {
        if (task.isActive()) {
          Date startDate = task.getSchedule().getStartDate();
          if (task.isStartIsMilestone() && startDate != null && startDate.getTime() > now && startDate.getTime() < nowPlusDelta) {
            storeEvent(task, startDate);
          }
          Date endDate = task.getSchedule().getEndDate();
          if (task.isEndIsMilestone() && endDate != null && endDate.getTime() > now && endDate.getTime() < nowPlusDelta) {
            storeEvent(task, endDate);
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

  private void storeEvent(Task task, Date startDate) throws StoreException {
    SomethingHappened change = new SomethingHappened();
    change.setIdAsNew();
    change.setIdentifiable(task);
    change.setEventType(Task.Event.TASK_MILESTONE_CLOSER+"");
    change.setMessageTemplate(Task.Event.TASK_MILESTONE_CLOSER+"_MESSAGE_TEMPLATE");
    change.getMessageParams().put("SUBJECT",JSP.limWr(task.getDisplayName(),30));
    change.getMessageParams().put("task", task.getDisplayName());
    change.getMessageParams().put("milestone", JSP.w(startDate));

    PageSeed ps = new PageSeed(ApplicationState.serverURL + "/applications/teamwork/task/taskOverview.jsp");
    ps.setCommand(Commands.EDIT);
    ps.setMainObjectId(task.getId());
    ButtonLink edit = new ButtonLink(ps);
    edit.label = task.getDisplayName();
    change.setLink(edit.toPlainLink());
    change.store();
  }
}
