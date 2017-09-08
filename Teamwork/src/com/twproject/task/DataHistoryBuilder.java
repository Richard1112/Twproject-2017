package com.twproject.task;

import com.twproject.resource.Person;
import org.jblooming.agenda.Period;
import org.jblooming.messaging.SomethingHappened;
import org.jblooming.ontology.Pair;
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

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class DataHistoryBuilder extends ExecutableSupport {

  public JobLogData run(JobLogData jobLogData) throws Exception {
    PersistenceContext pc = null;
    PersistenceContext pc2 =null;

    long lastRun=this.secondLastExecutionTime;

    // TaskDataHistory
    try {
      pc = PersistenceContext.getDefaultPersistenceContext();
      String hql = "select task from " + Task.class.getName() + " as task where task.lastModified>=:after";
      OqlQuery query = new OqlQuery(hql);
      query.getQuery().setTimestamp("after", new Date(lastRun));
      List<Task> lts = query.list();

      pc2= new PersistenceContext();
      int i=0;
      for (Task task : lts) {
        Period schedule = task.getSchedule();
        if (schedule==null)
          continue;

        TaskDataHistory tdh=task.getCurrentData();

        tdh.store(pc2);

        // si committa ogni x
        if(i++%200==0){
          pc2.commitAndClose();
          pc2= new PersistenceContext();
        }
      }
      pc2.commitAndClose();
      pc2= new PersistenceContext();

    // AssignmentDataHistory
      hql = "select ass from " + Assignment.class.getName() + " as ass where ass.lastModified>=:after";
      query = new OqlQuery(hql);
      query.getQuery().setTimestamp("after", new Date(lastRun));
      List<Assignment> lass = query.list();

      pc2= new PersistenceContext();
      i=0;
      for (Assignment ass : lass) {

        AssignmentDataHistory adh= new AssignmentDataHistory();
        adh.setCreatedOn(ass.getLastModified());  // si mette la data di ultima modifica del task per essere più precisi
        adh.setAssignmentId(ass.getId() + "");

        adh.setWorklogDone(ass.getWorklogDone());
        adh.setEstimatedWorklog(ass.getEstimatedWorklog());
        adh.setHourlyCost(ass.getHourlyCost());
        adh.setBudget(ass.getBudget());
        adh.setCostDone(ass.getCostDone(false));
        adh.setCostEstimated(ass.getCostEstimated(false));

        adh.store(pc2);

        // si committa ogni x
        if(i++%200==0){
          pc2.commitAndClose();
          pc2= new PersistenceContext();
        }
      }
      pc2.commitAndClose();
      pc2= new PersistenceContext();


      pc.commitAndClose();

      jobLogData.notes = jobLogData.notes + getClass().getName() + " executed on " + DateUtilities.dateAndHourToString(new Date());
    } catch (Throwable e) {
      Tracer.platformLogger.error(getClass().getName() + " error", e);
      jobLogData.successfull = false;

      if (pc != null)
        pc.rollbackAndClose();

      if (pc2!=null)
        pc2.rollbackAndClose();
    }

    return jobLogData;
  }

}