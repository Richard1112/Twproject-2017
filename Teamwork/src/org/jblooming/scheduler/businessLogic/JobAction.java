package org.jblooming.scheduler.businessLogic;

import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.agenda.Period;
import org.jblooming.agenda.ScheduleSupport;
import org.jblooming.ontology.businessLogic.DeleteHelper;
import org.jblooming.oql.QueryHelper;
import org.jblooming.page.HibernatePage;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.RemoveException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.scheduler.Executable;
import org.jblooming.scheduler.Job;
import org.jblooming.scheduler.Parameter;
import org.jblooming.scheduler.Scheduler;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.waf.ActionUtilities;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.display.DataTable;
import org.jblooming.waf.html.display.Paginator;
import org.jblooming.waf.html.input.ScheduleComposer;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageState;

import java.lang.reflect.Field;
import java.text.ParseException;
import java.util.Date;
import java.util.List;

public class JobAction {

  public void cmdSave(PageState pageState) throws PersistenceException, ActionException,  RemoveException {

//______________________________________________________________________________________________________________________________________________________________________


// Job


//______________________________________________________________________________________________________________________________________________________________________

    Job job = null;
    if (PersistenceHome.NEW_EMPTY_ID.equals(pageState.getMainObjectId())) {
      pageState.initializeEntries("table");
      job = new Job();
      job.setIdAsNew();
    } else {
      job = (Job) PersistenceHome.findByPrimaryKey(Job.class, pageState.getMainObjectId());
    }
    pageState.setMainObject(job);

    try {
      job.setName(pageState.getEntryAndSetRequired("NAME").stringValue());
    } catch (ActionException e) {
    }

    job.setDescription(pageState.getEntry("DESCRIPTION").stringValue());

    try {
      job.setExecutable(pageState.getEntryAndSetRequired("LAUNCHER_CLASS").stringValue());
    } catch (ActionException e) {
    }
    try {
      job.setEstimatedDuration(pageState.getEntry("ESTIMATED_DURATION").intValue());
    } catch (ParseException e) {
    }

    ActionUtilities.setString(pageState.getEntry("DESCRIPTION"),job,"description");
    ActionUtilities.setLong(pageState.getEntry("timeoutTime"),job,"timeoutTime");
    ActionUtilities.setBoolean(pageState.getEntry("onErrorSuspendScheduling"),job,"onErrorSuspendScheduling");
    ActionUtilities.setBoolean(pageState.getEntry("onErrorRetryNow"),job,"onErrorRetryNow");
    ActionUtilities.setBoolean(pageState.getEntry("enabled"),job,"enabled");

    ScheduleSupport schedule = ScheduleComposer.getSchedule("schedule", pageState);


    ClientEntry clazz = pageState.getEntry("LAUNCHER_CLASS");

    if (clazz.stringValueNullIfEmpty() != null) {

      try {
        Class theClass = Class.forName(clazz.stringValue());
        if (ReflectionUtilities.getInheritedClasses(theClass).contains(Executable.class)) {
          List<Field>  flds = ReflectionUtilities.getDeclaredInheritedParameterFields(theClass, Parameter.class);
            for (Field field : flds) {
              ClientEntry entry = pageState.getEntry(field.getName());
              if (entry.stringValue() != null) {
                String value = entry.stringValue();
                if (value != null && value.trim().length() > 0) {
                  job.getParameters().put(field.getName(), value);
                } else  {
                  job.getParameters().put(field.getName(), "");

                  //job.getParameters().remove(field.getName());
                }
              }
            }

        }
      } catch (ClassNotFoundException e) {
      }
    }


    if (!pageState.validEntries())
      throw new ActionException();

    ScheduleSupport oldSchedule = job.getSchedule();
    if (oldSchedule !=null){
      job.setSchedule(null);
      oldSchedule.remove();
    }
    schedule.store();
    job.setSchedule(schedule);
    job.store();


    /*Scheduler instance = Scheduler.getInstance();
    if (instance != null)
      instance.addJob(job);*/
    //17 Mar 2008: must commit otherwise reread in Scheduler goes MAD
    PersistenceContext.get(Job.class).checkPoint();
    Scheduler instance = Scheduler.getInstance();
    if (instance != null) {
      instance.stop();
      Scheduler.instantiate(instance.tick, pageState.getLoggedOperator().getDisplayName());
    }
  }

  public void cmdAdd(PageState pageState) {
    pageState.initializeEntries("table");
    Job job = new Job();
    job.setIdAsNew();
    pageState.setMainObject(job);
    //make(pageState, job);
    Period p = new Period(new Date(),new Date(System.currentTimeMillis()+CompanyCalendar.MILLIS_IN_HOUR));
    ScheduleComposer.make("schedule", p, pageState);
  }

  public void cmdEdit(PageState pageState) throws FindByPrimaryKeyException {
    Job job = (Job) PersistenceHome.findByPrimaryKey(Job.class, pageState.getMainObjectId());
    pageState.setMainObject(job);
    make(pageState, job);

    if (job.getSchedule() != null)
      ScheduleComposer.make("schedule", job.getSchedule(), pageState);
  }

  private void make(PageState pageState, Job job) {
    pageState.addClientEntry("NAME", job.getName());
    pageState.addClientEntry("DESCRIPTION", job.getDescription());
    pageState.addClientEntry("LAUNCHER_CLASS", job.getExecutable());

    pageState.addClientEntry("ESTIMATED_DURATION", job.getEstimatedDuration());
    pageState.addClientEntry("DESCRIPTION", JSP.w(job.getDescription()));
    pageState.addClientEntry("timeoutTime", job.getTimeoutTime() + "");
    pageState.addClientEntry("enabled", job.isEnabled()?Fields.TRUE:Fields.FALSE);
    pageState.addClientEntry("onErrorSuspendScheduling", job.isOnErrorSuspendScheduling()?Fields.TRUE:Fields.FALSE);
    pageState.addClientEntry("onErrorRetryNow", job.isOnErrorRetryNow()?Fields.TRUE:Fields.FALSE);

    if (job.getParameters() != null && job.getParameters().size() > 0) {
      for (String key : job.getParameters().keySet()) {
        String value = job.getParameters().get(key);
        pageState.addClientEntry(key, value);
      }
    }

  }

  public void cmdFind(PageState pageState) throws PersistenceException {
    String hql = "from " + Job.class.getName() + " as job";
    QueryHelper qhelp = new QueryHelper(hql);
    String filter = pageState.getEntry("JOB_NAME").stringValueNullIfEmpty();
    if (JSP.ex(filter)) {
      qhelp.addQBEClause("job.name", "name", filter, QueryHelper.TYPE_CHAR);
    }
    DataTable.orderAction(qhelp, "LH", pageState, "job.name");
    pageState.setPage(HibernatePage.getHibernatePageInstance(qhelp.toHql().getQuery(),
            Paginator.getWantedPageNumber(pageState),
            Paginator.getWantedPageSize("JOBPS", pageState)));
  }


  public void cmdDelete(PageState pageState) throws PersistenceException {
    pageState.initializeEntries("table");
    Job delenda = (Job) PersistenceHome.findByPrimaryKey(Job.class, pageState.getMainObjectId());
    DeleteHelper.cmdDelete(delenda, pageState);
    Scheduler instance = Scheduler.getInstance();
    if (instance!=null)
      instance.removeJob(delenda);
  }

  public void cmdRunNow(PageState pageState) throws PersistenceException {
    cmdEdit(pageState);
    Job job = (Job) pageState.getMainObject();

    Scheduler instance = Scheduler.getInstance();
    if (instance==null) {
      Scheduler.instantiate(5000, pageState.getLoggedOperator().getDisplayName());
      instance= Scheduler.getInstance();
      instance.run();
    }
    job.resetLastExecutionTime();
    job.store();
    instance.runAJob(job);
  }
}
