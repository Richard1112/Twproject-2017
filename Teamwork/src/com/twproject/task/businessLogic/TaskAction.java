package com.twproject.task.businessLogic;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.hibernate.Query;
import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.agenda.Period;
import org.jblooming.designer.DesignerField;
import org.jblooming.flowork.JBPMQueryHelper;
import org.jblooming.flowork.PlatformJbpmSessionFactory;
import org.jblooming.messaging.Listener;
import org.jblooming.messaging.SomethingHappened;
import org.jblooming.ontology.Pair;
import org.jblooming.ontology.PerformantNodeSupport;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.ontology.SerializedMap;
import org.jblooming.ontology.businessLogic.DeleteHelper;
import org.jblooming.oql.OqlQuery;
import org.jblooming.oql.QueryHelper;
import org.jblooming.page.HibernatePage;
import org.jblooming.page.ListPage;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.security.Area;
import org.jblooming.security.SecurityException;
import org.jblooming.uidgen.CounterHome;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.HashTable;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.NumberUtilities;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.ActionSupport;
import org.jblooming.waf.ActionUtilities;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.display.DataTable;
import org.jblooming.waf.html.display.Paginator;
import org.jblooming.waf.html.input.SmartCombo;
import org.jblooming.waf.html.state.Form;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.state.PersistentSearch;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.RestState;
import org.jbpm.JbpmContext;
import org.jbpm.db.GraphSession;
import org.jbpm.graph.def.Node;
import org.jbpm.graph.def.ProcessDefinition;
import org.jbpm.graph.def.Transition;
import org.jbpm.graph.exe.ProcessInstance;
import org.jbpm.graph.node.Join;
import org.jbpm.graph.node.TaskNode;
import org.jbpm.taskmgmt.def.Swimlane;
import org.jbpm.taskmgmt.exe.TaskInstance;

import com.twproject.document.TeamworkDocument;
import com.twproject.operator.TeamworkOperator;
import com.twproject.rank.EntityGroupRank;
import com.twproject.rank.Hit;
import com.twproject.rank.RankUtilities;
import com.twproject.resource.Person;
import com.twproject.resource.Resource;
import com.twproject.security.RoleTeamwork;
import com.twproject.security.TeamworkPermissions;
import com.twproject.setup.WizardSupport;
import com.twproject.task.Assignment;
import com.twproject.task.Issue;
import com.twproject.task.Task;
import com.twproject.task.TaskBricks;
import com.twproject.task.TaskDependency;
import com.twproject.task.TaskScheduleCandidate;
import com.twproject.task.TaskScheduleHistory;
import com.twproject.task.TaskStatus;
import com.twproject.task.TaskType;
import com.twproject.task.financial.Cost;
import com.twproject.task.process.TaskProcess;

import net.sf.json.JSONObject;


public class TaskAction extends ActionSupport {


  public TeamworkOperator logged;
  public Task task;

  public TaskAction(RestState pageState) {
    super(pageState);
    this.logged = (TeamworkOperator) pageState.getLoggedOperator();
  }

  public void cmdAdd() throws PersistenceException, SecurityException {

    task = new Task();

    task.setIdAsNew();
    restState.setMainObject(task);

    String parId = restState.getEntry(Fields.PARENT_ID).stringValueNullIfEmpty();
    if (parId != null) {

      Task parent = (Task) PersistenceHome.findByPrimaryKey(Task.class, parId);
      task.setParentAndStore(parent);

      task.bricks.suggestCodeFromParent();

      task.setStatus(parent.getStatus());
      task.setType(parent.getType());

      //determine default area
      task.setArea(parent.getArea());

      //put default schedule
      if (parent.getSchedule() != null) {
        Date startDate = parent.getSchedule().getStartDate();
        task.setSchedule(new Period(startDate, startDate));
        task.setDuration(1);
      }

      task.setProgressByWorklog(parent.isProgressByWorklog());

      //test permissions must be done after having determined parent and area
      task.testPermission(logged, TeamworkPermissions.task_canCreate);

    } else {
      logged.testPermission(TeamworkPermissions.project_canCreate);
      if (JSP.ex(restState.getEntry("TASK_CODE")))
        task.setCode(restState.getEntry("TASK_CODE").stringValueNullIfEmpty());
      task.setStatus(TaskStatus.STATUS_ACTIVE);
      task.setArea(logged.getDefaultAreaForPermission(TeamworkPermissions.project_canCreate));
      Date startDate = new Date();
      task.setSchedule(new Period(startDate, startDate));
      task.setDuration(1);
    }


    task.setOwner(logged);

    if (task.getParent() != null) {
      restState.addClientEntry(Fields.PARENT_ID, task.getParent().getId());
      //restState.addClientEntry("COLOR", task.getParent().getTaskColor());
    }
    restState.addClientEntry("TASK_CODE", task.getCode());
    restState.addClientEntry("STATUS", task.getStatus());

    if (task.getSchedule() != null) {
      restState.addClientEntry("START", task.getSchedule().getStartDate());
      restState.addClientEntry("END", task.getSchedule().getEndDate());
    }

  }

  public void cmdEdit() throws PersistenceException, SecurityException {
    editNoMake();
    SecurityException.riseExceptionIfNoPermission(task.hasPermissionFor(logged, TeamworkPermissions.task_canRead), TeamworkPermissions.task_canRead, task);
    make(task);
  }

  public Task editNoMake() throws PersistenceException {
    if (!JSP.ex(restState.mainObjectId) && JSP.ex(restState.getEntry("TASK_ID")))
      task = Task.load(restState.getEntry("TASK_ID").stringValueNullIfEmpty());
    else
      task = Task.load(restState.mainObjectId);

    restState.setMainObject(task);

    if(JSP.ex(task)){
    Hit.getInstanceAndStore(task, logged, .1);
    }
    return task;
  }

  public void cmdGuess() throws PersistenceException, ActionException {
    task = null;
    //first eventually try code
    try {
      task = (Task) PersistenceHome.findUnique(Task.class, "code", restState.mainObjectId);
    } catch (PersistenceException p) {
      throw new ActionException("REF_NOT_UNIQUE");
    }

    if (task == null)
      task = Task.load(restState.getMainObjectId());

    if (task != null) {
      restState.mainObjectId = task.getId();
      editNoMake();
      if (!task.hasPermissionFor(logged, TeamworkPermissions.task_canRead))
        throw new ActionException("REF_PERMISSION_LACKING");
      make(task);
    } else {
      throw new ActionException("REF_NOT_FOUND");
    }

  }


  public Set<Task> cmdSave() throws PersistenceException, SecurityException {

    boolean isQuiteNew = false;
    Task task;
    Task parent = null;

    String parId = restState.getEntry(Fields.PARENT_ID).stringValueNullIfEmpty();
    if (parId != null) {
      parent = (Task) PersistenceHome.findByPrimaryKey(Task.class, parId);
    }

    boolean isNew = PersistenceHome.NEW_EMPTY_ID.equals(restState.mainObjectId);
    if (isNew) {
      task = new Task();
      task.setIdAsNew();
      if (parent != null) {
        task.setArea(parent.getArea());
        parent.testPermission(logged, TeamworkPermissions.task_canCreate);
        task.setParentNode(parent);
      } else {
        //root project
        logged.testPermission(TeamworkPermissions.project_canCreate);
        task.setArea(logged.getDefaultAreaForPermission(TeamworkPermissions.project_canCreate));
      }

      task.setOwner(logged);
    } else
      task = Task.load(restState.getMainObjectId());

    restState.setMainObject(task);
    isQuiteNew = isNew || !JSP.ex(task.getName());

    task.testPermission(logged, TeamworkPermissions.task_canWrite);
    //currently has a fixed behaviour
    //task.setInherit(pageState.getEntry("INHERIT").checkFieldValue());
    //task.setPropagate(pageState.getEntry("PROPAGATE").checkFieldValue());
    task.setInherit(false);
    task.setPropagate(true);

    ActionUtilities.setIdentifiable(restState.getEntry("TASK_TYPE"), task, "type");

    ActionUtilities.setString(restState.getEntry("DESCRIPTION"), task, "description");
    ActionUtilities.setString(restState.getEntry("COLOR"), task, "color");

    //validity check start
    ActionUtilities.setString(restState.getEntryAndSetRequired("TASK_NAME"), task, "name");

    //set tags: si ripuliscono, si rendono univoci e si riseparano con ", " occhio allo spazio!
    task.setTags(StringUtilities.setToString(StringUtilities.splitToOrderSet(JSP.w(restState.getEntry("TASK_TAGS").stringValueNullIfEmpty()), ","), ", "));

    Date start = null;
    Date end = null;
    int duration = 1;
    CompanyCalendar cc = new CompanyCalendar();
    try {

      ClientEntry durCE = restState.getEntry("TASK_DURATION");
      start = restState.getEntry("START").dateValue();
      end = restState.getEntry("END").dateValue();
      if (durCE.isFilled())
        duration = durCE.durationInWorkingDays(true);

      if (duration < 1)
        duration = 1;

      if (end == null && start != null) {
        cc.setTime(start);
        int workingDays = duration - 1;
        cc.addWorkingDays(workingDays);
        end = cc.getTime();
      } else if (start == null && end != null) {
        cc.setTime(end);
        int workingDays = duration - 1;
        cc.addWorkingDays(-workingDays);
        start = cc.getTime();
      } else if (start == null && end == null) {
        start = new Date();
        cc.setTime(start);
        int workingDays = duration - 1;
        cc.addWorkingDays(workingDays);
        end = cc.getTime();
      }

      if (end.getTime() < start.getTime()) {
        restState.getEntry("END").errorCode = I18n.get("END_MUST_BE_AFTER_START");
      } else {
        //duration è sempre ricalcolata sulla base di start ed end
        duration = CompanyCalendar.getWorkingDaysCountInPeriod(new Period(start, end));
        restState.addClientEntry("TASK_DURATION", duration);
      }

    } catch (ActionException e) {
    } catch (ParseException e) {
    }
    //validity check end


    String code = restState.getEntry("TASK_CODE").stringValueNullIfEmpty();
    if (code == null && isNew) {
      code = genTaskCode(task);
      restState.addClientEntry("TASK_CODE", code);
    }
    task.setCode(code);
    // test code uniqueness
    if (Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("USEUNIQUECODES")) && JSP.ex(task.getCode()) && !task.isUnique("code")) {
      restState.getEntry("TASK_CODE").errorCode = I18n.get("KEY_MUST_BE_UNIQUE");
    }


    boolean didPBW = task.isProgressByWorklog();
    ActionUtilities.setBoolean(restState.getEntry("PROGRESS_BY_WORKLOG"), task, "progressByWorklog");
    ClientEntry progress = restState.getEntry("PROGRESS");
    if (!didPBW && task.isProgressByWorklog()) {
      long totalWorklogEstimated = task.getTotalWorklogEstimated();
      if (totalWorklogEstimated == 0 && task.getAssignementsSize() > 0) {
        progress.errorCode = I18n.get("WORKLOG_ESTIMATED_IS_ZERO");
      } else {
        if (totalWorklogEstimated != 0)
          task.setProgress(((double) task.getTotalWorklogDone() / totalWorklogEstimated) * 100);
        else
          task.setProgress(0);
      }
      //reset ce
      progress.setValue(NumberUtilities.decimalNoGrouping(task.getProgress(), NumberUtilities.DEFAULT_DECIMAL_PLACES));

    }

    task.setNotes(restState.getEntry("NOTES").stringValueNullIfEmpty());

    ActionUtilities.setInt(restState.getEntry("RELEVANCE"), task, "relevance");
    ActionUtilities.setDouble(progress, task, "progress");
    task.setCostCenter(restState.getEntry("COSTCENTER").stringValueNullIfEmpty());

    task.setStartIsMilestone(restState.getEntry("STARTISMILESTONE").checkFieldValue());
    task.setEndIsMilestone(restState.getEntry("ENDISMILESTONE").checkFieldValue());


    //Custom fields
    DesignerField.saveCustomFields("TASK_CUSTOM_FIELD_", 6, task, restState);

    task.setParentAndStore(task.getParent());

    Hashtable<Task, TaskScheduleCandidate> taskCandidates = new Hashtable<>();

    String dateChangeNotes = restState.getEntry("LOG_DATE_CHANGE").stringValueNullIfEmpty();

    boolean scheduleIsIdentical = false;
    if (restState.validEntries()) {
      //determine is something changed
      boolean sameDur = true;
      boolean sameStart = true;
      boolean sameEnd = true;

      if (!isNew) {
        //milliseconds are bastardic
        cc.setTime(end);
        Date endToEndOfDay = cc.setAndGetTimeToDayEnd();
        sameDur = duration == task.getDuration();
        sameStart = task.getSchedule() != null && start.equals(task.getSchedule().getStartDate());
        sameEnd = task.getSchedule() != null && (endToEndOfDay.getTime() / CompanyCalendar.MILLIS_IN_DAY) == (task.getSchedule().getEndDate().getTime() / CompanyCalendar.MILLIS_IN_DAY);
        scheduleIsIdentical = sameDur && sameStart && sameEnd;

        scheduleIsIdentical=task.scheduleIsIdentical(start,end,duration);

      }
      if (!scheduleIsIdentical) {
        if (start != null) {
          StringBuffer log = new StringBuffer();
          boolean scheduleOK = Task.analyzeScheduleChangesRun(start, duration, end, task, log, logged, taskCandidates);

          if (JSP.ex(log.toString()))
            restState.addMessageError(I18n.get("CHANGE_SCHEDULE_LOG") + ": " + log.toString(), I18n.get("ERROR_APOLOGIES"));

          if (scheduleOK) {
            for (TaskScheduleCandidate tsc : taskCandidates.values()) {
              tsc.taskScheduleHistory = tsc.task.changeSchedule(tsc.start, tsc.duration, tsc.end, dateChangeNotes, logged);
            }

          } else {
            if (!sameStart)
              restState.getEntry("START").errorCode = I18n.get("CHANGE_SCHEDULE_MISMATCH");
            if (!sameEnd)
              restState.getEntry("END").errorCode = I18n.get("CHANGE_SCHEDULE_MISMATCH");
            if (!sameDur)
              restState.getEntry("TASK_DURATION").errorCode = I18n.get("CHANGE_SCHEDULE_MISMATCH");
            if (sameDur && sameStart && sameEnd)
              restState.getEntry("TASK_DURATION").errorCode = I18n.get("CHANGE_SCHEDULE_MISMATCH");
          }
        } else {
          task.setDuration(duration);
        }
      }
    }

    Set<Task> changedTasks = new HashSet<>();

    if (restState.validEntries()) {
      task.store();

      // se il codice è sempre null lo genero con l'id
      if (!JSP.ex(task.getCode()) && isNew) {
        task.setCode("T" + task.getId());
        restState.addClientEntry("TASK_CODE", task.getCode());
      }


      //save schedule history
      for (TaskScheduleCandidate tsc : taskCandidates.values()) {
        if (tsc.taskScheduleHistory != null) {
          tsc.taskScheduleHistory.store();
        }
        tsc.task.store();
      }

      //show changes
      TaskScheduleCandidate mainTaskScheduleCandidate = taskCandidates.get(task);
      //show info message if dates are changed fro those you set
      if (mainTaskScheduleCandidate != null && ((mainTaskScheduleCandidate.start != null && !mainTaskScheduleCandidate.start.equals(start)) || mainTaskScheduleCandidate.duration != duration))
        restState.addMessageInfo(I18n.get("TASK_DATE_CHANGED_BY_CONSTRAINT"));


      if (mainTaskScheduleCandidate != null) {
        restState.addClientEntry("START", mainTaskScheduleCandidate.start);
        restState.addClientEntry("END", mainTaskScheduleCandidate.end);
        restState.addClientEntry("TASK_DURATION", mainTaskScheduleCandidate.duration);
      }

      //this is in order to read in same transaction
      TaskScheduleCandidate taskScheduleCandidate = mainTaskScheduleCandidate;
      if (taskScheduleCandidate != null) {
        TaskScheduleHistory taskScheduleHistory = taskScheduleCandidate.taskScheduleHistory;
        if (taskScheduleHistory != null)
          task.addScheduleHistory(taskScheduleHistory);
      }

      task.store();

      String oldStatus = task.getStatus();
      String newStatus = restState.getEntry("STATUS").stringValueNullIfEmpty();
      String statusChangeReason = restState.getEntry("LOG_STATUS_CHANGE").stringValueNullIfEmpty();

      //se stai per cambiare lo status si controllano i permessi
      if (oldStatus != null && !oldStatus.equals(newStatus))
        task.testPermission(logged, TeamworkPermissions.task_canChangeStatus);

      // trick in order to know if progress has been changed by status change
      double preProgr = task.getProgress();
      ArrayList<Pair<String, String[]>> errorMessages = new ArrayList<>();
      boolean validStateChange = task.changeStatusPersistAndPropagate(newStatus, statusChangeReason, changedTasks, logged, errorMessages);

      if (!validStateChange || errorMessages.size() > 0) {
        String errMess = "";
        for (Pair<String, String[]> m : errorMessages) {
          errMess += "\n" + (I18n.get(m.first, m.second));
        }
        restState.getEntry("STATUS").errorCode = I18n.get("INVALID_STATE_CHANGE") + errMess; // questo fa fare il rollback dall FCF
      }

      if (restState.validEntries()) {

        //  make the client entry of progress in case of status changed to complete -> 100% . I'll do it only if everithing is fine
        if (restState.validEntries() && preProgr != task.getProgress()) {
          restState.addClientEntry("PROGRESS", task.getProgress());
          // feedback
          restState.addMessageInfo(I18n.get("TASK_PROGRESS_SET_TO_100"));
        }
        //hit criteria
        boolean isRoot = task.getParent() == null;
        boolean statusChanged = newStatus != null && !newStatus.equals(oldStatus) && validStateChange;
        //scheduleIsIdentical
        //isNew
        Hit.getInstanceAndStore(task, logged, .5 + (isRoot ? .4 : 0) + (isNew ? .3 : 0) + (statusChanged ? .2 : 0) + (!scheduleIsIdentical ? .1 : 0));

        restState.removeEntry("LOG_STATUS_CHANGE");
        restState.removeEntry("LOG_DATE_CHANGE");

        if (parent != null && isQuiteNew) {
          parent.happenings.add(parent.generateChildTaskCreatedEvent(logged, task, restState));
          for (SomethingHappened sh : parent.happenings) {
            sh.setIdentifiableId(parent.getId() + ""); // this is necessary in order to update new_empty_id with right ones
            sh.store();
          }
        }
        //store the pending events
        for (SomethingHappened sh : task.happenings) {
          sh.setIdentifiableId(task.getId() + ""); // this is necessary in order to update new_empty_id with right ones
          sh.store();
        }

        task.store();

        for (Task t : changedTasks) {
          for (SomethingHappened sh : t.happenings) {
            sh.setIdentifiableId(t.getId() + ""); // this is necessary in order to update new_empty_id with right ones
            sh.store();
          }

        }
        changedTasks.add(task);

        // ok message feedback
        if (isNew)
          restState.addMessageOK(I18n.get("TASK_CORRECTLY_CREATED"));
        else
          restState.addMessageOK(I18n.get("TASK_CORRECTLY_SAVED"));
      }
    }
    changedTasks.addAll(taskCandidates.keySet());
    return changedTasks;
  }


  public Set<Task> cmdChangeStatus() throws PersistenceException, SecurityException {
    Task task = Task.load(restState.getMainObjectId());
    restState.setMainObject(task);

    Set<Task> changedTasks = new HashSet<>();

    String oldStatus = task.getStatus();
    String newStatus = restState.getEntry("NEW_STATUS").stringValueNullIfEmpty();
    String statusChangeReason = restState.getEntry("LOG_STATUS_CHANGE").stringValueNullIfEmpty();

    //se non cambia nulla si esce e via
    if (oldStatus.equals(newStatus))
      return changedTasks;

    //si controllano i permessi
    task.testPermission(logged, TeamworkPermissions.task_canChangeStatus);


    // trick in order to know if progress has been changed by status change
    double preProgr = task.getProgress();
    ArrayList<Pair<String, String[]>> errorMessages = new ArrayList<>();
    boolean validStateChange = task.changeStatusPersistAndPropagate(newStatus, statusChangeReason, changedTasks, logged, errorMessages);

    if (!validStateChange || errorMessages.size() > 0) {
      String errMess = "";
      for (Pair<String, String[]> m : errorMessages) {
        errMess += "\n" + (I18n.get(m.first, m.second));
      }
      restState.getEntry("NEW_STATUS").errorCode = I18n.get("INVALID_STATE_CHANGE") + errMess; // questo fa fare il rollback dall FCF
    }

    if (restState.validEntries()) {

      //  make the client entry of progress in case of status changed to complete -> 100% . I'll do it only if everithing is fine
      if (preProgr != task.getProgress()) {
        // feedback
        restState.addMessageInfo(I18n.get("TASK_PROGRESS_SET_TO_100"));
      }
      //hit criteria
      boolean isRoot = task.getParent() == null;
      boolean statusChanged = newStatus != null && !newStatus.equals(oldStatus) && validStateChange;
      //isNew
      Hit.getInstanceAndStore(task, logged, .5 + (isRoot ? .4 : 0) + (statusChanged ? .2 : 0));

      //store the pending events
      for (SomethingHappened sh : task.happenings) {
        sh.setIdentifiableId(task.getId() + ""); // this is necessary in order to update new_empty_id with right ones
        sh.store();
      }

      task.store();

      for (Task t : changedTasks) {
        for (SomethingHappened sh : t.happenings) {
          sh.setIdentifiableId(t.getId() + ""); // this is necessary in order to update new_empty_id with right ones
          sh.store();
        }

      }
      changedTasks.add(task);

      // ok message feedback
      restState.addMessageOK(I18n.get("TASK_CORRECTLY_SAVED"));
    }

    return changedTasks;
  }


  public String genTaskCode(Task task) throws StoreException {
    String code;
    String gtc = ApplicationState.getApplicationSetting("GENTASKCODES");
    if ((Fields.TRUE.equalsIgnoreCase(gtc) || "yes_year".equalsIgnoreCase(gtc)) && task.getType() != null) {

      if (Fields.TRUE.equalsIgnoreCase(gtc)) {
        //is there a counter for the type + year?
        code = task.getType().getStringValue() + "_" + CounterHome.next("COUNT_" + task.getType().getStringValue());
      } else {
        String yLastDigits = (new CompanyCalendar().get(Calendar.YEAR) + "").substring(2);
        code = task.getType().getStringValue() + "_" + yLastDigits + "_" + CounterHome.next("COUNT_" + task.getType().getStringValue());
      }
    } else {
      code = null;
    }
    return code;
  }

  public void cmdDelete() throws SecurityException, PersistenceException {

    Task delenda = Task.load(restState.getMainObjectId());
    if (delenda == null)
      return;

    delenda.testPermission(logged, TeamworkPermissions.task_canDelete);

    // check if task is process driven
    ProcessInstance processInstance = null;
    if (delenda.isProcessDriven()) {
      // the root is ok!
      if (delenda.getTaskProcess() != null) {
        //memorize flux instance
        processInstance = delenda.getTaskProcess().getProcessInstance();
      } else {
        throw new PlatformRuntimeException("Cannot delete child task when parent is process-driven.");
      }
    }

    Task parent = delenda.getParent();
    if (parent != null && parent.hasPermissionFor(logged, TeamworkPermissions.task_canRead)) {
      restState.attributes.put("PARENT_TASK_DELETED", parent);
    }
    DeleteHelper.cmdDelete(delenda, restState);

    // these was a process? close it
    if (processInstance != null) {
      processInstance.end();
    }

  }

  public void cmdPrepareDefaultFind() throws ActionException, PersistenceException {

    //search for default filter
    if (restState.getCommand() == null)
      if (!PersistentSearch.feedFromDefaultSearch("TASK", restState)) {
        // when not set use my open task
        restState.addClientEntry(Fields.FLD_FILTER_NAME, "PF_MY_OPEN_TASK");
        //restState.addClientEntry(Fields.FLD_FILTER_NAME, "PF_RECENTLY_USED");
      }

    if (!PersistentSearch.feedFromSavedSearch(restState)) {
      // uso di un filtro presettato
      String cmd = restState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty();
      if (JSP.ex(cmd)) {
        restState.getClientEntries().getClientEntries().clear();

        // add the filter name in order to display boldify
        restState.addClientEntry(Fields.FLD_FILTER_NAME, cmd);

        if ("PF_MY_OPEN_PROJECT".equals(cmd)) {
          restState.addClientEntry("STATUS", TaskStatus.STATUS_ACTIVE);
          restState.addClientEntry("ASSIGNEE", logged.getPerson().getId());
          restState.addClientEntry("ROOT_OR_STANDALONE", Fields.TRUE);

        } else if ("PF_MY_OPEN_TASK".equals(cmd)) {
          restState.addClientEntry("STATUS", TaskStatus.STATUS_ACTIVE);
          restState.addClientEntry("ASSIGNEE", logged.getPerson().getId());

        } else if ("PF_MY_OVERDUE_TASK".equals(cmd)) {
          restState.addClientEntry("ASSIGNEE", logged.getPerson().getId());
          restState.addClientEntry("STATUS", TaskStatus.STATUS_ACTIVE);
          restState.addClientEntry("END", "<today");

          restState.addClientEntry(Form.FLD_FORM_ORDER_BY + "TSKLST", "task.schedule.end");  // order by end date


        } else if ("PF_MY_FORTHCOMING_MILESTONES".equals(cmd)) {
          //pageState.addClientEntry("STATUS", TaskStatus.STATUS_ACTIVE);
          restState.addClientEntry("ASSIGNEE", logged.getPerson().getId());
          restState.addClientEntry("MILESTONE", "today:2w");

          restState.addClientEntry(Form.FLD_FORM_ORDER_BY + "TSKLST", "task.schedule.end");  // order by end date


        } else if ("PF_MY_FORTHCOMING_STARTS".equals(cmd)) {
          //pageState.addClientEntry("STATUS", TaskStatus.STATUS_ACTIVE);
          restState.addClientEntry("ASSIGNEE", logged.getPerson().getId());
          restState.addClientEntry("START", "today:1w");

          restState.addClientEntry(Form.FLD_FORM_ORDER_BY + "TSKLST", "task.schedule.start");  // order by start date


        } else if ("PF_MY_FORTHCOMING_ENDS".equals(cmd)) {
          //pageState.addClientEntry("STATUS", TaskStatus.STATUS_ACTIVE);
          restState.addClientEntry("ASSIGNEE", logged.getPerson().getId());
          restState.addClientEntry("END", "today:1w");
          restState.addClientEntry(Form.FLD_FORM_ORDER_BY + "TSKLST", "task.schedule.end");  // order by end date

        } else if ("PF_MY_JUST_CLOSED".equals(cmd)) {
          restState.addClientEntry("STATUS", TaskStatus.STATUS_DONE);
          restState.addClientEntry("ASSIGNEE", logged.getPerson().getId());
          restState.addClientEntry("END", "-2w:today");

          restState.addClientEntry(Form.FLD_FORM_ORDER_BY + "TSKLST", "task.schedule.end desc");  // order by end date


        } else if ("PF_OPEN_PROJECT".equals(cmd)) {
          restState.addClientEntry("STATUS", TaskStatus.STATUS_ACTIVE);
          restState.addClientEntry("ROOT_OR_STANDALONE", Fields.TRUE);

        } else if ("PF_OPEN_TASK".equals(cmd)) {
          restState.addClientEntry("STATUS", TaskStatus.STATUS_ACTIVE);

        } else if ("PF_OVERDUE_TASK".equals(cmd)) {
          restState.addClientEntry("STATUS", TaskStatus.STATUS_ACTIVE);
          restState.addClientEntry("END", "<today");
          restState.addClientEntry(Form.FLD_FORM_ORDER_BY + "TSKLST", "task.schedule.end");  // order by end date

        } else if ("PF_FORTHCOMING_MILESTONES".equals(cmd)) {
          //pageState.addClientEntry("STATUS", TaskStatus.STATUS_ACTIVE);
          restState.addClientEntry("MILESTONE", "today:1w");

        } else if ("PF_FORTHCOMING_STARTS".equals(cmd)) {
          //pageState.addClientEntry("STATUS", TaskStatus.STATUS_ACTIVE);
          restState.addClientEntry("START", "today:1w");
          restState.addClientEntry(Form.FLD_FORM_ORDER_BY + "TSKLST", "task.schedule.start");  // order by start date

        } else if ("PF_FORTHCOMING_ENDS".equals(cmd)) {
          //pageState.addClientEntry("STATUS", TaskStatus.STATUS_ACTIVE);
          restState.addClientEntry("END", "today:1w");
          restState.addClientEntry(Form.FLD_FORM_ORDER_BY + "TSKLST", "task.schedule.end");  // order by end date

        } else if ("PF_BUDGET_OVERFLOW".equals(cmd)) {
          restState.addClientEntry("STATUS", TaskStatus.STATUS_ACTIVE);
          restState.addClientEntry("BUDGET_OVERFLOW", "yes");

        } else if ("PF_MILESTONES_OVERDUE".equals(cmd)) {
          //pageState.addClientEntry("STATUS", TaskStatus.STATUS_ACTIVE);
          restState.addClientEntry("MILES_OVER", "yes");


        } else if ("PF_JUST_CLOSED".equals(cmd)) {
          restState.addClientEntry("STATUS", TaskStatus.STATUS_DONE);
          restState.addClientEntry("END", "-2w:today");
          restState.addClientEntry(Form.FLD_FORM_ORDER_BY + "TSKLST", "task.schedule.end desc");  // order by end date

        } else if ("PF_LAST_MODIFIED".equals(cmd)) {
          restState.addClientEntry("LAST_MODIFIED", ">-2w");

        } else if ("PF_RECENTLY_USED".equals(cmd)) {
          restState.addClientEntry("RECENTLY_USED", Fields.TRUE);
        }
      }
    }
  }

  public void cmdFind() throws ActionException, PersistenceException {
    //defaults
    cmdPrepareDefaultFind();

    //String hql = "select distinct task, taskschedule from " + Task.class.getName() + " as task";  //todo serve davvero il distinct
    //String hql = "select task, task.schedule from " + Task.class.getName() + " as task";
    String hql = "select task.id from " + Task.class.getName() + " as task";
    QueryHelper qhelp = new QueryHelper(hql);

    if (restState.getEntry("TYPE").intValueNoErrorCodeNoExc() > 0) {
      ActionUtilities.addOQLClause("TYPE", "task.type.id", "typeId", qhelp, QueryHelper.TYPE_INT, restState);
    } else {
      String typeText = restState.getEntry("TYPE" + SmartCombo.TEXT_FIELD_POSTFIX).stringValueNullIfEmpty();
      if (JSP.ex(typeText)) {
        //introduce left outer join
        qhelp.addJoinAlias(" left outer join task.type as taskType");
        ActionUtilities.addQBEClauseForJoinedEntity("TYPE" + SmartCombo.TEXT_FIELD_POSTFIX, "taskType.description", "typeName", qhelp, QueryHelper.TYPE_CHAR, restState);
      }
    }

    ActionUtilities.addQBEClause("CODE", "task.code", "code", qhelp, QueryHelper.TYPE_CHAR, restState);
    ActionUtilities.addQBEClause("TASK_ID", "task.id", "idx", qhelp, QueryHelper.TYPE_CHAR, restState);

    String statuses = restState.getEntry("STATUS").stringValueNullIfEmpty();
    if (JSP.ex(statuses)) {
      if (statuses.contains(","))
        qhelp.addOQLInClause("task.status", "statuses", StringUtilities.splitToList(statuses, ","));
      else
        ActionUtilities.addOQLClause("STATUS", "task.status", "status", qhelp, QueryHelper.TYPE_CHAR, restState);
    }

    ActionUtilities.addQBEClause("LAST_POST", "task.forumEntry.lastPostOnBranch", "lpob", qhelp, QueryHelper.TYPE_DATE, restState);
    ActionUtilities.addQBEClause("RELEVANCE", "task.relevance", "relevance", qhelp, QueryHelper.TYPE_INT, restState);
    ActionUtilities.addQBEClause("PROGRESS", "task.progress", "progress", qhelp, QueryHelper.TYPE_DOUBLE, restState);
    ActionUtilities.addQBEClause("START", "task.schedule.start", "start", qhelp, QueryHelper.TYPE_DATE, restState);
    ActionUtilities.addQBEClause("END", "task.schedule.end", "end", qhelp, QueryHelper.TYPE_DATE, restState);
    ActionUtilities.addQBEClause("LAST_MODIFIED", "task.lastModified", "lastModified", qhelp, QueryHelper.TYPE_DATE, restState);
    ActionUtilities.addQBEClause("CREATED_ON", "task.creationDate", "creationDate", qhelp, QueryHelper.TYPE_DATE, restState);

    ActionUtilities.addQBEClause("NOTES", "task.notes", "notes", qhelp, QueryHelper.TYPE_CHAR, restState);
    ActionUtilities.addOQLClause("COLOR", "task.color", "color", qhelp, QueryHelper.TYPE_CHAR, restState);


    // search by customer id
    String filter = restState.getEntry("CUST_ID").stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addOQLClause(" task.id in (select distinct assc.task.id from " + Assignment.class.getName() + " as assc where assc.resource.id=:custId and assc.role.name like :roleCust)", "custId", filter);
      qhelp.addParameter("roleCust", ApplicationState.getApplicationSetting("DEFAULT_CUSTOMER_ROLE_NAME", "Customer") + "%");
    }


    String resId = restState.getEntry("ASSIGNEE").stringValueNullIfEmpty();
    if (JSP.ex(resId)) {
      // si ricerca per la specifica risorsa o i suoi discendenti
      Resource res = (Resource) PersistenceHome.findByPrimaryKey(Resource.class, resId);
      if (res != null) {
        qhelp.addOQLClause("( assignment.resource.ancestorIds like :resanc or assignment.resource.id=:resid)");
        qhelp.addParameter("resanc", res.getChildAncentorIds() + "%");
        qhelp.addParameter("resid", res.getId());
      }
    } else if (JSP.ex(restState.getEntry("ASSIGNEE_txt"))) {
      // si ricerca per nome risorsa
      ActionUtilities.addQBEClauseForJoinedEntity("ASSIGNEE_txt", "assignment.resource.name", "resName", qhelp, QueryHelper.TYPE_CHAR, restState);
    }
    String ownerPersonId = restState.getEntry("ASSIGNMENT_CREATOR").stringValueNullIfEmpty();
    if (JSP.ex(ownerPersonId)) {
      Person ownerPerson = ((Person) PersistenceHome.findByPrimaryKey(Person.class, ownerPersonId));
      if (ownerPerson.getMyself() != null)
        restState.addClientEntry("ASSIGNMENT_OWNER_ID", ownerPerson.getMyself().getId());
    }

    String roleId = restState.getEntry("ROLE").stringValueNullIfEmpty();
    if (JSP.ex(roleId)) {
      // si ricerca per lo specifico ruolo
      ActionUtilities.addOQLClause("ROLE", "assignment.role.id", "assigrole", qhelp, QueryHelper.TYPE_CHAR, restState);
    } else if (JSP.ex(restState.getEntry("ROLE_txt"))) {
      // si ricerca per nome ruolo
      ActionUtilities.addQBEClauseForJoinedEntity("ROLE_txt", "assignment.role.name", "rolName", qhelp, QueryHelper.TYPE_CHAR, restState);
    }

    int subtaskOf = restState.getEntry("SUBTASK_OF").intValueNoErrorCodeNoExc();
    Task sto = Task.load(subtaskOf + "");
    if (sto != null) {
      qhelp.addOQLClause("(task.id=:tancid or task.ancestorIds like :tancids)", "tancids", sto.getChildAncentorIds() + "%");
      qhelp.addParameter("tancid", sto.getId());
    }


    ActionUtilities.addOQLClause("AREA", "task.area.id", "areaFilterId", qhelp, QueryHelper.TYPE_INT, restState);

    ActionUtilities.addQBEClause("ASSIG_CREATED_ON", "assignment.creationDate", "assCreaDate", qhelp, QueryHelper.TYPE_DATE, restState);


    String stsChDate = restState.getEntry("STATUS_CHANGE_DATE").stringValueNullIfEmpty();
    if (JSP.ex(stsChDate)) {
      qhelp.addJoinAlias("join task.statusHistory as stsHist");
      ActionUtilities.addQBEClause("STATUS_CHANGE_DATE", "stsHist.creationDate", "hist", qhelp, QueryHelper.TYPE_DATE, restState);
    }


    if (restState.getEntry("BUDGET_OVERFLOW").checkFieldValue()) {
      qhelp.addOQLClause("task.forecasted>0");
      qhelp.addOQLClause("task.forecasted<task.totalCostsDone");
    }

    if (restState.getEntry("MILES_OVER").checkFieldValue()) {
      qhelp.addOQLClause("task.schedule is not null");
      qhelp.addQueryClause("( task.status='" + TaskStatus.STATUS_SUSPENDED + "' and task.startIsMilestone=true and task.schedule.start<=:now");
      qhelp.addOrQueryClause("(task.status='" + TaskStatus.STATUS_SUSPENDED + "' or task.status='" + TaskStatus.STATUS_ACTIVE + "') and task.endIsMilestone=true and task.schedule.end<=:now )");
      qhelp.addParameter("now", new Date());
    }

    //ActionUtilities.addQBEClause("TASK_TAGS", "task.tags", "tags", qhelp, QueryHelper.TYPE_CHAR, restState);
    String tags = restState.getEntry("TASK_TAGS").stringValueNullIfEmpty();
    if (JSP.ex(tags)) {
      Set<String> tgs = StringUtilities.splitToSet(tags, ",");
      int i = 1;
      for (String tag : tgs) {
        tag=tag.trim().toUpperCase();
        qhelp.addOQLClause("upper(task.tags) like :tg1_"+i +" or upper(task.tags) like :tg2_"+i+" or upper(task.tags) like :tg3_"+i +" or upper(task.tags)=:tg4_"+i);
        qhelp.addParameter("tg1_" + i, tag + ", %"); //il tag all'inizio
        qhelp.addParameter("tg2_" + i, "%, " + tag + ", %"); //il tag è nel mezzo
        qhelp.addParameter("tg3_" + i, "%, " + tag); //il tag è alla fine
        qhelp.addParameter("tg4_" + i, tag); //il tag è solo soletto
        i++;
      }
    }


    if (restState.getEntry("RECENTLY_USED").checkFieldValue()) {
      List<EntityGroupRank> ranks = RankUtilities.getEntityRankStatistically(logged.getIntId(), Task.class.getName(), new Date());
      boolean something = JSP.ex(ranks);
      if (something) {
        List<String> ids = new ArrayList();
        for (int i = 0; i < ranks.size(); i++) {
          EntityGroupRank egr = ranks.get(i);
          ids.add(egr.id);
          if (i >= 19)
            break;
        }
        qhelp.addOQLInClause("task.id", "taskIds", ids);
      }
    }


    ActionUtilities.addQBEClause("ASSIGNMENT_OWNER_ID", "assignment.owner.id", "ownerId", qhelp, QueryHelper.TYPE_INT, restState);

    //questa e quella usata dalla search nella barra t#xxx# t:xxx e deve vincere sull'altra
    filter = restState.getEntry("NAME_DESCRIPTION_NOTE_CODE").stringValueNullIfEmpty();
    if (filter != null) {

      //per consentire la ricerca NOT si concatenano
      //qhelp.addQBEClause("concat(coalesce(task.code,''),' ' ,coalesce(task.name,''),' ',coalesce(task.description,''),' ',coalesce(task.notes,''))","nameDescNotCode",filter,QueryHelper.TYPE_CHAR);
      qhelp.addQBEClause("concat(coalesce(task.code,''),' ' ,coalesce(task.name,''),' ',coalesce(task.description,''))", "nameDescNotCode", filter, QueryHelper.TYPE_CHAR);  // tolte notes -> schianta Oracle se più di 4000 chars

      /*qhelp.addQBEORClauses(
        filter,
        qhelp.getOrElement("coalesce(task.code,'')", "code", QueryHelper.TYPE_CHAR), //was removed but added again to support search T:code
        qhelp.getOrElement("coalesce(task.name,'')", "name", QueryHelper.TYPE_CHAR),
        qhelp.getOrElement("coalesce(task.description,'')", "description", QueryHelper.TYPE_CHAR),
        qhelp.getOrElement("coalesce(task.notes,'')", "notes", QueryHelper.TYPE_CHAR)
      );*/
    } else {

      //questa e quella usata dalla taskList
      filter = restState.getEntry("NAME_DESCRIPTION").stringValueNullIfEmpty();
      if (filter != null) {

        //per consentire la ricerca NOT si concatenano
        //qhelp.addQBEClause("concat(coalesce(task.name,''),' ',coalesce(task.description,''),' ',coalesce(task.notes,''))","nameDesc",filter,QueryHelper.TYPE_CHAR);
        qhelp.addQBEClause("concat(coalesce(task.name,''),' ',coalesce(task.description,''))", "nameDesc", filter, QueryHelper.TYPE_CHAR);// tolte notes -> schianta Oracle se più di 4000 chars

        /*qhelp.addQBEORClauses(
          filter,
          qhelp.getOrElement("coalesce(task.name,'')", "name", QueryHelper.TYPE_CHAR),
          qhelp.getOrElement("coalesce(task.description,'')", "description", QueryHelper.TYPE_CHAR),
          qhelp.getOrElement("coalesce(task.notes,'')", "notes", QueryHelper.TYPE_CHAR)
        );*/
      }
    }


    //search for custom field
    DesignerField.queryCustomFields("TASK_CUSTOM_FIELD_", 6, "task", qhelp, restState);

    String miles = restState.getEntry("MILESTONE").stringValueNullIfEmpty();
    if (miles != null) {
      String msstart = qhelp.getQbeClause("task.schedule.start", "msstart", miles, QueryHelper.TYPE_DATE).toString();
      String msend = qhelp.getQbeClause("task.schedule.end", "msend", miles, QueryHelper.TYPE_DATE).toString();
      qhelp.addOQLClause("( (" + (JSP.ex(msstart) ? msstart + " and " : "") + " task.startIsMilestone=true) " +
              "or (" + (JSP.ex(msend) ? msend + " and " : "") + " task.endIsMilestone=true))");
    }


    if (Fields.TRUE.equalsIgnoreCase(restState.getEntry("ROOT_OR_STANDALONE").stringValueNullIfEmpty())) {
      qhelp.addOQLClause(" (task.parent is null)");
    } else if (Fields.FALSE.equalsIgnoreCase(restState.getEntry("ROOT_OR_STANDALONE").stringValueNullIfEmpty())) {
      qhelp.addOQLClause(" (task.parent is not null)");
    }


    //  security
    TaskBricks.addSecurityReadClauses(qhelp, restState);


    String newHhql = qhelp.getHqlString();
    newHhql = "select task, task.schedule from " + Task.class.getName() + " as task where task.id in ( " + newHhql + ")";
    qhelp.setHqlString(newHhql);


    if (I18n.isActive("CUSTOM_FEATURE_ORDER_TASK_BY_CODE"))
      DataTable.orderAction(qhelp, "TSKLST", restState, "task.code");
    else
      DataTable.orderAction(qhelp, "TSKLST", restState, "task.name");
    OqlQuery oqlQuery = qhelp.toHql();
    Query query = oqlQuery.getQuery();
    HibernatePage page = HibernatePage.getHibernatePageInstance(query, Paginator.getWantedPageNumber(restState), Paginator.getWantedPageSize("TSKLST", restState));

    restState.setPage(page);
  }


  private void make(Task task) {
    restState.addClientEntry("HIDDEN", (task.isHidden() ? Fields.TRUE : Fields.FALSE));
    restState.addClientEntry("OWNER", task.getOwner() != null ? task.getOwner().getId() : "");
    restState.addClientEntry("AREA", task.getArea() != null ? task.getArea().getId() + "" : null);
    restState.addClientEntry("INHERIT", (task.isInherit() ? Fields.TRUE : Fields.FALSE));
    restState.addClientEntry("PROPAGATE", (task.isPropagate() ? Fields.TRUE : Fields.FALSE));

    if (task.getParent() != null)
      restState.addClientEntry(Fields.PARENT_ID, task.getParent().getId());

    restState.addClientEntry("TASK_CODE", task.getCode());

    restState.addClientEntry("TASK_NAME", task.getName());
    if (task.getType() != null)
      restState.addClientEntry("TASK_TYPE", task.getType().getId().toString());
    restState.addClientEntry("DESCRIPTION", task.getDescription());
    restState.addClientEntry("TASK_TAGS", task.getTags());
    restState.addClientEntry("NOTES", task.getNotes());
    restState.addClientEntry("STATUS", task.getStatus());
    restState.addClientEntry("RELEVANCE", task.getRelevance());
    restState.addClientEntry("PROGRESS", task.getProgress());
    restState.addClientEntry("COLOR", task.getColor());

    //check coherence with duration

    restState.addClientEntry("TASK_DURATION", task.getDuration());

    if (task.getSchedule() != null) {
      restState.addClientEntry("START", task.getSchedule().getStartDate());
      restState.addClientEntry("END", task.getSchedule().getEndDate());
    }
    restState.addClientEntry("COSTCENTER", task.getCostCenter());
    restState.addClientEntry("STARTISMILESTONE", task.isStartIsMilestone());
    restState.addClientEntry("ENDISMILESTONE", task.isEndIsMilestone());
    restState.addClientEntry("PROGRESS_BY_WORKLOG", task.isProgressByWorklog());
    restState.removeEntry("LOG_DATE_CHANGE");
    restState.removeEntry("LOG_STATUS_CHANGE");

    if (task.getArea() != null)
      restState.addClientEntry("TASK_AREA", task.getArea().getId());
  }

  public void cmdSavePublicPage() throws PersistenceException, SecurityException {
    restState.initializeEntries("row");
    editNoMake();
    task.testPermission(logged, TeamworkPermissions.task_canWrite);

    // create publi page

    JSONObject jsonData = task.getJsonData();
    if (!jsonData.has("publicPage"))
      jsonData.element("publicPage", new JSONObject());
    JSONObject taskOptions = jsonData.getJSONObject("publicPage");
    Boolean isPublic = restState.getEntry("MASTER_PUBLIC_TASK").checkFieldValue();
    if (isPublic) {

      taskOptions.put("MASTER_PUBLIC_TASK", Fields.TRUE);

      for (ClientEntry ce : restState.getClientEntries().getClientEntries()) {
        if (ce.name.startsWith("PUBLIC_"))
          taskOptions.element(ce.name, ce.stringValueNullIfEmpty());
      }
    } else {
      jsonData.remove("publicPage");
    }
    task.store();

    // ok message feedback
    restState.addMessageOK(I18n.get("TASK_CORRECTLY_SAVED"));
  }

  public void cmdSaveSecurity() throws PersistenceException, SecurityException {

    editNoMake();
    task.testPermission(logged, TeamworkPermissions.task_canWrite);

    ClientEntry aCE = restState.getEntryAndSetRequired("TASK_AREA");
    try {
      String areaId = aCE.stringValue();

      Area area = (Area) PersistenceHome.findByPrimaryKey(Area.class, areaId);
      task.setArea(area);
      task.store();
      if (restState.getEntry("TASK_AREA_PROPAGATE").checkFieldValue()) {
        List<Task> descs = (List<Task>) task.getDescendants(Task.class);
        for (PerformantNodeSupport desc : descs) {
          ((Task) desc).setArea(area);
          desc.store();
        }
      }

      // ok message feedback
      restState.addMessageOK(I18n.get("TASK_CORRECTLY_SAVED"));

    } catch (ActionException e) {
    }

  }


  public void cmdTakeOwnership() throws PersistenceException, SecurityException {

    cmdEdit();
    task.testPermission(logged, TeamworkPermissions.task_canWrite);

    task.setOwner(logged);
    task.store();
    if (restState.getEntry("TAKE_OWNERSHIP_PROPAGATE").checkFieldValue()) {
      List<Task> descs = (List<Task>) task.getDescendants(Task.class);
      for (Task desc : descs) {
        desc.setOwner(logged);
        desc.store();
      }
    }
    // ok message feedback
    restState.addMessageOK(I18n.get("OWNERSHIP_ACQUIRED") + ": <b>" + task.getOwner().getDisplayName() + "</b>");
  }

  public boolean cmdRecode() throws PersistenceException, ActionException, SecurityException {
    editNoMake();
    task.testPermission(logged, TeamworkPermissions.task_canWrite);
    ClientEntry newCode = restState.getEntryAndSetRequired("newCode");
    boolean ret = true;
    try {
      task.recode(newCode.stringValue());
    } catch (ApplicationException ae) {
      restState.addMessageError(I18n.get(ae.getMessage()), I18n.get("RECODE_TASK_TREE"));
      newCode.errorCode = I18n.get(ae.getMessage());
      ret = false;
    }

    return ret;
  }

  public boolean cmdAddFromTemplate() throws PersistenceException, ActionException, SecurityException {
    editNoMake();
    task.testPermission(logged, TeamworkPermissions.task_canCreate);
    Task currentTask = task;
    //per usare il cmdClone() devo invertire le client entries del template e del main object
    restState.addClientEntry("CLONE_NEW_PARENT", task); //il new parent è il task corrente
    String templateId = restState.getEntryAndSetRequired("CHOOSE_TEMPLATE").stringValue();
    restState.mainObjectId = templateId; // il mainObject che sarà clonato è il task selezionato nel combo
    Task template = Task.load(templateId);

    if (!JSP.ex("TASK_NAME"))
      restState.addClientEntry("TASK_NAME", "[" + template.getName() + "]");

    //restState.addClientEntry("TASK_CODE", "");

    if (!JSP.ex(restState.getEntry("CLONE_NEWSTARTDATE"))) {
      restState.addClientEntry("CLONE_NEWSTARTDATE", currentTask.getSchedule().getStartDate());
    }

    boolean ret = cmdClone();

    return ret;
  }

  public boolean cmdClone() throws PersistenceException, ActionException, SecurityException {
    restState.initializeEntries("row");
    editNoMake();

    boolean clonePreserveParent = restState.getEntry("CLONE_PRESERVE_PARENT").checkFieldValue();
    Task newParentTask = null;
    if (clonePreserveParent)
      newParentTask = task.getParent();
    else if (JSP.ex(restState.getEntry("CLONE_NEW_PARENT"))) {
      newParentTask = Task.load(restState.getEntry("CLONE_NEW_PARENT").intValueNoErrorCodeNoExc() + "");
    }

    //check permissions
    if (newParentTask == null)
      logged.testPermission(logged, TeamworkPermissions.project_canCreate);
    else
      newParentTask.testPermission(logged, TeamworkPermissions.task_canCreate);

    String code = JSP.ex(restState.getEntry("TASK_CODE").stringValueNullIfEmpty()) ? restState.getEntry("TASK_CODE").stringValueNullIfEmpty() : genTaskCode(task);
    String name = restState.getEntryAndSetRequired("TASK_NAME").stringValue();

    Set<String> cloneFlags = new HashSet<>();
    if (clonePreserveParent) cloneFlags.add("CLONE_PRESERVE_PARENT");
    if (restState.getEntryOrDefault("CLONE_CODES").checkFieldValue()) cloneFlags.add("CLONE_CODES");
    if (restState.getEntryOrDefault("CLONE_CUSTOM_FIELDS").checkFieldValue()) cloneFlags.add("CLONE_CUSTOM_FIELDS");
    if (restState.getEntryOrDefault("CLONE_ASSIGS").checkFieldValue()) cloneFlags.add("CLONE_ASSIGS");
    if (restState.getEntryOrDefault("CLONE_ADDITIONAL_COSTS").checkFieldValue())
      cloneFlags.add("CLONE_ADDITIONAL_COSTS");
    if (restState.getEntryOrDefault("CLONE_ISSUES_COPY").checkFieldValue()) cloneFlags.add("CLONE_ISSUES_COPY");
    if (restState.getEntryOrDefault("CLONE_ISSUES_MOVE").checkFieldValue()) cloneFlags.add("CLONE_ISSUES_MOVE");
    if (restState.getEntryOrDefault("CLONE_ISSUES_CLOSED").checkFieldValue()) cloneFlags.add("CLONE_ISSUES_CLOSED");

    Date newStartDate = restState.getEntry("CLONE_NEWSTARTDATE").dateValueNoErrorNoCatchedExc(); // copia le date esistenti se non è specificata una nuova data

    String tyId = restState.getEntry("NEW_TASK_TYPE").stringValueNullIfEmpty();
    TaskType taskType = null;
    if (JSP.ex(tyId)) {
      taskType = (TaskType) PersistenceHome.findByPrimaryKey(TaskType.class, tyId);
    }

    Task clone = cloneTask(code, name, newStartDate, taskType, newParentTask, cloneFlags);
    this.task = clone;
    return true;
  }

  public boolean cmdMove() throws PersistenceException, ActionException, SecurityException {
    editNoMake();
    task.testPermission(logged, TeamworkPermissions.task_canWrite);

    ClientEntry task_new_parent = restState.getEntry("TASK_NEW_PARENT");
    String npid = task_new_parent.stringValueNullIfEmpty();
    Task oldParent = task.getParent();
    Task newParent = null;
    boolean moveOk = true;
    if (npid != null) {
      newParent = (Task) PersistenceHome.findByPrimaryKey(Task.class, npid);

      newParent.testPermission(logged, TeamworkPermissions.task_canCreate);

      task.setParentAndStore(newParent);

      Hashtable<Task, TaskScheduleCandidate> taskCandidates = new Hashtable<>();

      StringBuffer log = new StringBuffer();
      moveOk = Task.analyzeScheduleChangesRun(task.getSchedule().getStartDate(), task.getDuration(), task.getSchedule().getEndDate(), task, log, logged, taskCandidates);


      if (moveOk) {
        for (TaskScheduleCandidate tsc : taskCandidates.values()) {
          tsc.taskScheduleHistory = tsc.task.changeSchedule(tsc.start, tsc.duration, tsc.end, "", logged);
        }

        task.store();

        //touch  parent
        if (oldParent != null)
          oldParent.store();

        //set status in order to check if it is compatible with existeing tree/status
        String newStatus = task.getStatus();
        task.setStatus(TaskStatus.STATUS_UNDEFINED);  //per forzare il ricalcolo e la propagazione
        ArrayList<Pair<String, String[]>> errorMessages = new ArrayList<>();
        if (!task.changeStatusPersistAndPropagate(newStatus, "", new HashSet<Task>(), logged, errorMessages)) {
          String errMess = "";
          for (Pair<String, String[]> m : errorMessages) {
            errMess += "\n" + (I18n.get(m.first, m.second));
          }
          restState.addMessageError(errMess, I18n.get("INVALID_STATE_CHANGE"));
          task_new_parent.errorCode = I18n.get("INVALID_STATE_CHANGE") + errMess; // questo fa fare il rollback dall FCF
        }

      } else {
        restState.addMessageError(log.toString(), I18n.get("CHANGE_SCHEDULE_LOG"));
        task_new_parent.errorCode = I18n.get("CHANGE_SCHEDULE_LOG") + ": " + log.toString();
      }

    } else {
      // set it as root
      logged.testPermission(TeamworkPermissions.project_canCreate);
      task.setParentAndStore(null);
    }

    return moveOk;
  }

  public Task cloneTask(String code, String name, Date newStartDate, TaskType newTaskType, Set<String> cloneFlags) throws PersistenceException, ActionException {
    Task newParent = cloneFlags.contains("CLONE_PRESERVE_PARENT") ? task.getParent() : null;
    return cloneTask(code, name, newStartDate, newTaskType, newParent, cloneFlags);
  }

  public Task cloneTask(String code, String name, Date newStartDate, TaskType newTaskType, Task newParent, Set<String> cloneFlags) throws PersistenceException, ActionException {

    Map<Task, Task> tasksClones = new HashTable<>();

    //se il task ha dipendenze e non stiamo cambiando parent si deve ignorare la newStartDate ed usare la stessa data del task originale
    if (task.getParent() != null && task.getParent().equals(newParent) && task.getPreviousesSize() > 0) {
      newStartDate = task.getSchedule().getStartDate();
    } else if (newParent != null && newStartDate == null) { // se non hai passato la data e ti sposti sotto un nuovo parent prendi la data di inizio sua.
      newStartDate = newParent.getSchedule().getStartDate();
    }
    if (newStartDate == null)
      newStartDate = new Date();


    Task clone = cloneRecursively(task, newParent, code, name, cloneFlags, name, 0, newTaskType, tasksClones, newStartDate, restState);
    cloneDependenciesRecursively(task, tasksClones);
    this.restState.mainObjectId = clone.getId();


    CompanyCalendar cc = new CompanyCalendar(newStartDate);
    int duration = clone.getDuration() - 1;
    cc.addWorkingDays(duration);

    Hashtable<Task, TaskScheduleCandidate> taskCandidates = new Hashtable<>();

    StringBuffer log = new StringBuffer();
    Date newEnd = cc.getTime();
    boolean cloneOk = Task.analyzeScheduleChangesRun(newStartDate, clone.getDuration(), newEnd, clone, log, logged, taskCandidates);

    if (cloneOk) {



      for (TaskScheduleCandidate tsc : taskCandidates.values()) {
        tsc.taskScheduleHistory = tsc.task.changeSchedule(tsc.start, tsc.duration, tsc.end, "", logged);

      }

      //set status in order to check if it is compatible with existeing tree/status
      String newStatus = clone.getStatus();
      clone.setStatus(clone.getParent() == null ? TaskStatus.STATUS_UNDEFINED : clone.getParent().getStatus());  //per forzare il ricalcolo e la propagazione
      ArrayList<Pair<String, String[]>> errorMessages = new ArrayList<>();
      clone.changeStatusPersistAndPropagate(newStatus, "", new HashSet<Task>(), logged, errorMessages);

      /* errorMessages = new ArrayList<Pair<String, String[]>>();
         if (!clone.changeStatusPersistAndPropagate(newStatus, "", new HashSet<Task>(), logged, errorMessages)){
          String errMess = "";
          for (Pair<String, String[]> m : errorMessages) {
            errMess += "\n" + (I18n.get(m.first, m.second));
          }
          restState.addMessageError(errMess,I18n.get("INVALID_STATE_CHANGE"));
          if (restState.hasEntry("CLONE_NEW_PARENT")) restState.getEntry("CLONE_NEW_PARENT").errorCode = I18n.get("INVALID_STATE_CHANGE") + errMess; // questo fa fare il rollback dall FCF
          if (restState.hasEntry("CHOOSE_TEMPLATE")) restState.getEntry("CHOOSE_TEMPLATE").errorCode = I18n.get("INVALID_STATE_CHANGE") + errMess; // questo fa fare il rollback dall FCF
        }*/


    } else {
      restState.addMessageError(log.toString(), I18n.get("CHANGE_SCHEDULE_LOG"));
      String errorMessage = I18n.get("CHANGE_SCHEDULE_LOG") + ": " + log.toString();
      if (restState.hasEntry("CLONE_PRESERVE_PARENT"))
        restState.getEntry("CLONE_PRESERVE_PARENT").errorCode = errorMessage;
      if (restState.hasEntry("CLONE_NEW_PARENT")) restState.getEntry("CLONE_NEW_PARENT").errorCode = errorMessage;
      if (restState.hasEntry("CLONE_NEWSTARTDATE")) restState.getEntry("CLONE_NEWSTARTDATE").errorCode = errorMessage;
      if (restState.hasEntry("CHOOSE_TEMPLATE")) restState.getEntry("CHOOSE_TEMPLATE").errorCode = errorMessage;
      //throw new ActionException(errorMessage);
    }


    return clone;
  }

  private Task cloneRecursively(Task task, Task clonedParent, String newCode, String newName, Set<String> cloneFlags, String parentName, int order, TaskType newTaskType, Map<Task, Task> tasksClones,Date newRootStartDate, RestState restState) throws PersistenceException, ActionException {

    Task clone = new Task();

    // clone names
    if (newName != null)
      clone.setName(newName);
    else {
      if (task.getName() != null)
        clone.setName(task.getName());
      else
        clone.setName(parentName + "." + order);
    }

    //clone description and notes
    clone.setDescription(task.getDescription());
    clone.setNotes(task.getNotes());

    clone.setStatus(task.getStatus());
    clone.setType(task.getType());
    clone.setOrderFactor(task.getOrderFactor());
    clone.setProgressByWorklog(task.isProgressByWorklog());
    clone.setTags(task.getTags());
    clone.setForecasted(task.getForecasted());  //alias budget

    Period schedule = task.getSchedule();
    if (schedule != null) {
      Period p = new Period(schedule.getStartDate(), schedule.getEndDate());
      p.store();
      clone.setSchedule(p);
    }

    // Bicch 3/10/2016 se si mettono le milestones non si copia più niente. Andrebbero fatto tutti i conti e poi rimesse
    //clone.setStartIsMilestone(task.isStartIsMilestone());
    //clone.setEndIsMilestone(task.isEndIsMilestone());


    // duration is always cloned
    clone.setDuration(task.getDuration());

    if (clonedParent != null) {
      clone.setParentAndStore(clonedParent);
    }

    // clone codes
    if (newCode != null) // it is eventually not null only for the top level
      clone.setCode(newCode);
    else {
      if (cloneFlags.contains("CLONE_CODES") && task.getCode() != null)
        clone.setCode(task.getCode());
      else {
        clone.bricks.suggestCodeFromParent();
      }
    }

    if (Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("USEUNIQUECODES")) && JSP.ex(clone.getCode()) && !clone.isUnique("code")) {
      restState.getEntry("TASK_CODE").errorCode = I18n.get("KEY_MUST_BE_UNIQUE");
      //throw new ActionException(I18n.get("KEY_MUST_BE_UNIQUE"));
      //return null;
    }
    // set new type
    if (newTaskType != null)
      clone.setType(newTaskType);

    //security
    clone.setArea(task.getArea());


    clone.store();

    if (cloneFlags.contains("CLONE_ASSIGS")) {
      Iterator<Assignment> assIt = task.getAssignementsIterator();
      while (assIt.hasNext()) {
        Assignment assignment = assIt.next();
        assignment.getClonedInstance(clone).store();

        // notify the assignee
        assignment.generateAssignmentMessages(logged);
      }

      //subscriptions
      String hql = "from " + Listener.class.getName() + " as listen where listen.theClass = :theClass and listen.identifiableId = :identifiableId";
      OqlQuery oql = new OqlQuery(hql);
      oql.getQuery().setString("theClass", Task.class.getName());
      oql.getQuery().setString("identifiableId", task.getId().toString());
      List<Listener> origL = oql.list();
      for (Listener lor : origL) {
        Listener lcl = new Listener();
        lcl.setOwner(lor.getOwner());
        lcl.setIdAsNew();
        lcl.setIdentifiable(clone);
        lcl.setMedia(lor.getMedia());
        lcl.setEventType(lor.getEventType());
        lcl.setListenDescendants(lor.isListenDescendants());
        lcl.store();
      }


    }

    if (cloneFlags.contains("CLONE_CUSTOM_FIELDS")) {
      clone.setCustomField1(task.getCustomField1());
      clone.setCustomField2(task.getCustomField2());
      clone.setCustomField3(task.getCustomField3());
      clone.setCustomField4(task.getCustomField4());
      clone.setCustomField5(task.getCustomField5());
      clone.setCustomField6(task.getCustomField6());
    }

    if (cloneFlags.contains("CLONE_ADDITIONAL_COSTS")) {
      for (Cost cost : task.getCosts()) {
        Cost clonedCost = cost.getClonedInstance();
        clonedCost.store();
        clone.getCosts().add(clonedCost);
      }
    }


    order = 1;
    for (Task tc : task.getChildrenSorted()) {
      cloneRecursively(tc, clone, null, null, cloneFlags, clone.getCode(), order, newTaskType, tasksClones,newRootStartDate, restState);
      order++;
    }


    if (cloneFlags.contains("CLONE_ISSUES_COPY")) {

      CompanyCalendar cc = new CompanyCalendar();
      int deltaInWorkingDays = CompanyCalendar.getDistanceInWorkingDays(task.getSchedule().getStartDate(), newRootStartDate);


      for (Issue issue : task.getIssues()) {
        if (issue.getShouldCloseBy() == null || (issue.getStatus().isBehavesAsClosed() && !cloneFlags.contains("CLONE_ISSUES_CLOSED")))
          continue;

        Issue clonedIssue = issue.clone();
        clonedIssue.setTask(clone);

        //se ho pan e devo shiftare
        cc.setTime(issue.getShouldCloseBy());
        cc.addWorkingDays(deltaInWorkingDays);
        clonedIssue.setShouldCloseBy(cc.getTime());
        clonedIssue.store();

      }

    } else if (cloneFlags.contains("CLONE_ISSUES_MOVE")) {
      for (Issue issue : task.getIssues()) {
        if (issue.getStatus().isBehavesAsClosed() && !cloneFlags.contains("CLONE_ISSUES_CLOSED"))
          continue;
        issue.setTask(clone);
        issue.store();
      }
      task.markAsDirty(); //devo aggiornare il contatore sul vecchio task
    }


    tasksClones.put(task, clone);
    return clone;
  }

  private void cloneDependenciesRecursively(Task task, Map<Task, Task> tasksClones) throws StoreException, FindByPrimaryKeyException {

    //si prova a clonare le dipendenze sul task corrente
    if (task.getPreviouses().size() > 0)
      for (TaskDependency prev : task.getPreviouses()) {

        try {
          Task clone = tasksClones.get(task);

          Task prevOrig = prev.getDepends();
          Task prevCandidated = tasksClones.get(prevOrig);

          //se il task da cui dipendo NON è tra quelli copiati, posso usare il task originale se e solo se è nello stesso progetto
          if (prevCandidated == null) {
            if (prevOrig.getRoot().equals(clone.getRoot()))
              prevCandidated = prevOrig;
            else
              continue;
          }
          TaskDependency td = new TaskDependency();
          td.setTask(clone);
          td.setDependency(prevCandidated);
          td.setLag(prev.getLag());
          td.store();

          //le dipendenze sono inverse su entrambe i lati (previouses, nexts) se non si aggiornano a mano poi i check successivi non ne tengono conto
          //set in memory
          clone.getPreviouses().add(td);
          prevCandidated.getNexts().add(td);


        } catch (ApplicationException e) {
          throw new PlatformRuntimeException(e);
        }
      }

    for (Task tc : task.getChildrenSorted()) {
      cloneDependenciesRecursively(tc, tasksClones);
    }
  }


	public void cmdSnapshot() throws SecurityException, PersistenceException {
		cmdEdit();
		String docName = task.getDisplayName() + "_SNAPSHOT " + DateUtilities.dateAndHourToString(new Date());
		if (TeamworkDocument.load(docName) == null) {
			addDocToTask(task, restState.getEntry("filename").stringValueNullIfEmpty(), docName);
			task.store();
		}
		// ok message feedback
		restState.addMessageOK(I18n.get("SNAPSHOT_CORRECTLY_CREATED"));
	}

  public static void addDocToTask(Task task, String fileLocation, String docName) throws StoreException {
    TeamworkDocument document = new TeamworkDocument();

    document.setIdAsNew();
    document.setArea(task.getArea());
    document.setName(docName);
    document.setType(TeamworkDocument.IS_UPLOAD);
    if (JSP.ex(fileLocation)) {
      PersistentFile persistentFile = new PersistentFile(CounterHome.next(PersistentFile.PERSISTENTFILE_ID), fileLocation);
      persistentFile.setFileLocation(fileLocation);
      document.setFile(persistentFile);
      document.setTask(task);
      document.store();

      task.addDocument(document);
    }

  }


  public void cmdDoProcessStep() throws ActionException, PersistenceException, SecurityException {
    restState.initializeEntries("table");
    try {
      long taskInstanceId = restState.getEntry("TASKINSTANCE").longValue();
      long transitionId = restState.getEntry("TRANSITIONID").longValue();


      String hql = "from " + TaskInstance.class.getName() + " as ti where ti.id = :tiid";
      OqlQuery oql = new OqlQuery(hql);
      oql.getQuery().setLong("tiid", taskInstanceId);
      TaskInstance taskInstance = (TaskInstance) oql.uniqueResultNullIfEmpty();

      try {
        // try to execute the step
        Node from = JBPMQueryHelper.doStep(taskInstance.getId(), transitionId, restState);

        // perform an edit in order to get changes on task happened on process-side handlers
        cmdEdit();

        // if you have rights
        if (task.hasPermissionFor(logged, TeamworkPermissions.task_canWrite))
          cmdSave();

        //remove save messages
        restState.messagesFromController.clear();
        restState.addMessageOK(I18n.get("STEP_%%_CORRECTLY_EXECUTED", from.getName()));
      } catch (Throwable t) {
        restState.addMessageError("Error: " + (t.getMessage() == null ? t + "" : t.getMessage()));
        cmdEdit();
        throw new ActionException(t.getMessage());
      }
    } catch (ParseException e) {
      throw new ActionException(e);
    }
  }


  public void cmdCreateProcess() throws ActionException, PersistenceException, SecurityException, ApplicationException {
    restState.initializeEntries("table");
    JbpmContext jbpmContext = PlatformJbpmSessionFactory.getJbpmContext(restState);
    GraphSession gs = jbpmContext.getGraphSession();

    long processDefinitionId = 0;
    try {
      processDefinitionId = restState.getEntryAndSetRequired("TASK_PROCESS").longValue();
    } catch (ParseException e) {
      throw new ActionException(e);
    }
    ProcessDefinition processDefinition = gs.loadProcessDefinition(processDefinitionId);

    Hashtable<Task, TaskScheduleCandidate> taskCandidates = new Hashtable<>();


    boolean invalidClientEntries;

    Task parent = null;

    String parId = restState.getEntry(Fields.PARENT_ID).stringValueNullIfEmpty();
    if (parId != null) {
      parent = Task.load(parId);
    }

    task = new Task();
    task.setIdAsNew();
    if (parent != null) {
      task.setArea(parent.getArea());
      //parent.testPermission(logged, TeamworkPermissions.task_canCreate);
      //task.setOwner(logged);
      task.setParentNode(parent);
    } else
      task.setArea(logged.getDefaultAreaForPermission(TeamworkPermissions.task_canCreate));


    restState.setMainObject(task);

    if (parent != null)
      parent.testPermission(logged, TeamworkPermissions.task_canCreate);
    else
      logged.testPermission(TeamworkPermissions.project_canCreate);

    task.setOwner(logged);

    //currently has a fixed behaviour
    task.setInherit(false);
    task.setPropagate(true);

    String code = restState.getEntry("TASK_CODE").stringValueNullIfEmpty();
    if (code == null)
      code = genTaskCode(task);
    task.setCode(code);

    invalidClientEntries = !ActionUtilities.setString(restState.getEntryAndSetRequired("TASK_NAME"), task, "name");

    // set description from process
    task.setDescription(processDefinition.getDescription());

    CompanyCalendar cc = new CompanyCalendar(new Date());
    Date maxDate = cc.setAndGetTimeToDayStart();

    task.changeSchedule(cc.setAndGetTimeToDayStart(), 1, cc.setAndGetTimeToDayEnd(), "", logged);
    task.setStatus(TaskStatus.STATUS_ACTIVE);
    task.store();

    restState.mainObjectId = task.getId();


    Hit.getInstanceAndStore(task, logged, .5);

    Set<Node> visitedNodes = new HashSet();
    Map<Transition, Date> visitedTransitions = new HashMap();
    Set<Assignment> assigs = new HashSet();

    Map<Swimlane, Resource> visitedSwims = new HashMap();

    task.setParentAndStore(task.getParent());

    invalidClientEntries = invalidClientEntries || !navigateGraph(processDefinition.getStartState(), maxDate, task, visitedNodes, visitedTransitions, visitedSwims, taskCandidates, assigs);

    for (TaskScheduleCandidate tsc : taskCandidates.values()) {
      tsc.taskScheduleHistory = tsc.task.changeSchedule(tsc.start, tsc.duration, tsc.end, "Change from process", logged);
    }

    for (TaskScheduleCandidate tsc : taskCandidates.values()) {
      if (tsc.taskScheduleHistory != null) {
        tsc.taskScheduleHistory.store();
      }
      tsc.task.store();
    }


    // save assignments
    for (Assignment ass : assigs) {
      ass.store();

      // generate assignment event
      if (ass.getResource() instanceof Person) {
        Person assigneeP = (Person) ass.getResource();
        TeamworkOperator myself = assigneeP.getMyself();
        if (myself != null) {
          // notify the assignee
          ass.generateAssignmentMessages(restState.getLoggedOperator());

          // subscribe the assignee
          // add default make
          SerializedMap<String, String> subm = ass.getRole().getDefaultSubscriptions();
          if (subm != null) {
            for (String k : subm.keySet()) {
              restState.addClientEntry(k, subm.get(k));
            }
          }
          new AssignmentAction(restState).saveSubs(ass.getTask(), logged, ass.getResource().getMyself());
        }
      }
    }

    // everything is calm quiete --> instanziate the flux
    try {
      ProcessInstance processInstance = new ProcessInstance(processDefinition);
      TaskProcess tp = new TaskProcess();
      tp.setTask(task);
      tp.setProcessInstance(processInstance);
      tp.store();
      processInstance.getRootToken().signal();

      task.setTaskProcess(tp); //only in memory: it is an inverse
      // ok message feedback
      restState.addMessageOK(I18n.get("TASKPROCESS_CORRECTLY_CREATED"));

    } catch (Throwable t) {
      restState.addMessageError(t.getMessage());
      ClientEntry error = new ClientEntry("error", "");
      error.errorCode = t.getMessage();
      restState.addClientEntry(error);  //in order to force rollback
      throw new ActionException(t);
    }

  }

  /**
   * @param currentNode
   * @param maxDate
   * @param rootTask
   * @param visitedNodes
   * @param visitedTransition
   * @param taskCandidates
   * @param assigs
   * @return true if everithing goes fine . Task created have the same name of graph nodes (and externalid = nodeId) and assignment the description of graph task (and external id= jbpmTaskId)
   * @throws PersistenceException
   */
  private boolean navigateGraph(Node currentNode, Date maxDate, Task rootTask, Set<Node> visitedNodes, Map<Transition, Date> visitedTransition, Map<Swimlane, Resource> visitedSwims, Hashtable<Task, TaskScheduleCandidate> taskCandidates, Set<Assignment> assigs) throws PersistenceException, ApplicationException {

    boolean allOk = true;
    boolean allIncomingVisited = true;

    Object realNode = ReflectionUtilities.getUnderlyingObjectAsObject(currentNode);
    if (JSP.ex(currentNode.getArrivingTransitions()) && (realNode instanceof Join)) {
      long maxTime = maxDate.getTime();
      for (Transition trans : (Set<Transition>) currentNode.getArrivingTransitions()) {
        if (!visitedTransition.containsKey(trans)) {
          allIncomingVisited = false;
          break;
        } else {
          maxTime = Math.max(maxTime, visitedTransition.get(trans).getTime());
        }
      }
      maxDate = new Date(maxTime);
    }

    if (allIncomingVisited) {
      visitedNodes.add(currentNode);
      if (realNode instanceof TaskNode) {
        TaskNode taskNode = (TaskNode) realNode;

        Task newtask = new Task();
        newtask.setParentAndStore(rootTask);
        newtask.setName(taskNode.getName());

        String descr = taskNode.getDescription(); // here can contains the task duration

        int workingDays = 1;

        if (JSP.ex(descr)) {
          Pattern regex = Pattern.compile("duration:([\\S]*)", Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE);
          Matcher regexMatcher = regex.matcher(descr);
          if (regexMatcher.find()) {
            try {
              workingDays = DateUtilities.daysFromString(regexMatcher.group(1), true);
              descr = regexMatcher.replaceAll("");
            } catch (NumberFormatException nfe) {
            }
          }
        }
        newtask.setDescription(descr);
        newtask.setDuration(workingDays);

        newtask.bricks.suggestCodeFromParent();
        newtask.setArea(rootTask.getArea());
        newtask.setStatus(TaskStatus.STATUS_SUSPENDED);
        newtask.setExternalCode(taskNode.getId() + "");
        CompanyCalendar cc = new CompanyCalendar(maxDate);
        cc.moveToClosestWorkingDay();
        Date startDate = cc.setAndGetTimeToDayStart();
        cc.addWorkingDays(workingDays - 1);
        cc.moveToClosestWorkingDay();
        Date endDate = cc.setAndGetTimeToDayEnd();

//        TeamworkForumEntry forumEntry = newtask.getForumEntry();
//        forumEntry.setContent(newtask.getDisplayName());
//        forumEntry.setPostedOn(new Date());
//        forumEntry.store();

        //newtask.store();
        newtask.changeSchedule(startDate, workingDays, endDate, "", logged);
        StringBuffer log = new StringBuffer();

        allOk = Task.analyzeScheduleChangesRun(startDate, workingDays, endDate, newtask, log, logged, taskCandidates);

        Hit.getInstanceAndStore(newtask, logged, .05);

        cc.setTime(endDate);
        cc.add(CompanyCalendar.HOUR, 3);
        maxDate = cc.setAndGetTimeToDayStart();

        Set<org.jbpm.taskmgmt.def.Task> tset = taskNode.getTasks();
        for (org.jbpm.taskmgmt.def.Task jbTask : tset) {
          Swimlane swimlane = jbTask.getSwimlane();

          if (swimlane == null)
            continue; //managed by handlers

          Resource res = null;
          if (visitedSwims.containsKey(swimlane)) {
            res = visitedSwims.get(swimlane);
          } else {
            String resourceId = restState.getEntry("RESOURCE_" + jbTask.getId()).stringValueNullIfEmpty();
            if (JSP.ex(resourceId)) {
              // get the resource
              res = (Resource) PersistenceHome.findByPrimaryKey(Resource.class, resourceId);
              visitedSwims.put(swimlane, res);
            }
          }

          if (res != null) {
            // get the role
            RoleTeamwork role = null;
            if (swimlane != null) {
              String roleId = restState.getEntry("ROLE_" + jbTask.getId()).stringValueNullIfEmpty();
              if (JSP.ex(roleId)) {
                role = (RoleTeamwork) PersistenceHome.findByPrimaryKey(RoleTeamwork.class, roleId);
              } else {
                role = WizardSupport.getRoleByNameAndArea(swimlane.getName(), rootTask.getArea());
              }
            }
            if (role != null) {
              Assignment ass = new Assignment();
              ass.setTask(newtask);
              ass.setResource(res);
              ass.setOwner(logged);
              ass.setRole(role);
              ass.setDescription(taskNode.getName().equalsIgnoreCase(jbTask.getName()) ? "" : jbTask.getName());
              ass.setAssignmentDate(new Date());
              ass.setExternalCode(jbTask.getId() + "");

              // set description from process
              String jbTaskDescription = jbTask.getDescription();
              long estWl = 0;
              if (JSP.ex(jbTaskDescription)) {
                Pattern regex = Pattern.compile("estimated:([\\S]*)", Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE);
                Matcher regexMatcher = regex.matcher(jbTaskDescription);
                if (regexMatcher.find()) {
                  try {
                    estWl = DateUtilities.millisFromString(regexMatcher.group(1), true);
                    jbTaskDescription = regexMatcher.replaceAll("");
                  } catch (NumberFormatException nfe) {
                  }
                }

              }
              ass.setEstimatedWorklog(estWl); // guess the number?
              ass.setDescription(JSP.w(taskNode.getDescription()) + (JSP.ex(jbTaskDescription) && JSP.ex(taskNode.getDescription()) ? "\n" : "") + JSP.w(jbTaskDescription));

              assigs.add(ass);
            }
          }
        }
      }

      //comod approch: why use the longest? use the shortest? use uan at cas and bi eppi
      for (Transition trans : (List<Transition>) currentNode.getLeavingTransitions()) {
        visitedTransition.put(trans, maxDate);
        if (!visitedNodes.contains(trans.getTo()))
          allOk = allOk && navigateGraph(trans.getTo(), maxDate, rootTask, visitedNodes, visitedTransition, visitedSwims, taskCandidates, assigs);

        if (!allOk)
          break;
      }
    }

    return allOk;
  }


  public void cmdExport() throws ActionException, PersistenceException {
    cmdFind();
    if (JSP.ex(restState.getPage())) {
      List<Task> tks = new ArrayList();
      List<Object[]> elements = restState.getPage().getAllElements();
      for (Object[] ob : elements) {
        Task t = (Task) ob[0];
        if (t != null)
          tks.add(t);
      }
      if (tks.size() > 0) {
        ListPage lp = new ListPage(tks, 0, tks.size());
        restState.setPage(lp);
      }
    }
  }

}