package com.twproject.task;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import javax.persistence.Transient;

import org.hibernate.Query;
import org.hibernate.search.annotations.Boost;
import org.hibernate.search.annotations.DocumentId;
import org.hibernate.search.annotations.FieldBridge;
import org.hibernate.search.annotations.Indexed;
import org.hibernate.search.bridge.builtin.StringBridge;
import org.hibernate.Query;
import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.agenda.Period;
import org.jblooming.agenda.Schedule;
import org.jblooming.logging.Auditable;
import org.jblooming.messaging.SomethingHappened;
import org.jblooming.ontology.HasDenormalizedFields;
import org.jblooming.ontology.Node;
import org.jblooming.ontology.Pair;
import org.jblooming.ontology.PerformantNodeSupport;
import org.jblooming.operator.Operator;
import org.jblooming.operator.User;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.FindException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.RemoveException;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.security.Area;
import org.jblooming.security.Permission;
import org.jblooming.security.PermissionCache;
import org.jblooming.security.Role;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.HashTable;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.RestState;
import org.jbpm.graph.exe.ProcessInstance;

import com.opnlb.fulltext.Indexable;
import com.twproject.document.TeamworkDocument;
import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.PeopleAggregator;
import com.twproject.resource.Person;
import com.twproject.resource.Resource;
import com.twproject.security.RoleTeamwork;
import com.twproject.security.TeamworkPermissions;
import com.twproject.task.financial.Cost;
import com.twproject.utilities.TeamworkComparators;
import com.twproject.worklog.Worklog;
import com.twproject.worklog.WorklogPlan;

import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

@Indexed(index = "fulltext")
@Boost(1.5f)
public class Task extends TaskPersistent implements PeopleAggregator, Auditable, Indexable, HasDenormalizedFields, PermissionCache.PermissionCacheEnabled{

  public boolean resynchedDependencies = false;
  public TaskBricks bricks = new TaskBricks(this);
  public List<SomethingHappened> happenings = new ArrayList();


	private Set<TaskAudit> audits = new HashSet<>();

  public enum Event {
    TASK_STATUS_CHANGE,
    TASK_DATE_CHANGE,
    TASK_MILESTONE_CLOSER,
    TASK_EXPIRED,
    TASK_ISSUE_ADDED,
    TASK_ISSUE_CLOSED,
    TASK_UPDATED_ISSUE,
    TASK_WORKLOG_OVERFLOW,
    TASK_BUDGET_OVERFLOW,
    TASK_WORKLOG_MISPLACED,
    TASK_WORKLOG_OVERTIME,
    TASK_DIARY_CHANGE,
    TASK_CHILD_ADDED,
    TASK_REPORT_ADDED,
    TASK_DOCUMENT_ADDED
  }

  /**
   * Task constructor
   */
  public Task() {
    super();
  }


  @Override
@DocumentId
  @FieldBridge(impl = StringBridge.class)
  public Serializable getId() {
    return super.getId();
  }

  private void setParent(Task n) {
    parent = n;
  }

  public Task getParent() {
    return (Task) parent;
  }


  @Override
public Node getParentNode() {
    return getParent();
  }

  @Override
public void setParentNode(Node node) {
    setParent((Task) node);
  }


  /**
   * We overload SecuredNode's implementation as give the project manager
   * full control on task excluding changing project manager even if she (;-)) has no global permission on area.
   * If task is not persisted, we check on the parent if exists.
   *
   * @param u user for which we ask permission
   * @param p permission required
   */
  @Override
public boolean hasPermissionFor(User u, Permission p) {
    return PermissionCache.hasPermissionFor(u,this, p);
  }

  @Override
public boolean hasPermissionForUnCached(User u, Permission p) {
    boolean result = false;
    result = super.hasPermissionFor(u, p);

    if (!result)
      result = hasPermissionForBasedOnAssignment((TeamworkOperator) u, p);

    return result;
  }

  private boolean hasPermissionForBasedOnAssignment(TeamworkOperator operator, Permission p) {

    Person myself = operator.getPerson();
    boolean result = false;
    for (Assignment assignment : getAssignments()) {
      //case directly on the assig and also
      //case in which assig is on department and I am in department
      if (assignment.getResource().isPersonIn(myself)) {
        Role role = assignment.getRole();
        if (role != null && role.hasPermissionFor(p)) {
          result = true;
          break;
        }
      }

      //se è assegnata a una risorsa che gestisco
      if (!result && RoleTeamwork.getManagerRole().hasPermissionFor(p))
        result= assignment.getResource().hasPermissionFor(operator,p);

      if (result)
        break;

    }



    return result;
  }

  public Set<Area> getAreasForPermission(TeamworkOperator operator, Permission p) {

    Resource myself = operator.getPerson();
    Set<Area> areas = new HashSet();
    for (Assignment assignment : assignments) {
      if (assignment.getResource().equals(myself)) {
        RoleTeamwork role = assignment.getRole();
        if (role != null && role.getArea() != null && role.hasPermissionFor(p)) {
          areas.add(role.getArea());
        }
      }
    }

    if (getParent() != null && (isInherit() || getParent().isPropagate())) {
      Set<Area> parAreas = getParent().getAreasForPermission(operator, p);
      if (parAreas.size() > 0)
        areas.addAll(parAreas);
    }

    return areas;
  }


  public static List<String> getStatusesNames() {

    List<String> statuses = new ArrayList<>();
    Field[] flds = TaskStatus.class.getFields();

    for (Field fld : flds) {
      statuses.add(fld.getName());
    }
    return statuses;
  }


  public static String getStatusColor(String status) {
    String color = "#666666";
    if (TaskStatus.STATUS_ACTIVE.equals(status))
      color = "#3BBF67";
    else if (TaskStatus.STATUS_SUSPENDED.equals(status))
      color = "#F9C154";
    else if (TaskStatus.STATUS_DONE.equals(status))
      color = "#6EBEF4";
    else if (TaskStatus.STATUS_FAILED.equals(status))
      color = "#763A96";
    else if (TaskStatus.STATUS_UNDEFINED.equals(status))
      color = "#dededf";

    /*else if (TaskStatus.STATUS_TO_BE_SENT_TO_REVIEW.equals(status))
      color = "#888888";
    else if (TaskStatus.STATUS_TO_BE_APPROVED.equals(status))
      color = "#888888";*/
    return color;
  }

  public String getStatusColor() {
    return Task.getStatusColor(getStatus());
  }

  public String getTaskColor(){
    if (!JSP.ex(getColor()) && getParent()!=null)
      return getParent().getTaskColor();
    else
      return getColor();
  }

  @Override
public String getDisplayName() {
    return (getCode() != null && !getCode().equals("-") ? getCode() + " - " : "") + JSP.w(getName());
  }


  public boolean changeStatusPersistAndPropagate(String newStatus, String log, Set<Task> changedTasks, Operator whoIsChangingTheStatus, List<Pair<String, String[]>> errorMessages) throws PersistenceException {
    boolean ok = true;

    //takes care of new tasks
    if (isNew() && getParent()==null ) {
      setStatus(newStatus);

    } else {

      //notification are generated only if task is not new and it is created 6 hours before in order to not annoy users (features active)
      boolean notifyStatusChanges = !isNew() && (
        (I18n.isActive("CUSTOM_FEATURE_NOTIFY_ONLY_SIX_HOURS_OLDER_TASK") && (System.currentTimeMillis() - getCreationDate().getTime() > CompanyCalendar.MILLIS_IN_6_HOUR))
          || !I18n.isActive("CUSTOM_FEATURE_NOTIFY_ONLY_SIX_HOURS_OLDER_TASK"));


      Map<Task, String> changedTaskAndStatus = new Hashtable();

      // il cono deve contenere tutti i discendenti che non sono dipendenti. Questi vengono cambiati a cascata dal loro superior
      List<Task> candCone = getDescendants();
      List<Task> cone = new ArrayList();
      for (Task t:candCone) {
        if (t.getPreviousesSize() <= 0) {
          cone.add(t);
        }
      }


      ok = __changeStatusPersistAndPropagate(newStatus, true, false, false, whoIsChangingTheStatus,cone, changedTaskAndStatus, errorMessages);

      if (ok && notifyStatusChanges && changedTaskAndStatus.size() > 0) {


        for (Task task : changedTaskAndStatus.keySet()) {

          String oldStatus = changedTaskAndStatus.get(task);

          //generate task schedule history
          TaskStatusHistory tsth = new TaskStatusHistory();
          tsth.setIdAsNew();
          tsth.setTask(task);
          tsth.setChangeLog(log);
          tsth.setFromStatus(oldStatus);
          tsth.setToStatus(newStatus);
          tsth.store();


          //generate events
          SomethingHappened change = task.generateChangeStatusEvent(oldStatus, newStatus, log, whoIsChangingTheStatus);
          if (change != null)
            happenings.add(change);

        }

        //pass all changed task back
        changedTasks.addAll(changedTaskAndStatus.keySet());
      } else {

      }
    }
    return ok;
  }


  private boolean __changeStatusPersistAndPropagate(String newStatus, boolean manuallyChanged, boolean propagateFromParent, boolean propagateFromChildren, Operator whoIsChangingTheStatus,  List<Task> cone,Map<Task, String> changedTasks, List<Pair<String, String[]>> errorMessages) throws PersistenceException {
    String oldStatus = JSP.w(getStatus());

    if (newStatus.equals(oldStatus) && !isNew()) // se è nuovo deve comunque controllare
      return true;

    boolean todoOk = true;
    setStatus(newStatus);

    //xxxx -> STATUS_DONE            may activate dependent tasks, both suspended and undefined. Will set to done all children.
    //STATUS_FAILED -> STATUS_DONE   do nothing if not forced by hand
    if (TaskStatus.STATUS_DONE.equals(newStatus)) {

      // cannot close task if open issues
      if (I18n.isActive("CUSTOM_FEATURE_DO_NOT_CLOSE_TASK_IF_OPEN_ISSUES")) {
        if (getTotalIssuesOpen() > 0) {
          String[] p=new String[1];
          p[0]=this.getDisplayName();
          errorMessages.add(new Pair("CANNOT_CLOSE_TASK_IF_OPEN_ISSUE_%%", p ));
          return false;
        }
      }

      if ((manuallyChanged || !TaskStatus.STATUS_FAILED.equals(oldStatus))) { //cannot set failed task as closed for cascade - only if changed manually
        //can be closed only if superiors are already done
        for (TaskDependency tdSup : getPreviouses()) {
          Task sup = tdSup.getDepends();
          if (!TaskStatus.STATUS_DONE.equals(sup.getStatus()) && cone.indexOf(sup) < 0) { // è un errore se un predecessore è non chiuso ed è fuori dal cono
            if (manuallyChanged || propagateFromParent)  //genere un errore bloccante se è cambiato a mano o se il cambiamento arriva dal parent ed ho una dipendenza fuori dal cono (altrimenti avrei un attivo figlio di un chiuso
              errorMessages.add(new Pair("ERROR_%%_DEPENDS_ON_OPEN_TASK_%%", new String[]{this.getDisplayName(), sup.getDisplayName()}));
            todoOk = false;
            break;
          }
        }

        if (todoOk) {
          // set progress to 100% if needed by settings
          if (Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("SET100ONCLOSE")) && !isProgressByWorklog())
            setProgress(100.00);

          //set children as done
          propagateStatusToChildren(TaskStatus.STATUS_DONE,false,whoIsChangingTheStatus,cone,changedTasks,errorMessages );

          //set inferiors as active if outside the cone
          propagateStatusToInferiors(TaskStatus.STATUS_ACTIVE,whoIsChangingTheStatus,cone,changedTasks,errorMessages);

        }
      } else {
        todoOk = false;
      }


      //  STATUS_UNDEFINED -> STATUS_ACTIVE       all children become active, if they have no dependencies.
      //  STATUS_SUSPENDED -> STATUS_ACTIVE       sets to active all children and their descendants that have no inhibiting dependencies.
      //  STATUS_DONE -> STATUS_ACTIVE            all those that have dependencies must be set to suspended.
      //  STATUS_FAILED -> STATUS_ACTIVE          nothing happens: child statuses must be reset by hand.
    } else if (TaskStatus.STATUS_ACTIVE.equals(newStatus)) {

      if ((manuallyChanged || !TaskStatus.STATUS_FAILED.equals(oldStatus))) { //cannot set failed task as closed for cascade - only if changed manually

        //can be active only if superiors are already done
        for (TaskDependency tdSup : getPreviouses()) {
          Task sup = tdSup.getDepends();
          if (!TaskStatus.STATUS_DONE.equals(sup.getStatus())) {
            if (manuallyChanged || propagateFromChildren)
              errorMessages.add(new Pair("ERROR_%%_DEPENDS_ON_OPEN_TASK_%%", new String[]{this.getDisplayName(), sup.getDisplayName()}));
            todoOk = false;
            break;
          }
        }

        // check if parent is already active
        if (todoOk){
          Task parent = getParent();
          if (parent != null && !TaskStatus.STATUS_ACTIVE.equals(parent.getStatus())) {
            todoOk=false;
          }
        }

        if (todoOk) {
          if (TaskStatus.STATUS_UNDEFINED.equals(oldStatus) || TaskStatus.STATUS_SUSPENDED.equals(oldStatus)) {
            //set children as active
            propagateStatusToChildren(TaskStatus.STATUS_ACTIVE,true,whoIsChangingTheStatus,cone,changedTasks,errorMessages );
          }

          //set inferiors as suspended
          propagateStatusToInferiors(TaskStatus.STATUS_SUSPENDED,whoIsChangingTheStatus,cone,changedTasks,errorMessages);
        }
      } else {
        todoOk = false;
      }


      // xxxx -> STATUS_SUSPENDED       all active children and their active descendants become suspended. when not failed or forced
    } else if (TaskStatus.STATUS_SUSPENDED.equals(newStatus)) {
      if ((manuallyChanged || !TaskStatus.STATUS_FAILED.equals(oldStatus))) { //cannot set failed task as closed for cascade - only if changed manually

        // check if parent is already active
        Task parent = getParent();
        if (parent != null && !TaskStatus.STATUS_ACTIVE.equals(parent.getStatus())  && !TaskStatus.STATUS_SUSPENDED.equals(parent.getStatus())  ) {
          todoOk=false;
        }

        if (todoOk){
          //set children as suspended
          propagateStatusToChildren(TaskStatus.STATUS_SUSPENDED,true,whoIsChangingTheStatus,cone,changedTasks,errorMessages );

          //set inferiors as suspended
          propagateStatusToInferiors(TaskStatus.STATUS_SUSPENDED,whoIsChangingTheStatus,cone,changedTasks,errorMessages);

        }

      } else {
        todoOk = false;
      }

      // xxxx -> STATUS_FAILED children and dependent failed
      // xxxx -> STATUS_UNDEFINED  children and dependant become undefined.
    } else if (TaskStatus.STATUS_FAILED.equals(newStatus)||TaskStatus.STATUS_UNDEFINED.equals(newStatus)) {

      //set children as suspended
      propagateStatusToChildren(newStatus,false,whoIsChangingTheStatus,cone,changedTasks,errorMessages );

      //set inferiors as suspended
      propagateStatusToInferiors(newStatus,whoIsChangingTheStatus,cone,changedTasks,errorMessages);
    }


    if (todoOk) {
      // add task on changed list if not already there
      if (!changedTasks.containsKey(this))
        changedTasks.put(this, oldStatus);
    } else {
      setStatus(oldStatus);
    }
    return todoOk;
  }

  /**
   * A helper method to traverse an array of 'inferior' tasks
   * and signal a status change.
   */
  private void propagateStatusToInferiors( String status,Operator whoIsChangingTheStatus,  List<Task> cone,Map<Task, String> changedTasks, List<Pair<String, String[]>> errorMessages) throws PersistenceException {
    for (TaskDependency dep : this.getNexts()) {
      dep.getTask().__changeStatusPersistAndPropagate(status, false, false, false, whoIsChangingTheStatus, cone,changedTasks, errorMessages);
    }
  }

  /**
   * A helper method to loop children and propagate new status
   */
  private void propagateStatusToChildren(String newStatus, boolean skipClosedTasks,Operator whoIsChangingTheStatus, List<Task> cone, Map<Task, String> changedTasks, List<Pair<String, String[]>> errorMessages) throws PersistenceException {
    List<Task> chds = this.getChildrenSorted();
    for (Task t: chds)
      if (!(skipClosedTasks && TaskStatus.STATUS_DONE.equals(t.getStatus())) )
        t.__changeStatusPersistAndPropagate(newStatus, false, true, false, whoIsChangingTheStatus, cone,changedTasks, errorMessages);
  }


  public TaskScheduleHistory changeSchedule(Date ceStart, int duration, Date ceEnd, String log, Operator causedTheEvent) throws PersistenceException {

    //should we create/update/reset period ?
    Period oldSchedule = getSchedule();
    int oldDuration=getDuration();

    CompanyCalendar cc = new CompanyCalendar();
    cc.setTime(ceStart != null ? ceStart : new Date());
    Date start = cc.setAndGetTimeToDayStart();

    cc.setTime(ceEnd != null ? ceEnd : (ceStart != null ? ceStart : new Date()));
    Date end = cc.setAndGetTimeToDayEnd();
    Period p = new Period(start, end);

    /*boolean oldIsChanged =
      (getDuration() != duration) ||
        (oldSchedule == null && (ceStart != null || ceEnd != null)) ||
        (oldSchedule != null && (ceStart == null && ceEnd == null)) ||
        (oldSchedule == null || !oldSchedule.toString().equals(new Period(start, end).toString()));
    */
    boolean oldIsChanged=!scheduleIsIdentical(ceStart,ceEnd,duration);

    TaskScheduleHistory tsh = null;
    if (oldIsChanged) {
      if (ceStart == null && ceEnd == null) {
        setSchedule(null);
      } else {
        setSchedule(p);
        setDuration(duration);
      }

      //notification are generated only if task is not new and it is created 6 hours before in order to not annoy users (features active)
      boolean notifyDateChanges = !isNew() && (
        (I18n.isActive("CUSTOM_FEATURE_NOTIFY_ONLY_SIX_HOURS_OLDER_TASK") && (System.currentTimeMillis() - getCreationDate().getTime() > CompanyCalendar.MILLIS_IN_6_HOUR))
          || !I18n.isActive("CUSTOM_FEATURE_NOTIFY_ONLY_SIX_HOURS_OLDER_TASK"));

      if (notifyDateChanges) {
        tsh = new TaskScheduleHistory();
        tsh.setIdAsNew();
        tsh.setTask(this);
        tsh.setChangeLog(log);
        tsh.setSchedule(oldSchedule);

        //generate the event
        if (getSchedule() != null) {
          SomethingHappened change = new SomethingHappened();
          change.setIdAsNew();

          change.setEventType(Event.TASK_DATE_CHANGE.toString());
          change.getMessageParams().put("SUBJECT", JSP.limWr(getDisplayName(), 30));

          change.setMessageTemplate(Event.TASK_DATE_CHANGE + "_MESSAGE_TEMPLATE");

          change.getMessageParams().put("task", getDisplayName());

          change.getMessageParams().put("oldstart", oldSchedule != null ? JSP.w(oldSchedule.getStartDate()) : "-");
          change.getMessageParams().put("oldend", oldSchedule != null ? JSP.w(oldSchedule.getEndDate()) : "-");
          change.getMessageParams().put("start", JSP.w(start));
          change.getMessageParams().put("end", JSP.w(end));
          change.getMessageParams().put("reason", JSP.w(log));
          change.setWhoCausedTheEvent(causedTheEvent);

          PageSeed ps = new PageSeed(ApplicationState.serverURL + "/applications/teamwork/task/taskOverview.jsp");
          ps.setCommand(Commands.EDIT);
          ps.setMainObjectId(getId());
          ButtonLink edit = new ButtonLink(ps);
          edit.label = getDisplayName();
          change.setLink(edit.toPlainLink());

          change.setIdentifiable(this);
          happenings.add(change);
        }
      } else {
        if (oldSchedule != null)
          oldSchedule.remove();
      }


      //added on 23/11/2012 Silvia e rob
      //remove plan outside the task period scope
      if (JSP.ex(getAssignments())) {
        Query query = new OqlQuery("delete from " + WorklogPlan.class.getName() + " as pl where pl.assig in (:assigs) and (pl.inserted>:end or pl.inserted<:start)").getQuery();
        query.setParameterList("assigs", getAssignments());
        query.setTimestamp("start", getSchedule().getStartDate());
        query.setTimestamp("end", getSchedule().getEndDate());
        query.executeUpdate();
      }

      // aggiunto il 22/1/2014 Rob and silvia
      // shifta le issue aperte che hanno una data pianificata
      if (getTotalIssuesOpen() > 0) {
        Query query = new OqlQuery("select i from " + Issue.class.getName() + " as i where i.status.behavesAsOpen=true and i.shouldCloseBy is not null and i.task=:tsk").getQuery();
        query.setEntity("tsk", this);
        List<Issue> iss = query.list();
        for (Issue i : iss) {

          // se la vecchia issue era fuori della durata del task si mette all'inizio
          if (!oldSchedule.contains(i.getShouldCloseBy())) {
            i.setShouldCloseBy(ceStart);
          }

          boolean isPan=oldDuration==getDuration();

          int deltaInDays = Math.round((ceStart.getTime() - oldSchedule.getStartDate().getTime()) / CompanyCalendar.MILLIS_IN_DAY);
          //se ho pan e devo shiftare
          if (isPan && !I18n.isActive("CUSTOM_FEATURE_DO_NOT_SHIFT_ISSUES_PLANNED")){
            cc.setTime(i.getShouldCloseBy());
            cc.add(CompanyCalendar.DATE, deltaInDays);
            i.setShouldCloseBy(cc.getTime());
            i.store();
          }

          boolean isWithinNewDates=  getSchedule().contains(i.getShouldCloseBy());

          //è ancora dentro le date?
          if (!isWithinNewDates){
            //devo andare alla data di inizio del task
            i.setShouldCloseBy(getSchedule().getStartDate());
            i.store();

          }

        }
      }

    }
    return tsh;
  }



  public SomethingHappened generateChangeStatusEvent(String oldStatus, String newStatus, String log, Operator whoIsChangingTheStatus) {
    //generate the event
    SomethingHappened change = null;
    if (!(newStatus + "").equals(oldStatus + "")) {
      change = new SomethingHappened();
      change.setIdAsNew();
      change.setEventType(Event.TASK_STATUS_CHANGE.toString());
      change.getMessageParams().put("SUBJECT", JSP.limWr(getDisplayName(), 30));
      if ((TaskStatus.STATUS_ACTIVE + ", " + TaskStatus.STATUS_SUSPENDED + ", " + TaskStatus.STATUS_DONE + ", " + TaskStatus.STATUS_FAILED).contains(newStatus))
        change.setMessageTemplate(Event.TASK_STATUS_CHANGE + "_MESSAGE_TEMPLATE_" + newStatus);
      else
        change.setMessageTemplate(Event.TASK_STATUS_CHANGE + "_MESSAGE_TEMPLATE");

      change.getMessageParams().put("task", getDisplayName());
      change.getMessageParams().put("fromStatus", oldStatus);
      change.getMessageParams().put("toStatus", getStatus());
      change.getMessageParams().put("reason", log);
      change.setWhoCausedTheEvent(whoIsChangingTheStatus);

      PageSeed ps = new PageSeed(ApplicationState.serverURL + "/applications/teamwork/task/taskOverview.jsp");
      ps.setCommand(Commands.EDIT);
      ps.setMainObjectId(getId());
      ButtonLink edit = new ButtonLink(ps);
      edit.label = getDisplayName();
      change.setLink(edit.toPlainLink());
      change.setIdentifiable(this);
    }
    return change;
  }


  public SomethingHappened generateChildTaskCreatedEvent(Operator causedTheEvent, Task child, RestState pageState) {
    SomethingHappened change = new SomethingHappened();
    change.setIdAsNew();

    change.setEventType(Event.TASK_CHILD_ADDED.toString());
    change.getMessageParams().put("SUBJECT", JSP.limWr(getDisplayName(), 30));
    change.setMessageTemplate(pageState.getI18n(Event.TASK_CHILD_ADDED + "_MESSAGE_TEMPLATE") + " " + child.getDisplayName());

    change.getMessageParams().put("task", getDisplayName());
    change.setWhoCausedTheEvent(causedTheEvent);

    PageSeed ps = new PageSeed(ApplicationState.serverURL + "/applications/teamwork/task/taskOverview.jsp");
    ps.setCommand(Commands.EDIT);
    ps.setMainObjectId(child.getId());
    ButtonLink edit = new ButtonLink(ps);
    edit.label = child.getDisplayName();
    change.setLink(edit.toPlainLink());

    change.setIdentifiable(this);
    return change;

  }


  public void shift(int workingDays) {

    if (getSchedule() != null && getSchedule().getStartDate() != null) {

      CompanyCalendar cc = new CompanyCalendar();
      cc.setTime(getSchedule().getStartDate());
      cc.addWorkingDays(workingDays);
      getSchedule().setStartDate(cc.getTime());

      cc.setTime(getSchedule().getEndDate());
      cc.addWorkingDays(workingDays);
      getSchedule().setEndDate(cc.getTime());

    }

  }

  public boolean previousesContain(Task dependency) {
    boolean result = false;
    for (TaskDependency taskDependency : getPreviouses()) {
      if (taskDependency.getDepends().equals(dependency))
        result = true;
    }
    return result;
  }

  public boolean iDependOn(Task aTask) {
    boolean result = false;
    for (TaskDependency taskDependency : getPreviouses()) {
      if (taskDependency.getDepends().equals(aTask)) {
        result = true;
        break;
      } else
        result = taskDependency.getDepends().iDependOn(aTask);

    }
    return result;
  }

  public void addDependencyInMemory(TaskDependency td) {
    getPreviouses().add(td);
  }

  public void removeDependencyInMemory(TaskDependency taskDependency) {
    getPreviouses().remove(taskDependency);
  }

  public List<Task> getChildrenSorted() {

    if (isNew())
      return new ArrayList();

    List<Task> tasks = new ArrayList(getChildren());
    //Collections.sort(tasks, new TaskBricks.TaskByDateComparator());
    Collections.sort(tasks, new TeamworkComparators.TaskManualOrderComparator());
    return tasks;
  }


  public Set<Assignment> getHierarchyAssignments() {
    Set<Assignment> assigs = new HashSet(getAssignments());

    if (getParent() != null) {
      if (isInherit() || getParent().isPropagate())
        assigs.addAll(getParent().getHierarchyAssignments());
    }
    return assigs;
  }


  public static boolean analyzeScheduleChangesRun(Date newStart, int newDuration, Date newEnd, Task task, StringBuffer log, TeamworkOperator logged, Map<Task, TaskScheduleCandidate> taskCandidates) throws FindException {
    boolean res = analyzeScheduleChanges(newStart, newDuration, newEnd, task, log, logged, true, true, taskCandidates);

    CompanyCalendar cc = new CompanyCalendar();
    //verify permissions on really changed tasks
    if (res) {
      for (TaskScheduleCandidate tsc : taskCandidates.values()) {

        boolean hasBeenChanged = false;
        if (tsc.oldSchedule != null) {
          // si controlla che qualcosa sia cambiato davvero
          long ns = tsc.start.getTime();
          cc.setTimeInMillis(tsc.end.getTime());
          long ne = cc.setAndGetTimeToDayEnd().getTime();

          long oldStart = tsc.oldSchedule.getValidityStartTime();
          cc.setTimeInMillis(tsc.oldSchedule.getValidityEndTime());
          long oldEnd = cc.setAndGetTimeToDayEnd().getTime();

          hasBeenChanged = ne != oldEnd || ns != oldStart;
        } else {
          hasBeenChanged = true;
        }


        if (hasBeenChanged) {
          //se è cambiato si controlla se hai i permessi di scrittura
          if (!tsc.task.hasPermissionFor(logged, TeamworkPermissions.task_canWrite)) {
            log.append(I18n.get("NO_RIGHTS_FOR_CHANGING_FOLLOWING_TASK") + " \"" + tsc.task.getDisplayName() + "\"<br>");
            res = false;
            break;
          }
        }
      }

    }
    return res;
  }

  /**
   * BEWARE, YOU WHO ENTER: LEAVE ALL HOPES OF IMPROVING THIS METHOD!!!!
   *
   * @param levelEntry means that is the first node modified on the tree level, from which to recur on the parent
   * @return true if dates are consistent and can be set on tasks
   */
  private static boolean analyzeScheduleChanges(Date newStart, int newDuration, Date newEnd, Task task, StringBuffer log, TeamworkOperator logged, boolean changedByHand,
                                                boolean levelEntry, Map<Task, TaskScheduleCandidate> taskCandidates) throws FindException {
    boolean result = true;

    CompanyCalendar cc = new CompanyCalendar();
    cc.setTime(newStart);
    newStart = cc.setAndGetTimeToDayStart();
    cc.setTime(newEnd);
    newEnd = cc.setAndGetTimeToDayStart();

    /*
      in questa fase non so se i seguenti cambieranno davvero la data alla fine. Ad esempio se t3 dip da t1(20gg) e da t2(2gg) se ho task_write solo su t2 posso allungarlo a 3gg
       quindi il controllo sulla sicurezza si fa dopo
     */
    //if (!task.hasPermissionFor(logged, TeamworkPermissions.task_canWrite)) { // rimossa bicch+peter 26/8/2015

    Date oldStart = null;
    Date oldEnd = null;
    TaskScheduleCandidate taskScheduleCandidateForTask = createCandidate(task, taskCandidates);
    Period schedule = task.getSchedule();
    if (schedule == null)
      schedule = new Period(newStart, newEnd);
    cc.setTime(schedule.getStartDate());
    oldStart = cc.setAndGetTimeToDayStart();
    cc.setTime(schedule.getEndDate());
    oldEnd = cc.setAndGetTimeToDayStart();

    taskScheduleCandidateForTask.visited = true;

    if (taskScheduleCandidateForTask.start != null) {
      oldStart = taskScheduleCandidateForTask.start;
      oldEnd = taskScheduleCandidateForTask.end;
    }
    taskScheduleCandidateForTask.start = newStart;
    taskScheduleCandidateForTask.end = newEnd;
    taskScheduleCandidateForTask.duration = newDuration;

    if (!changedByHand && task.isStartIsMilestone() && !oldStart.equals(newStart)) {
      log.append(task.getDisplayName() + ": start is milestone - cannot move it from " + JSP.w(oldStart) + " to " + JSP.w(newStart) + "<br>");
      result = false;
    } else if (!changedByHand && task.isEndIsMilestone() && !oldEnd.equals(newEnd)) {
      log.append(task.getDisplayName() + ": end is milestone - cannot move it from " + JSP.w(oldEnd) + " to " + JSP.w(newEnd) + "<br>");
      result = false;
    } else if (!changedByHand && JSP.ex(task.getPreviouses()) && !oldStart.equals(newStart)){
      log.append(task.getDisplayName() + ": start depend on another task - cannot move it from " + JSP.w(oldStart) + " to " + JSP.w(newStart) + "<br>");
      result = false;
    }

    if (result)
      findStartFromLefts(task, taskCandidates, taskScheduleCandidateForTask);

    if (result)
      result = shiftDescendants(task, taskCandidates, log, cc);

    if (result)
      enlargeIfNeeded(task, taskCandidates);

    if (result)
      stabilizeAllNexts(task, taskCandidates);

    if (result)
      result = recurOnNexts(task, taskCandidates, log, logged);

    if (result)
      result = moveUpToParent(task, levelEntry, taskCandidates, log, logged);


    return result;
  }

  private static boolean moveUpToParent(Task task, boolean levelEntry, Map<Task, TaskScheduleCandidate> taskCandidates, StringBuffer log, TeamworkOperator logged) throws FindException {

    TaskScheduleCandidate mainTaskScheduleCandidate = taskCandidates.get(task);
    Date newStart = mainTaskScheduleCandidate.start;
    Date newEnd = mainTaskScheduleCandidate.end;

    boolean result = true;
    //move up to parent if exist and have no left dependencies, i.e. have examined ALL brothers
    Task parent = task.getParent();
    Period max = new Period(newStart, newEnd);
    if (parent != null && levelEntry) {
      for (Object o : parent.getChildren()) {
        Task child = (Task) o;
        TaskScheduleCandidate taskScheduleCandidate = taskCandidates.get(child);
        if (taskScheduleCandidate != null)
          max = max.union(new Period(taskScheduleCandidate.start, taskScheduleCandidate.end));
        else if (child.getSchedule() != null)
          max = max.union(child.getSchedule());
      }

      //if parent is wider, leave it wider
      Date maxStartDate = max.getStartDate();
      Date parentStartDate = parent.getSchedule() == null ? new Date(Long.MAX_VALUE) : parent.getSchedule().getStartDate();
      Date maxEndDate = max.getEndDate();
      Date parentEndDate = parent.getSchedule() == null ? new Date(Long.MIN_VALUE) : parent.getSchedule().getEndDate();

      if (parentEndDate != null) {
        if (maxStartDate.getTime() < parentStartDate.getTime() || (maxEndDate.getTime() > parentEndDate.getTime())) {

          Date newMinStart = new Date(Math.min(maxStartDate.getTime(), parentStartDate.getTime()));
          Date newMaxEnd = new Date(Math.max(maxEndDate.getTime(), parentEndDate.getTime()));
          int duration = CompanyCalendar.getWorkingDaysCountInPeriod(new Period(newMinStart, newMaxEnd));

          boolean problemFromParent = !analyzeScheduleChanges(newMinStart, duration, newMaxEnd, parent, log, logged, false, true, taskCandidates);
          if (problemFromParent) {
            result = false;
          }
        }
      }
    }
    return result;
  }

  private static boolean recurOnNexts(Task task, Map<Task, TaskScheduleCandidate> taskCandidates, StringBuffer log, TeamworkOperator logged) throws FindException {
    boolean result = true;
    //recur on nexts
    for (TaskDependency td : task.getNexts()) {

      Task next = td.getTask();
      TaskScheduleCandidate taskScheduleCandidateForNext = taskCandidates.get(next);
      if (!taskScheduleCandidateForNext.visited && next.getSchedule() != null) {
        if (!analyzeScheduleChanges(taskScheduleCandidateForNext.start, taskScheduleCandidateForNext.duration, taskScheduleCandidateForNext.end, next, log, logged, false, false, taskCandidates)) {
          result = false;
          break;
        }
      }
    }
    return result;
  }

  private static void stabilizeAllNexts(Task task, Map<Task, TaskScheduleCandidate> taskCandidates) {

    Date newEnd = taskCandidates.get(task).end;

    CompanyCalendar cc = new CompanyCalendar();
    //stabilize all nexts
    for (TaskDependency td : task.getNexts()) {

      Task next = td.getTask();

      TaskScheduleCandidate taskScheduleCandidateForNext = createCandidate(next, taskCandidates);
      cc.setTime(newEnd);
      cc.addWorkingDays(td.getLag() + 1);
      Date newStartOfDep = new Date(cc.getTime().getTime());
      int duration = taskScheduleCandidateForNext.duration;//getMaximumDurationOfPreviouses(next,taskCandidates);
      cc.addWorkingDays(duration - 1);
      Date newEndOfDep = new Date(cc.getTime().getTime());
      taskScheduleCandidateForNext.start = newStartOfDep;
      taskScheduleCandidateForNext.end = newEndOfDep;
    }
  }

  private static void enlargeIfNeeded(Task task, TaskScheduleCandidate taskScheduleCandidateForTask) {

    //get fatter if needed
    long maxEndFromChildren = 0;
    for (PerformantNodeSupport pns : task.getChildren()) {
      Task t = (Task) pns;

      if (t.getSchedule() != null && t.getSchedule().getEndDate() != null)
        maxEndFromChildren = Math.max(maxEndFromChildren, t.getSchedule().getEndDate().getTime());
    }
    if (taskScheduleCandidateForTask.end.getTime() < maxEndFromChildren) {
      taskScheduleCandidateForTask.end = new Date(maxEndFromChildren);
      taskScheduleCandidateForTask.duration = CompanyCalendar.getWorkingDaysCountInPeriod(new Period(taskScheduleCandidateForTask.start, taskScheduleCandidateForTask.end));
    }
  }

  private static void enlargeIfNeeded(Task task, Map<Task, TaskScheduleCandidate> taskCandidates) {

    //get fatter if needed
    long maxEndFromChildren = 0;
    for (PerformantNodeSupport pns : task.getChildren()) {
      Task t = (Task) pns;
      Schedule s = t.getSchedule();
      if(taskCandidates.containsKey(t)){
        s = new Period(taskCandidates.get(t).start, taskCandidates.get(t).end);
      }

      if (s != null && s.getEndDate() != null)
        maxEndFromChildren = Math.max(maxEndFromChildren, s.getEndDate().getTime());
    }

    TaskScheduleCandidate taskScheduleCandidate = taskCandidates.get(task);
    if (taskScheduleCandidate.end.getTime() < maxEndFromChildren) {
      taskScheduleCandidate.end = new Date(maxEndFromChildren);
      taskScheduleCandidate.duration = CompanyCalendar.getWorkingDaysCountInPeriod(new Period(taskScheduleCandidate.start, taskScheduleCandidate.end));
    }
  }

  private static boolean shiftDescendants(Task task, Map<Task, TaskScheduleCandidate> taskCandidates, StringBuffer log, CompanyCalendar cc) {

    Date newStart = taskCandidates.get(task).start;

    boolean result = true;
    //did start change? if yes, just shift everything if you can
    if (task.getSchedule() != null && !(newStart.getTime() == task.getSchedule().getStartDate().getTime()) && task.getChildrenSize() > 0) {
      //is there a miles on descendants ?
      List<Task> descs = task.getDescendants();
      Date startOfDescendants = task.getMinimumStartOfDescendants(taskCandidates);
      //bicch 22/4/2016: si shifta di quanto si è shiftato il padre, così come fa il gantt editor. La vecchia versione invece non shiftava se si rimaneva dentro le date
      //int workingDays = CompanyCalendar.getDistanceInWorkingDays(startOfDescendants, newStart);
      int workingDays = CompanyCalendar.getDistanceInWorkingDays( task.getSchedule().getStartDate(), newStart);
      int shiftInWorkingDays = workingDays - (int) Math.signum(workingDays);

      for (Task desc : descs) {
        //shift if not already ciancicated
        if (!taskCandidates.containsKey(desc) && desc.getSchedule() != null && desc.getSchedule().getStartDate() != null && desc.getSchedule().getEndDate() != null) {

          if (desc.isStartIsMilestone() || desc.isEndIsMilestone()) {
            result = false;
            log.append(task.getDisplayName() + ": there is a milestone - cannot shift it<br>");
          }

          cc.setTime(desc.getSchedule().getStartDate());
          cc.addWorkingDays(shiftInWorkingDays);
          Date newStartOfDesc = cc.getTime();

          cc.setTime(desc.getSchedule().getEndDate());
          cc.addWorkingDays(shiftInWorkingDays);
          Date newEndOfDesc = cc.getTime();

          TaskScheduleCandidate tsc = createCandidate(desc, taskCandidates);
          tsc.start = newStartOfDesc;
          tsc.end = newEndOfDesc;
          tsc.duration = desc.getDuration();
        }
      }
    }
    return result;
  }

  private static void findStartFromLefts(Task task, Map<Task, TaskScheduleCandidate> taskCandidates, TaskScheduleCandidate taskScheduleCandidateForTask) {
    CompanyCalendar cc = new CompanyCalendar();
    long maxEndOfPreviouses = 0;

    //find start from max of ends of lefts
    for (TaskDependency td : task.getPreviouses()) {
      Task previous = td.getDepends();
      if (previous.getSchedule() != null && previous.getSchedule().getEndDate() != null) {
        long time = 0;
        TaskScheduleCandidate taskScheduleCandidateForPrev = taskCandidates.get(previous);
        if (taskScheduleCandidateForPrev != null)
          time = taskScheduleCandidateForPrev.end.getTime();
        else
          time = previous.getSchedule().getEndDate().getTime();
        cc.setTimeInMillis(time);
        cc.addWorkingDays(td.getLag() + 1);
        maxEndOfPreviouses = Math.max(cc.getTime().getTime(), maxEndOfPreviouses);
      }
    }

    if (maxEndOfPreviouses > 0) {
      taskScheduleCandidateForTask.start = new Date(maxEndOfPreviouses);
      cc.setTimeInMillis(maxEndOfPreviouses);
      taskScheduleCandidateForTask.start = cc.setAndGetTimeToDayStart();
      cc.addWorkingDays(taskScheduleCandidateForTask.duration - 1);
      taskScheduleCandidateForTask.end = cc.getTime();
    }
  }

  private static TaskScheduleCandidate createCandidate(Task t, Map<Task, TaskScheduleCandidate> taskCandidates) {
    TaskScheduleCandidate tsc = taskCandidates.get(t);
    if (tsc == null) {
      tsc = new TaskScheduleCandidate(t);
      tsc.duration = t.getDuration();
      taskCandidates.put(t, tsc);
    }
    return tsc;
  }

  @Transient
  public Assignment getFirstAssignmentsForResource(Resource res) {
    return getFirstAssignmentsForResource(res,false);
  }

  public Assignment getFirstAssignmentsForResource(Resource res, boolean searchOnChildren) {
    Assignment ass = null;
    for (Assignment a : getAssignments()) {
      if (a.getResource().equals(res)) {
        ass = a;
        break;
      }
    }
    if (searchOnChildren && ass==null){
      for (Task ch: getChildrenSorted()){
        ass=ch.getFirstAssignmentsForResource(res,searchOnChildren);
        if (ass!=null)
          break;
      }
    }
    return ass;
  }

  @Transient
  public Assignment getFirstAssignmentsForRole(Role role) {
    Assignment ass = null;
    for (Assignment a : getAssignments()) {
      if (a.getRole().equals(role)) {
        ass = a;
        break;
      }
    }
    return ass;
  }

  /**
   * @param when
   * @return the task status on the specific date
   */

  @Transient
  public String getStatusOn(Date when) {
    String curStat = getStatus();

    CompanyCalendar cc = new CompanyCalendar();

    Iterator<TaskStatusHistory> tsii = getStatusHistoryIterator();
    while (tsii.hasNext()) {
      TaskStatusHistory tsi = tsii.next();

      //P&S: when is at hour 00:00; but if you activated sometime during the day, you should see it!!
      //so normalize when to the hour in this instant      
      cc.setTime(tsi.getCreationDate());
      cc.setAndGetTimeToDayStart();

      if (when.compareTo(cc.getTime()) < 0)
        curStat = tsi.getFromStatus();
      else
        break;
    }

    return curStat;
  }

  @Transient
  public Date getLastStatusChangeDate() {
    Date ret=getCreationDate();
    Iterator<TaskStatusHistory> tsii = getStatusHistoryIterator();
    if (tsii.hasNext())
      ret=tsii.next().getCreationDate();
    return ret;
  }



    public static class TaskMilestone implements Comparable {

    public Date when;
    public boolean isStart;
    public Task task;

    public TaskMilestone(Date when, boolean isStart, Task task) {
      this.when = when;
      this.isStart = isStart;
      this.task = task;
    }

    public int compareTo(Object miles) {

      TaskMilestone rd2 = (TaskMilestone) miles;
      if (when != null)
        return when.compareTo(rd2.when);
      else
        return 0;
    }

    public JSONObject jsonify() {
      JSONObject ret = new JSONObject();
      ret.element("when", when.getTime());
      ret.element("isStart", isStart);
      ret.element("taskId", task.getId());
      //ret.element("taskName",task.getDisplayName());
      ret.element("taskName", task.getName());
      ret.element("taskCode", task.getCode());
      return ret;
    }

  }

  public List<TaskMilestone> getMilestones() {
    return getMilestones(false, false);
  }

  public List<TaskMilestone> getMilestones(boolean includeMyself, boolean includeDescendant) {

    List<TaskMilestone> ml = new ArrayList<TaskMilestone>();
    if (includeMyself && getSchedule() != null) {
      if (isStartIsMilestone() && getSchedule().getStartDate() != null) {
        ml.add(new TaskMilestone(getSchedule().getStartDate(), true, this));
      }
      if (isEndIsMilestone() && getSchedule().getEndDate() != null) {
        ml.add(new TaskMilestone(getSchedule().getEndDate(), false, this));
      }

    }

    Collection<Task> chI = null;
    if (includeDescendant)
      chI = getDescendants();
    else
      chI = getChildrenSorted();

    for (Task task : chI) {
      if (task.getSchedule() != null) {
        if (task.isStartIsMilestone()) {
          ml.add(new TaskMilestone(task.getSchedule().getStartDate(), true, task));
        }
        if (task.isEndIsMilestone()) {
          ml.add(new TaskMilestone(task.getSchedule().getEndDate(), false, task));
        }
      }
    }

    Collections.sort(ml);
    return ml;
  }


  public boolean isActive() {
    return TaskStatus.STATUS_ACTIVE.equals(getStatus()); //|| TaskStatus.STATUS_UNDEFINED.equals(getStatus());
    //return ! (TaskStatus.STATUS_DONE.equals(getStatus()) || TaskStatus.STATUS_SUSPENDED.equals(getStatus()) || TaskStatus.STATUS_FAILED.equals(getStatus()));
  }

  public boolean isClosed() {
    return TaskStatus.STATUS_DONE.equals(getStatus()) || TaskStatus.STATUS_FAILED.equals(getStatus());
  }

  /**
   *
   * @return a set that preserves insertion order
   */
  public Set<Resource> getWorkGroup() {
    LinkedHashSet<Resource> result = new LinkedHashSet<Resource>();
    addWorkersToList(this, result);
    return result;
  }

  private static void addWorkersToList(Task task, LinkedHashSet<Resource> result) {

    //get from assigs
    Iterator it = task.getAssignementsIterator();
    while (it.hasNext()) {
      Assignment assignment = (Assignment) it.next();
      result.add(assignment.getResource());
    }

    //if propag., recurse
    if (task.isPropagate() && task.getChildrenNode().size() > 0) {
      for (Object child : task.getChildrenNode()) {
        addWorkersToList((Task) child, result);
      }
    }
  }

  public LinkedList<Issue> getOpenIssuesById(PageState pageState) throws PersistenceException {

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    LinkedList<Issue> ll = new LinkedList<Issue>();

    Iterator i = getIssuesIterator();
    while (i.hasNext()) {
      Issue issue = (Issue) i.next();
      if (issue.getStatus().isBehavesAsOpen()) {
        if (issue.hasPermissionFor(logged, TeamworkPermissions.issue_canRead))
          ll.add(issue);
      }
    }
    Collections.sort(ll);
    return ll;
  }


  @Transient
  public Task getRoot() throws FindByPrimaryKeyException {
    Task ret=this;
    List<String> ancestorIdsAsList = getAncestorIdsAsList();
    if (JSP.ex(ancestorIdsAsList))
      ret=Task.load(ancestorIdsAsList.get(0));

    return ret;
  }

  /**
   * does not include this
   * surfes objects instead of doing an optimized query
   * DESCENDANTS ARE SORTED BY DATE AND DISPLAYNAME
   */
  public List<Task> getDescendants() {
    return getDescendants(new TeamworkComparators.TaskManualOrderComparator());
  }

  public List<Task> getDescendants(Comparator c) {
    return addDescendants(this, new LinkedList(), c);
  }

  private List<Task> addDescendants(Task task, List<Task> descs, Comparator c) {
    List<Task> chss = new ArrayList(task.getChildren());
    Collections.sort(chss, c);
    for (PerformantNodeSupport performantNodeSupport : chss) {
      Task desc = (Task) performantNodeSupport;
      descs.add(desc);
      addDescendants(desc, descs, c);
    }
    return descs;
  }


  /**
   * @param showDescendants
   * @param orderBy
   * @return
   * @throws FindException
   */
  public TreeSet<Task> getDescendantsActiveOrdered(boolean showDescendants, String orderBy) throws FindException {

    String hql = "from " + Task.class.getName() + " as task where ( task.parent = :myself";
    if (showDescendants)
      hql = hql + " or task.ancestorIds like (:descs)  ";
    hql = hql + " ) and (task.status = :stActive or task.status = :stUndef)";
    if (orderBy != null)
      hql = hql + " order by " + orderBy;
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setString("stActive", TaskStatus.STATUS_ACTIVE);
    oql.getQuery().setString("stUndef", TaskStatus.STATUS_UNDEFINED);

    if (showDescendants)
      oql.getQuery().setString("descs", getChildAncentorIds() + "%");

    oql.getQuery().setEntity("myself", this);
    return new TreeSet<Task>(oql.list());
  }


  /**
   * @return worklog on this task only -> without children
   */
  @Transient
  public long getWorklogDone() {
    long twd = 0;
    for (Assignment a : getAssignments()) {
      twd += a.getWorklogDone();
    }
    return twd;
  }

  /**
   * @return the sum of worklogsEstimation for this task without children
   */
  @Transient
  public long getWorklogEstimated() {
    long twd = 0;
    for (Assignment a : getAssignments()) {
      twd += a.getEstimatedWorklog();
    }
    return twd;
  }

  /**
   * @return the sum of costs plus costs from assigs
   */
  @Transient
  public double getCostsDone() {
    double t = 0;
    for (Assignment a : getAssignments()) {
      t += a.getCostDone(true);
    }
    for (Cost c : getCosts()) {
      t += c.getRealCost();
    }
    return t;
  }

  /**
   * @return the sum of costs plus costs from assigs
   */
  @Transient
  public double getCostsEstimated() {
    double t = 0;
    for (Assignment a : getAssignments()) {
      t += a.getCostEstimated(true);
    }
    for (Cost c : getCosts()) {
      t += c.getEstimatedCost();
    }
    return t;
  }


  public void remove(PersistenceContext pc) throws RemoveException {
    Task parent = (Task) getParentNode();

    super.remove(pc);

    if (parent!=null)
      parent.markAsDirty();

    /*
    while (parent != null) {
      parent.setTotalWorklogDone(parent.getTotalWorklogDone() - getTotalWorklogDone());
      parent.setTotalWorklogEstimated(parent.getTotalWorklogEstimated() - getTotalWorklogEstimated());
      parent.setTotalCostsDone(parent.getTotalCostsDone() - getTotalCostsDone());
      parent.setTotalCostsEstimated(parent.getTotalCostsEstimated() - getTotalCostsEstimated());

      //issue totals
      parent.setTotalIssues(parent.getTotalIssues() - getTotalIssues());
      parent.setTotalIssuesOpen(parent.getTotalIssuesOpen() - getTotalIssuesOpen());
      parent.setTotalIssuesScoreClosed(parent.getTotalIssuesScoreClosed() - getTotalIssuesScoreClosed());
      parent.setTotalIssuesScoreOpen(parent.getTotalIssuesScoreOpen() - getTotalIssuesScoreOpen());

      parent = parent.getParent();
    }
    updateProgress();*/
  }


  public void store(PersistenceContext pc) throws StoreException {
    super.store(pc);
    //ogni volta che si salva il task si deve
    markAsDirty();
  }

  @Override
  public void recomputeDenormalizedFields(PersistenceContext pc) {
    updateTotals();
    updateProgress();
    updateIssueTotals();

    //deve
    Task parent = getParent();
    if (parent!=null)
      parent.markAsDirty();

  }


  /**
   *  Non itera sui padri
   */
  private void updateTotals() {
      long twd = 0;  //todo aaaarrgggghhhhh questo non funziona quando sposti una assegnazione. le inverse non sono aggiornate
      long twe = 0;

      double costE = 0;
      double costD = 0;
      Iterator i = getChildrenIterator();
      while (i.hasNext()) {
        Task child = (Task) i.next();
        twd += child.getTotalWorklogDone();
        twe += child.getTotalWorklogEstimated();
        costE += child.getTotalCostsEstimated();
        costD += child.getTotalCostsDone();
      }
      setTotalWorklogDone(twd + getWorklogDone());
      setTotalWorklogEstimated(twe + getWorklogEstimated());
      setTotalCostsDone(costD + getCostsDone());
      setTotalCostsEstimated(costE + getCostsEstimated());

  }



  /**
   *  Non itera sui padri
   */
  private void updateProgress() {
    if (isProgressByWorklog()) {
      long totalWorklogEstimated = getTotalWorklogEstimated();
      if (totalWorklogEstimated > 0) {
        long deltaWklgPerc = getTotalWorklogDone() * 100 / totalWorklogEstimated;
        setProgress(deltaWklgPerc);
      }
    }
  }


  /**
   *  Non itera sui padri
   */
  private void updateIssueTotals() {
    try {
      if (!isNew()) {

        //06/04/2015 aggiunte anche le issues sui discendenti
        String hql = "select count(iss.id),iss.status.behavesAsOpen,iss.gravity,sum(iss.estimatedDuration) from " + Issue.class.getName() + " as iss where (iss.task=:tsk or iss.task.ancestorIds like :descs) group by iss.status.behavesAsOpen,iss.gravity";
        OqlQuery oqlQuery = new OqlQuery(hql);
        oqlQuery.getQuery().setEntity("tsk", this);
        oqlQuery.getQuery().setString("descs", this.getChildAncentorIds()+"%");
        List<Object[]> res = oqlQuery.list();

        long totwl = 0;
        int totIss = 0;
        int totOpenIss = 0;
        int totScoreOpenIss = 0;
        int totScoreCloseIss = 0;

        for (Object[] os : res) {
          totIss += (Long) os[0];
          totwl += (Long) os[3];
          if ((Boolean) os[1]) {
            totOpenIss += (Long) os[0];
            totScoreOpenIss += Issue.getGravityScore((String) os[2]) * (Long) os[0];
          } else {
            totScoreCloseIss += Issue.getGravityScore((String) os[2]) * (Long) os[0];
          }
        }
        setTotalIssues(totIss);
        setTotalIssuesOpen(totOpenIss);
        setTotalIssuesScoreOpen(totScoreOpenIss);
        setTotalIssuesScoreClosed(totScoreCloseIss);
        setTotalEstimatedFromIssues(totwl);

      }
    } catch (Throwable t) {
      Tracer.platformLogger.error(t);
      throw new PlatformRuntimeException(t);
    }
  }


  public long getWorklogEstimatedFromUnassignedIssues() {
    long result = 0;
    if (!isNew()) {
      String hql = "select sum(issue.estimatedDuration) from " + Issue.class.getName() + " as issue where issue.task=:tsk and issue.assignedTo is null";
      OqlQuery oql = new OqlQuery(hql);
      oql.getQuery().setEntity("tsk", this);
      Object o = oql.uniqueResultNullIfEmpty();
      if (o != null)
        result = (Long) o;
    }
    return result;
  }


  public boolean isPersonIn(Person o) {
    return getPersons().contains(o);
  }

  public Set<Person> getPersons() {
    return getPersons(new HashSet<PeopleAggregator>(), new HashSet<Person>());
  }


  public Set<Person> getPersons(Set<PeopleAggregator> visitedNodes, Set<Person> workers) {

    Resource res;

    if (!visitedNodes.contains(this)) {

      visitedNodes.add(this);

      for (Object assignement : assignments) {
        Assignment assignment = (Assignment) assignement;
        res = assignment.getResource();
        if (res != null && !res.isHidden())
          workers.addAll(res.getPersons());
      }

      // visiting parent node

      if (this.isInherit() && getParent() != null) {
        workers.addAll((getParent()).getPersons(visitedNodes, workers));
      }

      // visiting children

      if (this.isPropagate()) {
        Iterator iter = this.getChildrenIterator();
        Task t;
        while (iter.hasNext()) {
          t = (Task) iter.next();
          workers.addAll(t.getPersons(visitedNodes, workers));
        }
      }
    }
    return workers;
  }

  public Date getMinimumStartOfDescendants(Map<Task, TaskScheduleCandidate> taskCandidates) {

    List<Task> descs = getDescendants();
    long min = Long.MAX_VALUE;
    for (Task task : descs) {
      TaskScheduleCandidate taskScheduleCandidate = taskCandidates.get(task);
      if (taskScheduleCandidate != null)
        min = Math.min(min, taskScheduleCandidate.start.getTime());
      else if (task.getSchedule() != null && task.getSchedule().getStartDate() != null)
        min = Math.min(min, task.getSchedule().getStartDate().getTime());
    }
    return new Date(min);
  }


  /**
   * @return true if there is a process associated to the task or on the direct parent
   */
  public boolean isProcessDriven() {
    return (getTaskProcess() != null) || (getParent() != null && getParent().getTaskProcess() != null);
  }

  public ProcessInstance getProcess() {
    ProcessInstance ret = null;
    if (getTaskProcess() != null)
      ret = getTaskProcess().getProcessInstance();
    else if (getParent() != null) {
      if (getParent().getTaskProcess() != null)
        ret = getParent().getTaskProcess().getProcessInstance();
    }
    return ret;
  }

  public Task getProcessRootTask() {
    Task ret = null;
    if (getTaskProcess() != null)
      ret = this;
    else if (getParent() != null) {
      if (getParent().getTaskProcess() != null)
        ret = getParent();
    }
    return ret;
  }


  public boolean isAssigned(Resource res) {
    boolean ret = false;
    for (Assignment ass : getAssignments()) {
      if (ass.getResource().equals(res)) {
        ret = true;
        break;
      }
    }
    return ret;
  }


  public Assignment getAssignementForResource(Resource res) {
    Assignment ret = null;
    for (Assignment ass : getAssignments()) {
      if (ass.getResource().equals(res)) {
        ret = ass;
        break;
      }
    }
    return ret;
  }

  public static Task loadByCode(String code) throws PersistenceException {
    return (Task) PersistenceHome.findUnique(Task.class, "code", code);
  }

  public static Task load(Serializable mainObjectId) throws FindByPrimaryKeyException {
    return load(mainObjectId, PersistenceContext.getDefaultPersistenceContext());
  }


  public static Task load(Serializable mainObjectId, PersistenceContext pc) throws FindByPrimaryKeyException {
    return (Task) PersistenceHome.findByPrimaryKey(Task.class, mainObjectId, pc);
  }

  /**
   * @return a map of errors and its value  e.g: TASK_SHOULD_BE_ALREADY_OPEN,1.0    TASK_IS_CLOSING_SOON:0.5
   * <p/>
   * 0= no error -> the key is not put in the map
   * 0.5= warning
   * 1= error
   */
  public Pair<String, Double> checkup(String messagesSeparator) {
    Map<String, Double> doubleMap = fullCheckup();

    double max = 0;
    String message = "";

    for (String s : doubleMap.keySet()) {
      message += I18n.get(s) + JSP.w(messagesSeparator);
      max = max > doubleMap.get(s) ? max : doubleMap.get(s);
    }

    return new Pair<String, Double>(message, max);
  }

  public Map<String, Double> fullCheckup() {
    Map<String, Double> errors = new HashTable();

    if (!isNew() && getSchedule() != null) {

      double startMillis = getSchedule().getStartDate().getTime();
      double endMillis = getSchedule().getEndDate().getTime();
      double now = System.currentTimeMillis();

      double value = 0;


      //------------------------- TASK  SUSPENDED
      if (TaskStatus.STATUS_SUSPENDED.equals(getStatus()) || TaskStatus.STATUS_UNDEFINED.equals(getStatus())) {

        //suspended, but should be open: now>start now<end
        if (now > startMillis && now < endMillis) {
          value = (now - startMillis) / (endMillis - startMillis);
          value = value * (isStartIsMilestone() ? 2 : 1); // in case of milestone gravity is doubled
          value = value > 1 ? 1 : value;
          errors.put(isStartIsMilestone() ? "CK_START_MILESTONE_MISSED" : "CK_SHOULD_BE_ALREADY_OPENED", value);

        }
        //------------------------- TASK  ACTIVE
      } else if (TaskStatus.STATUS_ACTIVE.equals(getStatus())) {

//        //opened before time:  now < start
//        if (now < startMillis) {
//          value = (double) (startMillis - now) / (CompanyCalendar.MILLIS_IN_WEEK);//weighted on a week
//          errors.put("CK_OPENED_BEFORE_START", Math.max(value, 1));
//
//          //should be already closed
//        } else
        if (now > endMillis) {
          value = 0.5 + (double) (now - endMillis) / (CompanyCalendar.MILLIS_IN_WEEK);//weighted on a week
          value = value * (isEndIsMilestone() ? 2 : 1); // in case of milestone gravity is doubled
          value = value > 1 ? 1 : value;
          errors.put(isEndIsMilestone() ? "CK_END_MILESTONE_MISSED" : "CK_SHOULD_BE_CLOSED", Math.max(value, 1));


          // here is the "good" period where to check accurately
        } else {

          //about to close:  90% <now< end
          value = (endMillis - startMillis) * .10;
          if (now > (endMillis - value) && now < endMillis) {
            value = (endMillis - now) / (endMillis - value); // compute % of 90%
            value = 0.5 * value * (isEndIsMilestone() ? 2 : 1); // in case of milestone gravity is doubled
            value = value > 1 ? 1 : value;
            errors.put("CK_ABOUT_TO_CLOSE", value);
          }

          // must compare progress with time spent
          double computedProgress = getProgress() > 100 ? 100 : getProgress();


          // if progres has been not set by hand I need and heuristic
          if (computedProgress <= 0) {
            computedProgress = getGuessedProgress();

          } else {  // my ehuristic is so good that i compare what PM wrote


          }

          //compare progress with time spent
          value = computedProgress / 100 - (now - startMillis) / (endMillis - startMillis);
          if (value > 0.4 && getProgress() > 0 && !isProgressByWorklog()) { // progress is really greater than time spent > 40% only if not wl overflow
            errors.put("CK_PROGRESS_SUSPICIOUSLY_IN_ADVANCE", Math.min(value, 1));  //warning

          } else if (value < -0.2) {
            errors.put("CK_PROGRESS_IN_DELAY", Math.min(-value, 1));  //warning

          }

        }


        //------------------------- TASK  DONE
      } else if (TaskStatus.STATUS_DONE.equals(getStatus())) {

        //closed before starts:  now < start
        if (now < startMillis) {
          errors.put("CK_CLOSED_BEFORE_START", 1.0); //error


          //closed in advance
        } else if (now < endMillis) {
          errors.put("CK_CLOSED_BEFORE_END", 0.3); //warning

        }

      }

      //-----------------  TASK COSTS/BUDGET
      value = getForecasted();
      if (value != 0) {
        value = ((getTotalCostsDone() / value) - 1) * 10; // delta is amplified -> 10% over budget = red
        if (value > 0)
          errors.put("CK_BUDGET_OVERFLOW", Math.min(value, 1)); //warning
      }

      //-----------------  TASK WORKLOG DONE/ESTIMATED
      value = getTotalWorklogEstimated();
      if (value > 0) {
        value = ((getTotalWorklogDone() / value) - 1) * 5; // delta is amplified -> 20% over estimation = red
        if (value > 0)
          errors.put("CK_WORKLOG_OVERFLOW", Math.min(value, 1)); //warning
      }
    }
    return errors;
  }


  @Transient
  private double getGuessedProgress() {
    double guessedProgress = 0;
    double issueClosedPerc = getTotalIssues() <= 0 ? -1 : (double) getTotalIssuesScoreClosed() / (getTotalIssuesScoreOpen() + getTotalIssuesScoreClosed());

    double wlDonePerc = getTotalWorklogEstimated() <= 0 ? -1 : (double) (getTotalWorklogDone()) / (getTotalWorklogEstimated());
    wlDonePerc = Math.min(wlDonePerc, 1);

    if (wlDonePerc < 0 && issueClosedPerc < 0) {
      /*double startMillis = getSchedule().getStartDate().getTime();
      double endMillis = getSchedule().getEndDate().getTime();
      double now = System.currentTimeMillis();
      guessedProgress = (double) (now - startMillis) / (endMillis - startMillis);*/
      guessedProgress = 0;
    } else if (wlDonePerc < 0) {
      guessedProgress = issueClosedPerc;
    } else if (issueClosedPerc < 0) {
      guessedProgress = wlDonePerc;
    } else
      guessedProgress = (wlDonePerc + issueClosedPerc) / 2;

    return guessedProgress * 100;
  }


  @Transient
  /**
   *  obj[0] integer dateInt -> 20111227
   *  obj[0] long  millis of work for that day
   */
  public List<Object[]> getWorkHeartBeat(boolean includeChild) {

    // calculate workdone
    String hql = "select year(wklg.inserted)*10000+month(wklg.inserted)*100+day(wklg.inserted), sum(wklg.duration) from " + Worklog.class.getName() + " as wklg where ";

    hql += "wklg.assig.task = :tsk ";

    if (includeChild)
      hql += " or wklg.assig.task.ancestorIds like :ancIds ";

    hql += "group by year(wklg.inserted)*10000+month(wklg.inserted)*100+day(wklg.inserted) ";
    hql += "order by year(wklg.inserted)*10000+month(wklg.inserted)*100+day(wklg.inserted) ";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setEntity("tsk", this);
    if (includeChild)
      oql.getQuery().setString("ancIds", getChildAncentorIds() + "%");

    return oql.getQuery().list();
  }

  /**
   * key integer dateInt -> 20111227
   * value integer  balance of issues: created-closed
   */
  public TreeMap<Integer, Integer> getIssueHeartBeat(boolean includeChild) {

    // calculate issueCreated
    String hql = "select year(issue.creationDate)*10000+month(issue.creationDate)*100+day(issue.creationDate), count(issue.id) from " + Issue.class.getName() + " as issue where ";

    hql += "issue.task = :tsk ";

    if (includeChild)
      hql += " or issue.task.ancestorIds like :ancIds ";

    hql += "group by year(issue.creationDate)*10000+month(issue.creationDate)*100+day(issue.creationDate) ";
    hql += "order by year(issue.creationDate)*10000+month(issue.creationDate)*100+day(issue.creationDate) ";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setEntity("tsk", this);
    if (includeChild)
      oql.getQuery().setString("ancIds", getChildAncentorIds() + "%");

    TreeMap<Integer, Integer> result = new TreeMap();
    for (Object os : oql.getQuery().list()) {
      int dateInt = (Integer) ((Object[]) os)[0];
      int quant = ((Long) ((Object[]) os)[1]).intValue();
      result.put(dateInt, quant);
    }

    // calculate issueClosed
    hql = "select year(issue.lastStatusChangeDate)*10000+month(issue.lastStatusChangeDate)*100+day(issue.lastStatusChangeDate), count(issue.id) from " + Issue.class.getName() + " as issue where ";

    hql += "(issue.task = :tsk ";

    if (includeChild)
      hql += "or issue.task.ancestorIds like :ancIds ";

    hql += ") ";

    hql += "and issue.lastStatusChangeDate is not null and issue.status.behavesAsClosed=true ";


    hql += "group by year(issue.lastStatusChangeDate)*10000+month(issue.lastStatusChangeDate)*100+day(issue.lastStatusChangeDate) ";
    hql += "order by year(issue.lastStatusChangeDate)*10000+month(issue.lastStatusChangeDate)*100+day(issue.lastStatusChangeDate)";
    oql = new OqlQuery(hql);
    oql.getQuery().setEntity("tsk", this);
    if (includeChild)
      oql.getQuery().setString("ancIds", getChildAncentorIds() + "%");


    for (Object os : oql.getQuery().list()) {
      int dateInt = (Integer) ((Object[]) os)[0];
      int quant = ((Long) ((Object[]) os)[1]).intValue();
      if (result.containsKey(dateInt))
        result.put(dateInt, result.get(dateInt) - quant);
      else
        result.put(dateInt, -quant);
    }

    return result;
  }

  public static Task loadFromKey(String key) throws FindByPrimaryKeyException {
    Task ret = null;
    String[] ss = (key + "").split("-");
    if (ss.length >= 2) {
      int id = Integer.parseInt(ss[0], Character.MAX_RADIX);
      Task load = Task.load(id + "");
      if (load != null && load.getKey().equalsIgnoreCase(key))
        ret = load;
    }
    return ret;
  }

  @Transient
  public String getKey() {
    String s = Integer.toString(getIntId(), Character.MAX_RADIX);
    return (s + "-" + StringUtilities.shaEncode(s, "z10b3lv4cc14" + getCreationDate().getTime())).toUpperCase();
  }


  public JSONObject jsonify() {
    return jsonify(false,null);
  }


  public JSONObject jsonify(boolean fullLoading, RestState pageState) {
    JSONObject ret = super.jsonify();
    ret.element("loadComplete", false);

    ret.element("id", getId());
    ret.element("status", getStatus());
    ret.element("statusColor", getStatusColor());
    ret.element("code", getCode());
    ret.element("name", getName());
    ret.element("description", getDescription());
    ret.element("tags", getTags());
    ret.element("notes", getNotes());
    ret.element("progress", getProgress());
    ret.element("color", getTaskColor());
    ret.element("progressByWorklog", isProgressByWorklog());
    ret.element("relevance", getRelevance());
    ret.element("typeId", getType() == null ? null : getType().getId());
    ret.element("type", getType() == null ? "" : getType().getDescription());
    ret.element("typeCode", getType() == null ? "" : getType().getStringValue());
    if (getSchedule() != null) {
      ret.element("startDate", JSP.w(getSchedule().getStartDate()));
      ret.element("endDate", JSP.w(getSchedule().getEndDate()));
      ret.element("start", getSchedule().getStartDate().getTime());
      ret.element("end", getSchedule().getEndDate().getTime());
    } else {
      ret.element("startDate", JSP.w(new Date()));
      Date fakeEnd = new Date(System.currentTimeMillis() + ((getDuration() == 0 ? 1 : getDuration()) * CompanyCalendar.MILLIS_IN_DAY));
      ret.element("endDate", JSP.w(fakeEnd));
      ret.element("start", System.currentTimeMillis());
      ret.element("end", fakeEnd.getTime());
    }
    ret.element("duration", (getDuration() == 0 ? 1 : getDuration())); //in days
    ret.element("startIsMilestone", isStartIsMilestone());
    ret.element("endIsMilestone", isEndIsMilestone());

    ret.element("totalWorklog", getTotalWorklogDone());
    ret.element("totalEstimated", getTotalWorklogEstimated());
    ret.element("totalEstimatedFromIssues", getTotalEstimatedFromIssues());

    ret.element("totalIssues", getTotalIssues());
    ret.element("openIssues", getTotalIssuesOpen());


    ret.element("lastModified", getLastModified() == null ? System.currentTimeMillis() : getLastModified().getTime());
    ret.element("lastModifier", getLastModifier());

    if (getParent() != null)
      ret.element("parentId", getParent().getId());

    if (fullLoading) {
      ret.element("loadComplete", true);
      List<Task> children = getChildrenSorted();
      if (JSP.ex(children)) {
        JSONArray jsa = new JSONArray();
        for (Task t : children) {
          JSONObject chld = new JSONObject();
          chld.element("id", t.getId());
          chld.element("name", t.getDisplayName());
          chld.element("status", t.getStatus());
          chld.element("statusColor", t.getStatusColor());
          jsa.add(chld);
        }
        ret.element("children", jsa);
      }

      JSONArray jsa = new JSONArray();
      for (Assignment ass : getAssignments())
        jsa.add(ass.jsonify());
      ret.element("assignments", jsa);
    }


    if (pageState!=null) {
      Operator logged = pageState.getLoggedOperator();
      if (hasPermissionFor(logged, TeamworkPermissions.task_cost_canRead)) {
        ret.element("budget", getForecasted());
        ret.element("totalCosts", getTotalCostsDone());
      }

      if (hasPermissionFor(logged, TeamworkPermissions.document_canRead)) {
        Set<TeamworkDocument> docs = getDocuments();
        if (JSP.ex(docs)) {
          JSONArray jsa = new JSONArray();
          for (TeamworkDocument td : docs)
            jsa.add(td.jsonify((PageState) pageState));
          ret.element("documents", jsa);
        }
      }
    }

    return ret;
  }


  /**
   * @return Pair< totalDescendant, totalDescendantClosed>
   */
  @Transient
  public Pair<Integer, Integer> getTotalDescendantsSize() {
    Pair<Integer, Integer> ret = new Pair<Integer, Integer>(0, 0);
    try {
      ret.first = getDescendantsSize(Task.class);
      if (ret.first > 0) {
        String hql = "select count(task.id) from " + Task.class.getName() + " as task where task.ancestorIds like (:descs) ";
        hql = hql + " and (task.status = :stdone)";
        OqlQuery oql = new OqlQuery(hql);
        oql.getQuery().setString("stdone", TaskStatus.STATUS_DONE);
        oql.getQuery().setString("descs", getChildAncentorIds() + "%");
        ret.second = ((Long) oql.uniqueResult()).intValue();
      }
    } catch (FindException e) {
      throw new PlatformRuntimeException(e);
    }
    return ret;

  }


  @Transient
  public TaskDataHistory getCurrentData() {
    TaskDataHistory tdh = new TaskDataHistory();
    tdh.setCreatedOn(getLastModified());  // si mette la data di ultima modifica del task per essere più precisi
    tdh.setTaskId(getId() + "");
    tdh.setStartDate(getSchedule().getStartDate());
    tdh.setEndDate(getSchedule().getEndDate());
    tdh.setDuration(getDuration());
    tdh.setStatus(getStatus());
    tdh.setProgress(getProgress());

    Set<Person> team = getPersons();
    tdh.setTeamSize(team == null ? 0 : team.size());

    Set documents = getDocuments();
    tdh.setTotalDocuments(documents == null ? 0 : documents.size());

    Pair<Integer, Integer> totalDescendantsSize = getTotalDescendantsSize();
    tdh.setTotalDescendant(totalDescendantsSize.first);
    tdh.setTotalDescendantClosed(totalDescendantsSize.second);

    tdh.setTotalWorklogDone(getTotalWorklogDone());
    tdh.setTotalWorklogEstimated(getTotalWorklogEstimated());
    tdh.setForecasted(getForecasted());
    tdh.setTotalCostsDone(getTotalCostsDone());
    tdh.setTotalCostsEstimated(getTotalCostsEstimated());
    tdh.setTotalEstimatedFromIssues(getTotalEstimatedFromIssues());

    tdh.setTotalIssues(getTotalIssues());
    tdh.setTotalIssuesOpen(getTotalIssuesOpen());
    tdh.setTotalIssuesScoreOpen(getTotalIssuesScoreOpen());
    tdh.setTotalIssuesScoreClosed(getTotalIssuesScoreClosed());
    return tdh;
  }

  @Transient
  public List<TaskDataHistory> getDataHistory() throws PersistenceException {
    String hql = "select tdh from " + TaskDataHistory.class.getName() + " as tdh where taskId =:taskId order by createdOn ";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setString("taskId", this.getId() + "");
    return oql.list();
  }


  @Transient
  public RoleTeamwork getCustomerRole() {
    return TaskBricks.getCustomerRole(this.getArea());
  }

  @Transient
  public Resource getCustomer(){
    Resource ret=null;
    String customerRoleName= ApplicationState.getApplicationSetting("DEFAULT_CUSTOMER_ROLE_NAME", "Customer");
    for (Assignment ass : getAssignments()) {
      if (customerRoleName.equalsIgnoreCase(ass.getRole().getName())) {
        ret=ass.getResource();
        break;
      }
    }

    if (ret==null && getParent()!=null){
      ret=getParent().getCustomer();
    }
    return ret;
  }


  public void recode(String newCode) throws StoreException, ApplicationException {
    setCode(newCode);
    // test code uniqueness
    boolean mustBeUnique = Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("USEUNIQUECODES")) && JSP.ex(newCode);
    if (mustBeUnique && !isUnique("code"))
      throw new ApplicationException("KEY_MUST_BE_UNIQUE");
    store();
    boolean ret=true;
    int i=1;
    for (Task child:getChildrenSorted()){
      child.recode(newCode + "." + JSP.paddTo(i,"00"));
      i++;
    }
  }


  public boolean scheduleIsIdentical(Date newStart, Date newEnd, int newDuration) {
    boolean sameDur = true;
    boolean sameStart = true;
    boolean sameEnd = true;

    CompanyCalendar cc = new CompanyCalendar();
    //milliseconds are bastardic
    cc.setTime(newEnd);
    Date endToEndOfDay = cc.setAndGetTimeToDayEnd();
    cc.setTime(newStart);
    Date startToStartOfDay = cc.setAndGetTimeToDayStart();
    sameDur = newDuration == getDuration();
    sameStart = getSchedule() != null && startToStartOfDay.equals(getSchedule().getStartDate());
    sameEnd = getSchedule() != null && (endToEndOfDay.getTime() / CompanyCalendar.MILLIS_IN_DAY) == (getSchedule().getEndDate().getTime() / CompanyCalendar.MILLIS_IN_DAY);
    return sameDur && sameStart && sameEnd;
  }
	public Set<TaskAudit> getAudits() {
		return audits;
	}

	public void setAudits(Set<TaskAudit> audits) {
		this.audits = audits;
	}

}
