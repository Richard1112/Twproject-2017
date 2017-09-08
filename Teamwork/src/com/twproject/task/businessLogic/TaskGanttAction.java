package com.twproject.task.businessLogic;

import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Person;
import com.twproject.resource.Resource;
import com.twproject.resource.ResourceBricks;
import com.twproject.security.RoleTeamwork;
import com.twproject.security.SecurityBricks;
import com.twproject.security.TeamworkPermissions;
import com.twproject.task.*;
import com.twproject.utilities.TeamworkComparators;
import net.sf.json.JSONArray;
import net.sf.json.JSONObject;
import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.messaging.Listener;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.messaging.SomethingHappened;
import org.jblooming.ontology.Pair;
import org.jblooming.ontology.SerializedMap;
import org.jblooming.ontology.businessLogic.DeleteHelper;
import org.jblooming.oql.OqlQuery;
import org.jblooming.oql.QueryHelper;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.security.*;
import org.jblooming.security.SecurityException;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.HashTable;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.constants.ObjectEditorConstants;
import org.jblooming.waf.constants.OperatorConstants;
import org.jblooming.waf.html.input.SmartCombo;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.ClientEntries;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.RestState;

import java.io.Serializable;
import java.util.*;

/**
 * Created by rbicchierai on 23/01/2015.
 */
public class TaskGanttAction extends TaskAction {

  Map<Serializable, String> resourceDecoder=null;
  Map<Serializable, RoleTeamwork> roleDecoder=null;


  public TaskGanttAction(RestState pageState) {
    super(pageState);
  }

  // -------------------------------------------------  SAVE FROM GANTT JSON -------------------------------------------------------------------------------------
  public void cmdSaveGantt(JSONObject jsonReturn, boolean createNew) throws Exception {
    restState.initializeEntries("cell");
    JSONObject jPrj = JSONObject.fromObject(restState.getEntry("prj").stringValueNullIfEmpty());
    if (createNew)
      _cleanProjectIds(jPrj);
    _cmdSaveGantt(jPrj, jsonReturn);
  }

  // -------------------------------------------------  IMPORT FROM TASK LIST JSON EXPORT -------------------------------------------------------------------------------------
  public void cmdImportProjectList(JSONObject jsonReturn) throws Exception {
    JSONObject jPrj = JSONObject.fromObject(restState.getEntry("prj").stringValueNullIfEmpty());

    Task task = Task.load(restState.getEntry("taskId").intValueNoErrorCodeNoExc() + "");
    if (task != null) {
      task.testPermission(logged, TeamworkPermissions.task_canCreate);
    } else {
      logged.testPermission(TeamworkPermissions.project_canCreate);
    }

    String rootName = restState.getEntry("filename").stringValueNullIfEmpty();
    rootName = JSP.ex(rootName) ? rootName : "Imported from json file on: " + DateUtilities.dateAndHourToString(new Date());

    int rootsFound = 0;
    JSONArray tasks = null;
    JSONObject prj = null;

    tasks = jPrj.getJSONArray("tasks");

    //there are task?
    if (tasks.size() > 0) {

      //check if multiple roots
      for (int i = 0; i < tasks.size(); i++) {
        JSONObject t = tasks.getJSONObject(i);
        rootsFound += t.getInt("level") == 0 ? 1 : 0;
      }

      //in case of multiple roots or importing under an axisting task
      // we will add the task (eventually new) on top and we will shift all dependencies and levels
      boolean shift = false;
      if (rootsFound > 1 && task == null) {
        // create a new task and shift deps and levels
        JSONObject jt = new JSONObject();

        jt.element("id", -9999);
        jt.element("code", "");
        jt.element("level", 0);
        jt.element("name", rootName);
        jt.element("status", TaskStatus.STATUS_ACTIVE);
        jt.element("startIsMilestone", false);
        jt.element("endIsMilestone", false);

        //copy date from first task
        JSONObject ft = tasks.getJSONObject(0);
        jt.element("start", ft.getLong("start"));
        jt.element("end", ft.getLong("end"));
        jt.element("duration", ft.getInt("duration"));


        tasks.add(0, jt);

        shift = true;
      } else if (task != null) {
        //add the task on top and shift
        JSONObject jt = task.jsonify(false, restState);
        jt.element("level", 0);
        tasks.add(0, jt);
        shift = true;
      }


      if (shift) {
        for (int i = 1; i < tasks.size(); i++) {
          JSONObject jt = tasks.getJSONObject(i);
          jt.element("level", jt.getInt("level") + 1); // increment level

          if (jt.has("depends")) {
            String deps = jt.getString("depends");
            if (JSP.ex(deps)) {
              String[] depList = StringUtilities.splitToArray(deps, ",");
              for (int j = 0; j < depList.length; j++) {
                depList[j] = (Integer.parseInt(depList[j]) + 1) + "";
              }
              jt.element("depends", StringUtilities.arrayToString(depList, ","));
            }
          }
        }
      }

    } else {
      throw new Exception("NO_TASKS_DEFINED");
    }


    //si razzano gli id
    _cleanProjectIds(jPrj);

    //e si chiama il save standard
    _cmdSaveGantt(jPrj, jsonReturn);
  }


  // -------------------------------------------------  SAVE FROM IMPORT DA gantt.twproject.com -------------------------------------------------------------------------------------
  public void cmdSaveMultiGantt(JSONObject jsonReturn, boolean createNew) throws Exception {
    JSONArray prjs = JSONArray.fromObject(restState.getEntry("prjs").stringValueNullIfEmpty());
    jsonReturn.element("projects", new JSONArray()); // si prepara l'array per la risposta

    for (JSONObject jPrj : (List<JSONObject>) prjs) {
      if (createNew)
        _cleanProjectIds(jPrj);
      _cmdSaveGantt(jPrj, jsonReturn);
      jsonReturn.getJSONArray("projects").add(jsonReturn.getJSONObject("project"));
      jsonReturn.remove("project");
    }
  }

  // -------------------------------------------------  SAVE FROM TASK lIST AS GANTT JSON -------------------------------------------------------------------------------------
  public void cmdSaveTaskList(JSONObject jsonReturn) throws Exception {
    restState.initializeEntries("cell");
    JSONObject taskListAsProject = JSONObject.fromObject(restState.getEntry("taskList").stringValueNullIfEmpty());
    _cmdSaveTaskList(taskListAsProject, jsonReturn);
  }


  //--------------------------------  REMOVE IDS IN ORDER TO FORCE PROJECT/RESOURCES/ASSIG CREATION -------------------------------------
  private void _cleanProjectIds(JSONObject jPrj) throws FindByPrimaryKeyException {

    JSONArray jTasks = jPrj.has("tasks") ? jPrj.getJSONArray("tasks") : new JSONArray();
    JSONArray jResources = jPrj.has("resources") ? jPrj.getJSONArray("resources") : new JSONArray();

    Map<String, String> resourceOldNew = new HashTable();
    Map<String, String> roleOldNew = new HashTable();

    //loop task
    for (Object o : jTasks) {
      JSONObject jTask = (JSONObject) o;
      //reset id
      jTask.element("id", "tmp_" + jTask.getString("id"));


      JSONArray jAssigs = jTask.has("assigs") ? jTask.getJSONArray("assigs") : new JSONArray();

      //loop assegnazioni
      for (Object a : jAssigs) {
        JSONObject jAssig = (JSONObject) a;
        //reset id
        jAssig.element("id", "tmp_" + jAssig.getString("id"));
        String resId = jAssig.getString("resourceId");


        //controllo se la risorsa è già stata decodificata
        if (!resourceOldNew.containsKey(resId)) {
          String resourceNameFromJson = null;
          JSONObject jRes = null;
          for (Object r : jResources) {
            jRes = (JSONObject) r;
            if (resId.equals(jRes.getString("id"))) {
              resourceNameFromJson = jRes.getString("name");
              break;
            }
          }

          //se la risorsa non c'è sul db oppure ha un'altro nome devo invalidare
          Resource localResource = Resource.load(resId);
          if (localResource == null || !localResource.getName().equals(resourceNameFromJson)) {
            jRes.element("id", "tmp_" + resId);
          }
          resourceOldNew.put(resId, jRes.getString("id"));

        }

        jAssig.element("resourceId", resourceOldNew.get(resId));

      }

      //i ruoli vanno a posto da soli

    }
    //i ruoli vanno a posto da soli


  }


  //-------------------------------------------------  NEW PROJECT JSON -------------------------------------------------------------------------------------
  public void cmdNewProject(RestState pageState, JSONObject json) throws PersistenceException {
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    JSONObject jProject = new JSONObject();


    //si passa l'offset del server
    jProject.element("serverTimeOffset", SessionState.getTimeZone().getOffset(System.currentTimeMillis()));


    //get permissions
    boolean globalCanWrite = logged.hasPermissionFor(TeamworkPermissions.project_canCreate); // stiamo creando un nuovo progetto -> quindi project_canCreate
    jProject.element("cannotCloseTaskIfIssueOpen", I18n.isActive("CUSTOM_FEATURE_DO_NOT_CLOSE_TASK_IF_OPEN_ISSUES"));
    jProject.element("canWriteOnParent", true);

    Map<String, Resource> existingResources = new HashTable();
    Map<String, Role> existingRoles = new HashTable();

    jProject.element("tasks", new JSONArray());


    //add resources
    _ganttAddResources(pageState, null, jProject, existingResources);

    //add roles
    _ganttAddRoles(pageState, null, jProject, existingRoles);

    jProject.element("canWrite", globalCanWrite);
    json.element("project", jProject);

  }


  //-------------------------------------------------  LOAD PROJECT JSON -------------------------------------------------------------------------------------
  public void cmdLoadProject(String taskId, RestState pageState, JSONObject json, boolean canAlwaysRead) throws PersistenceException {
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    Task task = Task.load(taskId);
    JSONObject jProject = new JSONObject();


    if (task != null) {
      if (canAlwaysRead || task.hasPermissionFor(logged, TeamworkPermissions.task_canRead)) {
        Task parent = task.getParent();

        //si passa l'offset del server
        jProject.element("serverTimeOffset", SessionState.getTimeZone().getOffset(System.currentTimeMillis()));


        // --------------------------- GET PERMISSIONS --------------------
        // sul gantt per avere il write devi anche avere il change status. Troppo complicato separare i due permessi a causa delle dipendenze
        boolean globalCanWrite = task.hasPermissionFor(logged, TeamworkPermissions.task_canWrite) && task.hasPermissionFor(logged, TeamworkPermissions.task_canChangeStatus);
        boolean globalCanDelete = task.hasPermissionFor(logged, TeamworkPermissions.task_canDelete);
        jProject.element("cannotCloseTaskIfIssueOpen", I18n.isActive("CUSTOM_FEATURE_DO_NOT_CLOSE_TASK_IF_OPEN_ISSUES"));
        jProject.element("canWriteOnParent", parent == null ? logged.hasPermissionFor(TeamworkPermissions.task_canWrite) : parent.hasPermissionFor(logged, TeamworkPermissions.task_canWrite));
        jProject.element("canAddIssue", task.hasPermissionFor(logged, TeamworkPermissions.issue_canCreate));

        if (parent != null) {
          jProject.element("minEditableDate", parent.getSchedule().getStartDate().getTime());
          jProject.element("maxEditableDate", parent.getSchedule().getEndDate().getTime());
        }

        //notification and note input are generated only if task is not new and it is created 6 hours before in order to not annoy users
        jProject.element("notifyStatusAndDateChanges", System.currentTimeMillis()-task.getCreationDate().getTime()> CompanyCalendar.MILLIS_IN_6_HOUR);

        Map<String, Resource> existingResources = new HashTable();
        Map<String, Role> existingRoles = new HashTable();

        //get descendant correctly sorted
        List<Task> tasks = task.getDescendants(new TeamworkComparators.TaskManualOrderComparator());

        //add "root" on top
        tasks.add(0, task);

        JSONArray jTasks = new JSONArray();

        int topLevel = task.getDepth();  // the top depth is set as 0
        for (Task t : tasks) {
          JSONObject jTask = t.jsonify(false, pageState);

          //add depth
          jTask.element("level", t.getDepth() - topLevel);

          //add dependencies
          String depString = "";

          for (TaskDependency td : t.getPreviouses()) {
            Task superior = td.getDepends();
            //retrieve superior index in list
            int pos = tasks.indexOf(superior);
            if (pos >= 0) {
              depString = depString + (depString.length() == 0 ? "" : ",") + (pos + 1) + (td.getLag() == 0 ? "" : ":" + td.getLag()); //dependencies are 1 based
            } else {
              jTask.element("hasExternalDep", true);
              //throw new Exception("Depends from a task out of scope.");
            }
          }

          for (TaskDependency td : t.getNexts()) {
            Task inferior = td.getTask();
            //retrieve inferior index in list
            int pos = tasks.indexOf(inferior);
            if (pos < 0) {
              jTask.element("hasExternalDep", true);
            }
          }

          jTask.element("depends", depString);
          jTask.element("canAddIssue", t.hasPermissionFor(logged, TeamworkPermissions.issue_canCreate));


          //add assignments
          JSONArray assigs = new JSONArray();
          for (Assignment ass : t.getAssignementsSortedByRole()) {
            JSONObject jAss = new JSONObject();
            jAss.element("id", ass.getId());
            jAss.element("roleId", ass.getRole().getId());
            jAss.element("resourceId", ass.getResource().getId());
            jAss.element("effort", ass.getEstimatedWorklog());
            assigs.add(jAss);

            existingResources.put(ass.getResource().getId() + "", ass.getResource());
            existingRoles.put(ass.getRole().getId() + "", ass.getRole());

          }
          jTask.element("assigs", assigs);

          //canWrite
          boolean canWrite = !t.isProcessDriven() && t.hasPermissionFor(logged, TeamworkPermissions.task_canWrite) && t.hasPermissionFor(logged, TeamworkPermissions.task_canChangeStatus);
          boolean canDelete = t.hasPermissionFor(logged, TeamworkPermissions.task_canDelete);
          globalCanWrite = globalCanWrite || canWrite;
          globalCanDelete = globalCanDelete || canDelete;
          jTask.element("canWrite", canWrite);
          jTask.element("canDelete", canDelete);

          jTasks.add(jTask);
        }

        jProject.element("tasks", jTasks);

        //add resources
        _ganttAddResources(pageState, task, jProject, existingResources);

        //add roles
        _ganttAddRoles(pageState, task, jProject, existingRoles);

        jProject.element("canWrite", globalCanWrite);
        jProject.element("canDelete", globalCanDelete);
        json.element("project", jProject);
      }
    }
  }

  void _ganttAddRoles(RestState pageState, Task task, JSONObject jProject, Map<String, Role> existingRoles) throws PersistenceException {
    JSONArray jRoles = new JSONArray();
    for (Role r : existingRoles.values()) {
      JSONObject jRes = new JSONObject();
      jRes.element("id", r.getId());
      jRes.element("name", r.getName());
      jRoles.add(jRes);
    }

    Set<Permission> permissionsRequired = new HashSet();
    //permissionsRequired.add(TeamworkPermissions.assignment_manage);
    permissionsRequired.add(TeamworkPermissions.assignment_canCRW);

    String hql = "select r from " + RoleTeamwork.class.getName() + " as r where r.area in (:areas) and r.localToAssignment=true order by r.name";
    Set<Area> areas = new HashSet();
    for (Permission permissionRequired : permissionsRequired) {
      Set<Area> areasForPerm = logged.getAreasForPermission(permissionRequired);
      if (areasForPerm != null && areasForPerm.size() > 0)
        areas.addAll(areasForPerm);

      //this is used if you have only task_create (a kickstarter)
      if (task != null && task.hasPermissionFor(logged, permissionRequired))
        areas.add(task.getArea());
    }

    if (task != null) {
      Person myPerson = logged.getPerson();
      if (myPerson != null) {
        Set<Assignment> assigs = task.getHierarchyAssignments();
        for (Assignment assignment : assigs) {
          if (myPerson.equals(assignment.getResource())) {
            for (Permission permissionRequiredOnArea : permissionsRequired) {
              if (assignment.getRole().hasPermissionFor(permissionRequiredOnArea)) {
                Area area = assignment.getRole().getArea();
                if (area != null)
                  areas.add(area);
              }
            }
          }
        }
      }
    }

    if (JSP.ex(areas)) {
      QueryHelper queryHelper = new QueryHelper(hql);
      OqlQuery query = new OqlQuery(hql);
      query.getQuery().setParameterList("areas", areas);
      List<Role> roles = query.list();
      for (Role r : roles) {
        if (!existingRoles.containsKey(r.getId() + "")) {
          JSONObject jRol = new JSONObject();
          jRol.element("id", r.getId());
          jRol.element("name", r.getName());
          jRoles.add(jRol);
        }
      }
    }


    /*
    //load roles with a skifid trick using a smart combo to get query
    mucca = SecurityBricks.getRoleComboForAssignments("ecchissene", task, false, pageState);
    List<Object[]> roles = mucca.fillResultList("", "");

    for (Object[] res : roles) {
      if (!existingRoles.containsKey(res[0] + "")) {
        JSONObject jRol = new JSONObject();
        jRol.element("id", res[0]);
        jRol.element("name", res[1]);

        jRoles.add(jRol);
      }
    }
    */
    jProject.element("roles", jRoles);
  }

  /**
   * fill resources with all assignable using the smart combo
   *
   * @throws PersistenceException
   */
  private void _ganttAddResources(RestState pageState, Task task, JSONObject jProject, Map<String, Resource> existingResources) throws PersistenceException {
    JSONArray jRess = new JSONArray();
    for (Resource r : existingResources.values()) {
      JSONObject jRes = new JSONObject();
      jRes.element("id", r.getId());
      jRes.element("name", r.getName());
      jRess.add(jRes);
    }

    Collections.sort(jRess, new Comparator<JSONObject>() {
      @Override
      public int compare(JSONObject o1, JSONObject o2) {
        return o1.getString("name").trim().compareToIgnoreCase(o2.getString("name").trim());
      }
    });

    jProject.element("resources", jRess);
  }


  private void _cmdSaveGantt(JSONObject jsonGanttData, JSONObject jsonReturn) throws Exception {

    //this is the default area for create new root and resources etc.
    Area area = logged.getPerson().getArea();

    //remove deleted tasks
    _ganttRemoveTasks(jsonGanttData);

    Map<Integer, Task> lastParents = new HashTable();

    //mapppa livello,ultimo contatore
    Map<Integer, Integer> levelLastNum = new HashTable<Integer, Integer>();

    boolean rootScheduleChanged = false;

    JSONArray jTasks = jsonGanttData.getJSONArray("tasks");
    List<Task> tasks = new ArrayList();

    String changesReasonWhy = I18n.get("CHANGED_FROM_GANTT_EDITOR");
    if (jsonGanttData.has("changesReasonWhy"))
      changesReasonWhy=jsonGanttData.getString("changesReasonWhy");

    Task rootTask=null;

    //first round: (eventually) create new tasks, set code, name, parent, progress and area
    for (Object o : jTasks) {
      JSONObject jsonTask = (JSONObject) o;

      String id = jsonTask.getString("id");
      int level = jsonTask.getInt("level");  //quando arrivano dalla lista di task i livelli sono tutti a 0
      String code = JSP.w(jsonTask.get("code"));

      Task parent = lastParents.get(level - 1);

      //is it a new task?
      Task task = Task.load(id);

      if (level==0)
        rootTask=task;

      Task[] taskHolder = new Task[]{task};  //serve per far passare il task alla funzione
      //si salva o crea il task corrente (una riga)
      boolean datesChanged=false;

      //se il task è stato cambiato
      boolean taskIsChanged = !(jsonTask.has("unchanged") && jsonTask.getBoolean("unchanged"));
      if (taskIsChanged) {
        datesChanged = _ganttSaveSingleTask(jsonTask, level, code, parent, taskHolder, changesReasonWhy,true);
        task = taskHolder[0];
        rootScheduleChanged = rootScheduleChanged || level == 0 && datesChanged;
      }

      //add task in list
      tasks.add(task);


      //set last task used for the level
      lastParents.put(level, task);

      // si setta l'order factor
      _ganttSetOrderFactor(levelLastNum, level, task);

      //add, remove, update assignemnts
      _ganttManageAssignment(jsonGanttData, area, jsonTask, task);

    }

    ArrayList<Pair<String, String[]>> statusErrorMessages = new ArrayList<Pair<String, String[]>>();


    //second round: set dependencies, statuses, and propagate
    for (int i = 0; i < jTasks.size(); i++) {
      JSONObject jsonTask = (JSONObject) jTasks.get(i);
      boolean taskIsChanged = !(jsonTask.has("unchanged") && jsonTask.getBoolean("unchanged"));
      if (taskIsChanged) {
        Task task = tasks.get(i);

        //elimina e ricrea le dipendenze sulla base della posizione delle righe. Funziona solo per un singolo progetto alla volta
        _ganttChangeDependenciesAndStatus(tasks, statusErrorMessages, jsonTask, task, changesReasonWhy);
      }
    }


    //force a propagation outside gannt scope if top task is not a root
    //Set<Task> chts = new HashSet();
    if (tasks.size() > 0) {
      Task top = tasks.get(0);

      if (top.getParent() != null) {
        if (rootScheduleChanged) {
          //propagate dates
          Hashtable<Task, TaskScheduleCandidate> taskCandidates = new Hashtable<Task, TaskScheduleCandidate>();
          StringBuffer log = new StringBuffer();
          boolean scheduleOK = Task.analyzeScheduleChangesRun(top.getSchedule().getStartDate(), top.getDuration(), top.getSchedule().getEndDate(), top, log, logged, taskCandidates);

          if (scheduleOK) {
            for (TaskScheduleCandidate tsc : taskCandidates.values()) {
              tsc.taskScheduleHistory = tsc.task.changeSchedule(tsc.start, tsc.duration, tsc.end, changesReasonWhy, logged);
            }
          } else {
            if (JSP.ex(log.toString()))
              throw new Exception(I18n.get("CHANGE_SCHEDULE_LOG") + ": " + log.toString());
          }
        }
      }
    }


    if (statusErrorMessages.size() == 0) {

      //save all happenings -> events
      _ganttNotifyEvents(tasks);

      //perform a checkpoint
      PersistenceContext.getDefaultPersistenceContext().checkPoint(); // todo ma a cosa serve? non lo dovrebbe fare fcf? lo facciamo per avere un load tranquillo?

      restState.addMessageOK(I18n.get("GANTT_SAVED_CORRECTLY"));

      //reload the whole project
      if (tasks.size() > 0) {
        cmdLoadProject(tasks.get(0).getId() + "", restState, jsonReturn, false);
      }

    } else {
      // statusErrorMessages su messageFromController
      for (Pair<String, String[]> er : statusErrorMessages) {
        restState.addMessageError(I18n.get(er.first, er.second));
      }

      jsonReturn.element("ok", false);
    }
  }


  private void _cmdSaveTaskList(JSONObject taskListAsProject, JSONObject jsonReturn) throws Exception {

    //this is the default area for create new root and resources etc.
    Area area = logged.getPerson().getArea();

    //remove deleted tasks
    _ganttRemoveTasks(taskListAsProject);

    JSONArray jTasks = taskListAsProject.getJSONArray("tasks");
    List<Task> tasks = new ArrayList();

    String changesReasonWhy = I18n.get("CHANGED_FROM_GANTT_EDITOR");
    if (taskListAsProject.has("changesReasonWhy"))
      changesReasonWhy=taskListAsProject.getString("changesReasonWhy");


    //first round: (eventually) create new tasks, set code, name, parent, progress and area
    for (Object o : jTasks) {
      JSONObject jsonTask = (JSONObject) o;

      String id = jsonTask.getString("id");
      String code = JSP.w(jsonTask.get("code"));

      Task task = Task.load(id);

      //is it a new task? dalla taskListAsGantt per il momento non si creano mai nuovi task.
      // Questo impedisce di avere delle cancellazioni alle righe precedenti che possono coinvolgere figli alle righe successive
      //todo si potrebbe controllare id = tmp_.....
      if (task == null)
        continue;


      //se il task è stato cambiato
      boolean taskIsChanged = !(jsonTask.has("unchanged") && jsonTask.getBoolean("unchanged"));
      if (taskIsChanged) {
        Task parent = task != null ? task.getParent() : null;

        Task[] taskHolder = new Task[]{task};
        //si salva o crea il task corrente (una riga) ad eccezione delle date
        _ganttSaveSingleTask(jsonTask, 0, code, parent, taskHolder, changesReasonWhy, false);
        task = taskHolder[0];
      }
      //add task in list
      tasks.add(task);

      //add, remove, update assignemnts
      _ganttManageAssignment(taskListAsProject, area, jsonTask, task);

    }

    ArrayList<Pair<String, String[]>> statusErrorMessages = new ArrayList<Pair<String, String[]>>();


    //second round: set statuses and propagate
    for (int i = 0; i < jTasks.size(); i++) {
      JSONObject jsonTask = (JSONObject) jTasks.get(i);

      boolean taskIsChanged = !(jsonTask.has("unchanged") && jsonTask.getBoolean("unchanged"));
      if (taskIsChanged) {
        Task task = tasks.get(i);

        //set statuses and propagate. N.B.: qui le dipendenze NON si toccano
        String newStatus = jsonTask.getString("status");
        Set<Task> chts = new HashSet();
        task.changeStatusPersistAndPropagate(newStatus, "Changed from Gantt", chts, logged, statusErrorMessages);
      }
    }


    //propagate date changes for all tasks
    for (int i = 0; i < jTasks.size(); i++) {
      JSONObject jsonTask = (JSONObject) jTasks.get(i);
      Task task = tasks.get(i);

      Date newStart = new Date(jsonTask.getLong("start"));
      Date newEnd = new Date(jsonTask.getLong("end")-1000);
      int newDuration = jsonTask.getInt("duration");

      if (!task.scheduleIsIdentical(newStart, newEnd, newDuration)) {
        //propagate dates
        Hashtable<Task, TaskScheduleCandidate> taskCandidates = new Hashtable<Task, TaskScheduleCandidate>();
        StringBuffer log = new StringBuffer();
        boolean scheduleOK = Task.analyzeScheduleChangesRun(newStart, newDuration, newEnd, task, log, logged, taskCandidates);

        if (scheduleOK) {
          for (TaskScheduleCandidate tsc : taskCandidates.values()) {
            tsc.taskScheduleHistory = tsc.task.changeSchedule(tsc.start, tsc.duration, tsc.end, "", logged);
          }
        } else {
          if (JSP.ex(log.toString()))
            throw new Exception(restState.getI18n("CHANGE_SCHEDULE_LOG") + ": " + log.toString());
        }
      }

    }

    if (statusErrorMessages.size() == 0) {
      //save all happenings -> events
      _ganttNotifyEvents(tasks);

      restState.addMessageOK(I18n.get("TASK_LIST_SAVED_OK"));

      //perform a checkpoint
      PersistenceContext.getDefaultPersistenceContext().checkPoint(); // todo ma siamo sicuri?

      //ricrea un project compatibile ma che in realtà è una task list
      _ganttLoadTaskList(tasks, jsonReturn);


    } else {
      //copy statusErrorMessages on errorMessages
      for (Pair<String, String[]> er : statusErrorMessages) {
        restState.addMessageError(I18n.get(er.first + "", er.second ));
      }

      jsonReturn.element("ok", false);
    }
  }

  private void _ganttLoadTaskList(List<Task> tasks, JSONObject jsonReturn) throws PersistenceException {
    if (tasks.size() > 0) {

      JSONObject jProject = new JSONObject();
      //si passa l'offset del server
      jProject.element("serverTimeOffset", SessionState.getTimeZone().getOffset(System.currentTimeMillis()));

      //get permissions
      boolean globalCanWrite = restState.getLoggedOperator().hasPermissionFor(logged, TeamworkPermissions.task_canWrite);
      jProject.element("cannotCloseTaskIfIssueOpen", I18n.isActive("CUSTOM_FEATURE_DO_NOT_CLOSE_TASK_IF_OPEN_ISSUES"));
      jProject.element("canAddIssue", true);
      jProject.element("canWriteOnParent", true); //todo ma siamo sicuri?

      Map<String, Resource> existingResources = new HashTable();
      Map<String, Role> existingRoles = new HashTable();

      JSONArray jTasks = new JSONArray();

      for (Task t : tasks) {
        JSONObject jTask = t.jsonify(false, restState);

        //add depth
        jTask.element("level", 0);

        //add assignments
        JSONArray assigs = new JSONArray();
        for (Assignment ass : t.getAssignementsSortedByRole()) {
          JSONObject jAss = new JSONObject();
          jAss.element("id", ass.getId());
          jAss.element("roleId", ass.getRole().getId());
          jAss.element("resourceId", ass.getResource().getId());
          jAss.element("effort", ass.getEstimatedWorklog());
          assigs.add(jAss);

          existingResources.put(ass.getResource().getId() + "", ass.getResource());
          existingRoles.put(ass.getRole().getId() + "", ass.getRole());

        }
        jTask.element("assigs", assigs);

        //canWrite
        //boolean canWrite = t.hasPermissionFor(logged, TeamworkPermissions.task_cost_canWrite) && !t.isProcessDriven();   // CHE PERLA!
        boolean canWrite = t.hasPermissionFor(logged, TeamworkPermissions.task_canWrite) && !t.isProcessDriven();
        globalCanWrite = globalCanWrite || canWrite;
        jTask.element("canWrite", canWrite);
        jTask.element("canAddIssue", t.hasPermissionFor(logged, TeamworkPermissions.issue_canCreate));

        jTasks.add(jTask);
      }

      //add tasks
      jProject.element("tasks", jTasks);

      //add resources
      this._ganttAddResources(restState, null, jProject, existingResources);

      //add roles
      _ganttAddRoles(restState, null, jProject, existingRoles);

      jProject.element("canWrite", globalCanWrite);
      jsonReturn.element("project", jProject);
    }
  }


  private void _ganttNotifyEvents(List<Task> tasks) throws StoreException {
    for (Task task : tasks) {
      //store the pending events
      for (SomethingHappened sh : task.happenings) {
        sh.setIdentifiableId(task.getId() + ""); // this is necessary in order to update new_empty_id with right ones
        sh.store();
      }
    }
  }

  private void _ganttChangeDependenciesAndStatus(List<Task> tasks, ArrayList<Pair<String, String[]>> statusErrorMessages, JSONObject jsonTask, Task task, String changedStatusReasonWhy) throws ApplicationException, PersistenceException {
    String newStatus = jsonTask.getString("status");

    //check if task has external deps
    boolean hasExternalDep = false;
    for (TaskDependency td : task.getPreviouses()) {
      if (tasks.indexOf(td.getDepends()) < 0) {
        hasExternalDep = true;
        break;
      }
    }

    //check if some external tasks depends on task
    if (!hasExternalDep) {
      for (TaskDependency td : task.getNexts()) {
        if (tasks.indexOf(td.getTask()) < 0) {
          hasExternalDep = true;
          break;
        }
      }
    }


    //if task has an external dependency do not change dates and deps
    if (!hasExternalDep) {
      //remove deps
      for (TaskDependency td : task.getPreviouses()) {
        td.remove();
      }
      //reset in-memory (inverse) collection
      task.setPreviouses(new HashSet<TaskDependency>());
      task.setNexts(new HashSet<TaskDependency>());

      String oldStatus = task.getStatus();

      if (!newStatus.equals(oldStatus)) {

        //check if open issues
        if (newStatus.equals(TaskStatus.STATUS_DONE) && I18n.isActive("CUSTOM_FEATURE_DO_NOT_CLOSE_TASK_IF_OPEN_ISSUES")) {
          if (task.getTotalIssuesOpen() > 0) {
            statusErrorMessages.add(new Pair("CANNOT_CLOSE_TASK_IF_OPEN_ISSUE_%%", new String[]{task.getDisplayName()}));
            return;
          }
        }

        //set status without propagating
        task.setStatus(newStatus);

        // set progress to 100% if needed by settings
        if (TaskStatus.STATUS_DONE.equals(newStatus) && Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("SET100ONCLOSE")) && !task.isProgressByWorklog())
          task.setProgress(100.00);


        SomethingHappened change = task.generateChangeStatusEvent(oldStatus, newStatus, "", logged);
        if (change != null)
          task.happenings.add(change);

        //notification are generated only if task is not new and it is created 6 hours before in order to not annoy users
        boolean notifyStatusChanges = !task.isNew() && (
          (I18n.isActive("CUSTOM_FEATURE_NOTIFY_ONLY_SIX_HOURS_OLDER_TASK") && (System.currentTimeMillis() - task.getCreationDate().getTime() > CompanyCalendar.MILLIS_IN_6_HOUR))
            || !I18n.isActive("CUSTOM_FEATURE_NOTIFY_ONLY_SIX_HOURS_OLDER_TASK"));


        if (notifyStatusChanges) {
          //generate task schedule history
          TaskStatusHistory tsth = new TaskStatusHistory();
          tsth.setIdAsNew();
          tsth.setTask(task);
          tsth.setChangeLog(changedStatusReasonWhy);
          tsth.setFromStatus(oldStatus);
          tsth.setToStatus(newStatus);
          tsth.store();
        }

      }

      //set dependencies
      if (jsonTask.get("depends") != null) {
        String depss = jsonTask.getString("depends");
        if (JSP.ex(depss)) {
          String[] deps = depss.split(",");
          for (String dep : deps) {
            String[] par = dep.split(":");
            int lag = 0;
            if (par.length > 1)
              lag = Integer.parseInt(par[1]);

            Task superior = tasks.get(Integer.parseInt(par[0]) - 1);

            //create a new dependency
            TaskDependency td = new TaskDependency();
            td.setIdAsNew();
            td.setTask(task);
            td.setDependency(superior);
            td.setLag(lag);
            td.store();

            //set in memory
            task.getPreviouses().add(td);
          }
        }

      }

    } else {
      //set statuses and propagate
      Set<Task> chts = new HashSet();
      task.changeStatusPersistAndPropagate(newStatus, "", chts, logged, statusErrorMessages);
    }
  }

  private boolean _ganttSaveSingleTask(JSONObject jsonTask, int level, String code, Task parent, Task[] taskHolder, String changesReasonWhy, boolean changeSchedule) throws SecurityException, PersistenceException {
    Task task = taskHolder[0];
    if (task != null) {
      //check security
      //R&S 30/3/2016 se non hai i permessi su un task deve andare avanti.
      // Ad esempio nel caso si abbia permesso pm su un sotto task e read sulla root, al salvataggio arriva anche la root su cui non si hanno permessi,
      // ma questo non deve fermare il save. Il tempo ci dirà se avevamo ragione
      // task.testPermission(logged, TeamworkPermissions.task_canWrite);
      if (!task.hasPermissionFor(logged, TeamworkPermissions.task_canWrite))
        return false;
      task.setCode(code);

      // test code uniqueness
      if (Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("USEUNIQUECODES")) && JSP.ex(task.getCode()) && !task.isUnique("code")) {
        throw new PlatformRuntimeException("'"+I18n.get("TASK_CODE")+"' "+I18n.get("KEY_MUST_BE_UNIQUE")+": '" +task.getCode()+"'");
      }

      //cannot change parent for the gantt root
      if (level != 0)
        task.setParentAndStore(parent);

    } else {
      //check security
      if (parent == null)
        logged.testPermission(TeamworkPermissions.project_canCreate);
      else
        parent.testPermission(logged, TeamworkPermissions.task_canCreate);

      //create a new task, set parent, set code and area, Si mette sul taskHolder per farlo tornare indietro
      task = new Task();
      taskHolder[0] = task;

      task.setInherit(false);
      task.setPropagate(true);

      if (parent != null) {
        task.setParentAndStore(parent);
        task.setProgressByWorklog(parent.isProgressByWorklog());
        if (!JSP.ex(code)) {
          task.bricks.suggestCodeFromParent();
        } else {
          task.setCode(code);
        }
        //task.setStatus(parent.getStatus());
        task.setType(parent.getType());

        //determine default area
        task.setArea(parent.getArea());

      } else {
        //this is a root task
        task.setArea(logged.getDefaultAreaForPermission(TeamworkPermissions.project_canCreate));
        if (!JSP.ex(code)) {
          task.setCode(this.genTaskCode(task)); //to guess the right code the "type" attribute should be set
        } else {
          task.setCode(code);
        }

      }
    }

    task.setName(jsonTask.getString("name"));
    if (!JSP.ex(task.getName())) {
      task.setName(task.getCode());
    }


    if (jsonTask.has("description"))
      task.setDescription(jsonTask.getString("description"));

    if (jsonTask.has("progress"))
      task.setProgress(jsonTask.getDouble("progress"));

    if (jsonTask.has("progressByWorklog"))
      task.setProgressByWorklog(jsonTask.getBoolean("progressByWorklog"));

    if (jsonTask.has("typeId") && JSP.ex(jsonTask.getString("typeId"))) {
      String typeId = jsonTask.getString("typeId");
      TaskType type = TaskType.load(typeId);
     if(type == null && jsonTask.has("typeCode") && JSP.ex(jsonTask.getString("typeCode"))){
       String tCode = jsonTask.getString("typeCode");
       type = TaskType.loadByCode(tCode);
     }
      task.setType(type);
    }

    if (jsonTask.has("relevance"))
      task.setRelevance(jsonTask.getInt("relevance"));

    //milestones
    task.setStartIsMilestone(jsonTask.getBoolean("startIsMilestone"));
    task.setEndIsMilestone(jsonTask.getBoolean("endIsMilestone"));

    //set dates without propagating
    Date start = new Date(jsonTask.getLong("start"));
    Date end = new Date(jsonTask.getLong("end")-1000);
    int duration = jsonTask.getInt("duration");


    if (changeSchedule) {
      TaskScheduleHistory taskHistory = task.changeSchedule(start, duration, end, changesReasonWhy, logged);

      if (taskHistory != null)
        taskHistory.store();
    }
    //store task and history
    task.store();


    //mark root dates changed in order to eventually propagate outside the gantt tree
    return !task.scheduleIsIdentical(start, end, duration);
  }


  private void _ganttSetOrderFactor(Map<Integer, Integer> levelLastNum, int level, Task task) {
    //si setta l'orderFactor
    Integer lastCount = levelLastNum.get(level);
    if (lastCount == null) {
      lastCount = 0;
    }
    lastCount++;
    levelLastNum.put(level, lastCount);
    boolean setOrder = (level == 0 && !JSP.ex(task.getOrderFactor())) || (level > 0);

    String padd = StringUtilities.paddTo(lastCount, "000");
    if (setOrder) {
      if (task.getParent() != null) {
        task.setOrderFactor(JSP.ex(task.getParent().getOrderFactor()) ? task.getParent().getOrderFactor() + "." + padd : padd);
      } else {
        task.setOrderFactor(padd);
      }
    }
  }

  private void _ganttManageAssignment(JSONObject jsonGanttData, Area area, JSONObject jsonTask, Task task) throws Exception {
    //can manage assigs?
    if (task.hasPermissionFor(logged, TeamworkPermissions.assignment_canCRW)) {

      RestState assRestState = new RestState(logged);
      AssignmentAction assignmentAction = new AssignmentAction(assRestState);

      //assignments
      if (jsonTask.get("assigs") != null) {
        JSONArray jAssigs = jsonTask.getJSONArray("assigs");
        Set<Assignment> toBeDeleted = new HashSet<Assignment>();
        toBeDeleted.addAll(task.getAssignments());

        for (Object ao : jAssigs) {
          JSONObject jAssig = (JSONObject) ao;

          String assId = jAssig.getString("id");
          Assignment assig = Assignment.load(assId);

          //se l'assegnazione non è stata modificata la salta. Questo valore è settato nel Gantt
          if (assig!=null && jAssig.has("unchanged") && jAssig.getBoolean("unchanged")){
            //rimuove l'assegnazione da quelle da cancellare
            toBeDeleted.remove(assig);
            continue;
          }

          if (resourceDecoder==null)
            resourceDecoder = _ganttGetResources(jsonGanttData);
          if (roleDecoder==null)
            roleDecoder = _ganttGetRoles(jsonGanttData, area);


          //no role, no party
          RoleTeamwork roleTeamwork = roleDecoder.get(jAssig.getString("roleId"));
          if (roleTeamwork == null)
            continue;


          //empty all ces
          assRestState.setClientEntries(new ClientEntries());
          //set the current task as default for AssignmentAction
          assRestState.addClientEntry("TASK_ID", task);

          //prepare client entries before calling Assignment Action

          boolean assigIsNew = false;
          //is a new assig
          if (assig == null) {
            assRestState.mainObjectId = PersistenceHome.NEW_EMPTY_ID + "";  //WARNING +"" is mandatory to have a string instead of a serializable -> hib skiant
            assignmentAction.cmdAdd();
            assigIsNew = true;

            //add role subs default client entries
            SerializedMap<String, String> subm = roleTeamwork.getDefaultSubscriptions();
            if (subm != null) {
              for (String k : subm.keySet()) {
                assRestState.addClientEntry(k, subm.get(k));
              }
            }


          } else {
            assRestState.mainObjectId = assId;
            assignmentAction.makeAssignment(assig);
          }

          //create role CE
          assRestState.addClientEntry("ASSIG_ROLE", roleTeamwork);

          assRestState.addClientEntryTime("ESTIMATE_WORKLOG", jAssig.getLong("effort"));

          //create assignee CEs
          String resId = jAssig.getString("resourceId");
          String resName = resourceDecoder.get(resId);

          assRestState.addClientEntry("ASSIGNEE", resId);
          assRestState.addClientEntry("ASSIGNEE" + SmartCombo.TEXT_FIELD_POSTFIX, resName);

          //make subscription notifications (in order to not reset it)
          Resource res = Resource.load(resId);
          if (res != null) {
            TeamworkOperator resMyself = res.getMyself();
            // perchè non dovrebbe creare le sottoscrizioni? non sono mica eventi!
            //if (resMyself != null && (!logged.equals(resMyself) || Fields.TRUE.equals(resMyself.getOption(OperatorConstants.NOTIFY_MY_EVENTS_TO_MYSELF)))) { // non notifichiamo noi stessi
            if (resMyself != null) {
              List<Listener> myListeners = task.bricks.getListeners(resMyself);
              for (Listener l : myListeners) {
                assRestState.addClientEntry((l.getEventType()), Fields.TRUE);
                assRestState.addClientEntry("TASK_NOTIFY_DESC", l.isListenDescendants() ? Fields.TRUE : Fields.FALSE);
                List<String> medias = StringUtilities.splitToList(l.getMedia(), ",");
                String type = l.getEventType();
                for (String media : medias) {
                  assRestState.addClientEntry(type + "_" + media, Fields.TRUE);
                }
              }

              for (String media : resMyself.getPreferredMediaOrDefault(MessagingSystem.Media.STICKY))
                assRestState.addClientEntry("ASSIGNEE_NOTIFY_" + media, Fields.TRUE);

            }
          }


          //add or make costs
          if (assig == null)
            assRestState.addClientEntry("ASSIG_COST", res == null ? JSP.currency(ResourceBricks.getDefaultHourlyCost()) : res.getHourlyCost() > 0 ? JSP.currency(res.getHourlyCost()) : JSP.currency(ResourceBricks.getDefaultHourlyCost()));
          else
            assRestState.addClientEntry("ASSIG_COST", JSP.currency(assig.getHourlyCost()));


          //execute command save on AssignmentAction
          assignmentAction.cmdSave();
          assig = (Assignment) assRestState.getMainObject();

          //la chiamata alla AssignmentAction mette dei message Ok che non hanno senso
          assRestState.removeMessagesOfType(PageState.MessageType.OK);


          //remove the current assig from ones that will be removed
          toBeDeleted.remove(assig);

        }

        //remove remaining assigs that should be untouched -> removed client side
        for (Assignment a : toBeDeleted) {
          assRestState.setMainObjectId(a.getId());
          assignmentAction.cmdDelete();
        }

      }
    }
  }

  private Map<Serializable, RoleTeamwork> _ganttGetRoles(JSONObject oneProject, Area area) throws Exception {
    //---------------------- missing roles creation
    Map<Serializable, RoleTeamwork> roleDecoder = new HashTable();
    if (oneProject.get("roles") != null) {
      List<JSONObject> jRoles = oneProject.getJSONArray("roles");
      for (JSONObject jRole : jRoles) {
        String roleId = jRole.get("id") + "";
        RoleTeamwork role = RoleTeamwork.load(roleId);
        //the role is a candidate for new
        String roleName = jRole.getString("name");
        if (role == null) {
          role = RoleTeamwork.guess(roleName, area, true);
        }

        if (role != null)
          roleDecoder.put(roleId, role);
        else
          throw new Exception("No Role found: " + roleName);
      }
    }
    return roleDecoder;
  }

  private Map<Serializable, String> _ganttGetResources(JSONObject oneProject) {
    //---------------------- resource decoding
    Map<Serializable, String> resourceDecoder = new HashTable();
    if (oneProject.get("resources") != null) {
      List<JSONObject> jRsess = oneProject.getJSONArray("resources");
      for (JSONObject jRes : jRsess) {
        resourceDecoder.put(jRes.get("id") + "", jRes.getString("name"));
      }
    }
    return resourceDecoder;
  }

  private void _ganttRemoveTasks(JSONObject oneProject) throws SecurityException, PersistenceException {
    //remove (eventually) deleted tasks
    if (oneProject.get("deletedTaskIds") != null) {
      JSONArray deletedTaskIds = oneProject.getJSONArray("deletedTaskIds");

      //set client entry for a global delete of children
      restState.addClientEntry(ObjectEditorConstants.FLD_DELETE_STYLE + "__" + "children", Commands.DELETE_DESCENDANTS);

      //save main objectId
      Serializable oldMainObjectId = restState.mainObjectId;


      for (Object id : deletedTaskIds) {
        restState.mainObjectId=id+"";
        cmdDelete(); // si chiama il command giusto

        /*
        Task delendo = Task.load(id + "");
        if (delendo != null) {
          delendo.testPermission(logged, TeamworkPermissions.task_canCreate);
          DeleteHelper.cmdDelete(delendo, restState);
        }*/
      }
      restState.mainObjectId=oldMainObjectId;
    }
  }

}
