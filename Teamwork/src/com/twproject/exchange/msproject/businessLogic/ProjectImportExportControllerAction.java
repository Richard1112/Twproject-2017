package com.twproject.exchange.msproject.businessLogic;

import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Person;
import com.twproject.resource.Resource;
import com.twproject.security.RoleTeamwork;
import com.twproject.security.TeamworkPermissions;
import com.twproject.task.Assignment;
import com.twproject.task.Task;
import com.twproject.task.TaskDependency;
import com.twproject.task.TaskStatus;
import net.sf.mpxj.*;
import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.agenda.Period;
import org.jblooming.oql.OqlQuery;
import org.jblooming.oql.QueryHelper;
import org.jblooming.persistence.exceptions.FindException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.security.Area;
import org.jblooming.utilities.CodeValueList;
import org.jblooming.utilities.HashTable;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.view.PageState;

import java.util.*;

/**
 * Created by Open Lab
 * info@open-lab.com
 * Date: Oct 23, 2006
 * Time: 2:04:53 PM
 */
public class ProjectImportExportControllerAction {

  public static net.sf.mpxj.Task exportNode(Task task, ProjectFile msProjectFile, int outlineLevel, Map<Person, net.sf.mpxj.Resource> personResource) throws FindException, MPXJException {

    net.sf.mpxj.Task mstask = msProjectFile.addTask();
    mstask.setOutlineLevel(outlineLevel);
/*
    PageSeed edit = new PageSeed(ApplicationState.serverURL+request.getContextPath()+"/application/teamwork/task/taskEditor.jsp");
    edit.mainObjectId = task.getId();
    edit.command = Commands.EDIT;
    mstask.setHyperlink(new ButtonLink(edit).toPlainLink());
*/
    //mstask.setID(Integer.parseInt(task.getId()+""));
    mstask.setType(net.sf.mpxj.TaskType.FIXED_DURATION);
    mstask.setNumber(1, Integer.parseInt(task.getId() + ""));
    mstask.setText(1, task.getCode());

    if (JSP.ex(task.getCode()))
      mstask.setWBS(task.getCode());

    mstask.setName(task.getName());
    if (task.getType() != null)
      mstask.setText(2, task.getType().getDescription());
    mstask.setText(3, task.getStatus());
    mstask.setNumber(2, task.getRelevance());
    //MS projects sets it :-(
    //mstask.setPercentageComplete(task.getProgress());
    mstask.setNumber(3, task.getTotalWorklogEstimated());

    mstask.setNotes(JSP.w(task.getDescription()) + "\n" + JSP.w(task.getNotes()));

    mstask.setPercentageComplete(task.getProgress());
    //mstask.setPercentageWorkComplete(task.getProgress());
    //mstask.setPhysicalPercent4Complete((int)task.getProgress());


    if (task.getSchedule() != null) {
      mstask.setStart(task.getSchedule().getStartDate());
      mstask.setActualStart(task.getSchedule().getStartDate());
      Duration dur = Duration.getInstance(task.getDuration(), TimeUnit.DAYS);
      mstask.setDuration(dur);
    }

    Set<Person> team = task.getPersons();
    for (Person person : team) {
      if (!personResource.containsKey(person)) {
        net.sf.mpxj.Resource resource = msProjectFile.addResource();
        resource.setName(JSP.w(person.getDisplayName()));
        //resource.setID(Integer.parseInt(person.getId() + ""));
        resource.setText(1, person.getId() + "");
        personResource.put(person, resource);
      }
    }

    if (task.isStartIsMilestone() || task.isEndIsMilestone()) {
      //mstask.setMilestone(true);
    }

    for (Assignment a : task.getAssignments()) {

      net.sf.mpxj.Resource msresource = personResource.get(a.getResource());
      if (msresource != null) {
        ResourceAssignment msassignment = mstask.addResourceAssignment(msresource);
        long estInMillis = a.getEstimatedWorklog();

        Duration dEW = Duration.getInstance(estInMillis / CompanyCalendar.MILLIS_IN_HOUR, TimeUnit.HOURS);
        Duration dWD = Duration.getInstance((a.getWorklogDone() / CompanyCalendar.MILLIS_IN_HOUR), TimeUnit.HOURS);
        Duration dRemaining = Duration.getInstance((estInMillis - a.getWorklogDone()) / CompanyCalendar.MILLIS_IN_HOUR, TimeUnit.HOURS);

        //18Jun2008 remmed out BEGIN
        /*msassignment.setWork(dEW);
        msassignment.setActualWork(dWD);
        msassignment.setRemainingWork(dRemaining);*/
        //18Jun2008 remmed out END

        //never was exported
        //msassignment.setPlannedWork(dEW);


        //msassignment.setCost(a.getHourlyCost());
        //msassignment.setPlannedCost(a.getHourlyCost());
        msresource.setStandardRate(new Rate(a.getHourlyCost(), TimeUnit.HOURS));
        msresource.setOvertimeRate(new Rate(a.getHourlyCost(), TimeUnit.HOURS));
      }
    }

    Map<Task, net.sf.mpxj.Task> taskMsTask = new HashTable();

    String hql = "from " + Task.class.getName() + " as task where task.parent = :myself order by task.schedule.start, task.code, task.name";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setEntity("myself", task);

    List<Task> csbdcn = oql.list();


    for (Task child : csbdcn) { //task.getChildrenSorted()
      net.sf.mpxj.Task mschild = exportNode(child, msProjectFile, mstask.getOutlineLevel() + 1, personResource);
      taskMsTask.put(child, mschild);
    }

    for (Task child : csbdcn) {

      for (TaskDependency td : child.getPreviouses()) {

        net.sf.mpxj.Task mstaskForChild = taskMsTask.get(child);
        net.sf.mpxj.Task mstaskForPredecessor = taskMsTask.get(td.getDepends());

        if (mstaskForPredecessor != null) {
          Relation rel1 = mstaskForChild.addPredecessor(mstaskForPredecessor, RelationType.FINISH_START, Duration.getInstance(0, TimeUnit.DAYS));
        }
      }

    }


    return mstask;

  }

  public static ImportResult importNode(ProjectFile msProjectFile, Task parent, TeamworkOperator logged, PageState pageState) throws PersistenceException, org.jblooming.security.SecurityException, ApplicationException {
    pageState.initializeEntries("row");

    Area defaultArea=null;
    if (parent!=null) {
      parent.testPermission(logged, TeamworkPermissions.task_canCreate);
      defaultArea=parent.getArea();
    } else {
      logged.testPermission(TeamworkPermissions.project_canCreate);
      defaultArea=logged.getDefaultAreaForPermission(TeamworkPermissions.project_canCreate);
    }

    logged.testPermission(TeamworkPermissions.resource_canCreate);

    ProjectHeader header = msProjectFile.getProjectHeader();
    Date start = header.getStartDate() != null ? header.getStartDate() : new Date();
    Date  end = header.getFinishDate() != null ? header.getFinishDate() : new Date();

    //si impostano start ed end all'inizio e fine giornata
    CompanyCalendar cc = new CompanyCalendar(start);
    start=cc.setAndGetTimeToDayStart();
    cc.setTime(end);
    end= cc.setAndGetTimeToDayEnd();


    Duration msDur = header.getDuration();
    int duration = CompanyCalendar.getWorkingDaysCountInPeriod(new Period(start, end));

    ImportResult ir = new ImportResult();

    ir.defaultArea = defaultArea;

    //find default role
    String hql = "select role from " + RoleTeamwork.class.getName() + " as role";

    QueryHelper qh = new QueryHelper(hql);
    String workerString = ApplicationState.getApplicationSetting("DEFAULT_WORKER_ROLE_NAME", "WORKER");
    qh.addQBEClause("name", "worker", workerString, QueryHelper.TYPE_CHAR);
    if (defaultArea != null) {
      qh.addOQLClause("role.area=:areax", "areax", defaultArea);
    }
    List<RoleTeamwork> all = qh.toHql().list();
    if (!JSP.ex(all))
      throw new PlatformRuntimeException("Not found a " + workerString + " role " + (defaultArea != null ? " for the area " + defaultArea.getName() : "") + ": create one for the import to work.");


    RoleTeamwork defaultWorker = all.get(0);
    ir.defaultRole = defaultWorker;

    Task root = null;
    if (parent != null)
      root = parent;
    else {
      root = new Task();
      root.setIdAsNew();
      String projectTitle = header.getProjectTitle();
      if (projectTitle == null || projectTitle.trim().length() == 0)
        projectTitle = "IMPORTED PROJECT - ROOT";
      root.setName(projectTitle);
      root.store();
      root.setCode("T"+root.getId());
      if (end == null || end.getTime() > System.currentTimeMillis())
        root.setStatus(TaskStatus.STATUS_ACTIVE);
      else
        root.setStatus(TaskStatus.STATUS_DONE);
      Period p = new Period(start, end);
      p.store();
      root.setSchedule(p);
      root.setTotalWorklogDone(0);
      root.setDuration(duration);
      root.setArea(ir.defaultArea);
      root.setOwner(logged);
      root.store();
    }
    ir.root = root;

    net.sf.mpxj.Task mstask;
    net.sf.mpxj.Task fakeRoot = (net.sf.mpxj.Task) msProjectFile.getChildTasks().get(0);
    Iterator iter = fakeRoot.getChildTasks().iterator();
    int i=0;
    while (iter.hasNext() == true) {
      i++;
      mstask = (net.sf.mpxj.Task) iter.next();
      importMSTask(mstask, root, header, ir, i, pageState);
    }

    for (net.sf.mpxj.Task mstaskRead : ir.createdTasks.keySet()) {
      List<Relation> successors = mstaskRead.getSuccessors();
      if (successors != null)
        for (Relation relation : successors) {
          Task mainTask = ir.createdTasks.get(relation.getSourceTask());
          Task depends = ir.createdTasks.get(relation.getTargetTask());
          TaskDependency td = new TaskDependency();
          td.setTask(depends);
          td.setDependency(mainTask);
          td.store();
        }
    }

    return ir;
  }

  private static void importMSTask(net.sf.mpxj.Task mstask, Task parent, ProjectHeader header, ImportResult ir, int orderFactor, PageState pageState) throws PersistenceException {

    Task newTask = new Task();
    newTask.setIdAsNew();
    newTask.setArea(ir.defaultArea);
    newTask.setParentAndStore(parent);
    newTask.setName(mstask.getName());

    //newTask.setCode(mstask.getUniqueID() + "");
    newTask.bricks.suggestCodeFromParent();

    if (mstask.getPercentageComplete() != null)
      newTask.setProgress(mstask.getPercentageComplete().doubleValue());

    Date start = mstask.getStart();
    //int duration = (int) mstask.getDuration().convertUnits(TimeUnit.DAYS, header).getDuration();
    Date end = mstask.getFinish();

    if (end == null)
      end = start;

    //si impostano start ed end all'inizio e fine giornata
    CompanyCalendar cc = new CompanyCalendar(start);
    start=cc.setAndGetTimeToDayStart();
    cc.setTime(end);
    end= cc.setAndGetTimeToDayEnd();

    int duration = CompanyCalendar.getDistanceInWorkingDays(start, end);

    Period p = new Period(start, end);
    p.store();
    newTask.setSchedule(p);
    newTask.setDuration(duration);


    //bicch 5/10/2016 cambiato il comportamento dell'import. Comanda il progress non le date. In questo modo si mettono attivi se non solo al 100%
    if (newTask.getProgress()<100) {
      newTask.setStatus(TaskStatus.STATUS_ACTIVE);
    } else {
      newTask.setStatus(TaskStatus.STATUS_DONE);
    }
    /*if (end == null || end.getTime() > System.currentTimeMillis() ) {
      newTask.setStatus(TaskStatus.STATUS_ACTIVE);
    }else {
      newTask.setStatus(TaskStatus.STATUS_DONE);
    }*/

    String padd = StringUtilities.paddTo(orderFactor, "00");
    if (newTask.getParent() != null) {
      newTask.setOrderFactor(JSP.ex(newTask.getParent().getOrderFactor()) ? newTask.getParent().getOrderFactor() + "." + padd : padd);
    } else {
      newTask.setOrderFactor(padd);
    }


    newTask.store();

    List assignments = mstask.getResourceAssignments();
    Iterator assignmentIter = assignments.iterator();

    while (assignmentIter.hasNext() == true) {
      ResourceAssignment msassignment = (ResourceAssignment) assignmentIter.next();
      net.sf.mpxj.Resource msresource = msassignment.getResource();
      if (msresource != null) {
        Assignment assig = new Assignment();
        assig.setIdAsNew();
        assig.setRole(ir.defaultRole);
        assig.setOwner(pageState.getLoggedOperator());
        assig.setActivity(Assignment.ACTIVITY_ALL_IN_ONE);
        if (msassignment.getWork() != null)
          assig.setEstimatedWorklog((long) (msassignment.getWork().convertUnits(TimeUnit.HOURS, header).getDuration() * CompanyCalendar.MILLIS_IN_HOUR));
        assig.setEnabled(true);
        assig.setTask(newTask);

        Resource aggignResource = ir.createdResources.get(msresource);

        if (aggignResource == null) {
          String hql = "select resource from " + Resource.class.getName() + " as resource";
          QueryHelper qh = new QueryHelper(hql);
          String completeName = msresource.getName();
          qh.addQBEClause("name", "completeName", "\"" + completeName + "\"", QueryHelper.TYPE_CHAR);
          List<Resource> all = qh.toHql().list();
          if (!JSP.ex(completeName))
            completeName = "imported_resource";
          if (all.size() > 0) {
            aggignResource = all.get(0);
          } else {
            Person person = new Person();
            person.setIdAsNew();
            if (completeName.indexOf(" ") > -1) {
              person.setPersonName(completeName.substring(completeName.indexOf(" ") + 1));
              person.setPersonSurname(completeName.substring(0, completeName.indexOf(" ")));
            } else {
              person.setPersonName(completeName);
              person.setPersonSurname(completeName);
            }

            person.setArea(ir.defaultArea);
            person.store();
            aggignResource = person;
          }
          ir.createdResources.put(msresource, aggignResource);
        }
        assig.setResource(aggignResource);
        assig.store();
      }
    }

    ir.createdTasks.put(mstask, newTask);

    List tasks = mstask.getChildTasks();
    Iterator iter = tasks.iterator();

    int j=0;
    while (iter.hasNext() == true) {
      j++;
      mstask = (net.sf.mpxj.Task) iter.next();
      importMSTask(mstask, newTask, header, ir, j, pageState);
    }


  }


  public static class ImportResult {

    public Map<net.sf.mpxj.Resource, Resource> createdResources = new HashTable();
    public List<AssignmentAndCandidates> assignmentsToFix = new ArrayList();
    public RoleTeamwork defaultRole;
    public Area defaultArea;
    public Task root;
    public Map<net.sf.mpxj.Task, Task> createdTasks = new HashTable();
  }

  public class AssignmentAndCandidates {

    public Assignment assig;
    public List<Resource> candidates;
  }

  public static CodeValueList getLocalizations() {

    CodeValueList cvl = new CodeValueList();
    cvl.add("de", "German");
    cvl.add("en", "English");
    cvl.add("fr", "French");
    cvl.add("es", "Spanish");
    cvl.add("it", "Italian");
    cvl.add("pt", "Portuguese");
    cvl.add("sv", "Swedish");
    cvl.add("zh", "Simplified chinese");
    return cvl;
  }

  public static String readProjectData(ProjectFile msProjectFile) {
    StringBuffer result = new StringBuffer();
    ProjectHeader header = msProjectFile.getProjectHeader();
    result.append(header.getProjectTitle());

    net.sf.mpxj.Task mstask;
    net.sf.mpxj.Task fakeRoot = (net.sf.mpxj.Task) msProjectFile.getChildTasks().get(0);
    Iterator iter = fakeRoot.getChildTasks().iterator();
    while (iter.hasNext() == true) {
      mstask = (net.sf.mpxj.Task) iter.next();
      readMSTask(mstask, result);
    }
    return result.toString();

  }

  public static void readMSTask(net.sf.mpxj.Task mstask, StringBuffer result) {
    result.append(" " + mstask.getName());
    List assignments = mstask.getResourceAssignments();
    Iterator assignmentIter = assignments.iterator();

    while (assignmentIter.hasNext() == true) {
      ResourceAssignment msassignment = (ResourceAssignment) assignmentIter.next();
      net.sf.mpxj.Resource msresource = msassignment.getResource();
      result.append(" " + msresource.getName());
    }

  }
}
