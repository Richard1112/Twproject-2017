package com.twproject.task.businessLogic;

import com.twproject.agenda.Event;
import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Person;
import com.twproject.resource.Resource;
import com.twproject.security.RoleTeamwork;
import com.twproject.security.TeamworkPermissions;
import com.twproject.task.Assignment;
import com.twproject.task.Issue;
import com.twproject.task.Task;
import com.twproject.task.TaskStatus;
import org.jblooming.ApplicationException;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.agenda.Period;
import org.jblooming.agenda.ScheduleDaily;
import org.jblooming.messaging.Listener;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.security.Area;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.ActionSupport;
import org.jblooming.waf.ActionUtilities;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.text.ParseException;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 8-set-2006 : 12.01.56
 */
public class ScrumControllerAction extends ActionSupport implements ActionController {

  public ScrumControllerAction(PageState pageState)  {
    super(pageState);
  }


  public PageState perform(HttpServletRequest request, HttpServletResponse response) throws PersistenceException,  org.jblooming.security.SecurityException, ApplicationException {
    PageState pageState = PageState.getCurrentPageState(request);
    String command = pageState.getCommand();

    try {
      if ("SAVE_ROOT_SCRUM_WIZ".equals(command)) {
        cmdSaveRoot();
      } else if ("GENERATE_SPRINT".equals(command)) {
        cmdGenerateSprint();
        PageSeed ps = pageState.pageFromRoot("task/taskEditor.jsp");
        ps.mainObjectId=pageState.getMainObject().getId();
        ps.command= Commands.EDIT;
        pageState.redirect(ps);


      } else if ("NEW_SPRINT".equals(command)) {
        cmdNewSprint();
      }
    } catch (ActionException ae) {
    } catch (ParseException p) {
    }


    return pageState;


  }

  private void cmdNewSprint() throws FindByPrimaryKeyException {
    Task t = new Task();
    t.setIdAsNew();

    Task bl=Task.load(restState.getEntry("PARENT_ID").intValueNoErrorCodeNoExc()+"");
    t.setParentAndStore(bl);

    restState.setMainObject(t);

    restState.addClientEntry("START", new Date());
    restState.addClientEntry("TASK_DURATION", 30);
    CompanyCalendar cc = new CompanyCalendar(new Date());
    cc.addWorkingDays(30);
    restState.addClientEntry("END", cc.getTime());


  }

  private void cmdGenerateSprint() throws PersistenceException, org.jblooming.security.SecurityException, ActionException, ParseException {

    TaskAction taskAction = new TaskAction(restState);
    //pageState.addClientEntry("PROGRESS_BY_WORKLOG", Fields.TRUE);
    restState.addClientEntry("STATUS", TaskStatus.STATUS_ACTIVE);
    restState.addClientEntry("TASK_TAGS", "SPRINT");

    taskAction.cmdSave();
    Task sprint = (Task) restState.getMainObject();

    boolean invalidClientEntries;
    // controller/action
    TeamworkOperator logged = (TeamworkOperator) restState.getLoggedOperator();
    Person loggedPerson = logged.getPerson();

    // disable the rights avalanche
    Task scrumRoot = sprint.getParent();
    scrumRoot.setPropagate(false);
    scrumRoot.store();

    if (restState.validEntries()) {

      sprint.store();

      Set<Resource> assignee = new HashSet();

      // must copy the assignment for "Scrum Master" and "Scrum Team" from root to the sprint
      for (Assignment ass : scrumRoot.getAssignments()) {
        String ROLE_SCRUM_MASTER_NAME = ApplicationState.getApplicationSetting("ROLE_SCRUM_MASTER_NAME", "Scrum Master");
        String ROLE_SCRUM_TEAM_NAME = ApplicationState.getApplicationSetting("ROLE_SCRUM_TEAM_NAME", "Scrum Team");

        if (ass.getRole().getName().equalsIgnoreCase(ROLE_SCRUM_TEAM_NAME) ||
            ass.getRole().getName().equalsIgnoreCase(ROLE_SCRUM_MASTER_NAME)) {
          Assignment assSprint = new Assignment();
          assSprint.setIdAsNew();
          assSprint.setOwner(restState.getLoggedOperator());
          assSprint.setRole(ass.getRole());
          assSprint.setResource(ass.getResource());
          assSprint.setActivity(Assignment.ACTIVITY_ALL_IN_ONE);
          assSprint.setAssignmentDate(new Date());
          assSprint.setTask(sprint);
          assSprint.store();
          assignee.add(ass.getResource());



          //subscibe to miles closer
          if (ReflectionUtilities.instanceOf(ass.getResource(), Person.class)) {
            TeamworkOperator myself = ((Person) ass.getResource()).getMyself();
            String mediasS = StringUtilities.unSplit(myself.getPreferredMediaOrDefault(MessagingSystem.Media.STICKY),",");
            Listener l = new Listener(myself);
            l.setIdAsNew();
            l.setIdentifiable(sprint);
            l.setMedia(mediasS);
            l.setEventType(Task.Event.TASK_MILESTONE_CLOSER + "");
            l.setListenDescendants(true);
            l.store();
          }
        }
      }

      // order the issues
      String idss = restState.getEntry("input_orderArea").stringValueNullIfEmpty();
      if (idss != null) {
        int i = 1;
        List<String> ids = StringUtilities.splitToList(idss, ",");
        for (String idSudic : ids) {
          String id = idSudic.substring(3);
          Issue issue = Issue.load(id);
          issue.setOrderFactor(i++);
          issue.store();
        }
      }

      //move the issue
      for (Issue issue : scrumRoot.getIssues())
        if (restState.getEntry("issue_" + issue.getId()).checkFieldValue()) {
          issue.setTask(sprint);
          issue.store();
        }

      scrumRoot.markAsDirty(); //issues as been moved

      if  (JSP.ex(restState.getEntry("SCRUM_STAND_UP_MEETING_TIME"))){

        long startTime = restState.getEntryAndSetRequired("SCRUM_STAND_UP_MEETING_TIME").timeValueInMillis();

        // create the daily meeting
        CompanyCalendar cc= new CompanyCalendar();
        cc.setAndGetTimeToDayStart();
        cc.setTimeInMillis(startTime);
        ScheduleDaily sw = new ScheduleDaily(cc.getTime(), (int) (15 * CompanyCalendar.MILLIS_IN_MINUTE), 1, 30, true, null);
        sw.setIdAsNew();
        sw.store();
        Event ev = new Event();
        ev.setIdAsNew();

        String meeting= restState.getEntry("SCRUM_STAND_UP_MEETING_DESC").stringValueNullIfEmpty();
        if (!JSP.ex(meeting))
          meeting= I18n.get("STANDUP_MEETING");
        ev.setSummary(meeting);
        invalidClientEntries = !ActionUtilities.setString(restState.getEntry("SCRUM_STAND_UP_MEETING_LOCATION"), ev, "location");
        ev.setAuthor(loggedPerson);
        ev.setSchedule(sw);
        ev.getTargets().addAll(assignee);
        ev.addReference(sprint);
        ev.store();
      }

      String message="<b>"+I18n.get("SCRUM_SPRINT_CREATED")+": "+scrumRoot.getDisplayName()+"</b> - "+I18n.get("SCRUM_SPRINT_CREATED_TEXT")+"<br>";
      restState.addMessageInfo(message);
    }

  }


  void cmdSaveRoot() throws PersistenceException, org.jblooming.security.SecurityException, ActionException {

    boolean invalidClientEntries;
    // controller/action
    TeamworkOperator logged = (TeamworkOperator) restState.getLoggedOperator();
    Person loggedPerson = logged.getPerson();
    logged.testPermission(TeamworkPermissions.project_canCreate);

    // get scrum roles
    RoleTeamwork productOwner = null;
    RoleTeamwork scrumMaster = null;
    RoleTeamwork scrumStakeholder = null;
    RoleTeamwork scrumTeam = null;

    Set<Area> areas = logged.getAreasForPermission(TeamworkPermissions.project_canCreate);
    String hql = "from " + RoleTeamwork.class.getName() + " as role where role.area in (:areas)";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setParameterList("areas", areas);

    List<RoleTeamwork> roles = oql.list();
    for (RoleTeamwork role : roles) {

      if (role.getName().equalsIgnoreCase("Product Owner"))
        productOwner = role;
      if (role.getName().equalsIgnoreCase("Scrum Master"))
        scrumMaster = role;
      if (role.getName().equalsIgnoreCase("Scrum Stakeholder"))
        scrumStakeholder = role;
      if (role.getName().equalsIgnoreCase("Scrum Team"))
        scrumTeam = role;
    }

    Task task = new Task();
    task.setIdAsNew();
    task.setStartIsMilestone(false);
    task.setEndIsMilestone(false);
    task.setArea(logged.getDefaultAreaForPermission(TeamworkPermissions.project_canCreate));
    task.setStatus(TaskStatus.STATUS_ACTIVE);
    task.setTags("SCRUM");


    Date start = null;
    Date end = null;
    try {
      start = restState.getEntryAndSetRequired("SCRUM_START").dateValue();
      end = restState.getEntryAndSetRequired("SCRUM_END").dateValue();
      if (end != null && start != null && end.getTime() < start.getTime()) {
        invalidClientEntries = true;
        restState.getEntry("SCRUM_END").errorCode = restState.getI18n("END_MUST_BE_AFTER_START");
      }
    } catch (ActionException e) {
      invalidClientEntries = true;
    } catch (ParseException e) {
      invalidClientEntries = true;
    }

    invalidClientEntries = !ActionUtilities.setString(restState.getEntryAndSetRequired("SCRUM_TASK_NAME"), task, "name");
    String code = restState.getEntry("SCRUM_TASK_CODE").stringValueNullIfEmpty();
    if (code == null)
      code = "-";
    task.setCode(code);

    ActionUtilities.setString(restState.getEntry("TASK_DESCRIPTION"), task, "description");

    ActionUtilities.setInt(restState.getEntry("SCRUM_DURATION"), task, "duration");
    ActionUtilities.setBoolean(restState.getEntry("STARTISMILESTONE"), task, "startIsMilestone");
    ActionUtilities.setBoolean(restState.getEntry("ENDISMILESTONE"), task, "endIsMilestone");

    if (!invalidClientEntries && restState.validEntries()) {

      Period p = new Period(start, end);
      p.setIdAsNew();
      p.store();
      task.setSchedule(p);

      task.store();

      long wdcip = 0;

      //assign product owner
      String assId = restState.getEntryAndSetRequired("PRODUCT_OWNER").stringValue();
      Assignment assig = new Assignment();
      assig.setIdAsNew();
      assig.setResource((Resource) PersistenceHome.findByPrimaryKey(Resource.class, assId));
      assig.setRole(productOwner);
      assig.setOwner(restState.getLoggedOperator());
      assig.setTask(task);
      assig.setActivity(Assignment.ACTIVITY_ALL_IN_ONE);
      assig.setEnabled(true);
      assig.setEstimatedWorklog(wdcip * 8 * CompanyCalendar.MILLIS_IN_HOUR);
      assig.store();

      //assignm scrum master
      assId = restState.getEntryAndSetRequired("SCRUM_MASTER").stringValue();
      assig = new Assignment();
      assig.setIdAsNew();
      assig.setResource((Resource) PersistenceHome.findByPrimaryKey(Resource.class, assId));
      assig.setRole(scrumMaster);
      assig.setOwner(restState.getLoggedOperator());
      assig.setTask(task);
      assig.setActivity(Assignment.ACTIVITY_ALL_IN_ONE);
      assig.setEnabled(true);
      assig.setEstimatedWorklog(wdcip * 8 * CompanyCalendar.MILLIS_IN_HOUR);
      assig.store();



      //assignm scrum stakeholder
      for (int i = 0; i < 3; i++) {
        assId = restState.getEntry("SCRUM_STAKEHOLDER_" + i).stringValueNullIfEmpty();
        if (assId != null) {
          assig = new Assignment();
          assig.setIdAsNew();
          assig.setResource((Resource) PersistenceHome.findByPrimaryKey(Resource.class, assId));
          assig.setRole(scrumStakeholder);
          assig.setOwner(restState.getLoggedOperator());
          assig.setTask(task);
          assig.setActivity(Assignment.ACTIVITY_ALL_IN_ONE);
          assig.setEnabled(true);
          assig.setEstimatedWorklog(wdcip * 8 * CompanyCalendar.MILLIS_IN_HOUR);
          assig.store();
        }
      }

      //assignm scrum team
      for (int i = 0; i < 7; i++) {
        assId = restState.getEntry("SCRUM_TEAM_" + i).stringValueNullIfEmpty();
        if (assId != null) {
          assig = new Assignment();
          assig.setIdAsNew();
          assig.setResource((Resource) PersistenceHome.findByPrimaryKey(Resource.class, assId));
          assig.setRole(scrumTeam);
          assig.setOwner(restState.getLoggedOperator());
          assig.setTask(task);
          assig.setActivity(Assignment.ACTIVITY_ALL_IN_ONE);
          assig.setEnabled(true);
          assig.setEstimatedWorklog(wdcip * 8 * CompanyCalendar.MILLIS_IN_HOUR);
          assig.store();
        }
      }

      restState.setMainObject(task);
    }

  }

}

