package com.twproject.task.businessLogic;

import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Person;
import com.twproject.resource.Resource;
import com.twproject.resource.ResourceBricks;
import com.twproject.resource.businessLogic.ResourceAction;
import com.twproject.security.RoleTeamwork;
import com.twproject.security.TeamworkPermissions;
import com.twproject.task.*;
import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.agenda.Period;
import org.jblooming.designer.DesignerField;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.ontology.businessLogic.DeleteHelper;
import org.jblooming.ontology.SerializedMap;
import org.jblooming.ontology.Identifiable;
import org.jblooming.operator.Operator;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.security.Role;
import org.jblooming.security.SecurityException;
import org.jblooming.utilities.*;
import org.jblooming.waf.ActionSupport;
import org.jblooming.waf.ActionUtilities;
import org.jblooming.waf.constants.*;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.html.input.Selector;
import org.jblooming.waf.html.input.SmartCombo;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.RestState;

import java.io.Serializable;
import java.text.ParseException;
import java.util.*;


public class AssignmentAction extends ActionSupport {


  public TeamworkOperator logged;
  private Task task;
  private Assignment assig;


  public AssignmentAction(RestState pageState) {
    super(pageState);
    this.logged = (TeamworkOperator) pageState.getLoggedOperator();
  }

  public void editNoMake() throws PersistenceException, SecurityException {
    Assignment assig = Assignment.load(restState.mainObjectId);
    this.assig=assig;
    restState.setMainObject(assig);

    // se c'è già su assig uso quello. robik 2015 04 02
    if (assig!=null && assig.getTask()!=null)
      restState.addClientEntry("TASK_ID",assig.getTask());
    loadTask();
  }

  public void loadTask() throws PersistenceException, SecurityException {
    this.task = Task.load(restState.getEntry("TASK_ID").intValueNoErrorCodeNoExc() + "");
    if (task != null) {
      SecurityException.riseExceptionIfNoPermission(task.hasPermissionFor(logged,TeamworkPermissions.task_canRead), TeamworkPermissions.task_canRead, task);
    }
    restState.attributes.put("REFERRAL_OBJECT", task);
  }


  public void makeAssignment(Assignment assig) {
    restState.addClientEntry("ACTIVITY", assig.getActivity());

    if (assig.getResource() != null)
      restState.addClientEntry("ASSIGNEE", assig.getResource().getId());

    if (assig.getRole() != null)
      restState.addClientEntry("ASSIG_ROLE", assig.getRole().getId());

    if (assig.getCostCenter() != null)
      restState.addClientEntry("COST_COSTCENTER", assig.getCostCenter().getId());

    restState.addClientEntry("ASSIG_DESCRIPTION", assig.getDescription());
    restState.addClientEntry("ASSIG_ENABLED", assig.isEnabled() ? Fields.TRUE : Fields.FALSE);
    restState.addClientEntryTime("ESTIMATE_WORKLOG", assig.getEstimatedWorklog());
    restState.addClientEntryCurrency("ASSIG_COST", assig.getHourlyCost());
    restState.addClientEntryCurrency("ASSIG_BUDGET", assig.getBudget());
    restState.addClientEntry("ASSIG_RISK", assig.getRisk());

    long stamanPrest = Period.getDayPeriodInstance(new Date()).getStartDate().getTime();
    // get first millis of today
    restState.addClientEntry("ASSPRIORITY", assig.getPriorityAtTime(stamanPrest));

  }

  public void cmdAdd() throws PersistenceException, SecurityException {
    loadTask();
    //SecurityException.riseExceptionIfNoPermission(task.bricks.canManageAssignment, TeamworkPermissions.assignment_manage, task);
    task.testPermission(logged, TeamworkPermissions.assignment_canCRW);


    Assignment assig = new Assignment();
    assig.setIdAsNew();
    assig.setTask(task); // added by robik 28/09/2008
    restState.setMainObject(assig);
    restState.addClientEntry("ACTIVITY", Assignment.ACTIVITY_ALL_IN_ONE);
    restState.addClientEntry("ASSIG_ENABLED", Fields.TRUE);
  }


  public void cmdSave() throws PersistenceException, ActionException, SecurityException {

    editNoMake();

    //SecurityException.riseExceptionIfNoPermission(task.bricks.canManageAssignment, TeamworkPermissions.assignment_manage, task);
    task.testPermission(logged, TeamworkPermissions.assignment_canCRW);

    Assignment assig;

    Serializable assId = restState.mainObjectId;
    boolean isNew = PersistenceHome.NEW_EMPTY_ID.equals(assId) || assId == null ;
    boolean resourceChanged=false;
    if (isNew) {
      assig = new Assignment();
      assig.setIdAsNew();
      assig.setOwner(logged);
    } else
      assig = (Assignment) PersistenceHome.findByPrimaryKey(Assignment.class, assId);


    Resource oldResource=assig.getResource(); //in order to remove notification in case of changing assig

    restState.setMainObject(assig);

    assig.setTask(task);


    ClientEntry assCe = restState.getEntry("ASSIGNEE");
    //support for adding resources on the fly
    boolean canCreateResource = task.getArea().hasPermissionFor(logged, TeamworkPermissions.resource_canCreate);
    if (!canCreateResource) {
      assCe.required = true;
    }

    String resId = assCe.stringValue().trim();
    Resource assignee = Resource.load(resId);
    if (assignee==null) {
      // recover the text part
      String assName = restState.getEntryAndSetRequired("ASSIGNEE" + SmartCombo.TEXT_FIELD_POSTFIX).stringValue();
      int pos = assName.indexOf(' ');
      String name = "";
      String surname = "";
      if (pos > -1) {
        name = assName.substring(0, pos);
        surname = assName.substring(pos + 1);
      } else {
        surname = assName;
      }
      String login = (name.length() > 0 ? name.charAt(0) : "") + surname;
      String password = (name.length() > 0 ? name : "1" + surname);

      // check if the resource already exists
      String resFilter = "select resource from " + Person.class.getName() + " as resource where coalesce(resource.personName,'') || coalesce(resource.personSurname,'') = :cn" +
              " or coalesce(resource.personSurname,'') || coalesce(resource.personName,'') =:cn";
      OqlQuery oql = new OqlQuery(resFilter);
      oql.getQuery().setString("cn", name + surname);
      List<Person> perss = oql.list();

      // look finding a resource where I have permissions
      for (Person p : perss) {
        if (p.hasPermissionFor(logged, TeamworkPermissions.resource_manage)) {
          assignee = p;
          break;
        }
      }

      // check if I've found one
      if (assignee == null) {

        // if there is no res found create it
        if (perss.size() <= 0) {
          // inject CE
          restState.addClientEntry("RESOURCE_TYPE", Person.class.getName());
          restState.addClientEntry("AREA", task.getArea());
          restState.addClientEntry(OperatorConstants.FLD_NAME, name);
          restState.addClientEntry(OperatorConstants.FLD_SURNAME, surname);
          restState.addClientEntry("LOGIN_NAME", login);
          restState.addClientEntry(OperatorConstants.FLD_IS_ENABLED, Fields.TRUE);
          restState.addClientEntry("PWD", password);
          restState.addClientEntry("PWD_RETYPE", password);
          restState.addClientEntry("PWD_RETYPE", password);

          restState.addClientEntry("ANAGRAPHICAL_ID", PersistenceHome.NEW_EMPTY_ID);
          restState.addClientEntry("LOCATION_DESCRIPTION", "-");

          restState.addClientEntry("WORK_DAILY_CAPACITY", CompanyCalendar.MILLIS_IN_WORKING_DAY);
          restState.addClientEntry("HOURLY_COST", CompanyCalendar.MILLIS_IN_WORKING_DAY);


          //add the operational role if ex
          restState.mainObjectId = PersistenceHome.NEW_EMPTY_ID;
          ResourceAction ra = new ResourceAction(restState);
          Role operationalRole = ra.getOperationalRole(logged.getPerson());
          if (operationalRole != null) {

            TreeMap<String, String> ctm = new TreeMap<String, String>();
            TreeMap<String, String> candTm = new TreeMap<String, String>();
            if (operationalRole != null)
              ctm.put(operationalRole.getId().toString(), operationalRole.getDisplayName());
            Selector.make("roles", candTm, ctm, restState);
          }

          try {
            ra.cmdSave();
            // get the just created resource
            assignee = (Resource) restState.getMainObject();

            //in order to create operator
            restState.mainObjectId=assignee.getId();
            ra.cmdSaveSecurity();

            // info message
            PageSeed resEd = restState.pageFromApplications("teamwork/resource/resourceEditor.jsp");
            resEd.mainObjectId = assignee.getId();
            resEd.command = Commands.EDIT;
            ButtonLink blRes = new ButtonLink(resEd);
            blRes.label = I18n.get("CLICK_HERE_TO_EDIT");
            //blRes.toPlainLink();
            restState.addMessageInfo(I18n.get("RESOURCE_%%_CREATED_WITH_LOGIN_%%_AND_PASSWORD", assignee.getDisplayName(), login, password) + "&nbsp;&nbsp;" + blRes.toPlainLink());


          } catch (ApplicationException e) {
            assCe.errorCode = "PROBLEMS_CREATING_RESOURCE";
            throw new ActionException(e);
          }

          //restore mainObject and mainObjectId
          restState.setMainObject(assig);

        } else {
          // here there is already a resource with that name, but you have no rights on it
          assCe.errorCode = "YOU_CANNOT_ASSIGN_THIS_RESOURCE";
          throw new ActionException("YOU_CANNOT_ASSIGN_THIS_RESOURCE");
        }
      }
    }

    assig.setResource(assignee);

    //si controlla che effettivamente si abbia il permesso di assegnare quella risorsa. Infatti nel caso di assenza per permesso assignment_canManage sul task potresti essere manager della risorsa
    assig.testPermission(logged, TeamworkPermissions.resource_manage);

    if (assignee!=null && oldResource!=null && !assignee.equals(oldResource))
      resourceChanged=true;

    try {
      String roleId = restState.getEntryAndSetRequired("ASSIG_ROLE").stringValue();
      assig.setRole(RoleTeamwork.load(roleId));
    } catch (ActionException e) {
    }

    assig.setActivity(restState.getEntry("ACTIVITY").stringValueNullIfEmpty());

    assig.setDescription(restState.getEntry("ASSIG_DESCRIPTION").stringValueNullIfEmpty());
    try {
      assig.setEstimatedWorklog(restState.getEntry("ESTIMATE_WORKLOG").durationInWorkingMillis(true));
    } catch (ActionException e) {
    } catch (ParseException e) {
    }
    assig.setEnabled(restState.getEntry("ASSIG_ENABLED").checkFieldValue());

    ActionUtilities.setCurrency(restState.getEntry("ASSIG_BUDGET"),assig,"budget");

    double dbg;
    try {
      if (JSP.ex(restState.getEntry("ASSIG_COST"))){
        dbg = restState.getEntry("ASSIG_COST").currencyValue();
        assig.setHourlyCost(dbg);
      } else if (assig.isNew() && assig.getHourlyCost()==0){
        //si recupera dalla risorsa se si hanno i permessi, altrimenti si usa il default
        if (assignee.hasPermissionFor(logged,TeamworkPermissions.resource_cost_canRead) && assignee.getHourlyCost() > 0)
          assig.setHourlyCost( assignee.getHourlyCost());
        else
          assig.setHourlyCost( ResourceBricks.getDefaultHourlyCost());
      }
    } catch (ParseException e) {
    } catch (ActionException e) {
    }



    try {
      assig.setRisk(restState.getEntry("ASSIG_RISK").intValue());
    } catch (ActionException e) {
    } catch (ParseException e) {
    }

    //Custom fields
    DesignerField.saveCustomFields( "ASSIGNMENT_CUSTOM_FIELD_",6,assig, restState);


    if (restState.validEntries()) {
      assignee = (Resource) ReflectionUtilities.getUnderlyingObject(assignee);

      //remove existing listener attached to the oldResource
      if (resourceChanged && oldResource.getMyself()!=null)
        task.bricks.removeListeners(oldResource.getMyself());


      if (isNew) {
        assig.setOwner(logged);
      }
      if (assignee instanceof Person) {
        Person p = (Person) assignee;
        TeamworkOperator teamworkOperator = p.getMyself();
        if (teamworkOperator != null) {
          saveSubs(task, logged, teamworkOperator);
        }
      }

      if (restState.getEntry("COST_COSTCENTER").stringValueNullIfEmpty() == null && assig.isNew()) {
        if (assignee.getMyCostAggregator() != null)
          restState.getEntry("COST_COSTCENTER").setValue(assignee.getMyCostAggregator().getId() + "");
      }
      ActionUtilities.setIdentifiable(restState.getEntry("COST_COSTCENTER"), assig, "costCenter");

      // priority
      long stamanPrest = Period.getDayPeriodInstance(new Date()).getStartDate().getTime();

      assig.store();

      AssignmentAction.updateAssignmentPriority(assig, restState.getEntry("ASSPRIORITY").intValueNoErrorCodeNoExc(), stamanPrest);

      // manually add the assig in order to make it available in the inverse collection
      task.getAssignments().add(assig);

      if (isNew) {
        // notify the assignee
        String prefix = "ASSIGNEE_NOTIFY_";
        List<MessagingSystem.Media> mediaSubscribed = MessagingSystem.mediaSubscribedList(prefix, restState);
        for (MessagingSystem.Media media : mediaSubscribed) {
          assig.generateAssignmentMessage(logged, media);
        }
      }

      // ok message feedback
      if (isNew)
        restState.addMessageOK(I18n.get("ASSIGNMENT_CORRECTLY_CREATED") + ": <b>" + assig.getResource().getDisplayName() + "</b>");
      else
        restState.addMessageOK(I18n.get("ASSIGNMENT_CORRECTLY_SAVED"));


    } else
      throw new ActionException();
  }

  public void cmdEdit() throws PersistenceException, SecurityException {
    editNoMake();
    task.testPermission(logged, TeamworkPermissions.task_canRead);
    makeAssignment(assig);
  }


  public static void updateAssignmentPriority(Assignment assignment, int newPriority, long stamanPrest) throws StoreException {
    if (!assignment.isNew()) {
      assignment.removePriority(stamanPrest);
    }
    int pri = assignment.getPriorityAtTime(stamanPrest);

    if (pri != newPriority) {
      // create a new AssignmentPriority
      AssignmentPriority ap;
      ap = new AssignmentPriority();
      ap.setAssignment(assignment);
      ap.setCutPoint(stamanPrest);
      ap.setPriority(newPriority);
      ap.store();
      assignment.addPriorityInMemory(ap);

    }
  }


  public void cmdSaveSubs(Operator logged, Operator operator) throws PersistenceException, ActionException, SecurityException {
    editNoMake();
    saveSubs(task, logged, operator);
    // ok message feedback
    if (restState.validEntries())
      restState.addMessageOK(I18n.get("SUBSCRIPTIONS_CORRECTLY_SAVED"));

  }

  public void saveSubs(Task currentTask, Operator loggedOperator, Operator subscriber) throws PersistenceException, SecurityException {
    currentTask.testPermission(loggedOperator, TeamworkPermissions.task_canRead);

    //remove existing
    currentTask.bricks.removeListeners(subscriber);
    //create new
    boolean tnd = restState.getEntry("TASK_NOTIFY_DESC").checkFieldValue();

    // register listeners
    for (Task.Event event : Task.Event.values()) {
      String mediaSubscribed = MessagingSystem.mediaSubscribed(event + "_", restState);
      if (mediaSubscribed.length() > 0 && !currentTask.bricks.isSomeAncestorListeningAndPropagatingForTheEvent(subscriber, event)) {
        currentTask.bricks.createListener(event, tnd, subscriber, mediaSubscribed);
      }
    }
  }


  public void cmdMoveAssignment() throws PersistenceException, SecurityException {
    Assignment assig = Assignment.load(restState.mainObjectId);
    Task newTask = (Task.load(restState.getEntry("ASSIG_NEW_TASK").intValueNoErrorCodeNoExc()+""));
    //if (assig!=null && newTask!=null && newTask.hasPermissionFor(logged,TeamworkPermissions.assignment_manage)){
    if (assig!=null && newTask!=null && newTask.hasPermissionFor(logged,TeamworkPermissions.assignment_canCRW)){

      //si deve aggiornare il task vecchio e le sue inverse
      assig.getTask().getAssignments().remove(assig);
      assig.getTask().markAsDirty();

      assig.setTask(newTask);
      assig.store();
      restState.setMainObject(assig);
      restState.addMessageOK(I18n.get("ASSIGNMENT_MOVED"));
    }
  }


  public void cmdDelete() throws SecurityException, PersistenceException {
    editNoMake();
    //SecurityException.riseExceptionIfNoPermission(canDelete, TeamworkPermissions.assignment_manage, task);
    task.testPermission(logged, TeamworkPermissions.assignment_canCRW);

    Assignment delenda = Assignment.load(restState.mainObjectId);
    if (task.isProcessDriven() && JSP.ex(delenda.getExternalCode()))
      throw new PlatformRuntimeException("Process-managed assignments cannot be removed");

    //remove existing listener attached to the Resource
    TeamworkOperator assigOp = delenda.getResource().getMyself();
    if (assigOp !=null)
      task.bricks.removeListeners(assigOp);

    DeleteHelper.cmdDelete(delenda, restState);
  }

  /**
   *
   * @param task
   * @param assignee
   * @param role
   * @return null se non c'è l'assegnazione e se non puoi crearla
   */
  public Assignment getOrCreateAssignment(Task task, Resource assignee, RoleTeamwork role) throws PersistenceException, SecurityException, ActionException {
    Assignment ret=task.getFirstAssignmentsForResource(assignee);

    if (assignee != null && task != null && ret==null) {
      // if you have permission to create assignment do it
      if (task.hasPermissionFor(logged, TeamworkPermissions.assignment_canCRW)) {

        // create the assign on task
        Assignment ass = new Assignment();
        ass.setResource(assignee);

        //si recupera dalla risorsa se si hanno i permessi, altrimenti si usa il default
        if (assignee.hasPermissionFor(logged,TeamworkPermissions.resource_cost_canRead) && assignee.getHourlyCost() > 0)
          ass.setHourlyCost( assignee.getHourlyCost());
        else
          ass.setHourlyCost( ResourceBricks.getDefaultHourlyCost());

        ass.setTask(task);
        ass.setOwner(logged);
        ass.setEnabled(true);
        
        if(role==null)
          role=TaskBricks.getWorkerRole(task.getArea());

        if (role==null) {
          restState.addMessageError(I18n.get("DEFAULT_ROLE_NOT_FOUND_%%", ApplicationState.getApplicationSetting("DEFAULT_WORKER_ROLE_NAME", "Worker")));
          return null;
        }

        ass.setRole(role);
        ass.store();
        ret=ass;
        long stamanPrest = Period.getDayPeriodInstance(new Date()).getStartDate().getTime();
        AssignmentAction.updateAssignmentPriority(ass, AssignmentPriority.PRIORITY_LOW, stamanPrest);


        //subscription and notification
        if (ass.getResource().getMyself() != null) {
          AssignmentAction assignmentAction = new AssignmentAction(restState);
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
          //impaninating taskid
          Serializable oldId = restState.mainObjectId;
          Identifiable oldMain = restState.getMainObject();
          restState.mainObjectId = ass.getId();
          restState.addClientEntry("TASK_ID", task);
          assignmentAction.cmdSaveSubs(assignmentAction.logged, ass.getResource().getMyself());
          restState.setMainObject(oldMain);
          restState.mainObjectId = oldId; // occhio: si ripristina il valore precedente
        }
        // show message informing about assignment creation
        PageSeed ps = restState.pageFromApplications("teamwork/task/taskAssignmentEditor.jsp");
        ps.addClientEntry("TASK_ID", ass.getTask().getId());
        ps.mainObjectId = ass.getId();
        ps.command = Commands.EDIT;

        restState.addMessageInfo(I18n.get("NEW_ASSIGNMENT_CREATED_FOR_%%", ass.getResource().getDisplayName(), task.getDisplayName()) + "<br><a href='" + ps.toLinkToHref() + "'>" + restState.getI18n("CLICK_HERE_TO_EDIT") + "</a>");

      } else {
        // show a warning message
        restState.addMessageWarning(I18n.get("ISSUE_ASSIGNEE_NOT_IN_TASK", assignee.getDisplayName(), task.getDisplayName()));
      }
    }
    return ret;
    
  }
}