package com.twproject.task.businessLogic;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.jblooming.ApplicationException;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.agenda.Period;
import org.jblooming.designer.DesignerField;
import org.jblooming.messaging.EventListenerMatcher;
import org.jblooming.messaging.Listener;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.messaging.SomethingHappened;
import org.jblooming.ontology.PerformantNode;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.ontology.businessLogic.DeleteHelper;
import org.jblooming.oql.OqlQuery;
import org.jblooming.oql.QueryHelper;
import org.jblooming.page.HibernatePage;
import org.jblooming.page.ListPage;
import org.jblooming.page.Page;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.security.Area;
import org.jblooming.security.SecurityException;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.HashTable;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.ActionSupport;
import org.jblooming.waf.ActionUtilities;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.constants.OperatorConstants;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.html.display.DataTable;
import org.jblooming.waf.html.display.Paginator;
import org.jblooming.waf.html.input.SmartCombo;
import org.jblooming.waf.html.state.Form;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.state.PersistentSearch;
import org.jblooming.waf.view.ClientEntries;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.RestState;

import com.twproject.operator.TeamworkOperator;
import com.twproject.rank.EntityGroupRank;
import com.twproject.rank.Hit;
import com.twproject.rank.RankUtilities;
import com.twproject.resource.Person;
import com.twproject.resource.Resource;
import com.twproject.security.TeamworkPermissions;
import com.twproject.task.Assignment;
import com.twproject.task.Issue;
import com.twproject.task.IssueBricks;
import com.twproject.task.IssueHistory;
import com.twproject.task.IssueImpact;
import com.twproject.task.IssueStatus;
import com.twproject.task.Task;
import com.twproject.task.TaskStatus;
import com.twproject.task.TaskType;
import com.twproject.worklog.Worklog;

import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

public class IssueAction extends ActionSupport {

  public TeamworkOperator logged;
  public Issue issue;

  public IssueAction(RestState pageState) {
    super(pageState);
    this.logged = (TeamworkOperator) pageState.getLoggedOperator();

  }

  public void cmdAdd(boolean isAClone) throws PersistenceException, org.jblooming.security.SecurityException {

    issue = new Issue();
    issue.setIdAsNew();
    restState.setMainObject(issue);
    ActionUtilities.setIdentifiable(restState.getEntry("ISSUE_TASK"), issue, "task");

    Person person = logged.getPerson();


    if (issue.getTask() != null) {
      issue.testPermission(logged, TeamworkPermissions.issue_canCreate);
      restState.addClientEntry("ASSIGNEE_FILTER", "ASSIGNEE_FROM_TASK");

    } /*else {
      //you always can add for yourself
      restState.addClientEntry("ASSIGNEE", person.getId());
      //logged.testPermission(TeamworkPermissions.issue_canCreate);
    }*/

    issue.setGravity(Issue.GRAVITY_MEDIUM);
    issue.setOwner(logged);

    // reset these fields only if not coming from clone
    if (!isAClone) {
      restState.addClientEntry("ASSIGNED_BY", person.getId());
      restState.addClientEntry("ISSUE_STATUS", IssueStatus.getStatusOpen());
      restState.addClientEntry("ISSUE_DATE_SIGNALLED", new Date());
    }

  }

  public Issue editNoMake() throws PersistenceException, SecurityException {
    issue = Issue.load(restState.getMainObjectId() + "");
		if (issue != null) {
			System.out.println(issue.getCode());
		}
		System.out.println("----");
    issue.testPermission(logged, TeamworkPermissions.issue_canRead);
    restState.setMainObject(issue);
    Hit.getInstanceAndStore(issue, logged, .1);
    return issue;
  }

  public void cmdEdit() throws PersistenceException, SecurityException {
    editNoMake();
    SecurityException.riseExceptionIfNoPermission(issue.hasPermissionFor(logged,TeamworkPermissions.issue_canRead), TeamworkPermissions.issue_canRead, issue);
    make(issue);
  }

  public void cmdGuess() throws PersistenceException, SecurityException, ActionException {
    //String originalRef = pageState.mainObjectId+"";
    issue = null;
    //first eventually try code
    if (Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("USECODEONISSUES"))) {
      try {
        issue = (Issue) PersistenceHome.findUnique(Issue.class, "code", restState.mainObjectId);
      } catch (PersistenceException p) {
        throw new ActionException("REF_NOT_UNIQUE");
      }
    }

    if (issue == null)
      issue = Issue.load(restState.getMainObjectId() + "");

    if (issue != null) {
      // il guess può essere chiamato sia sulla issueMulti che sulla IssueEdit
      // quindi si fa sia il make che il find, così sono tutto contenti!

      //security
      restState.mainObjectId = issue.getId();
      editNoMake();
      if (!issue.hasPermissionFor(logged,TeamworkPermissions.issue_canRead))
        throw new ActionException("REF_PERMISSION_LACKING");

      //edit part
      restState.mainObjectId = issue.getId();
      make(issue);

      //issueMulti part
      restState.addClientEntry("ISSUE_ID", issue.getId());
      cmdFind();


    } else {
      throw new ActionException("REF_NOT_FOUND");
    }

  }


  private void make(Issue issue) throws PersistenceException {

    restState.addClientEntry("ISSUE_DESCRIPTION", issue.getDescription());
    //if (issue.getNotes()!=null && JSP.ex(issue.getNotes().getText()))
    //restState.addClientEntry("ISSUE_NOTES", issue.getNotes());
    //else
    //  pageState.addClientEntry("ISSUE_NOTES","");
    restState.addClientEntry("ISSUE_TASK", issue.getTask());
    restState.addClientEntry("ISSUE_CODE", issue.getCode());
    restState.addClientEntry("ISSUE_TYPE", issue.getType());
    restState.addClientEntry("ISSUE_GRAVITY", issue.getGravity());
    restState.addClientEntry("ISSUE_TAGS", issue.getTags());
    //pageState.addClientEntry("ISSUE_PRIORITY", issue.getOrderFactor());
    restState.addClientEntry("ISSUE_IMPACT", issue.getImpact());
    restState.addClientEntry("ASSIGNEE", issue.getAssignedTo());
    restState.addClientEntry("ASSIGNED_BY", issue.getAssignedBy());
    restState.addClientEntry("ISSUE_STATUS", issue.getStatus());
    restState.addClientEntryTime("ISSUE_WORKLOG_ESTIMATED_TIME", issue.getEstimatedDuration());
    restState.addClientEntry("ISSUE_DATE_CLOSE_BY", issue.getShouldCloseBy());
    restState.addClientEntry("ISSUE_DATE_SIGNALLED", issue.getDateSignalled());
    restState.addClientEntry("EXT_REQUESTER_EMAIL", issue.getExtRequesterEmail());


    if (!issue.isNew()) {
      restState.addClientEntry("ISSUE_WORKLOG_ACTION", I18n.get("ISSUE_PREFILLED_ACTION") + " " + JSP.w(issue.getDescription()));
      long durationDelta = issue.getEstimatedDuration() - issue.getWorklogDone();
      if (durationDelta >= 0)
        restState.addClientEntryTime("ISSUE_WORKLOG_DELTA_ESTIMATED_TIME", durationDelta);

    }

    if (JSP.ex(issue.getFiles()))
      restState.addClientEntry("ISSUE_FILES", issue.getJsonData().getJSONArray("__files__"));


    String hql = "select listener from " + Listener.class.getName() + " as listener where listener.owner=:own and listener.identifiableId=:iid and listener.theClass=:tcl";
    QueryHelper listenerQH = new QueryHelper(hql);
    listenerQH.addParameter("own", logged);
    listenerQH.addParameter("iid", issue.getId().toString());
    listenerQH.addParameter("tcl", Issue.class.getName());
    Listener l = (Listener) listenerQH.toHql().uniqueResultNullIfEmpty();
    if (l != null) {
      List<String> medias = StringUtilities.splitToList(l.getMedia(), ",");
      for (String media : medias) {
        restState.addClientEntry("ISSUE_SUBSCRIBE_CLOSE_" + media, Fields.TRUE);
      }
    }
  }

  public boolean cmdSave() throws PersistenceException, ActionException, ApplicationException, SecurityException {

    Resource previousAssignee = null;

    issue = null;
    TeamworkOperator teamworkOperator = logged;
    Person loggedPerson = teamworkOperator.getPerson();


    if (PersistenceHome.NEW_EMPTY_ID.equals(restState.mainObjectId)) {
      issue = new Issue();
      issue.setIdAsNew();
      issue.setOwner(logged);
    } else {
      issue = Issue.load(restState.getMainObjectId() + "");
    }

    // this is created and only if something relevant changes it will be stored and set (transiently) to the issue
    IssueHistory history = new IssueHistory(issue);

    //create a issue textual copy
    String oldIssue = issue.getAbstractForIndexing();

    restState.setMainObject(issue);

    // hack to manage issue added from issueMultiEditor: in this case the issue is always created before and the id is not new, but the description is empty
    boolean isNew = issue.isNew() || !JSP.ex(issue.getDescription());

    ActionUtilities.setString(restState.getEntryAndSetRequired("ISSUE_DESCRIPTION"), issue, "description");
    try {
      issue.setDescription(restState.getEntryAndSetRequired("ISSUE_DESCRIPTION").stringValue());
    } catch (ActionException a) {
    }

    if (!isNew)
      previousAssignee = issue.getAssignedTo();

    Task oldTask = issue.getTask();
    ActionUtilities.setIdentifiable(restState.getEntry("ISSUE_TASK"), issue, "task");

    if (oldTask != null && !oldTask.equals(issue.getTask())) {
      oldTask.markAsDirty(); // this is an asynch call
    }

    //security should always be checked after task is set
    if (isNew) {
      issue.testPermission(logged, TeamworkPermissions.issue_canCreate);
    } else {
      issue.testPermission(logged, TeamworkPermissions.issue_canWrite);
    }

    ActionUtilities.setString(restState.getEntry("ISSUE_CODE"), issue, "code");
    ActionUtilities.setIdentifiable(restState.getEntry("ISSUE_TYPE"), issue, "type");
    ActionUtilities.setString(restState.getEntry("ISSUE_GRAVITY"), issue, "gravity");

    //ActionUtilities.setDouble(pageState.getEntry("ISSUE_PRIORITY"), issue, "priority");
    ActionUtilities.setIdentifiable(restState.getEntry("ISSUE_IMPACT"), issue, "impact");
    ActionUtilities.setIdentifiable(restState.getEntry("ASSIGNED_BY"), issue, "assignedBy");
    ActionUtilities.setDate(restState.getEntry("ISSUE_DATE_SIGNALLED"), issue, "dateSignalled");
    // è sempre in readonly  -- ActionUtilities.setString(restState.getEntry("EXT_REQUESTER_EMAIL"), issue, "extRequesterEmail");
    ActionUtilities.setDurationInMillis(restState.getEntry("ISSUE_WORKLOG_ESTIMATED_TIME"), true, issue, "estimatedDuration");

    //set tags: si ripuliscono, si rendono univoci e si riseparano con ", " occhio allo spazio!
    issue.setTags(StringUtilities.setToString(StringUtilities.splitToOrderSet(JSP.w(restState.getEntry("ISSUE_TAGS").stringValue()), ","),", ")  );

    //shouldCloseBy must be in the task interval
    try {
      ClientEntry ce = restState.getEntry("ISSUE_DATE_CLOSE_BY");
      Date date = ce.dateValue();
      if (date != null && issue.getTask() != null && issue.getTask().getSchedule() != null && !issue.getTask().getSchedule().contains(date)) {
        ce.errorCode = I18n.get("CLOSE_BY_OUT_OF_TASK_SCOPE") + ": " + issue.getTask().getDisplayName() + " " + DateUtilities.dateToString(issue.getTask().getSchedule().getStartDate()) + " - " + DateUtilities.dateToString(issue.getTask().getSchedule().getEndDate());
      }
      issue.setShouldCloseBy(date);

    } catch (ParseException e) {
    }

    //Custom ISSUE_CUSTOM_FIELD_
    DesignerField.saveCustomFields("ISSUE_CUSTOM_FIELD_", 6, issue, restState);

    // set area
    issue.setArea(teamworkOperator);


    // test code uniqueness
    if (Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("USEUNIQUECODES")) && JSP.ex(issue.getCode()) && !issue.isUnique("code")) {
      restState.getEntry("ISSUE_CODE").errorCode = I18n.get("KEY_MUST_BE_UNIQUE");
    }



    // ----------------------------------------------------------------------- START ASSIGNEE MANAGEMENT -----------------------------------------------------------------------
    Resource assignee = Resource.load(restState.getEntry("ASSIGNEE").stringValueNullIfEmpty() + ""); //if "null" no assignee :-)
    issue.setAssignedTo(assignee);

    // check if both task and assignee are null issues are by default assigned to logged alias TO-DO
    if (issue.getTask() == null & issue.getAssignedTo() == null) {
      issue.setAssignedTo(loggedPerson);
    }


    // ----------------------------------------------------------------------- STATUS (must be after assignee management for permission checking) -----------------------------------------------------------------------
    IssueStatus oldStatus = issue.getStatus();
    IssueStatus newStatus = IssueStatus.load(restState.getEntry("ISSUE_STATUS").intValueNoErrorCodeNoExc());
    if (newStatus == null) {
      newStatus = IssueStatus.getStatusOpen();
    }

    //si controlla di avere i permessi per cambiare lo status
    if (!newStatus.equals(oldStatus))
      issue.testPermission(logged, TeamworkPermissions.issue_canChangeStatus);

    issue.setIssueStatus(newStatus);

    if (!isNew) {
      long durationDelta = issue.getEstimatedDuration() - issue.getWorklogDone();
      if (durationDelta >= 0)
        restState.addClientEntryTime("ISSUE_WORKLOG_DELTA_ESTIMATED_TIME", durationDelta);
    }

    if (isNew) {
      // ----------------------------------------------------------------------- FILES IN CASE OF NEW ISSUE -----------------------------------------------------------------------
      //in case of new issue it could exists some pending files
      String s = restState.getEntry("PENDING_PF").stringValueNullIfEmpty();
      if (JSP.ex(s)) {
        JSONArray jsa = JSONArray.fromObject(s);
        for (Object o : jsa) {
          String uid = o.toString();
          PersistentFile pf = PersistentFile.deserialize(uid);
          if (pf != null) {
            issue.addFile(pf);
          }
        }
      }

      // ----------------------------------------------------------------------- ORDER IN CASE OF NEW ISSUE -----------------------------------------------------------------------
      if (JSP.ex(restState.getEntry("TEMP_ORDER"))) {
        int pos = restState.getEntry("TEMP_ORDER").intValueNoErrorCodeNoExc();
        boolean orderByResource = "BY_RESOURCE".equals(restState.getEntry("SORT_FLAVOUR").stringValueNullIfEmpty());
        if (orderByResource)
          issue.setOrderFactorByResource(pos);
        else
          issue.setOrderFactor(pos);
      }
    }

    if (restState.validEntries()) {
      if (PersistenceHome.NEW_EMPTY_ID.equals(restState.mainObjectId)) {
        issue.setOwner(logged);
      }


      issue.store();
      // save the history if needed
      if (isNew) {
        history = new IssueHistory(issue);
        history.store();
      } else {
        history.testChangedAndStore();
      }

      Hit.getInstanceAndStore(issue, logged, .2);
      //if there is the task, give it a little hit
      if (issue.getTask() != null)
        Hit.getInstanceAndStore(issue.getTask(), logged, .1);

      //remove all listener for owner
      String hql = "from " + Listener.class.getName() + " as listen where " +
        "listen.owner = :owner and listen.theClass = :theClass and listen.identifiableId = :identifiableId";
      OqlQuery oql = new OqlQuery(hql);
      oql.getQuery().setEntity("owner", teamworkOperator);
      oql.getQuery().setString("theClass", Issue.class.getName());
      oql.getQuery().setString("identifiableId", issue.getId().toString());
      List<Listener> delendi = oql.list();
      for (Listener l : delendi) {
        l.remove();
      }

      //recreate listener for owner
      String prefix = "ISSUE_SUBSCRIBE_CLOSE_";
      String mediaSubscribed = MessagingSystem.mediaSubscribed(prefix, restState);
      if (mediaSubscribed.length() > 0) {
        Listener l = new Listener(teamworkOperator);
        l.setIdAsNew();
        l.setIdentifiable(issue);
        l.setMedia(mediaSubscribed);
        l.setEventType(Issue.Event.ISSUE_CLOSE + "");
        l.setOneShot(true);
        //l.setValidityEnd();
        l.store();
      }

      //se è cambiato assegnatario
      if (issue.getAssignedTo() != null && (previousAssignee == null || !issue.getAssignedTo().equals(previousAssignee))) {
        createMessageForIssueAssigned(teamworkOperator, previousAssignee == null);
      }

      if (isNew) {
        createEventIssueAddedClosed(issue, isNew, teamworkOperator, teamworkOperator.getDisplayName());
      } else if (!newStatus.equals(oldStatus) && newStatus.isBehavesAsClosed()) {
        createEventIssueClosed(oldStatus, issue);
        createEventIssueAddedClosed(issue, isNew, teamworkOperator, teamworkOperator.getDisplayName());
      } else {
        //only if something is changed
        if (!oldIssue.equals(issue.getAbstractForIndexing()))
          createEventIssueUpdated(issue);
      }

      //eventually create assignment
      if (issue.getTask() != null)
        new AssignmentAction(restState).getOrCreateAssignment(issue.getTask(), assignee, null);
    }

    //manage feedback on issues
    if (!newStatus.equals(oldStatus) && newStatus.isAskForComment() && issue.getLastIssueHistory()!=null) {
      restState.addClientEntry("ASK_FOR_COMMENT", issue.getLastIssueHistory().getId());
    }

    if (!newStatus.equals(oldStatus) && newStatus.isAskForWorklog()) {
      restState.addClientEntry("ASK_FOR_WORKLOG", issue.getAssignmentOnTask(issue.getAssignedTo()));
    }

    return restState.validEntries();
  }



  public boolean cmdChangeStatus() throws PersistenceException, ActionException, ApplicationException, SecurityException {
    issue = null;
    TeamworkOperator teamworkOperator = logged;
    Person loggedPerson = teamworkOperator.getPerson();


    issue = Issue.load(restState.getMainObjectId() + "");

    // this is created and only if something relevant changes it will be stored and set (transiently) to the issue
    IssueHistory history = new IssueHistory(issue);

    restState.setMainObject(issue);

    // hack to manage issue added from issueMultiEditor: in this case the issue is always created before and the id is not new, but the description is empty
    boolean isNew = issue.isNew() || !JSP.ex(issue.getDescription());

    ActionUtilities.setString(restState.getEntryAndSetRequired("ISSUE_DESCRIPTION"), issue, "description");
    try {
      issue.setDescription(restState.getEntryAndSetRequired("ISSUE_DESCRIPTION").stringValue());
    } catch (ActionException a) {
    }

    // ----------------------------------------------------------------------- STATUS (must be after assignee management for permission checking) -----------------------------------------------------------------------
    IssueStatus oldStatus = issue.getStatus();
    IssueStatus newStatus = IssueStatus.load(restState.getEntry("ISSUE_STATUS").intValueNoErrorCodeNoExc());
    if (newStatus == null) {
      newStatus = IssueStatus.getStatusOpen();
    }

    //si controlla di avere i permessi per cambiare lo status
    if (!newStatus.equals(oldStatus))
      issue.testPermission(logged, TeamworkPermissions.issue_canChangeStatus);

    issue.setIssueStatus(newStatus);


    if (restState.validEntries()) {

      issue.store();
      // save the history if needed
      history.testChangedAndStore();

      Hit.getInstanceAndStore(issue, logged, .2);
      //if there is the task, give it a little hit
      if (issue.getTask() != null)
        Hit.getInstanceAndStore(issue.getTask(), logged, .1);


      if (!newStatus.equals(oldStatus) && newStatus.isBehavesAsClosed()) {
        createEventIssueClosed(oldStatus, issue);
        createEventIssueAddedClosed(issue, isNew, teamworkOperator, teamworkOperator.getDisplayName());
      }

    }

    //manage feedback on issues
    if (!newStatus.equals(oldStatus) && newStatus.isAskForComment() && issue.getLastIssueHistory()!=null) {
      restState.addClientEntry("ASK_FOR_COMMENT", issue.getLastIssueHistory().getId());
    }

    if (!newStatus.equals(oldStatus) && newStatus.isAskForWorklog()) {
      restState.addClientEntry("ASK_FOR_WORKLOG", issue.getAssignmentOnTask(issue.getAssignedTo()));
    }

    return restState.validEntries();
  }


  private void createMessageForIssueAssigned(TeamworkOperator teamworkOperator, boolean isFirstAssignment) throws StoreException {
    Resource resource = (Resource) ReflectionUtilities.getUnderlyingObject(issue.getAssignedTo());

    if (resource instanceof Person) {
      Person person = (Person) resource;

      TeamworkOperator assigOp = person.getMyself();
      if (assigOp != null && assigOp.isEnabled()) {

        // avoid to bother user with its events
        if (!assigOp.equals(teamworkOperator) || Fields.TRUE.equals(assigOp.getOption(OperatorConstants.NOTIFY_MY_EVENTS_TO_MYSELF))) {

          //si crea una sottoscrizione non persistente
          String mediasS = StringUtilities.unSplit(person.getMyself().getPreferredMediaOrDefault(MessagingSystem.Media.STICKY), ",");

          if (mediasS != null) {
            Listener listener = new Listener(teamworkOperator);
            listener.setIdAsNew();
            listener.setIdentifiable(issue);
            listener.setMedia(mediasS);
            listener.setEventType(Issue.Event.ISSUE_CLOSE + "");
            listener.setOneShot(true);
            listener.setOwner(assigOp);


            //si crea un evento non persistente
            SomethingHappened issueAssignedEvent;
            ButtonLink edit = IssueBricks.getPopoupLinkToEditor(issue.getId());
            edit.label = "I#" + issue.getMnemonicCode() + "#";//JSP.limWr(issue.getDisplayName(), 30);
            issueAssignedEvent = new SomethingHappened();
            issueAssignedEvent.setIdAsNew();
            issueAssignedEvent.setEventType(Issue.Event.ISSUE_ASSIGNED + "");
            issueAssignedEvent.getMessageParams().put("SUBJECT_REPLACEMENT", I18n.get("EVENT_" + Issue.Event.ISSUE_ASSIGNED) + ": " + JSP.limWr(issue.getDisplayName(), 20));

            issueAssignedEvent.setMessageTemplate(Issue.Event.ISSUE_ASSIGNED + "_MESSAGE_TEMPLATE");
            issueAssignedEvent.getMessageParams().put("issue", JSP.limWr(issue.getDisplayName(), 1000));
            issueAssignedEvent.getMessageParams().put("assignee", issue.getAssignedTo() != null ? issue.getAssignedTo().getDisplayName() : "unassigned");
            issueAssignedEvent.getMessageParams().put("gravity", I18n.get(issue.getGravity()));
            issueAssignedEvent.getMessageParams().put("modifier", logged.getPerson().getDisplayName());
            issueAssignedEvent.getMessageParams().put("status", issue.getStatus().getDescription());
            issueAssignedEvent.setWhoCausedTheEvent(logged);
            issueAssignedEvent.setLink(edit.toPlainLink());
            issueAssignedEvent.setIdentifiable(issue.getTask());

            //si fanno incontrare
            EventListenerMatcher.generateAndPersistMessage(listener, issueAssignedEvent);

          }

        }
      }
    }
   if(isFirstAssignment && JSP.ex(issue.getExtRequesterEmail())){
    issue.generateMessageForExternalRequester(false, false, true);
   }
  }


  private void notifyAssigneForNewIssue() {

  }

  private void createEventIssueClosed(IssueStatus oldStatus, Issue issue) throws StoreException {
    SomethingHappened change;
    String subject = issue.getTask() != null ? JSP.limWr(issue.getTask().getDisplayName(), 30) : "";
    ButtonLink edit = IssueBricks.getPopoupLinkToEditor(issue.getId());
    edit.label = "I#" + issue.getMnemonicCode() + "#";//JSP.limWr(issue.getDisplayName(), 30);
    change = new SomethingHappened();
    change.setIdAsNew();
    change.setEventType(Issue.Event.ISSUE_CLOSE + "");
    change.getMessageParams().put("SUBJECT", subject);
    change.setMessageTemplate(Issue.Event.ISSUE_CLOSE + "_MESSAGE_TEMPLATE");
    change.getMessageParams().put("issue", JSP.limWr(issue.getDescription(), 1000));
    change.getMessageParams().put("fromStatus", oldStatus.getDescription());
    change.getMessageParams().put("gravity", I18n.get(issue.getGravity()));
    change.getMessageParams().put("closer", logged.getPerson().getDisplayName());
    change.setWhoCausedTheEvent(logged);
    change.setLink(edit.toPlainLink());
    change.setIdentifiable(issue);
    change.store();
  }

  private void createEventIssueUpdated(Issue issue) throws StoreException {
    if (issue.getTask() != null) {
      SomethingHappened change;
      ButtonLink edit = IssueBricks.getPopoupLinkToEditor(issue.getId());
      edit.label = "I#" + issue.getMnemonicCode() + "#";//JSP.limWr(issue.getDisplayName(), 30);
      change = new SomethingHappened();
      change.setIdAsNew();
      change.setEventType(Task.Event.TASK_UPDATED_ISSUE + "");
      change.getMessageParams().put("SUBJECT", JSP.limWr(issue.getTask().getDisplayName(), 30));

      change.setMessageTemplate(Task.Event.TASK_UPDATED_ISSUE + "_MESSAGE_TEMPLATE");
      change.getMessageParams().put("issue", JSP.limWr(issue.getDisplayName(), 1000));
      change.getMessageParams().put("assignee", issue.getAssignedTo() != null ? issue.getAssignedTo().getDisplayName() : "unassigned");
      change.getMessageParams().put("gravity", I18n.get(issue.getGravity()));
      change.getMessageParams().put("modifier", logged.getPerson().getDisplayName());
      change.getMessageParams().put("closer", logged.getPerson().getDisplayName());
      change.getMessageParams().put("status", issue.getStatus().getDescription());

      change.setWhoCausedTheEvent(logged);
      change.setLink(edit.toPlainLink());
      change.setIdentifiable(issue.getTask());
      change.store();
    }
  }

  /**
   * @param issue
   * @param justAdded
   * @param logged         can be null
   * @param whoCausedEvent
   * @throws StoreException
   */
  public static void createEventIssueAddedClosed(Issue issue, boolean justAdded, TeamworkOperator logged, String whoCausedEvent) throws StoreException {
    if (issue.getTask() != null) {
      boolean hasBeenClosed = issue.getStatus().isBehavesAsClosed();

      ButtonLink edit = IssueBricks.getPopoupLinkToEditor(issue.getId());
      edit.label = "I#" + issue.getMnemonicCode() + "#";//JSP.limWr(issue.getDisplayName(), 30);

      String language = "";
      if (logged != null)
        language = logged.getLanguage();
      else
        language = ApplicationState.SYSTEM_LOCALE.getLanguage();

      SomethingHappened changeEvent = new SomethingHappened();
      changeEvent.setIdAsNew();
      changeEvent.setEventType((hasBeenClosed ? Task.Event.TASK_ISSUE_CLOSED : Task.Event.TASK_ISSUE_ADDED).toString());
      changeEvent.getMessageParams().put("SUBJECT", JSP.limWr(issue.getTask().getDisplayName(), 30) + " - I#" + issue.getId() + "#");
      if (justAdded) {
        changeEvent.setMessageTemplate("ISSUE_CREATED_MESSAGE_TEMPLATE");
        changeEvent.getMessageParams().put("creator", whoCausedEvent);
      } else {
        changeEvent.setMessageTemplate(Task.Event.TASK_UPDATED_ISSUE + "_MESSAGE_TEMPLATE");
        changeEvent.getMessageParams().put("closer", whoCausedEvent);
      }
      changeEvent.getMessageParams().put("issue", JSP.limWr(issue.getDisplayName(), 1000));
      changeEvent.getMessageParams().put("task", issue.getTask().getDisplayName());
      changeEvent.getMessageParams().put("status", issue.getStatus().getDescription());
      changeEvent.getMessageParams().put("gravity", I18n.getLabel(issue.getGravity(), language));

      if (logged != null) {
        changeEvent.setWhoCausedTheEvent(logged);
      }
      changeEvent.setLink(edit.toPlainLink());
      changeEvent.setIdentifiable(issue.getTask());
      changeEvent.store();


      //gestione dell'external requester
      if (JSP.ex(issue.getExtRequesterEmail())) {
        issue.generateMessageForExternalRequester(justAdded, hasBeenClosed, false);
      }

    }

  }


  public void cmdDelete() throws PersistenceException, SecurityException {
    issue = Issue.load(restState.getMainObjectId() + "");
    issue.testPermission(logged, TeamworkPermissions.issue_canDelete);
    DeleteHelper.cmdDelete(issue, restState);
  }

  public void cmdSaveAndAdd() throws PersistenceException, ApplicationException, ActionException, SecurityException {
    if (cmdSave()) {
      //use same entries but description, file, worklog
      restState.removeEntry("ISSUE_DESCRIPTION");
      restState.removeEntry("ISSUE_WORKLOG_TIME");

      //add a special entry for the id of the last saved issue
      restState.addClientEntry("_LAST_SAVED_ISSUE", restState.getMainObject().getId() + "");

      cmdAdd(false);
    }
  }

  public void cmdClone() throws PersistenceException, SecurityException {
    //use same entries but description, file, worklog
    restState.removeEntry("ISSUE_DESCRIPTION");
    restState.removeEntry("ISSUE_WORKLOG_TIME");
    cmdAdd(true);
  }


  public JSONObject cmdISS_PLANNER() throws ActionException, PersistenceException {
    JSONObject json = new JSONObject();
    json.element("ok", true);

    Date weekStart = new Date();
    Date weekEnd = new Date();
    boolean isForADay = restState.getEntry("FILTERFORADAY").checkFieldValue();


    long focusMillis = restState.getEntry("FOCUS_MILLIS").longValueNoErrorNoCatchedExc();
    focusMillis = focusMillis == 0 ? System.currentTimeMillis() : focusMillis;
    restState.addClientEntry("FOCUS_MILLIS", focusMillis);


    if (isForADay) {
      restState.addClientEntry("FLT_ISSUE_DATE_CLOSE_BY", new Date(focusMillis));
    } else {

      //determine the week boudaries
      Locale locale = restState.getLoggedOperator().getLocale();
      CompanyCalendar cc = new CompanyCalendar(new Date(focusMillis), locale);
      cc.set(CompanyCalendar.DAY_OF_WEEK, cc.getFirstDayOfWeek());
      weekStart = cc.setAndGetTimeToDayStart();
      cc.add(CompanyCalendar.DATE, 6);
      weekEnd = cc.setAndGetTimeToDayEnd();


      // prepare the dates for headers
      JSONObject dates = new JSONObject();
      cc.setTime(weekStart);
      for (int i = 0; i < 7; i++) {
        JSONObject dateInfo = new JSONObject();
        dateInfo.element("millis", cc.getTime().getTime());
        dateInfo.element("inUserFormat", DateUtilities.dateToString(cc.getTime()));
        String s = DateUtilities.dateToString(cc.getTime(), "EEE dd MMM");
        if (cc.isToday()) {
          s = s + " <small>(" + I18n.get("TODAY") + ")<small>";
          dateInfo.element("today", true);
        }
        dateInfo.element("label", s);
        dates.element(cc.get(CompanyCalendar.DAY_OF_WEEK) + "", dateInfo);
        cc.add(CompanyCalendar.DATE, 1);
      }
      json.element("weekStart", weekStart.getTime());
      json.element("weekEnd", weekEnd.getTime());
      json.element("dateHeaders", dates);

      //centralHeader
      String s = weekStart.getDate() + "";
      if (weekStart.getMonth() == weekEnd.getMonth())
        s += " - " + DateUtilities.dateToString(weekEnd, "dd MMMM");
      else {
        s = DateUtilities.dateToString(weekStart, "dd MMMM") + " - " + DateUtilities.dateToString(weekEnd, "dd MMMM");
      }
      s += " &nbsp;&nbsp;<small>(" + I18n.get("WEEK") + ": " + cc.get(CompanyCalendar.WEEK_OF_YEAR) + ")</small>";
      json.element("centralHeader", s);
      json.element("centralDay", weekStart.getTime() / 2 + weekEnd.getTime() / 2);

      // add the client entry to filter only the issue in the right period
      restState.addClientEntry("FLT_ISSUE_DATE_CLOSE_BY", DateUtilities.dateToString(weekStart) + ":" + DateUtilities.dateToString(weekEnd));
    }


    cmdFind();

    // si salvano le ce dopo che il primo find ha fatto il lavoro sporco recuperando l'eventuiale filtro di default o salvato
    ClientEntries clientEntries = new ClientEntries();
    clientEntries.addEntries(restState.getClientEntries());

    JSONArray jIssues = new JSONArray();
    Page issues = restState.getPage();

    Set<String> assignee = new HashSet<>();

    if (JSP.ex(issues)) {
      for (Iterator iterator = issues.getAllElements().iterator(); iterator.hasNext(); ) {
        Issue issue = Issue.load((String) ((Object[]) iterator.next())[0]);//((Object[])iterator.next())[0];
        if (issue == null) {
          continue;
        }

        jIssues.add(issue.jsonify());
        if (issue.getAssignedTo() != null)
          assignee.add(issue.getAssignedTo().getId() + "");
      }
      json.element("issues", jIssues);
    }

    //now filter for open and that should be closed only if the user is looking in the future or not scheduled (shouldClose == null)
    if (!isForADay) {
      if (weekEnd.getTime() > System.currentTimeMillis()) {
        restState.addClientEntry("FLT_ISSUE_DATE_CLOSE_BY", "<" + DateUtilities.dateToString(new Date(Math.min(System.currentTimeMillis(), weekStart.getTime()))) + " OR ()");
      } else {
        restState.addClientEntry("FLT_ISSUE_DATE_CLOSE_BY", "()");
      }

      //IssueBricks.addOpenStatusFilter(restState);  // robicch tolto 2/3/15

      cmdFind();

      //si recuperano le ce salvate prima.
      restState.addClientEntries(clientEntries);

      JSONArray openIssArray = new JSONArray();
      Page openIssues = restState.getPage();
      if (JSP.ex(openIssues)) {
        for (Iterator iterator = openIssues.getAllElements().iterator(); iterator.hasNext(); ) {
          Issue issue = Issue.load((String) ((Object[]) iterator.next())[0]);//((Object[])iterator.next())[0];
          if (issue == null) {
            continue;
          }
          openIssArray.add(issue.jsonify());
        }
        json.element("toBeRescheduled", openIssArray);
      }
    }

    //search for saved filter if there is merge it otherwise create it
    String savedPlannerFilter = logged.getOption("SAVED_PLANNER_SEARCH");

    // create the new string according to the new selected issues
    if (JSP.ex(savedPlannerFilter)) {
      String[] res = savedPlannerFilter.split(",");
      boolean found = false;

      for (String resId : assignee) { // loop for all the assignee in the list of selected issues
        for (String rid : res) {
          if (rid.equals(resId))
            found = true;
        }
        if (!found)
          savedPlannerFilter = JSP.ex(savedPlannerFilter) ? savedPlannerFilter + "," + resId : resId;
      }
    } else {
      for (String resId : assignee) {
        savedPlannerFilter = (JSP.ex(savedPlannerFilter) ? savedPlannerFilter + "," + resId : resId);
      }
    }


    if (JSP.ex(savedPlannerFilter)) {
      String[] res = savedPlannerFilter.split(",");
      JSONObject savedRes = new JSONObject();
      for (String rid : res) {
        Resource r = Resource.load(rid);
        if (r != null) {
          savedRes.element(rid, r.jsonify(false));
        }
      }
      json.element("savedFilter", savedRes);

      logged.putOption("SAVED_PLANNER_SEARCH", savedPlannerFilter);
      logged.store();
    }

    return json;
  }


  public void cmdPrepareDefaultFind() throws ActionException, PersistenceException {

    //search for default filter
    if (restState.getCommand() == null) {
      if (!PersistentSearch.feedFromDefaultSearch("ISSUEFILTER", restState))
        // when not set use my issues
        restState.addClientEntry(Fields.FLD_FILTER_NAME, "PF_MY_OPEN_ISSUES");
    }

    if (!PersistentSearch.feedFromSavedSearch(restState)) {
      String cmd = restState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty();
      if (JSP.ex(cmd)) {
        restState.getClientEntries().getClientEntries().clear();

        // add the filter name in order to display boldify
        restState.addClientEntry(Fields.FLD_FILTER_NAME, cmd);

        if ("PF_MY_OPEN_ISSUES".equals(cmd)) {
          IssueBricks.addOpenStatusFilter(restState);
          restState.addClientEntry("FLT_ISSUE_TASK_STATUS", TaskStatus.STATUS_ACTIVE);
          addMyselfToFilter();

        } else if ("PF_MY_OPEN_TODOS".equals(cmd)) { // used in mobile section only
          IssueBricks.addOpenStatusFilter(restState);
          restState.addClientEntry("FLT_ISSUE_TASK_STATUS", TaskStatus.STATUS_ACTIVE);
          addMyselfToFilter();

        } else if ("PF_MY_EXPIRED_ISSUES".equals(cmd)) {
          IssueBricks.addOpenStatusFilter(restState);
          addMyselfToFilter();
          restState.addClientEntry("FLT_ISSUE_DATE_CLOSE_BY", "<t");
          restState.addClientEntry(Form.FLD_FORM_ORDER_BY + "ISSUEFILTER", "issue.shouldCloseBy");

        } else if ("PF_EXPIRED_ISSUES".equals(cmd)) {
          IssueBricks.addOpenStatusFilter(restState);
          restState.addClientEntry("FLT_ISSUE_DATE_CLOSE_BY", "<t");
          restState.addClientEntry("FLT_ISSUE_TASK_STATUS", TaskStatus.STATUS_ACTIVE);
          restState.addClientEntry(Form.FLD_FORM_ORDER_BY + "ISSUEFILTER", "issue.shouldCloseBy");

        } else if ("PF_MY_INSERTED_ISSUES".equals(cmd)) {
          IssueBricks.addOpenStatusFilter(restState);
          restState.addClientEntry("FLT_ISSUE_ASSIGNED_BY", logged.getPerson().getId());

        } else if ("PF_ISSUES_OPENED_RECENTLY".equals(cmd)) {
          IssueBricks.addOpenStatusFilter(restState);
          CompanyCalendar calendar = new CompanyCalendar();
          calendar.setTime(new Date());
          calendar.add(CompanyCalendar.DAY_OF_MONTH, -7);
          String aWeekAgo = DateUtilities.dateToString(calendar.getTime());
          restState.addClientEntry("FLT_ISSUE_STATUS_LAST_CHANGE", ">" + aWeekAgo);

        } else if ("PF_ISSUES_CLOSED_RECENTLY".equals(cmd)) {
          IssueBricks.addCloseStatusFilter(restState);
          CompanyCalendar calendar = new CompanyCalendar();
          calendar.setTime(new Date());
          calendar.add(CompanyCalendar.DAY_OF_MONTH, -7);
          String aWeekAgo = DateUtilities.dateToString(calendar.getTime());
          restState.addClientEntry("FLT_ISSUE_STATUS_LAST_CHANGE", ">" + aWeekAgo);

        } else if ("PF_LONG_STANDING_ISSUES".equals(cmd)) {
          IssueBricks.addOpenStatusFilter(restState);
          CompanyCalendar calendar = new CompanyCalendar();
          calendar.setTime(new Date());
          calendar.add(CompanyCalendar.MONTH, -1);
          String lastMonth = DateUtilities.dateToString(calendar.getTime());
          restState.addClientEntry("FLT_ISSUE_LAST_MODIFIED", "<" + lastMonth);

        } else if ("PF_OPEN_SEVERE_ISSUES".equals(cmd)) {
          IssueBricks.addOpenStatusFilter(restState);
          restState.addClientEntry("FLT_ISSUE_TASK_STATUS", TaskStatus.STATUS_ACTIVE);
          restState.addClientEntry("FLT_ISSUE_GRAVITY", Issue.GRAVITY_BLOCK);

        } else if ("PF_MY_OPEN_SEVERE_ISSUES".equals(cmd)) {
          IssueBricks.addOpenStatusFilter(restState);
          restState.addClientEntry("FLT_ISSUE_TASK_STATUS", TaskStatus.STATUS_ACTIVE);

          restState.addClientEntry("FLT_ISSUE_GRAVITY", Issue.GRAVITY_BLOCK);
          addMyselfToFilter();

        } else if ("PF_LAST_MODIFIED".equals(cmd)) {
          restState.addClientEntry("FLT_ISSUE_LAST_MODIFIED", ">-2w");

        } else if ("PF_RECENTLY_USED".equals(cmd)) {
          restState.addClientEntry("RECENTLY_USED", Fields.TRUE);
        }

      }
    }

  }

  public void cmdFind() throws ActionException, PersistenceException {

    //defaults
    cmdPrepareDefaultFind();

    // cannot select issue directly as "distinct" fails with clob columns
    String hql = "select distinct issue.id, issue.status, issue.gravity, task.name, resource.name, issue.orderFactor, issue.orderFactorByResource, issue.shouldCloseBy, issue.creationDate , issue.lastStatusChangeDate, issue.shouldCloseBy";
    hql += ",issue.impact.id";
    hql += " from " + Issue.class.getName() + " as issue";

    QueryHelper qhelp = new QueryHelper(hql);

    // history flag
    boolean inHistory = restState.getEntry("FLT_ISSUE_INHISTORY").checkFieldValue();

    DesignerField.queryCustomFields("ISSUE_CUSTOM_FIELD_", 6, "issue", qhelp, restState);

    ActionUtilities.addOQLClause("ISSUE_ID", "issue.id", "issid", qhelp, QueryHelper.TYPE_CHAR, restState);

    //ActionUtilities.addOQLClause("FLD_ISSUE_ID", "issue.id", "issid", qhelp, QueryHelper.TYPE_CHAR, restState);

    ActionUtilities.addQBEClause("FLT_ISSUE_DESCRIPTION", "issue.description", "description", qhelp, QueryHelper.TYPE_CLOB, restState);

    ActionUtilities.addQBEClause("FLT_EXT_REQUESTER_EMAIL", "issue.extRequesterEmail", "extRequesterEmail", qhelp, QueryHelper.TYPE_CHAR, restState);

    ActionUtilities.addQBEClause("FLT_ISSUE_CODE", "issue.code", "code", qhelp, QueryHelper.TYPE_CHAR, restState);

    ActionUtilities.addQBEClause("FLT_ISSUE_DATE_CLOSE_BY", "issue.shouldCloseBy", "shouldCloseBy", qhelp, QueryHelper.TYPE_DATE, restState);

    ActionUtilities.addQBEClause("FLT_ISSUE_LAST_MODIFIED", "issue.lastModified", "lastModified", qhelp, QueryHelper.TYPE_DATE, restState);

    ActionUtilities.addQBEClause("FLT_ISSUE_DATE_SIGNALLED", "issue.dateSignalled", "dateSignalled", qhelp, QueryHelper.TYPE_DATE, restState);

    ActionUtilities.addOQLClause("FLT_ISSUE_ASSIGNED_BY", "issue.assignedBy.id", "assignedById", qhelp, QueryHelper.TYPE_CHAR, restState);

    ActionUtilities.addOQLClause("FLT_ISSUE_EXTERNAL_REQUESTER", "issue.extRequesterEmail", "extReqEmail", qhelp, QueryHelper.TYPE_CHAR, restState);

    ActionUtilities.addQBEClause("FLT_ISSUE_ESTIMATED", "issue.estimatedDuration", "esim", qhelp, QueryHelper.TYPE_LONG, restState);


    ActionUtilities.addQBEClause("FLT_ISSUE_STATUS_LAST_CHANGE", "issue.lastStatusChangeDate", "lastStatusChangeDate", qhelp, QueryHelper.TYPE_DATE, restState);


    //trick to filter only open
    if (JSP.ex(restState.getEntry("FLT_OPEN_ISSUES"))) {
      IssueBricks.addOpenStatusFilter(restState);
    }

    String isSts = restState.getEntry("FLT_ISSUE_STATUS").stringValueNullIfEmpty();
    if (JSP.ex(isSts)) {
      String[] idss = isSts.split(",");
      List<Integer> idsi = new ArrayList();
      for (String id : idss) idsi.add(Integer.valueOf(id)); //sono interi e vanno convertiti
      qhelp.addOQLInClause("issue.status.id", "statuses", idsi);
    }


    if (inHistory) {
      qhelp.addJoinAlias("join history.task as tsk");
    } else {
      qhelp.addJoinAlias("join issue.task as tsk");
    }



    String taskId = restState.getEntry("FLT_ISSUE_TASK").stringValueNullIfEmpty();
    if (JSP.ex(taskId)) {
      if (restState.getEntry("FLT_TASK_ISSUE_SHOW_CHILDREN").checkFieldValue()) {
        Task task = (Task) PersistenceHome.findByPrimaryKey(Task.class, taskId);
        qhelp.addOQLClause("tsk.ancestorIds like :ancs or tsk.id=:taskId", "ancs", task.getChildAncentorIds() + "%");
        qhelp.addParameter("taskId", task.getId());

      } else {
        qhelp.addOQLClause("tsk.id=:taskId", "taskId", taskId);
      }

    } else if (JSP.ex(restState.getEntry("FLT_ISSUE_TASK" + SmartCombo.TEXT_FIELD_POSTFIX))) {
      qhelp.addQBEORClauses(
        restState.getEntry("FLT_ISSUE_TASK" + SmartCombo.TEXT_FIELD_POSTFIX).stringValueNullIfEmpty(),
        qhelp.getOrElement("tsk.name", "taskName", QueryHelper.TYPE_CHAR),
        qhelp.getOrElement("tsk.code", "taskCode", QueryHelper.TYPE_CHAR));

    } else { // not to-do, thanks, issues only
      qhelp.addQueryClause(" tsk is not null");
    }


    String ts = restState.getEntry("FLT_ISSUE_TASK_STATUS").stringValueNullIfEmpty();
    if (JSP.ex(ts)) {
      if (ts.contains(","))
        qhelp.addOQLInClause("tsk.status", "tst", StringUtilities.splitToList(ts, ","));
      else
        qhelp.addOQLClause("tsk.status = :tst", "tst", ts);
    }

    String issue_gravity = restState.getEntry("FLT_ISSUE_GRAVITY").stringValueNullIfEmpty();
    if (JSP.ex(issue_gravity)) {
      qhelp.addOQLInClause("issue.gravity", "gravity", StringUtilities.splitToList(issue_gravity, ","));
    }


    String taskTypeId = restState.getEntry("ISSUE_TASK_TYPE").stringValueNullIfEmpty();
    if (JSP.ex(taskTypeId)) {
      qhelp.addOQLClause("tsk.type.id = :taskTypeId", "taskTypeId", Integer.parseInt(taskTypeId));
    } else if (JSP.ex(restState.getEntry("ISSUE_TASK_TYPE" + SmartCombo.TEXT_FIELD_POSTFIX))) {
      qhelp.addQBEClause("tsk.type.description", "taskTypeName", restState.getEntry("ISSUE_TASK_TYPE" + SmartCombo.TEXT_FIELD_POSTFIX).stringValueNullIfEmpty(), QueryHelper.TYPE_CHAR);
    }

    int issueTypeId = restState.getEntry("FLT_ISSUE_TYPE").intValueNoErrorCodeNoExc();
    if (issueTypeId > 0) {
      qhelp.addOQLClause("issue.type.id=:type", "type", issueTypeId);
    } else if (JSP.ex(restState.getEntry("FLT_ISSUE_TYPE" + SmartCombo.TEXT_FIELD_POSTFIX))) {
      qhelp.addQBEClause("issue.type.description", "issueTypeName", restState.getEntry("FLT_ISSUE_TYPE" + SmartCombo.TEXT_FIELD_POSTFIX).stringValueNullIfEmpty(), QueryHelper.TYPE_CHAR);
    }


    String tags = restState.getEntry("FLT_ISSUE_TAGS").stringValueNullIfEmpty();
    if (JSP.ex(tags)) {
      Set<String> tgs = StringUtilities.splitToSet(tags, ",");
      int i = 1;
      for (String tag : tgs) {
        tag=tag.trim().toUpperCase();
        qhelp.addOQLClause("upper(issue.tags) like :tg1_"+i +" or upper(issue.tags) like :tg2_"+i+" or upper(issue.tags) like :tg3_"+i +" or upper(issue.tags)=:tg4_"+i);
        qhelp.addParameter("tg1_" + i, tag + ", %"); //il tag all'inizio
        qhelp.addParameter("tg2_" + i, "%, " + tag + ", %"); //il tag è nel mezzo
        qhelp.addParameter("tg3_" + i, "%, " + tag); //il tag è alla fine
        qhelp.addParameter("tg4_" + i, tag); //il tag è solo soletto
        i++;
      }
    }

    try {
      int id = restState.getEntry("FLT_ISSUE_IMPACT").intValue();
      qhelp.addOQLClause("issue.impact.id=:impact", "impact", id);
    } catch (ActionException e) {
    } catch (ParseException e) {
    }

    if (restState.getEntry("FLT_ISSUE_HAVING_WRKLG").checkFieldValue()) {
      qhelp.addQueryClause("issue.worklogs.size>0");
    }

    if (inHistory) {
      ActionUtilities.addQBEClause("FLT_ISSUE_NOTES", "history.comment", "comment", qhelp, QueryHelper.TYPE_CLOB, restState);
    }


    // search by customer id
    String filter = restState.getEntry("CUST_ID").stringValueNullIfEmpty();
    if (filter != null) {
      if (!restState.getEntry("FLT_TASK_ISSUE_SHOW_CHILDREN").checkFieldValue()) {   // caso semplice solo per il singolo task

        qhelp.addOQLClause(" tsk.id in (select distinct assc.tsk.id from " + Assignment.class.getName() + " as assc where assc.resource.id=:custId and assc.role.name like :roleCust)", "custId", filter);
        qhelp.addParameter("roleCust", ApplicationState.getApplicationSetting("DEFAULT_CUSTOMER_ROLE_NAME", "Customer") + "%");

      } else {// caso complesso, con i parent. si deve prima estrarre i task e poi preparare la query
        QueryHelper tqh = new QueryHelper("select distinct assc.task.id,assc.task.ancestorIds from " + Assignment.class.getName() + " as assc where assc.resource.id=:custId and assc.role.name like :roleCust");
        tqh.addParameter("custId", filter);
        tqh.addParameter("roleCust", ApplicationState.getApplicationSetting("DEFAULT_CUSTOMER_ROLE_NAME", "Customer") + "%");
        List<Object[]> tsIdAncid = tqh.toHql().list();
        if (tsIdAncid.size() > 0) {
          int c = 1;
          String taskQuery = " task.id in (select p.id from " + Task.class.getName() + " as p where  ";
          for (Object[] obs : tsIdAncid) {
            taskQuery += ((c > 1 ? " or " : "") + "p.id=:tid" + c + " or p.ancestorIds like :ancid" + c);
            qhelp.addParameter("tid" + c, obs[0]);
            qhelp.addParameter("ancid" + c, "%" + (obs[1] == null ? obs[0] + PerformantNode.SEPARATOR : "" + obs[1] + obs[0] + PerformantNode.SEPARATOR));
            c++;
          }
          taskQuery += " )";
          qhelp.addOQLClause(taskQuery);

        }

      }
    }

    if (restState.getEntry("RECENTLY_USED").checkFieldValue()) {
      List<EntityGroupRank> ranks = RankUtilities.getEntityRankStatistically(logged.getIntId(), Issue.class.getName(), new Date());
      boolean something = JSP.ex(ranks);
      if (something) {
        List<String> ids = new ArrayList();
        for (int i = 0; i < ranks.size(); i++) {
          EntityGroupRank egr = ranks.get(i);
          ids.add(egr.id);
          if (i >= 19)
            break;
        }
        qhelp.addOQLInClause("issue.id", "issuexId", ids);
      }
    }


    boolean singleAssig = false;
    // check for unassigned issues
    if (restState.getEntry("FLT_ISSUE_UNASSIGNED").checkFieldValue()) {
      qhelp.addJoinAlias("left outer join issue.assignedTo as resource");
      qhelp.addQueryClause("resource is null");
    } else {

      if (inHistory) {
        qhelp.addJoinAlias("left outer join history.assignee as resource");
      } else {
        qhelp.addJoinAlias("left outer join issue.assignedTo as resource");
      }


      //assignee Person
      String assId = restState.getEntry("FLT_ISSUE_ASSIGNED_TO").stringValueNullIfEmpty();
      if (JSP.ex(assId)) {
        qhelp.addOQLClause("resource.id = :assignee", "assignee", assId);
        singleAssig = true;
      } else {
        String assigText = restState.getEntry("FLT_ISSUE_ASSIGNED_TO" + SmartCombo.TEXT_FIELD_POSTFIX).stringValueNullIfEmpty();
        if (JSP.ex(assigText)) {
          qhelp.addQBEORClauses(
            assigText,
            qhelp.getOrElement("resource.name", "name", QueryHelper.TYPE_CHAR),
            qhelp.getOrElement("resource.personSurname || ' ' || resource.personName", "surnameName", QueryHelper.TYPE_CHAR),
            qhelp.getOrElement("resource.personName || ' ' || resource.personSurname", "nameSurname", QueryHelper.TYPE_CHAR)
          );
        }
      }

      //assignee Company
      String companyId = restState.getEntry("FLT_ISSUE_ASSIGNED_COMPANY").stringValueNullIfEmpty();
      if (JSP.ex(companyId)) {
        //se non devo filtrare per i figli
        if (!restState.getEntry("FLT_COMPANY_ISSUE_SHOW_CHILDREN").checkFieldValue()) {
          qhelp.addOQLClause("resource.id = :company", "company", companyId);
        } else {
          Resource res = Resource.load(companyId);
          if (res != null) {
            qhelp.addOQLClause("resource.id = :company or resource.ancestorIds like :cpchil ", "cpchil", res.getChildAncentorIds() + "%");
            qhelp.addParameter("company", companyId);
          }
        }
        singleAssig = true;
      } else {
        String assigText = restState.getEntry("FLT_ISSUE_ASSIGNED_COMPANY" + SmartCombo.TEXT_FIELD_POSTFIX).stringValueNullIfEmpty();
        if (JSP.ex(assigText)) {
          qhelp.addQBEClause("resource.name", "companyName", assigText, QueryHelper.TYPE_CHAR);
          qhelp.addOQLClause("resource.class='COMPANY'");
        }
      }
    }

    // this is after because Query Helper invert order of join
    if (inHistory)
      qhelp.addJoinAlias("left outer join issue.issueHistories as history");


    filter = restState.getEntry("FLT_AREA").stringValueNullIfEmpty();
    if (filter != null) {
      Area a = (Area) PersistenceHome.findByPrimaryKey(Area.class, filter);
      qhelp.addOQLClause("tsk.area = :area", "area", a);
    }


    //----------------------------------- SET SECURITY --------------------------------------
    IssueBricks.addSecurityClauses(qhelp, restState);

    qhelp.addJoinAlias(" join issue.task as task");

    DataTable.orderAction(qhelp, "ISSUEFILTER", restState, "issue.gravity desc");

    // order by task or by assignee?
    boolean orderByResource = false;
    if (singleAssig) {
      // enable sortFlavour for assignee
      orderByResource = true;
    }

    if (!qhelp.getHqlString().contains("issue.gravity desc"))
      qhelp.addToHqlString(", issue.gravity desc");

    if (orderByResource)
      qhelp.addToHqlString(", issue.orderFactorByResource asc");
    else
      qhelp.addToHqlString(", issue.orderFactor asc");


    //if (JSP.ex(additionalSort))
    //  qhelp.addToHqlString("," + additionalSort);

    OqlQuery oqlQuery = qhelp.toHql();

    if (qhelp.isValidQBE()) {
      HibernatePage page = HibernatePage.getHibernatePageInstance(oqlQuery.getQuery(),
        Paginator.getWantedPageNumber(restState),
        Paginator.getWantedPageSize("ISSUEFILTER", restState));

      restState.setPage(page);
    }
  }


  private void addMyselfToFilter() {
    Person myPerson = logged.getPerson();
    restState.addClientEntry("FLT_ISSUE_ASSIGNED_TO", myPerson.getId());
  }


  public void cmdExport() throws ActionException, PersistenceException {
    cmdFind();
    if (JSP.ex(restState.getPage())) {
      List<Issue> iss = new ArrayList();
      List<Object[]> elements = restState.getPage().getAllElements();
      for (Object[] ob : elements) {
        Issue issue = Issue.load((String) ob[0]);
        if (issue != null)
          iss.add(issue);
      }
      if (iss.size() > 0) {
        ListPage lp = new ListPage(iss, 0, iss.size());
        restState.setPage(lp);
      }
    }
  }


  public void cmdUpgrade() throws PersistenceException, SecurityException {

    cmdEdit();

    Task parent = issue.getTask();
    Task newChild = new Task();

    newChild.setCode("[issue:" + issue.getId() + "]");
    newChild.setName("Generated from Issue: " + issue.getId());
/*
    if (issue.getNotes() != null && JSP.ex(issue.getNotes().getText())) {
      newChild.setNotes(JSP.limWr(issue.getNotes().getText() + "", 1850) + " I#" + issue.getMnemonicCode() + "#");
    }
*/

    newChild.setDescription(JSP.limWr(issue.getDescription() + "", 1850) + " I#" + issue.getMnemonicCode() + "#");
    Period p;
    if (issue.getShouldCloseBy() != null) {
      p = new Period(issue.getCreationDate(), issue.getShouldCloseBy());
    } else {
      p = new Period(issue.getCreationDate(), new Date());
    }
    p.store();
    newChild.setSchedule(p);
    newChild.setDuration(CompanyCalendar.getDistanceInWorkingDays(p.getStartDate(), p.getEndDate()));
    newChild.setArea(parent.getArea());
    newChild.setParentAndStore(parent);
    newChild.setStatus(parent.getStatus());
    if (JSP.ex(issue.getCode()))
      newChild.setCode(issue.getCode());

    String gtc = ApplicationState.getApplicationSetting("GENTASKCODES");
    if (Fields.TRUE.equalsIgnoreCase(gtc))
      newChild.bricks.suggestCodeFromParent();
    newChild.store();

    //disjoin worklogs
    for (Worklog workLog : issue.getWorklogs()) {
      workLog.setIssue(null);
      workLog.store();
    }

    for (PersistentFile pf : issue.getFiles()) {
      TaskAction.addDocToTask(newChild, pf.getFileLocation(), pf.getName());
    }

    if (JSP.ex(issue.getType())) {
      String hql = "select taskType from " + TaskType.class.getName() + " as taskType";
      QueryHelper qhelp = new QueryHelper(hql);
      qhelp.addQBEClause("description", "desc", issue.getType().getDescription(), QueryHelper.TYPE_CHAR);
      List<TaskType> tasktypes = qhelp.toHql().list();
      if (tasktypes.size() > 0) {
        newChild.setType(tasktypes.get(0));
      }
    }

    newChild.store();
    restState.setMainObject(newChild);

    //close the issue
    issue.setStatusClosed();
    issue.store();

    restState.addClientEntry("ISSUE_ID", issue.getId());

  }

  public void cmdClose() throws PersistenceException, ApplicationException, ActionException, SecurityException {
    restState.addClientEntry("ISSUE_STATUS", IssueStatus.getStatusClose());
    cmdSave();
    make((Issue) restState.getMainObject());
  }

  public void cmdSaveDur() throws SecurityException, ActionException, PersistenceException, ApplicationException {

    issue = Issue.load(restState.getMainObjectId() + "");
    if (issue != null) {
      long newDur = 0;
      try {
        newDur = restState.getEntry("ISSUE_WORKLOG_DELTA_ESTIMATED_TIME").durationInWorkingMillis(true);

        long est = newDur + issue.getWorklogDone();
        restState.addClientEntryTime("ISSUE_WORKLOG_ESTIMATED_TIME", est);

      } catch (ParseException e) {
      }
    }
    cmdSave();

  }

  public void cmdSortIssues() throws PersistenceException {

    // order the issues
    String idss = restState.getEntry("issues").stringValueNullIfEmpty();

    // order by task or by assignee?
    boolean orderByResource = "BY_RESOURCE".equals(restState.getEntry("SORT_FLAVOUR").stringValueNullIfEmpty());

    if (idss != null) {
      int i = 0;
      List<String> ids = StringUtilities.splitToList(idss, ",");
      for (String id : ids) {
        i++;
        Issue issue = Issue.load(id);
        if (issue != null && issue.hasPermissionFor(logged, TeamworkPermissions.issue_canWrite)) {
          if (orderByResource)
            issue.setOrderFactorByResource(i);
          else
            issue.setOrderFactor(i);
          issue.store();
        }
      }
    }

    //check if an issueId is passed for set gravity for moved issue
    String issuId = restState.getEntry("issueId").stringValueNullIfEmpty();
    String gravity = restState.getEntry("newGravity").stringValueNullIfEmpty();
    if (JSP.ex(issuId, gravity)) {
      Issue moved = Issue.load(issuId);
      if (moved != null && moved.hasPermissionFor(logged, TeamworkPermissions.issue_canWrite)) {
        moved.setGravity(gravity);
        moved.store();
      }

    }

  }

  public void cmdBulkMoveToTask() throws ActionException, PersistenceException, SecurityException {
    try {
      String taskId = restState.getEntryAndSetRequired("ISSUE_MOVE_TO_TASK").stringValue();
      Task task = (Task) PersistenceHome.findByPrimaryKey(Task.class, taskId);
      task.testPermission(logged, TeamworkPermissions.issue_canCreate);
      String comment = restState.getEntry("HIS_NOTES_TSK").stringValueNullIfEmpty();
      Set<String> ids = StringUtilities.splitToSet(restState.getEntry("issueIds").stringValueNullIfEmpty(), ",");
      for (String issueId : ids) {
        Issue issue = Issue.load(issueId);

        //se sto cambiando task devo aggiornare il vecchio
        if (issue.getTask()!=null && !issue.getTask().equals(task))
          issue.getTask().markAsDirty();


        IssueHistory history = new IssueHistory(issue);
        issue.testPermission(logged, TeamworkPermissions.issue_canWrite);
        issue.setTask(task);

        //eventually create assignment
        Assignment assig = new AssignmentAction(restState).getOrCreateAssignment(issue.getTask(), issue.getAssignedTo(), null); // il metodo fa i test di sicurezza da solo
        if (assig == null)
          issue.setAssignedTo(null);  // se non hai i diritti di assegnare resetto assegnatario

        issue.store();

        // save the history if needed
        history.testChangedAndStore();
        if (JSP.ex(comment) && issue.getLastIssueHistory() != null)
          issue.getLastIssueHistory().setComment(comment);

      }
      task.store();

    } catch (ActionException e) {
    }
    // go to search
    cmdFind();
  }

  public void cmdBulkCopyToTask() throws ActionException, PersistenceException, SecurityException {
    try {
      String taskId = restState.getEntryAndSetRequired("ISSUE_COPY_TO_TASK").stringValue();
      Task task = (Task) PersistenceHome.findByPrimaryKey(Task.class, taskId);
      task.testPermission(logged, TeamworkPermissions.issue_canCreate);
      Set<String> ids = StringUtilities.splitToSet(restState.getEntry("issueIds").stringValueNullIfEmpty(), ",");
      for (String issueId : ids) {
        issue = Issue.load(issueId);

        Issue cloned= issue.clone(); // si prende una copia
        cloned.setTask(task);

        //eventually create assignment
        Assignment assig = new AssignmentAction(restState).getOrCreateAssignment(cloned.getTask(), cloned.getAssignedTo(), null); // il metodo fa i test di sicurezza da solo
        if (assig == null)
          cloned.setAssignedTo(null);  // se non hai i diritti di assegnare resetto assegnatario

        cloned.store();

      }
      task.markAsDirty(); //devo aggiornare il contatore sul task
      task.store();

    } catch (ActionException e) {
    }
    // go to search
    cmdFind();
  }

  public void cmdBulkMoveToRes() throws ActionException, PersistenceException, SecurityException {
    try {
      String taskId = restState.getEntryAndSetRequired("ISSUE_MOVE_TO_RES").stringValue();
      Resource res = (Resource) PersistenceHome.findByPrimaryKey(Resource.class, taskId);
      //TeamworkPermissions.resource_canRead e TeamworkPermissions.issue_canWrite sono sufficienti
      // perchè se sposto su una risorsa già assegnata al task non devo creare una nuova assegnazione.
      // Il controllo lo fa getOrCreateAssignment()
      res.testPermission(logged, TeamworkPermissions.resource_canRead);

      Set<String> ids = StringUtilities.splitToSet(restState.getEntry("issueIds").stringValueNullIfEmpty(), ",");
      String comment = restState.getEntry("HIS_NOTES_RES").stringValueNullIfEmpty();

      for (String issueId : ids) {
        Issue issue = Issue.load(issueId);
        IssueHistory history = new IssueHistory(issue);

        if (issue.hasPermissionFor(logged, TeamworkPermissions.issue_canWrite)) {

          //get or create assignment
          Assignment assig = new AssignmentAction(restState).getOrCreateAssignment(issue.getTask(), res, null); // il metodo fa i test di sicurezza da solo

          // if the resource is not assigned and cannot create assig
          if (assig != null) {
            issue.setAssignedTo(res);

            issue.store();

            // save the history if needed
            history.testChangedAndStore();
            if (JSP.ex(comment) && issue.getLastIssueHistory() != null) {
              issue.getLastIssueHistory().setComment(comment);
            }

          } else {
            restState.addMessageWarning(I18n.get("ISSUE_COULDNT_BE_MOVED%%", JSP.limWr(issue.getDisplayName(), 50)));
          }
        } else {
          restState.addMessageWarning(I18n.get("ISSUE_COULDNT_BE_MOVED%%", JSP.limWr(issue.getDisplayName(), 50)));
        }
      }
    } catch (ActionException e) {
    }
    // go to search
    cmdFind();
  }

  public void cmdBulkSetStatus() throws ActionException, PersistenceException, SecurityException {
    IssueStatus status = IssueStatus.load(restState.getEntryAndSetRequired("ISSUE_STATUS_ALL").intValueNoErrorCodeNoExc());

    Set<String> ids = StringUtilities.splitToSet(restState.getEntry("issueIds").stringValueNullIfEmpty(), ",");
    String comment = restState.getEntry("HIS_NOTES_ST").stringValueNullIfEmpty();
    for (String issueId : ids) {
      Issue issue = Issue.load(issueId);
      IssueHistory history = new IssueHistory(issue);
      issue.testPermission(logged, TeamworkPermissions.issue_canWrite);
      //changeStatus(issue, status);
      issue.setIssueStatus(status);
      issue.store();
      // save the history if needed
      history.testChangedAndStore();
      if (JSP.ex(comment) && issue.getLastIssueHistory() != null)
        issue.getLastIssueHistory().setComment(comment);
    }

    // go to search
    cmdFind();
  }

  public void cmdBulkSetGravity() throws ActionException, PersistenceException, SecurityException {
    String gravity = restState.getEntryAndSetRequired("ISSUE_GRAVITY_ALL").stringValue();

    Set<String> ids = StringUtilities.splitToSet(restState.getEntry("issueIds").stringValueNullIfEmpty(), ",");
    for (String issueId : ids) {
      Issue issue = Issue.load(issueId);
      IssueHistory history = new IssueHistory(issue);
      issue.testPermission(logged, TeamworkPermissions.issue_canWrite);

      issue.setGravity(gravity);
      issue.store();
      // save the history if needed
      history.testChangedAndStore();
    }

    // go to search
    cmdFind();
  }

  public void cmdBulkSetImpact() throws ActionException, PersistenceException, SecurityException {
    IssueImpact impact = IssueImpact.load(restState.getEntryAndSetRequired("ISSUE_IMPACT_ALL").intValueNoErrorCodeNoExc());

    Set<String> ids = StringUtilities.splitToSet(restState.getEntry("issueIds").stringValueNullIfEmpty(), ",");
    for (String issueId : ids) {
      Issue issue = Issue.load(issueId);
      issue.testPermission(logged, TeamworkPermissions.issue_canWrite);
      issue.setImpact(impact);
      issue.store();
    }

    // go to search
    cmdFind();
  }

  public void cmdBulkAddComment() throws ActionException, PersistenceException, SecurityException {
    Set<String> ids = StringUtilities.splitToSet(restState.getEntry("issueIds").stringValueNullIfEmpty(), ",");
    String comment = restState.getEntry("HIS_NOTES_ST").stringValueNullIfEmpty();
    if (JSP.ex(comment)) {
      for (String issueId : ids) {
        Issue issue = Issue.load(issueId);
        if (issue != null && issue.hasPermissionFor(logged, TeamworkPermissions.issue_canWrite)) {
          IssueHistory history = new IssueHistory();
          history.setIssue(issue);
          history.setComment(comment);
          history.store();
        }
      }
    }

    // go to search
    cmdFind();
  }

  public void cmdBulkAddTags() throws ActionException, PersistenceException, SecurityException {
    Set<String> ids = StringUtilities.splitToSet(restState.getEntry("issueIds").stringValueNullIfEmpty(), ",");
    String tags = restState.getEntry("ISSUE_TAGS").stringValueNullIfEmpty();
    boolean replace = restState.getEntry("REPLACE_EXISTING").checkFieldValue();
    LinkedHashSet<String> tagsToAdd = StringUtilities.splitToOrderSet(JSP.w(tags), ","); //fa anche il trim dei singoli tag
    for (String issueId : ids) {
      Issue issue = Issue.load(issueId);
      if (issue != null && issue.hasPermissionFor(logged, TeamworkPermissions.issue_canWrite)) {
        LinkedHashSet<String> finalTags;
        if (replace) {
          finalTags = new LinkedHashSet();
        }else{
          finalTags = StringUtilities.splitToOrderSet(issue.getTags(), ",");
        }
        finalTags.addAll(tagsToAdd);
        issue.setTags(StringUtilities.setToString(finalTags,", ")); //attenzione lo spazio nel separatore ci DEVE essere

        issue.store();
      }
    }

    // go to search
    cmdFind();
  }

  public void cmdBulkSetNewDate() throws ActionException, PersistenceException, SecurityException {
    Set<String> ids = StringUtilities.splitToSet(restState.getEntry("issueIds").stringValueNullIfEmpty(), ",");
    Date newDate = restState.getEntry("ISSUE_NEWDATE_ALL").dateValueNoErrorNoCatchedExc();
    String errorMessage = "";
    for (String issueId : ids) {
      Issue issue = Issue.load(issueId);
      if (issue != null && issue.hasPermissionFor(logged, TeamworkPermissions.issue_canWrite)) {
        if (newDate != null && issue.getTask() != null && issue.getTask().getSchedule() != null && !issue.getTask().getSchedule().contains(newDate)) {
          errorMessage += "I#" + issue.getMnemonicCode() + "# &nbsp;&nbsp;";
        } else {
          issue.setShouldCloseBy(newDate);
          issue.store();
        }
      }
    }
    if (JSP.ex(errorMessage)) {
      restState.addMessageError(I18n.get("CLOSE_BY_OUT_OF_TASK_SCOPE") + ": " + errorMessage);
    }
    // go to search
    cmdFind();
  }

  public void cmdBulkDelIssues(boolean onlyIfEmpty) throws ActionException, PersistenceException {
    Set<String> ids = StringUtilities.splitToSet(restState.getEntry("issueIds").stringValueNullIfEmpty(), ",");
    for (String issueId : ids) {
      Issue issue = Issue.load(issueId);
      if (issue != null && issue.hasPermissionFor(logged, TeamworkPermissions.issue_canDelete)) {
        if (onlyIfEmpty) {
          if (!JSP.ex(issue.getDescription()))
            issue.remove();
        } else
          //issue.remove();
          DeleteHelper.cmdDelete(issue,restState);
      }
    }
    // go to search
    if (!onlyIfEmpty)
      cmdFind();
  }

  public void cmdBulkCloseIssues() throws ActionException, PersistenceException, SecurityException, ApplicationException {
    restState.addClientEntry("ISSUE_STATUS_ALL", IssueStatus.getStatusClose());
    cmdBulkSetStatus();
  }

  public void cmdBulkPrint() {
    Set<String> ids = StringUtilities.splitToSet(restState.getEntry("issueIds").stringValueNullIfEmpty(), ",");

    String hql = "select distinct issue.id, issue.status, issue.gravity, issue.orderFactor, issue.shouldCloseBy from " + Issue.class.getName() + " as issue where issue.id in (:ids)"; //, issue.task, issue.assignedTo
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setParameterList("ids", ids);
    restState.setPage(HibernatePage.getHibernatePageInstance(oql.getQuery(),
      Paginator.getWantedPageNumber(restState),
      Paginator.getWantedPageSize("ISSUEFILTER", restState)));

  }

  public void cmdBulkMergeIssues() throws SecurityException, PersistenceException, ActionException {
    Set<String> ids = StringUtilities.splitToSet(restState.getEntry("issueIds").stringValueNullIfEmpty(), ",");

    String hql = "select issue from " + Issue.class.getName() + " as issue where issue.id in (:ids)";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setParameterList("ids", ids);
    List<Issue> iss = oql.getQuery().list();

    // key is composed T+taskId+"_"+R:resourceId
    Map<String, Issue> masters = new HashTable();

    Set<Issue> toBeRemoved = new HashSet();

    for (Issue issue : iss) {
      issue.testPermission(logged, TeamworkPermissions.issue_canCreate);

      String key = "T" + (issue.getTask() == null ? "" : issue.getTask().getId()) + "_" + (issue.getAssignedTo() == null ? "" : issue.getAssignedTo().getId());

      if (masters.containsKey(key)) {
        Issue master = masters.get(key);
        master.setDescription(master.getDescription() + "\n" + issue.getDescription());
        master.setEstimatedDuration(master.getEstimatedDuration() + issue.getEstimatedDuration());
        issue.testPermission(logged,TeamworkPermissions.issue_canDelete);
        toBeRemoved.add(issue);
      } else {
        masters.put(key, issue);
      }
    }

    //remove damned issues
    for (Issue issue : toBeRemoved) {
      //issue.remove();
      DeleteHelper.cmdDelete(issue,restState);
    }

    // go to search
    cmdFind();

  }


}