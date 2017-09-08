package com.twproject.task.businessLogic;

import com.Ostermiller.util.CSVParser;
import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Person;
import com.twproject.resource.Resource;
import com.twproject.resource.ResourceBricks;
import com.twproject.resource.businessLogic.ResourceAction;
import com.twproject.security.TeamworkPermissions;
import com.twproject.task.*;
import org.jblooming.ApplicationException;
import org.jblooming.agenda.Period;
import org.jblooming.anagraphicalData.AnagraphicalData;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.ontology.SerializedMap;
import org.jblooming.oql.QueryHelper;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.security.Area;
import org.jblooming.security.Role;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.CollectionUtilities;
import org.jblooming.utilities.HashTable;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.ActionSupport;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.constants.FieldErrorConstants;
import org.jblooming.waf.constants.OperatorConstants;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.input.Uploader;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.text.ParseException;
import java.util.*;

/**
 * Created by IntelliJ IDEA.
 * User: schelazzi
 * Date: 23-set-2009
 * Time: 15.36.37
 * To change this template use File | Settings | File Templates.
 */
public class IssueImportControllerAction extends ActionSupport implements ActionController {

  public static final String STATUS_RESOLVED = "RESOLVED";


  public IssueImportControllerAction(PageState pageState) {
    super(pageState);
  }

  public PageState perform(HttpServletRequest request, HttpServletResponse response)
          throws PersistenceException, ActionException, org.jblooming.security.SecurityException, ApplicationException {
    PageState pageState = PageState.getCurrentPageState(request);
    Map<String, Integer> columnsPositions = new HashTable();
    if (pageState.command == null || "READ_FILE".equals(pageState.command)) {
      pageState.sessionState.setAttribute("ISSUE_IMPORT_LINES", null);
      pageState.sessionState.setAttribute("ISSUE_IMPORT_CP", null);
    }
    if ("READ_FILE".equals(pageState.command)) {
      readFile(pageState, columnsPositions, pageState.sessionState);

    } else if ("IMPORT_ISSUES".equals(pageState.command)) {

      try {
        importSelected(pageState);
      } catch (Throwable t) {
        pageState.addMessageError(pageState.getI18n("IMPORT_NOT_EXECUTED_CORRECTLY"));
        Tracer.platformLogger.error(t);
      }

    }

    return pageState;
  }

  private void importSelected(PageState pageState) throws PersistenceException, ParseException, ApplicationException {
    SessionState sessionState = pageState.sessionState;
    TeamworkOperator loggedOperator = (TeamworkOperator) pageState.getLoggedOperator();
    boolean validEntries = true;
    boolean canAddResource = loggedOperator.hasPermissionFor(TeamworkPermissions.resource_canCreate);
    boolean canAddTask = loggedOperator.hasPermissionFor(TeamworkPermissions.project_canCreate);
    Area areaRes = loggedOperator.getPerson().getArea();
    Set<String> ids = new HashSet();
    Map<String, Integer> columnsPositions = new HashTable();
    List<String[]> lines = null;
    lines = (List<String[]>) sessionState.getAttribute("ISSUE_IMPORT_LINES");
    columnsPositions = (Map<String, Integer>) sessionState.getAttribute("ISSUE_IMPORT_CP");
    String positiveFeedback = "";
    String negativeFeedback = "";
    int issueImported = 0;
    for (ClientEntry ce : pageState.getClientEntriesSet()) {
      if (ce.name.startsWith("CHECK_ISSUES_") && ce.checkFieldValue() == true) {
        String name = ce.name.substring("CHECK_ISSUES_".length(), ce.name.length());
        ids.add(name);
      }
    }
    for (String[] line : lines) {
      String id = line[columnsPositions.get(IssueImportControllerAction.COLUMNS.bug_id.name())];
      if (ids.contains(id)) {
        String descr = line[columnsPositions.get(IssueImportControllerAction.COLUMNS.short_desc.name())];
        String OS = line[columnsPositions.get(IssueImportControllerAction.COLUMNS.op_sys.name())];
        descr = descr + (JSP.ex(OS) ? " " + JSP.w(OS) : "");
        String prio = line[columnsPositions.get(IssueImportControllerAction.COLUMNS.priority.name())];
        String assign = line[columnsPositions.get(IssueImportControllerAction.COLUMNS.assigned_to.name())];
        String status = line[columnsPositions.get(IssueImportControllerAction.COLUMNS.bug_status.name())];
        String res = line[columnsPositions.get(IssueImportControllerAction.COLUMNS.resolution.name())];

        // create issue
        Issue issue = new Issue();
        issue.setIdAsNew();
        issue.setGravity(Issue.GRAVITY_HIGH);

        // task    &   assignee
        List<Resource> found = findResource(assign, pageState);
        Person resource = null;
        Task task = null;
        //found the task
        String taskIdByRow = pageState.getEntry("TASK_IMPORT_ISSUE" + id).stringValueNullIfEmpty();
        boolean createTask = pageState.getEntry("CHECK_CREATE_TASK_" + id).checkFieldValue();
        if (createTask && canAddTask) {
          String product = line[columnsPositions.get(IssueImportControllerAction.COLUMNS.product.name())];
          task = new Task();
          task.setIdAsNew();
          task.setArea(areaRes);
          task.setDescription("IMPORTED");
          task.setName(product);
          task.setStatus(TaskStatus.STATUS_ACTIVE);
          Date start = new Date();
          task.setSchedule(new Period(start, start));
          task.setDuration(1);
          task.setCode(product);
          task.setOwner(loggedOperator);
          task.store();
        } else if (JSP.ex(taskIdByRow))
          task = (Task) PersistenceHome.findByPrimaryKey(Task.class, taskIdByRow);
        else {
          String taskId = pageState.getEntry("TASK_IMPORT_ISSUE_ALL").stringValueNullIfEmpty();
          if (JSP.ex(taskId)) {
            task = (Task) PersistenceHome.findByPrimaryKey(Task.class, taskId);
          }
        }
        if (JSP.ex(task)) {
          if (found.size() == 1) {
            resource = (Person) found.get(0);
            issue.setAssignedTo(resource);
          } else {   // (found.size() == 0)
            if (canAddResource &&  task.hasPermissionFor(loggedOperator, TeamworkPermissions.assignment_canCRW)) {
              if (JSP.ex(assign)) {
                resource = new Person();
                resource.setIdAsNew();
                resource.setPersonName(assign);
                resource.setPersonSurname(assign);
                ResourceAction ra = new ResourceAction(pageState);
                Role operationalRole = ra.getOperationalRole(loggedOperator.getPerson());
                resource.setArea(areaRes);
                AnagraphicalData data = new AnagraphicalData();
                data.setIdAsNew();
                data.setOrderFactor(1);
                data.setLocationDescription("imported");
                data.setEmail(assign);
                resource.store();
                data.store();
                resource.getAnagraphicalDatas().add(data);
                TeamworkOperator operator = new TeamworkOperator();
                operator.setIdAsNew();
                operator.setLoginName(assign);
                operator.changePassword(assign);
                operator.setName(resource.getPersonName());
                operator.putOption(OperatorConstants.MEDIA_PREFERRED_CHANNEL, MessagingSystem.Media.LOG +"");
                operator.setSurname(resource.getPersonSurname());
                operator.store();
                operator.addRoleAndPersist(operationalRole);
                operator.store();
                resource.setMyself(operator);
                resource.store();
                issue.setAssignedTo(resource);
              }
            } else {
              validEntries = false;
              negativeFeedback = negativeFeedback + "<br>" + pageState.getI18n("NO_PERMISSION_TO_MANAGE_ASSIG_ON_TASK") + ": " + task.getDisplayName();
            }
          }
          if (!task.isAssigned(resource) && JSP.ex(assign)) {
            if (task.hasPermissionFor(loggedOperator, TeamworkPermissions.assignment_canCRW)) {
              // create the assign on task
              Assignment ass = new Assignment();
              ass.setResource(resource);
              ass.setOwner(pageState.getLoggedOperator());

              //si recupera dalla risorsa se si hanno i permessi, altrimenti si usa il default
              if (resource.hasPermissionFor(loggedOperator,TeamworkPermissions.resource_cost_canRead) && resource.getHourlyCost() > 0)
                ass.setHourlyCost( resource.getHourlyCost());
              else
                ass.setHourlyCost( ResourceBricks.getDefaultHourlyCost());

              ass.setHourlyCost(resource.getHourlyCost() > 0 ? resource.getHourlyCost() : ResourceBricks.getDefaultHourlyCost());
              ass.setTask(task);
              ass.setEnabled(true);
              ass.setRole(TaskBricks.getWorkerRole(task.getArea()));
              ass.store();
              long stamanPrest = Period.getDayPeriodInstance(new Date()).getStartDate().getTime();
              AssignmentAction.updateAssignmentPriority(ass, AssignmentPriority.PRIORITY_LOW, stamanPrest);

              //subscription and notification
              if (ass.getResource().getMyself() != null) {
                TaskAction taskAction = new TaskAction(pageState);

                // notify the assignee
                ass.generateAssignmentMessages(pageState.getLoggedOperator());


                // subscribe the assignee
                // add default make
                SerializedMap<String, String> subm = ass.getRole().getDefaultSubscriptions();
                if (subm != null) {
                  for (String k : subm.keySet()) {
                    pageState.addClientEntry(k, subm.get(k));
                  }
                }
              }
            } else {
              validEntries = false;
              negativeFeedback = negativeFeedback + "<br>" + pageState.getI18n("NO_PERMISSION_TO_MANAGE_ASSIG_ON_TASK") + ": " + task.getDisplayName();
            }
          }

        } else {
          validEntries = false;
          negativeFeedback = negativeFeedback + "<br>" + pageState.getI18n("NO_TASK_SEECTED_FOR_ISSUE") + ": " + descr;
        }
        if (validEntries) {
          String hql = "select impact from " + IssueImpact.class.getName() + " as impact";
          QueryHelper qhelp = new QueryHelper(hql);
          qhelp.addOQLClause("impact.description =:d", "d", prio);
          List<IssueImpact> impacts = qhelp.toHql().list();
          if (impacts.size() >= 1) {  // if gives mor than one restult I coose the first :-) ??
            issue.setImpact(impacts.get(0));
          } else {
            IssueImpact im = new IssueImpact();
            im.setIdAsNew();
            im.setArea(task.getArea());
            im.setDescription(prio);
            im.store();
            issue.setImpact(im);
          }
          issue.setDescription(descr);
          issue.setArea(task.getArea());
          //status
          if (STATUS_RESOLVED.equals(status)) {
            issue.setStatusClosed();
          } else {
            issue.setStatusOpen();
          }
          issue.setTask(task);
          issue.store();
          if (JSP.ex(res)) {
            issue.addComment(res).store();
          }

          issueImported++;
        } else {
          validEntries = true;
        }
      }
    }
    if (ids.size() == 0) {
      pageState.addMessageWarning(pageState.getI18n("NO_ISSUES_SELECTED"));
    }
    if (issueImported > 0) {
      positiveFeedback = issueImported + " " + pageState.getI18n("ISSUES_IMPORTED_CORRECTLY");
    }
    if (!"".equals(negativeFeedback)) {
      pageState.addMessageError(negativeFeedback);
    }
    if (!"".equals(positiveFeedback)) {
      pageState.addMessageInfo(positiveFeedback);
    }

  }

  private void readFile(PageState pageState, Map<String, Integer> columnsPositions, SessionState sessionState) {

    Exception exc = null;
    List<String[]> lines;
    try {
      Uploader.UploadHelper instance = Uploader.getHelper("ISSUE_FILE_TO_IMPORT", pageState);
      if (instance == null) {
        ClientEntry ce = new ClientEntry("ISSUE_FILE_TO_IMPORT", null);
        ce.errorCode = FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY;
        pageState.addClientEntry(ce);
      } else {

        File temporaryFile = instance.temporaryFile;
        if (temporaryFile == null || !temporaryFile.exists()) {
          ClientEntry ce = new ClientEntry("ISSUE_FILE_TO_IMPORT", null);
          ce.errorCode = FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY;
          pageState.addClientEntry(ce);
        } else {

          //do import
          lines = new ArrayList();
          //read first line
          FileInputStream fis = new FileInputStream(temporaryFile);
          InputStreamReader isr = new InputStreamReader(fis, Charset.forName("UTF-8"));
          CSVParser cvsr = new CSVParser(isr);
          cvsr.setEscapes("nrf", "\n\r\f");

          final Integer emailPos = 1;

          //validate titles
          if (emailPos == null) {
            exc = new Exception("Error importing " + instance.originalFileName + ": need e-mail column");
          } else {
            String values[][] = cvsr.getAllValues();
            for (int i = 0; i < values.length; i++) {
              String[] value = values[i];
              String id = value[0];
              if (IssueImportControllerAction.COLUMNS.bug_id.name().equals(id)) {
                int j = 0;
                for (String s : value) {
                  columnsPositions.put(s, j);
                  j++;
                }
              }
              lines.add(value);
            }
            Collections.sort(lines, new Comparator<String[]>() {
              public int compare(String[] a1, String[] a2) {
                if (a1 != null && a2 != null) {
                  String email1 = a1[emailPos];
                  String email2 = a2[emailPos];
                  if (email1 != null && email2 != null)
                    return email1.compareToIgnoreCase(email2);
                }
                return 0;
              }
            });
            if (columnsPositions.size() == 0) {
              columnsPositions.put(COLUMNS.bug_id.name(), 0);
              columnsPositions.put(COLUMNS.bug_severity.name(), 1);
              columnsPositions.put(COLUMNS.priority.name(), 2);
              columnsPositions.put(COLUMNS.op_sys.name(), 3);
              columnsPositions.put(COLUMNS.assigned_to.name(), 4);
              columnsPositions.put(COLUMNS.bug_status.name(), 5);
              columnsPositions.put(COLUMNS.resolution.name(), 6);
              columnsPositions.put(COLUMNS.short_desc.name(), 7);
            }
            sessionState.setAttribute("ISSUE_IMPORT_LINES", lines);
            sessionState.setAttribute("ISSUE_IMPORT_CP", columnsPositions);

          }

          isr.close();
          fis.close();

        }
      }
    } catch (Exception e) {
      pageState.addMessageError(pageState.getI18n("FILE_MALFORMED"));
    }
    // }

  }

  public static List<Resource> findResource(String email, PageState pageState) throws PersistenceException {
    List<Resource> resources = new ArrayList<Resource>();
    String hql = "select distinct resource from " + Person.class.getName() + " as resource";
    hql += " join resource.anagraphicalDatas as data";
    QueryHelper qhelp = new QueryHelper(hql);
    ResourceBricks.addSecurityClauses("resource", true, qhelp, CollectionUtilities.toList(TeamworkPermissions.resource_canRead), (TeamworkOperator) pageState.getLoggedOperator(), true, true);
    String baseFilter =
            "   (data.email like :email and data.email<>'')" +
                    " and resource.hidden = :falsity ";

    qhelp.addParameter("falsity", false);
    qhelp.addOQLClause(baseFilter, "email", JSP.w(email));
    qhelp.addToHqlString(" order by resource.name");
    resources = qhelp.toHql().list();
    return resources;
  }


  public static enum COLUMNS {
    bug_id, bug_severity, priority, op_sys, assigned_to, bug_status, resolution, short_desc, product
  }
}
