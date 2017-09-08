package com.twproject.worklog.businessLogic;

import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Resource;
import com.twproject.security.TeamworkArea;
import com.twproject.security.TeamworkPermissions;
import com.twproject.task.*;
import com.twproject.worklog.Worklog;
import com.twproject.worklog.WorklogStatus;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.designer.DesignerField;
import org.jblooming.messaging.SomethingHappened;
import org.jblooming.ontology.PerformantNode;
import org.jblooming.ontology.businessLogic.DeleteHelper;
import org.jblooming.oql.OqlQuery;
import org.jblooming.oql.QueryHelper;
import org.jblooming.page.HibernatePage;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.remoteFile.FileStorage;
import org.jblooming.security.Area;
import org.jblooming.security.SecurityException;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.ActionSupport;
import org.jblooming.waf.ActionUtilities;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.html.display.Paginator;
import org.jblooming.waf.html.input.SmartCombo;
import org.jblooming.waf.html.table.ListHeader;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.state.PersistentSearch;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.RestState;

import java.text.ParseException;
import java.util.*;


/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 5-ott-2005 : 14.39.35
 */
public class WorklogAction extends ActionSupport {

  public TeamworkOperator logged;
  public String url;
  private FileStorage docFSLog = null;
  private Date startDate = null;
  private Date endDate = null;
  private String host;
  private String content;

  public WorklogAction(RestState restState) {
    super(restState);
    this.logged = (TeamworkOperator) restState.getLoggedOperator();
  }


  public void cmdDelete() throws PersistenceException, SecurityException {
    //Worklog wkl = (Worklog) PersistenceHome.findByPrimaryKey(Worklog.class, restState.getEntry("WORKLOG_ID").stringValueNullIfEmpty());

    String wlId = restState.getEntry("wlId").stringValueNullIfEmpty();
    if (JSP.ex(wlId)) {
      Worklog wl = Worklog.load(wlId);
      wl.bricks.testWritePermission(logged);

      Issue issue = wl.getIssue();
      if (issue != null) {
        issue.removeWorklogInMemory(wl);
      }
      DeleteHelper.cmdDelete(wl, restState);
    }
  }


  public void cmdPrepareDefaultFind() throws PersistenceException {
    TeamworkOperator logged = (TeamworkOperator) restState.getLoggedOperator();

    boolean canReadWorklogGlobally = logged.hasPermissionFor(TeamworkPermissions.resource_manage) || logged.hasPermissionFor(TeamworkPermissions.worklog_manage);

    //search for default filter
    if (restState.getCommand() == null)
      if (!PersistentSearch.feedFromDefaultSearch("WORKLOGSFILTER", restState)) {
        // when not set use my open task
        restState.addClientEntry(Fields.FLD_FILTER_NAME, "PF_MY_MONTH_WL");
      }

    if (!canReadWorklogGlobally) {
      if (!JSP.ex(restState.getEntry("ASS_ID"))) {
        //check permission on task
        if (JSP.ex(restState.getEntry("TASK"))) {
          Task task = Task.load(restState.getEntry("TASK").stringValueNullIfEmpty() + "");
          if (task == null || !task.hasPermissionFor(logged, TeamworkPermissions.worklog_manage)) {
            restState.addClientEntry("RES_ID", logged.getPerson().getId());
          }
        } else {
          if (!JSP.ex(restState.getEntry("RES_ID"))) {
            restState.addClientEntry("RES_ID", logged.getPerson().getId());
          } else {
            //test permissions on resource
            Resource res = Resource.load(restState.getEntry("RES_ID").intValueNoErrorCodeNoExc() + "");
            if (res == null || (!res.hasPermissionFor(logged, TeamworkPermissions.resource_manage) && !res.hasPermissionFor(logged, TeamworkPermissions.worklog_manage))) {
              restState.addClientEntry("RES_ID", logged.getPerson().getId());
              restState.addClientEntry("RES_ID_txt", logged.getPerson().getDisplayName());
            }
          }
        }
      }
    }

    //search for default filter
    if (restState.getCommand() == null)
      PersistentSearch.feedFromDefaultSearch("WORKLOGSFILTER", restState);


    if (!PersistentSearch.feedFromSavedSearch(restState)) {
      // uso di un filtro presettato
      String cmd = restState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty();
      if (JSP.ex(cmd)) {
        restState.getClientEntries().getClientEntries().clear();

        // add the filter name in order to display boldify
        restState.addClientEntry(Fields.FLD_FILTER_NAME, cmd);

        if ("PF_MY_ODD_WL".equals(cmd)) {
          restState.addClientEntry("RES_ID", logged.getPerson().getId());
          restState.addClientEntry("WORKLOG_DURATION", ">" + DateUtilities.getMillisInDaysHoursMinutes(logged.getPerson().getWorkDailyCapacity()));

        } else if ("PF_MY_WEEK_WL".equals(cmd)) {
          restState.addClientEntry("WORKLOG_AT_DAY", "TW");
          restState.addClientEntry("RES_ID", logged.getPerson().getId());

        } else if ("PF_MY_MONTH_WL".equals(cmd)) {
          restState.addClientEntry("WORKLOG_AT_DAY", "LM");
          restState.addClientEntry("RES_ID", logged.getPerson().getId());

        } else if ("PF_MY_LASTWEEK_WL".equals(cmd)) {
          restState.addClientEntry("WORKLOG_AT_DAY", "LW");
          restState.addClientEntry("RES_ID", logged.getPerson().getId());
        }
      }
    }
  }


  public void cmdFind() throws PersistenceException {
    TeamworkOperator logged = (TeamworkOperator) restState.getLoggedOperator();
    cmdPrepareDefaultFind();

    boolean somethingSearched = false;
    String filter = null;
    String hql = "select worklog from " + Worklog.class.getName() + " as worklog";
    QueryHelper qhelp = new QueryHelper(hql);

    //areas
    String areaId = restState.getEntry("AREA").stringValueNullIfEmpty();
    if (JSP.ex(areaId)) {
      TeamworkArea ta = TeamworkArea.load(areaId);
      qhelp.addOrQueryClause("worklog.assig.task.area = :area");
      qhelp.addParameter("area", ta);
      somethingSearched = true;
    }

    // si aggiungo le aree su cui si è abilitati anche se c'è quella sopra in modo da essere sicuri che non si salti la sicurezza
    if (!logged.hasPermissionAsAdmin()) {
      Set<Area> areas = new HashSet();
      areas.addAll(logged.getAreasForPermission(TeamworkPermissions.resource_manage));
      areas.addAll(logged.getAreasForPermission(TeamworkPermissions.worklog_manage));
      if (JSP.ex(areas)) {
        qhelp.addOrQueryClause("worklog.assig.task.area in (:areas)");
        qhelp.addParameter("areas", areas);
      }
    }


    // search by task id
    filter = restState.getEntry("TASK").stringValueNullIfEmpty();
    if (filter != null) {
      somethingSearched = true;
      if (!restState.getEntry("TASK_WORKLOG_SHOW_CHILDREN").checkFieldValue()) {
        qhelp.addOQLClause(" worklog.assig.task.id = :taskId", "taskId", filter);
      } else {
        Task task = (Task) PersistenceHome.findByPrimaryKey(Task.class, filter);
        qhelp.addOQLClause(" worklog.assig.task.id in (select p.id from " + Task.class.getName() + " as p where p.id=:tid or p.ancestorIds like :ancid )", "tid", task.getId());
        qhelp.addParameter("ancid", task.getChildAncentorIds() + "%");
      }

      // search by name/code
    } else {
      filter = restState.getEntry("TASK" + SmartCombo.TEXT_FIELD_POSTFIX).stringValueNullIfEmpty();
      if (filter != null) {
        somethingSearched = true;

        if (!restState.getEntry("TASK_WORKLOG_SHOW_CHILDREN").checkFieldValue()) {   // caso semplice solo il task per nome o codice e non i discendenti

          qhelp.addQBEORClauses(filter, qhelp.getOrElement("worklog.assig.task.name", "taskName", QueryHelper.TYPE_CHAR), qhelp.getOrElement("worklog.assig.task.code", "taskCode", QueryHelper.TYPE_CHAR));

        } else {// caso complesso, si deve prima estrarre i task e poi preparare la query
          QueryHelper tqh = new QueryHelper("select t.id,t.ancestorIds from " + Task.class.getName() + " as t");
          tqh.addQBEORClauses(filter, qhelp.getOrElement("t.name", "taskName", QueryHelper.TYPE_CHAR), qhelp.getOrElement("t.code", "taskCode", QueryHelper.TYPE_CHAR));
          List<Object[]> tsIdAncid = tqh.toHql().list();
          if (tsIdAncid.size() > 0) {
            int c = 1;
            String taskQuery = " worklog.assig.task.id in (select p.id from " + Task.class.getName() + " as p where  ";
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
    }

    // search by customer id
    filter = restState.getEntry("CUST_ID").stringValueNullIfEmpty();
    if (filter != null) {
      somethingSearched = true;
      if (!restState.getEntry("TASK_WORKLOG_SHOW_CHILDREN").checkFieldValue()) {   // caso semplice solo per il singolo task

        qhelp.addOQLClause(" worklog.assig.task.id in (select distinct assc.task.id from " + Assignment.class.getName() + " as assc where assc.resource.id=:custId and assc.role.name like :roleCust)", "custId", filter);
        qhelp.addParameter("roleCust", ApplicationState.getApplicationSetting("DEFAULT_CUSTOMER_ROLE_NAME", "Customer")+"%");

      } else {// caso complesso, con i parent. si deve prima estrarre i task e poi preparare la query
        QueryHelper tqh = new QueryHelper("select distinct assc.task.id,assc.task.ancestorIds from " + Assignment.class.getName() + " as assc where assc.resource.id=:custId and assc.role.name like :roleCust");
        tqh.addParameter("custId", filter);
        tqh.addParameter("roleCust", ApplicationState.getApplicationSetting("DEFAULT_CUSTOMER_ROLE_NAME", "Customer")+"%");
        List<Object[]> tsIdAncid = tqh.toHql().list();
        if (tsIdAncid.size() > 0) {
          int c = 1;
          String taskQuery = " worklog.assig.task.id in (select p.id from " + Task.class.getName() + " as p where  ";
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

    filter = restState.getEntry("TASK_TYPE").stringValueNullIfEmpty();
    if (filter != null) {
      somethingSearched = true;
      qhelp.addQBEClause("worklog.assig.task.type.id", "typeDescription", filter, QueryHelper.TYPE_INT);
    }

    somethingSearched = somethingSearched | ActionUtilities.addQBEClause("TASK_TAGS", "worklog.assig.task.tags", "tags", qhelp, QueryHelper.TYPE_CHAR, restState);
    somethingSearched = somethingSearched | DesignerField.queryCustomFields("WORKLOG_CUSTOM_FIELD_", 4, "worklog", qhelp, restState);

    String resId = restState.getEntry("RES_ID").stringValueNullIfEmpty();
    if (JSP.ex(resId)) {
      qhelp.addOQLClause("worklog.assig.resource.id = :assignee", "assignee", resId);
      somethingSearched = true;
    } else {
      String assigText = restState.getEntry("RES_ID" + SmartCombo.TEXT_FIELD_POSTFIX).stringValueNullIfEmpty();
      if (JSP.ex(assigText)) {
        qhelp.addQBEORClauses(
          assigText,
          qhelp.getOrElement("worklog.assig.resource.name", "name", QueryHelper.TYPE_CHAR),
          qhelp.getOrElement("worklog.assig.resource.personSurname || ' ' || worklog.assig.resource.personName", "surnameName", QueryHelper.TYPE_CHAR),
          qhelp.getOrElement("worklog.assig.resource.personName || ' ' || worklog.assig.resource.personSurname", "nameSurname", QueryHelper.TYPE_CHAR)
        );
        somethingSearched = true;
      }
    }


    filter = restState.getEntry("WORKLOG_ACTION").stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addQBEClause("worklog.action", "action", filter, QueryHelper.TYPE_CHAR);
      somethingSearched = true;
    }

    filter = restState.getEntry("WORKLOG_AT_DAY").stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addQBEClause("worklog.inserted", "inserted", filter, QueryHelper.TYPE_DATE);
      somethingSearched = true;
    }

    filter = restState.getEntry("WORKLOG_CREATED").stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addQBEClause("worklog.creationDate", "creationDate", filter, QueryHelper.TYPE_DATE);
      somethingSearched = true;
    }

    filter = restState.getEntry("WORKLOG_DURATION").stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addQBEClause("worklog.duration", "duration", filter, QueryHelper.TYPE_MILLIS);
      somethingSearched = true;
    }

    if (restState.getEntry("ONLY_WORLOG_FROM_ISSUES").checkFieldValue()) {
      qhelp.addOQLClause(" worklog.issue is not null");
      somethingSearched = true;
    }

    if (JSP.ex(restState.getEntry("ASS_ID"))) {
      qhelp.addOQLClause("worklog.assig.id=:assId", "assId", restState.getEntry("ASS_ID").stringValueNullIfEmpty());
      somethingSearched = true;
    }

    if (JSP.ex(restState.getEntry("ISSUE_ID"))) {
      qhelp.addOQLClause("worklog.issue.id=:issueId", "issueId", restState.getEntry("ISSUE_ID").stringValueNullIfEmpty());
      somethingSearched = true;
    }


    int wlStat = restState.getEntry("WORKLOG_STATUS").intValueNoErrorCodeNoExc();
    if (wlStat == 0 && restState.getEntry("WORKLOG_STATUS").stringValueNullIfEmpty() != null) {
      qhelp.addOQLClause(" worklog.status is null");
    } else if (wlStat > 0) {
      qhelp.addOQLClause(" worklog.status.id =:wlstat", "wlstat", wlStat);
    }

    /* //todo sarebbe bello ma nel caso di "to be approved" il valore è 0 mentre sul record c'è un null
    String wlSts = restState.getEntry("WORKLOG_STATUS").stringValueNullIfEmpty();
    if (JSP.ex(wlSts)) {
      somethingSearched = true;
      String[] idss = wlSts.split(",");
      List<Integer> idsi = new ArrayList();
      for (String id : idss) idsi.add(Integer.valueOf(id)<0?null:Integer.valueOf(id)); //sono interi e vanno convertiti
      qhelp.addOQLInClause("worklog.status.id", "wlstat", idsi);
    }*/

    // compute sum
    String sumS = "select sum(worklog.duration),sum(worklog.duration*worklog.assig.hourlyCost)";
    qhelp.setHqlString(StringUtilities.replaceAllNoRegex(qhelp.getHqlString(), "select worklog", sumS));
    OqlQuery oqlQuerySum = qhelp.toHql();

    // restore query sanity before apply order
    qhelp.setHqlString(StringUtilities.replaceAllNoRegex(qhelp.getHqlString(), sumS, "select worklog"));
    ListHeader.orderAction(qhelp, "WKLGSLH", restState, "worklog.inserted");
    OqlQuery oqlQuery = qhelp.toHql();


    Object[] tots = (Object[]) oqlQuerySum.uniqueResultNullIfEmpty();
    if (tots != null && tots.length == 2 && tots[0] != null && tots[1] != null) {
      restState.addClientEntry("WORLOG_TOTAL", (Long) tots[0]);
      restState.addClientEntry("WORLOG_TOTAL_COST", ((Double) tots[1]).doubleValue() / CompanyCalendar.MILLIS_IN_HOUR);
    }

    if (somethingSearched)
      restState.setPage(HibernatePage.getHibernatePageInstance(oqlQuery.getQuery(), Paginator.getWantedPageNumber(restState), Paginator.getWantedPageSize("WKLGSLH", restState)));


  }

  public void cmdEdit() {
    //To change body of created methods use File | Settings | File Templates.
  }


  public void cmdMove() throws PersistenceException, org.jblooming.security.SecurityException {
    String asigId = restState.getEntry("NEW_ASSIG").stringValueNullIfEmpty();
    Assignment a = Assignment.load(asigId);
    Set<String> ids = StringUtilities.splitToSet(restState.getEntry("wlIds").stringValueNullIfEmpty(), ",");

    //check task security
    Task task = a.getTask();
    boolean taskCanMoveWorklog = task.hasPermissionFor(logged, TeamworkPermissions.worklog_manage);
    int worklogMoved = 0;
    if (taskCanMoveWorklog) {
      for (String wlId : ids) {
        Worklog worklog = Worklog.load(wlId);
        // si aggiorna il vecchio
        worklog.getAssig().markAsDirty();
        if (worklog.bricks.canWrite(logged)) {
          worklog.setAssig(a);
          worklog.store();
          worklogMoved++;
        }
      }
    }
    restState.addMessageOK(I18n.get("WORKLOGS_MOVED_FEEDBACK_%%", worklogMoved + ""));
  }

  public void changeStatus() throws PersistenceException, SecurityException {
    restState.initializeEntries("row");
    String stid = restState.getEntryAndSetRequired("WL_STATUS").stringValueNullIfEmpty();
    WorklogStatus wls = WorklogStatus.load(stid);
    Set<String> ids = StringUtilities.splitToSet(restState.getEntry("wlIds").stringValueNullIfEmpty(), ",");
    int worklogMoved = 0;
    for (String wlId : ids) {
      Worklog worklog = Worklog.load(wlId);
      if (worklog.hasPermissionFor(logged, TeamworkPermissions.worklog_manage)) {
        worklog.setStatus(wls);
        worklog.store();
        worklogMoved++;
      }
    }
    restState.addMessageOK(I18n.get("WORKLOGS_CHANGED_FEEDBACK_%%", worklogMoved + ""));
  }

  public void deleteWorklogs() throws PersistenceException, SecurityException {
    Set<String> ids = StringUtilities.splitToSet(restState.getEntry("wlIds").stringValueNullIfEmpty(), ",");
    int worklogDeleted = 0;
    for (String wlId : ids) {
      Worklog worklog = Worklog.load(wlId);
      if (worklog != null) {
        worklog.bricks.testWritePermission(logged); //devo tener presente anche la data e lo status
        //worklog.remove();
        DeleteHelper.cmdDelete(worklog,restState);
        worklogDeleted++;
      }
    }

    restState.addMessageOK(I18n.get("WORKLOGS_DELETED_FEEDBACK_%%", worklogDeleted + ""));
  }


  public void checkAndGenerateEventForInvalidWorklog(Worklog wl) throws PersistenceException {
    // check if wl is in the task scope
    if (wl.getAssig() != null && wl.getAssig().getTask() != null && wl.getInserted() != null) {
      Task task = wl.getAssig().getTask();
      boolean generateMisplacedError = false;
      if (task.getSchedule() != null && !task.getSchedule().contains(wl.getInserted())) {
        //out of dates
        generateMisplacedError = true;
      }
      if (!TaskStatus.STATUS_ACTIVE.equals(task.getStatusOn(wl.getInserted()))) {
        //task not active
        generateMisplacedError = true;
      }

      PageSeed ps = new PageSeed(ApplicationState.serverURL + "/applications/teamwork/task/taskAssignmentList.jsp");
      ps.setCommand(Commands.EDIT);
      ps.setMainObjectId(wl.getAssig().getId());
      ps.addClientEntry("TASK_ID", task.getId());
      ButtonLink edit = new ButtonLink(ps);
      edit.label = task.getDisplayName();


      if (generateMisplacedError) {
        SomethingHappened change = new SomethingHappened();
        change.setIdAsNew();
        change.setEventType(Task.Event.TASK_WORKLOG_MISPLACED + "");
        change.getMessageParams().put("SUBJECT", JSP.limWr(task.getDisplayName(), 30));
        change.setMessageTemplate(Task.Event.TASK_WORKLOG_MISPLACED + "_MESSAGE_TEMPLATE");
        change.setIdentifiable(task);
        change.getMessageParams().put("task", task.getDisplayName());
        change.getMessageParams().put("when", JSP.w(wl.getInserted()));
        change.getMessageParams().put("duration", DateUtilities.getMillisInHoursMinutes(wl.getDuration()));
        change.setWhoCausedTheEvent(logged);
        change.setLink(edit.toPlainLink());
        change.store();
      }

      if (wl.getDuration() > wl.getAssig().getResource().getWorkDailyCapacity()) {
        SomethingHappened change = new SomethingHappened();
        change.setIdAsNew();
        change.setEventType(Task.Event.TASK_WORKLOG_OVERTIME + "");
        change.getMessageParams().put("SUBJECT", JSP.limWr(task.getDisplayName(), 30));
        change.setMessageTemplate(Task.Event.TASK_WORKLOG_OVERTIME + "_MESSAGE_TEMPLATE");
        change.setIdentifiable(task);
        change.getMessageParams().put("task", task.getDisplayName());
        change.getMessageParams().put("when", JSP.w(wl.getInserted()));
        change.getMessageParams().put("duration", DateUtilities.getMillisInHoursMinutes(wl.getDuration()));
        change.setWhoCausedTheEvent(logged);
        change.setLink(edit.toPlainLink());
        change.store();
      }


    }
  }


  public void bulkSetStatus() throws PersistenceException, SecurityException {
    restState.initializeEntries("row");
    String stid = restState.getEntryAndSetRequired("WL_STATUS").stringValueNullIfEmpty();
    WorklogStatus wls = null;
    if (stid != null)
      wls = (WorklogStatus) PersistenceHome.findByPrimaryKey(WorklogStatus.class, stid);
    Set<String> ids = StringUtilities.splitToSet(JSP.w(restState.getEntry("wlIds").stringValueNullIfEmpty()), ",");
    for (String issueId : ids) {
      Worklog wl = (Worklog) PersistenceHome.findByPrimaryKey(Worklog.class, issueId);
      wl.testPermission(logged, TeamworkPermissions.worklog_manage);
      wl.setStatus(wls);
      wl.store();
    }
  }


  public void bulkMoveToTask() throws PersistenceException, SecurityException {
    String taskid = restState.getEntryAndSetRequired("NEW_TASK").stringValueNullIfEmpty();
    int moved = 0;
    if (taskid != null) {
      Task task = (Task) PersistenceHome.findByPrimaryKey(Task.class, taskid);
      //if (task.hasPermissionFor(logged, TeamworkPermissions.worklog_manage) || task.hasPermissionFor(logged, TeamworkPermissions.assignment_manage)) {
      if (task.hasPermissionFor(logged, TeamworkPermissions.worklog_manage)) {
        Set<String> ids = StringUtilities.splitToSet(JSP.w(restState.getEntry("wlIds").stringValueNullIfEmpty()), ",");
        for (String wlId : ids) {
          Worklog wl = (Worklog) PersistenceHome.findByPrimaryKey(Worklog.class, wlId);
          wl.testPermission(logged, TeamworkPermissions.worklog_manage);
          Resource res = wl.getAssig().getResource();
          Assignment ass = task.getFirstAssignmentsForResource(res);
          //si deve aggiornare il vecchio
          wl.getAssig().markAsDirty();

          if (ass != null) {
            wl.setAssig(ass);
            wl.store();
            moved++;
          }
        }

      }
    }
    restState.addClientEntry("WL_MOVED", moved);
  }

  public void bulkSetDate() throws PersistenceException, SecurityException {
    Date newDate = restState.getEntryAndSetRequired("NEW_DATE").dateValueNoErrorNoCatchedExc();
    if (newDate != null) {
      CompanyCalendar cc = new CompanyCalendar(newDate);
      Set<String> ids = StringUtilities.splitToSet(JSP.w(restState.getEntry("wlIds").stringValueNullIfEmpty()), ",");
      for (String issueId : ids) {
        Worklog wl = (Worklog) PersistenceHome.findByPrimaryKey(Worklog.class, issueId);
        wl.bricks.testWritePermission(logged); //devo tener presente anche la data e lo status
        cc.setMillisFromMidnight(CompanyCalendar.getMillisFromMidnight(wl.getInserted()));
        wl.setInserted(cc.getTime());
        wl.store();
      }
    }
  }


  // ----------------------------------------------------------------------- START WORKLOG MANAGEMENT -----------------------------------------------------------------------
  public Worklog cmdSave() throws ActionException, PersistenceException, SecurityException, ParseException {

    int assId = restState.getEntry("assId").intValueNoErrorCodeNoExc();
    int wlId = restState.getEntry("wlId").intValueNoErrorCodeNoExc();

    Worklog worklog= Worklog.load(wlId+"");
    if (worklog!=null)
      assId=worklog.getAssig().getIntId();

    ClientEntry actionCE = restState.getEntry("WORKLOG_ACTION");
    Date inserted = restState.getEntry("WORKLOG_INSERTIONDATE").dateValueNoErrorNoCatchedExc();
    inserted=inserted==null?new Date():inserted;
    long dur = restState.getEntryAndSetRequired("WORKLOG_DURATION").durationInWorkingMillis(true);
    String action = actionCE.stringValueNullIfEmpty();

    //cerco di capire se sono state inseriti 40 intendendo dire 40m quindi se ho inserito un wl piu di 3 volte l'orario di lavoro
    //todo noi si sostiene sempre di poter inserire 200h per singola riga
    //dur = dur > (CompanyCalendar.WORKING_HOUR_TOTAL * 3) ? (dur / CompanyCalendar.MILLIS_IN_HOUR) * CompanyCalendar.MILLIS_IN_MINUTE : dur;

    CompanyCalendar cc = new CompanyCalendar(inserted);
    cc.setMillisFromMidnight(CompanyCalendar.getMillisFromMidnight(new Date())); // in modo da creare un wl all'ora corrente ma nella data giusta

    if (assId>0) {
      Assignment ass = null;
      // delete, insert or update?
      if (wlId>0) {//delete or update?
        worklog.bricks.testWritePermission(logged);
        ass = worklog.getAssig();
        if (dur > 0) { //update
          worklog.setDuration(dur);

          //se è lo stesso giorno non lo tocco
          if (DateUtilities.dateToInt(inserted)!=DateUtilities.dateToInt(worklog.getInserted()))
            worklog.setInserted(cc.getTime());

          if (actionCE.name!=null) //in some cases action is not passed
            worklog.setAction(action);


          //Custom fields
          DesignerField.saveCustomFields("WORKLOG_CUSTOM_FIELD_", 4, worklog, restState);

          //si controllano permessi, date e status
          worklog.bricks.testWritePermission(logged);
          worklog.store();

          checkAndGenerateEventForInvalidWorklog(worklog);

        }
      } else if (dur > 0) { // insert
        worklog = new Worklog();
        worklog.setIdAsNew();
        ass = (Assignment) PersistenceHome.findByPrimaryKey(Assignment.class, assId+"");
        worklog.setAssig(ass);
        worklog.setDuration(dur);
        worklog.setInserted(cc.getTime());
        if (actionCE.name!=null) //in some cases action is not passed
          worklog.setAction(action);

        //Custom fields
        DesignerField.saveCustomFields("WORKLOG_CUSTOM_FIELD_", 4, worklog, restState);


        Issue issue = Issue.load(restState.getEntry("issueId").intValueNoErrorCodeNoExc() + "");
        if (issue != null) {
          worklog.setIssue(issue);
        }


        //si controllano permessi, date e status
        worklog.bricks.testWritePermission(logged);
        worklog.store();

        checkAndGenerateEventForInvalidWorklog(worklog);
      }
    }

    return worklog;
  }



/*
  public Worklog addWorklog() throws ActionException, PersistenceException, SecurityException {
    Worklog ret = null;

    long time = 0;
    try {
      time = restState.getEntry("WORKLOG_DURATION").durationInWorkingMillis(true);
    } catch (ParseException e) {
    }
    if (time > 0) {

      //cerco di capire se sono state inseriti 40 intendendo dire 40m quinde se ho inserito un wl piu di 1.5 volte l'orario di lavoro
      time = time > (CompanyCalendar.WORKING_HOUR_TOTAL * 1.2) ? (time / CompanyCalendar.MILLIS_IN_HOUR) * CompanyCalendar.MILLIS_IN_MINUTE : time;

      Assignment assig = Assignment.load(restState.getEntry("ASSIG_ID").intValueNoErrorCodeNoExc() + "");


      Worklog wl = new Worklog();
      wl.setIdAsNew(); // DO NOT remove! if you remove try first to save a wl from issue and worklog week

      wl.setAssig(assig);

      wl.setDuration(time);
      String action = restState.getEntry("WORKLOG_ACTION").stringValueNullIfEmpty();
      wl.setAction(action);

      Issue issue = Issue.load(restState.getEntry("ISSUE_ID").intValueNoErrorCodeNoExc() + "");
      if (issue != null) {
        wl.setIssue(issue);
      }

      Date d = restState.getEntry("WORKLOG_INSERTIONDATE").dateValueNoErrorNoCatchedExc();
      wl.setInserted(d != null ? d : new Date());
      if (issue != null) {
        issue.addWorklogInMemory(wl);
      }

      //Custom fields
      DesignerField.saveCustomFields("WORKLOG_CUSTOM_FIELD_", 4, wl, restState);


      wl.bricks.testWritePermission(logged);
      wl.store();
      ret = wl;


      checkAndGenerateEventForInvalidWorklog(wl);

    }
    restState.removeEntry("WORKLOG_DURATION");

    return ret;
  }
  */
}
