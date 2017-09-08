package com.twproject.task;

import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import javax.servlet.jsp.PageContext;

import org.apache.commons.lang.StringUtils;
import org.hibernate.HibernateException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.agenda.Period;
import org.jblooming.designer.DesignerField;
import org.jblooming.messaging.Listener;
import org.jblooming.ontology.Pair;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.ontology.VersionHome;
import org.jblooming.operator.Operator;
import org.jblooming.oql.OqlQuery;
import org.jblooming.oql.QueryHelper;
import org.jblooming.persistence.exceptions.FindException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.security.Area;
import org.jblooming.security.Permission;
import org.jblooming.security.Role;
import org.jblooming.utilities.CodeValueList;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.Bricks;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.html.button.ButtonSupport;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.display.PercentileDisplay;
import org.jblooming.waf.html.input.ColorValueChooser;
import org.jblooming.waf.html.input.Combo;
import org.jblooming.waf.html.input.SQLCombo;
import org.jblooming.waf.html.input.SmartCombo;
import org.jblooming.waf.html.layout.HtmlColors;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.RestState;

import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Person;
import com.twproject.resource.Resource;
import com.twproject.resource.ResourceBricks;
import com.twproject.security.RoleTeamwork;
import com.twproject.security.TeamworkPermissions;
import com.twproject.setup.WizardSupport;
import com.twproject.waf.html.StatusIcon;

import net.sf.json.JSONObject;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class TaskBricks extends Bricks {

  public Task mainObject;

  public TaskBricks(Task task) {
    this.mainObject = task;
  }

	// TODO
	public static SQLCombo getNoAddCombo(Task task, String fieldName, RestState pageState) throws PersistenceException {

		String hql = "select t.id, t.name, l.description, t.description from twk_task_customer_field t left join olpl_lookup l on t.dataType=l.id";

		SQLCombo resources = new SQLCombo(fieldName, hql, " where name like ?", "");

		resources.separator = "</td><td>";
		resources.fieldSize = 10;

		// PageSeed edit = new PageSeed(ApplicationState.serverURL +
		// "/applications/teamwork/resource/resourceEditor.jsp");
		// edit.command = "ED";
		// resources.addLinkToEntity(edit, I18n.get("HIS_RESOURCEEDITOR"));
		resources.searchAll = true;
		resources.convertToUpper = true;
		return resources;
	}

	// TODO
	@SuppressWarnings("unchecked")
	public static List<TaskCustomerFieldRelation> hasCustomField(Serializable taskId) {
		OqlQuery oqlForFiltering = new OqlQuery(
				"from " + TaskCustomerFieldRelation.class.getName() + " where task.id = '" + taskId.toString() + "'");

		List<TaskCustomerFieldRelation> rl;
		try {
			rl = oqlForFiltering.list();
			if (rl != null) {
				return rl;
			}
		} catch (FindException e) {
			e.printStackTrace();
		}

		return null;
	}
	
	public TaskAudit hasAudit(String userId, String taskId) throws PersistenceException {
		if (!StringUtils.isEmpty(userId)) {
			List<TaskAudit> result = TaskAudit.findList(taskId);
			if (result != null) {
				for (TaskAudit ta : result) {
					List<TaskAuditReview> lr = ta.getReviewers();
					if (lr != null) {
						for (TaskAuditReview r : lr) {
							if (r.getReviewer().getId().equals(userId)) {
								return ta;
							}
						}
					}
				}
			}
		}
		return null;
	}

	public List<TaskAudit> getAllAuditByReviewer(String reviewerId, String taskId) throws PersistenceException {
		List<TaskAudit> result = null;
		final OqlQuery oqlQuery = new OqlQuery(
				"from " + TaskAuditReview.class.getName()
						+ " as obj where obj.reviewer.id = :aparam and obj.mainAudit.task.id=:task and obj.mainAudit.isClosed=0 order by obj.");
		try {
			List<TaskAuditReview> rl = oqlQuery.getQuery().setParameter("aparam", reviewerId).setParameter("task", taskId).list();
			if (rl != null) {
				result = new ArrayList<>();
				for (TaskAuditReview r: rl) {
					result.add(r.getMainAudit());
				}
			}
		} catch (HibernateException e) {
			throw new PersistenceException(oqlQuery.doDebug(new Object[] { reviewerId }), e);
		}
		return result;
	}

	public List<TaskAudit> getAllAuditByTask(String reporterId, String taskId, int isClosed)
			throws PersistenceException {
		List<TaskAudit> result = null;
		final OqlQuery oqlQuery = new OqlQuery(
				"from " + TaskAudit.class.getName()
						+ " as obj where obj.task.id=:task and isClosed=:isClosed order by obj.");
		try {
			result = oqlQuery.getQuery().setParameter("task", taskId).setParameter("isClosed", isClosed).list();
		} catch (HibernateException e) {
			throw new PersistenceException(oqlQuery.doDebug(new Object[] { reporterId }), e);
		}
		return result;
	}

	public static Combo getDataTypeCombo(String fieldName, String dis) throws PersistenceException {
		String hql = "select tt.id, tt.description from " + DataType.class.getName() + " as tt ";
		QueryHelper queryHelperForFiltering = new QueryHelper(hql);

		String baseFilter = " (tt.description like :filter) ";

		queryHelperForFiltering.addOQLClause(baseFilter);

		queryHelperForFiltering.addToHqlString(" order by tt.intValue, tt.description");

		String whereForId = "where tt.id = :filter";

		// Combo impactSC = new Combo(fieldName, hql, null, whereForId);
		// impactSC.searchAll = true;
		// impactSC.queryHelperForFiltering = queryHelperForFiltering;
		// impactSC.separator = "";
		// impactSC.fieldSize = 20;
		OqlQuery oqlForFiltering = new OqlQuery(
				"from " + DataType.class.getName() + " where discriminator = '"+dis+"'");
		List<DataType> rl = oqlForFiltering.list();


		CodeValueList cvl = CodeValueList.getInstanceForIdentifiables(rl);
		Combo cb = new Combo(fieldName, "</td><td>", "", 255, cvl, null);
		cb.label = fieldName;

		return cb;
	}

	public static Combo getAuditTypeCombo(String fieldName, String vl) throws PersistenceException {
		String hql = "select tt.id, tt.description from " + DataType.class.getName() + " as tt ";
		QueryHelper queryHelperForFiltering = new QueryHelper(hql);

		String baseFilter = " (tt.description like :filter) ";

		queryHelperForFiltering.addOQLClause(baseFilter);

		queryHelperForFiltering.addToHqlString(" order by tt.intValue, tt.description");

		String whereForId = "where tt.id = :filter";

		// Combo impactSC = new Combo(fieldName, hql, null, whereForId);
		// impactSC.searchAll = true;
		// impactSC.queryHelperForFiltering = queryHelperForFiltering;
		// impactSC.separator = "";
		// impactSC.fieldSize = 20;
		OqlQuery oqlForFiltering = new OqlQuery(
				"from " + AuditType.class.getName());
		List<AuditType> rl = oqlForFiltering.list();

		CodeValueList cvl = CodeValueList.getInstanceForIdentifiables(rl);
		Combo cb = new Combo(fieldName, "</td><td>", "", 255, cvl, null);
		cb.label = fieldName;

		return cb;
	}

  /**
   * This method modifies the task query adding the security clause.
   *
   * @param qhelp
   * @param pageState
   * @throws PersistenceException
   */
  public static void addSecurityReadClauses(QueryHelper qhelp, RestState pageState) throws PersistenceException {
    Set<Permission> perms = new HashSet();
    perms.add(TeamworkPermissions.task_canRead);
    addSecurityClauses(qhelp, perms, pageState);
  }


  /**
   * This method modifies the task query adding the security clause.
   *
   * @param qhelp
   * @param permissionsRequiredOnArea
   * @param pageState
   * @throws PersistenceException
   */
  public static void addSecurityClauses(QueryHelper qhelp, Set<Permission> permissionsRequiredOnArea, RestState pageState) throws PersistenceException {

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    //take care that this alias is used also out of the method e.g. in search
    qhelp.addJoinAlias(" left outer join task.assignments as assignment");

    //added bicch 12/1/2015
    if (logged.hasPermissionAsAdmin())
      return;

    //open unique clause and set owner; qhelp puts the "and" before; all following clauses are in or
    qhelp.addOQLClause("( task.owner = :logged", "logged", logged);

    //areas
    Set<Area> areas = new HashSet();
    for (Permission p : permissionsRequiredOnArea) {
      Set<Area> ar = logged.getAreasForPermission(p);
      if (ar != null)
        areas.addAll(ar);
    }
    if (areas.size() > 0) {
      qhelp.addOrQueryClause("task.area in (:areas)");
      qhelp.addParameter("areas", areas);
    }

    //assignments
    Person myPerson = logged.getPerson();

    if (myPerson != null) {
      List<Resource> myAncs = myPerson.getAncestors();

      Set<Role> roles = new HashSet();
      for (Permission p : permissionsRequiredOnArea) {
        OqlQuery oqlQuery = new OqlQuery(
                " select distinct role from " + Assignment.class.getName() + " as ass join ass.role as role where role.permissionIds like :taskRead and " +
                        "ass.resource in (:myAncs)");

        oqlQuery.getQuery().setParameterList("myAncs", myAncs);

        oqlQuery.getQuery().setString("taskRead", "%" + p.toString() + "%");
        List<Role> r = oqlQuery.list();
        if (r != null)
          roles.addAll(r);
      }

      if (roles.size() > 0) {
        qhelp.addOrQueryClause("assignment.role in (:assigRoles) and assignment.resource in (:myAncs)");
        qhelp.addParameter("myAncs", myAncs);
        qhelp.addParameter("assigRoles", roles);

/*
        //added peter & bicch 12/1/2015
        // tentativo per far vedere anche i task su cui non si è assegnati ma si hanno permessi per eredità dal task parent
        //selezionare tutti i task su cui sono assegnato, tirare fuori gli id sui e dei figli e creare un "in", in or, sugli id
        String assigHql = "select assig.task from " + Assignment.class.getName() + " as assig where " +
          "assig.role in (:assigRoles) and " +
          "assig.resource in (:myAncs)";
        QueryHelper qh = new QueryHelper(assigHql);
        qh.addParameter("myAncs", myAncs);
        qh.addParameter("assigRoles", roles);

        List<Task> tasks = qh.toHql().list();

        Set<Serializable> taskIds = new HashSet<Serializable>();
        int i=0;
        for (Task t : tasks) {
          taskIds.addAll(t.getDescendantIds(Task.class));
          taskIds.add(t.getId());
          //qhelp.addOrQueryClause("task.ancestorIds like :taskAncIds"+i);
          //qhelp.addParameter("taskAncIds"+i, t.getChildAncentorIds()+"%");
          //i++;
        }

        if (taskIds.size()>0){
          qhelp.addOrQueryClause("task.id in (:taskIds)");
          qhelp.addParameter("taskIds", taskIds);
        }
*/
      }


      //se il mamager ha i permessi di issue read, si mette il suo staff
      if (RoleTeamwork.getManagerRole().hasPermissionFor(TeamworkPermissions.issue_canRead)) {
        TreeSet<Resource> myStaff = myPerson.getAllMyStaff();
        if (myStaff.size() > 0) {
          qhelp.addOrQueryClause("assignment.resource in (:myStaff)");
          qhelp.addParameter("myStaff", myStaff);

        }
      }


    }

    //in order to keep all security conditions in a unique and clause
    qhelp.addToHqlString(")");

  }


  public static SmartCombo getTaskCombo(String fieldName, boolean onlyActiveTasks, Permission permissionRequiredOnAreaOrAssigs, PageState pageState) throws PersistenceException {
    return getTaskCombo(fieldName, onlyActiveTasks, permissionRequiredOnAreaOrAssigs, "", pageState);
  }

  public static SmartCombo getTaskCombo(String fieldName, boolean onlyActiveTasks, Permission permissionRequiredOnAreaOrAssigs, String additionalHql, PageState pageState) throws PersistenceException {
    Set<Permission> perms = new HashSet();
    perms.add(permissionRequiredOnAreaOrAssigs);
    return getTaskCombo(fieldName, onlyActiveTasks, perms, additionalHql, pageState);
  }


  public static SmartCombo getTaskCombo(String fieldName, boolean onlyActiveTasks, Set<Permission> permissionRequiredOnAreaOrAssigs, String additionalHql, PageState pageState) throws PersistenceException {
    Set<String> allowedStatuses = null;
    if (onlyActiveTasks) {
      allowedStatuses = new HashSet();
      allowedStatuses.add(TaskStatus.STATUS_ACTIVE);
      allowedStatuses.add(TaskStatus.STATUS_UNDEFINED);
    }
    return getTaskCombo(fieldName, allowedStatuses, permissionRequiredOnAreaOrAssigs, additionalHql, pageState);
  }


  public static SmartCombo getTaskCombo(String fieldName, Set<String> allowedStatuses, Set<Permission> permissionRequiredOnAreaOrAssigs, String additionalHql, PageState pageState) throws PersistenceException {

    //String hql = "select distinct task.id, coalesce(task.code,'') || ' ' || task.name from " + Task.class.getName() + " as task ";
    String hql = "select distinct task.id, task.name, task.code from " + Task.class.getName() + " as task ";
    QueryHelper queryHelperForFiltering = new QueryHelper(hql);

    if (additionalHql != null && additionalHql.trim().length() > 0)
      queryHelperForFiltering.addOQLClause(additionalHql);

    addSecurityClauses(queryHelperForFiltering, permissionRequiredOnAreaOrAssigs, pageState);

    //nasconde i task non modificati da x mesi se la feature è attiva
    if (I18n.isActive("CUSTOM_FEATURE_HIDE_OLDER_TASK_FROM_COMBO")) {
      int months = new ClientEntry("dummy", I18n.get("CUSTOM_FEATURE_HIDE_OLDER_TASK_FROM_COMBO")).durationInWorkingDaysNoErrorNoCatchedExc(false);
      if (months > 0) {
        queryHelperForFiltering.addOQLClause("task.lastModified>=:oldChange", "oldChange", new Date(System.currentTimeMillis() - CompanyCalendar.MILLIS_IN_MONTH * months));
      }
    }

    //filter
    //String baseFilter = " (upper(coalesce(task.code,'') || ' ' || task.name) like :" + SmartCombo.FILTER_PARAM_NAME + ") ";
    String baseFilter = " (upper(task.code) like :" + SmartCombo.FILTER_PARAM_NAME + " or upper(task.name) like :" + SmartCombo.FILTER_PARAM_NAME + ") ";

    if (allowedStatuses != null && allowedStatuses.size() > 0) {
      baseFilter = baseFilter + " and (";
      int j = 1;
      for (String allowedStatus : allowedStatuses) {
        baseFilter = baseFilter + "task.status = :" + allowedStatus + "PAR ";
        if (j < allowedStatuses.size())
          baseFilter = baseFilter + " or ";
        queryHelperForFiltering.addParameter(allowedStatus + "PAR", allowedStatus);
        j++;
      }
      baseFilter = baseFilter + ")";
    }

    queryHelperForFiltering.addOQLClause(baseFilter);

    //queryHelperForFiltering.addToHqlString(" order by coalesce(task.code,'') || ' ' || task.name");
    if (I18n.isActive("CUSTOM_FEATURE_ORDER_TASK_BY_CODE"))
      queryHelperForFiltering.addToHqlString(" order by task.code, task.name");
    else
      queryHelperForFiltering.addToHqlString(" order by task.name,task.code");

    String whereForId = "where task.id = :" + SmartCombo.FILTER_PARAM_NAME;

    SmartCombo taskSC = new SmartCombo(fieldName, hql, null, whereForId);
    taskSC.searchAll = true;
    taskSC.queryHelperForFiltering = queryHelperForFiltering;
    taskSC.separator = "</td><td>";
    taskSC.convertToUpper = true;
    taskSC.fieldSize = 40;
    return taskSC;
  }

  @Deprecated
  public static OqlQuery getVisibleTasks(boolean onlyActiveTasks, Set<Permission> permissionRequiredOnAreaOrAssigs, String additionalHql, String orderBy, PageState pageState) throws PersistenceException {

    String hql = "select task, schedule from " + Task.class.getName() + " as task join task.schedule as schedule ";
    QueryHelper qh = new QueryHelper(hql);

    if (additionalHql != null && additionalHql.trim().length() > 0)
      qh.addOQLClause(additionalHql);

    addSecurityClauses(qh, permissionRequiredOnAreaOrAssigs, pageState);

    if (onlyActiveTasks) {
      String baseFilter = " (task.status = :stActive or task.status = :stUndef)";
      qh.addParameter("stActive", TaskStatus.STATUS_ACTIVE);
      qh.addParameter("stUndef", TaskStatus.STATUS_UNDEFINED);
      qh.addOQLClause(baseFilter);
    }

    if (orderBy != null && orderBy.trim().length() > 0)
      qh.addToHqlString(" " + orderBy);

    return qh.toHql();
  }

  public static SmartCombo getTaskTypeCombo(String fieldName, RestState pageState) throws PersistenceException {
    //String sqlSelect = "select tt.id, tt.stringValue || ' ' || tt.description from " + TaskType.class.getName() + " as tt ";
    String hql = "select tt.id, tt.description,tt.stringValue from " + TaskType.class.getName() + " as tt ";
    QueryHelper queryHelperForFiltering = new QueryHelper(hql);


    TeamworkOperator operator = (TeamworkOperator) pageState.getLoggedOperator();
    Set<Area> areas = operator.getAreasForPermissionPlusMine(TeamworkPermissions.task_canRead);
    queryHelperForFiltering.addOrQueryClause("tt.area in (:areas) or tt.area is null");
    queryHelperForFiltering.addParameter("areas", areas);

    //String baseFilter = " (tt.stringValue || ' ' || tt.description like :" + SmartCombo.FILTER_PARAM_NAME + ") ";
    String baseFilter = " (tt.description like :" + SmartCombo.FILTER_PARAM_NAME + ") ";

    queryHelperForFiltering.addOQLClause(baseFilter);

    queryHelperForFiltering.addToHqlString(" order by tt.stringValue, tt.description");

    String whereForId = "where tt.id = :" + SmartCombo.FILTER_PARAM_NAME;

    SmartCombo taskSC = new SmartCombo(fieldName, hql, null, whereForId);
    taskSC.searchAll = true;
    taskSC.queryHelperForFiltering = queryHelperForFiltering;
    taskSC.separator = "</td><td>";
    taskSC.fieldSize = 20;

    return taskSC;
  }


  public static SmartCombo getTaskTypeCombo(String fieldName, Task task, RestState restState) {
//    String sqlSelect = "select tt.id, tt.stringValue || ' ' || tt.description, tt.stringValue  from " + TaskType.class.getName() + " as tt ";
    String hql = "select tt.id, tt.description, tt.stringValue  from " + TaskType.class.getName() + " as tt ";
    QueryHelper queryHelperForFiltering = new QueryHelper(hql);

    //26March2008: RP decided to show only those of the area of the task       

    queryHelperForFiltering.addOQLClause("tt.area = :taskArea", "taskArea", task.getArea());

    //String baseFilter = " (tt.stringValue || ' ' || tt.description like :" + SmartCombo.FILTER_PARAM_NAME + ") ";
    String baseFilter = " (upper(tt.description) like :" + SmartCombo.FILTER_PARAM_NAME + ") ";

    queryHelperForFiltering.addOQLClause(baseFilter);

    queryHelperForFiltering.addToHqlString(" order by tt.stringValue, tt.description");

    String whereForId = "where tt.id = :" + SmartCombo.FILTER_PARAM_NAME;

    SmartCombo taskSC = new SmartCombo(fieldName, hql, null, whereForId);
    taskSC.searchAll = true;
    taskSC.queryHelperForFiltering = queryHelperForFiltering;
    taskSC.separator = "</td><td>";
    taskSC.fieldSize = 20;
    taskSC.convertToUpper=true;


    // uses a callback js function "taskTypeAddedCallback" to fill the value once saved
    if (restState.getLoggedOperator().hasPermissionFor(TeamworkPermissions.classificationTree_canManage)) {
      PageSeed taskTypeEd = new PageSeed(ApplicationState.contextPath + "/applications/teamwork/task/taskType.jsp");
      taskTypeEd.addClientEntry("once", "yes");
      ButtonSupport addTT = ButtonLink.getBlackInstance(I18n.get("ADD_TYPE"), taskTypeEd, "taskTypeAddedCallback");
      addTT.additionalCssClass = "small";
      taskSC.addEntityButton = addTT;
    }


    return taskSC;
  }

  public static SmartCombo getActiveOrPendingTaskCombo(String fieldName, Set<Permission> permissionRequiredOnAreaOrAssigs, String additionalHql, PageState pageState) throws PersistenceException {
    Set<String> allowedStatuses = new HashSet();
    allowedStatuses.add(TaskStatus.STATUS_ACTIVE);
    allowedStatuses.add(TaskStatus.STATUS_UNDEFINED);
    allowedStatuses.add(TaskStatus.STATUS_SUSPENDED);
    return getTaskCombo(fieldName, allowedStatuses, permissionRequiredOnAreaOrAssigs, additionalHql, pageState);
  }


  public static SmartCombo getAssignmentCombo(String fieldName,
                                              Set<String> allowedStatuses,
                                              Set<Permission> permissionRequiredOnAreaOrAssigs,
                                              String additionalHql, PageState pageState) throws PersistenceException {

    String hql = "select distinct assignment.id, coalesce(task.code,'') || ' ' || task.name || ' ' || assignment.resource.name || ' (' || assignment.role.code || ')'" +
            " from " + Task.class.getName() + " as task ";
    //+"join task.assignments as assig"; //rimosso S&R perchè mandava in bomba postgresql argezeit
    QueryHelper queryHelperForFiltering = new QueryHelper(hql);

    if (additionalHql != null && additionalHql.trim().length() > 0)
      queryHelperForFiltering.addOQLClause(additionalHql);

    addSecurityClauses(queryHelperForFiltering, permissionRequiredOnAreaOrAssigs, pageState);

    //filter
    String baseFilter = " (coalesce(task.code,'') || ' ' || task.name || " +
            "' ' || assignment.resource.name like :" + SmartCombo.FILTER_PARAM_NAME + ") ";


    //nasconde i task non modificati da x mesi se la feature è attiva
    if (I18n.isActive("CUSTOM_FEATURE_HIDE_OLDER_TASK_FROM_COMBO")) {
      int months = new ClientEntry("dummy", I18n.get("CUSTOM_FEATURE_HIDE_OLDER_TASK_FROM_COMBO")).durationInWorkingDaysNoErrorNoCatchedExc(false);
      if (months > 0) {
        queryHelperForFiltering.addOQLClause("task.lastModified>=:oldChange", "oldChange", new Date(System.currentTimeMillis() - CompanyCalendar.MILLIS_IN_MONTH * months));
      }
    }

    if (allowedStatuses != null && allowedStatuses.size() > 0) {
      baseFilter = baseFilter + " and (";
      int j = 1;
      for (String allowedStatus : allowedStatuses) {
        baseFilter = baseFilter + "task.status = :" + allowedStatus + "PAR ";
        if (j < allowedStatuses.size())
          baseFilter = baseFilter + " or ";
        queryHelperForFiltering.addParameter(allowedStatus + "PAR", allowedStatus);
        j++;
      }
      baseFilter = baseFilter + ")";
    }

    queryHelperForFiltering.addOQLClause(baseFilter);

    queryHelperForFiltering.addToHqlString(" order by coalesce(task.code,'') || ' ' || task.name || ' ' || assignment.resource.name || ' (' || assignment.role.code || ')'");

    //String whereForId = "where assignment.id = :" + SmartCombo.FILTER_PARAM_NAME;
    String whereForId = "join task.assignments as assignment where assignment.id = :" + SmartCombo.FILTER_PARAM_NAME;

    SmartCombo taskSC = new SmartCombo(fieldName, hql, null, whereForId);
    taskSC.searchAll = true;
    taskSC.queryHelperForFiltering = queryHelperForFiltering;
    taskSC.separator = "</td><td>";
    taskSC.fieldSize = 40;

    return taskSC;
  }


  public SmartCombo getAssignableResourceCombo(String fieldName, boolean havingEnabledLogin, RestState pageState) throws PersistenceException {
    return getAssignableResourceCombo(mainObject, fieldName, havingEnabledLogin, pageState);
  }

  /**
   * restituisce le risorse che puoi assegnare, ovvero quelli nel tuo staff, o quelli delle aree su cui hai il resource_manage
   *
   * @param task
   * @param fieldName
   * @param havingEnabledLogin
   * @param pageState
   * @return
   * @throws PersistenceException
   */
  public static SmartCombo getAssignableResourceCombo(Task task, String fieldName, boolean havingEnabledLogin, RestState pageState) throws PersistenceException {

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    Person loggedPerson = logged.getPerson();

    String hql = "select resource.id, resource.name, coalesce(resource.code,' ') from " + Resource.class.getName() + " as resource ";

    String whereForId = "where resource.id = :" + SmartCombo.FILTER_PARAM_NAME;

    SmartCombo resources = new SmartCombo(fieldName, hql, null, whereForId);

    QueryHelper queryHelperForFiltering = new QueryHelper(hql);

    //open unique clause and set owner; qhelp puts the "and" before; all following clauses are in orge
    //queryHelperForFiltering.addOQLClause("( resource.owner = :logged", "logged", logged);

    if (RoleTeamwork.getMyselfRole().hasPermissionFor(TeamworkPermissions.resource_manage)) {
      queryHelperForFiltering.addOrQueryClause("( resource.myself = :logged");
      queryHelperForFiltering.addParameter("logged", logged);
    } else {
      queryHelperForFiltering.addToHqlString("( 0=1 ");
    }

    //areas
    Set<Area> areas = new HashSet();

    Set<Area> areasForPerm = logged.getAreasForPermission(TeamworkPermissions.resource_manage);
    if (areasForPerm != null && areasForPerm.size() > 0)
      areas.addAll(areasForPerm);

    /*if (task!=null) {
      Set<Area> areasFromAss = task.getAreasForPermission(logged, TeamworkPermissions.resource_manage);
      if (areasFromAss.size() > 0)
        areas.addAll(areasFromAss);
    }*/

    if (areas.size() > 0) {
      queryHelperForFiltering.addOrQueryClause("resource.area in (:areas) or resource.area is null");
      queryHelperForFiltering.addParameter("areas", areas);
    }

    //get my staff se manager ha il rsource_can_manage
    if (RoleTeamwork.getManagerRole().hasPermissionFor(TeamworkPermissions.resource_manage)) {
      ResourceBricks.addMyStaffQueryClause("resource", queryHelperForFiltering, loggedPerson);
    }
    //in order to keep all security conditions in a unique and clause
    // or 0=1 server per avere un riosultato vuoto se non hai nessun permesso
    queryHelperForFiltering.addToHqlString(" ) ");

    //se non hai i permessi di CRW su assig il combo deve essere vuoto
    boolean assigCanCRW = (task != null && task.hasPermissionFor(logged, TeamworkPermissions.assignment_canCRW) || logged.hasPermissionFor(TeamworkPermissions.assignment_canCRW));
    if (!assigCanCRW)
      queryHelperForFiltering.addToHqlString(" and 0=1"); // query finta che restituisce sempre vuoto


    String baseFilter =
            "(upper(resource.personName || ' ' || resource.personSurname) like :" + SmartCombo.FILTER_PARAM_NAME +
                    " or upper(resource.personSurname || ' ' || resource.personName) like :" + SmartCombo.FILTER_PARAM_NAME +
                    " or upper(resource.name) like :" + SmartCombo.FILTER_PARAM_NAME +
                    " or upper(resource.code) like :" + SmartCombo.FILTER_PARAM_NAME +
                    ") and resource.hidden = false ";


    if (havingEnabledLogin) {
      baseFilter = baseFilter + " and ( resource.myself != null and resource.myself.enabled = true )";
    }

    queryHelperForFiltering.addOQLClause(baseFilter);

    queryHelperForFiltering.addToHqlString(" order by resource.name");

    resources.queryHelperForFiltering = queryHelperForFiltering;

    resources.separator = "</td><td>";
    resources.fieldSize = 40;


    //inject my direct staff in bold
    if (assigCanCRW) {
      //inject my direct staff in bold
      //si aggiunge li staff solo se managerRole contiene uno dei permessi
      boolean managerHasPerm = RoleTeamwork.getManagerRole().hasPermissionFor(TeamworkPermissions.resource_manage);
      boolean myselfHasPerm = RoleTeamwork.getMyselfRole().hasPermissionFor(TeamworkPermissions.resource_manage);

      TreeSet<Resource> allMyStaff = new TreeSet();
      if (myselfHasPerm)
        allMyStaff.add(loggedPerson);

      if (managerHasPerm)
        allMyStaff.addAll(loggedPerson.getAllMyStaff());

      if (allMyStaff.size() > 0) {
        List<Object[]> resOfAssigsOnTask = new ArrayList<>();
        for (Resource r : allMyStaff) {
          resOfAssigsOnTask.add(new Object[]{r.getId(), r.getName(), JSP.w(r.getCode())});
        }
        resources.additionalLines = resOfAssigsOnTask;
      }
    }

    PageSeed edit = new PageSeed(ApplicationState.serverURL + "/applications/teamwork/resource/resourceEditor.jsp");
    edit.command = Commands.EDIT;
    resources.addLinkToEntity(edit, I18n.get("HIS_RESOURCEEDITOR"));
    resources.searchAll = true;
    resources.convertToUpper = true;
    return resources;
  }

  public static SmartCombo getAllMyAssignmentsCombo(String fieldName, List<String> excludedIds, PageState pageState) {
    Person logged = ((TeamworkOperator) pageState.getLoggedOperator()).getPerson();
    return getAllAssignmentsOfCombo(fieldName, logged, excludedIds);
  }

  public static SmartCombo getAllAssignmentsOfCombo(String fieldName, Resource person, List<String> excludedIds) {
    return getAllAssignmentsOfCombo(fieldName, person, false, excludedIds);
  }

  public static SmartCombo getAllAssignmentsOfCombo(String fieldName, Resource person, boolean onlyOpenTask, List<String> excludedIds) {
    return getAllAssignmentsOfCombo(fieldName, person, null, onlyOpenTask, false, false, false, excludedIds);
  }

  public static SmartCombo getAllAssignmentsOfCombo(String fieldName, Resource person, Period period, boolean onlyOpenTask, boolean onlyPlannable, boolean onlyEnabledAssig, boolean onlyWithEstimation, List<String> excludedIds) {
    String hql = "select assignment.id, task.name,coalesce(task.code,''), assignment.role.code from " + Assignment.class.getName() + " as assignment join assignment.task as task";

    String whereForId = "where assignment.id = :" + SmartCombo.FILTER_PARAM_NAME;

    QueryHelper queryHelperForFiltering = new QueryHelper(hql);

    String baseFilter =
            "(" +
                    " coalesce(task.code,'') || ' ' || task.name || ' - ' || assignment.role.name like :" + SmartCombo.FILTER_PARAM_NAME +
                    " or task.code like :" + SmartCombo.FILTER_PARAM_NAME +
                    " or task.name like :" + SmartCombo.FILTER_PARAM_NAME +
                    " or assignment.role.name like :" + SmartCombo.FILTER_PARAM_NAME +

                    ") and assignment.resource=:myself";

    if (excludedIds != null && excludedIds.size() > 0) {
      baseFilter = baseFilter + " and assignment.id not in (:excludedIds)";
      queryHelperForFiltering.addParameter("excludedIds", excludedIds);
    }

    if (onlyOpenTask)
      baseFilter = baseFilter + " and task.status = '" + TaskStatus.STATUS_ACTIVE + "'";

    if (onlyPlannable)
      hql = hql + " and assignment.activity='" + Assignment.ACTIVITY_ALL_IN_ONE + "'";

    if (onlyEnabledAssig)
      baseFilter = baseFilter + " and assignment.enabled=:truth";


    if (onlyWithEstimation)
      baseFilter = baseFilter + " and (assignment.estimatedWorklog>0)";


    if (period != null) {
      baseFilter = baseFilter +
              " and  ( " + "" +
              "(assignment.task.schedule.start<=:periodEnd or assignment.task.schedule.start is null) and " +
              "(assignment.task.schedule.end>:periodStart or assignment.task.schedule.end is null) " +
              " )";
      queryHelperForFiltering.addParameter("periodStart", period.getStartDate());
      queryHelperForFiltering.addParameter("periodEnd", period.getEndDate());
    }


    //nasconde i task non modificati da x mesi se la feature è attiva
    if (I18n.isActive("CUSTOM_FEATURE_HIDE_OLDER_TASK_FROM_COMBO")) {
      int months = new ClientEntry("dummy", I18n.get("CUSTOM_FEATURE_HIDE_OLDER_TASK_FROM_COMBO")).durationInWorkingDaysNoErrorNoCatchedExc(false);
      if (months > 0) {
        queryHelperForFiltering.addOQLClause("task.lastModified>=:oldChange", "oldChange", new Date(System.currentTimeMillis() - CompanyCalendar.MILLIS_IN_MONTH * months));
      }
    }

    queryHelperForFiltering.addParameter("myself", person);
    queryHelperForFiltering.addOQLClause(baseFilter);

    queryHelperForFiltering.addToHqlString("order by task.code, task.name");

    SmartCombo resources = new SmartCombo(fieldName, hql, null, whereForId);
    resources.searchAll = true;
    resources.queryHelperForFiltering = queryHelperForFiltering;
    return resources;
  }

  public StatusIcon getStatusIcon(int size, PageState pageState) {
    String color = mainObject.getStatusColor();
    StatusIcon si = new StatusIcon(color, size, I18n.get(mainObject.getStatus()));
    return si;
  }

  public JspHelper getTaskCheckup(int size) {
    JspHelper helper = new JspHelper("/applications/teamwork/task/part/partTaskCheckup.jsp");
    helper.parameters.put("mode", "ROUND");
    helper.parameters.put("task", mainObject);
    helper.parameters.put("size", size);
    return helper;
  }

  public JspHelper getTaskCheckupBarVert(int width, int height) {
    JspHelper helper = new JspHelper("/applications/teamwork/task/part/partTaskCheckup.jsp");
    helper.parameters.put("mode", "VERT");
    helper.parameters.put("task", mainObject);
    helper.parameters.put("width", width);
    helper.parameters.put("height", height);
    return helper;
  }

  public JspHelper getTaskCheckupVuMeter(int width, int height) {
    JspHelper helper = new JspHelper("/applications/teamwork/task/part/partTaskCheckup.jsp");
    helper.parameters.put("mode", "VU");
    helper.parameters.put("task", mainObject);
    helper.parameters.put("width", width);
    helper.parameters.put("height", height);
    return helper;
  }


  public StatusIcon getStatusIcon(PageState pageState) {
    return getStatusIcon(12, pageState);
  }

  public PercentileDisplay getProgressBar() {
    return getProgressBar(true);
  }

  public PercentileDisplay getProgressBar(boolean drawCheckup) {
    PercentileDisplay pd = new PercentileDisplay(mainObject.getProgress());
    pd.urlToInclude = "/applications/teamwork/parts/partTaskPercentileDisplay.jsp";

    Pair<String, Double> checkup = mainObject.checkup("\n");
    pd.parameters.put("errMessage", checkup.first);
    pd.parameters.put("errValue", checkup.second);
    pd.parameters.put("drawCheckup", drawCheckup ? "yes" : "no");

    String color = HtmlColors.getGreenToRed(checkup.second.floatValue());

    pd.toolTip = I18n.get("PROGRESS") + ": " + JSP.perc(mainObject.getProgress()) + "%";// + step + " "+max;
    pd.width = "100%";
    pd.height = "18px";
    pd.backgroundColor = color;

    return pd;
  }


  public PercentileDisplay getBudgetBarForTask() {
    double budget = mainObject.getForecasted();
    double totalCost = mainObject.getTotalCostsDone();
    int onBudget = (int) (totalCost / budget * 100);
    PercentileDisplay pd = new PercentileDisplay(onBudget);
    pd.toolTip = I18n.get("COST_BUDGET") + " " + JSP.currency(totalCost) + "/" + JSP.currency(budget);
    pd.width = "120px";
    pd.height = "12px";
    if (onBudget == 100)
      pd.percentileColor = "#2CC02C";
    else if (onBudget > 100)
      pd.percentileColor = "#D30202";
    else
      pd.percentileColor = "#FFD515";
    return pd;
  }

  public void suggestCodeFromParent() {
    Task parent = mainObject.getParent();
    if (parent != null) {
      Collection versions = new ArrayList();
      for (Task taskChild : parent.getChildrenSorted()) {
        if (JSP.ex(taskChild.getCode()) && taskChild.getCode().lastIndexOf(".") > -1)
          versions.add(taskChild.getCode().substring(taskChild.getCode().lastIndexOf(".") + 1));
      }
      String parentCode = parent.getCode();
      if (!JSP.ex(parentCode) || "-".equals(parentCode))
        parentCode = "T" + parent.getId();

      boolean codeFound = false;
      String code = parentCode + "." + VersionHome.nextVersion(versions);
      if (Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("USEUNIQUECODES"))) {
        while (!codeFound) {
          mainObject.setCode(code);
          if (JSP.ex(mainObject.getCode()) && mainObject.isUnique("code")) {
            codeFound = true;
          } else {
            code = parentCode + "." + VersionHome.nextVersion(mainObject.getCode().substring(mainObject.getCode().lastIndexOf(".") + 1));
          }
        }
      } else {
        mainObject.setCode(code);
      }


    } else {
      mainObject.setCode(null);
    }
  }

  public ColorValueChooser getStatusChooser(String fieldName) {
    return getStatusChooser(fieldName, "STATUS");
  }

  public ColorValueChooser getStatusChooser(String fieldName, String type) {
    return getStatusChooser(fieldName, type, false, false);
  }

  @Deprecated
  public static ColorValueChooser getStatusChooser(String fieldName, String type, boolean showChoose, boolean multiple, PageState pageState) {
    return getStatusChooser(fieldName, type, showChoose, multiple);
  }

  public static ColorValueChooser getStatusChooser(String fieldName, String type, boolean showChoose, boolean multiple) {

    ColorValueChooser ccv;
    ccv = new ColorValueChooser(fieldName, type);
    ccv.multiSelect = multiple;

    ccv.addCodeColorValue(TaskStatus.STATUS_ACTIVE, "#3BBF67", I18n.get(TaskStatus.STATUS_ACTIVE));
    ccv.addCodeColorValue(TaskStatus.STATUS_SUSPENDED, "#F9C154", I18n.get(TaskStatus.STATUS_SUSPENDED));
    ccv.addCodeColorValue(TaskStatus.STATUS_DONE, "#6EBEF4", I18n.get(TaskStatus.STATUS_DONE));
    ccv.addCodeColorValue(TaskStatus.STATUS_FAILED, "#763A96", I18n.get(TaskStatus.STATUS_FAILED));
    ccv.addCodeColorValue(TaskStatus.STATUS_UNDEFINED, "#dededf", I18n.get(TaskStatus.STATUS_UNDEFINED));
    if (showChoose)
      ccv.addCodeColorValue("", "gray", I18n.get("ALL"));
    return ccv;
  }


  public String getPeriodCommentStart() {
    Period period = mainObject.getSchedule();
    if (period != null)
      return I18n.get(getPeriodCommentPrefix(period.getValidityStartTime()) + "_START_%%",
              (mainObject.isStartIsMilestone() ? "<span class='teamworkIcon' title='" + I18n.get("MILESTONE") + "'>^</span> " : "") +
                      "<span class='date'>" + DateUtilities.dateToFullString(period.getStartDate()) + "</span>");
    else
      return "Schedule null!";
  }

  public String getPeriodCommentEnd() {
    Period period = mainObject.getSchedule();
    if (period != null)
      return I18n.get(getPeriodCommentPrefix(period.getValidityEndTime()) + "_END_%%",
              (mainObject.isEndIsMilestone() ? "<span class='teamworkIcon' title='" + I18n.get("MILESTONE") + "'>^</span> " : "") +
                      "<span class='date'>" + DateUtilities.dateToFullString(period.getEndDate()) + "</span>");
    else
      return "Schedule null!";
  }

  private String getPeriodCommentPrefix(long millis) {
    String prepost = "";
    long now = System.currentTimeMillis();
    if (now < millis)
      prepost = "FUTURE";
    else
      prepost = "PAST";

    return "TASKPERIOD_" + mainObject.getStatus() + "_" + prepost;
  }


  public static ColorValueChooser getAssignmentPriorityCombo(String fieldName, int sizeInPx, PageState pageState) {
    ColorValueChooser ccv = new ColorValueChooser(fieldName, "ASSSIGPRIO", pageState);
    ccv.height = sizeInPx;
    ccv.addCodeColorValue(AssignmentPriority.PRIORITY_LOW + "", AssignmentPriority.getPriorityColor(AssignmentPriority.PRIORITY_LOW), I18n.get("PRIORITY_LOW_SHORT"));
    ccv.addCodeColorValue(AssignmentPriority.PRIORITY_MEDIUM + "", AssignmentPriority.getPriorityColor(AssignmentPriority.PRIORITY_MEDIUM), I18n.get("PRIORITY_MEDIUM_SHORT"));
    ccv.addCodeColorValue(AssignmentPriority.PRIORITY_HIGH + "", AssignmentPriority.getPriorityColor(AssignmentPriority.PRIORITY_HIGH), I18n.get("PRIORITY_HIGH_SHORT"));
    return ccv;
  }

  public static void assignmentPriorityToHtml(int sizeInPx, long time, Assignment a, PageContext pageContext) {
    try {
      int pri = a.getPriorityAtTime(time);
      pageContext.getOut().println("<div class=\"cvcColorSquare priorityLabel\" style=\"background-color:" + AssignmentPriority.getPriorityColor(pri) + "\" title=\"priority: " + pri + "\">" + AssignmentPriority.getPriorityDescription(pri) + "\n" +
              "</div>");

    } catch (IOException e) {
      throw new PlatformRuntimeException(e);
    }
  }

  public List<Listener> getListeners(Operator w) throws PersistenceException {
    String hql = "select listener from " + Listener.class.getName() + " as listener where listener.owner=:ownerx and listener.identifiableId=:identifiableId and listener.theClass=:theClass";
    OqlQuery listenerQH = new OqlQuery(hql);
    listenerQH.getQuery().setEntity("ownerx", w);
    listenerQH.getQuery().setString("identifiableId", mainObject.getId().toString());
    listenerQH.getQuery().setString("theClass", Task.class.getName());
    return listenerQH.list();
  }

  public boolean isSomeAncestorListeningAndPropagating(Operator w) throws PersistenceException {
    if (mainObject.getParent() != null) {
      String hql = "select count(listener.id) from " + Listener.class.getName() + " as listener where listener.owner=:ownerx and listener.identifiableId in (:identifiableIds) " +
              "and listener.theClass=:theClass and listenDescendants= true";
      OqlQuery listenerQH = new OqlQuery(hql);
      listenerQH.getQuery().setEntity("ownerx", w);
      listenerQH.getQuery().setParameterList("identifiableIds", mainObject.getAncestorIdsAsList());
      listenerQH.getQuery().setString("theClass", Task.class.getName());

      return (Long) listenerQH.uniqueResult() > 0;
    } else
      return false;
  }

  public boolean isSomeAncestorListeningAndPropagatingForTheEvent(Operator w, Task.Event type) throws PersistenceException {
    if (mainObject.getParent() != null) {
      String hql = "select count(listener.id) from " + Listener.class.getName() + " as listener where listener.owner=:ownerx and listener.identifiableId in (:identifiableIds) " +
              "and listener.theClass=:theClass and listenDescendants=true and listener.eventType=:evt";
      OqlQuery listenerQH = new OqlQuery(hql);
      listenerQH.getQuery().setEntity("ownerx", w);
      listenerQH.getQuery().setParameterList("identifiableIds", mainObject.getAncestorIdsAsList());
      listenerQH.getQuery().setString("theClass", Task.class.getName());
      listenerQH.getQuery().setString("evt", type.toString());

      return (Long) listenerQH.uniqueResult() > 0;
    } else
      return false;
  }


  public void createListener(Task.Event type, boolean tnd, Operator subscriber, String mediaSubscribed) throws PersistenceException {
    if (mediaSubscribed.length() > 0) {
      Listener l = new Listener(subscriber);
      l.setIdAsNew();
      l.setIdentifiable(mainObject);
      l.setMedia(mediaSubscribed);
      l.setEventType(type.toString());
      l.setListenDescendants(tnd);
      l.store();
    }
  }


  public void removeListeners(Operator subscriber) {
    //remove existing
    String hql = "delete from " + Listener.class.getName() + " as listen where listen.owner = :owner and listen.theClass = :theClass and" +
            " listen.identifiableId = :identifiableId";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setEntity("owner", subscriber);
    oql.getQuery().setString("theClass", Task.class.getName());
    oql.getQuery().setString("identifiableId", mainObject.getId().toString());
    oql.getQuery().executeUpdate();

  }

  public static List<TaskType> getTaskTypes(TeamworkOperator w, Permission p, Area additionalArea) throws PersistenceException {
    OqlQuery oql = makeTypeQuery("", p, w, additionalArea);
    return oql.list();
  }

  public static List<String> getTaskTypesNames(TeamworkOperator w, Permission p, Area additionalArea) throws PersistenceException {
    OqlQuery oql = makeTypeQuery("select distinct tt.description ", p, w, additionalArea);
    return oql.list();
  }

  private static OqlQuery makeTypeQuery(String root, Permission p, TeamworkOperator w, Area additionalArea) throws PersistenceException {

    boolean areasFound = false;

    Set<Area> areas = null;

    String hql = root + "from " + TaskType.class.getName() + " as tt ";

    if (!w.hasPermissionAsAdmin()) {
      areas = w.getAreasForPermission(p);
      if (additionalArea != null)
        areas.add(additionalArea);
      areasFound = areas != null && areas.size() > 0;
      if (areasFound)
        hql = hql + " where tt.area in (:areas)";
    }

    OqlQuery oql = new OqlQuery(hql);
    if (areasFound)
      oql.getQuery().setParameterList("areas", areas);
    else if (!w.hasPermissionAsAdmin()) {
      hql = hql + " where tt.id!=tt.id";
    }
    return oql;
  }


  public static RoleTeamwork getProjectManagerRole(Area area) {
    return WizardSupport.getRoleByNameAndArea(ApplicationState.getApplicationSetting("DEFAULT_PROJECT_MANAGER_ROLE_NAME", "Project manager"), area);
  }

  public static RoleTeamwork getWorkerRole(Area area) {
    return WizardSupport.getRoleByNameAndArea(ApplicationState.getApplicationSetting("DEFAULT_WORKER_ROLE_NAME", "Worker"), area);
  }

  public static RoleTeamwork getCustomerRole(Area area) {
    return WizardSupport.getRoleByNameAndArea(ApplicationState.getApplicationSetting("DEFAULT_CUSTOMER_ROLE_NAME", "Customer"), area);
  }


  public ButtonLink getPlanButton(PageState pageState) {
    PageSeed plSeed = pageState.pageFromRoot("task/plan/planByTask.jsp");
    plSeed.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));
    plSeed.mainObjectId = mainObject.getId();
    ButtonLink pb = new ButtonLink(I18n.get("WORKLOG_SEE_PLAN"), plSeed);
    return pb;
  }


  public static boolean hasCustomField() {
    return DesignerField.hasCustomField("TASK_CUSTOM_FIELD_", 6);
  }

  public String getDisplayName() {
    String ret = "";
    if (mainObject.getCode() != null && !mainObject.getCode().equals("-"))
      ret = JSP.makeTag("span", "class='dnTaskCode'", mainObject.getCode());
    ret += " " + JSP.w(mainObject.getName());
    return ret;
  }


  public static JSONObject getTaskStatusDecoder() {
    JSONObject ret = new JSONObject();
    String[] sts = new String[]{TaskStatus.STATUS_ACTIVE, TaskStatus.STATUS_DONE, TaskStatus.STATUS_FAILED, TaskStatus.STATUS_SUSPENDED, TaskStatus.STATUS_UNDEFINED};
    for (String s : sts) {
      JSONObject cs = new JSONObject();
      cs.element("label", I18n.get(s));
      String statusColor = Task.getStatusColor(s);
      cs.element("color", statusColor);
      cs.element("contrastColor", HtmlColors.contrastColor(statusColor));
      ret.element(s, cs);
    }

    return ret;
  }


  public PersistentFile getImage() {
    PersistentFile ret = null;
    if (mainObject.getJsonData().has("projectImage"))
      ret = PersistentFile.deserialize(mainObject.getJsonData().getString("projectImage"));
    return ret;
  }

  public void setImage(PersistentFile newImage) {
    if (newImage != null)
      mainObject.getJsonData().element("projectImage", newImage.serialize());
    else
      mainObject.getJsonData().remove("projectImage");
  }


  public String getImageUrl() {
    String ret = "";
    if (getImage() != null) {
      PageSeed imgPs = getImage().getPageSeed(false);
      imgPs.disableCache = false;
      ret = imgPs.toLinkToHref();
    }
    return ret;
  }

}
