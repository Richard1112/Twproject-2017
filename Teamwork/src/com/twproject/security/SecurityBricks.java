package com.twproject.security;

import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Person;
import com.twproject.task.Assignment;
import com.twproject.task.Task;
import org.jblooming.operator.Operator;
import org.jblooming.oql.OqlQuery;
import org.jblooming.oql.QueryHelper;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.security.Area;
import org.jblooming.security.Permission;
import org.jblooming.security.Role;
import org.jblooming.utilities.CodeValueList;
import org.jblooming.waf.Bricks;
import org.jblooming.waf.html.input.Combo;
import org.jblooming.waf.html.input.SmartCombo;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.RestState;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class SecurityBricks extends Bricks {

  public static SmartCombo getOperatorCombo(String fieldName) {

    String hql = "select operator.id, operator.name || ' ' || operator.surname from " + Operator.class.getName() + " as operator ";

    String whereForId = "where operator.id = :" + SmartCombo.FILTER_PARAM_NAME;

    String whereForFiltering =
      " where operator.name || ' ' || operator.surname like :" + SmartCombo.FILTER_PARAM_NAME +
        " or operator.surname || ' ' || operator.name like :" + SmartCombo.FILTER_PARAM_NAME;

    return new SmartCombo(fieldName, hql, whereForFiltering, whereForId);
  }

  public static SmartCombo getRoleComboForAssignments(String fieldName, Task task, PageState pageState) throws PersistenceException {
    return getRoleComboForAssignments(fieldName, task, false, pageState);
  }

  public static SmartCombo getRoleComboForAssignments(String fieldName, Task task, boolean shortLabels, RestState pageState) throws PersistenceException {
    Set<Permission> permissionsRequired = new HashSet();
    //permissionsRequired.add(TeamworkPermissions.assignment_manage);
    permissionsRequired.add(TeamworkPermissions.assignment_canCRW);
    return getRoleCombo(fieldName, permissionsRequired, task, shortLabels, pageState);
  }

  private static SmartCombo getRoleCombo(String fieldName, Set<Permission> permissionsRequired, Task task, boolean shortLabels, RestState pageState) throws PersistenceException {
    String ids = "";

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    String hql;
    if (shortLabels) {
      hql = "select p.id, p.code,p.name, p.area.name from " + RoleTeamwork.class.getName() + " as p ";
    } else {
      if (isSingleArea()) {
        hql = "select p.id, p.name from " + RoleTeamwork.class.getName() + " as p ";
      } else {
        hql = "select p.id, '(' || p.area.name || ') ' || p.name from " + RoleTeamwork.class.getName() + " as p ";
      }
    }

    Set<Area> areas = new HashSet();
    for (Permission permissionRequired : permissionsRequired) {
      Set<Area> areasForPerm = logged.getAreasForPermission(permissionRequired);
      if (areasForPerm != null && areasForPerm.size() > 0)
        areas.addAll(areasForPerm);

      //this is used if you have only task_create (a kickstarter)
      if (task!=null && task.hasPermissionFor(logged,permissionRequired))
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

    boolean first = true;
    for (Area a : areas) {
      if (!first)
        ids = ids + ",";
      first = false;
      ids = ids + a.getId();
    }

    String whereForFiltering;

    if (shortLabels)
      whereForFiltering = "where (" +
        "p.code || p.name like :" + SmartCombo.FILTER_PARAM_NAME +
        " or p.name || p.code like :" + SmartCombo.FILTER_PARAM_NAME +
        ")";
    else
      whereForFiltering = "where (" +
        "'(' || p.area.name || ') ' || p.name like :" + SmartCombo.FILTER_PARAM_NAME +
        " or p.area.name || ' ' || p.name like :" + SmartCombo.FILTER_PARAM_NAME +
        " or p.name like :" + SmartCombo.FILTER_PARAM_NAME +
        ")";

    if (ids.length() > 0)
      whereForFiltering = whereForFiltering + " and p.area.id in (" + ids + ")";
    else
      whereForFiltering = whereForFiltering + " and p.area is null";

    whereForFiltering = whereForFiltering + " and p.localToAssignment = :truth order by p.area.name, p.name";

    String whereForId = " where p.id = :" + SmartCombo.FILTER_PARAM_NAME;

    SmartCombo roles = new SmartCombo(fieldName, hql, whereForFiltering, whereForId);
    roles.searchAll = true;
    roles.fixedParams.put("truth", Boolean.TRUE);
    roles.separator = "</td><td >";
    roles.fieldSize = 30;

    //roles.classic=true;

    return roles;
  }

  public static Set<Role> getRolesFor(Permission permissionRequiredOnArea, boolean localToAssignment, RestState pageState) throws PersistenceException {

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();


    String ids = "";
    Set<Area> areas = getAreasForLogged(permissionRequiredOnArea, pageState);
    boolean first = true;
    for (Area a : areas) {
      if (!first)
        ids = ids + ",";
      first = false;
      ids = ids + a.getId();
    }

    String hql = "select p from " + RoleTeamwork.class.getName() + " as p ";
    QueryHelper qh = new QueryHelper(hql);

    if (localToAssignment)
      qh.addOQLClause("p.localToAssignment = :truth  ", "truth", Boolean.TRUE);
    else
      qh.addOQLClause("p.localToAssignment = :falsity ", "falsity", Boolean.FALSE);

    if (ids.length() > 0) {
      qh.addOQLClause("( p.area.id in (" + ids + ")  ");

      if (logged.hasPermissionAsAdmin()) {
        qh.addOrQueryClause("p.area is null");
      }
      qh.addToHqlString(")");

    } else if (logged.hasPermissionAsAdmin())
      qh.addQueryClause("p.area is null");


    //si tolgono i ruoli "riservati" manager e myself.
    qh.addOQLClause("not p.code like '(%)'");

    qh.addToHqlString(" order by p.name");

    List<Role> roles = qh.toHql().list();
    return new HashSet(roles);
  }

  private static long _lastRefresh=0;
  private static int _numOfAreas=0;
  public static int countActiveAreas(){
    if (System.currentTimeMillis()>_lastRefresh+60000) { // una query al minuto
      String q = "select count(a.id) from " + Area.class.getName() + " as a";
      _numOfAreas=((Long) new OqlQuery(q).uniqueResultNullIfEmpty()).intValue();
      _lastRefresh=System.currentTimeMillis();
    }
    return _numOfAreas;
  }

  public static boolean isSingleArea(){
    return SecurityBricks.countActiveAreas()<=1;
  }

  public static Set<Area> getAreasForLogged(Permission permissionRequiredOnArea, RestState pageState) throws PersistenceException {
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    return logged.getAreasForPermission(permissionRequiredOnArea);
  }

  public static Combo getAreaCombo(String fieldName, Permission permissionsRequiredOnArea, PageState pageState) throws PersistenceException {
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    Set<Area> areas = logged.getAreasForPermission(permissionsRequiredOnArea);

    CodeValueList cvl = CodeValueList.getInstanceForIdentifiables(areas);
    Combo cb = new Combo(fieldName, "</td><td>", "", 255, cvl, null);
    cb.label = pageState.getI18n(cb.fieldName);
    return cb;
  }


}
