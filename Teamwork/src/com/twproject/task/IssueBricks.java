package com.twproject.task;

import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Person;
import com.twproject.resource.Resource;
import com.twproject.security.RoleTeamwork;
import com.twproject.security.TeamworkPermissions;
import org.jblooming.designer.DesignerField;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.FindException;
import org.jblooming.security.Role;
import org.jblooming.waf.Bricks;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.html.button.ButtonJS;
import org.jblooming.waf.html.button.ButtonSupport;
import org.jblooming.waf.html.input.ColorValueChooser;
import org.jblooming.waf.html.input.SmartCombo;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;
import org.jblooming.oql.QueryHelper;
import org.jblooming.oql.OqlQuery;
import org.jblooming.security.Area;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.view.RestState;

import java.io.Serializable;
import java.util.Set;
import java.util.HashSet;
import java.util.List;
import java.util.TreeSet;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class IssueBricks extends Bricks {

  public Issue mainObject;


  public IssueBricks(Issue document) {
    this.mainObject = document;
  }


  public static ButtonJS getBlackEditor(Serializable id) {
    return getBlackEditor(id, Commands.EDIT);
  }

  public static ButtonJS getBlackEditor(Serializable id, String command) {
    return getBlackEditor(id, command, "");
  }

  public static ButtonJS getBlackEditor(Serializable id, String command, String params) {
    return new ButtonJS("openIssueEditorInBlack('" + id + "','" + command + "','" + params + "');");
  }

  public static ButtonLink getPopoupLinkToEditor(Serializable id) {
    PageSeed edit = new PageSeed(ApplicationState.serverURL + "/applications/teamwork/issue/issueList.jsp");
    edit.setCommand(Commands.FIND);
    edit.addClientEntry("ISSUE_ID", id);
    ButtonLink link = new ButtonLink(edit);
    return link;
  }

  public String getGravityColor() {
    String color = "#666666";
    if (Issue.GRAVITY_BLOCK.equals(mainObject.getGravity()))
      color = "#FF0000";
    else if (Issue.GRAVITY_CRITICAL.equals(mainObject.getGravity()))
      color = "#9A5932";
    else if (Issue.GRAVITY_HIGH.equals(mainObject.getGravity()))
      color = "#F9791C";  //FB7B1E
    else if (Issue.GRAVITY_MEDIUM.equals(mainObject.getGravity()))
      color = "#FFF32C";
    else if (Issue.GRAVITY_LOW.equals(mainObject.getGravity()))
      color = "#FFFFFF";
    return color;
  }

  public static SmartCombo getIssueCombo(String fieldName, boolean onlyOpenIssues, String additionalHql) {

    String hql = "select issue.id, substring(issue.description,1,40) || '..' from " + Issue.class.getName() + " as issue ";
    QueryHelper queryHelperForFiltering = new QueryHelper(hql);

    if (additionalHql != null && additionalHql.trim().length() > 0)
      queryHelperForFiltering.addOQLClause(additionalHql);

    //filter
    String baseFilter = " (issue.description like :" + SmartCombo.FILTER_PARAM_NAME + ")";

    if (onlyOpenIssues) {
      baseFilter = baseFilter + " and (issue.status.behavesAsOpen = true)";
    }

    queryHelperForFiltering.addOQLClause(baseFilter);

    queryHelperForFiltering.addToHqlString(" order by issue.description");


    String whereForId = "where issue.id = :" + SmartCombo.FILTER_PARAM_NAME;

    SmartCombo taskSC = new SmartCombo(fieldName, hql, null, whereForId);
    taskSC.queryHelperForFiltering = queryHelperForFiltering;

    taskSC.separator = "</td><td>";
    taskSC.fieldSize = 40;

    return taskSC;
  }

  public static ColorValueChooser getStatusChooser(String fieldName, String type, PageState pageState) throws FindException {
    return getStatusChooser(fieldName, type, false, false, pageState);
  }

  public static ColorValueChooser getStatusChooser(String fieldName, String type, boolean addChoose, boolean multiple, PageState pageState) throws FindException {

    ColorValueChooser ccv;
    ccv = new ColorValueChooser(fieldName, type, pageState);
    ccv.multiSelect = multiple;

    List<IssueStatus> il = new OqlQuery("select iss from " + IssueStatus.class.getName() + " as iss order by iss.orderBy").list();
    for (IssueStatus is : il) {
      ccv.addCodeColorValue(is.getId() + "", is.getColor(), is.getDescription());
    }
    if (addChoose)
      ccv.addCodeColorValue("", "gray", "- " + pageState.getI18n("EDITOR_CHOOSE") + " -");
    return ccv;
  }

  public static ColorValueChooser getGravityChooser(String fieldName, String type, boolean addChoose, boolean multiple, PageState pageState) {

    ColorValueChooser ccv;
    ccv = new ColorValueChooser(fieldName, type, pageState);
    ccv.multiSelect = multiple;
    if (addChoose)
      ccv.addCodeColorValue("", "gray", "- " + pageState.getI18n("EDITOR_CHOOSE") + " -");

    for (String grv : Issue.getGravities())
      ccv.addCodeColorValue(grv, getGravityColor(grv), pageState.getI18n(grv));

    return ccv;
  }

  public static int getGravityOrder(String gravity) {
    if (Issue.GRAVITY_BLOCK.equals(gravity)) return 1;
    else if (Issue.GRAVITY_CRITICAL.equals(gravity)) return 2;
    else if (Issue.GRAVITY_HIGH.equals(gravity)) return 3;
    else if (Issue.GRAVITY_MEDIUM.equals(gravity)) return 4;
    else
      return 99;
  }

  public static String getGravityColor(String gravity) {

    if (Issue.GRAVITY_BLOCK.equals(gravity)) return "#DB2727";
    else if (Issue.GRAVITY_CRITICAL.equals(gravity)) return "#8C6044";
    else if (Issue.GRAVITY_HIGH.equals(gravity)) return "#F9791C";
    else if (Issue.GRAVITY_MEDIUM.equals(gravity)) return "#F9C154";
    else
      return "#EEEEEE";
  }


  public static SmartCombo getIssueTypeCombo(String fieldName, Issue issue, PageState pageState) throws PersistenceException {
    String hql = "select tt.id, tt.description from " + IssueType.class.getName() + " as tt ";
    QueryHelper queryHelperForFiltering = new QueryHelper(hql);
    TeamworkOperator loggedOperator = (TeamworkOperator) pageState.getLoggedOperator();

    Set<Area> areas = new HashSet();

    //26March2008: RP decided to show only those of the area of the task
    if (issue != null && issue.getTask() != null) {
      areas.add(issue.getTask().getArea());
    } else {
      Area myArea = loggedOperator.getPerson().getArea();
      if (myArea != null)
        areas.add(myArea);

      Set<Area> areasGl = loggedOperator.getAreasForPermission(TeamworkPermissions.issue_canWrite);
      if (JSP.ex(areasGl))
        areas.addAll(areasGl);
    }

    queryHelperForFiltering.addQueryClause("tt.area in (:areas) or tt.area is null");
    queryHelperForFiltering.addParameter("areas", areas);

    String baseFilter = " (tt.description like :" + SmartCombo.FILTER_PARAM_NAME + ") ";

    queryHelperForFiltering.addOQLClause(baseFilter);

    queryHelperForFiltering.addToHqlString(" order by tt.intValue, tt.description");

    String whereForId = "where tt.id = :" + SmartCombo.FILTER_PARAM_NAME;

    SmartCombo taskSC = new SmartCombo(fieldName, hql, null, whereForId);
    taskSC.searchAll = true;
    taskSC.queryHelperForFiltering = queryHelperForFiltering;
    taskSC.separator = "</td><td>";
    taskSC.fieldSize = 15;

    if (pageState.getLoggedOperator().hasPermissionFor(TeamworkPermissions.classificationTree_canManage)) {
      PageSeed issueTypeEditor = pageState.pageFromRoot("issue/issueType.jsp");
      ButtonSupport addTT = ButtonLink.getBlackInstance(I18n.get("ADD_TYPE"), issueTypeEditor);
      addTT.enabled = loggedOperator.hasPermissionFor(TeamworkPermissions.classificationTree_canManage);
      taskSC.addEntityButton = addTT;
    }


    return taskSC;
  }


  public static SmartCombo getIssueImpactCombo(String fieldName, Issue issue, PageState pageState) throws PersistenceException {
    String hql = "select tt.id, tt.description from " + IssueImpact.class.getName() + " as tt ";
    QueryHelper queryHelperForFiltering = new QueryHelper(hql);
    TeamworkOperator loggedOperator = (TeamworkOperator) pageState.getLoggedOperator();

    Set<Area> areas = new HashSet();

    //26March2008: RP decided to show only those of the area of the task
    if (issue != null && issue.getTask() != null) {
      areas.add(issue.getTask().getArea());
    } else {
      Area myArea = loggedOperator.getPerson().getArea();
      if (myArea != null)
        areas.add(myArea);

      Set<Area> areasGl = loggedOperator.getAreasForPermission(TeamworkPermissions.issue_canWrite);
      if (JSP.ex(areasGl))
        areas.addAll(areasGl);
    }

    queryHelperForFiltering.addQueryClause("tt.area in (:areas) or tt.area is null");
    queryHelperForFiltering.addParameter("areas", areas);


    String baseFilter = " (tt.description like :" + SmartCombo.FILTER_PARAM_NAME + ") ";

    queryHelperForFiltering.addOQLClause(baseFilter);

    queryHelperForFiltering.addToHqlString(" order by tt.intValue, tt.description");

    String whereForId = "where tt.id = :" + SmartCombo.FILTER_PARAM_NAME;

    SmartCombo impactSC = new SmartCombo(fieldName, hql, null, whereForId);
    impactSC.searchAll = true;
    impactSC.queryHelperForFiltering = queryHelperForFiltering;
    impactSC.separator = "</td><td>";
    impactSC.fieldSize = 20;

    if (pageState.getLoggedOperator().hasPermissionFor(TeamworkPermissions.classificationTree_canManage)) {
      ButtonSupport addTT = ButtonLink.getBlackInstance(I18n.get("ADD"), pageState.pageFromRoot("issue/issueImpact.jsp"));
      addTT.additionalCssClass = "small";
      impactSC.addEntityButton = addTT;
    }

    return impactSC;
  }

  public static boolean hasCustomField() {
    return DesignerField.hasCustomField("ISSUE_CUSTOM_FIELD_", 6);
  }

  public static void addOpenStatusFilter(PageSeed pageSeed) {
    String openStatuses = "";
    for (IssueStatus iss : IssueStatus.getStatusesAsOpen())
      openStatuses += iss.getId() + ",";
    openStatuses = (openStatuses.endsWith(",") ? openStatuses.substring(0, openStatuses.length() - 1) : openStatuses);
    pageSeed.addClientEntry("FLT_ISSUE_STATUS", openStatuses);
  }

  public static void addCloseStatusFilter(PageSeed pageSeed) {
    String closeStatuses = "";
    for (IssueStatus iss : IssueStatus.getStatusesAsClose())
      closeStatuses += iss.getId() + ",";
    closeStatuses = (closeStatuses.endsWith(",") ? closeStatuses.substring(0, closeStatuses.length() - 1) : closeStatuses);
    pageSeed.addClientEntry("FLT_ISSUE_STATUS", closeStatuses);
  }


  public static void addSecurityClauses(QueryHelper qhelp, RestState restState) throws PersistenceException {
    TeamworkOperator logged = (TeamworkOperator) restState.getLoggedOperator();
    if (!logged.hasPermissionAsAdmin()) {

      Person loggedPerson1 = logged.getPerson();

      //take care that this alias is used also out of the method e.g. in search
      qhelp.addJoinAlias(" left outer join task.assignments as assignment");

      // TASK CLAUSES

      //open unique clause and set owner; qhelp puts the "and" before; all following clauses are in or
      qhelp.addOQLClause("( issue.owner = :logged ");

      //se il ruolo myself ha l'issue read
      if (RoleTeamwork.getMyselfRole().hasPermissionFor(TeamworkPermissions.issue_canRead)) {
        qhelp.addOrQueryClause("issue.assignedTo=:loggedRes ");
        qhelp.addParameter("loggedRes", loggedPerson1);
      }

      qhelp.addOrQueryClause("( ( task.owner = :logged");
      qhelp.addParameter("logged", logged);

      //areas
      Set<Area> areas = logged.getAreasForPermission(TeamworkPermissions.issue_canRead);
      if (areas.size() > 0) {
        qhelp.addOrQueryClause("task.area in (:areas)");
        qhelp.addParameter("areas", areas);
      }

      //assignments
      if (loggedPerson1 != null) {

        List<Resource> myAncs = loggedPerson1.getAncestors();

        OqlQuery oqlQuery = new OqlQuery(
          " select distinct role from " + Assignment.class.getName() + " as ass join ass.role as role where role.permissionIds like :issRead and " +
            "ass.resource in (:myAncs)");

        oqlQuery.getQuery().setParameterList("myAncs", myAncs);
        oqlQuery.getQuery().setString("issRead", "%" + TeamworkPermissions.issue_canRead.toString() + "%");

        List<Role> roles = oqlQuery.list();
        if (roles.size() > 0) {
          qhelp.addOrQueryClause("assignment.role in (:assigRoles) and assignment.resource in (:myAncs)");
          qhelp.addParameter("myAncs", myAncs);
          qhelp.addParameter("assigRoles", roles);
        }


        //se il mamager ha i permessi di issue read, si mette il suo staff
        if (RoleTeamwork.getManagerRole().hasPermissionFor(TeamworkPermissions.issue_canRead)) {
          TreeSet<Resource> myStaff = loggedPerson1.getAllMyStaff();
          if (myStaff.size()>0){
            qhelp.addOrQueryClause("assignment.resource in (:myStaff)");
            qhelp.addParameter("myStaff", myStaff);

          }
        }


      }

      //in order to keep all security conditions in a unique and clause
      qhelp.addToHqlString(" ) or task is null ");

      // RESOURCE CLAUSES
      //open unique clause and set owner; qhelp puts the "and" before; all following clauses are in or
      qhelp.addOQLClause("( resource.owner = :logged", "logged", logged);

      //areas
      if (areas.size() > 0) {
        qhelp.addOrQueryClause("resource.area in (:areas)");
      }

      //in order to keep all security conditions in a unique and clause
      qhelp.addToHqlString(") ) ");

      //end security big clause
      qhelp.addToHqlString(")");

    }
  }
}
