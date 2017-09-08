package com.twproject.security.businessLogic;

import com.twproject.operator.TeamworkOperator;
import com.twproject.security.RoleTeamwork;
import com.twproject.security.SecurityBricks;
import com.twproject.task.Task;
import com.twproject.waf.settings.ReportBricks;
import org.jblooming.security.*;
import org.jblooming.security.SecurityException;
import org.jblooming.utilities.JSP;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.ontology.businessLogic.DeleteHelper;
import org.jblooming.ontology.SerializedMap;
import org.jblooming.oql.QueryHelper;
import org.jblooming.page.HibernatePage;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.waf.ActionSupport;
import org.jblooming.waf.ActionUtilities;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.constants.RoleConstants;
import org.jblooming.waf.html.container.TabSet;
import org.jblooming.waf.html.display.DataTable;
import org.jblooming.waf.html.display.Paginator;
import org.jblooming.waf.html.input.Collector;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.state.PersistentSearch;
import org.jblooming.waf.view.ClientEntries;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageState;

import java.util.Set;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class TeamworkRoleAction extends ActionSupport {

  public TeamworkOperator logged;

  public TeamworkRoleAction(PageState pageState) {
    super(pageState);
    this.logged = (TeamworkOperator) pageState.getLoggedOperator();

  }

  public void cmdDelete() throws org.jblooming.security.SecurityException, PersistenceException {
    restState.initializeEntries("table");
    RoleTeamwork delenda = (RoleTeamwork) PersistenceHome.findByPrimaryKey(RoleTeamwork.class, restState.getMainObjectId());
    delenda.testPermission(logged, PlatformPermissions.role_canCreate);
    DeleteHelper.cmdDelete(delenda, restState);
  }

  public void cmdSave() throws org.jblooming.security.SecurityException, PersistenceException {
    restState.initializeEntries("table");

    RoleTeamwork role = null;

    boolean isNew = PersistenceHome.NEW_EMPTY_ID.equals(restState.getMainObjectId());

    if (isNew) {
      role = new RoleTeamwork();
      //role.setIdAsNewSer();
      role.setIdAsNew();
      role.setOwner(logged);
    } else {
      role = (RoleTeamwork) PersistenceHome.findByPrimaryKey(RoleTeamwork.class, restState.getMainObjectId());
      if (role == null) throw new FindByPrimaryKeyException();
    }

    String areaId = restState.getEntryAndSetRequired("AREA").stringValueNullIfEmpty();
    if ("SYSTEM_ROLE".equals(areaId)) {
      role.setArea(null);
    } else
      ActionUtilities.setIdentifiable(restState.getEntryAndSetRequired("AREA"), role, "area");

    role.testPermission(logged, PlatformPermissions.role_canWrite);

    ActionUtilities.setBoolean(restState.getEntry("LOCAL_TO_ASS"), role, "localToAssignment");
    ActionUtilities.setString(restState.getEntryAndSetRequired(RoleConstants.FLD_ROLE_NAME), role, "name");
    ActionUtilities.setString(restState.getEntryAndSetRequired(RoleConstants.FLD_ROLE_CODE), role, "code");

    ActionUtilities.setString(restState.getEntry(RoleConstants.FLD_ROLE_DESCRIPTION), role, "description");


    //permissions
    for (Permission p : ApplicationState.getPermissions()) {
      if (restState.getEntry(p.name).checkFieldValue())
        role.addPermission(p);
      else
        role.removePermission(p);
    }


    //report permissions
    for (ReportBricks.Report report : ReportBricks.getAllReports()) {
      if (restState.getEntry(report.requiredPermission.name).checkFieldValue())
        role.addPermission(report.requiredPermission);
      else
        role.removePermission(report.requiredPermission);
    }


    //subscriptions
    SerializedMap<String, String> subm = new SerializedMap();
    role.setDefaultSubscriptions(subm);
    for (Task.Event event: Task.Event.values() ){
      for (MessagingSystem.Media media : MessagingSystem.activeMedia) {
        String subscrField = event + "_" + media.toString().toUpperCase();
        if (JSP.ex(restState.getEntry(subscrField)))
          subm.put(subscrField, restState.getEntry(subscrField).stringValueNullIfEmpty());
      }
    }
    // add descend
    subm.put("TASK_NOTIFY_DESC", restState.getEntry("TASK_NOTIFY_DESC").stringValueNullIfEmpty());


    restState.setMainObject(role);

    if (restState.validEntries()) {
      role.store();
      if (isNew)
        TabSet.pointToTab("roleTabSet", "ROLE_PERMISSIONS_TAB", restState);
    }


    //si svuota la cache dei permessi
    PermissionCache.emptyCache();

    //si scaricano i ruoli in memoria
    RoleTeamwork.unloadRoles();

  }



  public void cmdFind() throws PersistenceException {

    String hql = "from " + RoleTeamwork.class.getName() + " as role";
    QueryHelper qhelp = new QueryHelper(hql);

    boolean recoveredFromSavedFilter = PersistentSearch.feedFromSavedSearch(restState);

    String FLD_des = restState.getEntry("name").stringValueNullIfEmpty();
    if (FLD_des != null) {
      qhelp.addQBEORClauses(
              FLD_des,
              qhelp.getOrElement("role.code", "code", QueryHelper.TYPE_CHAR),
              qhelp.getOrElement("role.name", "name", QueryHelper.TYPE_CHAR)
      );
    }

    String areaId = restState.getEntry("AREA").stringValueNullIfEmpty();

    if (areaId != null) {

      if ("__SYSTEM__".equals(areaId))
        qhelp.addOQLClause("role.area is null");
      else
        qhelp.addOQLClause("role.area.id= :areaId", "areaId", Integer.parseInt(areaId));
    }

    if (!logged.hasPermissionAsAdmin()) {
      Set<Area> areas = SecurityBricks.getAreasForLogged(PlatformPermissions.role_canRead, restState);
      if (areas.size() > 0)
        qhelp.addOQLClause("role.area in (:areas)", "areas", areas);
      else
        qhelp.addOQLClause("0=1");
    }

    DataTable.orderAction(qhelp, "ROLELH", restState, "role.name");

    restState.setPage(HibernatePage.getHibernatePageInstance(qhelp.toHql().getQuery(),
            Paginator.getWantedPageNumber(restState),
            Paginator.getWantedPageSize("ROLELH", restState)));

  }

  public ClientEntries make(RoleTeamwork role){

    ClientEntries ces = new ClientEntries();
    ces.addRequiredEntry(RoleConstants.FLD_ROLE_NAME, role.getName());
    ces.addRequiredEntry(RoleConstants.FLD_ROLE_CODE, role.getCode());

    ces.addEntry(RoleConstants.FLD_ROLE_DESCRIPTION, role.getDescription());

    if (role.getArea() != null)
      ces.addEntry("AREA", role.getArea().getId() + "");
    else
      ces.addEntry("AREA", "SYSTEM_ROLE");

    ces.addEntry("LOCAL_TO_ASS", role.isLocalToAssignment() ? Fields.TRUE : Fields.FALSE);

    for (Permission p : ApplicationState.getPermissions()) {
      if (role.hasPermissionFor(p))
        ces.addEntry(new ClientEntry(p.getName(), Fields.TRUE));
    }

    //report permissions
    for (ReportBricks.Report report : ReportBricks.getAllReports()) {
      if (role.hasPermissionFor(report.requiredPermission))
        ces.addEntry(new ClientEntry(report.requiredPermission.getName(), Fields.TRUE));
    }


    return ces;
  }

  public void cmdEdit() throws PersistenceException, org.jblooming.security.SecurityException {

    RoleTeamwork role = (RoleTeamwork) PersistenceHome.findByPrimaryKey(RoleTeamwork.class, restState.getMainObjectId());
    role.testPermission(logged, PlatformPermissions.role_canRead);


    //make subscriptions
    SerializedMap<String, String> subm = role.getDefaultSubscriptions();
    if (subm != null) {
      for (String k : subm.keySet()) {
        restState.addClientEntry(k, subm.get(k));
      }
    }


    restState.setMainObject(role);
    restState.getClientEntries().addEntries(make(role));
  }

  public void cmdAdd() throws PersistenceException, SecurityException {
    restState.initializeEntries("table");
    RoleTeamwork role = new RoleTeamwork();
    //role.setIdAsNewSer();
    role.setIdAsNew();

    boolean passed = logged.hasPermissionFor(PlatformPermissions.role_canCreate) || logged.getAreasForPermission(PlatformPermissions.role_canCreate).size() > 0;

    SecurityException.riseExceptionIfNoPermission(passed, PlatformPermissions.role_canCreate, logged);

    if (restState.getEntry("AREA").stringValueNullIfEmpty() != null) {

      Area a = (Area) PersistenceHome.findByPrimaryKey(Area.class, restState.getEntry("AREA").stringValueNullIfEmpty());
      role.setArea(a);
    }

    boolean ltp = restState.getEntry("LOCAL_TO_ASS").checkFieldValue();
    role.setLocalToAssignment(ltp);

    restState.setMainObject(role);
  }

  public void cmdMove(String s) throws FindByPrimaryKeyException {
    Collector.move("permColl", restState);
    RoleTeamwork role;
    if (PersistenceHome.NEW_EMPTY_ID.equals(restState.getMainObjectId())) {
      role = new RoleTeamwork();
      //role.setIdAsNewSer();
      role.setIdAsNew();
    } else {
      role = (RoleTeamwork) PersistenceHome.findByPrimaryKey(RoleTeamwork.class, restState.getMainObjectId());
      if (role == null)
        throw new FindByPrimaryKeyException();
    }
    restState.setMainObject(role);
  }


  public void cmdCrownMe() throws PersistenceException, SecurityException {
    restState.initializeEntries("table");
    cmdEdit();
    logged.addRoleAndPersist((RoleTeamwork) restState.getMainObject());
  }

  public void cmdClone() throws SecurityException, PersistenceException {
    restState.initializeEntries("table");

    RoleTeamwork role = (RoleTeamwork) PersistenceHome.findByPrimaryKey(RoleTeamwork.class, restState.getMainObjectId());

    role.testPermission(logged, PlatformPermissions.role_canRead);

    role.copy();
    role.setCode("[" + role.getCode() + "]");
    role.setName("[" + role.getName() + "]");

    restState.setMainObject(role);
    restState.getClientEntries().addEntries(make(role));

    role.store();

  }

}