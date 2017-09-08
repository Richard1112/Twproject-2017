package com.twproject.security;

import com.twproject.operator.TeamworkOperator;
import com.twproject.setup.WizardSupport;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.operator.Operator;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.security.Permission;
import org.jblooming.security.RoleWithArea;
import org.jblooming.security.Area;
import org.jblooming.utilities.JSP;
import org.jblooming.ontology.SerializedMap;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.oql.OqlQuery;
import org.jblooming.tracer.Tracer;
import org.hibernate.Query;

import java.util.List;
import java.io.Serializable;

public class RoleTeamwork extends RoleWithArea {

  private String code;
  private boolean localToAssignment;

  private static RoleTeamwork MANAGER_ROLE = null;
  private static RoleTeamwork MYSELF_ROLE = null;

  //private Set<Assignment> assignments = new HashSet<Assignment>();


  private SerializedMap<String, String> defaultSubscriptions = new SerializedMap<String, String>();

  public boolean isLocalToAssignment() {
    return localToAssignment;
  }

  public void setLocalToAssignment(boolean localToAssignment) {
    this.localToAssignment = localToAssignment;
  }


  public String getDisplayName() {
    return JSP.w(getName()) + (getArea() != null ? "&nbsp;(" + JSP.w(getArea().getName()) + ")" : "");
  }

  public String getCode() {
    return code;
  }

  public void setCode(String code) {
    this.code = code;
  }

  /**
   * @return a serialized map with default subsription for this role. Map key are the name of ClientEntry of subscriptions, value is the value :-)
   */
  public SerializedMap<String, String> getDefaultSubscriptions() {
    return defaultSubscriptions;
  }

  public void setDefaultSubscriptions(SerializedMap<String, String> defaultSubscriptions) {
    this.defaultSubscriptions = defaultSubscriptions;
  }

  public static RoleTeamwork load(Serializable roleId) throws FindByPrimaryKeyException {
    return (RoleTeamwork) PersistenceHome.findByPrimaryKey(RoleTeamwork.class, roleId);
  }

  /**
   * @param nameOrCode        used for finding roles name or code starting with
   * @param area              if null is not used for filtering
   * @param localToAssignment if null is not used for filtering
   * @return a Role using name or code
   */
  public static RoleTeamwork guess(String nameOrCode, Area area, boolean localToAssignment) {
    RoleTeamwork r = null;
    try {
      String hql = "select r from " + RoleTeamwork.class.getName() + " as r where r.localToAssignment=" + (localToAssignment ? "true" : "false") + " and (lower(r.name)=:n or lower(r.code)=:n)";
      hql += area == null ? "" : " and r.area=:a";
      Query query = new OqlQuery(hql).getQuery();
      query.setString("n", nameOrCode.toLowerCase());
      if (area != null)
        query.setEntity("a", area);
      query.setMaxResults(2);
      List<RoleTeamwork> rs = query.list();
      if (rs.size() == 1)
        r = rs.get(0);
    } catch (Throwable t) {
      Tracer.platformLogger.error(t);
    }

    return r;
  }


  public static RoleTeamwork getMyselfRole() {
    if (MYSELF_ROLE == null) {
      MYSELF_ROLE = (RoleTeamwork) PersistenceHome.findUniqueNullIfEmpty(RoleTeamwork.class, "code", "(MY)");
      if (MYSELF_ROLE == null)
        try {

          RoleTeamwork role;
          role = new RoleTeamwork();
          role.setIdAsNew();
          role.setCode("(MY)");
          role.setName("(Myself)");
          role.setDescription("It defines what a user can do on his own resource.");
          role.setLocalToAssignment(false);
          role.setArea(null);
          role.setOwner(null);

          role.addPermission(TeamworkPermissions.resource_canRead);
          //rm.addPermission(TeamworkPermissions.resource_canWrite);
          role.addPermission(TeamworkPermissions.resource_manage);
          role.addPermission(TeamworkPermissions.resource_cost_canRead);

          role.addPermission(TeamworkPermissions.issue_canRead);
          role.addPermission(TeamworkPermissions.issue_canChangeStatus);

          role.store();
          MYSELF_ROLE = role;

        } catch (PersistenceException e) {
          throw new PlatformRuntimeException(e);
        }
    }
    return MYSELF_ROLE;
  }


  public static RoleTeamwork getManagerRole() {
    if (MANAGER_ROLE == null) {
      MANAGER_ROLE = (RoleTeamwork) PersistenceHome.findUniqueNullIfEmpty(RoleTeamwork.class, "code", "(RM)");
      if (MANAGER_ROLE == null)
        try {
          RoleTeamwork role;
          role = new RoleTeamwork();
          role.setIdAsNew();
          role.setCode("(RM)");
          role.setName("(Manager)");
          role.setDescription("Is a 'manager' of a resource. It defines what the manager can do on his staff, their tasks and issues.");
          role.setLocalToAssignment(false);
          role.setArea(null);
          role.setOwner(null);


          role.addPermission(TeamworkPermissions.resource_canRead);
          //role.addPermission(TeamworkPermissions.resource_canWrite);
          role.addPermission(TeamworkPermissions.resource_manage);
          role.addPermission(TeamworkPermissions.resource_cost_canRead);

          role.addPermission(TeamworkPermissions.task_canRead);  // il manager può leggere l'elenco dei task delle sue risorse

          role.addPermission(TeamworkPermissions.issue_canRead);  // il manager può leggere l'elenco delle issue delle sue risorse
          //role.addPermission(TeamworkPermissions.issue_canChangeStatus);

          role.store();
          MANAGER_ROLE=role;
        } catch (PersistenceException e) {
          throw new PlatformRuntimeException(e);
        }
    }
    return MANAGER_ROLE;
  }


  public boolean hasPermissionForOR(List<Permission> permissions) {
    boolean ret=false;
    for (Permission p: permissions){
      if (this.hasPermissionFor(p)){
        ret=true;
        break;
      }
    }
    return ret;
  }

  public static void unloadRoles(){
    MANAGER_ROLE=null;
    MYSELF_ROLE=null;
  }
}

