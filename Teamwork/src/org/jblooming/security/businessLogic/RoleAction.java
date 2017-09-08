package org.jblooming.security.businessLogic;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.ontology.Identifiable;
import org.jblooming.ontology.businessLogic.DeleteHelper;
import org.jblooming.operator.Operator;
import org.jblooming.oql.QueryHelper;
import org.jblooming.page.HibernatePage;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.security.*;
import org.jblooming.security.SecurityException;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.ActionUtilities;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.constants.RoleConstants;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.display.DataTable;
import org.jblooming.waf.html.display.Paginator;
import org.jblooming.waf.html.input.Collector;
import org.jblooming.waf.html.state.Form;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.state.PersistentSearch;
import org.jblooming.waf.view.ClientEntries;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageState;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Set;
import java.util.TreeMap;

public class RoleAction {

  boolean hasAreas;

  public void cmdDelete(PageState pageState) throws org.jblooming.security.SecurityException, PersistenceException {
    Operator user = pageState.getLoggedOperator();
    user.testPermission(PlatformPermissions.role_canCreate);
    Role delenda = (Role) PersistenceHome.findByPrimaryKey(Role.class, pageState.getMainObjectId());
    DeleteHelper.cmdDelete(delenda, pageState);

  }


  public void cmdSave(PageState pageState) throws org.jblooming.security.SecurityException, PersistenceException, ActionException {
    boolean invalidEntries = false;
    Operator user = pageState.getLoggedOperator();
    user.testPermission(PlatformPermissions.role_canWrite);

    Role role = null;

    ClientEntry rt = pageState.getEntry("ROLE_TYPE");
    rt.required = true;
    try {
      if (PersistenceHome.NEW_EMPTY_ID.equals(pageState.getMainObjectId())) {
        String roleClassName = rt.stringValueNullIfEmpty();
        role = (Role) Class.forName(roleClassName).newInstance();
        //role.setIdAsNewSer();
        role.setIdAsNew();
      } else {
        role = (Role) PersistenceHome.findByPrimaryKey(Role.class, pageState.getMainObjectId());
        if (role == null) throw new FindByPrimaryKeyException();
      }
      invalidEntries = !ActionUtilities.setString(pageState.getEntryAndSetRequired(RoleConstants.FLD_ROLE_NAME), role, "name");

      ActionUtilities.setString(pageState.getEntry(RoleConstants.FLD_ROLE_DESCRIPTION), role, "description");

      if (hasAreas) {

        Method setArea = ReflectionUtilities.getDeclaredInheritedMethods(ReflectionUtilities.getUnderlyingObject(role).getClass()).get("setArea");

        String areaIdS = pageState.getEntry("AREA").stringValueNullIfEmpty();
        if ("SYSTEM_ROLE".equals(areaIdS)) {
          setArea.invoke(role, null);
        } else {
          int areaId = 0;
          areaId = pageState.getEntry("AREA").intValueNoErrorCodeNoExc();
          Area area = null;
          if (areaId > 0)
            area = (Area) PersistenceHome.findByPrimaryKey(Area.class, areaId);
          if (area != null) {
            if (setArea != null)
              setArea.invoke(role, area);
            else {
              pageState.getEntry("AREA").errorCode = role.getClass().getName() + " " + I18n.get("OBJ_HAS_NO_AREA_ATTRIBUTE");
            }
          }
        }
      }

      TreeMap<String, String> chosen = Collector.chosen("permColl", pageState);
      role.setPermissionIds(StringUtilities.setToString(chosen.keySet(), "|"));

      pageState.setMainObject(role);
    } catch (Exception e) {
      invalidEntries = true;
      rt.errorCode = e.getMessage();
    }


    if (invalidEntries) {
      throw new ActionException();
    }
    role.store();

    //si svuota la cache dei permessi
    PermissionCache.emptyCache();

  }

  public void cmdFind(PageState pageState) throws PersistenceException {

    final Operator logged = pageState.getLoggedOperator();
    String hql = "from " + Role.class.getName() + " as role ";

    if (pageState.getEntry(Form.FLD_FORM_ORDER_BY + "ROLELH").stringValueNullIfEmpty() == null) {
      hql = hql + " order by role.name ";
    }
    QueryHelper qhelp = new QueryHelper(hql);

    boolean recoveredFromSavedFilter = PersistentSearch.feedFromSavedSearch(pageState);

    String FLD_des = pageState.getEntry("name").stringValueNullIfEmpty();
    if (FLD_des != null) {
      qhelp.addQBEClause("role.name", "name", FLD_des, QueryHelper.TYPE_CHAR);
    }

    DataTable.orderAction(qhelp, "ROLELH", pageState);
    pageState.setPage(HibernatePage.getHibernatePageInstance(qhelp.toHql().getQuery(), Paginator.getWantedPageNumber(pageState), Paginator.getWantedPageSize(pageState)));

  }

  public ClientEntries make(Role role, PageState pageState)  {

    ClientEntries ces = new ClientEntries();
    ces.addRequiredEntry(RoleConstants.FLD_ROLE_NAME, role.getName());
    ces.addEntry(RoleConstants.FLD_ROLE_DESCRIPTION, role.getDescription());
    Class<? extends Identifiable> aClass = ReflectionUtilities.getUnderlyingObject(role).getClass();
    ces.addRequiredEntry("ROLE_TYPE", aClass.getName());

    if (hasAreas) {
      Method getArea = ReflectionUtilities.getDeclaredInheritedMethods(aClass).get("getArea");
      if (getArea != null) {
        Area area = null;
        try {
          area = (Area) getArea.invoke(role);
          if (area != null)
            ces.addEntry("AREA", area.getId() + "");
          else
            ces.addEntry("AREA", "SYSTEM_ROLE");          
        } catch (IllegalAccessException e) {
          throw new PlatformRuntimeException(e);
        } catch (InvocationTargetException e) {
          throw new PlatformRuntimeException(e);
        }
      }
    }

    Set<Permission> chosen = role.getPermissions();

    TreeMap<String, String> ctm = new TreeMap<String, String>();
    if (chosen != null && chosen.size() > 0) {
      for (Permission p : chosen) {
        if (p != null)
          ctm.put(p.getName(), I18n.get(p.getName()));
      }
    }
    TreeMap<String, String> candTm = new TreeMap<String, String>();

    for (Permission perm:ApplicationState.getPermissions())
      if (chosen == null || !ctm.keySet().contains(perm.getName()))
        candTm.put(perm.getName(), I18n.get(perm.getName()));


    Collector.make("permColl", candTm, ctm, pageState);

    for (Permission p : ApplicationState.getPermissions()) {
      if (role.hasPermissionFor(p))
        ces.addEntry(new ClientEntry(Fields.FORM_PREFIX + p.getName(), "on"));
    }

    return ces;
  }

  public void cmdEdit(PageState pageState) throws PersistenceException, SecurityException {
    Operator user = pageState.getLoggedOperator();
    user.testPermission(PlatformPermissions.role_canWrite);
    Role role = (Role) PersistenceHome.findByPrimaryKey(Role.class, pageState.getMainObjectId());
    pageState.setMainObject(role);
    pageState.getClientEntries().addEntries(make(role, pageState));
  }

  public void cmdAdd(PageState pageState) throws PersistenceException {
    Role role = new Role();
    role.setIdAsNew();
    pageState.setMainObject(role);
    pageState.getClientEntries().addEntries(make(role, pageState));
  }

  public void cmdMove(String s, PageState pageState) throws FindByPrimaryKeyException {
    Collector.move("permColl", pageState);
    Role role;
    if (PersistenceHome.NEW_EMPTY_ID.equals(pageState.getMainObjectId())) {
      role = new Role();
      //role.setIdAsNewSer();
      role.setIdAsNew();
    } else {
      role = (Role) PersistenceHome.findByPrimaryKey(Role.class, pageState.getMainObjectId());
      if (role == null)
        throw new FindByPrimaryKeyException();
    }
    pageState.setMainObject(role);
  }


}

