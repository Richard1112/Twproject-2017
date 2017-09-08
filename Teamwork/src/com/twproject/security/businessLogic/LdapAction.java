package com.twproject.security.businessLogic;

import com.opnlb.website.waf.WebSiteConstants;
import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Person;
import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.anagraphicalData.AnagraphicalData;
import org.jblooming.ldap.LdapUtilities;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.operator.Operator;
import org.jblooming.page.ListPage;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.security.Area;
import org.jblooming.security.LdapUser;
import org.jblooming.security.Role;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.waf.ActionSupport;
import org.jblooming.waf.constants.OperatorConstants;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.display.Paginator;
import org.jblooming.waf.html.input.Collector;
import org.jblooming.waf.html.input.Selector;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.state.PersistentSearch;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.RestState;

import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.directory.DirContext;
import javax.naming.directory.SearchControls;
import javax.naming.directory.SearchResult;
import java.util.*;


public class LdapAction extends ActionSupport {
  public TeamworkOperator logged;

  public LdapAction(PageState pageState)  {
    super(pageState);
    this.logged = (TeamworkOperator) pageState.getLoggedOperator();

  }

  public void cmdFind() throws PersistenceException {

    if (restState.getCommand() == null)
      PersistentSearch.feedFromDefaultSearch("LDAPFILTER", restState);

    boolean recoveredFromSavedFilter = PersistentSearch.feedFromSavedSearch(restState);
    try {
      makeLdapGroups();
    } catch (Throwable e) {
      restState.getEntry("ldapfilter").errorCode=e.getMessage();
    }

  }


  public void cmdImport() throws PersistenceException, ActionException {
    restState.initializeEntries("table");

    Set<String> chosenUsers = Collector.chosen("ldapUsers", restState).keySet();

    String filterUsers = "";

    HashMap<String, String> ldapSettings  = (HashMap<String, String>) ApplicationState.applicationParameters.get("LDAP");
    String userPrincipalName = ldapSettings.get(LdapUtilities.USER_PRINCIPAL_NAME.toUpperCase());
    if(userPrincipalName == null)
       userPrincipalName =  LdapUtilities.USER_PRINCIPAL_NAME;
    for (String user : chosenUsers)
      filterUsers += "("+userPrincipalName+"=" + user + ")";
       //filterUsers += "(uid=" + user + ")";
    if (filterUsers.length() > 0)
      filterUsers = "(|" + filterUsers + ")";
    String basedn = restState.getEntry(LdapUtilities.BASE_DN).stringValueNullIfEmpty();
    Vector<LdapUser> users = LdapUtilities.getUsers(null,filterUsers, basedn, getContext(restState));


    List<LdapUser> list = new ArrayList();
    boolean update_users = restState.getEntry("UPDATE_LDAPUSERS").checkFieldValue();
    boolean update_addPropperties = restState.getEntry("ADDITIONAL_PROPERTIES").checkFieldValue();
    String defaultPwd = restState.getEntry("DEFAULT_PWD").stringValueNullIfEmpty();
    //Check Operator
    int addUsers = 0;
    int updateUsers = 0;
    Map<String, List<String>> rolesMap = Collector.selectedCheckBoxes("ldapUsers", restState);
    for (LdapUser user : users) {
      // Update AnagraphicalData
      TeamworkOperator operator = null;
      try {
        operator = (TeamworkOperator) ReflectionUtilities.getUnderlyingObject(Operator.findByLoginName(user.getUsername().toLowerCase()));
      } catch (PersistenceException e) {
      }

      if (operator != null) {
        if (update_users) {
          Person resource = operator.getPerson();
          //authentication to LDAP
          operator.setAuthentication("LDAP");

          //update name and surname
          resource.setPersonName(user.getName());
          resource.setPersonSurname(user.getSurname());

          Set<AnagraphicalData> ads = resource.getAnagraphicalDatas();


          updateUsers++;
          if (ads.size() == 1) {
            AnagraphicalData anag = ads.iterator().next();
            if (update_addPropperties)
              LdapUtilities.makeAD(user, anag);
            resource.store();
          }
        }
      } else {//Make Resource

        List<String> idRoles = rolesMap.get(user.getUserPrincipalName());
        Area a = (Area) PersistenceHome.findByPrimaryKey(Area.class, restState.getEntryAndSetRequired("AREA").stringValue());
        createTeamworkPerson(update_addPropperties, defaultPwd, user, idRoles, a);

        addUsers++;

      }
      list.add(user);


    }
    restState.addClientEntry("addUsers", addUsers);
    restState.addClientEntry("updateUsers", updateUsers);
    ListPage lp = new ListPage(list, Paginator.getWantedPageNumber(restState), Paginator.getWantedPageSize(restState));
    restState.setPage(lp);

  }

  public static Person createTeamworkPerson(boolean updateAdditionalProps, String defaultPwd, LdapUser user, List<String> idRoles, Area area) throws StoreException, FindByPrimaryKeyException {

    TeamworkOperator newOperator = new TeamworkOperator();
    newOperator.setName(JSP.ex(user.getName()) ? user.getName() : "-");
    newOperator.setSurname(JSP.ex(user.getSurname()) ? user.getSurname() : "-");
    newOperator.setLoginName(user.getUsername());
    //authentication to LDAP
    newOperator.setAuthentication("LDAP");
    newOperator.putOption(OperatorConstants.MEDIA_PREFERRED_CHANNEL, MessagingSystem.Media.LOG + "");
    newOperator.putOption(WebSiteConstants.HOME_PAGE, "personFirstStart.jsp");

    if (defaultPwd != null)
      try {
        newOperator.changePassword( defaultPwd);
      } catch (ApplicationException e) {
        throw new PlatformRuntimeException(e);
      }
    else
      try {
        newOperator.changePassword( user.getUsername());
      } catch (ApplicationException e) {
        throw new PlatformRuntimeException(e);
      }

    if (idRoles != null) {
      for (String idRole : idRoles) {
        //check Administration role
        if (idRole.equals("administrator"))
          newOperator.setAdministrator(true);
        else
          newOperator.addRoleAndPersist((Role) PersistenceHome.findByPrimaryKey(Role.class, idRole));
      }
    }
    newOperator.store();

    AnagraphicalData anag = new AnagraphicalData();
    anag.setIdAsNew();
    anag.setOrderFactor(1);
    if (updateAdditionalProps)
      LdapUtilities.makeAD(user, anag);
    anag.setLocationDescription("-");
    anag.store();

    Person resource = new Person();
    resource.setIdAsNew();
    resource.setPersonName(user.getName());
    resource.setPersonSurname(user.getSurname());
    resource.setMyself(newOperator);
    resource.getAnagraphicalDatas().add(anag);
    resource.setArea(area);
    resource.setWorkDailyCapacity(CompanyCalendar.MILLIS_IN_WORKING_DAY);
    resource.store();

    return resource;
  }


  private static DirContext getContext(RestState pageState) {
    DirContext ctx = null;
    try {
      String provider_url = pageState.getEntry(LdapUtilities.PROVIDER_URL).stringValueNullIfEmpty();
      String security_auth = pageState.getEntry(LdapUtilities.SECURITY_AUTHENTICATION).stringValueNullIfEmpty();
      String security_principal = pageState.getEntry(LdapUtilities.SECURITY_PRINCIPAL).stringValueNullIfEmpty();
      String security_credential = pageState.getEntry(LdapUtilities.SECURITY_CREDENTIALS).stringValueNullIfEmpty();

      ctx = LdapUtilities.getContext(provider_url, security_auth, security_principal, security_credential);
    } catch (NamingException ne) {
      pageState.getEntry("ldapfilter").errorCode = ne.getMessage();
    }
    return ctx;
  }


  private static Vector<LdapUser> getUsers(RestState pageState, String addfilter, boolean show_disable_users) {
    Vector<LdapUser> users = null;
    try {
      DirContext ctx = getContext(pageState);
      if(ctx == null)
        return users;
      String basedn = pageState.getEntry(LdapUtilities.BASE_DN).stringValueNullIfEmpty();
      String pageFilter = pageState.getEntry("ldapfilter").stringValueNullIfEmpty();
      String filter = "";
      SearchControls sc = new SearchControls();
      sc.setSearchScope(SearchControls.SUBTREE_SCOPE);

      if (addfilter != null && addfilter.length() > 0)
        filter = "(&(|" + addfilter + ")(" + pageFilter + ")" + (show_disable_users ? LdapUtilities.getFilterForDisableUsers() : "") + ")";
      else if (!show_disable_users)
        filter = "(&(" + pageFilter + ")(" + LdapUtilities.getFilterForDisableUsers() + "))";
      else
        filter = pageFilter;
      if (filter != null) {
        NamingEnumeration<SearchResult> sr = ctx.search(basedn, filter, sc);
        users = LdapUtilities.getLdapUsers(sr);
      }
    } catch (NamingException ne) {
      Tracer.platformLogger.error(ne);
    } catch(Exception e){
      Tracer.platformLogger.error(e);
    }
    return users;
  }


  private void makeLdapGroups() throws  NamingException {
    //atts.get("memberof")
    //DirContext context = LdapUtilities.getDefaultContext();
    DirContext context = getContext(restState);
    if (context != null) {
      String filter = restState.getEntry("ldapfilter").stringValueNullIfEmpty();
      HashMap<String, String> groups = LdapUtilities.getGroups(ApplicationState.getApplicationSetting(LdapUtilities.BASE_DN), filter, context, -1);
      if(groups != null){
        TreeMap<String, String> ctm = new TreeMap<String, String>();
        TreeMap<String, String> candTm = new TreeMap<String, String>();
        Iterator iterator = groups.keySet().iterator();
        while (iterator.hasNext()) {
          String group = (String) iterator.next();
          String groupDesc = (String) groups.get(group);
          if (ctm.get(group) == null)
            candTm.put(group, JSP.limWr(groupDesc, 200));

        }
        Selector.make("ldapGroups", candTm, ctm, restState);
      }  
    }
  }

  private void makeLdapUsers(Vector<LdapUser> users)    {


    TreeMap<String, String> ctm = new TreeMap<String, String>();
    TreeMap<String, String> candTm = new TreeMap<String, String>();
    if (users != null)
      for (LdapUser user : users) {
        candTm.put(user.getUserPrincipalName() == null ? "" : user.getUserPrincipalName(), user.getDisplayUser());
      }


    Collector.make("ldapUsers", candTm, ctm, restState);
  }


}
