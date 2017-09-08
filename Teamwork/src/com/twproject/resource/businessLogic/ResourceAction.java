package com.twproject.resource.businessLogic;

import com.opnlb.website.content.Content;
import com.opnlb.website.waf.WebSiteConstants;
import com.twproject.document.TeamworkDocument;
import com.twproject.document.businessLogic.DocumentAction;
import com.twproject.messaging.stickyNote.StickyNote;
import com.twproject.operator.TeamworkOperator;
import com.twproject.rank.EntityGroupRank;
import com.twproject.rank.Hit;
import com.twproject.rank.RankUtilities;
import com.twproject.resource.Company;
import com.twproject.resource.Person;
import com.twproject.resource.Resource;
import com.twproject.resource.ResourceBricks;
import com.twproject.security.SecurityBricks;
import com.twproject.security.TeamworkArea;
import com.twproject.security.TeamworkPermissions;
import net.sf.json.JSONArray;
import net.sf.json.JSONObject;
import org.hibernate.Query;
import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.anagraphicalData.AnagraphicalData;
import org.jblooming.designer.DesignerField;
import org.jblooming.messaging.Listener;
import org.jblooming.messaging.Message;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.ontology.businessLogic.DeleteHelper;
import org.jblooming.operator.Operator;
import org.jblooming.operator.businessLogic.OptionAction;
import org.jblooming.oql.OqlQuery;
import org.jblooming.oql.QueryHelper;
import org.jblooming.page.HibernatePage;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.security.*;
import org.jblooming.security.SecurityException;
import org.jblooming.system.SystemConstants;
import org.jblooming.uidgen.CounterHome;
import org.jblooming.utilities.*;
import org.jblooming.waf.ActionSupport;
import org.jblooming.waf.ActionUtilities;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.constants.*;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.display.DataTable;
import org.jblooming.waf.html.display.Paginator;
import org.jblooming.waf.html.input.Selector;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.state.PersistentSearch;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.RestState;

import java.util.*;

public class ResourceAction extends ActionSupport {

  public TeamworkOperator logged;
  public Resource resource;


  public ResourceAction(RestState pageState) {
    super(pageState);
    this.logged = (TeamworkOperator) pageState.getLoggedOperator();
  }

  public void cmdPrepareDefaultFind() throws PersistenceException {
    //search for default filter

    if (restState.getCommand() == null)
      if (!PersistentSearch.feedFromDefaultSearch(Resource.RESOURCE, restState)) {
        // when not set use my department
        restState.addClientEntry(Fields.FLD_FILTER_NAME, "PF_RECENTLY_USED");
        //restState.addClientEntry(Fields.FLD_FILTER_NAME, "PF_RES_MY_DEPARTMENT");
        //restState.addClientEntry("NAME_SURNAME",logged.getDefaultEmail());
      }



    if (!PersistentSearch.feedFromSavedSearch(restState)) {
      String cmd = restState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty();
      if (JSP.ex(cmd)) {
        restState.getClientEntries().getClientEntries().clear();

        // add the filter name in order to display boldify
        restState.addClientEntry(Fields.FLD_FILTER_NAME, cmd);

        if ("PF_RES_MY_DEPARTMENT".equals(cmd)) {
          Person logPers = logged.getPerson();
          if (logPers.getParent() != null) {
            restState.addClientEntry("COMPANY", logPers.getParent());
          }
          restState.addClientEntry("RESOURCE_TYPE", "PERSON");
          restState.addClientEntry("BY_HIDDEN", "IS_NOT_HIDDEN");

        } else if ("PF_RES_TEAMWORK_USERS".equals(cmd)) {
          restState.addClientEntry("RESOURCE_TYPE", "PERSON");
          restState.addClientEntry("BY_LOGIN", "HAVING_LOGIN");
          restState.addClientEntry("BY_ENABLED", "IS_ENABLED");


        } else if ("PF_RES_PEOPLE".equals(cmd)) {
          restState.addClientEntry("RESOURCE_TYPE", "PERSON");

        } else if ("PF_RES_COMPANIES".equals(cmd)) {
          restState.addClientEntry("ROOT_OR_STANDALONE", true);
          restState.addClientEntry("RESOURCE_TYPE", "COMPANY");

        } else if ("PF_LAST_MODIFIED".equals(cmd)) {
          restState.addClientEntry("LAST_MODIFIED", ">-2w");

        } else if ("PF_RECENTLY_USED".equals(cmd)) {
          restState.addClientEntry("RECENTLY_USED", Fields.TRUE);
        }
      }
    }

  }

  public QueryHelper cmdFind() throws PersistenceException {
    cmdPrepareDefaultFind();

    boolean doQueryOnAnagData = true;
    ClientEntry entry = restState.getEntry("PHONE");
    doQueryOnAnagData = doQueryOnAnagData && entry.stringValueNullIfEmpty() == null;
    entry = restState.getEntry("ANAGRAPHICAL_DATA");
    doQueryOnAnagData = doQueryOnAnagData && entry.stringValueNullIfEmpty() == null;
    entry = restState.getEntry("NAME_SURNAME");
    doQueryOnAnagData = doQueryOnAnagData && entry.stringValueNullIfEmpty() == null;

    boolean isPerson = true;
    String defaultOrderBy = "resource.name";

    boolean somethingSearched = false;

    String hql = "select distinct resource from " + Resource.class.getName() + " as resource";
    if (!doQueryOnAnagData)
      hql += " left outer join resource.anagraphicalDatas as data";
    QueryHelper qhelp = new QueryHelper(hql);

    String filter = restState.getEntry("NAME_SURNAME").stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addQBEORClauses(
        filter,
        qhelp.getOrElement("resource.code", "rcode", QueryHelper.TYPE_CHAR),
        qhelp.getOrElement("resource.name", "name", QueryHelper.TYPE_CHAR),
        qhelp.getOrElement("resource.personName", "personName", QueryHelper.TYPE_CHAR),
        qhelp.getOrElement("resource.personSurname || ' ' || resource.personName", "surnameName", QueryHelper.TYPE_CHAR),
        qhelp.getOrElement("resource.personName || ' ' || resource.personSurname", "nameSurname", QueryHelper.TYPE_CHAR),
        qhelp.getOrElement("resource.personSurname", "personSurname", QueryHelper.TYPE_CHAR),
        qhelp.getOrElement("data.email", "email", QueryHelper.TYPE_CHAR));

      somethingSearched = true;
    }


    if (restState.getEntry("RECENTLY_USED").checkFieldValue()) {
      List<EntityGroupRank> ranks = RankUtilities.getEntityRankStatistically(logged.getIntId(), Company.class.getName(), new Date());
      ranks.addAll(RankUtilities.getEntityRankStatistically(logged.getIntId(), Person.class.getName(), new Date()));
      boolean something = JSP.ex(ranks);
      if (something) {
        List<String> ids = new ArrayList();
        for (int i = 0; i < ranks.size(); i++) {
          EntityGroupRank egr = ranks.get(i);
          ids.add(egr.id);
          if (i >= 19)
            break;
        }
        qhelp.addOQLInClause("resource.id", "resid", ids);
      }
    }


    String searchType = restState.getEntry("RESOURCE_TYPE").stringValueNullIfEmpty();
    isPerson=! "COMPANY".equalsIgnoreCase(searchType);
    if (JSP.ex(searchType)) {
      qhelp.addOQLClause("resource.class=:resClass", "resClass", searchType);
    }

    // search type specific filters
    //if ("PERSON".equals(searchType)) {
      filter = restState.getEntry("FLD_LOGIN_NAME").stringValueNullIfEmpty();
      if (filter != null ) {
        qhelp.addQBEClause("resource.myself.loginName", "loginName", filter, QueryHelper.TYPE_CHAR);
        somethingSearched = true;
      }

      String login = restState.getEntry("BY_LOGIN").stringValueNullIfEmpty();
      if (JSP.ex(login) ) {
        if ("HAVING_LOGIN".equals(login))
          qhelp.addOQLClause("resource.myself is not null");
        else
          qhelp.addOQLClause("resource.myself is null");
        somethingSearched = true;
      }

      String enabled = restState.getEntry("BY_ENABLED").stringValueNullIfEmpty();
      if (JSP.ex(enabled) ) {
        if ("IS_ENABLED".equals(enabled)) {
          qhelp.addOQLClause("resource.myself is not null and resource.myself.enabled = :truth");
          qhelp.addParameter("truth", Boolean.TRUE);
        } else if ("IS_DISABLED".equals(enabled)) {
          qhelp.addOQLClause("resource.myself is not null and resource.myself.enabled = :falsity");
          qhelp.addParameter("falsity", Boolean.FALSE);
        }
        somethingSearched = true;
      }

      String hidden = restState.getEntry("BY_HIDDEN").stringValueNullIfEmpty();
      if (JSP.ex(hidden) ) {
        if ("IS_HIDDEN".equals(hidden)) {
          qhelp.addOQLClause("resource.hidden = :hidden", "hidden", Boolean.TRUE);
        } else if ("IS_NOT_HIDDEN".equals(hidden)) {
          qhelp.addOQLClause("resource.hidden = :hidden", "hidden", Boolean.FALSE);
        }
      }

  //  } else if ("COMPANY".equals(searchType)) {
      filter = restState.getEntry("COMPANY_TYPE").stringValueNullIfEmpty();
      if (filter != null) {
        qhelp.addQBEClause("resource.type.id", "typeId", filter, QueryHelper.TYPE_INT);
      }
  //  }


    filter = restState.getEntry("JOB_DESCRIPTION").stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addQBEClause("resource.jobDescription", "jobDescription", filter, QueryHelper.TYPE_CHAR);
      somethingSearched = true;
    }


    filter = restState.getEntry("PHONE").stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addQBEORClauses(
        filter,
        qhelp.getOrElement("data.telephone", "telephone", QueryHelper.TYPE_CHAR),
        qhelp.getOrElement("data.mobile", "mobile", QueryHelper.TYPE_CHAR),
        qhelp.getOrElement("data.fax", "fax", QueryHelper.TYPE_CHAR)
      );
      somethingSearched = true;
    }
    somethingSearched = somethingSearched | ActionUtilities.addQBEClause("LAST_MODIFIED", "resource.lastModified", "lastModified", qhelp, QueryHelper.TYPE_DATE, restState);

    //search for custom field
    somethingSearched = somethingSearched | DesignerField.queryCustomFields("RESOURCE_CUSTOM_FIELD_", 6, "resource", qhelp, restState);

    filter = restState.getEntry("NOTES").stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addQBEClause("resource.notes", "notes", filter, QueryHelper.TYPE_CHAR);
    }

    filter = restState.getEntry("ANAGRAPHICAL_DATA").stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addQBEORClauses(
        filter,
        qhelp.getOrElement("data.address", "address", QueryHelper.TYPE_CHAR),
        qhelp.getOrElement("data.zip", "zip", QueryHelper.TYPE_CHAR),
        qhelp.getOrElement("data.locationDescription", "locationDescription", QueryHelper.TYPE_CHAR),
        qhelp.getOrElement("data.city", "city", QueryHelper.TYPE_CHAR),
        qhelp.getOrElement("data.province", "province", QueryHelper.TYPE_CHAR)
      );
      somethingSearched = true;
    }

    //somethingSearched = somethingSearched | ActionUtilities.addQBEClause("RESOURCE_TAGS", "resource.tags", "tags", qhelp, QueryHelper.TYPE_CHAR, restState);
    String tags=restState.getEntry("RESOURCE_TAGS").stringValueNullIfEmpty();
    if (JSP.ex(tags)){
      Set<String> tgs= StringUtilities.splitToSet(tags, ",");
      int i=1;
      for (String tag:tgs){
        somethingSearched=true;
        tag=tag.trim().toUpperCase();
        qhelp.addOQLClause("upper(resource.tags) like :tg1_"+i +" or upper(resource.tags) like :tg2_"+i+" or upper(resource.tags) like :tg3_"+i +" or upper(resource.tags)=:tg4_"+i);
        qhelp.addParameter("tg1_"+i,tag+", %"); //il tag all'inizio
        qhelp.addParameter("tg2_"+i,"%, "+tag+", %"); //il tag è nel mezzo
        qhelp.addParameter("tg3_"+i,"%, "+tag); //il tag è alla fine
        qhelp.addParameter("tg4_"+i,tag); //il tag è solo soletto
        i++;
      }
    }


    ClientEntry ce = restState.getEntry("COMPANY");
    if (ce.isFilled())
      try {
        String id = ce.stringValue();
        Company tc = (Company) PersistenceHome.findByPrimaryKey(Company.class, id);
        qhelp.addOQLClause("resource.ancestorIds like :children", "children", tc.getChildAncentorIds() + "%");
        somethingSearched = true;
      } catch (ActionException e) {
      }

    String id = restState.getEntry("ALL_STAFF_OF").stringValueNullIfEmpty();
    if (id != null && !"".equals(id)) {
      Resource parent = (Resource) PersistenceHome.findByPrimaryKey(Resource.class, id);
      String value = "";
      if (parent.getMyManagerIds() != null && !"".equals(parent.getMyManagerIds())) {
        value += parent.getMyManagerIds();
      }
      value += parent.getId() + "_";
      qhelp.addQBEClause("resource.myManagerIds", "myManagerIds", value, QueryHelper.TYPE_CHAR);
      somethingSearched = true;
    }

    if (Fields.TRUE.equalsIgnoreCase(restState.getEntry("ROOT_OR_STANDALONE").stringValueNullIfEmpty())) {
      qhelp.addOQLClause("resource.parent is null");
      somethingSearched = true;
    } else  if (Fields.FALSE.equalsIgnoreCase(restState.getEntry("ROOT_OR_STANDALONE").stringValueNullIfEmpty())) {
      qhelp.addOQLClause("resource.parent is not null");
      somethingSearched = true;


    }



    filter = restState.getEntry("AREA").stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addQBEClause("resource.area.id", "areaId", filter, QueryHelper.TYPE_INT);
    }


    if (!logged.hasPermissionAsAdmin())
      ResourceBricks.addSecurityClauses("resource", true, qhelp, CollectionUtilities.toList(TeamworkPermissions.resource_canRead), (TeamworkOperator) restState.getLoggedOperator(), isPerson, true);

    //per salvare il filtro nel db
    if (!somethingSearched) {
      somethingSearched = true;
    }

    if (somethingSearched) {
      DataTable.orderAction(qhelp, "RESLST", restState, defaultOrderBy);

      OqlQuery oqlQuery = qhelp.toHql();
      Query query = oqlQuery.getQuery();
      HibernatePage page = HibernatePage.getHibernatePageInstance(query, Paginator.getWantedPageNumber(restState), Paginator.getWantedPageSize("RESOURCELH", restState));

      restState.setPage(page);
    }

    return qhelp;
  }

  private void editNoMake() throws PersistenceException {
    Resource res = (Resource) PersistenceHome.findByPrimaryKey(Resource.class, restState.getMainObjectId());
    res = (Resource) ReflectionUtilities.getUnderlyingObject(res);
    restState.setMainObject(res);
    this.resource = res;
    Hit.getInstanceAndStore(resource, logged, .1);
  }


  public void cmdEdit() throws PersistenceException, SecurityException {
    editNoMake();
    if (!resource.hasPermissionFor(logged,TeamworkPermissions.resource_canRead))
      throw new SecurityException(SecurityConstants.I18N_PERMISSION_LACKING);
    make(resource);
  }


  public void cmdGuess() throws PersistenceException, SecurityException, ActionException {
    resource = null;
    //first eventually try code
    try {
      resource = (Resource) PersistenceHome.findUnique(Resource.class, "code", restState.mainObjectId);
    } catch (PersistenceException p) {
      throw new ActionException("REF_NOT_UNIQUE");
    }

    if (resource == null)
      resource = Resource.load(restState.getMainObjectId());

    if (resource != null) {
      restState.mainObjectId = resource.getId();
      editNoMake();
      if (!resource.hasPermissionFor(logged,TeamworkPermissions.resource_canRead))
        throw new ActionException("REF_PERMISSION_LACKING");
      make(resource);
    } else {
      throw new ActionException("REF_NOT_FOUND");
    }

  }


  private void make(Resource resource) throws PersistenceException {

    String anagrId = restState.getEntry("SELECT_ANAGRAPHICAL_ID").stringValueNullIfEmpty();
    restState.setMainObject(resource);
    restState.addClientEntry("SELECT_ANAGRAPHICAL_ID", anagrId);

    if (resource.getParent() != null) {
      restState.addClientEntry(Fields.PARENT_ID, resource.getParent().getId());
    }
    restState.addClientEntry("CODE", resource.getCode());
    if (resource.getArea() != null) {
      restState.addClientEntry("AREA", resource.getArea().getId());
    }
    restState.addClientEntry("USER_HIDDEN", resource.isHidden());

    restState.addClientEntry("BOSS", (resource.getMyManager() != null ? resource.getMyManager().getId() : ""));
    //pageState.addClientEntry("IS_STAFF", (resource.isStaff() ? Fields.TRUE : Fields.FALSE));
    restState.addClientEntry("COST_CENTER", resource.getMyCostAggregator());
    restState.addClientEntry("RESOURCE_TAGS", resource.getTags());
    restState.addClientEntry("JOB_DESCRIPTION", resource.getJobDescription());
    restState.addClientEntry("NOTE", resource.getNotes());
    restState.addClientEntryCurrency("HOURLY_COST", resource.getHourlyCost());
    restState.addClientEntryTime("WORK_DAILY_CAPACITY", resource.getWorkDailyCapacity());

    if (resource instanceof Company) {
      restState.addClientEntry("NAME", resource.getName());
      //* added by schelazzi
      restState.addClientEntry("TYPE", ((Company) resource).getType());
    } else {
      Person person = ((Person) resource);
      restState.addClientEntry("COURTESY_TITLE", person.getCourtesyTitle());
      restState.addClientEntry(OperatorConstants.FLD_NAME, person.getPersonName());
      restState.addClientEntry(OperatorConstants.FLD_SURNAME, person.getPersonSurname());
      //pageState.addClientEntry("PERSONAL_INTEREST", person.getPersonalInterest());
      restState.addClientEntry("HIRING_DATE", DateUtilities.dateToString(person.getHiringDate()));

      //as operator:
      TeamworkOperator myself = ((Person) resource).getMyself();

      if (myself != null) {
        restState.addClientEntry(OperatorConstants.FLD_ADMINISTRATOR, myself.hasPermissionAsAdmin() ? Fields.TRUE : Fields.FALSE);
        makeRoles(myself, resource);
        restState.addClientEntry("LOGIN_NAME", myself.getLoginName());

        if (!myself.isNew()) {
          restState.addClientEntry(OperatorConstants.FLD_IS_ENABLED, myself.isEnabled() ? Fields.TRUE : Fields.FALSE);
          restState.addClientEntry("PWD", OperatorConstants.PASSWORD_MASK);
          restState.addClientEntry("PWD_RETYPE", OperatorConstants.PASSWORD_MASK);
        } else
          restState.addClientEntry(OperatorConstants.FLD_IS_ENABLED, Fields.TRUE);

      } else {
        restState.addClientEntry(OperatorConstants.FLD_IS_ENABLED, Fields.TRUE);
        TreeMap<String, String> ctm = new TreeMap<String, String>();
        TreeMap<String, String> candTm = new TreeMap<String, String>();

        Role operationalRole = getOperationalRole(resource);
        if (operationalRole != null)
          ctm.put(operationalRole.getId().toString(), operationalRole.getDisplayName());

        Set<Role> cand = SecurityBricks.getRolesFor(TeamworkPermissions.resource_canWrite, false, restState);
        if (cand != null && cand.size() > 0) {
          for (Role role : cand) {
            if (!ctm.containsKey(role.getId().toString()))
              candTm.put(role.getId().toString(), role.getDisplayName());
          }
        }
        Selector.make("roles", candTm, ctm, restState);
      }

    }
  }

  private void makeRoles(TeamworkOperator myself, Resource resource) throws PersistenceException {

    Iterator<OperatorRole> chosen = myself.getOperatorRolesIterator();
    TreeMap<String, String> ctm = new TreeMap<String, String>();
    Role operationalRole = getOperationalRole(resource);
    boolean isOperationalInChosen = false;
    while (chosen.hasNext()) {
      Role role = chosen.next().getRole();
      if (role != null) {
        if (role.equals(operationalRole))
          isOperationalInChosen = true;
        ctm.put(role.getId().toString(), role.getDisplayName());
      }
    }

    TreeMap<String, String> candTm = new TreeMap<String, String>();
    //noinspection unchecked
    if (resource.hasPermissionFor(logged,TeamworkPermissions.resource_canWrite)) { // todo da cotrollare

      Set<Role> cand = SecurityBricks.getRolesFor(TeamworkPermissions.resource_canWrite, false, restState);

      if (cand != null && cand.size() > 0) {
        for (Role role : cand) {
          if (chosen == null || !ctm.keySet().contains(role.getId().toString())) {
            candTm.put(role.getId().toString(), role.getDisplayName());
          }
        }
      }
      if (operationalRole != null && !isOperationalInChosen)
        candTm.put(operationalRole.getId().toString(), operationalRole.getDisplayName());

    }
    Selector.make("roles", candTm, ctm, restState);
  }

  public void cmdAdd() throws ActionException, PersistenceException, SecurityException {

    String type = restState.getEntry("RESOURCE_TYPE").stringValue();
    Resource resource;
    try {
      resource = (Resource) Class.forName(type).newInstance();
    } catch (Exception e) {
      throw new PlatformRuntimeException(e);
    }
    resource.setIdAsNew();
    if (restState.getMainObjectId() != null) {
      Resource parent = (Resource) PersistenceHome.findByPrimaryKey(Resource.class, restState.getMainObjectId());
      resource.setParentNode(parent);
      restState.addClientEntry(Fields.PARENT_ID, parent.getId());

      //determine default area
      resource.setArea(parent.getArea());

    } else
      resource.setArea(logged.getDefaultAreaForPermission(TeamworkPermissions.resource_canCreate));

    restState.addClientEntry("AREA", resource.getArea());
    //test permissions must be done after having determined parent
    resource.testPermission(logged, TeamworkPermissions.resource_canCreate);

    resource.setOwner(logged);

    restState.addClientEntry(new ClientEntry(OperatorConstants.FLD_IS_ENABLED, Fields.TRUE));
    AnagraphicalData data = new AnagraphicalData();
    data.setIdAsNew();
    restState.addClientEntry("ANAGRAPHICAL_ID", data.getId().toString());
    restState.setMainObject(resource);

    //set time

    //set workDailyCapacity and workStartTime  to Resource in general
    long defaultWorkingHourPerDay = CompanyCalendar.MILLIS_IN_WORKING_DAY;

    resource.setWorkDailyCapacity(defaultWorkingHourPerDay);
    restState.addClientEntryTime("WORK_DAILY_CAPACITY", resource.getWorkDailyCapacity());

    restState.addClientEntryCurrency("HOURLY_COST", ResourceBricks.getDefaultHourlyCost());

    TreeMap<String, String> ctm = new TreeMap<String, String>();
    TreeMap<String, String> candTm = new TreeMap<String, String>();

    Role operationalRole = getOperationalRole(resource);
    if (operationalRole != null)
      ctm.put(operationalRole.getId().toString(), operationalRole.getDisplayName());

    Set<Role> cand = SecurityBricks.getRolesFor(TeamworkPermissions.resource_canWrite, false, restState);
    if (cand != null && cand.size() > 0) {
      for (Role role : cand) {
        if (!ctm.containsKey(role.getId().toString()))
          candTm.put(role.getId().toString(), role.getDisplayName());
      }
    }
    Selector.make("roles", candTm, ctm, restState);

  }


  public Role getOperationalRole(Resource resource) {
    TeamworkArea area = (TeamworkArea) ReflectionUtilities.getUnderlyingObject(resource.getArea());
    if (area != null) {
      return area.getOperationalRole();
    } else
      return null;
  }

  public void cmdDelete() throws org.jblooming.security.SecurityException, PersistenceException {
    editNoMake();
    resource.testPermission(logged, TeamworkPermissions.resource_canDelete);
    restState.setMainObject(resource);

    if (resource.getMyself() != null) {

      // remove listener
      Query query = new OqlQuery("delete from " + Listener.class.getName() + " as lis where lis.owner=:myp").getQuery();
      query.setEntity("myp", resource.getMyself());
      query.executeUpdate();

      //remove messages
      query = new OqlQuery("delete from " + Message.class.getName() + " as lis where lis.toOperator=:myp").getQuery();
      query.setEntity("myp", resource.getMyself());
      query.executeUpdate();

      //remove sticky
      query = new OqlQuery("delete from " + StickyNote.class.getName() + " as lis where lis.receiver=:myp or lis.author=:myp").getQuery();
      query.setEntity("myp", resource);
      query.executeUpdate();

      //remove page customizations
      String hql = "delete from " + Content.class.getName() + " where operator=:ope";
      OqlQuery oql = new OqlQuery(hql);
      oql.getQuery().setEntity("ope", resource.getMyself());
      oql.getQuery().executeUpdate();

    }
    DeleteHelper.cmdDelete(resource, restState);
  }

  public void cmdSave() throws ActionException, PersistenceException, ApplicationException, SecurityException {

    Resource resource;
    // ------------ Crea l'oggetto
    String type = restState.getEntry("RESOURCE_TYPE").stringValueNullIfEmpty();
    boolean isNew = PersistenceHome.NEW_EMPTY_ID.equals(restState.mainObjectId);
    try {
      if (isNew) {

        resource = (Resource) Class.forName(type).newInstance();
        resource.setIdAsNew();
        resource.setOwner(logged);

      } else {
        resource = (Resource) PersistenceHome.findByPrimaryKey(((Resource) Class.forName(type).newInstance()).getClass(), restState.getMainObjectId());
      }

    } catch (Exception e) {
      throw new PlatformRuntimeException(e);
    }

    restState.setMainObject(resource);

    // ------------ Assegna l'eventuale parent
    Resource oldParent = resource.getParent();
    ActionUtilities.setIdentifiable(restState.getEntry(Fields.PARENT_ID), resource, "parent");

    ActionUtilities.setIdentifiable(restState.getEntryAndSetRequired("AREA"), resource, "area");

    // ------------ test permissions
    //if (!resource.hasPermissionFor(logged,TeamworkPermissions.resource_canWrite) && !resource.equals(logged.getPerson()))
    if (!resource.hasPermissionFor(logged,TeamworkPermissions.resource_canWrite))
      throw new SecurityException(SecurityConstants.I18N_PERMISSION_LACKING);

    // ------------ informazioni generali
    Person person = null;
    if (resource instanceof Person) {
      person = ((Person) resource);
      ActionUtilities.setString(restState.getEntry(OperatorConstants.FLD_NAME), person, "personName");
      ActionUtilities.setString(restState.getEntryAndSetRequired(OperatorConstants.FLD_SURNAME), person, "personSurname");
      ActionUtilities.setString(restState.getEntry("COURTESY_TITLE"), person, "courtesyTitle");
      ActionUtilities.setDate(restState.getEntry("HIRING_DATE"), person, "hiringDate");
      //ActionUtilities.setString(pageState.getEntry("PERSONAL_INTEREST"), person, "personalInterest");

    } else {
      ActionUtilities.setString(restState.getEntryAndSetRequired("NAME"), resource, "name");

      ActionUtilities.setIdentifiable(restState.getEntry("TYPE"), resource, "type");
//      int typeId = pageState.getEntry("TYPE").intValueNoErrorCodeNoExc();
//      if (typeId > 0) {
//        DepartmentType dt = (DepartmentType) PersistenceHome.findByPrimaryKey(DepartmentType.class, typeId);
//        ((Company) resource).setType(dt);
//      }

    }
    ActionUtilities.setString(restState.getEntry("CODE"), resource, "code");

    // test code uniqueness
    if (Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("USEUNIQUECODES")) && JSP.ex(resource.getCode()) && !resource.isUnique("code")) {
      restState.getEntry("CODE").errorCode = I18n.get("KEY_MUST_BE_UNIQUE");
    }


    if (JSP.ex(restState.getEntry("HOURLY_COST").stringValueNullIfEmpty()))
      ActionUtilities.setCurrency(restState.getEntry("HOURLY_COST"), resource, "hourlyCost");

    ActionUtilities.setTime(restState.getEntry("WORK_DAILY_CAPACITY"), resource, "workDailyCapacity");

    // ------------ informazioni personali
    ActionUtilities.setBoolean(restState.getEntry("IS_STAFF"), resource, "staff");
    ActionUtilities.setIdentifiable(restState.getEntry("COST_CENTER"), resource, "myCostAggregator");
    ActionUtilities.setString(restState.getEntry("JOB_DESCRIPTION"), resource, "jobDescription");
    ActionUtilities.setString(restState.getEntry("NOTE"), resource, "notes");

    //set tags: si ripuliscono, si rendono univoci e si riseparano con ", " occhio allo spazio!
    resource.setTags(StringUtilities.setToString(StringUtilities.splitToOrderSet(JSP.w(restState.getEntry("RESOURCE_TAGS").stringValue()), ","),", ")  );

    DesignerField.saveCustomFields("RESOURCE_CUSTOM_FIELD_", 6, resource, restState);

    // -----------------  dati anagrafici
    String ads = restState.getEntry("ads").stringValueNullIfEmpty();
    if (JSP.ex(ads)) {
      JSONArray jsa = JSONArray.fromObject(ads);

      for (Object o : jsa) {
        JSONObject jad = (JSONObject) o;

        AnagraphicalData ad = null;
        if (!jad.getString("id").startsWith("new"))
          ad = AnagraphicalData.load(jad.getString("id"));
        if (ad == null) {
          ad = new AnagraphicalData();
          ad.setIdAsNew();
          resource.getAnagraphicalDatas().add(ad);
        }

        //check if ad the right one for the selected resource
        if (resource.getAnagraphicalDatas().contains(ad)) {
          ad.setOrderFactor(jad.getInt("order"));
          ad.setLocationDescription(jad.getString("location"));

          ad.setAddress(jad.getString("address"));
          ad.setCity(jad.getString("city"));
          ad.setProvince(jad.getString("province"));
          ad.setZip(jad.getString("zip"));
          ad.setCountry(jad.getString("country"));
          ad.setTelephone(jad.getString("telephone"));
          ad.setFax(jad.getString("fax"));
          ad.setMobile(jad.getString("mobile"));
          ad.setEmail(jad.getString("email"));
          ad.setUrl(jad.getString("url"));

          ad.store();
        }
      }
    }


    // -----------------  set manager
    ClientEntry bossCE = restState.getEntry("BOSS");
    Person boss = Person.load(bossCE.stringValueNullIfEmpty() + "");
    try {
      resource.setMyManagerAndPerfIds(boss);
    } catch (ActionException ae) {
      bossCE.errorCode = ae.getMessage();
      throw ae;
    }


    if (restState.validEntries()) {
      try {
        resource.setParentAndStore(resource.getParent());

      } catch (Throwable t) {
        restState.getEntry(Fields.PARENT_ID).errorCode = t.getMessage();
        resource.setParentNode(oldParent);
        //throw new PersistenceException(t.getMessage());
      }

    }
    if (restState.validEntries()) {
      resource.store();

      //hit criteria
      boolean isRoot = resource.getParent() == null;
      Hit.getInstanceAndStore(resource, logged, .5 + (isRoot ? .3 : 0) + (isNew ? .2 : 0) + (resource.getMyself() != null ? .2 : 0));


      // ok message feedback
      String LBL = (resource instanceof Person ? "PERSON" : "COMPANY");
      if (isNew)
        restState.addMessageOK(I18n.get(LBL + "_CORRECTLY_CREATED"));
      else
        restState.addMessageOK(I18n.get(LBL + "_CORRECTLY_SAVED"));

    }
  }

  public void cmdSaveSecurity() throws PersistenceException, SecurityException, ActionException, ApplicationException {

    Person person = Person.load(restState.getMainObjectId());
    if (person != null) {
      restState.setMainObject(person);

      // ------------ test permissions  per salvare la propria password
      if (!person.hasPermissionFor(logged,TeamworkPermissions.resource_canWrite) && !person.equals(logged.getPerson()))
        throw new SecurityException(SecurityConstants.I18N_PERMISSION_LACKING);


      TeamworkOperator operator = person.getMyself();
      if (operator == null) {
        operator = new TeamworkOperator();
        operator.putOption(WebSiteConstants.HOME_PAGE, "personFirstStart.jsp");
        operator.putOption(OperatorConstants.MEDIA_PREFERRED_CHANNEL, MessagingSystem.Media.LOG +"");
        operator.setLoginName("");
      }


      ClientEntry ceLN = restState.getEntryAndSetRequired("LOGIN_NAME");
      String loginName = ceLN.stringValueNullIfEmpty();

      boolean loginNameChanged = !loginName.equals(operator.getLoginName());
      if (loginNameChanged) {
        OqlQuery oq = new OqlQuery("from " + Operator.class.getName() + " user where user.loginName = :userLN");
        oq.getQuery().setString("userLN", loginName);
        List sameLogin = oq.list();
        if (sameLogin != null && sameLogin.size() > 0) {
          ceLN.errorCode = FieldErrorConstants.ERR_KEY_MUST_BE_UNIQUE;
        } else
          operator.setLoginName(loginName);
      }


      ClientEntry pswCe = restState.getEntry("PWD");
      ClientEntry pswCeRT = restState.getEntry("PWD_RETYPE");
      String psw = pswCe.stringValue();
      if (!psw.equals(OperatorConstants.PASSWORD_MASK)) {

        // test for password sufficient length
        int minLength =  new ClientEntry("aa", ApplicationState.getApplicationSetting(SystemConstants.FLD_PASSWORD_MIN_LEN)).intValueNoErrorCodeNoExc();
        if (psw.length() < minLength) {
          pswCe.errorCode = "ERR_PASSWORD_TOO_SHORT";
          throw new ActionException();
        }

        // test for retyped password identical
        if (!psw.equals(pswCeRT.stringValue())) {
          pswCe.errorCode = "ERR_PASSWORD_MUST_BE_IDENTICAL";
          pswCeRT.setValue("");
          throw new ActionException();
        }
        operator.changePassword(psw);
      }

      //copy name and surname fron resource
      operator.setName(person.getPersonName());
      operator.setSurname(person.getPersonSurname());


      ActionUtilities.setBoolean(restState.getEntry("USER_HIDDEN"), person, "hidden");

      //unfortunately must call only if can write
      if (person.hasPermissionFor(logged,TeamworkPermissions.resource_canWrite)) {
        ActionUtilities.setBoolean(restState.getEntry(OperatorConstants.FLD_IS_ENABLED), operator, "enabled");

        if (logged.hasPermissionAsAdmin())
          ActionUtilities.setBoolean(restState.getEntry(OperatorConstants.FLD_ADMINISTRATOR), operator, "administrator");
      }

      //in order to show the selected checkboxes in disabled form
      if (!person.hasPermissionFor(logged, PlatformPermissions.area_canManage))
        makeRoles(operator, person);


      if (restState.validEntries()) {
        person.setMyself(operator);
        operator.store();

        if (person.hasPermissionFor(logged, PlatformPermissions.area_canManage)) {
          //remove unsel operator roles
          Set<String> selIds = Selector.chosen("roles", restState).keySet();
          for (OperatorRole oprole : operator.getOperatorRoles()) {
            if (!selIds.contains(oprole.getRole().getId().toString()))
              oprole.remove();
          }
          for (String roleId : selIds) {
            operator.addRoleAndPersist((Role) PersistenceHome.findByPrimaryKey(Role.class, roleId));
          }
          //remove unsel operator groups
          Set<String> selGrpIds = Selector.chosen("direct_groups", restState).keySet();
          Iterator<OperatorGroup> i = operator.getOperatorGroupsIterator();
          while (i.hasNext()) {
            OperatorGroup or = i.next();
            if (!selGrpIds.contains(or.getGroup().getId().toString()))
              or.remove();
          }
          for (String grpId : selGrpIds) {
            operator.addGroupAndPersist((Group) PersistenceHome.findByPrimaryKey(Group.class, grpId));
          }
        }
        person.store();

        //hit criteria
        boolean isRoot = person.getParent() == null;
        Hit.getInstanceAndStore(person, logged, .5 + (isRoot ? .3 : 0) + (person.getMyself() != null ? .2 : 0));


        // ok message feedback
        restState.addMessageOK(I18n.get("PERSON_CORRECTLY_SAVED"));

      }
    }
  }


  public void cmdPerformDocumentAction() throws Exception {
    editNoMake();

    String docId = restState.getEntry("DOC_ID").stringValueNullIfEmpty();
    restState.setMainObjectId(docId);

    DocumentAction da = new DocumentAction(restState);

    Exception ex = null;
    try {
      String command = restState.getCommand();
      if ("ADD_DOCUMENT".equals(command)) {
        resource.testPermission(logged, TeamworkPermissions.document_canCreate);
        da.cmdAdd();
        //must have referral for security
        ((TeamworkDocument) restState.getMainObject()).setResource(resource);

      } else if ("ADD_VERSION".equals(command)) {
        resource.testPermission(logged, TeamworkPermissions.document_canCreate);
        da.cmdAddVersion();
        //must have referral for security
        ((TeamworkDocument) restState.getMainObject()).setResource(resource);

      } else if ("EDIT_DOCUMENT".equals(command)) {
        resource.testPermission(logged, TeamworkPermissions.document_canRead);
        da.cmdEdit();
      } else if ("DOCUMENT_DELETE_PREVIEW".equals(command)) {
        resource.testPermission(logged, TeamworkPermissions.document_canDelete);
        da.cmdEdit();

      } else if ("SAVE_DOCUMENT".equals(command)) {
        resource.testPermission(logged, TeamworkPermissions.document_canWrite);
        da.cmdSave();
        TeamworkDocument document = (TeamworkDocument) restState.getMainObject();
        restState.setAttribute("DOCUMENT", document);

        if (restState.validEntries()) {
          document.setResource(resource);
          document.store();
        }
      } else if (Commands.DELETE.equals(command)) {
        resource.testPermission(logged, TeamworkPermissions.document_canCreate);
        da.cmdDelete();
      }
    } catch (Exception e) {
      ex = e;
    }

    restState.setAttribute("DOCUMENT", restState.getMainObject());

    restState.setMainObjectId(resource.getId());
    cmdEdit();

    if (ex != null)
      throw ex;

    if (!restState.validEntries())
      throw new ActionException();
  }


  public void cmdEditOptions() throws SecurityException, PersistenceException {
    cmdEdit();
    Person person = (Person) restState.getMainObject();
    TeamworkOperator to = person.getMyself();
    OptionAction.make(to, restState);
  }

  public void saveOptions() throws SecurityException, PersistenceException, ActionException, ApplicationException {

    cmdEdit();
    Person person = (Person) restState.getMainObject();


    // ------------ test permissions
    if (!resource.hasPermissionFor(logged,TeamworkPermissions.resource_canWrite) && !person.equals(logged.getPerson())) //
      throw new SecurityException(SecurityConstants.I18N_PERMISSION_LACKING);


    TeamworkOperator to = person.getMyself();
    OptionAction.saveOptions(to, restState);

    saveSubscriptions();

    String homePage = restState.getEntry(WebSiteConstants.HOME_PAGE).stringValueNullIfEmpty();
    if (JSP.ex(homePage))
      to.putOption(WebSiteConstants.HOME_PAGE, homePage);

    to.putOption(OperatorConstants.NOTIFY_MY_EVENTS_TO_MYSELF, restState.getEntry(OperatorConstants.NOTIFY_MY_EVENTS_TO_MYSELF).checkFieldHtmlValue());

    to.store();

    TeamworkOperator logged = (TeamworkOperator) restState.getLoggedOperator();

    if (logged.equals(to)) {
      //se ho la sessione la devo aggiornare.
      SessionState ss = SessionState.getCurrentSessionState();
      if (ss != null) {
        //ss.setLocale(I18n.getLocale(logged.getOption(FLD_SELECT_LANG)));
        ss.resetLocaleAndTimeZone();

        //SessionState.getLocale();
        String value = restState.getEntry(OperatorConstants.FLD_CURRENT_SKIN).stringValueNullIfEmpty();
        if (value != null) {
          ss.setSkinName(value);
        }
      }
    }


    Hit.getInstanceAndStore(person, logged, .2);
  }

  public void saveSubscriptions() throws SecurityException, PersistenceException {

    cmdEdit();
    Person person = (Person) restState.getMainObject();
    TeamworkOperator to = person.getMyself();
    String value = "";
    for (MessagingSystem.Media media : MessagingSystem.activeMedia) {
      String subscrField = OperatorConstants.MEDIA_PREFERRED_CHANNEL + "_" + media.toString().toUpperCase();
      String thisMedia = restState.getEntry(subscrField).stringValueNullIfEmpty();
      if (Fields.TRUE.equals(thisMedia))
        value = value + (JSP.ex(value) ? "," : "") + media;
    }

    if (JSP.ex(value)) {
      to.putOption(OperatorConstants.MEDIA_PREFERRED_CHANNEL, value);
    } else
      to.getOptions().remove(OperatorConstants.MEDIA_PREFERRED_CHANNEL);

    to.store();
  }


  public void cmdSnapshot() throws SecurityException, PersistenceException {
    cmdEdit();
    addDocToResource(restState.getEntry("filename").stringValueNullIfEmpty(), resource.getDisplayName() + "_SNAPSHOT " + DateUtilities.dateAndHourToString(new Date()));
    resource.store();
    // ok message feedback
    restState.addMessageOK(I18n.get("SNAPSHOT_CORRECTLY_CREATED"));
  }

  private void addDocToResource(String fileLocation, String docName) throws StoreException {
    TeamworkDocument document = new TeamworkDocument();
    document.setIdAsNew();
    document.setArea(resource.getArea());
    document.setName(docName);
    document.setType(TeamworkDocument.IS_UPLOAD);
    if (JSP.ex(fileLocation)) {
      PersistentFile persistentFile = new PersistentFile(CounterHome.next(PersistentFile.PERSISTENTFILE_ID), fileLocation);
      persistentFile.setFileLocation(fileLocation);
      document.setFile(persistentFile);
      document.setResource(resource);
      document.store();

      resource.addDocument(document);
    }

  }


}