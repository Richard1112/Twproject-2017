package com.twproject.resource;

import java.text.NumberFormat;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.TreeSet;

import org.jblooming.company.DepartmentType;
import org.jblooming.designer.DesignerField;
import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.ontology.PerformantNode;
import org.jblooming.operator.Operator;
import org.jblooming.oql.OqlQuery;
import org.jblooming.oql.QueryHelper;
import org.jblooming.page.Page;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.security.Area;
import org.jblooming.security.OperatorRole;
import org.jblooming.security.Permission;
import org.jblooming.utilities.CollectionUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.Bricks;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.html.button.ButtonSupport;
import org.jblooming.waf.html.display.Img;
import org.jblooming.waf.html.input.SmartCombo;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.RestState;

import com.twproject.messaging.stickyNote.StickyNote;
import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.businessLogic.ResourceAction;
import com.twproject.security.RoleTeamwork;
import com.twproject.security.TeamworkPermissions;
import com.twproject.task.Assignment;
import com.twproject.task.Issue;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class ResourceBricks extends Bricks {

  public Resource mainObject;
  public TeamworkOperator logged;

  public ResourceBricks(Resource r) {
    this.mainObject = r;
  }


  public static SmartCombo getPersonCombo(String fieldName, boolean havingEnabledLogin, String additionalHql, PageState pageState) throws PersistenceException {
    return getPersonCombo(fieldName, TeamworkPermissions.resource_canRead, havingEnabledLogin, additionalHql, pageState);
  }

  public static SmartCombo getPersonCombo(String fieldName, Permission permissionRequired, boolean havingEnabledLogin, String additionalHql, PageState pageState) throws PersistenceException {

    String hql = "select resource.id, resource.name, coalesce(resource.code,' ') from " + Person.class.getName() + " as resource ";

    String whereForId = "where resource.id = :" + SmartCombo.FILTER_PARAM_NAME;

    SmartCombo resources = new SmartCombo(fieldName, hql, null, whereForId);

    QueryHelper queryHelperForFiltering = new QueryHelper(hql);

    //inject security clause
    addSecurityClauses("resource", true, queryHelperForFiltering, CollectionUtilities.toList(permissionRequired), (TeamworkOperator) pageState.getLoggedOperator(), true, true);

    String baseFilter =
      "  (upper(resource.personName || ' ' || resource.personSurname) like :" + SmartCombo.FILTER_PARAM_NAME +
        " or upper(resource.personSurname || ' ' || resource.personName) like :" + SmartCombo.FILTER_PARAM_NAME +
        " or upper(resource.name) like :" + SmartCombo.FILTER_PARAM_NAME +
        " or upper(resource.code) like :" + SmartCombo.FILTER_PARAM_NAME +
        ") and resource.hidden = false ";

    if (havingEnabledLogin) {
      baseFilter = baseFilter + " and ( resource.myself != null and resource.myself.enabled = true )";
    }

    queryHelperForFiltering.addOQLClause(baseFilter);

    if (additionalHql != null && additionalHql.trim().length() > 0)
      queryHelperForFiltering.addOQLClause(additionalHql);


    queryHelperForFiltering.addToHqlString(" order by resource.name");

    resources.queryHelperForFiltering = queryHelperForFiltering;
    resources.searchAll = true;
    resources.separator = "</td><td>";
    resources.fieldSize = 40;
    resources.convertToUpper = true;
    return resources;
  }

  public static SmartCombo getCompanyCombo(String fieldName, Permission permissionRequired, String additionalHql, PageState pageState) throws PersistenceException {
    return getResourceCombo(fieldName, permissionRequired, additionalHql, Company.class, pageState);
  }

	public static SmartCombo getResourceCombo1(String fieldName, RestState pageState, String taskId)
			throws PersistenceException {

		String hql = "select resource.id, resource.name, coalesce(resource.code,' ') from " + Person.class.getName()
				+ " as resource ";

		String whereForId = "where resource.id = :" + SmartCombo.FILTER_PARAM_NAME;

		SmartCombo resources = new SmartCombo(fieldName, hql, null, whereForId);

		QueryHelper queryHelperForFiltering = new QueryHelper(hql);

		// inject security clause
		TeamworkOperator looged = (TeamworkOperator) pageState.getLoggedOperator();

		if (looged.getPerson().getArea() != null) {
			queryHelperForFiltering.addQueryClause("resource.area in (:areas) or resource.area is null");
			Set<Area> areas = new HashSet<>();
			areas.add(looged.getPerson().getArea());
			queryHelperForFiltering.addParameter("areas", areas);
		}

		String hql2 = "from " + Assignment.class.getName() + " as am where am.task.id=:taskId";
		OqlQuery oql = new OqlQuery(hql2);
		oql.setParameter("taskId", taskId);
		List<Assignment> o = oql.list();
		List<String> rs = new ArrayList<>();
		for (Assignment oo : o) {
			if (!looged.getPerson().getId().equals(oo.getResource().getId())) {
				rs.add(oo.getResource().getId().toString());
			}
		}
		if (!rs.isEmpty()) {
			queryHelperForFiltering.addQueryClause("resource.id in (:ids)");
			queryHelperForFiltering.addParameter("ids", rs);
		}

		String hql3 = "select r.id from " + Resource.class.getName() + " as r,"
				+ Operator.class.getName() + " as p,"
				+ OperatorRole.class.getName() + " as pl," + RoleTeamwork.class.getName() + " as orl"
				+ " where r.myself.id=p.id" + " and p.id=pl.operator.id" + " and pl.role.id=orl.id"
				+ " and orl.permissionIds like '%TW_taskadt_a%' ";

		QueryHelper queryHelper3 = new QueryHelper(hql3);
		if (looged.getPerson().getArea() != null) {
			queryHelper3.addQueryClause(" and orl.area.id=:area ");
			queryHelper3.addParameter("area", looged.getPerson().getArea());
		}
		OqlQuery oql3 = new OqlQuery(hql3);
		List<String> o3 = oql3.list();

		if (!o3.isEmpty()) {
			queryHelperForFiltering.addOrQueryClause(" resource.id in (:id) ");
			queryHelperForFiltering.addParameter("id", o3);
		}

		String baseFilter = "";

		baseFilter = "(upper(resource.personName || ' ' || resource.personSurname) like :"
				+ SmartCombo.FILTER_PARAM_NAME + " or upper(resource.personSurname || ' ' || resource.personName) like:"
				+ SmartCombo.FILTER_PARAM_NAME + " or upper(resource.name) like :" + SmartCombo.FILTER_PARAM_NAME
				+ " or upper(resource.code) like :" + SmartCombo.FILTER_PARAM_NAME + ") and resource.hidden = false";

		queryHelperForFiltering.addOQLClause(baseFilter);

		queryHelperForFiltering.addToHqlString(" order by resource.name");

		resources.queryHelperForFiltering = queryHelperForFiltering;
		resources.searchAll = true;
		resources.separator = "</td><td>";
		resources.fieldSize = 40;
		resources.convertToUpper = true;
		return resources;
	}

  public static SmartCombo getResourceCombo(String fieldName, Permission permissionRequired, String additionalHql, Class resClass, RestState pageState) throws PersistenceException {

    String hql = "select resource.id, resource.name, coalesce(resource.code,' ') from " + resClass.getName() + " as resource ";

    String whereForId = "where resource.id = :" + SmartCombo.FILTER_PARAM_NAME;

    SmartCombo resources = new SmartCombo(fieldName, hql, null, whereForId);

    QueryHelper queryHelperForFiltering = new QueryHelper(hql);

    //inject security clause
    addSecurityClauses("resource", true, queryHelperForFiltering, CollectionUtilities.toList(permissionRequired), (TeamworkOperator) pageState.getLoggedOperator(), false, true);

    String baseFilter = "";
    if (resClass.equals(Company.class)) {
      baseFilter = "( upper(resource.name) like :" + SmartCombo.FILTER_PARAM_NAME + "" +
        " or upper(resource.code) like :" + SmartCombo.FILTER_PARAM_NAME +
        ") and resource.hidden = false ";
    } else {
      baseFilter =
        "(upper(resource.personName || ' ' || resource.personSurname) like :" + SmartCombo.FILTER_PARAM_NAME +
          " or upper(resource.personSurname || ' ' || resource.personName) like :" + SmartCombo.FILTER_PARAM_NAME +
          " or upper(resource.name) like :" + SmartCombo.FILTER_PARAM_NAME +
          " or upper(resource.code) like :" + SmartCombo.FILTER_PARAM_NAME +
          ") and resource.hidden = false";
    }

    if (additionalHql != null)
      queryHelperForFiltering.addToHqlString(additionalHql);

    queryHelperForFiltering.addOQLClause(baseFilter);

    queryHelperForFiltering.addToHqlString(" order by resource.name");

    resources.queryHelperForFiltering = queryHelperForFiltering;
    resources.searchAll = true;
    resources.separator = "</td><td>";
    resources.fieldSize = 40;
    resources.convertToUpper = true;
    return resources;
  }


  public static Person myself(PageState pageState) {
    return ((TeamworkOperator) pageState.getLoggedOperator()).getPerson();
  }


  /**
   * @param resourceAlias
   * @param isInAnd
   * @param queryHelperForFiltering alias for resource MUST be"resource"
   * @param permissions             are in OR
   * @param logged
   * @param includeMyself
   * @param includeMyStaff          @throws PersistenceException
   */
  public static void addSecurityClauses(String resourceAlias, boolean isInAnd, QueryHelper queryHelperForFiltering, List<Permission> permissions, TeamworkOperator logged, boolean includeMyself, boolean includeMyStaff) throws PersistenceException {

    //long millis=System.currentTimeMillis();
    //open unique clause and set owner; qhelp puts the "and" before; all following clauses are in or
    if (isInAnd)
      queryHelperForFiltering.addOQLClause("( " + resourceAlias + ".owner = :logged", "logged", logged);
    else {
      queryHelperForFiltering.addToHqlString("( " + resourceAlias + ".owner = :logged");
      queryHelperForFiltering.addParameter("logged", logged);
    }

    if (includeMyself && RoleTeamwork.getMyselfRole().hasPermissionForOR(permissions)) {
      queryHelperForFiltering.addOrQueryClause("" + resourceAlias + ".myself = :logged");
      queryHelperForFiltering.addParameter("logged", logged);
    }

    //areas
    Set<Area> areas = new HashSet<>();
    for (Permission permission : permissions) {
      areas.addAll(logged.getAreasForPermission(permission));
    }

    if (areas.size() > 0) {
      queryHelperForFiltering.addOrQueryClause("" + resourceAlias + ".area in (:areas) or " + resourceAlias + ".area is null");
      queryHelperForFiltering.addParameter("areas", areas);
    } else
      queryHelperForFiltering.addOrQueryClause("0=1");  // se non si hanno permessi su nessuna area (size=0) non devo vedere niente


    if (includeMyStaff  && RoleTeamwork.getManagerRole().hasPermissionForOR(permissions)) {
      addMyStaffQueryClause(resourceAlias, queryHelperForFiltering, logged.getPerson());
    }

    //in order to keep all security conditions in a unique and clause
    queryHelperForFiltering.addToHqlString(")");
  }


  /**
   * @param resourceAlias
   * @param queryHelperForFiltering alias for resource MUST be"resource"
   * @param loggedPerson
   */
  public static void addMyStaffQueryClause(String resourceAlias, QueryHelper queryHelperForFiltering, Person loggedPerson) {
    //get my staff
    //use manager ids for performance
    queryHelperForFiltering.addOrQueryClause("" + resourceAlias + ".myManagerIds is not null and " + resourceAlias + ".myManagerIds like :myStemmedIds"); // not null for hsqldb bug
    String param = (loggedPerson.getMyManagerIds() != null ? loggedPerson.getMyManagerIds() : "") + loggedPerson.getId() + PerformantNode.SEPARATOR + "%";
    queryHelperForFiltering.addParameter("myStemmedIds", param);

    //get hirarchy of managed departments -> get resources of managed departments
    OqlQuery depQ = new OqlQuery("select d.id,d.ancestorIds from " + Company.class.getName() + " as d where d.myManager=:m");
    depQ.getQuery().setEntity("m", loggedPerson);
    List<Object[]> deps = depQ.getQuery().list();
    if (JSP.ex(deps)) { // for hsqldb or+like null poinet error
      queryHelperForFiltering.addToHqlString("");
      queryHelperForFiltering.addOrQueryClause("(" + resourceAlias + ".ancestorIds is not null");
      boolean first = true;
      for (Object[] c : deps) {
        String p = (c[1] != null ? c[1] + "" : "") + c[0] + PerformantNode.SEPARATOR + "%";
        if (first)
          queryHelperForFiltering.addQueryClause("(" + resourceAlias + ".ancestorIds like '" + p + "'");
        else
          queryHelperForFiltering.addOrQueryClause("" + resourceAlias + ".ancestorIds like '" + p + "'");
        first = false;
      }
      queryHelperForFiltering.addToHqlString("))");
    }
  }


  public static SmartCombo getInspectableResourcesCombo(String fieldName, PageState pageState) throws PersistenceException {
    return getInspectableResourcesCombo(fieldName, CollectionUtilities.toList(TeamworkPermissions.resource_manage, TeamworkPermissions.worklog_manage),pageState);
  }

  public static SmartCombo getInspectableResourcesCombo(String fieldName, List<Permission> permissions, PageState pageState) throws PersistenceException {
    String hql = "select resource.id, resource.name, coalesce(resource.code,' ') from " + Resource.class.getName() + " as resource ";
    String whereForId = "where resource.id = :" + SmartCombo.FILTER_PARAM_NAME;
    SmartCombo resources = new SmartCombo(fieldName, hql, null, whereForId);
    QueryHelper queryHelperForFiltering = new QueryHelper(hql);

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    Person loggedPerson = logged.getPerson();
    addSecurityClauses("resource", true, queryHelperForFiltering, permissions, logged, true, true);

    String baseFilter =
      "(upper(resource.personName || ' ' || resource.personSurname) like :" + SmartCombo.FILTER_PARAM_NAME +
        " or upper(resource.personSurname || ' ' || resource.personName) like :" + SmartCombo.FILTER_PARAM_NAME +
        " or upper(resource.name) like :" + SmartCombo.FILTER_PARAM_NAME +
        " or upper(resource.code) like :" + SmartCombo.FILTER_PARAM_NAME +
        ") and resource.hidden = false";

    queryHelperForFiltering.addOQLClause(baseFilter);

    queryHelperForFiltering.addToHqlString(" order by resource.name");

    resources.queryHelperForFiltering = queryHelperForFiltering;
    resources.searchAll = true;
    resources.separator = "</td><td>";
    resources.convertToUpper = true;
    resources.fieldSize = 40;


    //inject my direct staff in bold
    //si aggiunge li staff solo se managerRole contiene uno dei permessi
    boolean managerHasPerm= RoleTeamwork.getManagerRole().hasPermissionForOR(permissions) ;
    boolean myselfHasPerm= RoleTeamwork.getMyselfRole().hasPermissionForOR(permissions) ;

    TreeSet<Resource> allMyStaff= new TreeSet();
    if (myselfHasPerm)
      allMyStaff.add(loggedPerson);

    if (managerHasPerm)
      allMyStaff.addAll(loggedPerson.getAllMyStaff());

    if (allMyStaff.size()>0) {
      List<Object[]> resOfAssigsOnTask = new ArrayList<>();
      for (Resource r : allMyStaff) {
        resOfAssigsOnTask.add(new Object[]{r.getId(), r.getName(), JSP.w(r.getCode())});
      }
      resources.additionalLines = resOfAssigsOnTask;
    }



    return resources;
  }

  public static SmartCombo getInspectableCustomersCombo(String fieldName, PageState pageState) throws PersistenceException {
    String hql = "select distinct resource.id, resource.name, coalesce(resource.code,' ') from " + Assignment.class.getName() + " as ass join ass.resource as resource";
    String whereForId = "where resource.id = :" + SmartCombo.FILTER_PARAM_NAME;
    SmartCombo resources = new SmartCombo(fieldName, hql, null, whereForId);
    QueryHelper queryHelperForFiltering = new QueryHelper(hql);

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    Person loggedPerson = logged.getPerson();
    //addSecurityClauses("resource", true, queryHelperForFiltering, CollectionUtilities.toList(TeamworkPermissions.assignment_manage), logged, true, true);
    addSecurityClauses("resource", true, queryHelperForFiltering, CollectionUtilities.toList(TeamworkPermissions.resource_canRead), logged, true, true);

    String baseFilter =
      "ass.role.name like :roleCust and " +
        "(upper(resource.personName || ' ' || resource.personSurname) like :" + SmartCombo.FILTER_PARAM_NAME +
        " or upper(resource.personSurname || ' ' || resource.personName) like :" + SmartCombo.FILTER_PARAM_NAME +
        " or upper(resource.name) like :" + SmartCombo.FILTER_PARAM_NAME +
        " or upper(resource.code) like :" + SmartCombo.FILTER_PARAM_NAME +
        ") and resource.hidden = false";

    queryHelperForFiltering.addOQLClause(baseFilter);
    queryHelperForFiltering.addParameter("roleCust", ApplicationState.getApplicationSetting("DEFAULT_CUSTOMER_ROLE_NAME", "Customer")+"%");
    queryHelperForFiltering.addToHqlString(" order by resource.name");

    resources.queryHelperForFiltering = queryHelperForFiltering;
    resources.searchAll = true;
    resources.separator = "</td><td>";
    resources.convertToUpper = true;
    resources.fieldSize = 40;

    return resources;
  }


  public ButtonSupport getPopupMessage(PageState pageState) {

    PageSeed pss = new PageSeed("");
    pss.setCommand(Commands.ADD);
    pss.addClientEntry("receiver", mainObject.getId());
    ButtonSupport cstickyL = new StickyNote().bricks.getEditorInBlack("", pss, pageState);
    cstickyL.iconChar = "&igrave;";
    cstickyL.outputModality = ButtonSupport.TEXT_ONLY;
    cstickyL.toolTip = I18n.get("SEND_MESSAGE");
    return cstickyL;
  }


  public static List<Person> getPersonByEmail(String email) throws PersistenceException {
    String hql = "select distinct p from " + Person.class.getName() + " as p join p.anagraphicalDatas as ad where upper(ad.email) like :email";
    OqlQuery query = new OqlQuery(hql);
    query.getQuery().setString("email", email.toUpperCase() + "%");
    return query.list();
  }

  public static List<Company> guessCompanyFromName(String name) throws PersistenceException {
    String hql = "select resource from " + Company.class.getName() + " as resource";
    QueryHelper qh = new QueryHelper(hql);
    qh.addQBEClause("name", "completeName", name, QueryHelper.TYPE_CHAR);
    qh.convertToUpper = true;
    return qh.toHql().list();
  }

  public static List<Person> guessPersonFromName(String name) throws PersistenceException {

    String hql =
      "select resource from " + Person.class.getName() + " as resource";

    //find exact
    String exact = hql + " where (upper(resource.personName || ' ' || resource.personSurname) = :name) or (upper(resource.personSurname || ' ' || resource.personName) = :name)";
    OqlQuery oql = new OqlQuery(exact);
    oql.setParameter("name", name.toUpperCase());
    List<Person> list = oql.list();
    if (list.size() == 1)
      return list;
    else {
      QueryHelper qh = new QueryHelper(hql);
      qh.addQBEORClauses(name,
        qh.getOrElement("resource.personName || ' ' || resource.personSurname", "name", QueryHelper.TYPE_CHAR),
        qh.getOrElement("resource.personSurname || ' ' || resource.personName", "name", QueryHelper.TYPE_CHAR));
      qh.convertToUpper = true;
      list = qh.toHql().list();
      return list;
    }
  }

  private String getGravatarLink(String email, int size) {
    String emailOk = JSP.ex(email) ? email : (mainObject.getId() + "@accasaccio.com");
    return ResourceBricks.getGravatarUrl(emailOk,size);
  }
  public String getAvatarImageUrl() {
    String ret = "";
    if (this.mainObject.getMyPhoto() != null) {
      PageSeed imgPs = this.mainObject.getMyPhoto().getPageSeed(false);
      imgPs.disableCache = false;
      ret = imgPs.toLinkToHref();

      // questo uso è deprecabile. Rimane solo per compatibilità
    } else if (JSP.ex(mainObject.getMyself()) && JSP.ex(mainObject.getMyself().getOptions().get("IMAGEURL"))) {
      ret = mainObject.getMyself().getOptions().get("IMAGEURL");

    } else if (JSP.ex(mainObject.getDefaultEmail())) {
      ret = getGravatarLink(mainObject.getDefaultEmail(), 120);
    } else {
      ret = ApplicationState.contextPath + "/img/noPhoto_TW.jpg";
    }
    return ret;
  }

  public Img getAvatarImage(String size) {
    Img img = new Img(getAvatarImageUrl(), mainObject.getDisplayName());
    img.script = "class='face " + (JSP.ex(size) ? size : "") + "'";
    return img;
  }


  /**
   * Ritorna sempre l'immagine di gravatar
    * @param email
   * @param size
   * @return
   */
  public static String getGravatarUrl(String email, int size) {
    String pictureUrl = JSP.ex(email) ? email : ("info@twproject.com");
    pictureUrl = "https://www.gravatar.com/avatar/" + StringUtilities.md5Encode(pictureUrl, "") + "?s=" + size + "&d=identicon";
    return pictureUrl;
  }



  public String getScoreBadge(PageState pageState) {
    if (!Fields.FALSE.equals(ApplicationState.getApplicationSetting("SHOW_USER_SCORES"))) {
      int score = getScore(pageState);

      return "<div id=\"scrxx\" style=\"display:none;\"></div><div id='scoreTableId' class='score' title=\"" + I18n.get("TEAMWORK_CURRENT_SCORE_HELP") + "\">" + score + "</div>";
    } else
      return "&nbsp";

  }

  public int getScore(PageState pageState) {
    int score = 0;
    //compute score ONCE per request
    String scoreInPageState = (String) pageState.getAttribute("TW_USER_SCORE");
    if (JSP.ex(scoreInPageState)) {
      score = Integer.parseInt(scoreInPageState);
    } else {
      if (mainObject.getMyself() != null) {
        score = mainObject.getMyself().getScore();
        pageState.setAttribute("TW_USER_SCORE", score + "");
      }
    }
    return score;
  }

  public ButtonLink getPlanButton(PageState pageState) {
    PageSeed plSeed = pageState.pageFromRoot("task/plan/planByResource.jsp");
    Bricks.addReferral(mainObject.getId(), Resource.class, plSeed);
    plSeed.setCommand("FIND_BY_ENTITY");
    plSeed.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));
    plSeed.setCommand("FIND_BY_ENTITY");
    ButtonLink pb = new ButtonLink(I18n.get("WORKLOG_SEE_RESOURCE_PLAN"), plSeed);
    return pb;
  }


  /**
   * @param restState using "FIND_BY_ENTITY" or "WG_IDS" ce
   * @return a list of resource identified by a PeopleAggregator use as reference in a page
   * then add logged if none selected
   * and inject clientEntries required by workgroup composer
   */
  public static List<Resource> fillWorkGroup(RestState restState) throws PersistenceException {
    List<Resource> resources = new ArrayList();
    if ("FIND_BY_ENTITY".equals(restState.getCommand())) {

      IdentifiableSupport is = Bricks.getReferral(restState);
      if (is instanceof PeopleAggregator) {
        resources.addAll(((PeopleAggregator) is).getPersons());
      } else if (is instanceof Issue) {
        resources.add(((Issue) is).getAssignedBy());
      }
    } else if (JSP.ex(restState.getEntry("WG_IDS"))) {
      List<String> ids = new ArrayList<>();
      ids.addAll(StringUtilities.splitToList(restState.getEntry("WG_IDS").stringValueNullIfEmpty(), ","));
      String hql = "select distinct resource from " + Person.class.getName() + " as resource order by resource.personSurname,resource.personName,resource.name";
      QueryHelper qhelp = new QueryHelper(hql);
      qhelp.addOQLInClause("id", "ids", ids);
      List<Resource> ress = qhelp.toHql().list();
      resources.addAll(ress);

    }

    //add logged if any
    if (resources.size() == 0 && restState.getLoggedOperator() != null)
      resources.add(((TeamworkOperator) restState.getLoggedOperator()).getPerson());


    String sendToIds = "";
    String sendTo = "";

    if (resources.size() > 0) {
      //reconstruct ids
      for (Resource res : resources) {
        sendToIds += (sendToIds.length() == 0 ? "" : ",") + res.getId();
        sendTo += (sendTo.length() == 0 ? "" : ";") + res.getDisplayName();
      }
    }


    restState.addClientEntry("WG_NAMES", JSP.encode(sendTo));
    restState.addClientEntry("WG_IDS", sendToIds);

    return resources;

  }

  public static boolean hasCustomField() {
    return DesignerField.hasCustomField("RESOURCE_CUSTOM_FIELD_", 6);
  }

  public static double getDefaultHourlyCost() {
    double cost = 20.0;
    String costStr = ApplicationState.getApplicationSetting("ASSIG_COST");
    if (JSP.ex(costStr)) {
      NumberFormat numberInstance = NumberFormat.getNumberInstance(Locale.US); // global.properties always in locale.US. NOT the system locale!!!
      try {
        cost = numberInstance.parse(costStr).doubleValue();
      } catch (ParseException e) {
      }
    }

    return cost;
  }

  public static SmartCombo getDepartmentTypeCombo(String fieldName, PageState pageState) {
    String hql = "select p.id, p.description from " + DepartmentType.class.getName() + " as p ";
    String whereForFiltering = "where p.description like :" + SmartCombo.FILTER_PARAM_NAME + " order by p.description";
    String whereForId = "where p.id = :" + SmartCombo.FILTER_PARAM_NAME;
    SmartCombo departmentType = new SmartCombo(fieldName, hql, whereForFiltering, whereForId);
    departmentType.label = I18n.get("TYPE");
    departmentType.fieldSize = 25;
    departmentType.separator = "<br>";

    if (pageState.getLoggedOperator().hasPermissionAsAdmin()) {
      ButtonSupport addTT = ButtonLink.getBlackInstance(JSP.wHelp(I18n.get("ADD_TYPE")), pageState.pageFromRoot("resource/departmentType.jsp"));
      addTT.additionalCssClass = "small";
      departmentType.addEntityButton = addTT;
    }

    return departmentType;
  }


  public static List<Resource> getPersonByTag(String tag,Boolean activeOnly, RestState restState) throws PersistenceException {
    List<Resource> resources = new ArrayList();
    if (JSP.ex(tag)) {
      RestState rs = new RestState(restState.getLoggedOperator());
      rs.command=Commands.FIND;
      rs.addClientEntry("RESOURCE_TYPE", "PERSON");
      rs.addClientEntry("RESOURCE_TAGS", tag);
      if (activeOnly)
        rs.addClientEntry("BY_ENABLED", "IS_ENABLED");

      ResourceAction ra = new ResourceAction(rs);
      ra.cmdFind();

      Page page = rs.getPage();
      if (JSP.ex(page)){
        resources=page.getAllElements();
      }

    }
    return resources;

  }


}
