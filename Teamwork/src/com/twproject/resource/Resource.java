package com.twproject.resource;

import com.twproject.agenda.Event;
import com.twproject.document.TeamworkDocument;
import com.twproject.operator.TeamworkOperator;
import com.twproject.security.RoleTeamwork;
import com.twproject.security.TeamworkPermissions;
import com.twproject.task.Assignment;
import com.twproject.task.TaskStatus;
import com.twproject.task.financial.CostAggregator;
import com.twproject.utilities.TeamworkComparators;
import com.twproject.worklog.Worklog;
import net.sf.json.JSONArray;
import net.sf.json.JSONObject;
import org.hibernate.search.annotations.*;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.agenda.Period;
import org.jblooming.anagraphicalData.AnagraphicalData;
import org.jblooming.classification.Taggable;
import org.jblooming.company.Location;
import org.jblooming.designer.DesignerData;
import org.jblooming.logging.Auditable;
import org.jblooming.ontology.*;
import org.jblooming.operator.User;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.FindException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.security.Permission;
import org.jblooming.security.PermissionCache;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.exceptions.ActionException;

import javax.persistence.Transient;
import java.io.Serializable;
import java.util.*;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public abstract class Resource extends SecuredNodeWithAreaSupport implements PeopleAggregator, Documentable, Auditable, Taggable, PermissionCache.PermissionCacheEnabled {

  public static final String RESOURCE = "RESOURCE";

  private String name;
  protected Resource parent;
  private String code;
  private Location location;
  private Set<AnagraphicalData> anagraphicalDatas = new HashSet<AnagraphicalData>();
  private Set<TeamworkDocument> documents = new HashSet<TeamworkDocument>();
  private String notes;

  /**
   * Introduced in order to introduce virtual or multiuser resources, such as "a board", "a counsel" "stearing committee"
   */

  private boolean staff = false;

  private String externalCode;

  private Person myManager;
  //for performance
  private String myManagerIds;

  private CostAggregator myCostAggregator;

  // read only colls: is the collection of resources where "this" is the manager
  private Set<Resource> myStaff;

  protected String jobDescription;

  public ResourceBricks bricks = new ResourceBricks(this);
  private PersistentFile myPhoto;

  private double hourlyCost;
  private long workDailyCapacity;

  private String tags;

  private String customField1;
  private String customField2;
  private String customField3;
  private String customField4;
  private String customField5;
  private String customField6;


  @Field(name = "content")
  @Boost(3)
  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public Set<Person> getPersons() {

    return getPersons(new HashSet<Resource>(), new HashSet<Person>());
  }

  public abstract Set<Person> getPersons(Set<Resource> visitedNodes, Set<Person> workers);

  public void setDocuments(Set<TeamworkDocument> documents) {
    this.documents = documents;
  }

  public Set getDocuments() {
    return documents;
  }

  public int getDocumentsSize() {
    return getDocuments() != null ? getDocuments().size() : 0;
  }

  public void addDocument(TeamworkDocument document) {
    documents.add(document);
  }


  public Iterator<TeamworkDocument> getDocumentsIterator() {
    return documents.iterator();
  }


  private void setParent(Resource n) {
    parent = n;
  }

  public Resource getParent() {
    return parent;
  }

  public Node getParentNode() {
    return getParent();
  }

  public void setParentNode(Node node) {
    setParent((Resource) node);
  }

  public String getCode() {
    return code;
  }

  public void setCode(String code) {
    this.code = code;
  }

  public Location getLocation() {
    return location;
  }

  public void setLocation(Location location) {
    this.location = location;
  }

  public Set<AnagraphicalData> getAnagraphicalDatas() {
    return anagraphicalDatas;
  }

  public void setAnagraphicalDatas(Set<AnagraphicalData> anagraphicalDatas) {
    this.anagraphicalDatas = anagraphicalDatas;
  }

  public String getNotes() {
    return notes;
  }

  public void setNotes(String notes) {
    this.notes = notes;
  }

  public boolean isStaff() {
    return staff;
  }

  public void setStaff(boolean staff) {
    this.staff = staff;
  }

  public Person getMyManager() {
    return myManager;
  }

  private void setMyManager(Person myManager) {
    this.myManager = myManager;
  }

  public CostAggregator getMyCostAggregator() {
    return myCostAggregator;
  }

  public void setMyCostAggregator(CostAggregator myCostAggregator) {
    this.myCostAggregator = myCostAggregator;
  }

  public Set<Resource> getMyStaff() {
    return myStaff;
  }

  private void setMyStaff(Set<Resource> myStaff) {
    this.myStaff = myStaff;
  }

  @org.hibernate.search.annotations.Field(name = "content", analyze = Analyze.YES, index = Index.YES, store = Store.NO)
  @Boost(2)
  public String getTags() {
    return tags;
  }

  public void setTags(String tags) {
    this.tags = tags;
  }

  public abstract String getDisplayName();

  public String getExternalCode() {
    return externalCode;
  }

  public void setExternalCode(String externalCode) {
    this.externalCode = externalCode;
  }


  public String getJobDescription() {
    return jobDescription;
  }

  public void setJobDescription(String jobDescription) {
    this.jobDescription = jobDescription;
  }


  public String getMyManagerIds() {
    return myManagerIds;
  }

  public void setMyManagerIds(String myManagerIds) {
    this.myManagerIds = myManagerIds;
  }

  public PersistentFile getMyPhoto() {
    return myPhoto;
  }

  public void setMyPhoto(PersistentFile myPhoto) {
    this.myPhoto = myPhoto;
  }

  public double getHourlyCost() {
    return hourlyCost;
  }

  public void setHourlyCost(double hourlyCost) {
    this.hourlyCost = hourlyCost;
  }

  public String getCustomField1() {
    return customField1;
  }

  public void setCustomField1(String customField1) {
    this.customField1 = customField1;
  }

  public String getCustomField2() {
    return customField2;
  }

  public void setCustomField2(String customField2) {
    this.customField2 = customField2;
  }

  public String getCustomField3() {
    return customField3;
  }

  public void setCustomField3(String customField3) {
    this.customField3 = customField3;
  }

  public String getCustomField4() {
    return customField4;
  }

  public void setCustomField4(String customField4) {
    this.customField4 = customField4;
  }

  public String getCustomField5() {
    return customField5;
  }

  public void setCustomField5(String customField5) {
    this.customField5 = customField5;
  }

  public String getCustomField6() {
    return customField6;
  }

  public void setCustomField6(String customField6) {
    this.customField6 = customField6;
  }


  public List<Assignment> getAssignments() throws FindException {
    OqlQuery oql = new OqlQuery("from " + Assignment.class.getName() + " as ass where ass.resource=:res order by ass.id");
    oql.getQuery().setEntity("res", this);
    return oql.list();
  }

  public int getAssignmentsSize() throws FindException {
    OqlQuery oql = new OqlQuery("select count(ass) from " + Assignment.class.getName() + " as ass where ass.resource=:res");
    oql.getQuery().setEntity("res", this);
    return ((Long) oql.uniqueResult()).intValue();
  }



  public void setMyManagerAndPerfIds(Person myManager) throws ActionException {
    //only if mymanager is changed
    if (myManager != null && !myManager.equals(getMyManager()) || myManager == null && getMyManager() != null) {
      boolean wasFirstLevel = !JSP.ex(getMyManagerIds());

      if (myManager != null) {

        //this must not be on manager's managers
        if (JSP.ex(myManager.getMyManagerIds())) {
          if ((SEPARATOR + myManager.getMyManagerIds()).indexOf(SEPARATOR + getId() + SEPARATOR) >= 0)
            throw new ActionException("You can't set this resource as manager as it is 'managed' by '"+getName()+"'");//   of its own manager ancestor ids = '" + myManager.getMyManagerIds() + "', manager id = '" + myManager.getId() + "'");
        }

        myManager.getMyStaff().add(this); //add in memory
        setMyManagerIds((JSP.ex(myManager.getMyManagerIds()) ? myManager.getMyManagerIds() : "") + myManager.getId() + SEPARATOR);
      } else {
        setMyManagerIds(null);
      }

      setMyManager(myManager);


      //update staff codes
      //get hirarchy of managed resources
      OqlQuery depQ = new OqlQuery("select res from " + Resource.class.getName() + " as res where res.myManagerIds like:dep");
      String search = (wasFirstLevel ? "" : SEPARATOR) + getId() + SEPARATOR;
      depQ.getQuery().setString("dep", (wasFirstLevel ? "" : "%") + search + "%");


      try {
        List<Resource> allRes = depQ.list();
        for (Resource r : allRes) {
          String oldMans = r.getMyManagerIds();
          int pos = oldMans.indexOf(search);

          String newMans = oldMans.substring(pos + search.length());
          r.setMyManagerIds(JSP.w(getMyManagerIds()) + getId() + SEPARATOR + newMans);

        }

      } catch (FindException f) {
        throw new PlatformRuntimeException(f);
      }
    }
  }


  public List<Assignment> getActiveAssignments(Period period, boolean showActiveOnly) throws PersistenceException {
    return getActiveAssignments(period, showActiveOnly, true, false);
  }


  public List<Assignment> getActiveAssignments(Period period, boolean showActiveOnly, boolean showActiveTasksOnly, boolean showPlannableOnly) throws PersistenceException {
    return getActiveAssignments(period, showActiveOnly, showActiveTasksOnly, showPlannableOnly, false);
  }

  public List<Assignment> getActiveAssignments(Period period, boolean showActiveOnly, boolean showActiveTasksOnly, boolean showPlannableOnly, boolean showWithEstimationOnly) throws PersistenceException {
    String hql = "select assignment from " + Assignment.class.getName() + " as assignment where assignment.resource=:resource";

    if (showPlannableOnly) {
      hql = hql + " and assignment.activity='" + Assignment.ACTIVITY_ALL_IN_ONE + "'";
    }

    if (showActiveOnly) {
      hql = hql + " and assignment.enabled=:truth";
    }

    if (showWithEstimationOnly) {
      hql = hql + " and (assignment.estimatedWorklog>0)";
    }

    boolean mustFilterByHand = false;

    // when there is a period the status should refer to that period not the current status
    if (period != null) {
      hql = hql +
              " and  ( " + "" +
              "(assignment.task.schedule.start<=:periodEnd or assignment.task.schedule.start is null) and " +
              "(assignment.task.schedule.end>:periodStart or assignment.task.schedule.end is null) " +
              " )";

      if (showActiveTasksOnly) {
        mustFilterByHand = true;
      }


    } else {
      if (showActiveTasksOnly) {
        hql = hql + " and assignment.task.status=:STATUS_ACTIVE";
      }
    }

    hql = hql + " order by assignment.task.code, assignment.task.name";

    OqlQuery query = new OqlQuery(hql);
    query.setParameter("resource", this);

    if (showActiveOnly) {
      query.setParameter("truth", Boolean.TRUE);
    }

    if (showActiveTasksOnly && !mustFilterByHand) {
      query.setParameter("STATUS_ACTIVE", TaskStatus.STATUS_ACTIVE);
    }

    if (period != null) {
      query.setParameter("periodEnd", period.getValidityEndDate());
      query.setParameter("periodStart", period.getValidityStartDate());
    }


    List<Assignment> list = query.list();

    List<Assignment> ret = list;
    if (mustFilterByHand) {
      ret = new ArrayList<Assignment>();
      for (Assignment ass : list) {
        // P&S: changed from start date to end date
        //this shows those that have just been closed, but not those that have just been opened!
        //if (TaskStatus.STATUS_ACTIVE.equals(ass.getTask().getStatusOn(period.getStartDate())))
        //

        String statusOnEnd = ass.getTask().getStatusOn(period.getEndDate());
        String statusOnStart = ass.getTask().getStatusOn(period.getStartDate());
        if (((TaskStatus.STATUS_ACTIVE.equals(statusOnEnd)) || (TaskStatus.STATUS_ACTIVE.equals(statusOnStart))))
          ret.add(ass);
      }
    }
    return ret;

  }


  public List<Assignment> getExpiredAssignments(Date date, boolean showActiveOnly, boolean showPlannableOnly) throws PersistenceException {
    String hql = "select assignment from " + Assignment.class.getName() + " as assignment where assignment.resource=:resource";

    if (showPlannableOnly) {
      hql = hql + " and assignment.activity='" + Assignment.ACTIVITY_ALL_IN_ONE + "'";
    }

    if (showActiveOnly) {
      hql = hql + " and assignment.enabled=:truth";
    }

    // when there is a period the status should refer to that perios not the current status
    if (date != null) {
      hql = hql + " and (assignment.task.schedule.end<=:whenx )";
    }

    hql = hql + "and assignment.task.status=:STATUS_ACTIVE";

    hql = hql + " order by assignment.task.code, assignment.task.name";

    OqlQuery query = new OqlQuery(hql);
    query.setParameter("resource", this);

    if (showActiveOnly) {
      query.setParameter("truth", Boolean.TRUE);
    }

    query.setParameter("STATUS_ACTIVE", TaskStatus.STATUS_ACTIVE);

    if (date != null) {
      query.setParameter("whenx", date);
    }

    return query.list();

  }

  /**
   * @param period
   * @param showActiveOnly
   * @param showActiveTasksOnly
   * @return
   */
  public List<Assignment> getAssignmentsByPriority(Period period, boolean showActiveOnly, boolean showActiveTasksOnly) {

    List<Assignment> assigs = null;
    long when = 0;
    try {
      if (period == null) {
        period = new Period(0, CompanyCalendar.MILLIS_IN_YEAR * 100);  // I'll wait for your complaints.....
        when = System.currentTimeMillis();
      } else {
        when = (period.getEndDate().getTime() + period.getStartDate().getTime()) / 2;
      }
      assigs = getActiveAssignments(period, showActiveOnly, showActiveTasksOnly, false);
      Collections.sort(assigs,new TeamworkComparators.AssignmentByPriority(new Date(when)));

    } catch (PersistenceException e) {
      throw new PlatformRuntimeException(e);
    }


    return assigs;
  }

  public long getWorklogDone(Date from) {
    return getWorklogDone(from, null);
  }

  public long getWorklogDone(Date from, Date to) {

    String hql = "select sum(wklg.duration) from " + Worklog.class.getName() + " as wklg where wklg.assig.resource = :assignee and wklg.inserted>=:fromx";
    if (to != null)
      hql = hql + " and wklg.inserted<=:tox";

    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setEntity("assignee", this);
    oql.getQuery().setTimestamp("fromx", from);
    if (to != null)
      oql.getQuery().setTimestamp("tox", to);
    try {
      return (Long) oql.uniqueResult();
    } catch (FindException e) {
      return 0;
    }
  }

  public List<AnagraphicalData> getAnagraphicalDataOrdered() {

    ArrayList<AnagraphicalData> orderAnagraphicalData = new ArrayList<AnagraphicalData>(getAnagraphicalDatas());

    Collections.sort(orderAnagraphicalData, new TeamworkComparators.AnagraphicalDataComparator());

    return orderAnagraphicalData;
  }

  public List<AnagraphicalData> getAnagraphicalDataOrderedNoHidden() {
    List<AnagraphicalData> ads = getAnagraphicalDataOrdered();
    List<AnagraphicalData> adsNoHid = new ArrayList<AnagraphicalData>();
    for (AnagraphicalData ad : ads) {
      if (!ad.isHideAnagraphicalData())
        adsNoHid.add(ad);
    }
    return adsNoHid;
  }

  /**
   * default email is the first one!
   *
   * @return
   */
  public String getDefaultEmail() {

    List<AnagraphicalData> ads = getAnagraphicalDataOrderedNoHidden();
    String email = null;
    for (AnagraphicalData ad : ads) {
      if (JSP.ex(ad.getEmail())) {
        email = ad.getEmail();
        break;
/*
        if (email == null)
          email = ad.getEmail();
        else
          email = email + "," + ad.getEmail();
*/
      }
    }
    return email;
  }



  abstract public TeamworkOperator getMyself();

  public List<Resource> getChildrenOrdered() {

    List ch = new ArrayList(getChildren());
    Collections.sort(ch, new TeamworkComparators.ResourceComparator());

    return ch;
  }


  public List<Assignment> getAssignmentsWithWorklog(Period filterPeriod) throws PersistenceException {
    // we will select a list of wl
    String hql = "select distinct wl.assig from " + Worklog.class.getName() + " as wl where (wl.inserted between :stad and :endd) and wl.assig.resource=:res";

    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setTimestamp("stad", filterPeriod.getStartDate());
    oql.getQuery().setTimestamp("endd", filterPeriod.getEndDate());
    oql.getQuery().setEntity("res", this);


    return oql.list();

  }


  public static Resource load(Serializable id) throws FindByPrimaryKeyException {
    return (Resource) PersistenceHome.findByPrimaryKey(Resource.class, id);
  }


  public static Resource loadByRef(String idCodeName) {
    Resource resource = null;
    try {
      resource = Resource.load(idCodeName);
    } catch (PersistenceException p) {
    }
    //test with code
    if (resource == null) {
      try {
        resource = (Resource) PersistenceHome.findUnique(Resource.class, "code", idCodeName);
      } catch (PersistenceException p) {
      }
    }

    //test with name
    if (resource == null) {
      try {
        resource = (Resource) PersistenceHome.findUnique(Resource.class, "name", idCodeName);
      } catch (PersistenceException p) {
      }
    }
    return resource;
  }

  /**
   * for each day the workable millis for that day. If day is holidays -1  if is completely unaivailable millis are -2
   *
   * @param period
   * @return
   */
  public TreeMap<Integer, Long> getWorkablePlan(Period period) throws FindException {
    Tracer.Profiler prof = Tracer.getProfiler("getWorkablePlan");
    TreeMap<Integer, Long> ret = new TreeMap();

    //first read unavail from agenda
    TreeMap<Integer, Long> unavailPerDay = new TreeMap();
    for (Period up : Event.getUnavailabilityPeriodsInPeriodFor(this, period)) {
      int key = DateUtilities.dateToInt(up.getStartDate());
      Long unavailToday = unavailPerDay.get(key);
      if (unavailToday == null)
        unavailToday = 0l;

      unavailPerDay.put(key, unavailToday + up.getDurationInMillis());
    }


    CompanyCalendar cc = new CompanyCalendar(period.getStartDate());
    while (cc.getTime().before(period.getEndDate())) {
      int key = DateUtilities.dateToInt(cc.getTime());

      if (!cc.isWorkingDay()) {
        ret.put(key, -1l); // -1== holydays

        // take care of unavailability in agenda
      } else if (unavailPerDay.containsKey(key)) {
        long unavailToday = unavailPerDay.get(key);
        if (unavailToday >= getWorkDailyCapacity())
          ret.put(key, -2l); // -2 unavailable at all
        else
          ret.put(key, getWorkDailyCapacity() - unavailToday);
      } else {
        ret.put(key, getWorkDailyCapacity());
      }

      cc.add(CompanyCalendar.DATE, 1);
    }

    prof.stop();
    return ret;
  }


  /**
   * @param period
   * @return  for each day the sum of worklog in millis for that day.
   */
  public TreeMap<Integer, Long> getWorkedPlan(Period period) throws FindException {
    Tracer.Profiler proffq = Tracer.getProfiler("getWorkedPlan");

    TreeMap<Integer, Long> ret = new TreeMap<Integer, Long>();

    String hql = "select year(wlp.inserted)*10000+month(wlp.inserted)*100+day(wlp.inserted), sum(wlp.duration) from " + Worklog.class.getName() + " as wlp " +
      "where wlp.inserted>=:pst and wlp.inserted<=:pen and wlp.assig.resource=:res " +
      "group by year(wlp.inserted)*10000+month(wlp.inserted)*100+day(wlp.inserted) ";

    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setParameter("res", this);
    oql.getQuery().setTimestamp("pst", period.getStartDate());
    oql.getQuery().setTimestamp("pen", period.getEndDate());
    List<Object[]> o = oql.list();

    for (Object[] oo : o) {
      int day = ((Number) oo[0]).intValue();
      long dur = ((Number) oo[1]).longValue();
      ret.put(day, dur);

    }
    proffq.stop();
    return ret;
  }


  public long getWorkDailyCapacity() {
    return workDailyCapacity;
  }

  public void setWorkDailyCapacity(long workDailyCapacity) {
    this.workDailyCapacity = workDailyCapacity;
  }

  @Transient
  public String getMnemonicCode() {
    String codeOrId = getId() + "";
    if (JSP.ex(getCode()) && isUnique("code")) {
      codeOrId = getCode();
    }
    return codeOrId;
  }

  @Transient
  public static Resource findResourceByExternalCode(String externalId) {
    Resource res = null;
    try {
      res = (Resource) PersistenceHome.findFirst(Resource.class, "externalCode", externalId);
    } catch (PersistenceException p) {
    }
    return res;
  }

  public boolean hasPermissionFor(User u, Permission p) {
    return PermissionCache.hasPermissionFor(u,this,p);
  }


  public boolean zzhasPermissionForUnCached(User u, Permission p) {
    Person loggedPerson = ((TeamworkOperator) u).getPerson();
    // novità aggiunta il 1/9/2015 P&R
    // check managers: managers can: resource_canRead, resource_assignment_manage, task_assignment_manage
    // e dal 23/5/2016 anche resource_cost_canRead per far vedere il costo della risorsa all'assegnazione senza dover avere un permesso globale
    String prefix = (loggedPerson.getMyManagerIds() != null ? loggedPerson.getMyManagerIds() : "") + loggedPerson.getId() + PerformantNode.SEPARATOR ;
    if ( (u.equals(getMyself()) && ( p.equals(TeamworkPermissions.resource_canRead) ||
            p.equals(TeamworkPermissions.resource_cost_canRead)))  ||
            ((getMyManagerIds()+"").startsWith(prefix)  &&
            ( p.equals(TeamworkPermissions.resource_canRead) ||
            p.equals(TeamworkPermissions.resource_manage)||
            p.equals(TeamworkPermissions.resource_cost_canRead)) ) )
      return true;
    else
      return super.hasPermissionFor(u, p);
  }


  public boolean hasPermissionForUnCached(User u, Permission p) {
    boolean ret=false;
    Person loggedPerson = ((TeamworkOperator) u).getPerson();

    // se il permesso è tra quelli su myself e sono io
    if (RoleTeamwork.getMyselfRole().hasPermissionFor(p))
      ret=loggedPerson.equals(this);

    //se il permesso è tra quelli su manager ed  è il manager
    if (!ret && RoleTeamwork.getManagerRole().hasPermissionFor(p))
      ret=loggedPerson.equals(this.getMyManager());

    if (!ret)
      ret= super.hasPermissionFor(u, p);  //questp fà anche l'iterazione sui dipartimenti

    return ret;
  }



  public JSONObject jsonify() {
    return jsonify(false);
  }

  public JSONObject jsonify(boolean fullLoading) {
    JSONObject ret = super.jsonify();

    ret.element("id", getId());
    ret.element("displayName", getDisplayName());
    ret.element("loadComplete", false);

    List<AnagraphicalData> adds = getAnagraphicalDataOrdered();
    if (adds.size() > 0) {
      ret.element("ads_id", adds.get(0).getId());
      ret.element("email", adds.get(0).getEmail());
      ret.element("mobile", adds.get(0).getMobile());
      ret.element("telephone", adds.get(0).getTelephone());
      ret.element("address", adds.get(0).getAddress());
    }

    ret.element("avatarUrl", bricks.getAvatarImageUrl());

    if (fullLoading) {
      ret.element("loadComplete", true);
      JSONArray ads = new JSONArray();
      for (AnagraphicalData ad : adds)
        ads.add(ad.jsonify());
      ret.element("personalData", ads);

      try {
        JSONArray jsa = new JSONArray();
        for (Assignment ass : getActiveAssignments(null, true))
          jsa.add(ass.jsonify());

        ret.element("assignments", jsa);
      } catch (PersistenceException e) {
      }

    }
    return ret;
  }




  public String getAbstractForIndexing() {
    String ad = "";
    for (AnagraphicalData anagraphicalData : getAnagraphicalDatas()) {
      ad = ad + anagraphicalData.getAbstractForIndexing();
    }

    //recover designer data
    List<DesignerData> datas = DesignerData.getAllInstances(getId(), Company.class.getName());
    StringBuffer bb= new StringBuffer();
    for (DesignerData dd:datas){
      for (String val:dd.getValueMap().values()){
        if (JSP.ex(val))
          bb.append(JSP.w(val)+" ");
      }
    }


    return JSP.w(
      getName() + " R#" + getMnemonicCode() + "#\n" +
        (JSP.ex(getMyself()) ? JSP.w(getMyself().getLoginName()) + "\n" : "") +
        (JSP.ex(getNotes()) ? getNotes() + "\n" : "") +
        (JSP.ex(getJobDescription()) ? getJobDescription() + "\n" : "") +
        (JSP.ex(ad) ? ad + "\n" : "") +
        (JSP.ex(getTags()) ? JSP.w(getTags()) + "\n" : "") +
        (JSP.ex(getMyCostAggregator()) ? getMyCostAggregator().getDisplayName() + "\n" : "") +
        JSP.w(getCustomField1()) + " " +
        JSP.w(getCustomField2()) + " " +
        JSP.w(getCustomField3()) + " " +
        JSP.w(getCustomField4()) + " " +
        JSP.w(getCustomField5()) + " " +
        JSP.w(getCustomField6()) +

        (bb.length() > 0 ? "\n" + JSP.w(bb.toString()) : "")
    );
  }
}
