package com.twproject.task;

import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Person;
import com.twproject.resource.Resource;
import com.twproject.security.RoleTeamwork;
import com.twproject.security.TeamworkPermissions;
import com.twproject.task.financial.Cost;
import com.twproject.task.financial.CostAggregator;
import com.twproject.utilities.TeamworkComparators;
import com.twproject.worklog.Worklog;
import com.twproject.worklog.WorklogPlan;
import com.twproject.worklog.WorklogSupport;
import net.sf.json.JSONObject;
import org.hibernate.Query;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.agenda.Period;
import org.jblooming.designer.DesignerField;
import org.jblooming.messaging.Message;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.messaging.SomethingHappened;
import org.jblooming.ontology.*;
import org.jblooming.operator.Operator;
import org.jblooming.operator.User;
import org.jblooming.oql.OqlQuery;
import org.jblooming.oql.QueryHelper;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.*;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.security.Permission;
import org.jblooming.security.PermissionCache;
import org.jblooming.security.Role;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import javax.persistence.Transient;
import java.io.Serializable;
import java.util.*;


public class Assignment extends SecuredLoggableHideableSupport implements Comparable, HasDenormalizedFields, PermissionCache.PermissionCacheEnabled {

  private String description;
  private Operator owner;
  protected Resource resource;
  private Task task;
  private RoleTeamwork role;

  protected long worklogDone;
  protected long estimatedWorklog;

  // transient
  private Long worklogPlanned = null;

  private Date assignmentDate;

  private String customField1;
  private String customField2;
  private String customField3;
  private String customField4;
  private String customField5;
  private String customField6;

  //read only
  private Set<WorklogSupport> worklogSupports = new HashSet();

  //read only
  private Set<AssignmentPriority> priorities = new TreeSet<AssignmentPriority>(new TeamworkComparators.AssignmentPriorityComparator());

  private Set<Cost> costs = new TreeSet<Cost>(new PlatformComparators.CreationDateComparator());


  private boolean counted = false;
  private Date countingStartedAt;
  private String activity = ACTIVITY_ALL_IN_ONE;
  private boolean induceWorklog = true;
  private int risk;

  private boolean enabled = true;

  private double hourlyCost;
  private double budget;

  private CostAggregator costCenter;

  private String externalCode;

  public static final String ACTIVITY_ALL_IN_ONE = "ACTIVITY_ALL_IN_ONE";
  public static final String ACTIVITY_REPEATED_IN_TIME = "ACTIVITY_REPEATED_IN_TIME";

  public Assignment() {
  }

  public static List<String> getActivityTypes() {

    List<String> acts = new ArrayList<String>();
    acts.add(ACTIVITY_ALL_IN_ONE);
    acts.add(ACTIVITY_REPEATED_IN_TIME);

    return acts;
  }

  public Resource getResource() {
    return resource;
  }

  public void setResource(Resource resource) {
    this.resource = resource;
  }

  /**
   * @return in millis
   */
  public long getEstimatedWorklog() {
    return estimatedWorklog;
  }

  public void setEstimatedWorklog(long estimatedWorklog) {
    this.estimatedWorklog = estimatedWorklog;
  }

  public Date getAssignmentDate() {
    return assignmentDate;
  }

  public void setAssignmentDate(Date assignmentDate) {
    this.assignmentDate = assignmentDate;
  }

  public Task getTask() {
    return task;
  }

  public void setTask(Task task) {
    this.task = task;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public Date getLastModified() {
    return lastModified;
  }

  public void setLastModified(Date lastModified) {
    this.lastModified = lastModified;
  }

  public String getName() {
    return (getDescription() != null ? getDescription() : getTask().getName());
  }

  public boolean isCounted() {
    return counted;
  }

  public void setCounted(boolean counted) {
    this.counted = counted;
    setCountingStartedAt(new Date());
  }

  public Date getCountingStartedAt() {
    return countingStartedAt;
  }

  public void setCountingStartedAt(Date countingStartedAt) {
    this.countingStartedAt = countingStartedAt;
  }

  public String getActivity() {
    return activity;
  }

  public void setActivity(String activity) {
    this.activity = activity;
  }

  public boolean isInduceWorklog() {
    return induceWorklog;
  }

  public void setInduceWorklog(boolean induceWorklog) {
    this.induceWorklog = induceWorklog;
  }

  public int getRisk() {
    return risk;
  }

  public void setRisk(int risk) {
    this.risk = risk;
  }

  public RoleTeamwork getRole() {
    return role;
  }

  public void setRole(RoleTeamwork role) {
    this.role = role;
  }

  public Operator getOwner() {
    return owner;
  }

  public void setOwner(Operator owner) {
    this.owner = owner;
  }

  public boolean isEnabled() {
    return enabled;
  }

  public void setEnabled(boolean enabled) {
    this.enabled = enabled;
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

  /**
   * @return in MILLIS
   */
  public long getWorklogDone() {
    return worklogDone;
  }

  public void setWorklogDone(long worklogDone) {
    this.worklogDone = worklogDone;
  }

  @Transient
  public double getCostDone(boolean includeWorklog) {
    double t=0;
    if (includeWorklog)
      t = getWorklogDone() * getHourlyCost() / CompanyCalendar.MILLIS_IN_HOUR;
    for (Cost c : getCosts()) {
      t += c.getRealCost();
    }
    return t;
  }

  @Transient
  public double getCostEstimated(boolean includeWorklog) {
    double t=0;
    if (includeWorklog)
      t = getEstimatedWorklog() * getHourlyCost() / CompanyCalendar.MILLIS_IN_HOUR;
    return t + getBudget();
  }


  /**
   * @param worklogStatusId "" means all, "0" status == null, "n" the status id
   * @return in MILLIS
   */
  public long getWorklogDone(String worklogStatusId) {
    QueryHelper qh = new QueryHelper("select sum(wklg.duration) from " + Worklog.class.getName() + " as wklg");
    qh.addOQLClause("wklg.assig = :assig", "assig", this);

    if (JSP.ex(worklogStatusId)) {
      if (worklogStatusId.equals("0"))
        qh.addOQLClause("wklg.status is null");
      else if (JSP.ex(worklogStatusId)) {
        qh.addOQLClause("wklg.status.id = :sts", "sts", Integer.parseInt(worklogStatusId));
      }
    }
    try {
      return (Long) qh.toHql().uniqueResult();
    } catch (PersistenceException e) {
      return 0;
    }
  }

  /**
   * @param period
   * @return a Pair with number of worklog in period, total worklog in millis
   * @throws FindException
   */
  public Pair<Integer, Long> getWorklogDone(Period period) {

    String hql = "select count(wklg.id), sum(wklg.duration) from " + Worklog.class.getName() + " as wklg where wklg.assig = :assig and wklg.inserted between :st and :en ";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setEntity("assig", this);
    oql.getQuery().setTimestamp("st", period.getStartDate());
    oql.getQuery().setTimestamp("en", period.getEndDate());
    Object[] oa = (Object[]) oql.getQuery().uniqueResult();
    Pair<Integer, Long> ret = new Pair(0, 0L);
    if (oa != null && oa[0] != null && oa[1] != null) {
      ret.first = ((Number) oa[0]).intValue();
      ret.second = ((Number) oa[1]).longValue();
    }
    return ret;
  }


  public String getDisplayNameWithTask() {
    String result =  task.getDisplayName() + " ['" + role.getName() + "'" + (JSP.ex(getDescription()) ? " " + getDescription() : "") + "]";
    return result;
  }

  public String getDisplayNameWithResource() {
    String result =  resource.getDisplayName() + " ['" + role.getName() + "'" + (JSP.ex(getDescription()) ? " " + getDescription() : "") + "]";
    return result;
  }

  public String getDisplayNameFull() {
    return task.getDisplayName() + ": " + resource.getDisplayName()+" [" + role.getCode() + "]";
  }

  public String getDisplayName() {
    return getDisplayNameFull();
  }


  private String getDisplayName(String as) {
    String result = getTask().getDisplayName() + " [" + as + " '" + role.getName() + "'" + (JSP.ex(getDescription()) ? " " + getDescription() : "") + "]";
    return result;
  }


  public Set getPriorities() {
    return priorities;
  }

  private void setPriorities(Set priorities) {
    this.priorities = priorities;
  }

  public void addPriorityInMemory(AssignmentPriority priority) {
    priorities.add(priority);
  }

  public void removePriority(long stamanPrest) { //solo quella per il millisecondo passato
    if (priorities != null && priorities.size()>0){
      //first remove the today's one
      String hql = "delete from " + AssignmentPriority.class.getName() + " where assignment=:ass and cutPoint=:staman";
      OqlQuery oql = new OqlQuery(hql);
      oql.getQuery().setEntity("ass", this);
      oql.getQuery().setLong("staman", stamanPrest);
      oql.getQuery().executeUpdate();
      //remove from memory too
      for (AssignmentPriority ap : priorities) {
        if (ap.getCutPoint()==stamanPrest){
          priorities.remove(ap);
          break;
        } else if (ap.getCutPoint()<stamanPrest)
          break;
      }
    }
  }


  public int getPriorityAtTime(long time) {
    int ret = AssignmentPriority.PRIORITY_LOW;
    AssignmentPriority ap = getAssignmentPriorityAtTime(time);
    if (ap != null)
      ret = ap.getPriority()<AssignmentPriority.PRIORITY_LOW?AssignmentPriority.PRIORITY_LOW:ap.getPriority();
    return ret;
  }

  public AssignmentPriority getAssignmentPriorityAtTime(long time) { //todo si può ottimizzare partendo dal fondo, dato che di solito si cerca la priorità ad oggi
    AssignmentPriority ret = null;
    if (priorities != null && priorities.size() > 0) {
      for (AssignmentPriority ap : priorities) {
        if (ap.getCutPoint() <= time) {
          ret = ap;
        } else
          break;
      }
    }
    return ret;
  }

  @Transient
  public static boolean hasCustomField() {
    return DesignerField.hasCustomField("ASSIGNMENT_CUSTOM_FIELD_", 6);
  }


  public Worklog closeCounter(PageState pageState) throws PersistenceException {
    long elapsed = System.currentTimeMillis() - countingStartedAt.getTime();
    setCounted(false);
    setCountingStartedAt(null);
    store();
    Worklog w=null;
    //if (elapsed>60000) {  // non registra worklog se meno di un minuto

      w = new Worklog();
      w.setIdAsNew();
      w.setAssig(this);
      w.setDuration(elapsed);
      w.setInserted(new Date());
      if (pageState != null) {
        SessionState sessionState = pageState.getSessionState();
        String action = (String) sessionState.getAttribute("ISSUE_WORKLOG_ACTION");
        w.setAction(JSP.w(action));
        sessionState.setAttribute("ISSUE_WORKLOG_ACTION", null);
      }
      // w.store(); // è chiamato sull'Assignment
   // }
    return w;
  }


  public Assignment getClonedInstance(Task newTask) {
    Assignment a = new Assignment();
    a.setIdAsNew();
    a.setRole(getRole());
    a.setResource(getResource());
    a.setTask(newTask);
    a.setEnabled(isEnabled());
    a.setOwner(getOwner());
    a.setDescription(getDescription());
    a.setEstimatedWorklog(getEstimatedWorklog());
    a.setAssignmentDate(new Date());
    a.setActivity(getActivity());
    a.setInduceWorklog(isInduceWorklog());
    a.setRisk(getRisk());
    a.setHourlyCost(getHourlyCost());
    return a;
  }

  public void setHourlyCost(double hourlyCost) {
    this.hourlyCost = hourlyCost;
  }

  public double getHourlyCost() {
    return hourlyCost;
  }


  public double getBudget() {
    return budget;
  }

  public void setBudget(double budget) {
    this.budget = budget;
  }

  public CostAggregator getCostCenter() {
    return costCenter;
  }

  public void setCostCenter(CostAggregator costCenter) {
    this.costCenter = costCenter;
  }

  public void store(PersistenceContext pc) throws StoreException {
    super.store(pc);
    getTask().getAssignments().add(this); // in order to update inverse

    // enqueue denormalized fields computation
    getTask().markAsDirty();
  }


  @Transient
  public void remove(PersistenceContext pc) throws RemoveException {
    Task task = getTask();
    task.getAssignments().remove(this);
    super.remove(pc);
    // enqueue denormalized fields computation
    task.markAsDirty();
  }

  @Override
  public void recomputeDenormalizedFields(PersistenceContext pc) {
    updateWorklogDone();
    checkOverwork(0);
    if (getTask()!=null)
      getTask().markAsDirty();
  }


  public boolean hasPermissionFor(User u, Permission p) {
    return PermissionCache.hasPermissionFor(u,this,p);
  }


  public boolean hasPermissionForUnCached(User u, Permission p) {
    boolean ret = false;
    // aggiunta il 8/1/16 R&S per far si che il manager possa gestire le assegnazioni dei suoi
    if (!ret&& resource!=null)
      ret=resource.hasPermissionFor(u,p); // questa controlla administrato, permessi globali e il manager/myself

    if (!ret && task != null)
      ret = hasUserPermissionOnAssignments(task,(TeamworkOperator)u, p);


    return ret;
  }


  // controlla solo se l'operatore ha un'assegnazione con quel permesso sul task o sulla catena dei padri
  private boolean hasUserPermissionOnAssignments(Task task, TeamworkOperator operator, Permission p) {

    Person myself = operator.getPerson();
    boolean result = false;
    for (Assignment assignment : task.getAssignments()) {
      //case directly on the assig and also
      //case in which assig is on department and I am in department
      if (assignment.getResource().isPersonIn(myself)) {
        Role role = assignment.getRole();
        if (role != null && role.hasPermissionFor(p)) {
          result = true;
          break;
        }
      }
    }
    if (!result && task.getParent()!=null)
      result= hasUserPermissionOnAssignments(task.getParent(), operator, p);

    return result;
  }

  @Transient
  public List<Worklog> getWorklogs() throws FindException {
    String hql = "select wlp from " + Worklog.class.getName() + " as wlp where wlp.assig = :assig order by wlp.inserted desc";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setEntity("assig", this);
    return oql.list();
  }

  @Transient
  public long getWorklogCount() throws FindException {
    String hql = "select count(wlp.id) from " + Worklog.class.getName() + " as wlp where wlp.assig = :assig";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setEntity("assig", this);
    return (Long) oql.uniqueResult();
  }


  private void updateWorklogDone() {
    long wd = getWorklogDone("");
    setWorklogDone(wd); // all computed
  }




  public void checkOverwork(long additionalWork)  {
    //overwork check
    try {
      long worklogDoneAndSaved = getWorklogDone() + additionalWork;
      if (getEstimatedWorklog() > 0 && getEstimatedWorklog() < worklogDoneAndSaved) {

        // Just in case multiple save: actually does not have much sense, a SomethingHappened object is removed every few seconds
        String hql = "select ev from " + SomethingHappened.class.getName() + " as ev where ev.eventType = :eventType and ev.theClass=:theClass and ev.identifiableId=:idId";
        Query query = null;
        query = new OqlQuery(hql).getQuery();
        query.setString("eventType", Task.Event.TASK_WORKLOG_OVERFLOW + "");
        query.setString("theClass", Task.class.getName());
        query.setString("idId", getTask().getId() + "");

        List<SomethingHappened> li = query.list();
        if (li == null || li.size() == 0) {

          SomethingHappened change = new SomethingHappened();
          change.setIdAsNew();
          change.setEventType(Task.Event.TASK_WORKLOG_OVERFLOW + "");
          change.setMessageTemplate(Task.Event.TASK_WORKLOG_OVERFLOW + "_MESSAGE_TEMPLATE");
          change.setIdentifiable(getTask());
          change.getMessageParams().put("assig", getResource().getDisplayName());
          change.getMessageParams().put("estWklg", DateUtilities.getMillisInHoursMinutes(getEstimatedWorklog()));
          change.getMessageParams().put("totWklg", DateUtilities.getMillisInHoursMinutes(worklogDoneAndSaved));
          Resource resource = (Resource) ReflectionUtilities.getUnderlyingObject(getResource());
          if (resource instanceof Person)
            change.setWhoCausedTheEvent(((Person) resource).getMyself());

          PageSeed ps = new PageSeed(ApplicationState.serverURL + "/applications/teamwork/task/taskAssignmentList.jsp");
          ps.setCommand(Commands.FIND);
          ps.addClientEntry("TASK_ID", getTask().getId());
          ButtonLink edit = new ButtonLink(ps);
          edit.label = getDisplayNameWithTask();
          change.setLink(edit.toPlainLink());
          change.store();
        }
      }
    }catch (Throwable t){
      throw new PlatformRuntimeException(t);
    }
  }


  /**
   * @param mainObjectId serializable but is a String
   * @return
   * @throws FindByPrimaryKeyException
   */
  public static Assignment load(Serializable mainObjectId) throws FindByPrimaryKeyException {
    return (Assignment) PersistenceHome.findByPrimaryKey(Assignment.class, mainObjectId);
  }

  public long getWorklogEstimatedFromIssues() {

    long result = 0;
    if (!isNew()) {

      // compute estimate wl by planned issues
      // WARNING: estimation by issue is diveden by the number of assignment for the resource non the same task. Otherwise the estimation will result multipled.
      // hanck becouse on isseu there is no assignment but task and resource.

      OqlQuery oqlQuery = new OqlQuery("select count(*) from " + Assignment.class.getName() + " as ass where ass.resource=:res and ass.task=:tsk");
      oqlQuery.getQuery().setParameter("res", getResource());
      oqlQuery.getQuery().setParameter("tsk", getTask());
      Long numOfAssig = (Long) oqlQuery.uniqueResultNullIfEmpty();
      if (numOfAssig == null)
        numOfAssig = 1L;


      String hql = "select sum(issue.estimatedDuration) from " + Issue.class.getName() + " as issue where issue.task=:tsk and issue.assignedTo = :ast";
      OqlQuery oql = new OqlQuery(hql);
      oql.getQuery().setEntity("ast", getResource());
      oql.getQuery().setEntity("tsk", getTask());
      Object o = oql.uniqueResultNullIfEmpty();
      if (o != null)
        result = (Long) o / numOfAssig;
    }
    return result;
  }


  /**
   * @return a map containing estimation by "plan" and planned issues
   *  Long[0]= estimation byplan
   *  Long[1]= 1-> plan has note 0-> no note
   * @throws FindException
   */
  public Map<Integer, PlannedWork> getWorklogPlan(Period period, boolean considerIssues) throws FindException {
    Tracer.Profiler prof = Tracer.getProfiler("getWorklogPlan");
    Tracer.Profiler profp = Tracer.getProfiler("getWorklogPlan plan");

    Map<Integer, PlannedWork> dayDuration = new Hashtable();

    String hql = "select year(wlp.inserted)*10000+month(wlp.inserted)*100+day(wlp.inserted), wlp.duration, wlp.action from " + WorklogPlan.class.getName() +
            " as wlp where wlp.assig.id = :assig and wlp.inserted>=:pst and wlp.inserted<=:pen order by wlp.inserted";

    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setParameter("assig", this.getId());
    oql.getQuery().setTimestamp("pst", period.getStartDate());
    oql.getQuery().setTimestamp("pen", period.getEndDate());
    List<Object[]> o = oql.list();

    for (Object[] oo : o) {
      int day = ((Number) oo[0]).intValue();
      PlannedWork pw= new PlannedWork();
      pw.loadByPlan= ((Number) oo[1]).longValue();
      pw.hasNotes= JSP.ex((String) oo[2]) ;
      dayDuration.put(day, pw);
    }

    profp.stop();

    if (considerIssues) {

      // compute estimate wl by planned issues
      // WARNING: estimation by issue is diveded by the number of assignment for the resource on the same task. Otherwise the estimation will result multipled.
      // hack because on issue there is no assignment but task and resource.

      Tracer.Profiler profi = Tracer.getProfiler("getWorklogPlan issues");

      OqlQuery oqlQuery = new OqlQuery("select count(*) from " + Assignment.class.getName() + " as ass where ass.resource.id=:res and ass.task.id=:tsk");
      oqlQuery.getQuery().setParameter("res", getResource().getId());
      oqlQuery.getQuery().setParameter("tsk", getTask().getId());
      Long numOfAssig = (Long) oqlQuery.uniqueResultNullIfEmpty();
      if (numOfAssig == null)
        numOfAssig = 1L;


      hql = "select year(iss.shouldCloseBy)*10000+month(iss.shouldCloseBy)*100+day(iss.shouldCloseBy), sum(iss.estimatedDuration) from " + Issue.class.getName() + " as iss " +
              "where iss.shouldCloseBy is not null and iss.estimatedDuration>0 and iss.assignedTo.id=:res and iss.task.id=:tsk " +
              "and iss.shouldCloseBy>=:pst and iss.shouldCloseBy<=:pen " +
              "group by year(iss.shouldCloseBy)*10000+month(iss.shouldCloseBy)*100+day(iss.shouldCloseBy) ";

      oql = new OqlQuery(hql);
      oql.getQuery().setParameter("res", getResource().getId());
      oql.getQuery().setParameter("tsk", getTask().getId());
      oql.getQuery().setTimestamp("pst", period.getStartDate());
      oql.getQuery().setTimestamp("pen", period.getEndDate());
      o = oql.list();

      for (Object[] oo : o) {
        int day = ((Number) oo[0]).intValue();
        long dur = (((Number) oo[1]).longValue()) / numOfAssig;

        PlannedWork pdur = dayDuration.get(day);
        if (pdur == null) {
          pdur= new PlannedWork();
          pdur.loadByPlan=-1; // non c'era piano bisogna dirglielo
        }
        pdur.loadByIssue= dur;

        dayDuration.put(day,pdur);
      }

      profi.stop();

    }
    prof.stop();

    return dayDuration;
  }


  @Transient
  /**
   * returna a TreeMap with key = day int  and 
   */
  public TreeMap<Integer, Long> getWorklogDoneByDay(Period period) {
    Tracer.Profiler prof = Tracer.getProfiler("getWorklogDoneByDay");
    // calculate workdone
    String hql = "select year(wklg.inserted)*10000+month(wklg.inserted)*100+day(wklg.inserted), sum(wklg.duration) from " + Worklog.class.getName() + " as wklg where wklg.assig.id = :assig and wklg.inserted between :st and :en " +
            "group by year(wklg.inserted)*10000+month(wklg.inserted)*100+day(wklg.inserted)";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setParameter("assig", this.id);
    oql.getQuery().setTimestamp("st", period.getStartDate());
    oql.getQuery().setTimestamp("en", period.getEndDate());
    List<Object[]> list = oql.getQuery().list();
    TreeMap<Integer, Long> ret = new TreeMap<Integer, Long>();
    for (Object[] o : list) {
      ret.put((Integer) o[0], (Long) o[1]);
    }

    prof.stop();
    return ret;
  }

  @Transient
  /**
   * returna a TreeMap with key = day int  and
   */
  public TreeMap<Integer, Long> getWorklogDoneByDay() {
    Tracer.Profiler prof = Tracer.getProfiler("getWorklogDoneByDay");
    // calculate workdone
    String hql = "select year(wklg.inserted)*10000+month(wklg.inserted)*100+day(wklg.inserted), sum(wklg.duration) from " + Worklog.class.getName() + " as wklg where wklg.assig.id = :assig " +
            "group by year(wklg.inserted)*10000+month(wklg.inserted)*100+day(wklg.inserted)";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setParameter("assig", this.id);
    List<Object[]> list = oql.getQuery().list();
    TreeMap<Integer, Long> ret = new TreeMap<Integer, Long>();
    for (Object[] o : list) {
      ret.put((Integer) o[0], (Long) o[1]);
    }

    prof.stop();
    return ret;
  }

  @Transient
  // WARNING: get the cached value after the first call even if startFrom is changed
  public long getWorklogPlanned(Date startFrom) throws FindException {
    if (worklogPlanned == null) {
      String hql = "select sum(wlp.duration) from " + WorklogPlan.class.getName() + " as wlp where wlp.assig = :assig and wlp.inserted>=:startFrom";

      OqlQuery oql = new OqlQuery(hql);
      oql.getQuery().setEntity("assig", this);
      oql.getQuery().setTimestamp("startFrom", startFrom);
      Long t = (Long) oql.uniqueResultNullIfEmpty();
      if (t == null)
        worklogPlanned = 0L;
      else
        worklogPlanned = t;
    }
    return worklogPlanned.longValue();
  }


  private Set<WorklogSupport> getWorklogSupports() {
    return worklogSupports;
  }

  private void setWorklogSupports(Set<WorklogSupport> worklogSupports) {
    this.worklogSupports = worklogSupports;
  }


  public String getExternalCode() {
    return externalCode;
  }

  public void setExternalCode(String externalCode) {
    this.externalCode = externalCode;
  }


  public static List<Assignment> sortAssignmentsByPriority(Date when, List<Assignment> assigsToBeSorted) {
    Collections.sort(assigsToBeSorted, new TeamworkComparators.AssignmentByPriority(when));
    return assigsToBeSorted;
  }


  public JSONObject jsonify() {
    JSONObject ret = super.jsonify();
    ret.element("id", getId());
    ret.element("description", getDescription());
    ret.element("role", getRole().getName());
    ret.element("roleCode", getRole().getCode());
    ret.element("roleId", getRole().getId());
    ret.element("resName", getResource().getDisplayName());
    ret.element("resAvatarUrl", getResource().bricks.getAvatarImageUrl());
    ret.element("resId", getResource().getId());
    //ret.element("taskName", getTask().getDisplayName());
    ret.element("taskName", getTask().getName());
    ret.element("taskCode", getTask().getCode());
    ret.element("taskId", getTask().getId());
    ret.element("estimated", getEstimatedWorklog());
    ret.element("done", getWorklogDone());
    ret.element("isCounted", isCounted());
    ret.element("countingStartedMillis", getCountingStartedAt() == null ? null : getCountingStartedAt().getTime());
    return ret;
  }


  public void generateAssignmentMessages(Operator from) throws PersistenceException {
    TeamworkOperator to = getResource().getMyself();

    if (to == null && getResource().getMyManager() != null) { //try with the manager: in case of company/department the manager is notified instead of the resource
      to = getResource().getMyManager().getMyself();
    }
    if (to!=null) { //tanto se è null il messaggio non si manda comunque
      Set<String> medias = to.getPreferredMediaOrDefault(MessagingSystem.Media.STICKY);
      for (String media : medias)
        generateAssignmentMessage(from, MessagingSystem.Media.valueOf(media));
    }
  }

  public void generateAssignmentMessage(Operator from, MessagingSystem.Media media) throws PersistenceException {
    Operator to = getResource().getMyself();

    if (to == null && getResource().getMyManager() != null) { //try with the manager: in case of company/department the manager is notified instead of the resource
      to = getResource().getMyManager().getMyself();
    }


    if (to != null) {
      String language = to.getLanguage();
      Message message = new Message();
      message.setFromOperator(from);
      message.setToOperator(to);
      message.setMedia(media.toString());
      message.setDefaultExpires();
      message.setSubject(I18n.getLabel("ASSIGNMENT_NOTIFICATION", language) + ": " + JSP.limWr(this.getTask().getDisplayName(), 40) + "\n");
      PageSeed ps = new PageSeed(ApplicationState.serverURL + "/applications/teamwork/task/taskAssignmentList.jsp");

      ps.command = Commands.EDIT;
      ps.addClientEntry("TASK_ID", this.getTask().getId());
      ps.mainObjectId = this.getId();
      ButtonLink editLink = ButtonLink.getTextualInstance(I18n.getLabel("VIEW_ASSIGNMENT", language), ps);
      if (media == MessagingSystem.Media.RSS) {
        String link = ps.toLinkToHref();
        message.setLink(link);
      } else
        message.setLink(editLink.toPlainLink());

      String body="";
      //se hai i permessi di lettura o se la custom feature è attiva
      if (I18n.isActive("CUSTOM_FEATURE_ALWAYS_SHOW_TASK_PATH")||task.getRoot().hasPermissionFor(to, TeamworkPermissions.task_canRead))
        body+="<div style='font-size:11px'>"+ getTask().getPath(" / ", false)+"</div>";
      body+=getDisplayName(I18n.getLabel("ASSIG_AS", language));
      message.setMessageBody(body);
      message.setReceived(new Date());
      message.store();
    }
  }


  public static void generateDefaultSubscriptions(Assignment assig) throws PersistenceException {

    PageSeed trick = new PageSeed("fake");
    SerializedMap<String, String> subm = assig.getRole().getDefaultSubscriptions();
    if (subm != null) {
      for (String k : subm.keySet()) {
        trick.addClientEntry(k, subm.get(k));
      }
    }
    TeamworkOperator subscriber = assig.getResource().getMyself();
    Task task = assig.getTask();

    //remove existing
    task.bricks.removeListeners(subscriber);

    //create new
    boolean tnd = trick.getEntry("TASK_NOTIFY_DESC").checkFieldValue();

    // register listeners
    for (Task.Event event : Task.Event.values()) {
      String mediaSubscribed = MessagingSystem.mediaSubscribed(event + "_", trick);
      if (mediaSubscribed.length() > 0 && !task.bricks.isSomeAncestorListeningAndPropagatingForTheEvent(subscriber, event)) {
        task.bricks.createListener(event, tnd, subscriber, mediaSubscribed);
      }
    }

  }

  public static void notifyAndSubscribe(Assignment ass, TeamworkOperator loggedOperator) throws PersistenceException {
    // generate assignment event
    if (ass.getResource() instanceof Person) {
      Person assigneeP = (Person) ass.getResource();
      TeamworkOperator myself = assigneeP.getMyself();
      if (myself != null) {
        // notify the assignee
        ass.generateAssignmentMessages(loggedOperator);
        // subscribe the assignee
        generateDefaultSubscriptions(ass);
      }
    }
  }

  public Set<Cost> getCosts() {
    return costs;
  }

  private void setCosts(Set costs) {
    this.costs = costs;
  }

  /**
   * @return estimated, real
   */
  public double computeCosts() {
    double real = 0;
    for (Cost c : getCosts()) {
      real += c.getRealCost();
    }
    return real;
  }


  @Transient
  public static Assignment getCountedAssignment(TeamworkOperator logged) {
    String hql = "select a from " + Assignment.class.getName() + " as a where a.counted=true and a.resource=:res";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setEntity("res",logged.getPerson());
    return (Assignment) oql.uniqueResultNullIfEmpty();
  }

  /*
     *  Long[0]= estimation byplan
   *  Long[1]= 1-> plan has note 0-> no note

   */
  public static class PlannedWork {
    public long loadByPlan=-1;   // 0=non devi lavorare per quel giorno  -1= non c'è lavoro da piano   xx= milisec. da lavorare
    public long loadByIssue=0;
    public boolean hasNotes=false;

    // considera che il piano è settato a -1 quando non c'è pianificazione
    public long getTotalPlanned(){
      return (loadByPlan<0?0:loadByPlan)+loadByIssue;
    }
  }
}
