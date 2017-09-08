package com.twproject.agenda;

import com.opnlb.fulltext.Indexable;
import com.twproject.meeting.Meeting;
import com.twproject.resource.Person;
import com.twproject.resource.Resource;
import com.twproject.operator.TeamworkOperator;
import com.twproject.security.TeamworkArea;
import com.twproject.utilities.TeamworkComparators;
import org.apache.lucene.analysis.core.StopAnalyzer;
import org.hibernate.Query;
import org.hibernate.annotations.Type;
import org.hibernate.annotations.ForeignKey;
import org.hibernate.search.annotations.*;
import org.hibernate.search.bridge.builtin.StringBridge;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.agenda.EventType;
import org.jblooming.agenda.Period;
import org.jblooming.agenda.ScheduleSupport;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.logging.Sniffable;
import org.jblooming.ontology.Identifiable;
import org.jblooming.ontology.SecuredLoggableSupport;
import org.jblooming.operator.User;
import org.jblooming.operator.Operator;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.FindException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.security.Permission;
import org.jblooming.security.Area;
import org.jblooming.security.PermissionCache;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.settings.I18n;

import javax.persistence.*;
import java.io.Serializable;
import java.util.*;

import net.sf.json.JSONObject;
import net.sf.json.JSONArray;

/**
 * Models the notion of event in time.Events are persistent
 * and collectable by groups. They tipically concern a human operator,
 * but as notion they cover also automation ends.
 */
@Indexed(index = "fulltext")
@Entity
@Table(name = "twk_agendaevent")
public class Event extends SecuredLoggableSupport implements Sniffable, Indexable, Comparable, PermissionCache.PermissionCacheEnabled {

  private Set<Resource> targets = new HashSet<Resource>();
  private Person author;
  private ScheduleSupport schedule;
  private String location;
  private boolean personal;
  private boolean reminder;
  private boolean unavailability;

  private EventType type;
  private int status;
  private String summary;
  private String description;
  // refernces used for creating links on the event display
  private Set<String> references = new HashSet<String>();
  private Meeting meeting;
  private String icalId;

  private boolean isFromExternalCalendar=false;

  private String exceptions = "";               // comma separated list of milliseconds pointing to the first millisecond of the "deleted" event


  public Event() {
  }


  public Event(Serializable aeId) {
    setId(aeId);
  }

  @Id
  @Type(type = "string")
  @Column(length = 15)
  @DocumentId
  @FieldBridge(impl = StringBridge.class)
  public Serializable getId() {
    return super.getId();
  }


  public void setReminder(boolean b) {
    this.reminder = b;
  }

  public boolean isReminder() {
    return reminder;
  }

  @ManyToMany
  @ForeignKey(name = "fk_ag_tar_ev", inverseName = "fk_ag_tar_res")
  @JoinTable(name = "twk_agenda_tar", joinColumns = {@JoinColumn(name = "event")}, inverseJoinColumns = {@JoinColumn(name = "elt")})
  public Set<Resource> getTargets() {
    return targets;
  }

  public void setTargets(Set<Resource> targets) {
    this.targets = targets;
  }

  @ElementCollection
  @JoinTable(name = "twk_event_references", joinColumns = @JoinColumn(name = "event_id"))
  @Column(name = "elt", nullable = false)
  @ForeignKey(name = "fk_eventRef_event")
  private Set<String> getReferences() {
    return references;
  }

  private void setReferences(Set<String> references) {
    this.references = references;
  }

  @ManyToOne(cascade = CascadeType.REMOVE)
  @ForeignKey(name = "fk_event_schedule")
  @org.hibernate.annotations.Index(name = "idx_event_schedule")
  public ScheduleSupport getSchedule() {
    return schedule;
  }

  /**
   * Permits to calculate the time intersections of it.
   */
  public void setSchedule(ScheduleSupport schedule) {
    this.schedule = schedule;
  }

  @ManyToOne
  @ForeignKey(name = "fk_event_eventType")
  @org.hibernate.annotations.Index(name = "idx_event_eventType")
  public EventType getType() {
    return type;
  }

  public void setType(EventType type) {
    this.type = type;
  }

  public int getStatus() {
    return status;
  }

  /**
   * Not in use yet.
   */
  public void setStatus(final int status) {
    this.status = status;
  }

  @Column(length = 2000)
  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public String getLocation() {
    return location;
  }

  /**
   * Where it will take place.
   */
  public void setLocation(String location) {
    this.location = location;
  }


  public boolean isPersonal() {
    return personal;
  }

  /**
   * Whether it is visible to non-owners.
   */
  public void setPersonal(boolean personal) {
    this.personal = personal;
  }

  @ManyToOne
  @ForeignKey(name = "fk_agendaevent_author")
  @org.hibernate.annotations.Index(name = "idx_agendaevent_author")
  public Person getAuthor() {
    return author;
  }

  public void setAuthor(Person author) {
    this.author = author;
  }

  @Transient
  public Date getLastModified() {
    return lastModified;
  }

  @Transient
  public void setLastModified(Date lastModified) {
    this.lastModified = lastModified;
  }


  public void addReference(Identifiable reference) {
    getReferences().add(generateName(reference));
  }


  public void removeReference(Identifiable reference) {
    getReferences().remove(generateName(reference));
  }

  public String getSummary() {
    return summary;
  }

  public void setSummary(String summary) {
    this.summary = summary;
  }

  public String getIcalId() {
    return icalId;
  }

  public void setIcalId(String icalId) {
    this.icalId = icalId;
  }

  public boolean isUnavailability() {
    return unavailability;
  }

  public void setUnavailability(boolean unavailability) {
    this.unavailability = unavailability;
  }


  @Column(length = 2000)
  public String getExceptions() {
    return exceptions;
  }

  public void setExceptions(String exceptions) {
    this.exceptions = exceptions;
  }


  @ManyToOne(cascade = CascadeType.REMOVE)
  @ForeignKey(name = "fk_event_meeting")
  @org.hibernate.annotations.Index(name = "idx_event_meeting")
  public Meeting getMeeting() {
    return meeting;
  }

  public void setMeeting(Meeting meeting) {
    this.meeting = meeting;
  }

  @Transient
  public Iterator getReferencesIterator() {
    return references.iterator();
  }

  @Transient
  public String getDisplayName() {
    return JSP.w(getSummary()) + (!JSP.ex(getSummary()) ? JSP.limWr(getDescription(), 20) : "");
  }


  @Transient
  public void store(PersistenceContext pc) throws StoreException {
    super.store(pc);
    if (getIcalId() == null)
      setIcalId("TW_" + getId());
  }

  @Transient
  public Iterator getReferencesInstancesIterator() {
    Set<Identifiable> refs = new HashSet<Identifiable>();
    if (references != null && references.size() > 0) {
      for (Iterator<String> iterator = new HashSet<String>(references).iterator(); iterator.hasNext();) {
        String name = iterator.next();
        String[] spl = name.split("__");
        String id = spl[0];
        String clazz = spl[1];
        try {
          refs.add(PersistenceHome.findByPrimaryKey((Class<? extends Identifiable>) Class.forName(clazz), id));
        } catch (FindByPrimaryKeyException e) {
          references.remove(name);
        } catch (ClassNotFoundException e) {
          throw new PlatformRuntimeException(e);
        }

      }
    }
    return refs.iterator();
  }

  @Transient
  public static Event newInstanceWithScheduleNull(Event e) {

    Event newEvent = new Event();
    newEvent.targets = e.targets;
    newEvent.author = e.author;
    newEvent.schedule = null;
    newEvent.location = e.location;
    newEvent.personal = e.personal;
    newEvent.type = e.type;
    newEvent.status = e.status;
    newEvent.description = e.description;
    newEvent.lastModified = e.lastModified;
    newEvent.references = e.references;
    return newEvent;
  }

  // --------------------------------------------------------- UTILITY METHODS ---------------------------------------------------


  /**
   * @param resource
   * @param period   if null get today
   * @param refine   If true loop events in order to remove the event doesen't match
   * @return a list of candidate events. Do not checks recursive events, but the edge only.
   *         The list is order by schedule.start: this do not means it is order correctly ;-)
   * @throws FindException
   */
  @Transient
  public static List<Event> getEventsInPeriodFor(Resource resource, Period period, boolean showAuthored, boolean refine) throws FindException {

    if (period == null) {
      period = Period.getDayPeriodInstance(new Date());
    }
    String hql = "select event from " + Event.class.getName() + " as event left join event.targets as target where " +
            "(target=:logged " + (showAuthored ? "or author=:logged" : "") + ") " +
            "and not (event.schedule.end < :start or event.schedule.start > :end) order by event.schedule.start";
    OqlQuery oql = new OqlQuery(hql);
    Query query = oql.getQuery();
    query.setParameter("logged", resource);
    query.setParameter("start", period.getValidityStartDate());
    query.setParameter("end", period.getValidityEndDate());
    List<Event> result = (List<Event>) oql.list();


    if (refine) { // remove non-exactly-matching events
      List<Event> refined = new ArrayList<Event>();
      for (Event event : result) {
        if (!refined.contains(event)) {
          ScheduleSupport schedule = event.getSchedule();
          if (schedule.overlap(period)) {
            refined.add(event);
          }
        }
      }
      return refined;
    } else {
      return result;
    }
  }

  /**
   */
  @Transient
  public static List<Period> getUnavailabilityPeriodsInPeriodFor(Resource resource, Period period) throws FindException {

    if (period == null) {
      period = Period.getDayPeriodInstance(new Date());
    }
    String hql = "select event from " + Event.class.getName() + " as event left join event.targets as target where " +
            "(target=:logged and event.unavailability=true)" +
            "and not (event.schedule.end < :start or event.schedule.start > :end) order by event.schedule.start";
    OqlQuery oql = new OqlQuery(hql);
    Query query = oql.getQuery();
    query.setParameter("logged", resource);
    query.setParameter("start", period.getValidityStartDate());
    query.setParameter("end", period.getValidityEndDate());
    List<Event> events = (List<Event>) oql.list();

    List<Period> result = new ArrayList();


    // first explode recurrent and/or trim larger periods
    for (Event event : events) {
      List<Period> uperiods = event.getSchedule().getPeriods(period, true,event.getExceptions());
      if (uperiods!=null){
        for (Period u:uperiods){
          //loop for every day in examining period intersecting the working time
          CompanyCalendar cc = new CompanyCalendar(u.getValidityStartDate());
          while (cc.getTimeInMillis()<u.getValidityEndTime()){
            Period workDay= Period.getDayPeriodInstance(cc.getTime());
            Period matchUnav = u.intersection(workDay);
            if (matchUnav!=null)
              result.add(matchUnav);
            cc.add(CompanyCalendar.DATE,1);
          }
        }
      }
    }

    return result;
  }


  /**
   * @param resource
   * @param period   if null get today
   * @return a Map<Period,Event> list of atomic event as periods for the matching period and person. It checks for recursive events.
   *         The list is order by schedule.start: this do not means it is order correctly ;-)
   * @throws FindException
   */
  @Transient
  public static TreeMap<Period, Event> getPeriodsInPeriodFor(Resource resource, Period period, boolean showAuthored) throws FindException {
    if (period == null) {
      period = Period.getDayPeriodInstance(new Date());
    }
    List<Event> result = getEventsInPeriodFor(resource, period, showAuthored, true);

    TreeMap<Period, Event> disgraned = new TreeMap<Period, Event>();

    for (Event event : result) {
      ScheduleSupport schedule = event.getSchedule();
      List<Period> sgran = schedule.getPeriods(period, false,event.getExceptions());
      for (Period p : sgran) {
        disgraned.put(p, event);
      }
    }
    return disgraned;
  }

  @Transient
  public static List<Event> getFilteredEventsInPeriodWithCollisionFor(List<Resource> resources, Period period, int typeId,
                                                                      boolean showWork, boolean isOlnyMine, boolean showAuthored, boolean showTargets,
                                                                      boolean isPersonal, boolean unavailability) throws PersistenceException {
    if (period == null) {
      period = Period.getDayPeriodInstance(new Date());
    }
    String hql = "select distinct event from " + Event.class.getName() + " as event join event.targets as target where ";
    hql += " (not (event.schedule.end < :start or event.schedule.start > :end)) "; //order by event.schedule.start

    if (showTargets)
      hql += " and ( target in (:myList) )";
    if (showWork)
      hql += " and ( target in (:myList) and personal=:isPersonal)";
    if (showAuthored)
      hql += " and ( author in (:myList) )";
    //if(showTargets && showAuthored)
    //  sqlSelect+=")";
    if (unavailability)
      hql += " and ( unavailability=:unavailability )";

    if (isPersonal)
      hql += " and ( author in (:myList) and personal=:isPersonal )";
    else if (isOlnyMine)
      hql += " and ( target in (:myList) and size(event.targets) = 1) ";
    if (typeId > 0)
      hql += " and ( event.type.id=:typeId )";

    //avoid reminders  commentato il 21/5 La Cate ci maledirà
    /*if (resources.size() > 1) {
      hql += " and ( event.reminder=false)";
    }*/

    OqlQuery oql = new OqlQuery(hql);
    Query query = oql.getQuery();
    if (showAuthored || showTargets || isOlnyMine || isPersonal || showWork)
      query.setParameterList("myList", resources);
    if (isPersonal || showWork)
      query.setBoolean("isPersonal", isPersonal);
    if (unavailability)
      query.setBoolean("unavailability", unavailability);
    if (typeId > 0)
      query.setParameter("typeId", typeId);
    query.setTimestamp("start", period.getValidityStartDate());
    query.setTimestamp("end", period.getValidityEndDate());


    List<Event> result = (List<Event>) oql.list();
    Collections.sort(result, new TeamworkComparators.EventPeriodComparator());
    return result;
  }

  @Transient
  public static List<Event> getMeetingsInPeriodFor(Resource resource, Period period, boolean showAuthored, boolean refine) throws FindException {
    if (period == null) {
      period = Period.getDayPeriodInstance(new Date());
    }
    String hql = "select event from " + Event.class.getName() + " as event left join event.targets as target join event.meeting.discussionPoints as dp where " +
            "(target=:logged " + (showAuthored ? "or author=:logged" : "") + " or dp.lead = :logged) " +
            "and not (event.schedule.end < :start or event.schedule.start > :end) order by event.schedule.start";
    OqlQuery oql = new OqlQuery(hql);
    Query query = oql.getQuery();
    query.setParameter("logged", resource);
    query.setParameter("start", period.getValidityStartDate());
    query.setParameter("end", period.getValidityEndDate());
    List<Event> result = (List<Event>) oql.list();


    if (refine) { // remove non-exactly-matching events
      List<Event> refined = new ArrayList<Event>();
      for (Event event : result) {
        if (!refined.contains(event)) {
          ScheduleSupport schedule = event.getSchedule();
          if (schedule.overlap(period)) {
            refined.add(event);
          }
        }
      }
      return refined;
    } else {
      return result;
    }

  }


  @Transient
  public String getAbstractForIndexing() {
    return JSP.w(
            JSP.w(getSummary()) +  " E#"+getId()+"#\n" +
                    JSP.w(getLocation()) + "\n" +
                    JSP.w(getDescription()) + "\n" +
              (getSchedule()==null?"":JSP.w(getSchedule().getScheduleDescription("")))
    );
  }

  @Transient
  @Fields({
          @Field(name = "fullcontent", analyzer = @Analyzer(impl = StopAnalyzer.class)),
          @Field(name = "content")
  })
  private String getContentForIndexing() {
    return getAbstractForIndexing();
  }

  /**
   * Careful: this is a fake implementation: it is meant to be used only for checking read permissions
   */

  public boolean hasPermissionFor(User u, Permission p) {
    return PermissionCache.hasPermissionFor(u, this, p);
  }

  @Transient
  public boolean hasPermissionForUnCached(User u, Permission p) {
    if (!isPersonal())
      return true;

    if (getAuthor() != null && getAuthor().getMyself() != null && getAuthor().getMyself().equals(u))
      return true;
    else
      return false;
  }

  @Transient
  private String generateName(Identifiable reference) {
    return reference.getId() + "__" + ReflectionUtilities.deProxy(reference.getClass().getName());
  }

  @Transient
  public int getTargetSize() {
    int ret = 0;
    if (targets != null)
      ret = targets.size();
    return ret;
  }

  @Transient
  public String getName() {
    return getSummary();
  }

  public static Event load(Serializable mainObjectId) throws FindByPrimaryKeyException {
    return (Event) PersistenceHome.findByPrimaryKey(Event.class, mainObjectId);
  }

  @IndexedEmbedded
  @Transient
  public Area getArea() {
    Area ret=null;

    if (getAuthor()!=null)
      ret= getAuthor().getArea();
    else {
      //in caso di estrema emergenza prende un'area a caso !!!
      OqlQuery oqlQuery = new OqlQuery("from " + TeamworkArea.class.getName());
      oqlQuery.getQuery().setMaxResults(1);
      List<TeamworkArea> list = oqlQuery.getQuery().list();
      if (JSP.ex(list))
        ret = list.get(0);
    }
    return ret;
  }

  public JSONObject jsonify() {
    return jsonify(null);
  }


  public JSONObject jsonify(PageState pageState) {
    JSONObject ret = super.jsonify();

    ret.element("id", getId());
    ret.element("authorId", getAuthor().getId());
    ret.element("authorName", getAuthor().getDisplayName());

    //questi due sono alimentati di default, ma sovrascritti se si passa il logged in base ai permessi
    ret.element("summary",getSummary());
    ret.element("description",getDescription());


    ret.element("location",getLocation());
    ret.element("isPersonal",isPersonal());
    ret.element("isReminder",isReminder());
    ret.element("isUnavailability",isUnavailability());


    ret.element("isMeeting",getMeeting()!=null);
    ret.element("icalId",getIcalId());

    JSONArray ress= new JSONArray();
    for (Resource res:getTargets()){
      JSONObject jres = new JSONObject();
      jres.element("resId",res.getId());
      jres.element("resName",res.getDisplayName());
      jres.element("resAvatarUrl",res.bricks.getAvatarImageUrl());
      ress.add(jres);
    }
    ret.element("targets",ress);
    ret.element("schedule",getSchedule().jsonify());

    ret.element("type",getType()!=null?getType().getDescription():null);
    ret.element("isExternal",isFromExternalCalendar());
    ret.element("exceptions", getExceptions());

    TeamworkOperator loggedOperator=null;
    if (pageState!=null)
      loggedOperator = (TeamworkOperator) pageState.getLoggedOperator();


    if (loggedOperator!=null ) {
      Person res = loggedOperator.getPerson();
      boolean isAuthor = res.equals(getAuthor());
      boolean isInvolved = getTargets().contains(res);

      ret.element("canManage", isAuthor && !isFromExternalCalendar());
      ret.element("isInvolved", isInvolved);

      ret.element("summary", isPersonal() && !isInvolved ? I18n.get("IS_PERSONAL") : getSummary());
      ret.element("description", isPersonal() && !isInvolved ? "" : getDescription());
    }

    return ret;
  }

  /**
   * @deprecated use getAuthor().getMyself - this is not persistent 
   */
  @Transient
  public Operator getOwner() {
    return getAuthor().getMyself();
  }


  @Transient
  public boolean isFromExternalCalendar() {
    return isFromExternalCalendar;
  }

  public void setFromExternalCalendar(boolean fromExternalCalendar) {
    isFromExternalCalendar = fromExternalCalendar;
  }
}




