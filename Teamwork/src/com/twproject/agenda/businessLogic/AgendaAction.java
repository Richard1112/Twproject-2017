package com.twproject.agenda.businessLogic;

import com.twproject.agenda.Event;
import com.twproject.agenda.IcalUtilities;
import com.twproject.document.TeamworkDocument;
import com.twproject.operator.TeamworkOperator;
import com.twproject.rank.Hit;
import com.twproject.resource.Person;
import com.twproject.resource.Resource;
import com.twproject.resource.ResourceBricks;
import com.twproject.meeting.DiscussionPoint;
import com.twproject.meeting.Meeting;
import com.twproject.security.TeamworkPermissions;
import net.fortuna.ical4j.model.property.Method;
import net.sf.json.JSONArray;
import net.sf.json.JSONObject;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.agenda.EventType;
import org.jblooming.agenda.Period;
import org.jblooming.agenda.ScheduleSupport;
import org.jblooming.messaging.Listener;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.ontology.SerializedList;
import org.jblooming.ontology.businessLogic.DeleteHelper;
import org.jblooming.operator.Operator;
import org.jblooming.oql.OqlQuery;
import org.jblooming.oql.QueryHelper;
import org.jblooming.page.HibernatePage;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.security.Permission;
import org.jblooming.utilities.CollectionUtilities;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.ActionSupport;
import org.jblooming.waf.ActionUtilities;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.constants.OperatorConstants;
import org.jblooming.waf.constants.SecurityConstants;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.display.DataTable;
import org.jblooming.waf.html.display.Paginator;
import org.jblooming.waf.html.input.ScheduleComposer;
import org.jblooming.waf.html.input.SmartCombo;
import org.jblooming.waf.html.table.ListHeader;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.state.PersistentSearch;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.RestState;

import java.text.ParseException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class AgendaAction extends ActionSupport {

  public TeamworkOperator logged;
  public Event event;


  public AgendaAction(RestState pageState) throws PersistenceException {
    super(pageState);
    this.logged = (TeamworkOperator) pageState.getLoggedOperator();
  }

  public void cmdPrepareDefaultFind() throws PersistenceException {
    //search for default filter
    if (restState.getCommand() == null)
      if (!PersistentSearch.feedFromDefaultSearch("EVENT", restState)) {
        restState.addClientEntry("VALIDITY_PERIOD_START", ">-2w");
        restState.addClientEntry("VALIDITY_PERIOD_END", "<2w");
        restState.addClientEntry("FILTER", "NONE");
      }
  }


  public void cmdFind() throws PersistenceException {
    Person loggedPerson = Person.getLoggedPerson(restState);

    cmdPrepareDefaultFind();

    boolean somethingSearched;

    String hql = "select event, schedule, event.author, event.author.name from " + Event.class.getName() + " as event join event.schedule as schedule ";

    QueryHelper qhelp = new QueryHelper(hql);
    boolean recoveredFromSavedFilter = PersistentSearch.feedFromSavedSearch(restState);
    somethingSearched = recoveredFromSavedFilter;

    String filter = restState.getEntry("OBJECT_DESCRIPTION").stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addQBEORClauses(filter, qhelp.getOrElement("event.summary", "summary", QueryHelper.TYPE_CHAR), qhelp.getOrElement("event.description", "description", QueryHelper.TYPE_CHAR));
      // neal coso si cerchi per descrizione non si devono vedere quelli privati
      qhelp.addOQLClause("event.personal=false or event.author=:me","me",loggedPerson);
      somethingSearched=true;
    }

    ClientEntry ce = restState.getEntry("EVENT_TYPE");
    if (ce.isFilled())
      try {
        int anInt = ce.intValue();
        qhelp.addQBEClause("event.type.id", "typeId", anInt + "", QueryHelper.TYPE_INT);
        somethingSearched = true;
      } catch (ActionException e) {
      } catch (ParseException e) {
      }

    qhelp.setDistinct();
    qhelp.addJoinAlias("event.targets target");

    filter = restState.getEntry("TARGET").stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addOQLClause("target.id= :targetId", "targetId", filter);
      somethingSearched = true;
    }


    ce = restState.getEntry("AUTHOR");
    if (ce.isFilled()){
      try {
        int anInt = ce.intValue();
        qhelp.addQBEClause("event.author.id", "authorId", anInt + "", QueryHelper.TYPE_CHAR);
        somethingSearched = true;
      } catch (ActionException e) {
      } catch (ParseException e) {
      }
    }

    filter = restState.getEntry("LOCATION").stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addQBEClause("event.location", "location", filter, QueryHelper.TYPE_CHAR);
      somethingSearched = true;
    }

    somethingSearched= somethingSearched ||ActionUtilities.addQBEClause("VALIDITY_PERIOD_START", "schedule.start", "schstart", qhelp, QueryHelper.TYPE_DATE, restState);
    somethingSearched= somethingSearched ||ActionUtilities.addQBEClause("VALIDITY_PERIOD_END", "schedule.end", "schend", qhelp, QueryHelper.TYPE_DATE, restState);

    ce = restState.getEntry("IS_MEETING");
    if (ce.checkFieldValue()) {
      qhelp.addQueryClause("event.meeting != null");
      somethingSearched = true;

      filter = restState.getEntry("BOARD").stringValueNullIfEmpty();
      if (filter != null) {
        qhelp.addJoinAlias("event.meeting meeting");
        qhelp.addOQLClause("meeting.board.id = :boardId", "boardId", filter);
        somethingSearched = true;
      }
    }

    filter = restState.getEntry("FILTER").stringValueNullIfEmpty();
    if (filter != null) {
      if ("WORK".equals(filter)) {
        qhelp.addQBEClause("target.id", "idTarget", loggedPerson.getId().toString(), QueryHelper.TYPE_CHAR);
        qhelp.addOQLClause(" (event.personal = :personal)", "personal", Boolean.FALSE);
        somethingSearched = true;
      } else  if ("PERSONAL".equals(filter)) {
        qhelp.addQBEClause("event.author.id", "idAuthor", loggedPerson.getId().toString(), QueryHelper.TYPE_CHAR);
        qhelp.addOQLClause(" (event.personal = :personal)", "personal", Boolean.TRUE);
        somethingSearched = true;
      } else if ("AUTHOR".equals(filter)) {
        qhelp.addQBEClause("event.author.id", "idAuthor", loggedPerson.getId().toString(), QueryHelper.TYPE_CHAR);
        somethingSearched = true;
      } else if ("ONLY_MINE".equals(filter)) {
        qhelp.addQBEClause("target.id", "idTarget", loggedPerson.getId().toString(), QueryHelper.TYPE_CHAR);
        qhelp.addToHqlString("and size(event.targets) = 1");
        somethingSearched = true;
      } else if ("NONE".equals(filter)) {
        if (restState.getEntry("TARGET").stringValueNullIfEmpty() == null) {
          qhelp.addQBEClause("target.id", "idTarget", loggedPerson.getId().toString(), QueryHelper.TYPE_CHAR);
          somethingSearched = true;
        }
      } else if ("UNAVAIL".equals(filter)) {
        if (restState.getEntry("TARGET").stringValueNullIfEmpty() == null) {
          qhelp.addOQLClause("event.unavailability = :truth", "truth", Boolean.TRUE);
          somethingSearched = true;
        }
      }
    }

    //add security clauses
    qhelp.addToHqlString(somethingSearched? " and (": " where (");
    ResourceBricks.addSecurityClauses("event.author",false, qhelp, CollectionUtilities.toList(TeamworkPermissions.resource_canRead), logged, true, true);
    qhelp.addToHqlString(" or ");
    ResourceBricks.addSecurityClauses("target", false, qhelp, CollectionUtilities.toList(TeamworkPermissions.resource_canRead), logged, true, true);
    qhelp.addToHqlString(")");


    DataTable.orderAction(qhelp, "AGELST", restState, "schedule.start desc");

    if (!somethingSearched) {
      somethingSearched = true;
    }

    if (somethingSearched) {
      restState.setPage(HibernatePage.getHibernatePageInstance(qhelp.toHql().getQuery(), Paginator.getWantedPageNumber(restState), Paginator.getWantedPageSize("AGELH", restState)));

    }
  }


  public void cmdFindTarget() throws PersistenceException {
    Event event;
    if (!PersistenceHome.NEW_EMPTY_ID.equals(restState.getMainObjectId())) {
      event = (Event) PersistenceHome.findByPrimaryKey(Event.class, restState.getMainObjectId());
    } else {
      event = new Event();
      event.setIdAsNew();
    }
    restState.setMainObject(event);

  }


  public void cmdRemoveMe() throws PersistenceException {
    Person loggedPerson = Person.getLoggedPerson(restState);
    Event event = (Event) PersistenceHome.findByPrimaryKey(Event.class, restState.getMainObjectId());
    event.getTargets().remove(loggedPerson);
    event.store();

    String hql = "from " + Listener.class.getName() + " as listen where listen.owner = :owner and listen.theClass = :theClass and listen.identifiableId = :identifiableId";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setEntity("owner", loggedPerson.getMyself());
    oql.getQuery().setString("theClass", Event.class.getName());
    oql.getQuery().setString("identifiableId", event.getId().toString());
    Listener delendo = (Listener) oql.uniqueResultNullIfEmpty();
    if (delendo != null)
      delendo.remove();

    // send notify by e-mail
    TeamworkOperator author = event.getAuthor().getMyself();
    if (author != null) {
      if (Fields.TRUE.equals(author.getOption(OperatorConstants.SEND_EVENT_BY_ICAL))) {
        Set<Resource> toTheOwnerOnly = new HashSet();
        toTheOwnerOnly.add(event.getAuthor());
        net.fortuna.ical4j.model.Calendar ical = IcalUtilities.getICal(IcalUtilities.getDeclineVEvent(event, loggedPerson), Method.REPLY);
        IcalUtilities.sendIcalByMailQueued(toTheOwnerOnly, event.getSummary(), ical,event.getId()+"");
      }
    }
  }

  public void cmdSave() throws PersistenceException, ActionException {
    boolean isNew = false;
    boolean invalidEntry = false;
    Event event;
    if (restState.mainObjectId==null || PersistenceHome.NEW_EMPTY_ID.equals(restState.getMainObjectId())) {
      event = new Event();
      event.setIdAsNew();
      isNew = true;
    } else {
      event = (Event) PersistenceHome.findByPrimaryKey(Event.class, restState.getMainObjectId());

      //test security
      boolean canSave=logged.equals(event.getAuthor().getMyself()) || !event.isPersonal()&&logged.hasPermissionAsAdmin();
      if (!canSave)
        throw new SecurityException("No permission to edit this agenda event.");

    }
    restState.setMainObject(event);
    String value = restState.getEntryAndSetRequired("AGENDA_SUMMARY").stringValue();
    if (value != null)
      event.setSummary(value);
    else
      invalidEntry = true;

    Person loggedPerson = Person.getLoggedPerson(restState);
    if (isNew) {
      event.setAuthor(loggedPerson);
    }

    event.setDescription(restState.getEntry("AGENDA_DESCRIPTION").stringValueNullIfEmpty());
    event.setLocation(restState.getEntry("LOCATION").stringValueNullIfEmpty());

    event.setPersonal(restState.getEntry("IS_PERSONAL").checkFieldValue());
    event.setUnavailability(restState.getEntry("IS_UNAVAILABLE").checkFieldValue());

    value = restState.getEntry("TYPE").stringValueNullIfEmpty();

    if (value != null) {
      EventType t = (EventType) PersistenceHome.findByPrimaryKey(EventType.class, value);
      event.setType(t);
    }
    boolean isRemainder = restState.getEntry("IS_REMINDER").checkFieldValue();
    if (isRemainder)
      event.setReminder(true);
    else
      event.setReminder(false);

    List<Resource> resources= ResourceBricks.fillWorkGroup(restState);


    if (event.getTargetSize() > 0) {
      for (Resource resource : new HashSet<Resource>(event.getTargets())) {
        if (!resources.contains(resource))
          event.getTargets().remove(resource);
      }
    }
    for (Resource resource: resources) {
      event.getTargets().add(resource);
    }

    long alertTimeValue = 0;
    try {
      alertTimeValue = restState.getEntryOrDefault("ALERT_BEFORE_X_HOURS").timeValueInMillis();
    } catch (ParseException e) {
    }

    // save rimasugl of meeting just in case
    Meeting meeting = event.getMeeting();
    if (meeting != null) {
      ActionUtilities.setIdentifiable(restState.getEntry("MEETING_BOARD"), meeting, "board");


      //save discussion points
      String dipsS = restState.getEntry("dips").stringValueNullIfEmpty();
      if (JSP.ex(dipsS)) {
        JSONArray disp = JSONArray.fromObject(dipsS);
        for (Object o : disp) {
          JSONObject jdip = (JSONObject) o;

          DiscussionPoint dip = null;
          if (!jdip.getString("id").startsWith("new"))
            dip = DiscussionPoint.load(jdip.getString("id"));
          if (dip == null) {
            dip = new DiscussionPoint();
            dip.setIdAsNew();
            dip.setMeeting(meeting);
            dip.setOwner(logged);
          }

          //create a fake pageSeed in order to use ActionUtilities
          PageSeed ps= new PageSeed("fake");
          for (Object k:jdip.keySet()){
            ps.addClientEntry(k.toString(),jdip.get(k));
          }

          ActionUtilities.setDurationInMillis(ps.getEntry("timeScheduled"), false, dip, "timeScheduled");
          ActionUtilities.setIdentifiable(ps.getEntry("type"), dip, "type");
          ActionUtilities.setIdentifiable(ps.getEntry("status"), dip, "status");
          ActionUtilities.setIdentifiable(ps.getEntry("task"), dip, "task");
          ActionUtilities.setIdentifiable(ps.getEntry("lead"), dip, "lead");
          ActionUtilities.setString(ps.getEntry("title"), dip, "title");
          ActionUtilities.setInt(ps.getEntry("orderBy"), dip, "orderBy");
          ActionUtilities.setString(ps.getEntry("minute"), dip, "minute");

          if (dip.getLead()!=null){
            if (!event.getTargets().contains(dip.getLead())){
              event.getTargets().add(dip.getLead());
              event.store();
            }
          }

          // save docs
          dip.setDocuments(new SerializedList());  // always reset
          if (jdip.has("docIds") && dip.getTask()!=null && dip.getTask().hasPermissionFor(logged,TeamworkPermissions.document_canRead)){
            JSONArray docIds = jdip.getJSONArray("docIds");
            for (int i=0;i<docIds.size();i++){
              String docId=docIds.getString(i);
              //check if docId comes from the good task
              for (TeamworkDocument td: (Set<TeamworkDocument>)dip.getTask().getDocuments()) {
                if (td.getId().equals(docId)) {
                  dip.getDocuments().add(docId);
                  break;
                }
              }

            }
          }


          dip.store();
        }
      }
      meeting.store(); //per aggiornare l'indice dei dip
    }

    if (!invalidEntry) {

      ScheduleSupport schedule = ScheduleComposer.getSchedule("SCHEDULE", restState);
      if (schedule != null) {
        ScheduleSupport oldSchedule = event.getSchedule();
        if (oldSchedule != null) {
          //se le date non sono uguali, resetto exceptions
          if (!oldSchedule.getPeriod().toString().equals(schedule.getPeriod().toString()))
            event.setExceptions(null);

          event.getSchedule().remove();
        }
        schedule.store();
        event.setSchedule(schedule);
      } else
        invalidEntry = true;
    }

    if (!invalidEntry) {
      TeamworkOperator logged = (TeamworkOperator) restState.getLoggedOperator();
      event.store();
      String happyParty = "";
      for (Resource r : event.getTargets()) {
        happyParty = happyParty + r.getDisplayName() + " ";
      }

      Set<Resource> subscribedToClient = new HashSet();
      for (Resource p : event.getTargets()) {
        TeamworkOperator teamworkOperator = p.getMyself();
        if (teamworkOperator != null) {
          String sendOPT = teamworkOperator.getOption(OperatorConstants.SEND_EVENT_BY_ICAL);
          if (sendOPT != null && !Fields.FALSE.equals(sendOPT))
            subscribedToClient.add(p);
        }
      }

      if (subscribedToClient.size() > 0) {
        net.fortuna.ical4j.model.Calendar ical = IcalUtilities.getICal(IcalUtilities.getInviteVEvent(event, alertTimeValue), Method.REQUEST);
        IcalUtilities.sendIcalByMailQueued(subscribedToClient, event.getSummary(), ical,event.getId()+"");
      }

      Hit.getInstanceAndStore(event, logged, isNew ? .3 : .2);
      Hit.getInstanceAndStore(event, event.getTargets(), isNew ? .2 : .1);
    }

    restState.addClientEntry("FOCUS_MILLIS", (event.getSchedule() != null ? event.getSchedule().getStartDate().getTime() : System.currentTimeMillis() + ""));
    if (invalidEntry) {
      throw new ActionException("SOMETHING WRONG");
    }
  }

  public static void createAlertListener(Event event, long alertTimeValue) throws PersistenceException {

    for (Resource r : event.getTargets()) {

      TeamworkOperator myself = r.getMyself();
      if (myself != null) {
        //recreate listener
        String hql = "from " + Listener.class.getName() + " as listen where listen.owner = :owner and listen.theClass = :theClass and listen.identifiableId = :identifiableId";
        OqlQuery oql = new OqlQuery(hql);
        oql.getQuery().setEntity("owner", myself);
        oql.getQuery().setString("theClass", Event.class.getName());
        oql.getQuery().setString("identifiableId", event.getId().toString());
        Listener delendo = (Listener) oql.uniqueResultNullIfEmpty();
        if (delendo != null)
          delendo.remove();

        if (alertTimeValue > 0) {
          Listener l = new Listener();
          l.setIdAsNew();
          l.setIdentifiable(event);
          l.setMedia(StringUtilities.unSplit(myself.getPreferredMediaOrDefault(MessagingSystem.Media.STICKY),","));
          l.setEventType("AGENDA_EVENT_APPROACHING");
          l.setValidityEnd(event.getSchedule().getEndDate());
          l.setOwner(myself);
          l.getAdditionalParams().put("alertTime", alertTimeValue + "");
          l.getAdditionalParams().put("lastTimeAlerted", "0");
          l.store();

        }
      }
    }
  }


  public void cmdAdd() throws PersistenceException {
    Person author = Person.getLoggedPerson(restState);

    Event event = new Event();
    event.setIdAsNew();
    restState.setMainObject(event);
    ScheduleSupport period;
    if (JSP.ex(restState.getEntry("SCHEDULE"))) {
      period = ScheduleComposer.getSchedule("SCHEDULE", restState);
    } else {

      long startMillis = restState.getEntry("FOCUS_MILLIS").longValueNoErrorNoCatchedExc();
      startMillis = startMillis <= 0 ? System.currentTimeMillis() : startMillis;
      restState.addClientEntry("FOCUS_MILLIS",startMillis);

      //set hour to now
      CompanyCalendar cc = new CompanyCalendar();
      cc.setAndGetTimeToDayStart();
      long h = System.currentTimeMillis() - cc.getTime().getTime();
      h = ((long) (h / (CompanyCalendar.MILLIS_IN_MINUTE * 15))) * CompanyCalendar.MILLIS_IN_MINUTE * 15;

      cc.setTimeInMillis(startMillis);
      startMillis = cc.setAndGetTimeToDayStart().getTime() + h;

      long endMillis = startMillis + CompanyCalendar.MILLIS_IN_MINUTE * 30;

      period = new Period(startMillis, endMillis);
    }
    event.setSchedule(period);
    ScheduleComposer.make("SCHEDULE", period, restState);

    restState.addClientEntry("AGENDA_DESCRIPTION", restState.getEntry("description").stringValueNullIfEmpty());
    restState.addClientEntry("AGENDA_SUMMARY", restState.getEntry("summary").stringValueNullIfEmpty());
    restState.addClientEntry("LOCATION", restState.getEntry("location").stringValueNullIfEmpty());
  }

  public Event editNoMake() throws FindByPrimaryKeyException, StoreException {
    event = (Event) PersistenceHome.findByPrimaryKey(Event.class, restState.getMainObjectId());
    restState.setMainObject(event);
    Hit.getInstanceAndStore(event, logged, .1);
    return event;
  }

  public void cmdEdit() throws PersistenceException {
    event = editNoMake();
    make(event);
  }

  public void cmdGuess() throws PersistenceException, ActionException {
    event = null;
      if (event == null)
        event = Event.load(restState.getMainObjectId());

      if (event != null) {
        restState.mainObjectId = event.getId();
        editNoMake();
        if (!event.hasPermissionFor(logged, new Permission("FAKE_PERMISISON")))
          throw new ActionException("REF_PERMISSION_LACKING");
        make(event);
      } else {
        throw new ActionException("REF_NOT_FOUND");
      }

  }


  private void make(Event event) throws PersistenceException {
    makeEvent(event);
    if (event.getSchedule() != null) {
      ScheduleComposer.make("SCHEDULE", event.getSchedule(), restState);
    }
  }

  private void makeEvent(Event event) {
    boolean hasPermissionToRead=(event.getAuthor() != null && event.getAuthor().equals(logged.getPerson()))||(event.isPersonal() && event.getTargets().contains(logged.getPerson()))||(!event.isPersonal() );

    restState.addClientEntry("LOCATION", hasPermissionToRead?event.getLocation(): I18n.get("IS_PERSONAL"));
    restState.addClientEntry("FOCUS_MILLIS", (event.getSchedule() != null ? event.getSchedule().getStartDate().getTime() : ""));
    restState.addClientEntry("AGENDA_SUMMARY", hasPermissionToRead?event.getSummary(): I18n.get("IS_PERSONAL"));
    restState.addClientEntry("AGENDA_DESCRIPTION", hasPermissionToRead?event.getDescription(): I18n.get("IS_PERSONAL"));

    restState.addClientEntry("IS_PERSONAL", (event.isPersonal() ? Fields.TRUE : ""));
    restState.addClientEntry("IS_REMINDER", (event.isReminder() ? Fields.TRUE : ""));
    restState.addClientEntry("IS_UNAVAILABLE", (event.isUnavailability() ? Fields.TRUE : ""));

    if (event.getType() != null) {
      restState.addClientEntry("TYPE", event.getType().getId());
      restState.addClientEntry("TYPE" + SmartCombo.TEXT_FIELD_POSTFIX, event.getType().getDescription());
    }

    String hql = "from " + Listener.class.getName() + " as listen where listen.owner = :owner and listen.theClass = :theClass and listen.identifiableId = :identifiableId";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setEntity("owner", event.getAuthor().getMyself());
    oql.getQuery().setString("theClass", Event.class.getName());
    oql.getQuery().setString("identifiableId", event.getId().toString());
    Listener l = (Listener) oql.uniqueResultNullIfEmpty();
    if (l != null) {
      MessagingSystem.makeMedias(l.getMedia(), "ALERT_BEFORE_X_HOURS_", restState);
      restState.addClientEntry("ALERT_BEFORE_X_HOURS", DateUtilities.getMillisInHoursMinutes(Long.parseLong((l.getAdditionalParams().get("alertTime")))));
    }

    if (event.getMeeting() != null)
      restState.addClientEntry("MEETING_BOARD", event.getMeeting().getBoard());

    String ids="";
    for (Resource res : event.getTargets()){
      ids+=(ids.length()==0?"":",")+res.getId();
    }
    restState.addClientEntry("WG_IDS",ids);

  }

  public void cmdDelete() throws PersistenceException {
    Event delenda = Event.load(restState.getMainObjectId());
    // send notify by e-mail
    if (logged.hasPermissionAsAdmin() || logged.equals(delenda.getAuthor().getMyself())) {
      Set<Resource> subscribedToCLient = new HashSet();
      for (Resource p : delenda.getTargets()) {
        TeamworkOperator teamworkOperator = p.getMyself();
        if (teamworkOperator != null) {
          String sendOPT = teamworkOperator.getOption(OperatorConstants.SEND_EVENT_BY_ICAL);
          if (sendOPT != null && !Fields.FALSE.equals(sendOPT))
            subscribedToCLient.add(p);
        }
      }

      if (delenda.getSchedule() != null && subscribedToCLient.size() > 0) {
        net.fortuna.ical4j.model.Calendar ical = IcalUtilities.getICal(IcalUtilities.getCancelVEvent(delenda), Method.CANCEL);
        IcalUtilities.sendIcalByMailQueued(subscribedToCLient, delenda.getSummary(), ical,delenda.getId()+"");
      }
      restState.setMainObject(delenda);
      DeleteHelper.cmdDelete(delenda, restState);
    }
  }

}