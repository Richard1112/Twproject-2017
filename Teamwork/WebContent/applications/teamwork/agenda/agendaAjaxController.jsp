<%@ page import="com.twproject.agenda.AgendaBricks, com.twproject.agenda.Event, com.twproject.agenda.IcalUtilities, com.twproject.agenda.PeriodEvent, com.twproject.agenda.businessLogic.AgendaAction, com.twproject.meeting.DiscussionPoint,
                 com.twproject.operator.TeamworkOperator, com.twproject.rank.Hit, com.twproject.resource.Person, com.twproject.resource.Resource, com.twproject.resource.ResourceBricks, com.twproject.task.Assignment,
                 com.twproject.task.Issue, com.twproject.task.Task, com.twproject.task.TaskBricks, net.fortuna.ical4j.model.property.Method, net.sf.json.JSONArray, net.sf.json.JSONObject, org.hibernate.Query,
                 org.jblooming.PlatformRuntimeException, org.jblooming.agenda.CompanyCalendar, org.jblooming.agenda.Period, org.jblooming.agenda.ScheduleSupport, org.jblooming.ontology.businessLogic.DeleteHelper,
                  org.jblooming.oql.OqlQuery, org.jblooming.oql.QueryHelper, org.jblooming.persistence.exceptions.PersistenceException, org.jblooming.utilities.JSP, org.jblooming.waf.JSONHelper,
                  org.jblooming.waf.SessionState, org.jblooming.waf.constants.Fields, org.jblooming.waf.constants.OperatorConstants, org.jblooming.waf.constants.SecurityConstants,
                   org.jblooming.waf.exceptions.ActionException, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, org.jblooming.waf.view.RestState, java.util.*, com.twproject.security.TeamworkPermissions, org.jblooming.remoteFile.Document, com.twproject.document.TeamworkDocument" %><%

  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  JSONHelper jsonHelper = new JSONHelper();
  JSONObject json = jsonHelper.json;

  AgendaAction action = new AgendaAction(pageState);
  try {
    // -------------------------------------------- GET  EVENTS --------------------------------------------------------------------------------
    if ("GETEVENTS".equals(pageState.command)) {

      getEvents(pageState, json);

      // -------------------------------------------- GET TASK/ISSUES --------------------------------------------------------------------------------
    } else if ("GETTASKISS".equals(pageState.command)) {
      getTaskIssueEvents(pageState, json);


      // -------------------------------------------- MICRO SAVE EVENT --------------------------------------------------------------------------------
    } else if ("MICSAVE".equals(pageState.command)) {
      Event event = Event.load(pageState.getEntry("id").intValueNoErrorCodeNoExc() + "");

      boolean isNew = false;
      boolean invalidEntry = false;
      Person loggedPerson = Person.getLoggedPerson(pageState);

      //create a new event
      if (event == null) {
        event = new Event();
        event.setIdAsNew();
        event.setAuthor(loggedPerson);
        isNew = true;

        //set schedule
        long start = pageState.getEntry("startMillis").longValueNoErrorNoCatchedExc();
        start = start > 0 ? start : System.currentTimeMillis();
        long end = pageState.getEntry("endMillis").longValueNoErrorNoCatchedExc();
        end = end > 0 ? end : System.currentTimeMillis() + 30 * CompanyCalendar.MILLIS_IN_MINUTE;

        if (pageState.getEntry("isFullDay").checkFieldValue()){
          CompanyCalendar cc = new CompanyCalendar(start);
          start=cc.setAndGetTimeToDayStart().getTime();
          cc.setTimeInMillis(end);
          end=cc.setAndGetTimeToDayEnd().getTime();
        }
        Period period = new Period(start,end);

        period.store();
        event.setSchedule(period);

        //set attendees
        List<Resource> resources = ResourceBricks.fillWorkGroup(pageState);
        if (event.getTargetSize() > 0) {
          for (Resource resource : new HashSet<Resource>(event.getTargets())) {
            if (!resources.contains(resource))
              event.getTargets().remove(resource);
          }
        }
        for (Resource resource : resources) {
          event.getTargets().add(resource);
        }

      } else {
        //test security
        if (!logged.equals(event.getAuthor().getMyself()) && !logged.hasPermissionAsAdmin())
          throw new SecurityException("No permission to edit this agenda event.");
      }

      String value = pageState.getEntryAndSetRequired("summary").stringValue();
      if (value != null)
        event.setSummary(value);
      else
        invalidEntry = true;

      event.setDescription(pageState.getEntry("description").stringValueNullIfEmpty());
      event.setLocation(pageState.getEntry("location").stringValueNullIfEmpty());
      event.setPersonal(pageState.getEntry("isPersonal").checkFieldValue());
      event.setUnavailability(pageState.getEntry("isUnavailable").checkFieldValue());
      event.setReminder(pageState.getEntry("isReminder").checkFieldValue());

      if (!invalidEntry) {
        event.store();
        notifyIcal(event,Method.REQUEST);

        Hit.getInstanceAndStore(event, logged, isNew ? .3 : .2);
        Hit.getInstanceAndStore(event, event.getTargets(), isNew ? .2 : .1);
      }

      getEvents(pageState, json);

      if (invalidEntry) {
        throw new ActionException("SOMETHING WRONG");
      }

      // -------------------------------------------- MOVE / RESIZE EVENT --------------------------------------------------------------------------------
    } else if ("MOVRESEV".equals(pageState.command)) {
      Event ev = Event.load(pageState.getEntry("id").intValueNoErrorCodeNoExc() + "");
      if (ev != null && logged.hasPermissionAsAdmin() || logged.equals(ev.getAuthor().getMyself())) {
        long start = pageState.getEntry("startMillis").longValueNoErrorNoCatchedExc();
        long dur = pageState.getEntry("duration").longValueNoErrorNoCatchedExc();

        CompanyCalendar cc = new CompanyCalendar(start);

        ScheduleSupport schedule = ev.getSchedule();
        if (schedule instanceof Period){
          schedule.setStartDate(cc.getTime());
          schedule.setDurationInMillis(dur);
        } else {
          schedule.setStartTimeInMillis(cc.getMillisFromMidnight());
          schedule.setDurationInMillis(dur);
        }

        ev.store();

        notifyIcal(ev,Method.REQUEST);

        //get all events for this view type (week, day, month) and for the focused millis in order to get recurrent events updated
        getEvents(pageState, json);

      }


      // -------------------------------------------- DELETE EVENT --------------------------------------------------------------------------------
    } else if ("DELEV".equals(pageState.command)) {
      Event ev = Event.load(pageState.getEntry("id").intValueNoErrorCodeNoExc() + "");
      if (ev != null && logged.hasPermissionAsAdmin() || logged.equals(ev.getAuthor().getMyself())) {

        long startMillis= pageState.getEntry("startMillis").longValueNoErrorNoCatchedExc();
        // se è un periodo lo cancello, se è una ricerrenza modifico solo la ricorrenza creando eccezione
        if(ev.getSchedule() instanceof Period){
          notifyIcal(ev,Method.CANCEL);
          DeleteHelper.cmdDelete(ev,pageState);
        } else if (startMillis>0) {
          ev.setExceptions((JSP.ex(ev.getExceptions())?ev.getExceptions()+",":"")+startMillis);
          ev.store();
          notifyIcal(ev, Method.REQUEST);
        }

        //get all events for this view type (week, day, month) and for the focused millis in order to get recurrent events updated
        getEvents(pageState, json);
      }



      // -------------------------------------------- BULK REMOVE EVENTS --------------------------------------------------------------------------------
    } else if ("DELSEEV".equals(pageState.command)) {

      JSONArray jsa = JSONArray.fromObject(pageState.getEntry("ids").stringValueNullIfEmpty());
      JSONArray deleted = new JSONArray();

      for (Object oid : jsa) {
        String id = oid.toString();

        Event delenda = Event.load(id);
        if (logged.hasPermissionAsAdmin() || delenda != null && logged.equals(delenda.getAuthor().getMyself())) {
          DeleteHelper.cmdDelete(delenda, pageState);
          deleted.add(id);
        }
      }
      json.element("deleted", deleted);

      // -------------------------------------------- DELETE DISCUSSION POINT --------------------------------------------------------------------------------
    } else if ("DELDIP".equals(pageState.command)) {
      DiscussionPoint d = DiscussionPoint.load(pageState.getEntry("id").intValueNoErrorCodeNoExc());
      if (d!=null && (logged.hasPermissionAsAdmin() || (d.getMeeting().getOwner().equals(logged))) ) 
        //d.remove();
        DeleteHelper.cmdDelete(d,pageState);
      else
        throw new SecurityException(SecurityConstants.I18N_PERMISSION_LACKING);

      // -------------------------------------------- RETRIEVE DOCUMENTS FOR TASK --------------------------------------------------------------------------------
    } else if ("REFRESHDOC".equals(pageState.command)) {
      Task task=Task.load(pageState.getEntry("taskId").intValueNoErrorCodeNoExc()+"");
      if (task!=null && task.hasPermissionFor(logged, TeamworkPermissions.document_canRead)){
        Set<TeamworkDocument> docs = task.getDocuments();
        if (JSP.ex(docs)){
          JSONArray ds= new JSONArray();
          for (TeamworkDocument d:docs){
            ds.add(d.jsonify(pageState));
          }
          json.element("documents",ds);
        }
      }

    }

  } catch (Throwable t) {
    jsonHelper.error(t);
  }

  jsonHelper.close(pageContext);

%><%!
  /**
    send Ical notification using a queue
  **/
  private void notifyIcal(Event event, Method method ) {
    long alertTimeValue = CompanyCalendar.MILLIS_IN_MINUTE*30;
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
      net.fortuna.ical4j.model.Calendar ical=null;
      if (Method.REQUEST.equals(method))
        ical = IcalUtilities.getICal(IcalUtilities.getInviteVEvent(event, alertTimeValue), method);
      else if (Method.CANCEL.equals(method))
        ical = IcalUtilities.getICal(IcalUtilities.getCancelVEvent(event), method);
      else
        throw new PlatformRuntimeException("Icalendar Method NOT supported");
      IcalUtilities.sendIcalByMailQueued(subscribedToClient, event.getSummary(), ical,event.getId()+"");
    }
  }

  private void getEvents(PageState pageState, JSONObject json) throws PersistenceException {
    long focusMillis = pageState.getEntry("focusMillis").longValueNoErrorNoCatchedExc();
    String calendarType = pageState.getEntry("type").stringValueNullIfEmpty();

    focusMillis = focusMillis <= 0 ? System.currentTimeMillis() : focusMillis;

    Date focusDate = new Date(focusMillis);

    CompanyCalendar cc = new CompanyCalendar(focusDate);


    Period period = null;
    if ("DAY".equals(calendarType)) {
      period = Period.getDayPeriodInstance(focusDate);

    } else if ("MONTH".equals(calendarType)) {
      period = Period.getMonthPeriodInstance(focusDate);

    } else {//WEEK by default
      period = Period.getWeekPeriodInstance(focusDate, SessionState.getLocale());

    }

    //load events
    List<Resource> resourcesFromFilter = ResourceBricks.fillWorkGroup(pageState);

    boolean onlyMeInIt = false;
    boolean showAuthored = false;
    boolean showTargets = false;
    boolean isPersonal = false;
    boolean showWork = false;
    boolean unavail = false;

    String filter = pageState.getEntry("FILTER").stringValueNullIfEmpty();
    if ("WORK".equals(filter)) {
      onlyMeInIt = false;
      showAuthored = false;
      showTargets = false;
      showWork = true;
      isPersonal = false;
    } else if ("PERSONAL".equals(filter)) {
      onlyMeInIt = false;
      showAuthored = false;
      showTargets = false;
      isPersonal = true;
      showWork = false;
    } else if ("AUTHOR".equals(filter)) {
      onlyMeInIt = false;
      showAuthored = true;
      showTargets = false;
      isPersonal = false;
      showWork = false;
    } else if ("ONLY_ME_IN_IT".equals(filter)) {
      onlyMeInIt = true;
      showAuthored = false;
      showTargets = false;
      isPersonal = false;
      showWork = false;
    } else if ("UNAVAIL".equals(filter)) {
      unavail = true;
      showTargets = true;
    } else {
      onlyMeInIt = false;
      showAuthored = false;
      showTargets = true;
      isPersonal = false;
      showWork = false;
    }

    List<Event> candEventsInPeriod = Event.getFilteredEventsInPeriodWithCollisionFor(resourcesFromFilter, period, 0, showWork, onlyMeInIt, showAuthored, showTargets, isPersonal, unavail);
    candEventsInPeriod.addAll(AgendaBricks.getIcalExternalEvents(pageState, period));

    //remove duplicated events
    HashSet<String> visitedIcalId = new HashSet();
    List<Event> eventsInPeriod = new ArrayList();
    for (Event ev:candEventsInPeriod ){
      if (!visitedIcalId.contains(ev.getIcalId())){
        visitedIcalId.add(ev.getIcalId());
        eventsInPeriod.add(ev);
      }
    }


    JSONArray jsa = new JSONArray();


    //cc.setTime(period.getStartDate());
    //while (cc.getTime().getTime() < period.getEndDate().getTime()) {

    //  Period theWorkingDayPeriod = Period.getDayPeriodInstance(cc.getTime());
      Period theWorkingDayPeriod = period;

      ArrayList<PeriodEvent> oneDayPeriodEvents = new ArrayList();
      for (Event event : eventsInPeriod) {
        // get working day
        List<Period> periods = event.getSchedule().getPeriods(theWorkingDayPeriod, true,event.getExceptions());
        for (Period p : periods) {
          p.setIdAsNew();
          oneDayPeriodEvents.add(new PeriodEvent(p, event));
        }
      }

      Collections.sort(oneDayPeriodEvents);
      for (PeriodEvent pev : oneDayPeriodEvents) {
        JSONObject jEv = pev.event.jsonify(pageState);
        jEv.element("startMillis", pev.period.getStartDate().getTime());
        jEv.element("endMillis", pev.period.getEndDate().getTime());
        jEv.element("trimmed", pev.period.isTrimmed());
        jsa.add(jEv);
      }

      //cc.add(CompanyCalendar.DATE, 1);
    //}
    json.element("events", jsa);
  }

  private void getTaskIssueEvents(RestState pageState, JSONObject json) throws PersistenceException {

    long focusMillis = pageState.getEntry("focusMillis").longValueNoErrorNoCatchedExc();
    String calendarType = pageState.getEntry("type").stringValueNullIfEmpty();

    focusMillis = focusMillis <= 0 ? System.currentTimeMillis() : focusMillis;

    Date focusDate = new Date(focusMillis);

    Period period = null;
    if ("DAY".equals(calendarType)) {
      period = Period.getDayPeriodInstance(focusDate);

    } else if ("MONTH".equals(calendarType)) {
      period = Period.getMonthPeriodInstance(focusDate);

    } else {//WEEK by default
      period = Period.getWeekPeriodInstance(focusDate, SessionState.getLocale());

    }

    //load resources
    List<Resource> resourcesFromFilter = ResourceBricks.fillWorkGroup(pageState);

    if(!pageState.getEntry("TASKONLY").checkFieldValue()) {
      //-------------------------------------- load issues
      String hql = " select year(iss.shouldCloseBy)*10000+month(iss.shouldCloseBy)*100+day(iss.shouldCloseBy),iss.assignedTo.id,count(iss), sum(iss.estimatedDuration) from " + Issue.class.getName() + " as iss where (iss.shouldCloseBy between :d1 and :d2) and iss.assignedTo in (:ress) and iss.status.behavesAsOpen=true " +
        "group by year(iss.shouldCloseBy)*10000+month(iss.shouldCloseBy)*100+day(iss.shouldCloseBy),iss.assignedTo.id ";


      Query query = new OqlQuery(hql).getQuery();
      query.setTimestamp("d1", period.getStartDate());
      query.setTimestamp("d2", period.getEndDate());
      query.setParameterList("ress", resourcesFromFilter);
      List<Object[]> isss = query.list();


      if (JSP.ex(isss))
        json.element("issues", isss);
    }


    if(!pageState.getEntry("ISSUEONLY").checkFieldValue()) {
      //-------------------------------------- load tasks
      QueryHelper qh = new QueryHelper("select task from " + Task.class.getName() + " as task");
      qh.addOQLClause("((task.schedule.start between :d1 and :d2) or (task.schedule.end between :d1 and :d2)) and assignment.resource in (:ress) ");
      qh.setParameter("d1", period.getStartDate());
      qh.setParameter("d2", period.getEndDate());
      qh.setParameter("ress", resourcesFromFilter);

      TaskBricks.addSecurityReadClauses(qh, pageState);

      qh.addToHqlString("order by task.schedule.start");

      List<Task> tks = qh.toHql().list();
      Set<Task> tasks = new HashSet<Task>(tks);

      JSONArray jts = new JSONArray();
      for (Task t : tasks) {

        JSONObject jt = new JSONObject();

        long millis;
        String startEnd;
        if (period.contains(t.getSchedule().getStartDate())) {
          millis = t.getSchedule().getStartDate().getTime();
          startEnd = I18n.get("FLD_START");
        } else {
          millis = t.getSchedule().getEndDate().getTime();
          startEnd = I18n.get("FLD_END");
        }

        jt.element("millis", millis);
        jt.element("mnemo", t.getMnemonicCode());
        jt.element("name", t.getDisplayName());
        jt.element("msg", startEnd);

        JSONArray rs = new JSONArray();
        for (Assignment ass : t.getAssignments()) {
          if (resourcesFromFilter.contains(ass.getResource())) {
            rs.add(ass.getResource().getId());
          }
        }
        jt.element("resources", rs);

        jts.add(jt);
      }

      if (JSP.ex(jts))
        json.element("tasks", jts);
    }



  }



%>