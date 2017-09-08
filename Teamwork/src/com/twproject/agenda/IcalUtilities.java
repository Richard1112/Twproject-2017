package com.twproject.agenda;

import com.twproject.agenda.businessLogic.AgendaAction;
import com.twproject.rank.Hit;
import com.twproject.resource.Person;
import com.twproject.resource.Resource;
import com.twproject.resource.ResourceBricks;
import net.fortuna.ical4j.data.CalendarBuilder;
import net.fortuna.ical4j.data.ParserException;
import net.fortuna.ical4j.model.Calendar;
import net.fortuna.ical4j.model.*;
import net.fortuna.ical4j.model.Date;
import net.fortuna.ical4j.model.component.VAlarm;
import net.fortuna.ical4j.model.component.VEvent;
import net.fortuna.ical4j.model.component.VFreeBusy;
import net.fortuna.ical4j.model.parameter.PartStat;
import net.fortuna.ical4j.model.property.*;
import net.fortuna.ical4j.model.property.Priority;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpMethod;
import org.apache.commons.httpclient.cookie.CookiePolicy;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.httpclient.params.HttpMethodParams;
import org.apache.log4j.*;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.agenda.*;
import org.jblooming.agenda.Period;
import org.jblooming.messaging.MailHelper;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.oql.QueryHelper;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.system.SystemConstants;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.settings.PlatformConfiguration;
import org.jblooming.waf.view.PageSeed;

import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.internet.AddressException;
import javax.mail.internet.InternetAddress;
import java.io.*;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.*;


public class IcalUtilities {

  private static Timer emailQueueTimer;
  private static Map<String,IcalMessageCarrier> emailQueue= new Hashtable<String,IcalMessageCarrier>();

  static Calendar getIcalForPerson(Person loggedPerson) throws PersistenceException {
  java.util.Calendar calStart = java.util.Calendar.getInstance();
  calStart.set(java.util.Calendar.MONTH, calStart.get(java.util.Calendar.MONTH) - 1);
  java.util.Date start = calStart.getTime();
  String hql = "select event from " + Event.class.getName() + " as event";
  QueryHelper qhelp = new QueryHelper(hql);
  qhelp.setDistinct();
  qhelp.addJoinAlias("event.targets target");
  qhelp.addOQLClause("target.id= :targetId", "targetId", loggedPerson.getId().toString());
  if (start != null)   {
    qhelp.addOQLClause("(event.schedule.start >= :start", "start", start);
  }
  qhelp.addOrQueryClause("event.schedule.end >= :today)" );
  qhelp.addParameter("today",new java.util.Date());
  List<Event> eventi = qhelp.toHql().list();
  Calendar ical = getICalendar();
  ical.getProperties().add(Method.PUBLISH);

  ical.getProperties().add(new XXProperty("X-WR-CALNAME", "Twproject Calendar")); //
  long mezzor = CompanyCalendar.MILLIS_IN_MINUTE * 30;
  for (Event event : eventi) {
    ical.getComponents().add(getInviteVEvent(event, mezzor));
  }
  return ical;
}

  public static List<Event> getEventsFromStream(Person person, boolean noPersist, InputStream inputStream) throws IOException, ParserException, PersistenceException {
    CalendarBuilder cb = new CalendarBuilder();
    Calendar iCalendar = cb.build(inputStream);
    List<Event> events = new ArrayList<Event>();
    for (Iterator it = iCalendar.getComponents().iterator(); it.hasNext(); ) {
      try {
        Component component = (Component) it.next();
        Event event = null;
        if (component instanceof VFreeBusy) {
          VFreeBusy vFree = (VFreeBusy) component;
          event = manageIncomingFreeBusy(vFree, person, noPersist);
        } else if (component instanceof VEvent) {
          VEvent ve = (VEvent) component;
          event = manageIncomingEvent(iCalendar, ve, person, null, noPersist);
        }
        if (event!=null)
          events.add(event);

        if (event != null && noPersist) {
          PersistenceContext.getDefaultPersistenceContext().session.evict(event);
        }

      } catch (ActionException e) {
        Tracer.platformLogger.debug(e);
      }
    }
    return events;
  }

  public static List<Event> createEvents(URL url, Person person, boolean noPersist) throws IOException, ParserException, PersistenceException {
    HttpClient client = new HttpClient();

    client.getParams().setParameter(HttpMethodParams.USER_AGENT,
      "Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.2) Gecko/20100316 Firefox/3.6.2");

    HttpMethod method = new GetMethod(url.toExternalForm());
    method.getParams().setCookiePolicy(CookiePolicy.RFC_2109);
    client.executeMethod(method);
    InputStream inputStream = method.getResponseBodyAsStream();
    List<Event> events = getEventsFromStream(person, noPersist, inputStream);

    inputStream.close();
    method.releaseConnection();

    fixAuthorAndTargets(events);
    return events;
  }

  public static List<Event> getEventsFromURLs(String urlsCommaSeparated) throws ParserException, IOException, PersistenceException {
    List<Event> iCalEvents = new ArrayList<Event>();
    if (JSP.ex(urlsCommaSeparated)) {
      String[] links = urlsCommaSeparated.split(",");
      for (String s : links) {
        if (JSP.ex(s)) {
          URL url = new URL(s);
          iCalEvents.addAll(IcalUtilities.createEvents(url, null, true));
        }
      }
    }
    return iCalEvents;
  }


  public static List<Event> createEvent(File file, Person person, boolean noPersist) throws IOException, ParserException, PersistenceException {
    InputStream inputStream = new FileInputStream(file);
    List<Event> events = getEventsFromStream(person, noPersist, inputStream);
    inputStream.close();
    fixAuthorAndTargets(events);
    return events;
  }

  private static void fixAuthorAndTargets(List<Event> events) {
    Person anonym = new Person();
    anonym.setPersonSurname("External Calendar");
    for (Event event : events) {
      event.setFromExternalCalendar(true);
      if (event.getAuthor() == null) {
        event.setAuthor(anonym);
      } else {
        Person resource = new Person();
        resource.setPersonSurname(event.getAuthor().getPersonSurname());
        resource.setPersonName(event.getAuthor().getPersonName());
        event.setAuthor(resource);
      }
      if (event.getTargetSize() == 0) {
        event.getTargets().add(anonym);
      } else {
        Set<Resource> nres = new HashSet();
        for (Resource r : event.getTargets()) {
          Person resource = new Person();
          resource.setPersonSurname(((Person) r).getPersonSurname());
          resource.setPersonName(((Person) r).getPersonName());
          nres.add(resource);
        }
        event.setTargets(nres);
      }
    }
  }

  public static enum SEND_MAIL_MODALITY {
    ICAL_IN_BODY, ICAL_IN_BODY_OUTLOOK2007, ICAL_AS_ATTACH
  }

  public static VEvent getInviteVEvent(Event event, long millisBeforeAlert) {

    VEvent vEvent = createVEventBody(event);

    Set<Resource> atts = event.getTargets();
    for (Resource resource : atts) {
      if (JSP.ex(resource.getDefaultEmail())) {
        XXProperty a = getAttendee(resource, "ROLE=REQ-PARTICIPANT;RSVP=TRUE");
        vEvent.getProperties().add(a);
      }
    }

    String desc = JSP.w(event.getDescription()).trim();

    PageSeed ps = new PageSeed(ApplicationState.serverURL + "/applications/teamwork/agenda/agendaEditor.jsp");
    ps.setCommand(Commands.EDIT);
    ps.mainObjectId = event.getId();
    desc = (desc.length() > 0 ? desc + "\n\n" : "") + "To see the complete set of participants click here:\n" + ps.toLinkToHref();

    Description d = new Description(desc);
    vEvent.getProperties().add(d);

    if (millisBeforeAlert > 0) {
      int min = (int) (millisBeforeAlert / CompanyCalendar.MILLIS_IN_MINUTE);
      Dur dur = new Dur((min > 0 ? "-" : "") + "PT" + min + "M");
      VAlarm v = new VAlarm(dur);
      Action a = new Action("DISPLAY");
      v.getProperties().add(a);
      d = new Description("Reminder");
      v.getProperties().add(d);
      vEvent.getAlarms().add(v);
    }

    return vEvent;
  }

  public static VEvent getDeclineVEvent(Event event, Resource decliner) {

    VEvent vEvent = createVEventBody(event);

    Set<Resource> atts = event.getTargets();

    if (JSP.ex(decliner.getDefaultEmail())) {

      XXProperty a = getAttendee(decliner, "PARTSTAT=DECLINED");
      vEvent.getProperties().add(a);
    }

    String desc = "DECLINED:" + JSP.w(event.getDescription());

    PageSeed ps = new PageSeed(ApplicationState.serverURL + "/applications/teamwork/agenda/agendaEditor.jsp");
    ps.setCommand(Commands.EDIT);
    ps.mainObjectId = event.getId();
    desc = desc + "\n\nTo see the event click here:\n" + ps.toLinkToHref();

    Description d = new Description(desc);
    vEvent.getProperties().add(d);


    return vEvent;
  }

  public static VEvent getCancelVEvent(Event event) {

    VEvent vEvent = createVEventBody(event);

    Set<Resource> atts = event.getTargets();
    for (Resource resource : atts) {
      if (JSP.ex(resource.getDefaultEmail())) {
        XXProperty a = getAttendee(resource, "ROLE=REQ-PARTICIPANT;RSVP=TRUE");
        vEvent.getProperties().add(a);
      }
    }
    String desc = JSP.w(event.getDescription());

    Description d = new Description(desc);
    vEvent.getProperties().add(d);

    return vEvent;
  }


  public static net.fortuna.ical4j.model.Calendar getICal(VEvent vEvent, Method method) {
    net.fortuna.ical4j.model.Calendar iCalendar = getICalendar();
    iCalendar.getComponents().add(vEvent);
    iCalendar.getProperties().add(method);

    TimeZoneRegistry timeZoneRegistry = TimeZoneRegistryFactory.getInstance().createRegistry();
    net.fortuna.ical4j.model.TimeZone timeZone = timeZoneRegistry.getTimeZone(ApplicationState.SYSTEM_TIME_ZONE.getID());
    if (timeZone!=null){
      iCalendar.getComponents().add(timeZone.getVTimeZone());
    }

    //iCalendar.validate(true);
    return iCalendar;
  }

  public static Event manageIncomingEvent(Calendar iCalendar, VEvent vEvent, Person sender, Message message) throws PersistenceException, ActionException {
    return manageIncomingEvent(iCalendar, vEvent, sender, message, false);
  }

  public static Event manageIncomingEvent(Calendar iCalendar, VEvent vEvent, Person sender, Message message, boolean noPersist) throws PersistenceException, ActionException {

    Event event = null;
    boolean isOutlookNoExchange = iCalendar.getProductId().getValue().indexOf("Microsoft Corporation//Outlook") > -1;

    if (Method.CANCEL.equals(iCalendar.getMethod())) {
      String UID = vEvent.getUid().getValue();
      if (UID != null) {
        event = getEventByUID(UID);
        if (event != null && sender!=null && sender.equals(event.getAuthor().getMyself())) {
          event.remove();
        }
      }

    } else if (Method.REPLY.equals(iCalendar.getMethod())||Method.COUNTER.equals(iCalendar.getMethod())) {

      String UID = vEvent.getUid().getValue();
      if (UID != null) {
        event = getEventByUID(UID);
        if (event != null) {

            PropertyList pl = vEvent.getProperties();
            Iterator pli = pl.iterator();

            while (pli.hasNext()) {
              net.fortuna.ical4j.model.Property p = (Property) pli.next();
              if (p instanceof Attendee) {
                Attendee a = (Attendee) p;
                if (PartStat.DECLINED.equals(a.getParameter(Parameter.PARTSTAT))) {
                  URI uriA = a.getCalAddress();
                  String emailA = uriA.getSchemeSpecificPart();
                  List<Person> target = ResourceBricks.getPersonByEmail(emailA);
                  if (target != null && JSP.ex(event.getTargets()) && target.contains(sender)) {
                    boolean removed =event.getTargets().remove(sender);
                    event.store();
                    if (removed){
                      //notify to author
                      Person author = event.getAuthor();
                      org.jblooming.messaging.Message twMessage = new org.jblooming.messaging.Message();
                      twMessage.setSubject(I18n.get("AGENDA_UPGRADE"));
                      twMessage.setToOperator(author.getMyself());

                      if (sender != null)
                        twMessage.setFromOperator(sender.getMyself());

                      twMessage.setDefaultExpires();
                      twMessage.setMedia(StringUtilities.unSplit(author.getMyself().getPreferredMediaOrDefault(MessagingSystem.Media.STICKY),","));
                      String messageBody = I18n.getLabel("ICAL_ATTENDEE_REMOVED_%%",JSP.ex(sender)?sender.getDisplayName():emailA);
                      messageBody = messageBody + "\n" + event.getAbstractForIndexing();
                      twMessage.setMessageBody(messageBody);
                      PageSeed ps = new PageSeed(ApplicationState.contextPath + "/applications/teamwork/agenda/agendaEditor.jsp");
                      ps.setCommand(Commands.EDIT);
                      ps.setMainObjectId(event.getId());
                      ButtonLink editLink = ButtonLink.getTextualInstance(event.getSummary(), ps);

                      twMessage.setMessageBody(messageBody);
                      twMessage.store();
                    }

                  }
                } else if (PartStat.TENTATIVE.equals(a.getParameter(Parameter.PARTSTAT))) {

                  Person author = event.getAuthor();
                  Event newEvent = new Event();
                  newEvent = createEventFromVEvent(vEvent, isOutlookNoExchange, newEvent,  sender, noPersist);

                  String messageBody = I18n.get("ICAL_NEW_PROPOSAL");
                  newEvent.setId(event.getId());// not persisted!!!!!! only for getting the right link
                  messageBody = messageBody + "\n" + newEvent.getAbstractForIndexing();

                  org.jblooming.messaging.Message twMessage = new org.jblooming.messaging.Message();
                  twMessage.setToOperator(author.getMyself());
                  if (sender != null)
                    twMessage.setFromOperator(sender.getMyself());
                  twMessage.setSubject(I18n.get("AGENDA_UPGRADE"));
                  twMessage.setDefaultExpires();
                  twMessage.setMedia(StringUtilities.unSplit(author.getMyself().getPreferredMediaOrDefault(MessagingSystem.Media.STICKY),","));
                  twMessage.setMessageBody(messageBody);
                  PageSeed ps = new PageSeed(ApplicationState.serverURL + "/applications/teamwork/agenda/agendaEditor.jsp");
                  ps.setCommand(Commands.EDIT);
                  ps.setMainObjectId(event.getId());
                  ButtonLink editLink = ButtonLink.getTextualInstance(event.getSummary(), ps);

                  twMessage.setLink(editLink.toPlainLink());
                  twMessage.store();

                  String warning = I18n.get("ICAL_NEW_PROPOSAL_NOTIFIED");
                  try {
                    if (message!=null)
                      MailHelper.replyToMessage(message, warning);
                  } catch (MessagingException e) {
                    Tracer.platformLogger.error(e);
                  }

                }
            }
          }
        }
      }
    } else if (Method.REQUEST.equals(iCalendar.getMethod())) {
      event = createOrUpdateEvent(vEvent, isOutlookNoExchange,  sender, noPersist);
    } else if (Method.PUBLISH.equals(iCalendar.getMethod())) {
      event = createOrUpdateEvent(vEvent, isOutlookNoExchange,  sender, noPersist);
    }
    return event;
  }

  public static Event manageIncomingFreeBusy(VFreeBusy vFreeBusy, Person sender,  boolean noPersist) throws PersistenceException, ActionException {

    String UID = vFreeBusy.getUid().getValue();
    if (UID != null) {
      Event event = getEventByUID(UID);
      if (event != null)
        event.remove();
    }
    Event event = null;
    event = createEventFromVFreeBusy(vFreeBusy, event, sender);
    if (!noPersist) {
      event.getSchedule().store();
      event.store();
    }
    return event;
  }

  private static Event createEventFromVFreeBusy(VFreeBusy vFreeBusy, Event event, Person sender) throws PersistenceException {
    event = new Event();
    event.setIdAsNew();

    List<Attendee> attendees = new ArrayList();
    RRule rrule = null;
    PropertyList pl = vFreeBusy.getProperties();
    Iterator pli = pl.iterator();

    while (pli.hasNext()) {
      Property p = (Property) pli.next();
      if (p instanceof Attendee) {
        attendees.add((Attendee) p);
      } else if (p instanceof RRule)
        rrule = (RRule) p;
    }

    Person authr = sender;
    String uid = vFreeBusy.getUid().getValue();
    event.setIcalId(uid);
    event.setUnavailability(true);
    event.setSummary(vFreeBusy.getProperty("SUMMARY").getValue());
    event.setDescription(vFreeBusy.getProperty("SUMMARY").getValue());

    event.getTargets().clear();
    for (Attendee a : attendees) {
      URI uriA = a.getCalAddress();
      String emailA = uriA.getSchemeSpecificPart();
      List<Person> target = ResourceBricks.getPersonByEmail(emailA);

      if (target.size() == 0) {
        if (!emailA.equals(ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_FROM)))
          Tracer.platformLogger.info("For email " + emailA + " no person found");
      } else {
        String severalNames = "";
        for (Person person : target) {
          event.getTargets().add(person);
          severalNames = severalNames + person.getDisplayName() + " ";
        }

      }
    }
    if (authr != null) {
      event.getTargets().add(authr);
      event.setAuthor(authr);
    }
    ScheduleSupport ss = null;
    java.util.Date start = vFreeBusy.getStartDate().getDate();
    if (rrule == null) {
      Period p = new Period(start, vFreeBusy.getEndDate().getDate());
      ss = p;
    }
    event.setSchedule(ss);
    return event;
  }


  /**
   * Every time a message is enqueued timer is reset
   * @param tos
   * @param subject
   * @param ical
   * @param keyToCleanQueue
   */
  public synchronized static void sendIcalByMailQueued(Set<Resource> tos, String subject, Calendar ical,String keyToCleanQueue) {

    //stop queue time
    if (emailQueueTimer !=null){
      emailQueueTimer.cancel();
      emailQueueTimer=null;
    }

    //clear events with same key
    emailQueue.remove(keyToCleanQueue);

    InternetAddress fromIA = MailHelper.getSystemInternetAddress();
    Set<InternetAddress> tosIA = new HashSet();

    for (Resource to : tos) {
      if (JSP.ex(to.getDefaultEmail())) {
        List<String> mailTOs = MailHelper.mailTosAsList(to.getDefaultEmail());
        for (String mailTO : mailTOs) {
          try {
            InternetAddress toIA = new InternetAddress(mailTO);
            toIA.setPersonal(to.getDisplayName(), "UTF-8");
            tosIA.add(toIA);
          } catch (Throwable t) {
            Tracer.platformLogger.error("sendIcalByMail failed on e-mail", t);
          }
        }
      }

    }

    //create carrier
    IcalMessageCarrier messageCarrier = new IcalMessageCarrier(tosIA, JSP.limWr(subject, 50), ical.toString());

    //enqueue carrier
    emailQueue.put(keyToCleanQueue,messageCarrier);


    //schedule timer in two minutes
    emailQueueTimer= new Timer();
    emailQueueTimer.schedule(new TimerTask() {
      public void run() {
        for (IcalMessageCarrier mc: emailQueue.values()){
          MailHelper.sendMailWithHeader(mc.tos, mc.subject, "text/calendar;method=REQUEST;charset=UTF-8",mc.body);
        }
        emailQueue.clear();
      }
    }, CompanyCalendar.MILLIS_IN_MINUTE*2);
//    }, 10); //send now

  }


  static private class IcalMessageCarrier {
    InternetAddress from;
    Set<InternetAddress> tos;
    String subject;
    String body;

    IcalMessageCarrier(Set<InternetAddress> tos, String subject, String body) {
      this.tos = tos;
      this.subject = subject;
      this.body = body;
    }

  }

  public static void sendIcalByMail(Set<Resource> tos, String subject, Calendar ical) {
    Set<InternetAddress> tosIA = new HashSet();

    for (Resource to : tos) {
      if (JSP.ex(to.getDefaultEmail())) {
        List<String> mailTOs = MailHelper.mailTosAsList(to.getDefaultEmail());
        for (String mailTO : mailTOs) {
          try {
            InternetAddress toIA = new InternetAddress(mailTO);
            toIA.setPersonal(to.getDisplayName(), "UTF-8");
            tosIA.add(toIA);
          } catch (Throwable t) {
            Tracer.platformLogger.error("sendIcalByMail failed on e-mail", t);
          }
        }
      }
    }
    MailHelper.sendMailWithHeader(tosIA, JSP.limWr(subject, 50), "text/calendar;method=REQUEST;charset=UTF-8", ical.toString());
  }

  private static InternetAddress generatePersonalMail(String fromMails, String dispName) {
    InternetAddress fromIA;
    try {
      String fromEmail = MailHelper.mailTosAsList(fromMails).get(0);
      fromIA = new InternetAddress(fromEmail);
      fromIA.setPersonal(dispName, "UTF-8");
    } catch (AddressException e) {
      throw new PlatformRuntimeException(e);
    } catch (UnsupportedEncodingException e) {
      throw new PlatformRuntimeException(e);
    }
    return fromIA;
  }


  public static net.fortuna.ical4j.model.Calendar getICalendar() {

    ComponentList cl = new ComponentList();
    net.fortuna.ical4j.model.Calendar iCalendar = new net.fortuna.ical4j.model.Calendar(cl);
    iCalendar.getProperties().add(new ProdId("-//Teamwork//ICalendar export " + ApplicationState.getApplicationVersion() + "//EN"));
    iCalendar.getProperties().add(Version.VERSION_2_0);
    iCalendar.getProperties().add(CalScale.GREGORIAN);

    return iCalendar;
  }

  //------------------------------------------------ PRIVATE SUPPORT METHODS  ------------------------------------------------

  private static WeekDay getIcalWeekDay(int day) {
    if (CompanyCalendar.MONDAY == day)
      return WeekDay.MO;
    else if (CompanyCalendar.TUESDAY == day)
      return WeekDay.TU;
    else if (CompanyCalendar.WEDNESDAY == day)
      return WeekDay.WE;
    else if (CompanyCalendar.THURSDAY == day)
      return WeekDay.TH;
    else if (CompanyCalendar.FRIDAY == day)
      return WeekDay.FR;
    else if (CompanyCalendar.SATURDAY == day)
      return WeekDay.SA;
    else if (CompanyCalendar.SUNDAY == day)
      return WeekDay.SU;
    else
      throw new PlatformRuntimeException("Invalid day: " + day);
  }

  private static int[] getWeekDaysFromIcal(Recur recur) {

    int[] days = new int[recur.getDayList().size()];
    WeekDayList wdl = recur.getDayList();
    Iterator it = wdl.iterator();
    int j = 0;
    while (it.hasNext()) {
      String weekDay = ((WeekDay) it.next()).getDay().toUpperCase();
      if (WeekDay.MO.toString().equals(weekDay))
        days[j] = CompanyCalendar.MONDAY;
      else if (WeekDay.TU.toString().equals(weekDay))
        days[j] = CompanyCalendar.TUESDAY;
      else if (WeekDay.WE.toString().equals(weekDay))
        days[j] = CompanyCalendar.WEDNESDAY;
      else if (WeekDay.TH.toString().equals(weekDay))
        days[j] = CompanyCalendar.THURSDAY;
      else if (WeekDay.FR.toString().equals(weekDay))
        days[j] = CompanyCalendar.FRIDAY;
      else if (WeekDay.SA.toString().equals(weekDay))
        days[j] = CompanyCalendar.SATURDAY;
      else if (WeekDay.SU.toString().equals(weekDay))
        days[j] = CompanyCalendar.SUNDAY;
      else
        throw new PlatformRuntimeException("Invalid day: " + weekDay);
      j++;
    }
    return days;
  }

  private static Event createOrUpdateEvent(VEvent vEvent, boolean isOutlookNoExchange, Person sender, boolean noPersist) throws PersistenceException, ActionException {

    Event event = null;
    String UID = vEvent.getUid().getValue();
    if (UID != null) {
      event = getEventByUID(UID);
    }

    if (event != null && !event.isNew()) {
      if (!noPersist) {
        event.getSchedule().remove();
        event.setSchedule(null);
      }
    }

    event = createEventFromVEvent(vEvent, isOutlookNoExchange, event, sender,  noPersist);
    if (event != null && !noPersist) {
      ScheduleSupport scheduleSupport = event.getSchedule();
      scheduleSupport.store();
      event.store();
      Hit.getInstanceAndStore(event, event.getTargets(),PersistenceHome.NEW_EMPTY_ID.equals(event.getId()) ? .2 : .1);
      ComponentList cl = vEvent.getAlarms();
      if (cl != null && cl.size() > 0) {
        VAlarm v = (VAlarm) cl.iterator().next();
        Trigger t = v.getTrigger();
        Dur dur = t.getDuration();
        long millis = (dur.getWeeks() * CompanyCalendar.MILLIS_IN_WEEK + dur.getDays() * CompanyCalendar.MILLIS_IN_DAY * dur.getHours() * CompanyCalendar.MILLIS_IN_HOUR +
            dur.getMinutes() * CompanyCalendar.MILLIS_IN_MINUTE + dur.getSeconds() * 1000) * (dur.isNegative() ? 1 : -1);
        AgendaAction.createAlertListener(event, millis);
      }
    }
    return event;
  }


  private static Event createEventFromVEvent(VEvent vEvent, boolean isOutlookNoExchange, Event event, Person sender,  boolean noPersist) throws PersistenceException, ActionException {
    String email = "";
    Person authr = null;

    Organizer o = vEvent.getOrganizer();
    if (o != null) {
      // noi ci aspettiamo qualcosa del tipo     ORGANIZER:MAILTO:i@open-lab.com
      // ma alle volte arriva secondo formato IMIP (apple) ORGANIZER;CN=Tiz Can;EMAIL=tiz.can@mail.ch:mailto:ical+16ba7767-4df3-4a6e-aaca-0f5d448dc9ab@mail.ch
      // se c'è un parametro email usiamo quello, altrimenti si estrae alla vecchia
      if (o.getParameter("EMAIL")!=null){
        email = o.getParameter("EMAIL").getValue();
      } else {
        URI uri = o.getCalAddress();
        email = uri.getSchemeSpecificPart();
      }
      List<Person> organizers = ResourceBricks.getPersonByEmail(email);
      if (organizers.size() == 0 && ! noPersist)
        throw new ActionException("For email " + email + " no person found");

      if (organizers.size() > 1) {
        String names = "";
        for (Person person : organizers) {
          names = names + person.getDisplayName() + " ";
        }
        throw new ActionException("For email " + email + " several persons found:" + names);
      } else if (organizers.size() ==1){
        authr = organizers.get(0);

      } else if (noPersist){ //in case of reading from external calendar
        authr = sender;
      }
    } else {
      authr = sender;
    }
    if (event == null) {
      event = new Event();
      event.setIdAsNew();
    } else {
      authr = event.getAuthor();
    }

    List<Attendee> attendees = new ArrayList();
    RRule rrule = null;
    Clazz privateEvent =null;

    PropertyList pl = vEvent.getProperties();
    Iterator pli = pl.iterator();

    while (pli.hasNext()) {
      Property p = (Property) pli.next();
      if (p instanceof Attendee) {
        attendees.add((Attendee) p);
      } else if (p instanceof RRule){
        rrule = (RRule) p;
      } else if (p instanceof Clazz){
        privateEvent = (Clazz)p;
      } else if (p instanceof ExDate) {
        ExDate exd = (ExDate) p;
        Iterator it = exd.getDates().iterator();
        while (it.hasNext()){
          DateTime extDate = (DateTime) it.next();
          event.setExceptions((JSP.ex(event.getExceptions())?event.getExceptions()+",":"")+extDate.getTime());
        }
      }
    }

    ScheduleSupport schedule = createSchedule(vEvent, rrule);

    //se non si riesce a creare uno schedule si ritorna un null
    if (schedule==null)
      return null;

    /*if(privateEvent  != null && privateEvent.getValue().equalsIgnoreCase("PRIVATE"))
    event.setPersonal(true); */
    event.setIcalId(vEvent.getUid().getValue());


    if (vEvent.getLocation() != null)
      event.setLocation(vEvent.getLocation().getValue());

    if (vEvent.getSummary() != null)
      event.setSummary(vEvent.getSummary().getValue());

    if (vEvent.getDescription() != null) {
      String description = vEvent.getDescription().getValue();
      if (isOutlookNoExchange && description.lastIndexOf("~*")>=0) {
        description = description.substring(description.lastIndexOf("~*") + 4);
      }
      event.setDescription(description);
    }
    event.getTargets().clear();
    for (Attendee a : attendees) {
      URI uriA = a.getCalAddress();
      String emailA = uriA.getSchemeSpecificPart();
      List<Person> target = ResourceBricks.getPersonByEmail(emailA);

      if (target.size() == 0) {
        if (!emailA.equals(ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_FROM)))
          Tracer.platformLogger.info("For email " + emailA + " no person found");
      } else {
        String severalNames = "";
        for (Person person : target) {
          event.getTargets().add(person);
          severalNames = severalNames + person.getDisplayName() + " ";
        }
        if (target.size() > 1)
          Tracer.platformLogger.info("For email " + email + " several persons found:" + severalNames);
      }
    }
    //add also organizer if missing as Outlook always puts it in
    if (authr != null) {
      event.setAuthor(authr);
      event.getTargets().add(authr);
    }

    event.setSchedule(schedule);
    return event;
  }


  private static ScheduleSupport createSchedule(VEvent vEvent, RRule rrule) {
    ScheduleSupport ss = null;
    if (vEvent.getStartDate()==null || vEvent.getEndDate()==null) {
      Tracer.platformLogger.debug("vEvent start or end dates are null:\n" + vEvent+"\n");
      return null;
    }

    java.util.Date start = vEvent.getStartDate().getDate();
    java.util.Date end = vEvent.getEndDate().getDate();


    if (rrule == null) {
      Period p = new Period(start, end);
      ss = p;
    } else {

      Recur recur = rrule.getRecur();
      int repetition = recur.getCount();

      Date endRecur=null;
      if (repetition == -1 && recur.getUntil() != null) {
        endRecur=recur.getUntil();
      }

      String freqType = recur.getFrequency();
      if (Recur.DAILY.equals(freqType)) {
       ss = new ScheduleDaily(start, (int) (end.getTime() - start.getTime()), recur.getInterval(), repetition,false,endRecur);

      } else if (Recur.WEEKLY.equals(freqType)) {
        ss = new ScheduleWeekly(getWeekDaysFromIcal(recur), start, (int) (end.getTime() - start.getTime()), recur.getInterval(), repetition,endRecur);

      } else if (Recur.MONTHLY.equals(freqType)) {
        ss = new ScheduleMonthly(start, (int) (end.getTime() - start.getTime()), recur.getInterval(), repetition,endRecur);

      } else if (Recur.YEARLY.equals(freqType)) {
        ss = new ScheduleYearly(start, (int) (end.getTime() - start.getTime()), recur.getInterval(), repetition,endRecur);
      }

      PropertyList pl = vEvent.getProperties();
      Iterator pli = pl.iterator();
    }
    return ss;
  }

  private static void setSchedule(ScheduleSupport schedule, VEvent vEvent) {
    TimeZoneRegistry registry = TimeZoneRegistryFactory.getInstance().createRegistry();
    net.fortuna.ical4j.model.TimeZone timezone = registry.getTimeZone(ApplicationState.SYSTEM_TIME_ZONE.getID());

    DtStart start = new DtStart(new DateTime(schedule.getStartDate()));
    start.setTimeZone(timezone);
    vEvent.getProperties().add(start);
    DtEnd end = new DtEnd(new DateTime(schedule.getStartDate().getTime() + schedule.getDurationInMillis()));
    end.setTimeZone(timezone);
    vEvent.getProperties().add(end);

    if (schedule instanceof Period) {

    } else if (schedule instanceof ScheduleDaily) {

      ScheduleDaily scheduleDaily = (ScheduleDaily) schedule;
      Recur recur = new Recur(Recur.DAILY, scheduleDaily.getRepetitions());
      recur.setInterval(scheduleDaily.getFrequency());
      RRule rrule = new RRule(recur);
      vEvent.getProperties().add(rrule);

    } else if (schedule instanceof ScheduleWeekly) {

      ScheduleWeekly scheduleWeekly = (ScheduleWeekly) schedule;
      Recur recur = new Recur(Recur.WEEKLY, scheduleWeekly.getRepetitions());
      recur.setInterval(scheduleWeekly.getFrequency());

      int[] days = scheduleWeekly.getDays();
      for (int i = 0; i < days.length; i++) {
        int day = days[i];
        recur.getDayList().add(getIcalWeekDay(day));
      }

      RRule rrule = new RRule(recur);
      vEvent.getProperties().add(rrule);

    } else if (schedule instanceof ScheduleMonthly) {

      ScheduleMonthly scheduleMonthly = (ScheduleMonthly) schedule;
      Recur recur = new Recur(Recur.MONTHLY, scheduleMonthly.getRepetitions());
      recur.setInterval(scheduleMonthly.getFrequency());

      if (scheduleMonthly.getDayInWeek() == 0) {
        CompanyCalendar cc = new CompanyCalendar(scheduleMonthly.getStartDate());
        recur.getMonthDayList().add(cc.get(CompanyCalendar.DAY_OF_MONTH));

      } else {
        int diw = scheduleMonthly.getDayInWeek();
        int wim = scheduleMonthly.getWeekInMonth();
        wim = wim == 5 ? -1 : wim;
        recur.getDayList().add(new WeekDay(getIcalWeekDay(diw), wim));
      }

      RRule rrule = new RRule(recur);
      vEvent.getProperties().add(rrule);

    } else if (schedule instanceof ScheduleYearly) {

      ScheduleYearly scheduleYearly = (ScheduleYearly) schedule;
      Recur recur = new Recur(Recur.YEARLY, scheduleYearly.getRepetitions());
      recur.setInterval(scheduleYearly.getFrequency());

      CompanyCalendar cc = new CompanyCalendar(scheduleYearly.getStartDate());

      if (scheduleYearly.getDayInWeek() == 0) {

        recur.getMonthDayList().add(cc.get(CompanyCalendar.DAY_OF_MONTH));
        recur.getMonthList().add(cc.get(CompanyCalendar.MONTH) + 1);

      } else {

        recur.getMonthList().add(cc.get(CompanyCalendar.MONTH) + 1);

        int diw = scheduleYearly.getDayInWeek();
        int wim = scheduleYearly.getWeekInMonth();
        wim = wim == 5 ? -1 : wim;
        recur.getDayList().add(new WeekDay(getIcalWeekDay(diw), wim));
      }

      RRule rrule = new RRule(recur);
      vEvent.getProperties().add(rrule);
    }

  }

  private static Event getEventByUID(String UID) {
    Event event;
    event = (Event) PersistenceHome.findUniqueNullIfEmpty(Event.class, "icalId", UID);
    return event;
  }

  public static XXProperty getAttendee(Resource resource, String parameters) {
    String attMess = "CN=\"" + resource.getName() + "\";" + parameters + ":MAILTO"; // notice that attendee requires apex around the name
    String value = resource.getDefaultEmail();
    XXProperty a = new XXProperty("ATTENDEE;" + attMess, value);
    return a;
  }

  private static VEvent createVEventBody(Event event) {
    TimeZoneRegistry registry = TimeZoneRegistryFactory.getInstance().createRegistry();

    ScheduleSupport schedule = (ScheduleSupport) ReflectionUtilities.getUnderlyingObject(event.getSchedule());
    VEvent vEvent = new VEvent();

    Created c = new Created(new DateTime(new Date()));

    vEvent.getProperties().add(c);

    String summaryS = event.getSummary();
    Summary summary = new Summary(summaryS);
    vEvent.getProperties().add(summary);

    setSchedule(schedule, vEvent);

    //EXDATE -> exeptions
    if (JSP.ex(event.getExceptions())){
      for (String dt:StringUtilities.splitToSet(event.getExceptions(),",")){
        ExDate exDate = new ExDate();
        exDate.setTimeZone(vEvent.getStartDate().getTimeZone());
        exDate.getDates().add(new Date(Long.parseLong(dt)));
        vEvent.getProperties().add(exDate);
      }
    }


    LastModified lm = new LastModified(new DateTime(new Date()));
    vEvent.getProperties().add(lm);

    if (event.getLocation() != null) {
      Location l = new Location(event.getLocation());
      vEvent.getProperties().add(l);
    }
    if(event.isPersonal()){
     vEvent.getProperties().add(Clazz.PRIVATE);
    }else{
      vEvent.getProperties().add(Clazz.PUBLIC);
    }
    //this makes the received event read-only in OSX Mail and no invitation in Out 2007
    /*if (JSP.ex(auth.getDefaultEmail())) {
      Organizer o = null;
      try {
        //o = new Organizer("MAILTO:" + auth.getDefaultEmail());
        o = new Organizer("MAILTO:" + MailHelper.getSystemInternetAddress());
      } catch (URISyntaxException e) {
        throw new PlatformRuntimeException(e);
      }
      vEvent.getProperties().add(o);
    }*/

    Organizer o = null;
    try {
      o = new Organizer("MAILTO:" + MailHelper.getSystemInternetAddress().getAddress());
    } catch (URISyntaxException e) {
      throw new PlatformRuntimeException(e);
    }
    vEvent.getProperties().add(o);



    Priority p = new Priority(5);
    vEvent.getProperties().add(p);

    Sequence s = new Sequence(0);
    vEvent.getProperties().add(s);

    vEvent.getProperties().add(new Uid(event.getIcalId()));

    return vEvent;
  }

  public static class XXProperty extends Property implements Escapable {

    private static final long serialVersionUID = 2331763266954894541L + 1234;

    private String value;

    /**
     * Constructs an uninitialised non-standard property.
     */
    public XXProperty(final String name) {
      //super(name,PropertyFactoryImpl.getInstance()); // new version
      super(name);
    }

    public XXProperty(final String aName, final String aValue) {
      //super(aName,PropertyFactoryImpl.getInstance()); // new version
      super(aName);
      setValue(aValue);
    }

    public XXProperty(final String aName, final ParameterList aList,
                      final String aValue) {
      //super(aName, aList,PropertyFactoryImpl.getInstance()); // new version
      super(aName, aList);
      setValue(aValue);
    }

    public final void setValue(final String aValue) {
      this.value = aValue;
    }

    public final String getValue() {
      return value;
    }

    public final void validate() throws ValidationException {
    }

  }

  public static void enableDebugging() {
    Logger logger = Logger.getLogger("net.fortuna.ical4j");
    logger.removeAllAppenders();
    PatternLayout pl = new PatternLayout();
    pl.setConversionPattern("%d{yyyy MMM dd HH:mm:ss} %5p %c{1}:%L - %m%n");

    ConsoleAppender consoleAppender = new ConsoleAppender(pl);
    consoleAppender.setName("ical4jConsoleAppender");
    logger.addAppender(consoleAppender);

    File log = new File(PlatformConfiguration.logFilesRoot + "ical4j.log");
    if (!log.exists())
      log.getParentFile().mkdirs();
    DailyRollingFileAppender hibFileAppender = null;
    try {
      hibFileAppender = new DailyRollingFileAppender(pl, log.getPath(), "'.'yyyy-ww");
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
    hibFileAppender.setName("ical4jFileAppender");
    logger.addAppender(hibFileAppender);
    logger.setLevel(Level.DEBUG);
  }

  public static void disableDebugging() {
     Logger logger = Logger.getLogger("net.fortuna.ical4j");
    logger.removeAllAppenders();
  }

}
