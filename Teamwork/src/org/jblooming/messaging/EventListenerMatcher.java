package org.jblooming.messaging;

import org.jblooming.security.License;
import org.hibernate.Query;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.ontology.PerformantNode;
import org.jblooming.ontology.SerializedMap;
import org.jblooming.operator.Operator;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.scheduler.ExecutableSupport;
import org.jblooming.scheduler.JobLogData;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.constants.OperatorConstants;
import org.jblooming.waf.settings.I18n;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class EventListenerMatcher extends ExecutableSupport {

  public JobLogData run(JobLogData jobLogData) throws Exception {
    PersistenceContext pc = null;

    try {

      pc = PersistenceContext.getDefaultPersistenceContext();

      CompanyCalendar cc = new CompanyCalendar();
      cc = new CompanyCalendar();
      cc.add(CompanyCalendar.DAY_OF_YEAR, -7);
      //remove old somthing happened if more then 100

      String hqlSomethingHappened = "select count(sh) from " + SomethingHappened.class.getName() + " as sh";
      OqlQuery oql = new OqlQuery(hqlSomethingHappened);
      long count = (Long) oql.uniqueResultNullIfEmpty();
      if (count > 100) {
        cc = new CompanyCalendar();
        cc.add(CompanyCalendar.DAY_OF_YEAR, -1);
        OqlQuery oql2 = new OqlQuery("delete from " + SomethingHappened.class.getName() + " as sh where sh.happenedAt < :when");
        oql2.getQuery().setDate("when", cc.getTime());
        oql2.getQuery().executeUpdate();
      }

      pc.checkPoint();
      // get all the events
      OqlQuery eventQH = new OqlQuery("from " + SomethingHappened.class.getName());
      List<SomethingHappened> events = eventQH.list();

      // loop through each event
      for (SomethingHappened event : events) {
        try {
          if (License.assertLevel(10)) {
            List<Listener> listeners = getListeners(event);
            // these listeners already match the event
            // generate the appropriate message

            for (Listener listener : listeners) {
              generateAndPersistMessage(listener, event);
              listener.setLastMatchingDate(new Date());
              if (listener.isOneShot())
                listener.remove();
              else
                listener.store();
            }
          }
        } catch (Throwable e) {
          Tracer.platformLogger.error("EventListenerMatcher error", e);
        }
        event.remove();
      }

      pc.commitAndClose();

      jobLogData.notes = jobLogData.notes + "EventListenerMatcher executed on " + DateUtilities.dateAndHourToString(new Date());
    } catch (Throwable e) {
      Tracer.platformLogger.error("EventListenerMatcher error", e);
      if (pc != null) {
        pc.rollbackAndClose();
      }
      jobLogData.successfull = false;
    }
    return jobLogData;
  }

  private List<Listener> getListeners(SomethingHappened event) throws PersistenceException {

    List<Listener> listeners = new ArrayList();
    List<Listener> listeners2 = new ArrayList();
    if (event.getHappeningExpiryDate() == null || event.getHappeningExpiryDate().getTime() > System.currentTimeMillis()) {

      List<String> ids = new ArrayList();

      //does event refer to node ?
      boolean refersToNode = false;
      Class main = null;
      try {
        main = Class.forName(event.getTheClass());
      } catch (ClassNotFoundException e) {
        throw new PlatformRuntimeException(e);
      }
      if (event.getTheClass() != null) {

        refersToNode = ReflectionUtilities.extendsOrImplements(main, PerformantNode.class);

      }
      //get ancestors
      if (refersToNode) {
        try {
          PerformantNode pn = (PerformantNode) PersistenceHome.findByPrimaryKey(main, event.getIdentifiableId());
          if (pn != null && pn.getAncestorIds() != null) {
            ids = StringUtilities.splitToList(pn.getAncestorIds(), PerformantNode.SEPARATOR);
          }
        } catch (Throwable e) {
          //do nothing
        }
      }
      ids.add(event.getIdentifiableId());

      String hql = "from " + Listener.class.getName() + " as l ";
      hql = hql + " where l.theClass=:theClass ";
      hql = hql + " and (l.identifiableId in (:identifiableIds) or l.identifiableId is null) ";
      hql = hql + " and (l.eventType = :eventType or l.eventType is null) ";
      hql = hql + " and ( l.validityEnd is null or (l.validityEnd >= :nownow) )";
      hql = hql + " and ( l.validityStart is null or (l.validityStart <= :nownow) )";

      OqlQuery oql = new OqlQuery(hql);
      oql.getQuery().setString("theClass", event.getTheClass());
      oql.getQuery().setParameterList("identifiableIds", ids);
      oql.getQuery().setString("eventType", event.getEventType());
      oql.getQuery().setTimestamp("nownow", event.getHappenedAt());

      listeners = oql.list();
      for (Listener l : listeners) {
        if (l.getIdentifiableId().equals(event.getIdentifiableId()) || l.isListenDescendants()) {
          listeners2.add(l);
        }
      }
    }
    return listeners2;
  }

  public static void generateAndPersistMessage(Listener listener, SomethingHappened s) throws StoreException {

    Operator op = listener.getOwner();

    // avoid to bother user with its events  or notify disabled users
    if (!op.isEnabled() || (op.equals(s.getWhoCausedTheEvent()) && !Fields.TRUE.equals(op.getOption(OperatorConstants.NOTIFY_MY_EVENTS_TO_MYSELF))))
      return;

    SerializedMap<String, String> mps = s.getMessageParams();
    String template=I18n.get(JSP.w(s.getMessageTemplate()),mps);

    List<String> medias = StringUtilities.splitToList(listener.getMedia(), ",");

    for (String media : medias) {
      Message message = new Message();
      if (op != null)
        message.setToOperator(op);
      else if (s.getWhoCausedTheEvent() != null)
        message.setToOperator(s.getWhoCausedTheEvent());

      message.setDefaultExpires();

      message.setMedia(media);

      if (s.getWhoCausedTheEvent() != null)
        message.setFromOperator(s.getWhoCausedTheEvent());

      // if there is a param called "SUBJECT_REPLACEMENT" it will be used instead of eventType
      if (JSP.ex(mps.get("SUBJECT_REPLACEMENT")))
        message.setSubject(I18n.get(mps.get("SUBJECT_REPLACEMENT")));
      else
        message.setSubject(I18n.get("EVENT_" + s.getEventType()));

      // if there is a param called "subject" or "SUBJECT" it will be appended to subject
      String addSubject = JSP.w(mps.get("subject")) + "" + JSP.w(mps.get("SUBJECT"));
      if (JSP.ex(addSubject))
        message.setSubject(message.getSubject() + ": " + addSubject);


      if (MessagingSystem.Media.RSS.toString().equals(media)) {
        String link = s.getLink();
        link = link.substring(link.indexOf("\"") + 1);
        link = link.substring(0, link.indexOf("\""));
        message.setLink(link);
      } else
        message.setLink(s.getLink());

      message.setMessageBody(template);
      message.setReceived(new Date());


      //remove older duplicated messages
      removeDuplicates(message);


      message.store();
    }
  }


  private static int removeDuplicates(Message likeThis){

    if (!JSP.ex(likeThis.getSubject()) || !JSP.ex(likeThis.getMessageBody()) || !JSP.ex(likeThis.getMedia()) || likeThis.getToOperator()==null)
      return 0;  // se il messaggio non è specifico ci si leva

    String hql="delete from "+Message.class.getName()+" as m where m.subject=:sbj and m.toOperator=:op and m.media=:med";// and m.messageBody=:bod ";

    if (!likeThis.isNew())
      hql+=" and m.id<>:id";

    Query query = new OqlQuery(hql).getQuery();
    query.setString("sbj",likeThis.getSubject());
    query.setEntity("op", likeThis.getToOperator());
    query.setString("med", likeThis.getMedia());
    //query.setString("bod",likeThis.getMessageBody());

    if (!likeThis.isNew())
      query.setSerializable("id", likeThis.getId());

    int q = query.executeUpdate();
    return q;

  }

}
