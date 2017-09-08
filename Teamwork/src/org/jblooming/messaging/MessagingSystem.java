package org.jblooming.messaging;

import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.operator.Operator;
import org.jblooming.oql.QueryHelper;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.constants.Fields;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.view.RestState;

import java.io.Serializable;
import java.util.*;

/**
 * This should work as follows:
 * if something happens, and there is a ready listener for that something,
 * a message is created and enqued to be sent.
 * <p/>
 * An example is: you subscribe "changes on task"
 * <p/>
 * A message may be enqueued also directly e.g. when the user wants to send it,
 * and it may be enqueued in several media at the same time.
 */
public class MessagingSystem implements Serializable {

  public static int NUMBER_OF_ATTEMPTS = 10;
  public static long DELAY_BETWEEN_ATTEMPTS = CompanyCalendar.MILLIS_IN_MINUTE * 10;

  public static final String TIMER_EVENT = "TIMER_EVENT";

  public static String mediaSubscribed(String prefix, PageSeed pageSeed) {
    String mediaSubscribed = "";
    for (Media media : activeMedia) {
      if (pageSeed.getEntry(prefix + media.toString()).checkFieldValue()) {
        mediaSubscribed = mediaSubscribed + (mediaSubscribed.length() > 0 ? "," : "") + media.toString();
      }
    }
    return mediaSubscribed;
  }

  public static void makeMedias(String medias, String entryPrefix, RestState pageState) {
    if (medias != null) {
      List<String> mediasL = StringUtilities.splitToList(medias, ",");
      for (String media : mediasL) {
        pageState.addClientEntry(entryPrefix + media, Fields.TRUE);
      }
    }
  }

  public static List<Media> mediaSubscribedList(String prefix, RestState pageState) {
    List<Media> mediaSubscribed = new ArrayList();
    for (Media media : activeMedia) {
      if (pageState.getEntry(prefix + media.toString()).checkFieldValue()) {
        mediaSubscribed.add(media);
      }
    }
    return mediaSubscribed;
  }

  public enum Media {
    STICKY, EMAIL, SMS, IM, NEWS, BLOG, RSS, DIGEST, LOG
  }

  public static List<Media> activeMedia = new ArrayList();



  public static int getUnreadMessageCount(Operator operator, String media) throws PersistenceException {
    String hql="select count(id) from "+Message.class.getName()+" as m where toOperator=:op";
    QueryHelper qhelp = new QueryHelper(hql);
    qhelp.addParameter("op", operator);
    if (JSP.ex(media)){
      qhelp.addOQLClause("m.media=:mm","mm",media);
    }
    qhelp.addOQLClause("m.readOn is null");
    Object res = qhelp.toHql().uniqueResultNullIfEmpty();
    return res==null?0: ((Long)res).intValue();
  }

}


