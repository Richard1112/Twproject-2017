package org.jblooming.operator.businessLogic;

import com.twproject.agenda.Event;
import com.twproject.agenda.IcalUtilities;
import org.jblooming.messaging.Listener;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.operator.Operator;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.constants.OperatorConstants;
import static org.jblooming.waf.constants.OperatorConstants.*;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.RestState;

import java.io.IOException;
import java.util.List;


/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class OptionAction {


  public void cmdEdit(PageState pageState) throws PersistenceException {

    make(pageState);
  }

  protected void make(PageState pageState) {
    make(pageState.getLoggedOperator(), pageState);
  }

  public static void make(Operator operator, RestState pageState) {

    pageState.addClientEntry(FLD_SELECT_LANG, operator.getLanguage());

    /*String whb = operator.getOption(FLD_WORKING_HOUR_BEGIN);
    if (whb != null) {
      pageState.addClientEntry(FLD_WORKING_HOUR_BEGIN, whb);
    }

    String whe = operator.getOption(FLD_WORKING_HOUR_END);
    if (whe != null) {
      pageState.addClientEntry(FLD_WORKING_HOUR_END, whe);
    }*/

    String whe = operator.getOption(FLD_WORKING_HOUR_TOTAL);
    if (whe != null) {
      pageState.addClientEntry(FLD_WORKING_HOUR_TOTAL, whe);
    }

    pageState.addClientEntry(OP_PAGE_SIZE, operator.getOption(OP_PAGE_SIZE));
    String s = operator.getOption(FLD_CURRENT_SKIN);
    if (!JSP.ex(s))
      s = ApplicationState.getApplicationSetting(FLD_CURRENT_SKIN);
    pageState.addClientEntry(FLD_CURRENT_SKIN, s);

    String operatorOption = operator.getOption(SEND_EVENT_BY_ICAL);
    pageState.addClientEntry(SEND_EVENT_BY_ICAL, operatorOption);
    MessagingSystem.makeMedias(operator.getOption(MEDIA_PREFERRED_CHANNEL), MEDIA_PREFERRED_CHANNEL + "_", pageState);

    pageState.addClientEntry("PREFERRED_COLOR", operator.getOption("PREFERRED_COLOR"));

    //pageState.addClientEntry("GOOGLE_LOGIN_USER", operator.getOption("GOOGLE_LOGIN_USER"));
    pageState.addClientEntry("SEND_TO_GOOGLE", operator.getOption("SEND_TO_GOOGLE"));
    pageState.addClientEntry("DEFAULT_EXT_CALENDAR", operator.getOption("DEFAULT_EXT_CALENDAR"));
    pageState.addClientEntry( OperatorConstants.NOTIFY_MY_EVENTS_TO_MYSELF, operator.getOption( OperatorConstants.NOTIFY_MY_EVENTS_TO_MYSELF));
    pageState.addClientEntry("REMEMBER_LOGIN" , operator.getOption( "REMEMBER_LOGIN"));


    pageState.addClientEntry("HOME_PAGE", operator.getOption("HOME_PAGE"));
  }

  public void cmdSave(PageState pageState, String contextPath) throws PersistenceException {

    SessionState sessionState = pageState.sessionState;
    Operator logged = pageState.getLoggedOperator();
    logged = (Operator) PersistenceHome.findByPrimaryKey(Operator.class, logged.getId());
    pageState.setMainObject(logged);

    saveOptions(logged, pageState);

    String value = pageState.getEntry(FLD_CURRENT_SKIN).stringValueNullIfEmpty();
    if (value != null) {
      sessionState.setSkinName(value);
    }

    logged.store();
  }

  public static void saveOptions(Operator operator, RestState restState) {

    String value = restState.getEntry(FLD_SELECT_LANG).stringValueNullIfEmpty();
    operator.setLanguage(value);

    value = restState.getEntry(OP_PAGE_SIZE).stringValueNullIfEmpty();
    if (value != null)
      operator.putOption(OP_PAGE_SIZE, value);
    else
      operator.getOptions().remove(OP_PAGE_SIZE);

    operator.putOption(SEND_EVENT_BY_ICAL, restState.getEntry(SEND_EVENT_BY_ICAL).checkFieldHtmlValue());

    /*value = restState.getEntry(FLD_WORKING_HOUR_BEGIN).stringValueNullIfEmpty();
    if (value != null)
      operator.putOption(FLD_WORKING_HOUR_BEGIN, value);
    else
      operator.getOptions().remove(FLD_WORKING_HOUR_BEGIN);

    value = restState.getEntry(FLD_WORKING_HOUR_END).stringValueNullIfEmpty();
    if (value != null)
      operator.putOption(FLD_WORKING_HOUR_END, value);
    else
      operator.getOptions().remove(FLD_WORKING_HOUR_END);
    */

    value = restState.getEntry(FLD_WORKING_HOUR_TOTAL).stringValueNullIfEmpty();
    if (value != null)
      operator.putOption(FLD_WORKING_HOUR_TOTAL, value);
    else
      operator.getOptions().remove(FLD_WORKING_HOUR_TOTAL);

    value = restState.getEntry(FLD_CURRENT_SKIN).stringValueNullIfEmpty();
    if (value != null) {
      operator.putOption(FLD_CURRENT_SKIN, value);
    } else
      operator.getOptions().remove(FLD_CURRENT_SKIN);

    value = restState.getEntry("PREFERRED_COLOR").stringValueNullIfEmpty();
    if (value != null) {
      operator.putOption("PREFERRED_COLOR", value);
    } else
      operator.getOptions().remove("PREFERRED_COLOR");

    value = restState.getEntry("REMEMBER_LOGIN").stringValueNullIfEmpty();
       if (value != null)
     operator.putOption("REMEMBER_LOGIN", value);

    /*
    value = restState.getEntry("GOOGLE_LOGIN_USER").stringValueNullIfEmpty();
    if (value != null) {
      operator.putOption("GOOGLE_LOGIN_USER", value);
    } else
      operator.getOptions().remove("GOOGLE_LOGIN_USER");
    */
    value = restState.getEntry("SEND_TO_GOOGLE").stringValueNullIfEmpty();
    if (value != null)
      operator.putOption("SEND_TO_GOOGLE", value);
    else
      operator.getOptions().remove("SEND_TO_GOOGLE");


    value = restState.getEntry("DEFAULT_EXT_CALENDAR").stringValueNullIfEmpty();

    //il link dei calendari è anche in sessione
    // se c'è una sessione è stiamo salvaldo le opzioni del logged
    // devo forzare un refresh degli iCal la prossima volta che entro nell'agenda
    if (!(value+"").equals(operator.getOption("DEFAULT_EXT_CALENDAR")+"") && operator.equals(restState.getLoggedOperator()) && restState instanceof PageState){
      PageState pageState = (PageState) restState;
      pageState.sessionState.getAttributes().remove("EXTERNAL_CAL_REFRESH_TIME");

      if (JSP.ex(value)) {
        try {
          List<Event> iCalEvents= IcalUtilities.getEventsFromURLs(value);
          pageState.sessionState.setAttribute("DEFAULT_EXT_CALENDAR", value);
          pageState.sessionState.setAttribute("DEFAULT_CAL_EVENTS", iCalEvents);
          pageState.sessionState.setAttribute("EXTERNAL_CAL_REFRESH_TIME", System.currentTimeMillis());

        } catch (IOException e) {  // errore lanciato se l'url non è scritto corretamente
          pageState.getEntry("DEFAULT_EXT_CALENDAR").errorCode = I18n.get("CALENDAR_LINK_NOT_VALID");
        } catch (Throwable e) { // errori lanciati in generale se il file è corrotto(formato ical non valido)---> ho messo questo perche lancia tanti tipi diversi errore
          pageState.getEntry("DEFAULT_EXT_CALENDAR").errorCode = I18n.get("CALENDAR_FILE_MALFORMED");
        }

      } else {
        pageState.sessionState.getAttributes().remove("DEFAULT_EXT_CALENDAR");
        pageState.sessionState.getAttributes().remove("DEFAULT_CAL_EVENTS");
      }

    }

    if (value != null) {
      operator.putOption("DEFAULT_EXT_CALENDAR", value);
    }else {
      operator.getOptions().remove("DEFAULT_EXT_CALENDAR");

    }

  }

  public static void cmdUpdateLoggedOption(RestState pageState, String optionName, String optionValue) throws PersistenceException {
    Operator logged = pageState.getLoggedOperator();
    if (logged != null) {
      logged = (Operator) PersistenceHome.findByPrimaryKey(Operator.class, logged.getId());
      logged.putOption(optionName, optionValue);
      logged.store();
    }
  }

  public void cmdDelete(PageState pageState) throws PersistenceException {
    String id = pageState.getMainObjectId().toString();
    Listener l = (Listener) PersistenceHome.findByPrimaryKey(Listener.class, id);
    if (l != null)
      l.remove();
    pageState.stopPageAfterController = true;
    //make(pageState);
  }

  public void cmdRemoveOption(PageState pageState) throws PersistenceException {

    String[] opId_key = ((String) pageState.mainObjectId).split("___");

    String opId = opId_key[0];
    String key = opId_key[1];

    Operator operator = (Operator) PersistenceHome.findByPrimaryKey(Operator.class, opId);
    operator.getOptions().remove(key);
    operator.store();

    SessionState sessionState = pageState.sessionState;
    Operator logged = pageState.getLoggedOperator();

    if (logged.equals(operator))
      logged.getOptions().remove(key);
  }

}
