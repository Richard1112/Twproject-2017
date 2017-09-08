package org.jblooming.waf;

import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.operator.Operator;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.HashTable;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.html.state.ScreenElementStatus;
import org.jblooming.waf.settings.Application;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.ApplicationSupport;
import org.jblooming.waf.settings.PlatformConfiguration;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import javax.servlet.http.HttpSessionBindingEvent;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


public class SessionState implements javax.servlet.http.HttpSessionBindingListener {

  private Map attributes = new HashTable();

  private PageSeed loginPendingUrl;

  private Locale locale;
  private TimeZone timeZone;

  private String[] localizedDateFormats=null;

  private int opId = -1;

  private String skinName;

  public Map<String, ScreenElementStatus> screenElementsStatus = new HashMap<String, ScreenElementStatus>();

  public static int totalSessionStates = 0;

  /**
   * @deprecated
   */
  public static Set<Integer> totalOpIds = new HashSet<Integer>();

  private Application application;
  public String sessionId;

  public SessionState(HttpSession session) {
    sessionId =session.getId();
  }


  public PageSeed getLoginPendingUrl() {
    return loginPendingUrl;
  }

  public void setLoginPendingUrl(PageSeed loginPendingUrl) {
    this.loginPendingUrl = loginPendingUrl;
  }

  public void setAttribute(Object key, Object object) {
    if (getAttributes() == null) {
      setAttributes(new HashTable());
    }
    if (object == null)
      getAttributes().remove(key);
    else
      getAttributes().put(key, object);
  }

  public Object getAttribute(Object key) {
    if (getAttributes() == null) {
      setAttributes(new HashTable());
    }
    return getAttributes().get(key);
  }


  /**
   *
   * @return the Local from user settings or the system one
   */
  public static Locale getLocale() {
    SessionState ss = SessionState.getCurrentSessionState();
    if (ss == null)
      return ApplicationState.SYSTEM_LOCALE;
    else
      return ss._getLocale();
  }


  private Locale _getLocale() {
    if (locale == null)
      _loadLocTZ();
    return locale;
  }


  public static TimeZone getTimeZone() {
    return ApplicationState.SYSTEM_TIME_ZONE;
    /*
    SessionState ss = getCurrentSessionState();
    if (ss == null)
      return ApplicationState.SYSTEM_TIME_ZONE;
    else
      return ss._getTimeZone();*/
  }

  private TimeZone _getTimeZone() {
    if (timeZone == null)
      _loadLocTZ();
    return timeZone;
  }

  public void resetLocaleAndTimeZone() {
    this.locale = null;
    this.timeZone = null;
    //when set locale reset date formats
    localizedDateFormats=null;
  }

  private void _loadLocTZ() {
    try {
      Operator operator = getOpid()>-1 ? Operator.load(getOpid()) :  null;
      if (operator!=null){
        locale = operator.getLocale();
        timeZone=operator.getTimeZone();
      } else{
        locale = ApplicationState.SYSTEM_LOCALE;
        timeZone= ApplicationState.SYSTEM_TIME_ZONE;
      }
    } catch (PersistenceException e) {
      throw new PlatformRuntimeException(e);
    }
  }


  public String getLocalizedDateFormat(int field){
    if (localizedDateFormats==null){
      localizedDateFormats =DateUtilities.getLocalizedDateFormats(getLocale());
    }
    return localizedDateFormats[field];
  }

  public boolean isOperatorLogged() {
    return (opId != -1);
  }

  public Map getAttributes() {
    return attributes;
  }

  public void setAttributes(Map attributes) {
    this.attributes = attributes;
  }


  public void setLoggedOperator(Operator op) throws PersistenceException, ApplicationException {
    this.opId = (Integer) op.getId();
    totalOpIds.add(opId);
  }


  public int getOpid(){
    return opId;
  }

  public static SessionState _getSessionState(HttpServletRequest request) {
    HttpSession session = request.getSession(true);
    SessionState ss = (SessionState) session.getAttribute(Fields.SESSION);

    if (ss == null) {
      ss = new SessionState(session);

      if (session != null) {
        session.setAttribute(Fields.SESSION, ss);
        session.setAttribute("__ACTIVE_SESSIONS__", new SessionCounter());
      }
    }

    if (ss.locale == null) {
      ss.locale=request.getLocale();
    }

    return ss;
  }

  public String getSkinName() {
    return skinName;
  }

  public void setSkinName(String skinName) {
    this.skinName = skinName;
  }

  public Application getApplication() {
    if (application == null)
      application = ApplicationState.platformConfiguration.getDefaultApplication();
    return application;
  }

  public void setApplication(Application application) {
    this.application = application;
  }

  public void valueBound(HttpSessionBindingEvent event) { // add graziella - 23/10/2008
    Collection<Application> apps = ApplicationState.platformConfiguration.applications.values();
    for (Application app : apps)
      if (app instanceof ApplicationSupport) {
        ApplicationSupport _app = (ApplicationSupport) app;
        _app.sessionStateValueBound();
      }
  }

  public void valueUnbound(HttpSessionBindingEvent event) { // add graziella - 23/10/2008
    Collection<Application> apps = ApplicationState.platformConfiguration.applications.values();
    for (Application app : apps)
      if (app instanceof ApplicationSupport) {
        ApplicationSupport _app = (ApplicationSupport) app;
        _app.sessionStateValueUnbound();
      }

    if (opId > 0) {
    PersistenceContext pc = null;
    try {
        pc = new PersistenceContext();
        Operator op = (Operator) PersistenceHome.findByPrimaryKey(PlatformConfiguration.defaultOperatorSubclass, opId, pc);
        if (op != null) {
        final Map screenElementsStatuses = this.screenElementsStatus;
        if (screenElementsStatuses != null && screenElementsStatuses.size() > 0) {
          for (Iterator iterator = screenElementsStatuses.keySet().iterator(); iterator.hasNext();) {
            final String key = (String) iterator.next();
            ScreenElementStatus screenElementStatus = (ScreenElementStatus) screenElementsStatuses.get(key);
            String value = screenElementStatus.toPersistentString(key);
            if (JSP.ex(value))
              op.putOption(ScreenElementStatus.SES_QUALIFIER + key, value);
            else
              op.getOptions().remove(ScreenElementStatus.SES_QUALIFIER + key);
          }
        }

          op.setLastRequestOn(new Date());
        op.store(pc);
        totalOpIds.remove(op.getId());
      }
        pc.commitAndClose();
    } catch (Throwable throwable) {
        if (pc != null)
          pc.rollbackAndClose();
      Tracer.platformLogger.warn("Attempting to find operator in session and save it in valueUnbound - failed: " + throwable.getMessage(), throwable);
      }
    }
  }


  public void setApplication(String applicationName) {
    setApplication(ApplicationState.platformConfiguration.applications.get(applicationName));
  }


  public static SessionState getCurrentSessionState() {
    SessionState ret=null;
    PageState pageState = PageState.getCurrentPageState();
    if (pageState!=null )//&& pageState instanceof PageState)
      ret=((PageState)pageState).sessionState;
    return ret;
  }

}