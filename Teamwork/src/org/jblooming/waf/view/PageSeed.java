package org.jblooming.waf.view;

import org.jblooming.ApplicationException;
import org.jblooming.ontology.*;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.NumberUtilities;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.html.button.Link;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;

import javax.servlet.http.HttpServletRequest;
import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.*;


/**
 * @author pietro polsinelli info@twproject.com
 */
public class PageSeed {

  public String href;
  public Serializable mainObjectId;

  private ClientEntries clientEntries = new ClientEntries();

  public String command;

  private boolean loginRequiring = true;

  private boolean popup;

  private String resubmitCheck;

  private boolean noResubmit = false;

  // this is used to generate the view id.
  //  true it generates _VP_V_ID=1819467356&
  //  false  --nothing--
  public boolean disableCache= true;


  public PageSeed() {
  }

  public PageSeed(String href) {
    this.setHref(href);
  }


  public String getName() {
    return getName(getHref());
  }


  public PageSeed getNewInstance() {
    PageSeed url = new PageSeed(getHref());

    url.setMainObjectId(getMainObjectId());

    if (getClientEntries().getEntryKeys() != null) {
      Iterator<String> i = getClientEntries().getEntryKeys().iterator();
      while (i.hasNext()) {
        ClientEntry entry = getClientEntries().getEntry((String) i.next());
        url.addClientEntry(entry.getNewInstance());
      }
    }

    url.setPopup(isPopup());

    if (getCommand() != null)
      url.setCommand(getCommand());

    url.setLoginRequiring(isLoginRequiring());

    return url;
  }

  public PageSeed addClientEntry(ClientEntry ce) {
    if (ce != null && ce.name != null) {
      getClientEntries().addEntry(ce);
    }
    return this;
  }

  public PageSeed addClientEntry(String key, String value) {
    return addClientEntry(new ClientEntry(key, value));
  }

  public PageSeed addClientEntry(String key, Serializable value) {
    return addClientEntry(key, value.toString());
  }

  public PageSeed addClientEntry(String s, Date date) {
    return addClientEntry(s, DateUtilities.dateToString(date));
  }

  public PageSeed addClientEntry(String key, SerializedList list) {
    return addClientEntry(key, list.serialize());
  }

  public PageSeed addClientEntry(String key, SerializedMap map) {
    return addClientEntry(key, map.serialize());
  }

  public PageSeed addClientEntry(String key, Object o) {
    return addClientEntry(key, o.toString());
  }


  public PageSeed addClientEntry(String s, Integer value) {
    return addClientEntry(s, value + "");
  }

  public PageSeed addClientEntry(String s, Double value) {
    return addClientEntry(s, value, NumberUtilities.DEFAULT_DECIMAL_PLACES);
  }

	public PageSeed addClientEntry(String s, Double value, int decimalPlaces) {
		return addClientEntry(s, NumberUtilities.decimalNoGrouping(value, decimalPlaces));
  }

  public PageSeed addClientEntryCurrency(String s, Double value) {
    return addClientEntry(s, NumberUtilities.currency(value));
  }

  /**
   * this will return in hours 
   */
  public PageSeed addClientEntryTime(String s, Long millis) {
    return addClientEntry(s, DateUtilities.getMillisInHoursMinutes(millis));
  }

  public void addClientEntry(String s, PersistentText persistentText) {
    if (persistentText!=null)
      addClientEntry(s,persistentText.getText());
  }


  public void addClientEntry(String s, Identifiable identifiable) {
    if (identifiable!=null)
      addClientEntry(s,identifiable.getId());
  }

  public void addClientEntry(String s, PersistentFile persistentFile) {
    if (persistentFile!=null)
      addClientEntry(s,persistentFile.serialize());
  }

  public void addClientEntry(String s, Boolean truefalse) {
    addClientEntry(s, truefalse ? Fields.TRUE : Fields.FALSE);
  }

  public void addClientEntries(ClientEntries ce) {
    if (ce != null && ce.getEntryKeys() != null && ce.getEntryKeys().size() > 0) {
      Iterator i = ce.getEntryKeys().iterator();
      while (i.hasNext()) {
        getClientEntries().addEntry(ce.getEntry((String) i.next()));
      }
    }
  }


  public void removeEntry(String key) {
    clientEntries.deleteEntry(key);
  }

  public void removeEntriesMatching(String cePartialName) {
    for (ClientEntry ce : getClientEntriesSet()) {
      if (ce.name.contains(cePartialName))
        removeEntry(ce.name);
    }
  }


  public ClientEntries getClientEntries() {
    if (clientEntries == null)
      clientEntries=new ClientEntries();    
    return clientEntries;
  }

  public void setClientEntries(ClientEntries clientEntries) {
    this.clientEntries = clientEntries;
  }


  public Set<ClientEntry> getClientEntriesSet() {
    return new HashSet(clientEntries.getClientEntries());
  }

  public ClientEntry getEntry(String clientEntryName) {
    ClientEntry result = new ClientEntry(null, null);
    if (clientEntryName != null) {
      ClientEntry clientEntry = clientEntries.getEntry(clientEntryName);
      if (clientEntry != null )
        result = clientEntry;
    }
    return result;
  }


  public ClientEntry getEntryAndSetRequired(String clientEntryName) {
    ClientEntry ce = getEntry(clientEntryName);
    ce.required = true;
    return ce;
  }



  public static PageSeed getConfiguredInstance(String urlName) throws ApplicationException {
    Object o = ApplicationState.getConfiguredUrls().get(urlName);
    if (o == null)
      throw new ApplicationException(urlName + " is not a configured view");
    PageSeed u = (PageSeed) o;
    return u.getNewInstance();
  }


  public synchronized String toString() {
    return this.toLinkToHref();
  }

  public String toLinkToHref() {
    return new Link(this).getHref();
  }

  public void setCommand(String command) {
    this.command = command;

  }

  public static String getName(String href) {
    return StringUtilities.stripToLegal(href).replaceAll("\\.", "_");
  }

  public void setMainObjectId(Serializable id) {
    this.mainObjectId = id;

  }

  public static String getContextualHref(String href, HttpServletRequest request) {

    return request.getContextPath() + ((request.getContextPath().endsWith("/") || href.startsWith("/")) ? "" : "/") + href;
  }

  public boolean isPopup() {
    return popup;
  }

  public void setPopup(boolean popup) {
    this.popup = popup;
  }

  public String getHref() {
    return href;
  }

  public void setHref(String href) {
    this.href = href;
  }

  public Serializable getMainObjectId() {
    return mainObjectId;
  }

  public String getCommand() {
    return command;
  }

  public boolean isLoginRequiring() {
    return loginRequiring;
  }

  public void setLoginRequiring(boolean loginRequiring) {
    this.loginRequiring = loginRequiring;
  }

  public String getResubmitCheck() {
    return resubmitCheck;
  }

  public void setResubmitCheck(String resubmitCheck) {
    this.resubmitCheck = resubmitCheck;
  }


  public String getJspName() {
    return href.substring(href.lastIndexOf("/") + 1);
  }



  /**
   * @param page
   * @return the page from the root of web application
   */
  public PageSeed pageFromWebApp(String page) {
    if (!page.startsWith("/"))
      page = "/" + page;
    return new PageSeed(ApplicationState.contextPath + page);
  }


  /**
   * @param page
   * @return the page from the root of applications
   */
  public PageSeed pageFromApplications(String page) {
    if (!page.startsWith("/"))
      page = "/" + page;
    return new PageSeed(ApplicationState.contextPath + "/applications" + page);
  }


  /**
   * @param page
   * @return the page from root of current application with context path
   */
  public PageSeed pageFromCommonsRoot(String page) {
    if (!page.startsWith("/"))
      page = "/" + page;
    return new PageSeed(ApplicationState.contextPath + "/commons" + page);
  }

  public String getCEErrors(String lineSeparator){
    String ret="";

    for (ClientEntry ce : getClientEntriesSet()) {
      if (ce.errorCode!=null){
        ret=ret+ I18n.get(ce.name) +": "+I18n.get(ce.errorCode)+lineSeparator;
      }
    }
    return ret;
  }

  public boolean hasEntry(String name){
    return getClientEntries().hasEntry(name);
  }
}
