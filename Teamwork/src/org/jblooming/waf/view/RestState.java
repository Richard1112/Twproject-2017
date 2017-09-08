package org.jblooming.waf.view;

import org.jblooming.ApplicationException;
import org.jblooming.InitializationRuntimeException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.ontology.Identifiable;
import org.jblooming.operator.Operator;
import org.jblooming.page.Page;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.utilities.HashTable;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;

import java.lang.reflect.InvocationTargetException;
import java.util.*;

public class RestState extends PageSeed {
  protected Operator operator = null; // added after reforma 7/10/2010
  public Map<String, Page> pages = new HashTable();
  private Identifiable mainObject;
  public Map attributes = new HashMap();

  public boolean multipart = false;

  /**
   * this list is used to manage communication from controller to the page. CE errors are not inserted ther, but displayed automatically by the component
   */
  public Set<PageState.Message> messagesFromController = new HashSet<PageState.Message>();


  public RestState() {
  }

  public RestState( Operator operator) {
    this();
    this.operator=operator;
  }

  public RestState(String url) {
    this();
    if (url == null || url.trim().length() == 0)
      throw new PlatformRuntimeException("BuildView invalid parameters.");
    setHref(url);
  }


  /**
   * @deprecated use I8n.get(name)
   * @param name
   * @return
   */
  public String getI18n(String name) {
    return I18n.get(name);
  }

  public Identifiable getMainObject() {
    return mainObject;
  }

  public void setMainObject(Identifiable mainObject) {
    this.mainObject = mainObject;
    if (this.mainObject !=null)
      this.mainObjectId=mainObject.getId(); // questa è una scommessa !!!!!!
  }

  public Object getAttribute(String key) {
    return attributes.get(key);
  }

  public void setAttribute(String key, Object value) {
    attributes.put(key, value);
  }

  public Page getPage() {
    return pages.get("DEFAULT_PAGE");
  }

  public void setPage(Page page) {
    this.pages.put("DEFAULT_PAGE", page);
  }

  public boolean validEntries() {
    return getClientEntries().validEntries();
  }

  public ClientEntry getEntryOrDefault(String name) {
    ClientEntry ce = getEntry(name);
    ce.persistInOptions = true;
    if (ce.name == null) {
      Operator op = getLoggedOperator();
      String option = null;
      if (op!=null)
         option=op.getOptionOrDefault(name);
      if (option != null) {
        ce.name = name;
        ce.setValue(option);
        addClientEntry(ce);
      }
    }
    return ce;
  }

  public ClientEntry getEntryOrDefault(String name, String defaultValue) {
    ClientEntry ce = getEntryOrDefault(name);
    if (ce.stringValueNullIfEmpty() == null) {
      ce.name = name;
      ce.setValue(defaultValue);
      addClientEntry(ce);
    }
    return ce;
  }

  public void saveEntriesInDefaults() throws PersistenceException {
    Operator op = getLoggedOperator();
    if (op != null) {
      boolean changedSomeOption = false;
      for (ClientEntry ce : getClientEntriesSet()) {
        if (ce.persistInOptions) {

          String value = ce.stringValueNullIfEmpty();
          String optValue = op.getOption(ce.name);
          if (value != null && !value.equals(optValue)) {
            op.putOption(ce.name, ce.stringValueNullIfEmpty());
            changedSomeOption = true;
          } else if (value == null && optValue != null) {
            op.getOptions().remove(ce.name);
            changedSomeOption = true;
          }
        }
      }
      if (changedSomeOption) {
        op.store();
      }
    }
  }

  public void setError(String errorCode) {
    //create an error to force FCF to rollback transaction
    ClientEntry ce = new ClientEntry("__ERROR", "error");
    ce.errorCode= JSP.ex(ce)? errorCode :"JSON Error";
    addClientEntry(ce);
  }

  public PageState.Message addMessageError(String message) {
    return addMessageError(message,null);
  }
  public PageState.Message addMessageError(String message, String title) {
    PageState.Message message1 = new PageState.Message(PageState.MessageType.ERROR, message);
    message1.title=JSP.ex(title)?title:message1.title;
    messagesFromController.add(message1);
    return message1;
  }

  public PageState.Message addMessageWarning(String message) {
    PageState.Message message1 = new PageState.Message(PageState.MessageType.WARNING, message);
    messagesFromController.add(message1);
    return message1;
  }

  public PageState.Message addMessageInfo(String message) {
    PageState.Message message1 = new PageState.Message(PageState.MessageType.INFO, message);
    messagesFromController.add(message1);
    return message1;
  }

  public PageState.Message addMessageOK(String message) {
    PageState.Message message1 = new PageState.Message(PageState.MessageType.OK, message);
    messagesFromController.add(message1);
    return message1;
  }

  public void removeMessagesOfType(PageState.MessageType type){
    for(Iterator<PageState.Message>i=messagesFromController.iterator();i.hasNext();) {
      PageState.Message m = i.next();
      if (type.equals(m.type))
        i.remove();
    }
  }

  public void resetLoggedOperator() {
    operator=null;
    getLoggedOperator();
  }

  public Operator getLoggedOperator() {
    return operator;
  }


  public void initializeEntries(String from){
    try {
      ReflectionUtilities.invoke(ApplicationState.applicationParameters.get("get"), "initializeEntries", from);
    } catch (IllegalAccessException e) {
      throw new InitializationRuntimeException(e.getMessage());
    } catch (InvocationTargetException e) {
      Throwable te = e.getTargetException();
      if (te instanceof InitializationRuntimeException)
        throw (InitializationRuntimeException) te;
      else
       throw new InitializationRuntimeException(te);
    }
  }
}
