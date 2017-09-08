package org.jblooming.waf.view;


import org.jblooming.PlatformRuntimeException;
import org.jblooming.operator.Operator;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.ThreadLocalPersistenceContextCarrier;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.security.InvalidTokenException;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.HttpUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.ScreenArea;
import org.jblooming.waf.ScreenRoot;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.html.container.ButtonBar;
import org.jblooming.waf.html.core.HtmlBootstrap;
import org.jblooming.waf.html.core.JspIncluder;
import org.jblooming.waf.html.display.DeletePreviewer;
import org.jblooming.waf.html.display.HeaderFooter;
import org.jblooming.waf.html.state.Form;
import org.jblooming.waf.settings.Application;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.settings.PlatformConfiguration;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.jsp.PageContext;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.*;

public class PageState extends RestState {


  public SessionState sessionState;

  public Set initedElements = new HashSet();

  /**
   * elements of the screen
   */
  private Form form;
  private HeaderFooter headerFooter;

  /**
   * template composition
   */
  public boolean screenRunning = false;


  /**
   * stopPageAfterController inhibit the call to page generation
   * it should be set to true when a page/controller is called by ajax. E.G removing a line from a list
   * without refreshing the page and having this command inside the controller
   */
  public boolean stopPageAfterController = false;

  public ScreenArea runningControllerScreenArea;

  private Stack screenAreas = new Stack();
  public ScreenRoot rootScreen;

  /**
   * display validation
   */
  public HtmlBootstrap.HtmlBootstrappers htmlBootstrappers = new HtmlBootstrap.HtmlBootstrappers();

  private String focusedObjectDomId = null;

  private JspIncluder mainJspIncluder;
  private ButtonBar mainButtonBar;


  private static ThreadLocal<PageState> __threadLocalPageState = new ThreadLocal<PageState>() {
    protected PageState initialValue() {
      return null;
    }
  };


  public PageState(String url, SessionState sm) {
    super(url);
    __threadLocalPageState.set(this);
    setSessionState(sm);
  }

  @Deprecated
  public PageState(Operator operator) {
    throw new PlatformRuntimeException("Invalid usage of costructor. PageState need a Session; user is there and cannot be set.");
  }


  public Operator getLoggedOperator() {
    try {
      if (sessionState.getOpid() != -1) {
        if (this.operator == null) {
          this.operator = (Operator) PersistenceHome.findByPrimaryKey(PlatformConfiguration.defaultOperatorSubclass, sessionState.getOpid());
        }
        return operator;
      } else
        return null;
    } catch (Throwable t) {
      throw new PlatformRuntimeException(t);
    }
  }

  public void resetLoggedOperator() {
    operator = null;
    this.getLoggedOperator();
  }


  public void setFocusedObjectDomId(String domId) {
    focusedObjectDomId = domId;
  }

  public String getFocusedObjectDomId() {
    return focusedObjectDomId;
  }


  public String getColor(String colorName) {
    return I18n.get("COLOR_" + colorName);
  }


  public SessionState getSessionState() {
    return sessionState;
  }

  public void setSessionState(SessionState sessionState) {
    this.sessionState = sessionState;
  }


  /**
   * @return
   * @deprecated use with request or pageContext getCurrentPageState(request). DO NOT REMOVE it is used by SessionState
   */
  public static PageState getCurrentPageState() {
    return __threadLocalPageState.get();
  }

  public static PageState getCurrentPageState(HttpServletRequest request) {
    return getCurrentPageState();
  }

  public static PageState getCurrentPageState(PageContext pageContext) {
    return getCurrentPageState((HttpServletRequest) pageContext.getRequest());
  }


  public PageState perform(HttpServletRequest request, HttpServletResponse response) {
    while (!screenAreas.empty()) {
      try {
        ScreenArea screenArea = (ScreenArea) screenAreas.pop();
        ActionController controller = screenArea.controller;
        runningControllerScreenArea = screenArea;
        if (controller != null) {
          controller.perform(request, response);
        }
      } catch (Exception e) {
        throw new PlatformRuntimeException(e);
      }
    }

    if (validEntries()) {
      ThreadLocalPersistenceContextCarrier carrier = PersistenceContext.threadLocalPersistenceContextCarrier.get();
      if (carrier != null) {
        for (PersistenceContext pc : carrier.persistenceContextMap.values()) {
          pc.checkPoint();
        }
      } else {
        System.out.println("Carrier is null");
      }
    }
    return this;
  }

  public void setForm(Form f) {
    this.form = f;
  }

  public Form getForm() {
    return form;
  }

  public PageState registerPart(ScreenArea screenArea) {
    this.screenAreas.push(screenArea);
    return this;
  }

  public String toString() {
    return "\nScreenBasic = " + rootScreen;
  }

  public void toHtml(PageContext pageContext) {
    if (rootScreen == null)
      throw new PlatformRuntimeException("PageState calling toHtml on rootScreen==null");
    if (!stopPageAfterController)
      rootScreen.toHtml(pageContext);
  }

  public static void buildPartsAndClientEntries(HttpServletRequest request, PageState pageState) {

    ClientEntries clientEntries = new ClientEntries();

    Map<String, String[]> reqParams = request.getParameterMap();

    if (JSP.ex((String) request.getAttribute("javax.servlet.forward.request_uri"))) {
      reqParams = new Hashtable(request.getParameterMap());

      //se ci sono in query string vincono!
      String qs = request.getQueryString();
      if (JSP.ex(qs)) {
        try {
          String[] params = qs.split("&");
          for (int i = 0; i < params.length; i++) {
            String[] pair = params[i].split("=");
            if (pair.length > 1)
              reqParams.put(pair[0], new String[]{URLDecoder.decode(pair[1].replace("+", "%2B"), "UTF-8").replace("%2B", "+")});
          }
        } catch (UnsupportedEncodingException ue) {
          Tracer.platformLogger.error("Invalid encoding", ue);
        }
      }
    }

    for (String paramName : reqParams.keySet()) {
      String[] val = reqParams.get(paramName);

      if (Commands.COMMAND.equals(paramName)) {
        pageState.setCommand(val[0].trim());

      } else if (paramName.equals(Fields.OBJECT_ID)) {
        pageState.setMainObjectId(val[0].trim());

      } else if (paramName.equals(Fields.POPUP)) {
        pageState.setPopup(true);

      } else if (paramName.equals(Fields.VIEW_ID)) {
        continue;

      } else {
        // aggiunto il replace del \r\n con \n perchè la textarea li conta come un soilo carattere e quindi falsa la maxlenght e stonfa il db
        //clientEntries.addEntry(new ClientEntry(paramName, StringUtilities.arrayToString(request.getParameterValues(paramName), ",").trim()));
        clientEntries.addEntry(new ClientEntry(paramName, StringUtilities.arrayToString(val, ",").trim().replace("\r\n", "\n")));
      }
    }

    pageState.setClientEntries(clientEntries);
  }

  public HeaderFooter getHeaderFooter() {
    if (headerFooter == null)
      headerFooter = new HeaderFooter(this);
    return headerFooter;
  }

  private void updateSeed(PageSeed newSeed) {
    this.href = newSeed.getHref();
    this.setMainObjectId(newSeed.getMainObjectId());
    setClientEntries(newSeed.getClientEntries());
    this.command = newSeed.getCommand();
    setLoginRequiring(newSeed.isLoginRequiring());
    setPopup(newSeed.isPopup());
  }

  /**
   * @see org.jblooming.waf.ScreenBasic changeBody(org.jblooming.waf.view.PageState, org.jblooming.waf.view.PageSeed)
   *      WARNING: do not use contextPath in newPageseed:
   *      CORRECT: newpageseed= new PageSeed("/commons/bblabla/xxx.jsp");
   *
   *      OLD
   *      NOT CORRECT: newpageseed= pagestate.pageFromCommonRoot("/bblabla/xxx.jsp");
   *      NOT CORRECT: newpageseed= pagestate.pageFromRoot("xxx.jsp");
   *
   *      2015 04 01  BE Positive!!!!
   *      if newSeed start with context path -> remove it
   *      NOW CORRECT: newpageseed= pagestate.pageFromCommonRoot("/bblabla/xxx.jsp");
   *      NOW CORRECT: newpageseed= pagestate.pageFromRoot("xxx.jsp");
   *
   */
  public void redirect(PageSeed newSeed) {
    if (newSeed.href.startsWith(ApplicationState.contextPath+"/"))
      newSeed.href=newSeed.href.replace(ApplicationState.contextPath+"/","/");

    updateSeed(newSeed);
    final ScreenArea basicScreen = this.runningControllerScreenArea;
    ScreenRoot lw = basicScreen.parent;
    this.screenRunning = false;
    lw.urlToInclude = newSeed.href;
  }

  public void setApplication(Application application) {
    sessionState.setApplication(application);
  }

  public Application getApplication() {
    return sessionState.getApplication();
  }

  public PageSeed thisPage(HttpServletRequest request) {
    int i = href.indexOf("?");
    if (i == -1)
      i = href.length();

    String newHref = href.substring(0, i);
    //if there is no path, do not touch it
    if (!(newHref.indexOf("/") == -1))
      newHref = request.getContextPath() + newHref;
    return new PageSeed(newHref);
  }

  public void setMainJspIncluder(JspIncluder jspIncluder) {
    this.mainJspIncluder = jspIncluder;
  }

  public JspIncluder getMainJspIncluder() {
    return mainJspIncluder;
  }

  public PageSeed pageInThisFolder(String page, HttpServletRequest request) {
    String realURI = HttpUtilities.realURI(request);
    String href = request.getContextPath() + realURI.substring(0, realURI.lastIndexOf("/")) + "/" + page;
    return new PageSeed(href);
  }


  /**
   * @param page
   * @return the page from root of current application
   */
  public PageSeed pageFromRoot(String page) {
    if (!page.startsWith("/"))
      page = "/" + page;
    String rootF = getApplication().getRootFolder();
    if (!rootF.startsWith("/"))
      rootF = "/" + rootF;
    return new PageSeed(ApplicationState.contextPath + rootF + page);
  }


  /**
   * @param request
   * @return the pagesed representing the current part. It is used tipically in the webParts
   */
  public PageSeed pagePart(HttpServletRequest request) {
    String realURI = HttpUtilities.realURI(request);
    return new PageSeed(request.getContextPath() + realURI);
  }

  public void setButtonBar(ButtonBar bb2) {
    this.mainButtonBar = bb2;
  }

  public ButtonBar getButtonBar() {
    return mainButtonBar;
  }


  public PageSeed getNewInstance() {
    PageSeed ps = super.getNewInstance();
    // pageState differs satanically from pageSeed in that its href by default is not prefixed with context path
    ps.href = ApplicationState.contextPath + ps.href;
    return ps;
  }

  public String tokenCreate(String tokenName) {
    String token = StringUtilities.generatePassword(20);
    this.sessionState.setAttribute(tokenName, token);
    return token;
  }

  public static String TOKEN = "__tk";

  public void tokenCreate(String tokenName, PageSeed destination) {
    destination.addClientEntry(TOKEN, tokenCreate(tokenName));
  }

  public void tokenValidate(String tokenName) throws InvalidTokenException {
    tokenValidate(tokenName, true);
  }

  public void tokenValidate(String tokenName, boolean removeToken) throws InvalidTokenException {
    String token = sessionState.getAttribute(tokenName) + "";
    if (removeToken)
      sessionState.getAttributes().remove(tokenName);
    String s = getEntry(TOKEN).stringValueNullIfEmpty();
    if (!token.equals(s)) {
      addMessageError(I18n.get("INVALID_SECURITY_TOKEN"));
      throw new InvalidTokenException("INVALID_SECURITY_TOKEN");
    }
  }

  public void tokenClone(String tokenName, PageSeed destination) {
    String sesTk = sessionState.getAttribute(tokenName) + "";
    destination.addClientEntry(TOKEN, sesTk);
  }

  public String tokenGetCurrent(String tokenName) {
    return sessionState.getAttribute(tokenName) + "";
  }

  public String tokenGetCurrent() {
    return this.getEntry(TOKEN).stringValueNullIfEmpty();
  }

  //---------------------------------------------------- MESSAGES FROM CONTROLLER MANAGEMENT ------------ START ---------------------------------------

  public static enum MessageType {ERROR, WARNING, INFO, OK}


  public static class Message implements Comparable{
    public MessageType type;
    public String title;
    public String message;

    public Message(MessageType type, String message) {
      this.type = type;
      this.message = message;
    }

    @Override
    public int compareTo(Object o) {
      Message m1 = (Message)o;
      return (m1.type+" "+m1.title+" "+m1.message).compareTo(type+" "+title+" "+message);
    }

    public boolean equals(Object o) {
      return this.compareTo(o) == 0;
    }

    public int hashCode() {
      return  (type+" "+title+" "+message).hashCode();
    }

  }

//---------------------------------------------------- MESSAGES FROM CONTROLLER MANAGEMENT ------------ END ---------------------------------------


}


