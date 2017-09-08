package org.jblooming.waf;

import org.apache.log4j.MDC;
import org.jblooming.InitializationRuntimeException;
import org.jblooming.PlatformExceptionCarrier;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.operator.Operator;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.ThreadLocalPersistenceContextCarrier;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.security.*;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.api.APIFilter;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.constants.SettingsConstants;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.PlatformConfiguration;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class FrontControllerFilter implements Filter {

  public static Set<String> ignoredPatterns = new HashSet();


  public static String ERROR_PAGE_PATH_FROM_ROOT = "/commons/administration/error.jsp";
  public static final String page = ".page";

  public void init(FilterConfig config) throws ServletException {

    //linux no x-windows compatibility attempt
    System.setProperty("java.awt.headless", "true");
  }

  public void destroy() {
  }

  public void doFilter(ServletRequest req, ServletResponse res, FilterChain chain) throws IOException, ServletException {


    HttpServletRequest request = (HttpServletRequest) req;
    HttpServletResponse response = (HttpServletResponse) res;
    String charset = "utf-8";
    //di default si mette utf-8 se non diversamente specificato
    if (!JSP.ex(request.getCharacterEncoding()))
      request.setCharacterEncoding(charset);
    // in case of use with tomcat 4.1
    // response.setCharacterEncoding non supported, it must be replaced with response.setContentType
    response.setContentType("charset=\"utf-8\"");
    response.setCharacterEncoding(charset);

    //check if header has been set form CachingFilter
    if (!response.containsHeader("Expires"))
      response.addHeader("Expires", "Sat, 23 Sep 2000 01:01:01 GMT");

    if (!response.containsHeader("Cache-Control"))
      response.addHeader("Cache-Control", "no-cache");

    final String resource = request.getServletPath().toLowerCase();
    //PersistenceContext pc = null;

    //inject client ip on log4j mapped debug context
    MDC.put("clientIp", request.getRemoteHost());

    boolean matchesServletPath = false;
    for (String ip : AccessControlFilter.servletPath) {
      if (resource.toLowerCase().startsWith(ip.toLowerCase())) {
        matchesServletPath = true;
        break;
      }
    }

    if (resource.equalsIgnoreCase("") ||
            resource.equalsIgnoreCase("/") ||
            resource.toUpperCase().endsWith(".JSP") ||
            resource.toUpperCase().endsWith(page.toUpperCase()) ||
            APIFilter.isApiResource(resource) ||
            matchesServletPath) {

      if (ApplicationState.platformConfiguration != null && ApplicationState.platformConfiguration.development) {
        request.setAttribute("time", System.currentTimeMillis());
      }

      try {

        //this is necessary for instruct "non thread-local" code for default hib factory
        PersistenceContext.switchToFirst();

        boolean doChain = true;

        Map map = ApplicationState.getConfiguredUrls();
        if (map.size() == 0) {
          PageSeed vc = new PageSeed(ApplicationState.contextPath + AccessControlFilter.LOGIN_PAGE_PATH_FROM_ROOT);
          map.put(SettingsConstants.ROOT_LOGIN, vc);
          vc.setLoginRequiring(false);

          vc = new PageSeed(ApplicationState.contextPath + "/command.jsp");
          map.put(SettingsConstants.ROOT_COMMAND, vc);
        }

        //loggable, audit trail and subscribe enaction
        SessionState sessionState = SessionState._getSessionState(request);

        if (sessionState != null && sessionState.getOpid() != -1) {
          //Operator loggedOperator = pageState.getLoggedOperator();
          Operator loggedOperator = (Operator) PersistenceHome.findByPrimaryKey(PlatformConfiguration.defaultOperatorSubclass, sessionState.getOpid());
          sessionState.setLoggedOperator(loggedOperator);
          PersistenceContext.threadLocalPersistenceContextCarrier.get().setOperator(loggedOperator);
          //inject client ip on log4j mapped debug context
          MDC.put("loginName", loggedOperator == null ? null : loggedOperator.getLoginName() + "(" + loggedOperator.getId() + ")");
        }


        doChain = buildPageState(request, sessionState);

        //used for example in Flowork to inject session
        if (ApplicationState.platformConfiguration.defaultApplication != null)
          ApplicationState.platformConfiguration.defaultApplication.configBeforePerform(request);


        if (doChain) {
          chain.doFilter(req, response);
        }

        //28Apr2008
        //must rollback in case of action exception otherwise dirty gets saved by reachability
        PageState pageState = PageState.getCurrentPageState(request);
        boolean validEntries = pageState.validEntries();
        if (validEntries)
          pageState.saveEntriesInDefaults();

        ThreadLocalPersistenceContextCarrier carrier = PersistenceContext.threadLocalPersistenceContextCarrier.get();
        if (carrier != null) {
          Collection<PersistenceContext> contexts = carrier.persistenceContextMap.values();

          for (PersistenceContext pc : contexts) {
            if (validEntries) {
              pc.commitAndClose();

            } else
              pc.rollbackAndClose();
          }
        }

      } catch (Throwable throwable) {

        try {
          ThreadLocalPersistenceContextCarrier carrier = PersistenceContext.threadLocalPersistenceContextCarrier.get();
          if (carrier != null) {
            Collection<PersistenceContext> contexts = carrier.persistenceContextMap.values();
            for (PersistenceContext pc : contexts) {
              pc.rollbackAndClose();
            }
          }



          if (throwable instanceof ServletException && ((ServletException) throwable).getRootCause() != null) {
            Throwable thr = ((ServletException) throwable).getRootCause();
            throwInformedException(request, thr);
          } else
            throwInformedException(request, throwable);

        } catch (PlatformRuntimeException toBePrinted) {

          Throwable rootCause = Tracer.getRootCause(throwable);

          if (toBePrinted.getCause() instanceof InvalidTokenException || (toBePrinted.getCause() != null && toBePrinted.getCause().getCause() instanceof InvalidTokenException)) {
            try {
              SessionState.getCurrentSessionState().getAttributes().put("__ERROR__", "INVALID_RESUBMIT");
            } catch (Throwable e) {
            }
            response.sendRedirect(request.getContextPath() + "/index.jsp");

          } else if (rootCause instanceof InitializationRuntimeException){
            InitializationRuntimeException ire= (InitializationRuntimeException) rootCause;
            response.sendRedirect(request.getContextPath() + ire.recoveringErrorURL );

          } else if (!ApplicationState.platformConfiguration.development) {

            //check if I is an amichevol exeption that can be redirected to home
            Throwable cause=toBePrinted.getCause();
            Set<Throwable>visited=new HashSet<Throwable>();
            while (!visited.contains(visited)){
              if (cause instanceof org.jblooming.security.SecurityException){
                break;
              } else {
                visited.add(cause);
                cause=cause.getCause();
                if (cause==null)
                  break;
              }
            }

            //controllo se ho trovato qualcosa di buono -> se sono uscito perchè null o perchè loop non è buono
            // gestisco localmente
            if (cause!=null && !visited.contains(cause)){
              request.getSession().setAttribute("_SOFTERROR",cause);
              response.sendRedirect(request.getContextPath() + "/index.jsp");

            // vado su bugsvoice
            } else {

              Tracer.platformLogger.error(toBePrinted.getMessage(), throwable);
              response.getWriter().print("<html><body>';\">--></select></textarea></script></table></table>" +
                "Redirecting to error page...<iframe style=\"display:none\" src=\"" + ApplicationState.serverURL + ERROR_PAGE_PATH_FROM_ROOT + "\"></iframe>" +
                "");
            }
          } else {
            Tracer.platformLogger.error(toBePrinted.getMessage(), throwable);
            //Tracer.platformLogger.error("",toBePrinted);
            throw toBePrinted;
          }
        }
      } finally {
        /**
         *  BE CAREFUL: you MUST remove by hand becouse ThreadLocal is local in a request but pooled by the application server
         *  it can be reused acronn requests --> if it is not clean could be a disaster.
         */
        PersistenceContext.threadLocalPersistenceContextCarrier.remove();
      }

    } else
      chain.doFilter(request, res);
  }

  private void throwInformedException(HttpServletRequest request, Throwable throwable) {

    PlatformExceptionCarrier exceptionCarrier = new PlatformExceptionCarrier();
    exceptionCarrier.exception = throwable;
    exceptionCarrier.requestURI = request.getRequestURI();
    exceptionCarrier.queryString = request.getQueryString();
    exceptionCarrier.command = request.getParameter(Commands.COMMAND);
    exceptionCarrier.objectID = request.getParameter(Fields.OBJECT_ID);
    request.getSession().setAttribute("PLAT_EXCEPTION", exceptionCarrier);

    throw new PlatformRuntimeException(
            "RequestURL: " + request.getRequestURL() +
                    (JSP.ex(request.getQueryString()) ? "?" + request.getQueryString() + "\n" : "") +
                    (JSP.ex(request.getParameter(Commands.COMMAND)) ? "COMMAND: " + request.getParameter(Commands.COMMAND) + "\n" : "") +
                    (JSP.ex(request.getParameter(Fields.OBJECT_ID)) ? "OBJECT_ID: " + request.getParameter(Fields.OBJECT_ID) + "\n" : "") +
                    throwable.getMessage(), throwable);

  }

  protected boolean buildPageState(HttpServletRequest request, SessionState sessionState)
          throws ServletException, IOException {

    String url = request.getServletPath();

    /*String forwardUrl = (String) request.getAttribute("javax.servlet.forward.request_uri");
    if (JSP.ex(forwardUrl)){
      url=forwardUrl;
      if (url.startsWith(request.getContextPath()))
        url=url.substring(request.getContextPath().length());
    }*/

    PageState pageState = new PageState(url, sessionState);

    String contentType = request.getContentType();
    pageState.multipart = contentType != null && contentType.startsWith("multipart/form-data");

    PageState.buildPartsAndClientEntries(request, pageState);

    return true;
  }
}
