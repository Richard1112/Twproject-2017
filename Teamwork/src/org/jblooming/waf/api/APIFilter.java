package org.jblooming.waf.api;

import org.jblooming.tracer.Tracer;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.HashTable;
import org.jblooming.uidgen.CounterHome;
import org.jblooming.agenda.CompanyCalendar;

import javax.servlet.Filter;
import javax.servlet.http.HttpServletRequest;
import java.io.File;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.*;
import java.security.NoSuchAlgorithmException;

/**
 * @author Federico Soldani - fsoldani@open-lab.com
 */
public abstract class APIFilter implements Filter {

  public static long millisToExpiry = 5000;
  public static int maxTryInInterval = 50;
  public static long bannedIpSwipeTime = CompanyCalendar.MILLIS_IN_MINUTE*5;

  public static String API_SERVER_NAME = "api.jblooming.org";
  public static String API_SCHEME = "http";


  public static Map<String, Long> bannedIPS = new HashTable();
  //public static TreeSet<RequestHistory> lastReqs = new TreeSet();
  public static SortedSet<RequestHistory> lastReqs = Collections.synchronizedSortedSet(new TreeSet());

  public static boolean isApiResource(String resource) {
    boolean ret=false;
    //ret= OAuthBricks.isOAuthURI(resource)|| (null != APIBricks.SupportedFormat.getSupportedFormatFromFileName(resource));
    return ret;            
  }

  /**
   * Obtain decoded Uri without contextPath
   *
   * @param request HttpServletRequest
   * @return decoded Uri without context Path
   */
  protected String getPathWithoutContext(HttpServletRequest request) {
    String requestUri = request.getRequestURI();

    if (requestUri == null) {
      requestUri = "";
    }

    String decodedRequestUri = decodeRequestString(request, requestUri);


    return decodedRequestUri;
  }

  /**
   * Decode the string with a URLDecoder. The encoding will be taken
   * from the request, falling back to the default for your platform ("ISO-8859-1" on windows).
   *
   * @param request HttpServletRequest
   * @param source  String that contain URL to decode
   * @return String with decoded url
   */
  protected String decodeRequestString(HttpServletRequest request, String source) {
    String enc = request.getCharacterEncoding();
    if (enc != null) {
      try {
        return URLDecoder.decode(source, enc);
      } catch (UnsupportedEncodingException ex) {
        Tracer.platformLogger.error("Could not decode: " + source + " (header encoding: '" + enc + "'); exception: " + ex.getMessage());
      }
    }

    return source;
  }

  public void destroy() {
    if (ApplicationState.platformConfiguration.development) {
      //System.out.println("-- destroy APIFilter --");
      Tracer.platformLogger.info("-- destroy APIFilter --");
    }

  }

  /**
   * Default method with default parameters
   *
   * @param request HttpServletRequest to be checked
   * @return true if request is allowed, false otherwise
   */
  public static boolean canAccept(HttpServletRequest request) {
    return canAccept(request, millisToExpiry, maxTryInInterval);
  }


  /**
   * Check if Request can be accepted as valid request or throw away as evil attack!
   *
   * @param request               HttpServletRequest to be analyzed
   * @param millisToExpiryParam   inteval's range to check into
   * @param maxTryInIntervalParam max number of request allowed in the interval
   * @return true if request is a valid request, false otherwise
   */


  public static boolean canAccept(HttpServletRequest request, long millisToExpiryParam, long maxTryInIntervalParam) {
    String ip = request.getRemoteAddr();

    //check banned
    synchronized (bannedIPS) {
      if (bannedIPS.containsKey(ip)) {
        long when = bannedIPS.get(ip);
        if (when + bannedIpSwipeTime < System.currentTimeMillis())
          bannedIPS.remove(ip);
        else {
          bannedIPS.put(ip, System.currentTimeMillis());
          return (false);
        }
      }
    }


    // create a RequestHistory now-millisToExpiry
    RequestHistory req = new RequestHistory();
    req.ip = "fake";
    req.lastRequestMillis = System.currentTimeMillis() - millisToExpiryParam;

    //trim lastReqs of older req
    synchronized (lastReqs) {
      //SortedSet<RequestHistory> historySortedSet = lastReqs.headSet(req);
      Set<RequestHistory> historySortedSet = new HashSet(lastReqs.headSet(req));
      if (JSP.ex(historySortedSet)) {
        lastReqs.removeAll(historySortedSet);
      }
    }


    //count for occurencies
    int c = 0;
    synchronized (lastReqs) {
      for (RequestHistory ri : lastReqs) {
        if (ri.ip.equals(ip)) {
          c++;
          if (c > maxTryInIntervalParam) {
            break;
          }
        }
      }
    }

    //check max
    if (c > maxTryInIntervalParam) {
      synchronized (bannedIPS) {
        bannedIPS.put(ip, System.currentTimeMillis());
        Tracer.platformLogger.info("Banned: " + ip + " on " + new Date());
      }
      return false;
    }


    // create and add RequestHistory now
    RequestHistory rreq = new RequestHistory();
    rreq.ip = ip;
    rreq.lastRequestMillis = System.currentTimeMillis();
    synchronized (lastReqs) {
      lastReqs.add(rreq);
    }

    return true;

  }


  public static class RequestHistory implements Comparable {
    public long lastRequestMillis;
    public String ip;

    public boolean equals(Object o) {
      return lastRequestMillis == (((RequestHistory) o).lastRequestMillis);
    }

    public int hashCode() {
      return ip.hashCode();
    }

    public int compareTo(Object o) {
      return (int) (lastRequestMillis - ((RequestHistory) o).lastRequestMillis);
    }

  }

}
