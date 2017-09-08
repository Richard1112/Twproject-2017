package org.jblooming.waf.html.display;

import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.core.HtmlBootstrap;
import org.jblooming.waf.view.PageState;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.utilities.JSP;

import javax.servlet.ServletException;
import javax.servlet.jsp.PageContext;
import java.io.IOException;
import java.util.List;
import java.util.ArrayList;

/**
 * GoogleMapDrawer (c) 2008 - Open Lab - www.open-lab.com
 *
 * see: http://code.google.com/intl/it/apis/maps/index.html
 * get a key @ http://code.google.com/intl/it/apis/maps/signup.html
 *
 * INPUT, TEXTAREA, and SELECT are the only tags allowed
 *
 * final value is stored by Google API into two hidden fields called
 * lat (latitude)
 * lng (longitude)
 * NO DIFFERENT NAME IS SUPPORTED and they must be stored into the respective fields of AnagraphicalData in order to maximize response speed
 * thus
 * REMENBER to update your anagraphical data save action
 * add to additionalParams serialized map fields lng and lat
 * data.getAdditionalParams().put("lng", pageState.getEntry("lng").stringValueNullIfEmpty());
 * data.getAdditionalParams().put("lat", pageState.getEntry("lng").stringValueNullIfEmpty());
 * and make action
 * pageState.addClientEntry("lat", getAdditionalParams().get("lng"));
 * pageState.addClientEntry("lng", getAdditionalParams().get("lat"));
 *
 * NB: it can works only if intranet can access internet (main google js are accessed via web)
 */
public class GoogleMapDrawer extends JspHelper implements HtmlBootstrap {

  /**
   * MANDATORY: it is used by Google js to identify request
   */
  public String activationKey;
  public String listenedZipEntry = "ZIP";
  // NB: ADDRESS is a reserved word and can't be used
  public String listenedStreetEntry = "ADDRESSX";
  public String listenedCityEntry = "CITY";
  public String listenedProvinceEntry = "PROVINCE";
  public String listenedCountryEntry = "COUNTRY";
  public List<String> geoMapTypes = new ArrayList<String>();

  public List<Marker> markers = new ArrayList<Marker>();

  public int width = 350;
  public int height = 250;

  public static final String G_NORMAL_MAP = "G_NORMAL_MAP";
  public static final String G_PHYSICAL_MAP = "G_PHYSICAL_MAP";
  public static final String G_HYBRID_MAP = "G_HYBRID_MAP";

  public static final String DRAW_MAP = "D_M";
  public static final String DRAW_STATIC_MAP = "D_S_M";

  public GoogleMapDrawer(String key) {
    this.activationKey = key;
    this.urlToInclude = "/commons/layout/googleMapDrawer/partGoogleMapDrawer.jsp";
  }

  public String getDiscriminator() {
    return GoogleMapDrawer.class.getName();
  }

  public boolean validate(PageState pageState) throws IOException, ServletException {
    return true;
  }

  public void init(PageContext pageContext) {
    PageState ps = PageState.getCurrentPageState(pageContext);
    if (!ps.initedElements.contains(getDiscriminator())) {
      pageContext.getRequest().setAttribute(ACTION, INITIALIZE);
      super.toHtml(pageContext);
      ps.initedElements.add(getDiscriminator());
    }
  }

  public void drawInteractiveMap(PageContext pageContext) {
    init(pageContext);
    pageContext.getRequest().setAttribute(ACTION, DRAW_MAP);
    super.toHtml(pageContext);
  }

  public void drawStaticMap(PageContext pageContext) {
    init(pageContext);
    pageContext.getRequest().setAttribute(ACTION, DRAW_STATIC_MAP);
    super.toHtml(pageContext);
  }

  public void addMarker(String latitude, String longitude) {
    addMarker(latitude, longitude, null);
  }

  public void addMarker(String latitude, String longitude, String infoWindowString) {
    addMarker(latitude, longitude, infoWindowString, null);
  }

  public void addMarker(String latitude, String longitude, String infoWindowString, String iconUrl) {
    if(JSP.ex(latitude) && JSP.ex(longitude)) {
      Marker marker = new Marker();
      marker.latitude = latitude;
      marker.longitude = longitude;
      marker.iconUrl = iconUrl;
      marker.infoWindowString = infoWindowString;
      this.markers.add(marker);
    }
  }

  /**
   * @deprecated
   * @param pageContext
   */
  public void toHtml(PageContext pageContext) {
    throw new PlatformRuntimeException ("Call drawInteractiveMap(pageContext) or drawStaticMap(pageContext) ");
  }

  public class Marker {
    public String latitude;
    public String longitude;
    public String iconUrl;
    public String infoWindowString;

    private Marker() {
    }

  }

}