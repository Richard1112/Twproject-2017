package org.jblooming.waf.html.core;

import org.jblooming.utilities.JSP;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.html.state.Form;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.view.PageSeed;

import javax.servlet.http.HttpServletRequest;
import java.util.Iterator;


/**
 * Dedicated to Roberto Bicchierai
 * Date: 14-feb-2003
 * Time: 19.54.56
 *
 * @author Pietro Polsinelli dev@open-lab.com
 */
public abstract class UrlComposer extends HtmlRenderer {

  public String outputModality = OUTPUT_AS_LINK;
  public HttpServletRequest request;
  public boolean debug = false;
  protected Form form;
  // ww4 requires by default SEO urls
  public static boolean DISABLE_VIEW_ID = Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("DISABLE_VIEW_ID"));

  private static boolean debugActive = false;

  public static final String OUTPUT_AS_LINK = "OUTPUT_AS_LINK";
  public static final String OUTPUT_AS_FORM = "OUTPUT_AS_FORM";
  public static final String OUTPUT_AS_JS_SUBMIT = "OUTPUT_AS_JS_SUBMIT";
  public static final String OUTPUT_AS_JS_LAUNCH_CES = "OUTPUT_AS_JS_LAUCH_CES";


  public UrlComposer(PageSeed v) {
    this.url = v;
  }

  public void doDebug(HttpServletRequest request) {
    this.request = request;
    this.debug = true;
  }

  public String getHref() {

    StringBuffer href = new StringBuffer(512);

    if (url != null) {
      if (outputModality.equals(OUTPUT_AS_LINK))
        href.append(url.getHref() + '?');

      if ((!outputModality.equals(OUTPUT_AS_JS_SUBMIT)) && !DISABLE_VIEW_ID && url.disableCache)
        addPair(href, Fields.VIEW_ID, this.hashCode() + "");

      addPair(href, Commands.COMMAND, url.getCommand());

      if (url.mainObjectId != null)
        addPair(href, Fields.OBJECT_ID, url.mainObjectId + "");

      if (url.isPopup())
        addPair(href, Fields.POPUP, Fields.TRUE);

      for (Iterator iterator = url.getClientEntries().getEntryKeys().iterator(); iterator.hasNext();) {
        String key = (String) iterator.next();
        String value = url.getClientEntries().getEntry(key).stringValueNullIfEmpty();
        addPair(href, key, value);
      }
    }
    return href.toString();
  }

  private void addPair(StringBuffer href, String key, String value) {

    if (value == null)
      value = "";
    if (outputModality.equals(OUTPUT_AS_FORM)) {
      generateHiddenInput(href, key, value);
    } else if (outputModality.equals(OUTPUT_AS_JS_SUBMIT)) {
      href.append("obj('").append(getUniqueName() + key).append("').value='").append(JSP.javascriptEncode(JSP.encode(value))).append("';");
    } else if (!value.equals("")) {
      if (href.length() > 0 && !href.toString().endsWith("?"))
        href.append("&");
      href.append(key).append('=').append(JSP.urlEncode(value));
    }
  }

  public void generateHiddenInput(StringBuffer href, String key, String value) {
    href.append("<input type =\"hidden\"  id=\"").append(getUniqueName() + key).append("\" name=\"");
    href.append(key).append("\" value=\"").append(JSP.encode(value)).append("\" savedValue=\"\" >\n");
  }

  public String getUniqueName() {
    if (form == null)
      return id;
    else
      return form.id;
  }

}
