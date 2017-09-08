package org.jblooming.waf.html.input;

import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.view.PageState;

import javax.servlet.jsp.PageContext;
import java.io.Serializable;


/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 23-feb-2007 : 15.22.38
 */
public class UrlFileStorage extends JspHelper {

  public boolean readOnly = false;
  public boolean required = false;
  public boolean downloadOnly = false;
  public String initialValue;
  public static final String DRAW = "DRAW";
  public String fieldName;

  public String separator = "&sbsp;";
  public String label;
  public Serializable referralObjectId;


  public UrlFileStorage(String fieldName) {
    this.urlToInclude = "/applications/teamwork/document/partUrlFileStorage.jsp";
    this.fieldName = fieldName;
    this.id = fieldName;
  }

  public String getDiscriminator() {
    return UrlFileStorage.class.getName();
  }


  public void toHtml(PageContext pageContext) {
    pageContext.getRequest().setAttribute(ACTION, DRAW);
    super.toHtml(pageContext);
  }


}
