package org.jblooming.waf.html.button;

import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.display.Img;
import org.jblooming.waf.view.PageState;
import org.jblooming.ontology.LoggableIdentifiableSupport;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.security.SecurableWithArea;
import org.jblooming.security.Area;

import javax.servlet.jsp.PageContext;
import java.io.Serializable;

/**
 * @author Pietro Polsinelli : ppolsinelli@open-lab.com
 */
public class ButtonImg extends JspHelper {

  public ButtonSupport button;
  public Img image;

  public ButtonImg(ButtonSupport bs, Img img) {
    img.disabled = !bs.enabled;
    image = img;
    this.button = bs;
    this.id = bs.id;
    this.urlToInclude = "/commons/layout/partImgButton.jsp";
  }

  public ButtonSupport restoreButton() {
    image = null;
    button.toolTip = toolTip;
    return button;
  }

  public void setMainObjectId(Serializable id) {
    button.setMainObjectId(id);
  }


  public void disable() {
    image.disabled = true;
    button.enabled = false;
  }


}
