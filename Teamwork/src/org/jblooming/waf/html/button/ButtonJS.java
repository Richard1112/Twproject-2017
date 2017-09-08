package org.jblooming.waf.html.button;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.utilities.JSP;

import java.io.Serializable;


public class ButtonJS extends ButtonSupport {

  public String onClickScript;
  /**
   * create confirm popup
   */
  public boolean confirmRequire = false;
  public String confirmQuestion;

  public String additionalScript;
  public String additionalOnClickScript;


  public ButtonJS() {
    super();
  }

  public ButtonJS(String onClickScript) {
    this("",onClickScript);
  }

  public ButtonJS(String label,String onClickScript) {
    super("partButton.jsp");
    this.label=label;
    this.onClickScript = onClickScript;
  }

  public String getLaunchedJsOnActionListened() {
    String ret = null;
    if (onClickScript != null)
      ret = onClickScript;
    if (additionalScript != null)
      ret = ret + " " + additionalScript;
    return ret;
  }

  public void setMainObjectId(Serializable id) {
    //throw new PlatformRuntimeException("setMainObjectId implementation not supported: do it by hand, lazy fool! Hahahahaha!");
  }


  public String generateLaunchJs() {

    String sb = "";
    if (enabled) {
      sb+=" onclick=\"";
      sb+=generateJs();
      sb+=sb.endsWith(";")?"":";";
      sb+="return false; ";
      sb+="\" ";// close onclick string
      if (additionalScript != null && additionalScript.trim().length() > 0)
        sb+=' '+additionalScript;
    }
    return sb.toString();
  }


  public String getLabel() {
    return label;
  }

  public boolean isEnabled() {
    return enabled;
  }

  public String getToolTip() {
    return toolTip;
  }

  public StringBuffer generateJs() {

    StringBuffer sb = new StringBuffer();
    if (enabled) {
      if (confirmRequire) {
        sb.append("$(this).confirm(function(){");
      }

      sb.append(JSP.w(onClickScript));
      sb.append(JSP.w(additionalOnClickScript));

      if (confirmRequire) {
        sb.append("}"); // close function block
        if (JSP.ex(confirmQuestion))
          sb.append(",'"+JSP.javascriptEncode(confirmQuestion)+"'");
        sb.append(");"); // close confirm block
      }

    }
    return sb;
  }


}

