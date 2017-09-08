package org.jblooming.waf.html.display;

import org.jblooming.ontology.PersistentFile;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.system.SystemConstants;

import javax.servlet.http.HttpServletRequest;
import java.io.File;

/**
 * MultimediaFile (c) 2005 - Open Lab - www.open-lab.com
 */
public class MultimediaFile extends JspHelper {

  public String fileUrl;

  public String areaName = "";
  public String label;

  public String border;
  public String width;
  public String height;
  public String align = "";
  public String script = "";
  public String style = "";
  public String movieShowControl = "1";
  public String movieShowDisplay = "0";
  public String movieShowStatusBar = "0";
  public String movieWidth = "320";
  public String movieHeight = "282";
  public String flashParams = "";
  public String loop = "0";
  public boolean showAsList = false;
  public PersistentFile pf;
  public boolean inhibitToolTip = true;
  public String alternativeImage = "";

  // for quicktime, if a number is the scaling factor
  // use "tofit" to make the video fit a rectangle without keeping the aspect ratio
  // use "aspect" to make the video fit as much of a rectangle as possible while keeping the aspect ratio
  public String scale = "1";
  public String autoplay = "true";

  public MultimediaFile (PersistentFile pf, HttpServletRequest request) {
    this.pf = pf;
    this.urlToInclude = "/commons/layout/partMultimediaFile.jsp";
  }

}