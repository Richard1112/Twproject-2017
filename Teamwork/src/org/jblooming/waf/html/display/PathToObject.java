package org.jblooming.waf.html.display;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.ontology.PerformantNodeSupport;
import org.jblooming.security.Permission;
import org.jblooming.waf.html.button.ButtonSupport;
import org.jblooming.waf.html.core.HtmlBootstrap;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import javax.servlet.jsp.PageContext;
import java.util.Comparator;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class PathToObject extends JspHelper implements HtmlBootstrap {

  public PerformantNodeSupport node;
  public PageSeed destination;
  public PageSeed selfDestination; // if null use the same page, otherwise use selfDestination
  public ButtonSupport rootDestination;
  public String separator = "";

  public Comparator comparator;

  public String  alternativeCEForMainObject=null; //if null uses "mainObjectId". if specified use it to set the CE for navigating.

  public Permission canClick;

  public static final String init = PathToObject.class.getName();
  public static final String DRAW = "DRAW";
  public static final String CLOSE = "CLOSE";
  private boolean drawCalled=false;
  private boolean closeCalled=false;

  public String displayNameForNewObject="...";
  public JspHelper currentNodeLeftElement=null;


  public PathToObject(PerformantNodeSupport node){
    this.node = node;
    urlToInclude = "/commons/layout/pathToObject/partPathToObject.jsp";
  }

  public String getDiscriminator() {
    return init;
  }

  public void draw(PageContext pageContext)  {
    drawCalled = true;
    pageContext.getRequest().setAttribute(ACTION, DRAW);
    super.toHtml(pageContext);
  }

  public boolean validate(PageState pageState) {
    return drawCalled && closeCalled;
  }

  public void close(PageContext pageContext){
    if (!drawCalled)
      throw new PlatformRuntimeException("Call start before end");
    closeCalled = true;
    pageContext.getRequest().setAttribute(ACTION, CLOSE);
    super.toHtml(pageContext);
  }

  public void toHtml(PageContext pageContext) {
      this.draw(pageContext);
      this.close(pageContext);
  }



}
