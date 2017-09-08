package com.twproject.waf.html;

import com.twproject.document.TeamworkDocument;
import org.jblooming.ontology.Documentable;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.view.PageSeed;

import javax.servlet.jsp.PageContext;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class DocumentDrawer extends JspHelper {

  public boolean recurseOnChildren = true;
  public Documentable startingPoint; //il primo elemento che viene stampato
  public Documentable documentable;
  public TeamworkDocument currentDocument;
  public boolean sorted = false;
  public boolean drawOnlyRoots = true;
  public boolean drawingChild;  //used to discriminate first documentable from its child


  /**
   *  da usare nel loop dei children
   * @param mainObject
   * @param startingPoint è l'oggetta da cui si inizia a disegnare
   */
  public DocumentDrawer(Documentable mainObject, Documentable startingPoint) {
    super();
    this.urlToInclude = "/applications/teamwork/document/partDocumentDrawer.jsp";
    this.documentable = mainObject;
    this.startingPoint = startingPoint;
  }

  /**
   * Da usare nella prima chiamata
   * @param mainObject
   */
  public DocumentDrawer(Documentable mainObject){
    this(mainObject,mainObject);
  }

   public void drawDocumentable(PageContext pageContext)  {
    pageContext.getRequest().setAttribute(ACTION, "OBJECTPART");
    super.toHtml(pageContext);
  }

  public void drawDocument(TeamworkDocument document, PageContext pageContext) {
    currentDocument = document;
    if (drawOnlyRoots && document.getParent()!=null)
      return;
    pageContext.getRequest().setAttribute(ACTION, "DOCPART");
    super.toHtml(pageContext);

  }


  /**
   * @deprecated
   */
  public void toHtml(PageContext pageContext) {
    throw new RuntimeException("Call task and doc");
  }


}
