package org.jblooming.designer;

import org.jblooming.waf.html.core.*;
import org.jblooming.waf.view.*;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.ontology.PersistentFile;

import javax.servlet.jsp.*;
import javax.servlet.*;
import java.util.*;
import java.io.*;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class Detail extends DesignerField {
  public int maxLine;  //questo attributo serve esclusivamente ad un uso interno.....NON MODIFICARE!!!!!!!
  public boolean firstLineVisible = false;
  @Deprecated
  public boolean drawContainer = true;
  public boolean confirmRequire = true;

  /*if true, the component may not be added or removed, but just corrected*/
  public boolean correctionOnly;

  public boolean showAsTable=true;



  public LinkedHashMap<String, DesignerField> detailDesignerFields = new LinkedHashMap<String, DesignerField>();

  protected Detail() {
    super();
  }

  public void add(DesignerField designerField) {
    if(designerField instanceof Detail)
      throw new PlatformRuntimeException("You cannot add a detail to another detail.");
    if(PersistentFile.class.getName().equals(designerField.kind))
          throw new PlatformRuntimeException("You currently cannot add a PersistentFile field to detail.");
    if (designerField.name==null || designerField.name.indexOf("_")>-1)
      throw new PlatformRuntimeException("Invalid name "+name+" for detail ('_' is not legal)");

    designerField.readOnly=designerField.readOnly || readOnly;
    designerField.exportable=designerField.exportable || exportable;
    detailDesignerFields.put(designerField.name, designerField);
  }

  
  public void toHtml(PageContext pageContext) {
      new Drawer(this).toHtml(pageContext);
  }

  public class Drawer extends JspHelper implements HtmlBootstrap {

    public Detail detail;

    public Drawer(Detail fd) {
      super();
      this.detail = fd;
      urlToInclude = "/commons/layout/designer/partDesignerDetail.jsp";
    }

    public String getDiscriminator() {
      return Drawer.class.getName();
    }

    public boolean validate(PageState pageState) throws IOException, ServletException {
      return true;
    }
  }
}
