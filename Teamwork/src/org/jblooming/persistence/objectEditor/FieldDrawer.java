package org.jblooming.persistence.objectEditor;

import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.state.Form;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class FieldDrawer extends JspHelper {

  public FieldFeature fieldFeature;
  public Class mainObjectClass;
  public Form form;
  public boolean autoSize = false;
  public boolean submitOnKeyReturn = false;

  public FieldDrawer(FieldFeature fieldFeature, Class mainObjectClass, Form form){
    super();
    this.fieldFeature = fieldFeature;
    this.mainObjectClass = mainObjectClass;
    this.form = form;
    this.urlToInclude = "/commons/layout/partFieldDrawer.jsp";
  }

}
