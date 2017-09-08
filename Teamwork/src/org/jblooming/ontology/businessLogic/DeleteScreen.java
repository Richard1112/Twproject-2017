package org.jblooming.ontology.businessLogic;

import org.jblooming.waf.ActionController;
import org.jblooming.waf.ScreenArea;
import org.jblooming.waf.view.PageSeed;

/**
 * @deprecated 
 */
public class DeleteScreen extends ScreenArea {

  public String listHref;
  public String editHref;
  public PageSeed back;

  public DeleteScreen(ActionController ac) {
    super(ac, "/commons/administration/objectEditor/partDelete.jsp");
  }

}
