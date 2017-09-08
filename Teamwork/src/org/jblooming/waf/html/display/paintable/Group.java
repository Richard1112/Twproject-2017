package org.jblooming.waf.html.display.paintable;

import org.jblooming.PlatformRuntimeException;

import javax.servlet.jsp.PageContext;
import java.util.Set;
import java.util.HashSet;


/**
 *
 * this is a group of paintables. The group is only a logic group; the elements must be added both to the Folio and to the Group.ù
 * When a group is added to Folio is added in the groups collection. The group cannot be added to folio.
 *
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class Group extends Paintable {

  public Set<Paintable> elements = new HashSet<Paintable>();

  public Group() {
    super();
  }


  public String getPaintActionName() {
    return DRAW_GROUP;
  }


  public void add(Paintable p){
    elements.add(p);
  }


  public void toHtml(PageContext pageContext) {
    throw new PlatformRuntimeException("A group cannot be added to the folio");
  }


  public void shift (double x, double y){
    for (Paintable p:elements){
      p.top = p.top+y;
      p.left=p.left+x;
    }

  }


}

