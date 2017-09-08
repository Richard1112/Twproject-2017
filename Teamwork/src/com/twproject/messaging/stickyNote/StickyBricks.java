package com.twproject.messaging.stickyNote;

import org.jblooming.waf.Bricks;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.html.button.ButtonSupport;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 17-feb-2006 : 11.44.43
 */
public class StickyBricks extends Bricks {
   public StickyNote mainObject;


 public StickyBricks(StickyNote mainObject){
   this.mainObject=mainObject;
 }

  /**
   *
   * @param commandAndEntries  a fake ps with only command and client entries
   * @param pageState  @return
   */
  public ButtonSupport getEditorInBlack(String label, PageSeed commandAndEntries, PageState pageState){
    PageSeed ps = pageState.pageFromRoot("sticky/stickyEditor.jsp");
    ps.addClientEntries(commandAndEntries.getClientEntries());
    ps.command=commandAndEntries.command;
    ps.setMainObjectId(mainObject.getId());
    ps.setPopup(true);

    return ButtonLink.getBlackInstance(label,mainObject.getH()<600?600:mainObject.getH(),mainObject.getW()<600?600:mainObject.getW(),ps );
  }


}
