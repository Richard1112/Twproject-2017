package com.opnlb.website.forum;

import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.operator.Operator;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.messaging.SomethingHappened;
import org.jblooming.utilities.JSP;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Feb 1, 2007
 * Time: 4:05:55 PM
 */
public class ForumBricks {

  public ForumEntry mainObject;

  public ForumBricks(ForumEntry document) {
    this.mainObject = document;
  }

  public String myPage() {

    int depth = mainObject.isNew() && mainObject.getParent() != null ? mainObject.getParent().getDepth() + 1 : mainObject.getDepth();
    
    String mypage = "";
    if (depth == 0) {
      mypage = "forum.jsp";
    }else  if (depth == 1) {
        mypage = "discussion.jsp";
    } else if (depth == 2) {
      mypage = "thread.jsp";
    } else if (depth == 3) {
      mypage = "thread.jsp";
    } else
      mypage = "home.page";

    return mypage;
  }

  public PageSeed editPage(PageState pageState) {
    PageSeed edit = pageState.pageFromRoot(myPage());
    edit.setCommand(Commands.EDIT);
    edit.mainObjectId = mainObject.getRootPost().getId();    
    return edit;
  }

   public void generateSaveEvent(Operator logged) throws StoreException {
    SomethingHappened change = new SomethingHappened();
    change.setIdAsNew();
    change.setEventType("FORUM_NEW_POST");
    change.setMessageTemplate("NEW_POST_MESSAGE_TEMPLATE");
     // remember that "subject" param will be appended to the message' subject
    change.getMessageParams().put("subject", mainObject.getRootPost().getTitle());
    change.getMessageParams().put("post", JSP.limWr(JSP.cleanHTML(mainObject.getContent()),1000));
    change.setWhoCausedTheEvent(logged);
    String href = ApplicationState.serverURL +  "/applications/forum/" + mainObject.getRootPost().bricks.myPage();
    PageSeed ps = new PageSeed(href);
    ps.mainObjectId = mainObject.getRootPost().getId();
    ps.command = Commands.EDIT;
    change.setLink(new ButtonLink(mainObject.getRootPost().getTitle(),ps).toPlainLink());
    change.setIdentifiable(mainObject);
    change.store();
  }

}
