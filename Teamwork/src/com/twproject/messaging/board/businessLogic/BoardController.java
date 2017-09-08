package com.twproject.messaging.board.businessLogic;

import org.jblooming.ApplicationException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.RemoveException;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.PlatformConstants;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class BoardController implements ActionController {

  public PageState perform(HttpServletRequest request, HttpServletResponse response)
    throws PersistenceException, ActionException, org.jblooming.security.SecurityException, ApplicationException {

    PageState pageState = PageState.getCurrentPageState(request);

    BoardAction ba = new BoardAction(pageState);

    String command = pageState.getCommand();

    if (Commands.ADD.equals(command))
      ba.cmdAdd();

    else if (Commands.EDIT.equals(command) || Commands.DELETE_PREVIEW.equals(command))
      ba.cmdEdit();

    else if (Commands.SAVE.equals(command))
      ba.cmdSave();

    else if ("GUESS".equals(command)) {
      String originalRef = pageState.mainObjectId + "";
      try {
        ba.cmdGuess();
      } catch (ActionException ae) {
        PageSeed newSeed = pageState.pageFromRoot("tools/invalidReference.jsp");
        newSeed.addClientEntry("CAUSE", ae.getMessage());
        newSeed.addClientEntry("TYPE", "BOARD");
        newSeed.addClientEntry("REF", originalRef);
        pageState.redirect(newSeed);

      }

    } else if ("SAVE_SUBS".equals(command))
      ba.cmdSaveSubs();

    else if (Commands.DELETE.equals(command)) {
        try {
          ba.cmdDelete();
          //rboard editor is in popup and it is closed js side
        } catch (RemoveException ex) {
          // in order to feedback operator in partDelete.jsp
          pageState.setAttribute(PlatformConstants.DELETE_EXCEPTION, ex);
        }


    } else if (Commands.FIND.equals(command)) {
      ba.cmdFind();
    }

    return pageState;
  }
}
