package org.jblooming.operator.businessLogic;

import org.jblooming.messaging.MessagingSystem;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.input.Selector;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.TreeMap;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class OptionController implements ActionController {

  OptionAction optionAction;

   public OptionController() {
    this.optionAction = new OptionAction();
  }

  public OptionController(OptionAction oa) {
    this.optionAction = oa;
  }

  public PageState perform(HttpServletRequest request, HttpServletResponse response)
    throws PersistenceException, ActionException, org.jblooming.security.SecurityException {

    PageState pageState = PageState.getCurrentPageState(request);
    String command = pageState.getCommand();

    if (Commands.SAVE.equals(command))
      optionAction.cmdSave(pageState, request.getContextPath());
     else if ("REMOVE_OPTION".equals(command))
      optionAction.cmdRemoveOption(pageState);
    else if (Commands.DELETE.equals(command))
      optionAction.cmdDelete(pageState);
    else
      optionAction.cmdEdit(pageState);

    TreeMap candidates = new TreeMap();
    for (MessagingSystem.Media media : MessagingSystem.Media.values())
      candidates.put(media.ordinal(), media.name());
    Selector.make("MSG_CHANNEL", candidates, null, pageState);
    return pageState;
  }
}

