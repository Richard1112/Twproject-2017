package org.jblooming.flowork.businessLogic;

import org.jblooming.ApplicationException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.input.Collector;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */
public class FlowFormSetupController implements ActionController {

  public PageState perform(HttpServletRequest request, HttpServletResponse response)
          throws PersistenceException, ActionException, org.jblooming.security.SecurityException, ApplicationException, IOException {

    PageState pageState = PageState.getCurrentPageState(request);
    final String command = pageState.getCommand();

    FlowFormSetupAction sfa = new FlowFormSetupAction();

    if (Collector.isCollectorCommand("CPF", command))

      Collector.move("CPF", pageState);

    else if (Commands.SAVE.equals(command)) {

      //first synch
      pageState.setCommand(Commands.SYNCHRONIZE);
      Collector.move("CPF", pageState);

      //save

       sfa.save(pageState);

    } else {

       sfa.edit(pageState);


    }

    return pageState;
  }
}
