package org.jblooming.designer.businessLogic;

import org.jblooming.waf.*;
import org.jblooming.waf.exceptions.*;
import org.jblooming.waf.constants.*;
import org.jblooming.waf.view.*;
import org.jblooming.operator.*;
import org.jblooming.*;
import org.jblooming.designer.*;
import org.jblooming.persistence.exceptions.*;

import javax.servlet.http.*;
import java.io.*;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */

public class DesignerController implements ActionController {

  protected Designer designer;

  public DesignerController(Designer designer) {
    this.designer = designer;
  }

  public PageState perform(HttpServletRequest request, HttpServletResponse response)
    throws PersistenceException, ActionException, org.jblooming.security.SecurityException, ApplicationException {

    PageState pageState = PageState.getCurrentPageState(request);

    DesignerAction designerAction = new DesignerAction();

    String command = pageState.getCommand();

    if (Commands.SAVE.equals(command)) {

      designerAction.cmdSave(designer,pageState);
      if(pageState.getClientEntries().validEntries()) {

        String url = pageState.getEntry("URL_TO_REDIRECT").stringValueNullIfEmpty();
        if(url != null) {
          Serializable id = pageState.getMainObjectId();          
          PageSeed ps = new PageSeed(url);
          pageState.redirect(ps);
          pageState.setMainObjectId(id);
        }
      }
    } else
      designerAction.cmdEdit(designer,pageState);

    return pageState;
  }
}