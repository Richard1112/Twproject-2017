package org.jblooming.waf;

import org.jblooming.ApplicationException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.system.SystemConstants;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.container.Container;
import org.jblooming.waf.html.core.JspIncluder;
import org.jblooming.waf.html.state.ScreenElementStatus;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.settings.businessLogic.I18nController;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.lang.reflect.Constructor;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 14-feb-2006 : 18.01.30
 */
public class DefaultCommandController implements ActionController {

  PageState pageState;
  String domId;


  public PageState perform(HttpServletRequest request, HttpServletResponse response) throws PersistenceException, ActionException, org.jblooming.security.SecurityException, ApplicationException, IOException {

    boolean debug = false;
    pageState = PageState.getCurrentPageState(request);
    domId = pageState.getEntry(SystemConstants.DOM_ID).stringValueNullIfEmpty();
    response.setContentType("text/plain");
    try {

      if (pageState != null && pageState.getCommand() != null) {
        String command = pageState.getCommand();
        if (debug) {
          Tracer.platformLogger.info(" System.out commands.jsp: " + command + "\n" + pageState.getClientEntries().toString());
        }



        if (command.equals(Commands.CMD_MOVE)) {
          cmdMove();

        } else if (command.equals(Commands.CMD_COLLAPSE)) {
          cmdCollapse();

        } else if (command.equals(Commands.CMD_RESTORE)) {
          cmdRestore();

        } else if (command.equals(Commands.CMD_HIDE)) {
          cmdHide();

        } else if (command.equals(Commands.CMD_ICONIZE)) {
          cmdIconize();

        } else if (command.equals(Commands.CMD_SHOW)) {
          cmdShow();

        } else if (command.equals(Commands.CMD_RESIZE)) {
          cmdResize();

          // call generic controller
        } else if ("CALLCONTR".equals(command)) {
          cmdCallController(request, response);


        } else if ("SAVE_IN_OPT".equalsIgnoreCase(command)) {
          cmdSaveEntryInOptions();

        }

      }
    } catch (Throwable e) {
      Tracer.platformLogger.error(e);
    }
    return pageState;
  }

  private void cmdSaveEntryInOptions() throws PersistenceException {
    String name = pageState.getEntry("CEName").stringValueNullIfEmpty();
    String value = pageState.getEntry("CEValue").stringValueNullIfEmpty();
    if (JSP.ex(name)) {
      ClientEntry ce = pageState.getEntryOrDefault(name);
      if (JSP.ex(value)) {
        ce.setValue(value);
      }
    }
  }

  private void cmdCallController(HttpServletRequest request, HttpServletResponse response) {
    //"executeCommand(\"CALLCONTR\",\"CTCL=" + controller.getName() + "&CTRM=" + command + "&OBID=" + objId +  "\");";
    //what if I have to pass more than one argument? Just set it as concainschifated value of  OBID
    String className = pageState.getEntry("CTCL").stringValueNullIfEmpty();
    String cmd = pageState.getEntry("CTRM").stringValueNullIfEmpty();
    String obId = pageState.getEntry("OBID").stringValueNullIfEmpty();

    if (JSP.ex(className) && JSP.ex(cmd)) { //&& JSP.ex(obId)
      try {
        Class<? extends ActionController> controllerCl = (Class<? extends ActionController>) Class.forName(className);
        Constructor cWithPageState = null;
        try {
          cWithPageState = controllerCl.getConstructor(PageState.class);
        } catch (NoSuchMethodException e) {
        } catch (SecurityException e) {
        }
        ActionController controller = null;
        if (cWithPageState != null) {
          controller = (ActionController) cWithPageState.newInstance(pageState);
        } else
          controller = controllerCl.newInstance();

        pageState.command = cmd;
        pageState.mainObjectId = obId;
        controller.perform(request, response);
      } catch (Throwable e) {
        Tracer.platformLogger.error("DefaultCommandController: CallController. classname:" + className + " object id:" + obId, e);
      }

    } else {
      Tracer.platformLogger.error("DefaultCommandController: CallController. ControllerClass param missing, or command missing"); //or obj id missing,
    }
  }

  private void cmdResize() throws ApplicationException {
    if (!domId.startsWith(JspIncluder.DOM_ID)) {
      ScreenElementStatus cs = pageState.sessionState.screenElementsStatus.get(domId);
      if (cs == null)
        cs = new ScreenElementStatus(domId);
      String w = pageState.getEntry("W").stringValueNullIfEmpty();
      String h = pageState.getEntry("H").stringValueNullIfEmpty();
      cs.w = w;
      cs.h = h;

      pageState.sessionState.screenElementsStatus.put(domId, cs);
    }
  }

  private void cmdHide() throws ApplicationException {
    if (!domId.startsWith(JspIncluder.DOM_ID)) {
      ScreenElementStatus cs = pageState.sessionState.screenElementsStatus.get(domId);
      if (cs == null)
        cs = new ScreenElementStatus(domId);
      cs.status = Container.HIDDEN;
      pageState.sessionState.screenElementsStatus.put(domId, cs);
    }
  }

  private void cmdIconize() throws ApplicationException {
    if (!domId.startsWith(JspIncluder.DOM_ID)) {
      ScreenElementStatus cs = pageState.sessionState.screenElementsStatus.get(domId);
      if (cs == null)
        cs = new ScreenElementStatus(domId);
      cs.status = Container.ICONIZED;
      pageState.sessionState.screenElementsStatus.put(domId, cs);
    }
  }

  private void cmdCollapse() throws ApplicationException {
    if (!domId.startsWith(JspIncluder.DOM_ID)) {
      ScreenElementStatus cs = pageState.sessionState.screenElementsStatus.get(domId);
      if (cs == null)
        cs = new ScreenElementStatus(domId);
      cs.status =  Container.COLLAPSED;
      pageState.sessionState.screenElementsStatus.put(domId, cs);
    }
  }

  private void cmdRestore() throws ApplicationException {
    if (!domId.startsWith(JspIncluder.DOM_ID)) {
      ScreenElementStatus cs = pageState.sessionState.screenElementsStatus.get(domId);
      if (cs == null)
        cs = new ScreenElementStatus(domId);
      cs.status = Container.DEFAULT;
      pageState.sessionState.screenElementsStatus.put(domId, cs);
    }
  }

  private void cmdShow() throws ApplicationException {
    if (!domId.startsWith(JspIncluder.DOM_ID)) {
      ScreenElementStatus cs = pageState.sessionState.screenElementsStatus.get(domId);
      if (cs == null)
        cs = new ScreenElementStatus(domId);
      cs.status = Container.DEFAULT ;
      pageState.sessionState.screenElementsStatus.put(domId, cs);
    }
  }

  private void cmdMove() throws ApplicationException {
    if (!domId.startsWith(JspIncluder.DOM_ID)) {
      int x = pageState.getEntry("X").intValueNoErrorCodeNoExc();
      int y = pageState.getEntry("Y").intValueNoErrorCodeNoExc();
      ScreenElementStatus cs = pageState.sessionState.screenElementsStatus.get(domId);
      if (cs == null)
        cs = new ScreenElementStatus(domId);
      cs.x = x;
      cs.y = y;
      pageState.sessionState.screenElementsStatus.put(domId, cs);
    }
  }

}
