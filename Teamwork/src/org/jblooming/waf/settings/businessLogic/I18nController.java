package org.jblooming.waf.settings.businessLogic;

import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.operator.User;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public class I18nController implements ActionController {

  public PageState perform(HttpServletRequest request, HttpServletResponse response) throws PersistenceException, ActionException, org.jblooming.security.SecurityException, ApplicationException {
    PageState pageState = PageState.getCurrentPageState(request);

    User loggedOp = pageState.getLoggedOperator();
    I18nAction a = new I18nAction();

    String command = pageState.getCommand();

    if (I18n.CMD_DUMP.equals(command)) {
      try {
        a.cmdDump(pageState,request);
      } catch (IOException e) {
        throw new PlatformRuntimeException(e);
      }

    } else if (Commands.EDIT.equals(command)) {
      a.cmdEdit(pageState);

    } else if (I18n.CMD_NEW_ENTRY.equals(command)) {
      a.cmdEdit(pageState);


    } else if (Commands.SAVE.equals(command)) {      
      a.cmdSave(pageState,request);

    } else if (I18n.CMD_CHANGEMODALITY.equals(command)) {
      a.cmdChangeModality(pageState);

    } else if (I18n.CMD_NEW_LANGUAGE.equals(command)) {
      a.cmdNewLanguage(pageState);

    } else if (I18n.CMD_STORE_LABEL.equals(command)) {
      a.cmdStoreLabel(pageState);

    } else if (I18n.CMD_REMOVE_LABEL.equals(command)) {
      a.cmdRemoveLabel(pageState);

    } else if ("I18N_FILE".equals(command)) {
      a.cmdImportFromFile(pageState);

    } else if (I18n.CMD_RELOAD.equals(command)) {
        a.cmdReload();
    } else if ("I18N_SAVE_ENABLED_LANG".equals(command)) {
      a.cmdSaveEnabledLanguages(pageState);
    } else {
      a.cmdList(pageState);
    }

    return pageState;
  }

}
