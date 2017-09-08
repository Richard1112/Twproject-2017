package org.jblooming.persistence.objectEditor.businessLogic;

import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.RemoveException;
import org.jblooming.persistence.objectEditor.FieldFeature;
import org.jblooming.persistence.objectEditor.ObjectEditor;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.html.input.SmartCombo;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.PlatformConstants;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.view.ClientEntries;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 12-apr-2005 : 16.07.38
 */
public class ObjectEditorController implements ActionController {
  protected ObjectEditor objectEditor;

  public ObjectEditorController(ObjectEditor objectEditor) {
    this.objectEditor = objectEditor;
  }

  public PageState perform(HttpServletRequest request, HttpServletResponse response) throws PersistenceException, ActionException, org.jblooming.security.SecurityException, ApplicationException, IOException {

    PageState pageState = PageState.getCurrentPageState(request);
    ObjectEditorAction oea = new ObjectEditorAction();

    String command = pageState.getCommand();


    if (Commands.EDIT.equals(command))
      oea.cmdEdit(objectEditor, pageState);

    else if (Commands.ADD.equals(command)) {
      oea.cmdAdd(objectEditor, pageState);

    } else if (Commands.DUPLICATE.equals(command)) {
        oea.cmdDuplicate(objectEditor, pageState);

    } else if (Commands.SAVE.equals(command)) {
      try {
        oea.cmdSave(objectEditor, pageState);
      } catch (ActionException e) {
      }

    } else if (Commands.DELETE.equals(command)) {
      try {
        oea.cmdDelete(objectEditor, pageState);
        pageState.setCommand(Commands.FIND);
        for (FieldFeature fieldFeature : objectEditor.displayFields.values()) {
          String fieldName = fieldFeature.fieldName;
          pageState.removeEntry(fieldName);
          if (fieldFeature.smartCombo != null) {
            fieldName = fieldName + SmartCombo.TEXT_FIELD_POSTFIX;
            pageState.removeEntry(fieldName);
          }
          if (fieldFeature.boolAsCombo) {
            pageState.removeEntry(fieldName);
          }
        }
        oea.cmdFind(objectEditor, pageState);
        objectEditor.urlToInclude = ObjectEditor.listUrl;
      } catch (RemoveException ex) {
        // in order to feedback operator in partDelete.jsp
        pageState.setAttribute(PlatformConstants.DELETE_EXCEPTION, ex);
      } catch (Exception ex) {
        throw new PlatformRuntimeException(ex);
      }

    } else if ("DEL_ALL".equals(command)) {
      oea.cmdDelAll(objectEditor, pageState);

    } else if (Commands.DELETE_PREVIEW.equals(command)) {
      oea.cmdDeletePreview(objectEditor, pageState);

    } else if (Commands.FIND.equals(command)) {
      oea.cmdFind(objectEditor, pageState);
    } else {
      oea.unmake(objectEditor, pageState);
      oea.cmdFind(objectEditor, pageState);
    }

    pageState.setMainJspIncluder(objectEditor);


    return pageState;
  }

}
