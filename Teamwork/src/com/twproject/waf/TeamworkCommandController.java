package com.twproject.waf;

import com.twproject.messaging.stickyNote.StickyNote;
import com.twproject.messaging.stickyNote.StickyNoteDrawer;
import com.twproject.worklog.Worklog;
import com.twproject.security.TeamworkPermissions;
import net.sf.json.JSONObject;
import org.jblooming.ApplicationException;
import org.jblooming.messaging.MailHelper;
import org.jblooming.ontology.businessLogic.DeleteHelper;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.system.SystemConstants;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.DefaultCommandController;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.Serializable;
import java.util.List;

public class TeamworkCommandController extends DefaultCommandController {

  public PageState perform(HttpServletRequest request, HttpServletResponse response) throws PersistenceException, ActionException, org.jblooming.security.SecurityException, ApplicationException, IOException {
    PageState pageState = PageState.getCurrentPageState(request);


    if (JSP.w(pageState.command).indexOf(StickyNoteDrawer.STICKY_COMMAND_SUFFIX) > -1) {


      Serializable id = JSP.w(pageState.getEntry(SystemConstants.DOM_ID).stringValueNullIfEmpty()).replace("STKN_","");
      if (id != null) {
        StickyNote sn = (StickyNote) PersistenceHome.findByPrimaryKey(StickyNote.class, id);

        //may have just been deleted
        if (sn != null) {

          if ((Commands.CMD_MOVE+StickyNoteDrawer.STICKY_COMMAND_SUFFIX).equals(pageState.command)) {

            sn.setX(pageState.getEntry("X").intValueNoErrorCodeNoExc());
            sn.setY(pageState.getEntry("Y").intValueNoErrorCodeNoExc());

            sn.store();

          } else if ((Commands.CMD_RESIZE+StickyNoteDrawer.STICKY_COMMAND_SUFFIX).equals(pageState.command)) {

            String w = pageState.getEntry("W").stringValueNullIfEmpty();
            String h = pageState.getEntry("H").stringValueNullIfEmpty();
            if (w != null) {
              w = StringUtilities.replaceAllNoRegex(w, "px", "");
            }
            sn.setW(Integer.parseInt(w));

            if (h != null) {
              h = StringUtilities.replaceAllNoRegex(h, "px", "");
            }
            sn.setH(Integer.parseInt(h));
            sn.store();

          } else if ((Commands.CMD_HIDE+StickyNoteDrawer.STICKY_COMMAND_SUFFIX).equals(pageState.command)) {
            sn.remove();
          } else if ((Commands.CMD_CLOSE+StickyNoteDrawer.STICKY_COMMAND_SUFFIX).equals(pageState.command)) {
            sn.remove();

          } else if ((Commands.CMD_COLLAPSE+StickyNoteDrawer.STICKY_COMMAND_SUFFIX).equals(pageState.command)) {
            sn.setIconized(!sn.isIconized());
            sn.store();

          } else if ((Commands.CMD_ICONIZE+StickyNoteDrawer.STICKY_COMMAND_SUFFIX).equals(pageState.command)) {
            sn.setIconized(true);
            sn.store();

          } else if ((Commands.CMD_RESTORE+StickyNoteDrawer.STICKY_COMMAND_SUFFIX).equals(pageState.command)) {
            sn.setIconized(false);
            sn.store();
          }
        }
      }

    } else {
      super.perform(request, response);
    }

    return pageState;

  }
}