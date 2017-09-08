package com.twproject.agenda.businessLogic;

import com.twproject.agenda.AgendaBricks;
import com.twproject.agenda.Event;
import com.twproject.meeting.Meeting;
import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.RemoveException;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.PlatformConstants;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;


public class AgendaController implements ActionController {

  public PageState perform(HttpServletRequest request, HttpServletResponse response) throws PersistenceException, ActionException, org.jblooming.security.SecurityException, ApplicationException {
    PageState pageState = PageState.getCurrentPageState(request);
    AgendaAction action = new AgendaAction(pageState);

    String command = pageState.getCommand();

    if (Commands.ADD.equals(command)) {
      action.cmdAdd();
    } else if (Commands.EDIT.equals(command)) {
      action.cmdEdit();
    } else if ("GUESS".equals(command)) {
      String originalRef = pageState.mainObjectId + "";
      try {
        action.cmdGuess();
      } catch (ActionException ae) {
        PageSeed newSeed = pageState.pageFromRoot("tools/invalidReference.jsp");
        newSeed.addClientEntry("CAUSE", ae.getMessage());
        newSeed.addClientEntry("TYPE", "EVENT");
        newSeed.addClientEntry("REF", originalRef);
        pageState.redirect(newSeed);

      }

    } else if (Commands.DELETE_PREVIEW.equals(command)) {
      action.cmdEdit();
    } else if (Commands.DELETE.equals(command)) {
      try {
        action.cmdDelete();
        PageSeed ps = pageState.pageFromRoot("agenda/agendaWeekDay.jsp");

        if (JSP.ex(pageState.getEntry("DO_NOT_REDIR")))
          pageState.addClientEntry("REDIRECT_TO",ps.toLinkToHref());
        else
          pageState.redirect(ps);

      } catch (RemoveException ex) {
        // in order to feedback operator in partDelete.jsp
        pageState.setAttribute(PlatformConstants.DELETE_EXCEPTION, ex);
      }
    } else if ("REMOVE_ME".equals(command)) {
      action.cmdRemoveMe();
    } else if (Commands.SAVE.equals(command)) {
      try {
        action.cmdSave();

        if (pageState.getMainObject()!=null && ((Event)pageState.getMainObject()).getMeeting()==null) {
          PageSeed newPs = AgendaBricks.getAgendaView(pageState);
          newPs.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));
          newPs.addClientEntry(pageState.getEntry("WG_IDS"));
          newPs.addClientEntry(pageState.getEntry("WG_NAMES"));
          try {
            response.sendRedirect(newPs.toLinkToHref());
          } catch (Exception e) {
            throw new PlatformRuntimeException(e);
          }
        }


      } catch (ActionException e) {
      }
    } else if ("SAVE_AND_CREATE_MEETING".equals(command)) {
      pageState.initializeEntries("row");
      action.cmdSave();
      Event event = (Event) pageState.getMainObject();
      Meeting meeting = new Meeting();
      meeting.setIdAsNew();
      meeting.store();
      meeting.setEvent(event);
      event.setMeeting(meeting);
      event.store();

    } else if ("FIND_TARGET".equals(command)) {
      action.cmdFindTarget();

    } else if (Commands.FIND.equals(command)){
      action.cmdFind();
    } else{
      action.cmdPrepareDefaultFind();
    }


    return pageState;
  }
}

