package com.twproject.forum.businessLogic;

import com.twproject.task.Task;
import com.twproject.task.businessLogic.AssignmentAction;
import com.twproject.forum.TeamworkForumEntry;
import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.ontology.businessLogic.DeleteHelper;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.RemoveException;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.PlatformConstants;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public class ForumController implements ActionController {

  public PageState perform(HttpServletRequest request, HttpServletResponse response) throws PersistenceException, ActionException, org.jblooming.security.SecurityException, ApplicationException, IOException {

    PageState pageState = PageState.getCurrentPageState(request);

    ForumAction action = new ForumAction(pageState);
    String command = pageState.getCommand();

    if (Commands.ADD.equals(command)) {
      action.cmdAdd();

    } else if (Commands.EDIT.equals(command)||"LIST_POSTS".equals(command)) {
      action.cmdEdit();

    } else if (Commands.SAVE.equals(command)) {
      try {
        action.cmdSavePost();
        PageSeed ps;
        TeamworkForumEntry thread = TeamworkForumEntry.load(pageState.getEntry("THREAD_ID").intValueNoErrorCodeNoExc());
        if (thread!=null){
          ps= pageState.pageFromRoot("task/taskForumThread.jsp");
          ps.mainObjectId=thread.getId();
          ps.command="LIST_POSTS";

        }else{
          ps= pageState.pageFromRoot("task/taskForumList.jsp");
          ps.setCommand("");
        }
        ps.addClientEntry("TASK_ID", action.task.getId());
        pageState.redirect(ps);

      } catch (ActionException e) {
      }

    } else if (Commands.DELETE.equals(command)) {
      try {
        action.cmdDelete();
        PageSeed ps;
        TeamworkForumEntry thread = TeamworkForumEntry.load(pageState.getEntry("THREAD_ID").intValueNoErrorCodeNoExc());
        if (thread!=null){
          ps= pageState.pageFromRoot("task/taskForumThread.jsp");
          ps.mainObjectId=thread.getId();
          ps.command="LIST_POSTS";

        }else{
          ps= pageState.pageFromRoot("task/taskForumList.jsp");
          ps.setMainObjectId(pageState.mainObjectId);
          ps.setCommand("");
        }
        ps.addClientEntry("TASK_ID", action.task.getId());


        if (JSP.ex(pageState.getEntry("DO_NOT_REDIR")))
          pageState.addClientEntry("REDIRECT_TO",ps.toLinkToHref());
        else
          pageState.redirect(ps);


      } catch (Throwable ex) {
        // in order to feedback operator in partDelete.jsp
        pageState.setAttribute(PlatformConstants.DELETE_EXCEPTION, ex);
      }

    } else if ("REPLY".equals(command)) {
      action.cmdReply();


    } else if (Commands.DELETE_PREVIEW.equals(command)) {
      action.cmdEdit();

    } else { // FIND
      action.editNoMake();
    }

    try {
      ReflectionUtilities.invoke(ApplicationState.applicationParameters.get("get"), "doFilter", request, response);
    } catch (Throwable e) {
      throw new PlatformRuntimeException(e);
    }


    return pageState;
  }


}