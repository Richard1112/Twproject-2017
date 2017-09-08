package com.twproject.forum.businessLogic;

import com.twproject.forum.TeamworkForumEntry;
import com.twproject.operator.TeamworkOperator;
import com.twproject.rank.Hit;
import com.twproject.security.TeamworkPermissions;
import com.twproject.task.*;
import org.jblooming.messaging.SomethingHappened;
import org.jblooming.ontology.businessLogic.DeleteHelper;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.security.SecurityException;
import org.jblooming.utilities.*;
import org.jblooming.waf.ActionSupport;
import org.jblooming.waf.ActionUtilities;
import org.jblooming.waf.constants.*;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import java.io.Serializable;
import java.util.*;


public class ForumAction extends ActionSupport {


  public TeamworkOperator logged;
  public Task task;
  public TeamworkForumEntry post;


  public ForumAction(PageState pageState) {
    super(pageState);
    this.logged = (TeamworkOperator) pageState.getLoggedOperator();
  }


  public void editNoMake() throws PersistenceException {
    if (JSP.ex(restState.mainObjectId)) {
      post = TeamworkForumEntry.load(restState.mainObjectId);
      if (post != null && post.getTask() != null)
        restState.addClientEntry("TASK_ID", post.getTask());
    }
    loadTask();
  }


  private void loadTask() throws PersistenceException {
    this.task = Task.load(restState.getEntry("TASK_ID").intValueNoErrorCodeNoExc() + "");
    restState.attributes.put("REFERRAL_OBJECT", task);
  }


  public void cmdEdit() throws PersistenceException, ActionException, SecurityException {
    editNoMake();
    task.testPermission(logged, TeamworkPermissions.task_canRead);

    restState.setMainObject(post);
    restState.addClientEntry("POST_MESSAGE", post.getContent());
    restState.addClientEntry("POST_SUBJECT", post.getTitle());
    Hit.getInstanceAndStore(post, logged, .1);

  }

  public void cmdDelete() throws PersistenceException, ActionException, SecurityException {
    editNoMake();
    task.testPermission(logged, TeamworkPermissions.task_canWrite);
    DeleteHelper.cmdDelete(post, restState);
  }


  public void cmdAdd() throws PersistenceException, SecurityException {
    restState.initializeEntries("cell");
    editNoMake();
    task.testPermission(logged, TeamworkPermissions.task_canRead);
    TeamworkForumEntry tfe = new TeamworkForumEntry();
    tfe.setIdAsNew();
    tfe.setAuthor(logged);

    restState.setMainObject(tfe);
  }

  public void cmdReply() throws PersistenceException, SecurityException, ActionException {
    restState.initializeEntries("cell");
    // usa il cmdSavePost
    //si creano le CE con quello che si ha
    Serializable oldMainObjectId = restState.mainObjectId;
    restState.addClientEntry("THREAD_ID", oldMainObjectId);
    restState.addClientEntry("POST_MESSAGE", restState.getEntry("REPLY_MESSAGE").stringValueNullIfEmpty());
    restState.mainObjectId=null;
    cmdSavePost();
    restState.removeEntry("POST_MESSAGE");
    restState.removeEntry("REPLY_MESSAGE");
    restState.mainObjectId=oldMainObjectId;
    cmdEdit();
  }

  public void cmdSavePost() throws PersistenceException, ActionException, SecurityException {
    restState.initializeEntries("cell");
    editNoMake();
    TeamworkForumEntry root = task.getForumEntry();
    //se non c'è si crea
    if (root==null){
      root = new TeamworkForumEntry();
      root.setTask(task);
      task.setForumEntry(root);
      root.store();
    }

    TeamworkForumEntry parent = TeamworkForumEntry.load(restState.getEntry("THREAD_ID").intValueNoErrorCodeNoExc() + "");
    parent = parent == null ? root : parent;

    TeamworkForumEntry forumEntry = null;
    if (restState.mainObjectId == null || PersistenceHome.NEW_EMPTY_ID.equals(restState.mainObjectId)) {
      forumEntry = new TeamworkForumEntry();
      //not a slip, we want externat people to be able to comment
      task.testPermission(logged, TeamworkPermissions.task_canRead);
      forumEntry.setParent(parent);
    } else {
      forumEntry =post;
      if (!(logged.hasPermissionAsAdmin() || logged.equals(forumEntry.getAuthor())))
        throw new SecurityException(SecurityConstants.I18N_PERMISSION_LACKING + " forum write");
    }
    ActionUtilities.setString(restState.getEntry("POST_SUBJECT"), forumEntry, "title");

    String content = restState.getEntry("POST_MESSAGE").stringValueNullIfEmpty();
    if (content == null)
      content = "";
    content = content.replaceAll("\r", "").replaceAll("\n", "<br>");
    forumEntry.setContent(content);

    forumEntry.setLastPosterOnBranch(logged.getPerson().getDisplayName());
    forumEntry.setAuthor(logged);

    Date postedOn = new Date();
    forumEntry.setPostedOn(postedOn);
    forumEntry.setLastPostOnBranch(postedOn);
    parent.setLastPostOnBranch(postedOn);
    root.setLastPostOnBranch(postedOn);

    if (restState.validEntries()) {

      forumEntry.setParentAndStore(parent);
      forumEntry.setTask(task);
      forumEntry.store();
      parent.store();
      root.store();

      SomethingHappened change = new SomethingHappened();
      change.setIdAsNew();
      change.setEventType(Task.Event.TASK_DIARY_CHANGE + "");
      change.setWhoCausedTheEvent(logged);
      change.getMessageParams().put("SUBJECT", JSP.limWr(task.getDisplayName(), 30));
      change.setMessageTemplate(Task.Event.TASK_DIARY_CHANGE + "_MESSAGE_TEMPLATE");

      change.getMessageParams().put("task", task.getDisplayName());

      PageSeed ps = new PageSeed(ApplicationState.serverURL + "/applications/teamwork/task/taskForumThread.jsp");
      ps.command = "LIST_POSTS";
      ps.addClientEntry("TASK_ID", task.getId());

      if (task.getForumEntry().equals(parent)) { // editing tlog
        change.getMessageParams().put("postTitle", JSP.w(forumEntry.getTitle()));
        ps.mainObjectId = forumEntry.getId();
      } else {
        change.getMessageParams().put("postTitle", JSP.w(parent.getTitle()));
        ps.mainObjectId = parent.getId();
      }

      ButtonLink edit = new ButtonLink(ps);
      edit.label = task.getDisplayName();
      change.setLink(edit.toPlainLink());
      change.setIdentifiable(task);
      change.store();
      Hit.getInstanceAndStore(forumEntry, logged, .1);

      // ok message feedback
      restState.addMessageOK(I18n.get("POST_CORRECTLY_SAVED"));


    }

    if (task.getForumEntry().equals(parent)) { // editing tlog
      restState.setCommand("");
    } else {
      restState.setCommand("LIST_THREAD");
      restState.addClientEntry("THREAD_ID", parent.getId());
    }
  }


}