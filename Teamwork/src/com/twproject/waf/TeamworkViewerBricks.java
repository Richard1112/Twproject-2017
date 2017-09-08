package com.twproject.waf;

import com.teamwork.expand.TaskReport;
import com.twproject.agenda.Event;
import com.twproject.document.TeamworkDocument;
import com.twproject.forum.TeamworkForumEntry;
import com.twproject.meeting.Meeting;
import com.twproject.messaging.board.Board;
import com.twproject.resource.Person;
import com.twproject.resource.Resource;
import com.twproject.security.TeamworkPermissions;
import com.twproject.task.Issue;
import com.twproject.task.IssueBricks;
import com.twproject.task.Task;
import com.twproject.worklog.Worklog;
import com.twproject.worklog.WorklogPlan;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.designer.DesignerData;
import org.jblooming.ontology.Identifiable;
import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.EntityViewerBricks;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Oct 29, 2007
 * Time: 4:20:21 PM
 */
public class TeamworkViewerBricks implements EntityViewerBricks {

  public EntityLinkSupport getLinkSupportForEntity(Identifiable i, PageState pageState) {

    EntityLinkSupport result = new EntityLinkSupport();

    if (i instanceof Issue) {
      result.bs = IssueBricks.getBlackEditor(i.getId());
      Issue issue = (Issue) i;
      result.bs.label = "I#"+issue.getMnemonicCode()+ "#  "+JSP.limWr(issue.getDisplayName(), 200);
      result.bs.iconChar = "i";
      result.readPermission = TeamworkPermissions.issue_canRead;

    } else if (i instanceof Task) {
      Task task = (Task) i;
      PageSeed ps = pageState.pageFromRoot("task/taskOverview.jsp");
      ps.mainObjectId = task.getId();
      ps.command = Commands.EDIT;
      result.bs = new ButtonLink("T#"+task.getMnemonicCode()+ "#  "+task.getName(), ps);

      result.bs.iconChar = "t";

      result.readPermission = TeamworkPermissions.task_canRead;

    } else if (i instanceof Resource) {
      PageSeed ps = pageState.pageFromRoot("resource/resourceEditor.jsp");
      Resource resource=(Resource)i;
      ps.mainObjectId = i.getId();
      ps.command = Commands.EDIT;
      result.bs = new ButtonLink("R#"+resource.getMnemonicCode()+ "#  "+i.getName(), ps);

      boolean isPerson = (i instanceof Person);
      result.bs.iconChar = "M";

      result.readPermission = TeamworkPermissions.resource_canRead;

    } else if (i instanceof Board) {
      PageSeed ps = pageState.pageFromRoot("board/board.jsp");
      ps.mainObjectId = i.getId();
      ps.command = Commands.EDIT;
      result.bs = new ButtonLink(i.getName(), ps);

      result.bs.iconChar = "B";

      result.readPermission = TeamworkPermissions.board_canRead;

    } else if (i instanceof Event) {
      Event e = (Event) i;
      PageSeed ps = pageState.pageFromRoot("agenda/agendaEditor.jsp");
      ps.mainObjectId = i.getId();
      ps.command = Commands.EDIT;
      result.bs = new ButtonLink(e.getDisplayName(), ps);

      result.bs.iconChar = "C";

      //any :-D
      result.readPermission = TeamworkPermissions.board_canRead;

    } else if (i instanceof TeamworkForumEntry) {
      TeamworkForumEntry fe = (TeamworkForumEntry) i;
      PageSeed ps = pageState.pageFromRoot("task/taskForumThread.jsp");
      if (fe.getTask() != null) {
        ps.mainObjectId = fe.getId();
        ps.addClientEntry("TASK_ID", fe.getTask().getId());
        ps.command = "LIST_POSTS";
        result.bs = new ButtonLink(JSP.ex(fe.getTitle()) ? fe.getTitle() : JSP.limWr(fe.getContentClean(), 50), ps);
        result.bs.iconChar = "Q";
        result.readPermission = TeamworkPermissions.task_canRead;
      }

    } else if (i instanceof Meeting) {
      Meeting m = (Meeting)i;
      PageSeed ps = pageState.pageFromRoot("agenda/agendaEditor.jsp");
      ps.mainObjectId = m.getEvent().getId();
      ps.command = Commands.EDIT;
      result.bs = new ButtonLink(m.getEvent().getSummary(), ps);
      result.bs.iconChar = "C";

      //any :-D
      result.readPermission = TeamworkPermissions.board_canRead;

    } else if (i instanceof Worklog) {
      Worklog w = (Worklog) i;
      PageSeed ps = pageState.pageFromRoot("task/worklog/worklogAssignmentList.jsp");
      ps.command=Commands.FIND;
      ps.addClientEntry("ASS_ID", w.getAssig().getId());
      result.bs = ButtonLink.getBlackInstance(w.getAssig().getTask().getDisplayName(), 600,800,ps);

      result.bs.iconChar = "A";

      result.readPermission = TeamworkPermissions.worklog_manage;

    } else if (i instanceof WorklogPlan) {
      WorklogPlan w = (WorklogPlan) i;
      PageSeed ps = pageState.pageFromRoot("task/plan/planByTask.jsp");
      ps.mainObjectId = w.getAssig().getTask().getId();
      ps.command = Commands.EDIT;
      ps.addClientEntry("FOCUS_MILLIS",w.getInserted().getTime());
      result.bs = new ButtonLink(JSP.ex(w.getAction()) ? w.getAction() : w.getAssig().getDisplayName(), ps);
      result.bs.iconChar = "€";
      result.bs.additionalCssClass="lreq30 lreqLabel";
      result.readPermission = TeamworkPermissions.task_canRead;

    } else if (i instanceof TeamworkDocument) {

      TeamworkDocument td = (TeamworkDocument) i;
      result.bs = td.bricks.getReferralButtonPointToDoc();
      if(result.bs == null){
        result.bs = td.bricks.getContentLink(pageState).getButton();
      }
      result.bs.label = td.getName();
      result.shortDescription = td.getName();
      result.longDescription = td.getName();
      result.readPermission = TeamworkPermissions.document_canRead;

      result.bs.iconChar = "c";

     }else if (i instanceof TaskReport) {

    	 TaskReport td = (TaskReport) i;
         result.bs = td.bricks.getReferralButtonPointToReport();
         if(result.bs == null){
           result.bs = td.bricks.getContentLink(pageState).getButton();
         }
         result.bs.label = td.getName();
         result.shortDescription = td.getName();
         result.longDescription = td.getName();
         result.readPermission = TeamworkPermissions.document_canRead;

         result.bs.iconChar = "c";

      } else if (i instanceof DesignerData) {

      DesignerData dd = (DesignerData)i;
      try {
        IdentifiableSupport is = (IdentifiableSupport) PersistenceHome.findByPrimaryKey((Class<? extends Identifiable>) Class.forName(dd.getReferenceClassName()),dd.getReferenceId());
        result = getLinkSupportForEntity(is,pageState); 
      } catch (Throwable e) {
        throw new PlatformRuntimeException(e);
      }


    } else {
      throw new PlatformRuntimeException("Class unhandled: " + i.getClass());
    }



    return result;
    
  }

}
