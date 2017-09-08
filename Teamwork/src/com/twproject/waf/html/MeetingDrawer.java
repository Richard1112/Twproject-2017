package com.twproject.waf.html;

import org.jblooming.waf.html.core.JspHelper;
import com.twproject.agenda.Event;
import com.twproject.meeting.Meeting;

import javax.servlet.jsp.PageContext;

public class MeetingDrawer  extends JspHelper {

  public Meeting meeting;


  public MeetingDrawer(Meeting meeting) {
     super();
     this.urlToInclude = "/applications/teamwork/meeting/partMeetingEditor.jsp";
     this.meeting =  meeting;
   }

   public void drawMeeting(PageContext pageContext) {
    super.toHtml(pageContext);
  }
}
