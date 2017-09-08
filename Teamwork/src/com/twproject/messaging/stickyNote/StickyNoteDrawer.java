package com.twproject.messaging.stickyNote;

import org.jblooming.waf.html.container.Container;
import org.jblooming.waf.html.core.JspHelper;

import javax.servlet.jsp.PageContext;


public class StickyNoteDrawer extends JspHelper {

  public StickyNote stickyNote;
  public boolean canWrite=true;
  public boolean belongsToBoard=false;


  public static final String STICKY_COMMAND_SUFFIX="_STICKY";
  public String containment; //a jquery selector

  public StickyNoteDrawer(StickyNote sn){
    this.urlToInclude = "/applications/teamwork/sticky/stickyDrawer.jsp";
    stickyNote=sn;
  }


  public void toHtml(PageContext pageContext)  {
      pageContext.getRequest().setAttribute(ACTION, Container.BOX_START);
      super.toHtml(pageContext);
  }


  public String getContainerTableId(){
      return "sntbl_"+stickyNote.getId();
  }
  public String getContainerDivId(){
      return "sndivbdid_"+stickyNote.getId();
  }


  public String getContainerTitleId(){
      return "sntitid_"+stickyNote.getId();
  }

}
