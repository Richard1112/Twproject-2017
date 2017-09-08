package com.twproject.waf.html;

import com.twproject.agenda.Event;
import com.twproject.resource.Resource;
import org.jblooming.waf.html.core.HtmlBootstrap;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.display.paintable.Paintable;
import org.jblooming.waf.html.state.Form;
import org.jblooming.waf.view.PageState;

import javax.servlet.ServletException;
import java.io.IOException;
import java.util.HashMap;

public class EventWriter extends JspHelper implements HtmlBootstrap {
  public Event event;
  public HashMap<Resource, String> colorInfo;
  public boolean showTargetLegenda=true;
  public Paintable rectangle;


  public EventWriter(String id, Form form, Event event, HashMap<Resource, String> colorInfo) {
    this.id = id;
    this.event=event;
    urlToInclude = "/applications/teamwork/agenda/waf/partEventWriter.jsp";
    this.colorInfo=colorInfo;
  }


  public String getDiscriminator() {
    return this.getClass().getName();
  }

  public boolean validate(PageState pageState) throws IOException, ServletException {
    return true;
  }
  
}
