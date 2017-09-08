package com.twproject.waf.html;

import com.twproject.agenda.PeriodEvent;
import com.twproject.resource.Resource;
import org.jblooming.waf.html.core.HtmlBootstrap;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.display.paintable.Paintable;
import org.jblooming.waf.view.PageState;

import javax.servlet.ServletException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;

public class EventListWriter extends JspHelper implements HtmlBootstrap {
  public ArrayList<PeriodEvent> periodEvents;


  public EventListWriter(ArrayList<PeriodEvent> periodEvents, HashMap<Resource, String> colorInfo) {
    super("/applications/teamwork/agenda/waf/partEventListWriter.jsp");
    this.periodEvents=periodEvents;
  }

  public String getDiscriminator() {
    return this.getClass().getName();
  }

  public boolean validate(PageState pageState) throws IOException, ServletException {
    return true;
  }
}
