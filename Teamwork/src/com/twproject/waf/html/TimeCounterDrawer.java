package com.twproject.waf.html;

import com.twproject.task.Assignment;
import org.jblooming.waf.html.core.JspHelper;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class TimeCounterDrawer extends JspHelper {


  public Assignment assig;


  public TimeCounterDrawer(Assignment assig) {
    super();
    this.urlToInclude = "/applications/teamwork/task/partDrawTimeCounterLine.jsp";
    this.assig = assig;
  }



}
