package com.twproject.waf.html;

import com.twproject.task.Issue;
import com.twproject.task.Task;
import org.jblooming.utilities.HashTable;
import org.jblooming.waf.html.core.JspHelper;

import javax.servlet.jsp.PageContext;
import java.util.Map;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class IssueDrawer extends JspHelper {

  public boolean recurseOnChildren = true;
  public Task task;
  public Map<String, Long> totWByIssue;
  public Issue currentIssue;
  public boolean readMode = false;
  public boolean linkToResources = true;
  public boolean sorted = false;
  public boolean dragEnabled=false;
  public boolean justInserted=false;


  public IssueDrawer(Task task) {
    super();
    this.urlToInclude = "/applications/teamwork/issue/partIssueLine.jsp";
    this.task = task;
  }

  public void drawIssue(Issue issue, PageContext pageContext) {
    currentIssue = issue;
    super.toHtml(pageContext);
  }


  /**
   * @deprecated
   */
  public void toHtml(PageContext pageContext) {
    throw new RuntimeException("Call task and issue");
  }


}
