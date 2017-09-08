<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.task.Issue, com.twproject.task.Task,
                 com.twproject.task.businessLogic.IssueAction, com.twproject.waf.html.IssueDrawer, org.jblooming.persistence.PersistenceHome, org.jblooming.tracer.Tracer, org.jblooming.utilities.JSP,
                 org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.view.PageState, java.util.Date, com.twproject.task.IssueBricks, org.jblooming.utilities.ReflectionUtilities, com.twproject.task.Assignment, com.twproject.resource.Resource" %>
<%


  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  try {

    IssueAction action= new IssueAction(pageState);


    if ("REFRESHROW".equals(pageState.command)) {
      String issueId = pageState.getEntry("ISSUEID").stringValueNullIfEmpty();
      if (JSP.ex(issueId)) {
        Issue issue = Issue.load(issueId);
        IssueDrawer issueDrawer = new IssueDrawer(null);
        issueDrawer.dragEnabled = pageState.getEntry("DRAGENABLED").checkFieldValue();
        issueDrawer.urlToInclude = "/applications/teamwork/issue/partIssueLine.jsp";
        issueDrawer.readMode=true;
        issueDrawer.drawIssue(issue, pageContext);
      }

    } else if ("CLONE".equals(pageState.command)) {
      String issueId = pageState.getEntry("ISSUEID").stringValueNullIfEmpty();
      if (JSP.ex(issueId)) {
        Issue issue = Issue.load(issueId);

        Issue newIssue = new Issue();
        newIssue.setArea(issue.getArea());
        newIssue.setAssignedBy(issue.getAssignedBy());
        newIssue.setAssignedTo(issue.getAssignedTo());
        newIssue.setDateSignalled(new Date());
        newIssue.setGravity(issue.getGravity());
        newIssue.setImpact(issue.getImpact());
        newIssue.setShouldCloseBy(issue.getShouldCloseBy());
        newIssue.setTask(issue.getTask());
        newIssue.setTags(issue.getTags());
        newIssue.setIssueStatus(issue.getStatus());
        newIssue.setType(issue.getType());
        newIssue.setId("new_"+System.currentTimeMillis());

        if (pageState.getEntry("FULL").checkFieldValue()){
          newIssue.setDescription(issue.getDescription());
          newIssue.setEstimatedDuration(issue.getEstimatedDuration());
          newIssue.setOrderFactor(issue.getOrderFactor());
          newIssue.setOrderFactorByResource(issue.getOrderFactorByResource());

          if (IssueBricks.hasCustomField()) {
            for (int i = 1; i < 7; i++)
              ReflectionUtilities.setFieldValue("customField" + i, newIssue, ReflectionUtilities.getFieldValue("customField" + i, issue));
          }
        }


        pageState.setMainObject(newIssue);

        IssueDrawer issueDrawer = new IssueDrawer(null);
        issueDrawer.urlToInclude = "/applications/teamwork/issue/partIssueLine.jsp";
        issueDrawer.dragEnabled = pageState.getEntry("DRAGENABLED").checkFieldValue();
        issueDrawer.drawIssue(newIssue, pageContext);

      }

    } else if ("EDIT".equals(pageState.command)) {
      String issueId = pageState.getEntry("ISSUEID").stringValueNullIfEmpty();
      if (JSP.ex(issueId)) {
        Issue issue = Issue.load(issueId);
        IssueDrawer issueDrawer = new IssueDrawer(null);
        issueDrawer.urlToInclude = "/applications/teamwork/issue/partIssueLine.jsp";
        issueDrawer.dragEnabled = pageState.getEntry("DRAGENABLED").checkFieldValue();
        issueDrawer.drawIssue(issue, pageContext);
      }

    } else if ("ADD".equals(pageState.command)) {
      action.cmdAdd(false);
      Issue newIssue = (Issue) pageState.getMainObject();
      newIssue.setDateSignalled(new Date());
      newIssue.setArea(logged);
      newIssue.setStatusOpen();
      newIssue.setTask(Task.load(pageState.getEntry("ISSUE_TASK").intValueNoErrorCodeNoExc()+""));
      newIssue.setAssignedBy(Resource.load(pageState.getEntry("ASSIGNEE").intValueNoErrorCodeNoExc() + ""));
      newIssue.setId("new_" + System.currentTimeMillis());

      IssueDrawer issueDrawer = new IssueDrawer(null);
      issueDrawer.justInserted = true;
      issueDrawer.dragEnabled = pageState.getEntry("DRAGENABLED").checkFieldValue();
      issueDrawer.urlToInclude = "/applications/teamwork/issue/partIssueLine.jsp";
      issueDrawer.drawIssue(newIssue, pageContext);
    }

    
  } catch (Throwable e) {
    out.println("error=true;");
    out.println("message=\"" + JSP.javascriptEncode(e.getMessage()) + "\"");
    Tracer.platformLogger.error(e);
  }

%>