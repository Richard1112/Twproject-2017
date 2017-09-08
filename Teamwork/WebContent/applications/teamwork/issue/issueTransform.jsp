<%@ page import="com.twproject.task.Issue,
                 com.twproject.task.Task,
                 com.twproject.task.TaskBricks,
                 com.twproject.task.businessLogic.IssueController,
                 com.twproject.waf.TeamworkPopUpScreen,
                 org.jblooming.persistence.PersistenceHome,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.SessionState,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.button.ButtonSupport,
                 org.jblooming.waf.html.container.ButtonBar,
                 org.jblooming.waf.html.container.Container,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new IssueController(), request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);
  } else {

/*
________________________________________________________________________________________________________________________________________________________________________


  boolean and objects for whole logic

________________________________________________________________________________________________________________________________________________________________________

*/

  if ("TRANSFORM".equals(pageState.getCommand())) {

     Task task = (Task) pageState.getMainObject();

%><h1><%=task.getDisplayName()%></h1><%

    String issuesId = pageState.getEntry("ISSUE_ID").stringValueNullIfEmpty();

    if(JSP.ex(issuesId)){
    Issue issue = Issue.load(issuesId);

%>
    <script type="text/javascript">
      getBlackPopupOpener().$("body").trigger("issueEvent",[{type:"save",response:{issue:<%=issue.jsonify()%>}}]);
    </script>
    <h2><%=I18n.get("EXPLAIN_ISSUE_UPGRADED",task.getDisplayName(),task.getParent().getDisplayName())%></h2>
    <div class="separator"></div><%
      ButtonSupport support = TaskBricks.getButtonForEntity(task, pageState);
      support.iconChar="";
      support.label = I18n.get("GO_TO_TASK_PAGE");
      support.additionalCssClass = "big first";
      support.toHtml(pageContext);


      }

  } else {

        Issue issue = (Issue) pageState.getMainObject();
          boolean isNew = PersistenceHome.NEW_EMPTY_ID.equals(issue.getId());

          Task task = issue.getTask();



/*
________________________________________________________________________________________________________________________________________________________________________


  begin draw

________________________________________________________________________________________________________________________________________________________________________

*/
  PageSeed self = pageState.thisPage(request);
  self.mainObjectId = issue.getId();
  self.setCommand(Commands.EDIT);
  self.setPopup(true);
  Form f = new Form(self);
  f.encType = Form.MULTIPART_FORM_DATA;
  pageState.setForm(f);
  f.start(pageContext);







        //path to object
%>

<table cellpadding="2" cellspacing="0">
  <tr class="containerTitle" style="font-size:16px">
  <td>id:&nbsp;<%=JSP.w(issue.getId())%></td><td nowrap>&nbsp;&nbsp;&nbsp;
  <%

    PageSeed ps = pageState.pageFromRoot("issue/issueEditor.jsp");
    ps.mainObjectId = issue.getId();
    ps.setCommand(Commands.EDIT);
    ButtonLink prevL = new ButtonLink("", ps);

    prevL.iconChar="N";
    prevL.label=I18n.get("BACK");

%><%prevL.toHtmlInTextOnlyModality(pageContext);%>

</tr></table>
<%
/*
________________________________________________________________________________________________________________________________________________________________________


  main draw

________________________________________________________________________________________________________________________________________________________________________

*/
  Container upgradeC = new Container();
  upgradeC.level = 2;

  upgradeC.start(pageContext);

      %><br><h2><%
      %><%=I18n.get("EXPLAIN_ISSUE_UPGRADE",issue.getTask().getDisplayName())%><br><%

      if (issue.getAssignedTo()!=null) {
        %><%=I18n.get("EXPLAIN_ISSUE_LOSS_PERSON",issue.getAssignedTo().getDisplayName())%><br><%
      }
      %></h2><%



/*
________________________________________________________________________________________________________________________________________________________________________


    END

________________________________________________________________________________________________________________________________________________________________________

*/
  ButtonBar bb2 = new ButtonBar();
  if (!isNew) {
    bb2.loggableIdentifiableSupport = issue;
  }

      ButtonSubmit save = ButtonSubmit.getSaveInstance(f, I18n.get("UPGRADE_TO_TASK"));
      save.variationsFromForm.setCommand("TRANSFORM");
      save.additionalCssClass="first big";
      bb2.addButton(save);


  bb2.toHtml(pageContext);

      upgradeC.end(pageContext);


   f.end(pageContext);
    }
  }

%>
