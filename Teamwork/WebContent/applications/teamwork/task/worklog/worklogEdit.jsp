<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.waf.TeamworkPopUpScreen, com.twproject.worklog.Worklog, org.jblooming.persistence.PersistenceHome, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.input.DateField, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jblooming.waf.settings.I18n, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.core.JspHelper, org.jblooming.designer.DesignerField, com.twproject.worklog.businessLogic.WorklogAction"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
   pageState.toHtml(pageContext);

  } else {

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    Worklog w = (Worklog) PersistenceHome.findByPrimaryKey(Worklog.class,pageState.mainObjectId);

    if (Commands.SAVE.equals(pageState.command) &&  w.bricks.canWrite(logged)) {

//      w.setDuration(pageState.getEntry("ISSUE_WORKLOG_TIME").durationInWorkingMillis(true));
//      w.setAction(pageState.getEntry("WORKLOG_ACTION").stringValue());
//      w.setInserted(pageState.getEntry("WORKLOG_AT_DAY").dateValue());
//
//      DesignerField.saveCustomFields("WORKLOG_CUSTOM_FIELD_", 4, w, pageState);
//      //ActionUtilities.setIdentifiable(pageState.getEntry("ISSUE"),w,"issue");
//      w.store();

      Worklog worklog = new WorklogAction(pageState).cmdSave();

    } else {
      pageState.addClientEntryTime("WORKLOG_DURATION",w.getDuration());
      pageState.addClientEntry("WORKLOG_ACTION",w.getAction());
      pageState.addClientEntry("WORKLOG_INSERTIONDATE",w.getInserted());
      pageState.addClientEntry("ISSUE",w.getIssue());
    }

    PageSeed self = pageState.thisPage(request);
    self.mainObjectId = w.getId();
    self.setCommand(Commands.SAVE);

    Form f = new Form(self);
    f.start(pageContext);
    pageState.setForm(f);



  PageSeed edTask = pageState.pageFromRoot("task/taskAssignmentEditor.jsp");
  edTask.command = Commands.EDIT;
  edTask.addClientEntry("TASK_ID", w.getAssig().getTask().getId());
  edTask.mainObjectId = w.getAssig().getId();
  ButtonLink editLink = ButtonLink.getTextualInstance(I18n.get("EDIT_ASSIGNMENT"), edTask);
  editLink.label = w.getAssig().getDisplayName();

  TextField dur = new TextField("WORKLOG_DURATION","</td><td>");
    dur.label = "WORKLOG";
    dur.separator = "<br>";
  dur.fieldSize=6;


  DateField df = new DateField("WORKLOG_INSERTIONDATE");
    df.labelstr = "WORKLOG_INSERTIONDATE";
    df.separator = "<br>";
    df.size = 10;


TextField.hiddenInstanceToHtml("wlId", w.getId()+"",pageContext);

%>
<h2><%=I18n.get("WORKLOG_EDIT")%></h2>
<table class="table">
  <tr>
    <td><%=I18n.get("ASSIGNMENT")%></td>
    <td><%editLink.toHtmlInTextOnlyModality(pageContext);%></td>
  </tr>
  <%
    if(w.getIssue()!=null){
  %>
  <tr>
    <td><label><%=I18n.get("ISSUE")%></label></td>
    <td><%=w.getIssue() != null ? JSP.encode(w.getIssue().getDescription()) : ""%></td>
  </tr>
  <%
    }
  %>
  <tr><td>&nbsp;</td></tr>
  <tr>
    <td><%dur.toHtmlI18n(pageContext);%></td>
    <td><%df.toHtmlI18n(pageContext);%></td>
  </tr>
  <tr><td colspan="2">
    <%

      // questa e il path local senza contextpath a questa pagina che può essere sovrascritto con una custom label
      String descriptionFormUrl = "/applications/teamwork/task/worklog/worklogDescriptionForm.jsp";
      if (I18n.isActive("CUSTOM_FEATURE_WORKLOG_FORM")) {
        descriptionFormUrl = I18n.get("CUSTOM_FEATURE_WORKLOG_FORM");
      }

      JspHelper wlDF = new JspHelper(descriptionFormUrl);
      if (w.getAssig() != null)
        wlDF.parameters.put("assig", w.getAssig());
      if (w != null)
        wlDF.parameters.put("worklog", w);
      wlDF.toHtml(pageContext);

    %></td></tr>
</table>
<%

    ButtonBar bb = new ButtonBar();
    ButtonSubmit saveInstance = ButtonSubmit.getSaveInstance(f, I18n.get("SAVE"));
    boolean canWrite = w.bricks.canWrite(logged);
    saveInstance.enabled = canWrite;

    bb.addButton(saveInstance);

    bb.toHtml(pageContext);

    f.end(pageContext);

  }
  %>


