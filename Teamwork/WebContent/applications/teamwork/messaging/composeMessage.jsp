<%@ page import="com.twproject.operator.TeamworkOperator,
                 com.twproject.resource.Person,
                 com.twproject.resource.ResourceBricks,
                 com.twproject.security.TeamworkPermissions,
                 com.twproject.task.Task,
                 com.twproject.task.TaskBricks,
                 com.twproject.waf.TeamworkHBFScreen,
                 org.jblooming.messaging.MailHelper,
                 org.jblooming.messaging.Message,
                 org.jblooming.messaging.MessagingSystem,
                 org.jblooming.ontology.IdentifiableSupport,
                 org.jblooming.ontology.SerializedList,
                 org.jblooming.persistence.PersistenceHome,
                 org.jblooming.security.Permission,
                 org.jblooming.system.SystemConstants,
                 org.jblooming.tracer.Tracer,
                 org.jblooming.utilities.JSP,
                 org.jblooming.utilities.StringUtilities,
                 org.jblooming.waf.Bricks,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields,
                 org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.container.Container,org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.SmartCombo, org.jblooming.waf.html.input.TextArea,
org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, java.util.Date, java.util.List, java.util.Map, com.twproject.waf.TeamworkPopUpScreen, org.jblooming.agenda.CompanyCalendar, org.jblooming.waf.view.*, com.twproject.document.TeamworkDocument, com.twproject.document.businessLogic.DocumentAction"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);

  } else {
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    String feedback = "";

    //get the eventual referral
    IdentifiableSupport referral = Bricks.getReferral(pageState);
    boolean referralIsTask=referral != null && referral instanceof Task;



      if ("SEND".equals(pageState.getCommand())) {
      String subject = pageState.getEntry("MESSAGE_SUBJECT").stringValue();
      String body = JSP.convertLineFeedToBR(pageState.getEntry("MESSAGE").stringValue());
      boolean sentSomething = false;

      String sendToIds = pageState.getEntry("WG_IDS").stringValueNullIfEmpty();
      String sendTo = pageState.getEntry("WG_NAMES").stringValue();

        if (JSP.ex(sendToIds)) {

        String taskID = pageState.getEntry("TASK_TO_LINK").stringValueNullIfEmpty();
        if (taskID != null) {
          Task task = (Task) PersistenceHome.findByPrimaryKey(Task.class, taskID);
          PageSeed ps = new PageSeed(ApplicationState.serverURL + "/applications/teamwork/task/taskOverview.jsp");
          ps.setCommand(Commands.EDIT);
          ps.setMainObjectId(task.getId());
          ButtonLink edit = new ButtonLink(ps);
          edit.label = task.getDisplayName();
          body = body + "<hr>link:&nbsp;" + edit.toPlainLink();
        }

        Map<String, ClientEntry> ces = pageState.getClientEntries().getEntriesStartingWithStripped("MESSAGE_MEDIA_");

        feedback = I18n.get("CHOOSE_AT_LEAST_ONE_MEDIA");


        for (String key : ces.keySet()) {
          if (ces.get(key).checkFieldValue()) {
            sentSomething = true;
            List<String> strings = StringUtilities.splitToList(sendToIds, ",");
            for (String to : strings) {
              to = to.trim();
              Person p = (Person) PersistenceHome.findByPrimaryKey(Person.class, to);
              if (p.getMyself() != null) {
                Message message = new Message();
                message.setFromOperator(logged);
                message.setToOperator(p.getMyself());
                message.setDefaultExpires();
                message.setMedia(key);
                message.setSubject(subject);
                message.setMessageBody(body);
                message.setReceived(new Date());
                message.store();
              }
            }
          }
        }

        //devo archiviare il messaggio come documento sul task?
        if (referralIsTask && ((Task)referral).hasPermissionFor(logged,TeamworkPermissions.document_canCreate) && pageState.getEntry("ARCHIVE_ON_TASK_DOCUMENT").checkFieldValue()){
          Task task = (Task) referral;
          RestState rs= new RestState(logged);
          rs.setMainObjectId(PersistenceHome.NEW_EMPTY_ID);
          rs.addClientEntry("TASK_ID", task.getId());
          rs.addClientEntry("DOCUMENT_NAME", subject);
          rs.addClientEntry("DOCUMENT_AUTHORED", new Date());
          rs.addClientEntry("DOCUMENT_AUTHOR", logged.getDisplayName());
          rs.addClientEntry("DOCUMENT_AREA", task.getArea());
          rs.addClientEntry("DOCUMENT_TYPE", TeamworkDocument.IS_CONTENT);
          rs.addClientEntry("SUMMA", body+ "<br><small>"+I18n.get("MESSAGE_SENT_TO")+": "+sendTo+"</small>");
          new DocumentAction(rs).cmdSave();

        }

        if (sentSomething) {
          feedback = I18n.get("MESSAGE_SENT");
          %><script>closeBlackPopup();</script><%
        }
      }

    } else {

      if ("FIND_BY_ENTITY".equals(pageState.getCommand())) {
        ResourceBricks.fillWorkGroup(pageState);
      }

    }




    //prepare pageSeed for form
    PageSeed self = pageState.thisPage(request);
    self.setCommand(Commands.FIND);

    SerializedList<Permission> permissions = new SerializedList();
    permissions.add(TeamworkPermissions.resource_canRead);
    //permissions.add(TeamworkPermissions.assignment_manage);
    self.addClientEntry("PERM_REQUIRED", permissions);

    boolean launchSelector = false;
    if (!JSP.ex(pageState.getEntry("WG_IDS"))){
      launchSelector=true;
      ResourceBricks.fillWorkGroup(pageState);  // in questo modo mette se stesso
    }

    if (referral != null) {
      Bricks.addReferral(referral.getId(), referral.getClass(), self);
      if (referralIsTask && ((Task) referral).hasPermissionFor(logged,TeamworkPermissions.task_canRead)) {
        pageState.addClientEntry("TASK_TO_LINK", referral.getId());
      }
    }

    Form f = new Form(self);
    f.alertOnChange = true;
    pageState.setForm(f);

    f.start(pageContext);



    String title = "";
    if (referral != null)
      title += I18n.get("WORK_GROUP")+":&nbsp;\""+referral.getName()+"\"";

%><script>$("#TOOLS_MENU").addClass('selected');</script>

<h1><%=I18n.get("MESSAGE_COMPOSE")%></h1>
<h4><%=title%></h4>

<table class="table" cellpadding="2" cellspacing="0"><%

  pageState.addClientEntry("MESSAGE_FROM",logged.getDisplayName());
  TextField from = new TextField("MESSAGE_FROM", "<br>");
  from.required = true;
  from.fieldSize = 30;
  from.readOnly=true;
%> <tr><td valign="top"><%from.toHtmlI18n(pageContext);%></td><%

  ButtonSupport wp = ButtonSubmit.getSubmitInstanceInBlack(pageState.getForm(), request.getContextPath() + "/applications/teamwork/workgroup/workgroupPopup.jsp", 500, 580);
  wp.label = "";
  wp.iconChar="r";
  wp.additionalCssClass = "addWorkgroup";

  TextField.hiddenInstanceToHtml("WG_IDS",pageContext);

  TextField to = new TextField("WG_NAMES", "<br>");
  to.label=I18n.get("MESSAGE_TO");
  to.required = true;
  to.fieldSize = 50;
  to.readOnly=true;
  to.script = "onclick=\"" + wp.generateJs().toString() + "\"";



  TextArea message = new TextArea("MESSAGE", "<br>", 120, 6, "");
  message.maxlength=4000;
  message.script="style=width:100%";
  ButtonJS insert = new ButtonJS();
  insert.onClickScript = "copyNamesOnText()";
  insert.label = I18n.get("INSERT_TARGETS_IN_BODY");


  %></tr><tr>
  <td valign="top"><%to.toHtml(pageContext);%> &nbsp;<%wp.toHtml(pageContext);%> &nbsp;<small><%insert.toHtmlInTextOnlyModality(pageContext);%></small></td></tr>
    <tr>
      <td colspan="2"><label><%=I18n.get("MESSAGE_MEDIA")%>*</label> <%

        for(MessagingSystem.Media media : MessagingSystem.activeMedia) {


          pageState.getEntryOrDefault("MESSAGE_MEDIA_" + media.toString().toUpperCase());
          String subscrField = "MESSAGE_MEDIA_" + media.toString().toUpperCase();
          CheckField cf = new CheckField(subscrField, "", false);
          cf.label = I18n.get(media.toString());
          cf.toHtml(pageContext);
          %>&nbsp;&nbsp;&nbsp;&nbsp;<%
        }

        if ("SEND".equals(pageState.getCommand())) {
          pageState.addMessageInfo(feedback);
        }

  %></td></tr></table>

  <table class="table" cellpadding="2" cellspacing="0"> <%

  TextField subject = new TextField("MESSAGE_SUBJECT", "<br>");
  subject.script="style=width:100%";
  %> <tr><td><%subject.toHtmlI18n(pageContext);%></td></tr> <%

  message.label = I18n.get("MESSAGE_BODY");
  message.required = true;
  message.script="style=width:100%";
  %> <tr><td colspan="2"><%message.toHtml(pageContext);%></td></tr> <%

    if (referralIsTask){
      SmartCombo taskToLink = TaskBricks.getTaskCombo("TASK_TO_LINK",false,TeamworkPermissions.task_canRead,pageState);
      taskToLink.separator="<br>";
      taskToLink.fieldSize =60;

      pageState.getEntryOrDefault("ARCHIVE_ON_TASK_DOCUMENT","yes");
      CheckField generateDoc= new CheckField("ARCHIVE_ON_TASK_DOCUMENT","&nbsp;",true);

      %> <tr><td ><%taskToLink.toHtmlI18n(pageContext);%> &nbsp;&nbsp; <%
        // se siamo su un task e si può creare un doc si mette il check
        if (referralIsTask && ((Task)referral).hasPermissionFor(logged,TeamworkPermissions.document_canCreate)){
          generateDoc.toHtmlI18n(pageContext);
        }%>
      </td></tr> <%
    }

  %></table> <%

  ButtonBar bb = new ButtonBar();

  ButtonSubmit send = new ButtonSubmit(f);
  send.label = I18n.get("SEND");
  send.variationsFromForm.setCommand("SEND");
  send.additionalCssClass="first";
  send.alertOnRequired = true;
  send.alertOnChange = false;
  bb.addButton(send);

  bb.toHtml(pageContext);

  f.end(pageContext);

  if (launchSelector) {
    %><script type="text/javascript" defer="yes"><%=((ButtonJS)wp).onClickScript%></script><%
  }

  %>
<script>
  function copyNamesOnText (){
    $("#MESSAGE").val("<%=I18n.get("PEOPLE_INVOLVED")%>:\n"+$("#WG_NAMES").val());
  }


</script>


<%


  }
%>
