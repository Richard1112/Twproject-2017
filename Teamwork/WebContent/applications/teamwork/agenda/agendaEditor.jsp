<%@ page import="com.twproject.agenda.AgendaBricks, com.twproject.agenda.Event, com.twproject.agenda.businessLogic.AgendaController, com.twproject.resource.Person,
                 com.twproject.resource.Resource, com.twproject.resource.ResourceBricks, com.twproject.security.TeamworkPermissions, com.twproject.waf.TeamworkHBFScreen,
                 org.jblooming.ontology.SerializedList, org.jblooming.security.Permission, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.html.display.Img, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.TextField, com.twproject.meeting.Meeting, org.jblooming.waf.html.container.*, com.twproject.waf.html.MeetingDrawer, org.jblooming.waf.html.button.ButtonSupport" %><%


  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new AgendaController(), request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);

    pageState.toHtml(pageContext);
  } else {
    Event event = (Event) pageState.getMainObject();
    Person loggedPerson = Person.getLoggedPerson(pageState);

    boolean isExternal= JSP.ex(event.getIcalId())&& !event.getIcalId().startsWith("TW_");

    boolean isAuthor = true;
    if (!event.isNew() && !event.getAuthor().equals(loggedPerson))
      isAuthor = false;

    PageSeed self = pageState.thisPage(request);
    self.setMainObjectId(event.getId());
    self.setCommand("FIND_TARGET");
    SerializedList<Permission> permissions = new SerializedList();
    permissions.add(TeamworkPermissions.resource_canRead);
    self.addClientEntry("PERM_REQUIRED", permissions);

    List<Resource> resources= ResourceBricks.fillWorkGroup(pageState);
    self.addClientEntry("WG_IDS", JSP.w(pageState.getEntry("WG_IDS").stringValueNullIfEmpty()));
    self.addClientEntry("AGENDA_TYPE","");
    self.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));

    long focusMillis= pageState.getEntry("FOCUS_MILLIS").longValueNoErrorNoCatchedExc();
    focusMillis= focusMillis==0?System.currentTimeMillis():focusMillis;
    pageState.addClientEntry("FOCUS_MILLIS",focusMillis);


    Form form = new Form(self);
    form.alertOnChange = true;
    pageState.setForm(form);
    form.start(pageContext);


%>
<script>$("#AGENDA_MENU").addClass('selected');</script>

<div class="mainColumn">
  <div class="pathToObject"><%
  PageSeed ps = new PageSeed("agendaEditor.jsp");
  ps.setCommand(Commands.ADD);

  ps=AgendaBricks.getAgendaView(pageState);
  ButtonLink bl = new ButtonLink(ps);
  bl.label=I18n.get("AGENDA");
  bl.toHtmlInTextOnlyModality(pageContext);
  Meeting meeting = event.getMeeting();%>
<div class="currentNode"><h1><%=(meeting != null ? I18n.get("MEETING") : I18n.get("APPOINTMENT"))%>: <%
  if (!event.isNew()){
    %><%=event.getSummary()%><%
  } else {
    %>...<%
  }
  %></h1></div></div>

<%if (!event.isNew()){
  %> <div class="pathCodeWrapper" title="<%=I18n.get("REFERENCE_CODE")%>">
  <span class="pathCode"><%="E#"+event.getId()+"#"%>&nbsp;
  <b><%=event.getSchedule().getScheduleDescription("")%></b>

  <%if (isExternal){
    %>&nbsp;<i><%=I18n.get("EXTERNAL_MANAGED_EVENT")%></i><%
  }
  %></span></div><%
}

    if (meeting!=null)
      pageState.addClientEntry("AGE_TABSET","AGE_MEETING_TAB");
    else
      pageState.addClientEntry("AGE_TABSET","AGE_EVENT_TAB");

    TabSet tabset = new TabSet("AGE_TABSET",pageState);


    Tab eventTab = new Tab("AGE_EVENT_TAB", I18n.get("EVENT_DATA"));
    tabset.addTab(eventTab);


    Tab meetingTab = new Tab("AGE_MEETING_TAB", I18n.get("MEETING"));
    if (meeting==null)
      meetingTab.enabled=false;

    tabset.addTab(meetingTab);

    tabset.drawBar(pageContext);

    eventTab.start(pageContext);

    JspHelper ped = new JspHelper("partEventData.jsp");
    ped.parameters.put("resources",resources);
    ped.toHtml(pageContext);

    eventTab.end(pageContext);

    meetingTab.start(pageContext);
    %>
    <table border="0" cellpadding="0" cellspacing="0" class="table lreq20 lreqPage">
      <tr>
        <td id="meeting_editor">
          <%
            if (event.getMeeting() != null) {
              MeetingDrawer md = new MeetingDrawer(event.getMeeting());
              md.drawMeeting(pageContext);
            }
          %>
        </td>
      </tr>
    </table>
    <%
    meetingTab.end(pageContext);

    tabset.end(pageContext);


      ButtonBar bb = new ButtonBar();
      bb.loggableIdentifiableSupport = event;

      ButtonSubmit saveButton = ButtonSubmit.getSaveInstance(form, I18n.get("SAVE"));
      saveButton.additionalCssClass = "big first";
      saveButton.additionalOnClickScript="jsonifySchedule($('[name=startDate]'));";



      if ((isAuthor || !event.isPersonal() && pageState.getLoggedOperator().hasPermissionAsAdmin()) && !isExternal) {
        if (meeting != null) {
          ButtonJS button = new ButtonJS(I18n.get("SAVE"), "saveEventAndDIP();");
          button.additionalCssClass = "big first";
          bb.addButton(button);
        } else {
          bb.addButton(saveButton);
        }
      }

      if (isAuthor && ((!event.isNew() && meeting == null) || event.isNew())) {
        ButtonSubmit b = ButtonSubmit.getSaveInstance(pageState);
        b.variationsFromForm.setCommand("SAVE_AND_CREATE_MEETING");
        b.label = I18n.get("SET_MEETING");
        b.additionalCssClass = "big lreq20 lreqLabel";
        bb.addButton(b);
      }

      if ((isAuthor || pageState.getLoggedOperator().hasPermissionAsAdmin()) && !isExternal) {
        if (!event.isNew()) {
          DeletePreviewer deletePreviewer = new DeletePreviewer("AGE_DEL",AgendaController.class, pageState);
          ButtonSupport del = deletePreviewer.getDeleteButton(I18n.get("DELETE"), event.getId() );
          del.additionalCssClass="big delete";

          bb.addButton(del);
        }

      }

      if (!isAuthor && event.getTargets().contains(loggedPerson)) {
        PageSeed remMe = new PageSeed("agendaWeekDay.jsp");
        remMe.setMainObjectId(event.getId());
        remMe.setCommand("REMOVE_ME");
        ButtonJS remMeBut = new ButtonJS("window.location.href='" + remMe.toLinkToHref() + "'");
        remMeBut.additionalCssClass = "big delete";
        remMeBut.label = I18n.get("REMOVE_ME");
        //remMeBut.confirmRequire = true;
        bb.addButton(remMeBut);
      }


      bb.toHtml(pageContext);
      %>
  </div>


<script type="text/javascript">
  function saveEventAndDIP(){
    //set command to save

    //execute jsonifySchedule just in case...
    jsonifySchedule($("[name=startDate]"));

    var form =$("#<%=form.id%>");

    var dipRows=$("#dipRows");
    var dips=[];

    //save all tinymce open editor
    for(var i in tinymce.editors){
      tinymce.editors[i].save();
    }

    //get data from open editor
    dipRows.find(".DIPEditor").each(function(){
      var editor=$(this);
      var idDip=editor.attr("discId");
      var fromEdi={};
      fromEdi.id=idDip;
      fromEdi.orderBy=parseInt(editor.attr("orderBy"));
      editor.find(":input[name]:not(:checkbox)").each(function(){
        var inp=$(this);
        fromEdi[inp.prop("name")]=inp.val();
      });

      //doc ids
      var docIds=[];
      editor.find(".dipDocs :checkbox:checked").each(function(){
        var inp=$(this);
        docIds.push(inp.attr("docId"))
      });
      fromEdi.docIds=docIds;


      dips.push(fromEdi);
      editor.remove();// in order to avoid send inutil ce
    });

    //then check if viewer has been resorted
    dipRows.find(".DIPView").each(function(){
      var viewer=$(this);
      var idDip=viewer.attr("discId");
      var inMem=getDipFromId(idDip);
      if (viewer.attr("orderBy")!=inMem.orderBy){
        inMem.orderBy=parseInt(viewer.attr("orderBy"));
        dips.push(inMem);
      }
    });

    if (dips.length>0){
      // inject hidden field on form
      var hid=$("<input type='hidden' name='dips'>");
      hid.val(JSON.stringify(dips));
      form.append(hid);
    }
    <%=saveButton.generateJs()%>
  }

</script>
  </div>

<div class="rightColumn noprint">
  <div class="tools">
    <%
      if (!event.isNew()) {
        PageSeed print = pageState.pageFromRoot("agenda/agendaEditorPrint.jsp");
        print.mainObjectId = pageState.getMainObject().getId();
        print.setCommand(Commands.EDIT);
        ButtonJS btPrint = ButtonSubmit.getBlackInstance(pageState.getForm(),print.href);
        bl.toolTip = I18n.get("PRINT");
        btPrint.label = "";
        btPrint.iconChar = "p";
       %><div class="toolsElement"><% btPrint.toHtml(pageContext);%></div><%
      }
    %>
  </div>
</div>
<%
    form.end(pageContext);
  }
%>
