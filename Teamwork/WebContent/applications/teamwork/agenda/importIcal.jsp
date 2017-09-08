<%@ page import="com.twproject.agenda.Event,
                 com.twproject.agenda.IcalUtilities,
                 com.twproject.operator.TeamworkOperator,
                 com.twproject.resource.Person,
                 com.twproject.waf.TeamworkHBFScreen,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.container.ButtonBar,
                 org.jblooming.waf.html.input.Uploader,
                 org.jblooming.waf.html.input.Uploader.UploadHelper,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState,
                 java.io.File,
                 java.io.FileInputStream,
                 java.util.List"
        %><script>$("#AGENDA_MENU").addClass('selected');</script>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
  Person loggedPerson = logged.getPerson();


  if (!pageState.screenRunning) {
    pageState.screenRunning = true;

    final ScreenArea body = new ScreenArea( request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);

    //action
    if (Commands.SAVE.equals(pageState.command)) {
      UploadHelper helper = Uploader.getHelper("ICALFILE", pageState);
      if (helper != null) {
        File temporaryFile = helper.temporaryFile;
        if (temporaryFile != null && temporaryFile.exists()) {
          FileInputStream inputStream = new FileInputStream(temporaryFile);
          List<Event> events = IcalUtilities.getEventsFromStream(loggedPerson, false, inputStream);
          pageState.attributes.put("events",events);
          inputStream.close();
        }
      }
      pageState.removeEntry("ICALFILE");
    }


    pageState.toHtml(pageContext);
  } else {

    PageSeed self = pageState.thisPage(request);
    
    List<Event> events = (List<Event>) pageState.attributes.get("events");

    self.command = Commands.FIND;
    Form f = new Form(self);
    pageState.setForm(f);
    f.alertOnChange = false;
    f.encType = Form.MULTIPART_FORM_DATA;
    f.start(pageContext);


%><h1><%=I18n.get("INTRO_IMPORT_CAL")%></h1>
<hr>
  <table border="0" class="table"><tr><td><%
    Uploader upl = new Uploader("ICALFILE", pageState);
    upl.separator = "<br>";
    upl.label = I18n.get("ICAL_FILE_TO_IMPORT");
    upl.size=30;

    upl.toHtml(pageContext);

%><br><%

    ButtonBar bb = new ButtonBar();
    ButtonSubmit sub = new ButtonSubmit(f);
    sub.variationsFromForm.command = Commands.SAVE;
    sub.label = I18n.get("PROCEED");
    sub.additionalCssClass = "big first";
    bb.addButton(sub);

    bb.toHtml(pageContext);


%></td></tr><%

  if (JSP.ex(events)){
    %><tr><th colspan="6"class="tableHead"><%=I18n.get("EVENTS_IMPORTED")+": "+events.size()%></th></tr><tr> <td colspan="6"><%
    for (Event event : events) {
      %><%=event.getAbstractForIndexing()%><hr><%
    }
    %></td> </tr><%
}
    %></td></tr></table><%
    f.end(pageContext);
  }
%>