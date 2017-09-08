<%@ page import="com.twproject.operator.TeamworkOperator,
                 org.jblooming.security.License,
                 com.twproject.waf.TeamworkPopUpScreen,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.container.ButtonBar,
                 org.jblooming.waf.html.input.TextArea,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.ApplicationState,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState" %>
<%

  if (ApplicationState.isHostMode && !request.getRemoteAddr().startsWith("82.119.201.30")  && !request.getRemoteAddr().startsWith("88.149.176.89"))
    return;

  PageState pageState = PageState.getCurrentPageState(request);

  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
  logged.testIsAdministrator();

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    //put controller !
    final ScreenArea body = new ScreenArea(request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);

    if (Commands.SAVE.equals(pageState.command)) {

      String licOrig = pageState.getEntry("PASTE_HERE_LICENSE").stringValue();

      License license = License.fromString(licOrig);

      if (license.loadOk) {
        try {
          license.storeLicense();
          pageState.setCommand("LICENSE_SAVED");
          ApplicationState.applicationParameters.remove("LIC_EXPIRES");
          ApplicationState.applicationParameters.remove("FILTER_ERROR");
          pageState.initializeEntries("div");

          %><script>top.location.href="administrationIntro.jsp"</script><%

        } catch (Exception e) {
          pageState.getEntry("PASTE_HERE_LICENSE").errorCode = "Impossible to register license.";
        }
      } else {
        pageState.getEntry("PASTE_HERE_LICENSE").errorCode = "Impossible to register license.Check if the license contains all the need information";
      }
    } else {
      /*try {
        pageState.addClientEntry("PASTE_HERE_LICENSE", License.getLicense().getLicenseWrapped());
      } catch (Exception e) {
        pageState.addClientEntry("PASTE_HERE_LICENSE", "No license found.");
      }*/
    }

    pageState.toHtml(pageContext);

  } else {
    PageSeed self = pageState.thisPage(request);
    self.setCommand(Commands.SAVE);
    Form f = new Form(self);
    f.start(pageContext);


%><h1><%=I18n.get("TWPROJECT_LICENSE")%></h1><%

  TextArea lic = new TextArea("PASTE_HERE_LICENSE", "<br>", 64, 10, null);
  lic.script = "style='font-family:monospace;font-size:13px;'";
  lic.required = true;
  lic.disabled = "LICENSE_SAVED".equals(pageState.command);
  lic.maxlength = 1000;
  lic.label = "";
  lic.toHtmlI18n(pageContext);

  ButtonBar bb = new ButtonBar();
  ButtonSubmit saveInstance = ButtonSubmit.getSaveInstance(f, I18n.get("REGISTER_LICENSE"));
  saveInstance.additionalCssClass = "first";
  saveInstance.enabled = !"LICENSE_SAVED".equals(pageState.command);
  bb.addButton(saveInstance);
  bb.toHtml(pageContext);

%>
<big><%
  if ("LICENSE_SAVED".equals(pageState.command)) {
%><%=I18n.get("LICENSE_REGISTERED")%><%
} else {
%><%=I18n.get("LICENSE_WILL_BE_OVERWRITTEN_WARN")%><%
  }
%></big>
<%


    f.end(pageContext);
  }
%>
