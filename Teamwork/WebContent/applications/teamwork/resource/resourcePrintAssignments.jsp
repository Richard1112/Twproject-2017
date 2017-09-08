<%@ page import="com.twproject.resource.Resource, com.twproject.resource.businessLogic.ResourceController, com.twproject.task.Assignment, com.twproject.waf.TeamworkPopUpScreen, org.jblooming.agenda.Period, org.jblooming.system.SystemConstants, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonImg, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.container.DivOnMouseover, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.Img, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.Date, java.util.List, org.jblooming.waf.settings.I18n, java.util.Collections, com.twproject.utilities.TeamworkComparators" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new ResourceController(), request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);
  } else {
    PageSeed self = pageState.thisPage(request);
    self.setCommand(Commands.EDIT);
    self.mainObjectId = pageState.mainObjectId;
    Form f = new Form(self);
    pageState.setForm(f);
    f.start(pageContext);

    Resource resource = Resource.load(pageState.getMainObjectId());
    List<Assignment> assigs = resource.getActiveAssignments(Period.getDayPeriodInstance(new Date()),false,true,false);
    Collections.sort(assigs, new TeamworkComparators.AssignmentByPriority(new Date()));

    Img log = new Img(ApplicationState.getApplicationSetting(SystemConstants.PRINT_LOGO), "");
%>
<table border="0" width="99%" align="center" cellpadding="3" cellspacing="0">
          <tr>
           <td align="left" width="90%"><h3><%log.toHtml(pageContext);%> <%=pageState.getI18n("RESOURCE")%></h3></td>
            <td align="right"><%

              ButtonJS print = new ButtonJS("window.print();");
              print.label = "";
              print.toolTip = I18n.get("PRINT_PAGE");
              print.iconChar = "p";
              print.toHtmlInTextOnlyModality(pageContext);

            %></td>
          </tr>
      </table>
<%
    JspHelper helper = new JspHelper("/applications/teamwork/resource/part/partResourceEditorPrint.jsp");
    helper.parameters.put("resource", resource);
    helper.toHtml(pageContext);
%><%

  helper = new JspHelper("/applications/teamwork/resource/part/partPrintAssignments.jsp");
  helper.parameters.put("resource", resource);
  helper.toHtml(pageContext);

  f.end(pageContext);
  }
%>
