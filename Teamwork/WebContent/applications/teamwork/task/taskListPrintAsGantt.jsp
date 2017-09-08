<%@ page import="com.twproject.task.businessLogic.TaskController, com.twproject.waf.TeamworkPopUpScreen, org.jblooming.system.SystemConstants, org.jblooming.waf.ScreenArea, org.jblooming.waf.SessionState, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.Img, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jblooming.waf.settings.I18n" %><%
  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new TaskController(), request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);

  } else {

    PageSeed self = pageState.thisPage(request);
    self.setCommand(Commands.EDIT);
    self.mainObjectId = pageState.mainObjectId;
    Form f = new Form(self);
    pageState.setForm(f);
    f.start(pageContext);

    Img logo = new Img(ApplicationState.getApplicationSetting(SystemConstants.PRINT_LOGO), "");

%><table border="0" width="99%" align="center" cellpadding="3" cellspacing="0" class="noprint">
  <tr>
    <td align="left" width="70%"><%logo.toHtml(pageContext);%></td>
        <td align="left">
      <a href="javascript:$('#divOnMouseoverImg_id').fadeOut();$('#divOnMouseover_id').fadeOut();window.print();window.close()">
        <%
        Img print = new Img("printIcon.png", pageState.getI18n("PRINT_PAGE"), "20", "20");
        print.toHtml(pageContext);
        %> <%=I18n.get("PRINT_PAGE")%></a>
        <small><%=I18n.get("GANTT_PRINT_PAGE_HELP")%></small>
    </td>
  </tr>
</table>
<table border="0" id="GANNT_LIST_PLACE" cellpadding="0"  class="table"><tr><td align="center"><%
      JspHelper dcheck = new JspHelper("/applications/teamwork/task/partTaskListAsGantt.jsp");
      dcheck.parameters.put("page", pageState.getPage());
      dcheck.toHtml(pageContext);
    %></td></tr></table><%

  f.end(pageContext);

  }
%>
