<%@ page import="org.jblooming.waf.view.PageSeed, com.twproject.security.TeamworkPermissions, com.twproject.operator.TeamworkOperator, org.jblooming.waf.view.PageState, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.settings.I18n, org.jblooming.waf.constants.Fields, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.html.button.ButtonSupport" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  if (logged.hasPermissionFor(TeamworkPermissions.project_canCreate)) {

%><br>
<div id="no_task_add_box" class="footerWarningWrapper" style="display: none;">

  <div class="footerWarning" style="width: 33%">

          <div><span class="teamworkIcon" style="font-size:40px">t</span></div>
          <div><%
            PageSeed newTask = new PageSeed(request.getContextPath() + "/applications/teamwork/task/taskNew.jsp");
            newTask.addClientEntry("ADD_TYPE", "ADD_TASK");
            newTask.addClientEntry("TASK_NAME",pageState.getEntry("NAME_DESCRIPTION").stringValueNullIfEmpty());

            newTask.setCommand(Commands.ADD);
            ButtonSupport bl = ButtonLink.getBlackInstance( I18n.get("ADD_TASK"), 720, 800, newTask);
            bl.iconChar="P";
            bl.toHtml(pageContext);
          %></div>
          <p><%=I18n.get("ADD_TASK_HELP")%></p>

        </div>

        <div class="footerWarning" style="width: 33%">
          <div><span class="teamworkIcon" style="font-size:50px">&pound;</span></div>
          <div><%
            if (!Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("DISABLEWORKFLOW"))){
              PageSeed newProcess = pageState.pageFromRoot("processes/createProcess.jsp");
              newProcess.setCommand(Commands.ADD);
              bl = new ButtonLink(I18n.get("ADD_TASK_PROCESS"), newProcess);
              bl.iconChar="P";
              bl.additionalCssClass="lreq30 lreqLabel";
              bl.toHtml(pageContext);
            }
          %></div>
          <p><%=I18n.get("ADD_TASK_PROCESS_HELP")%></p>
        </div>


        <div class="footerWarning" style="width: 33%">

          <div><span class="teamworkIcon" style="font-size:50px">K</span></div>
          <div><%
            ButtonLink blScrum = new ButtonLink(I18n.get("SCRUM_PROJECT_CREATION"),pageState.pageFromRoot("task/scrum/projectWizard.jsp"));
            blScrum.iconChar="P";
            blScrum.toHtml(pageContext);
          %></div>
          <p><%=I18n.get("ADD_SCRUM_HELP")%> <a href="http://en.wikipedia.org/wiki/Scrum_%28development%29" target="_blank">Scrum Wikipedia</a></p>

        </div>

</div><%
  }

%>
