<%@page pageEncoding="UTF-8" %><%@ page import="com.opnlb.website.page.WebSitePage,
                 com.opnlb.website.template.Template,
                 com.opnlb.website.util.TemplateManager,
                 com.opnlb.website.waf.WebSiteConstants,
                 com.twproject.operator.TeamworkOperator,
                 com.twproject.resource.Company,
                 com.twproject.resource.Person,
                 com.twproject.security.TeamworkPermissions, com.twproject.task.IssueBricks, org.jblooming.persistence.PersistenceHome, org.jblooming.utilities.DateUtilities, org.jblooming.waf.ScreenBasic, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.DivOnMouseover, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.Date"%>
<%

  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  String pageName = request.getAttribute("pageName") + "";
  String pageId = TemplateManager.getIdFromPageName(pageName);

  WebSitePage wsp = (WebSitePage) PersistenceHome.findUniqueNullIfEmpty(WebSitePage.class, "id", pageId);

  if ("SET_AS_MY_HOME".equals(pageState.command)) {
    TeamworkOperator loggedOnDb = (TeamworkOperator) pageState.getLoggedOperator();
    loggedOnDb.putOption(WebSiteConstants.HOME_PAGE, wsp.getName() + ".page");
  }

  Template template = null;
  if (wsp != null) {
    template = wsp.getDefaultTemplate();
  }

  if (!pageState.screenRunning) {
    ScreenBasic.preparePage(pageContext);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);


  } else {
    if (wsp == null ) {
%>Page "<%=pageName%>" is not currently available. Contact system administrator.<%
} else if (template == null) {
%>Template has not been set on page "<%=pageName%>". Contact system administrator.<%

} else {
%>
<style type="text/css">

  #twInnerContainer{
    border-bottom: 1px solid #f6f6f6;
  }

  h1 .teamworkIcon {
    font-size: 100%;
  }

  .dashboardChooserArrow .button.textual.icon{
    width: 40px !important;
    text-align: center;
    height: 25px;
  }

</style>

<script>
  $("#HOME_MENU").addClass('selected');
  $("body").addClass("home");

  /*
    $(function(){
      $(".rightColumn").fadeIn(500);
    });
  */

</script>

<div class="topHome">
  <div class="dashboardHeader">

    <h1 style="display: inline-block"><%=wsp.getFrontOfficeTitle()%></h1>
    <div class="dashboardLinksBox">
      <h2 style="display: inline-block; padding-right: 30px;font-weight:400"><%=DateUtilities.dateToFullString(new Date())%></h2>
        <%
          ButtonJS opener ;

          boolean canCreateProject = logged.hasPermissionFor(TeamworkPermissions.project_canCreate);
          boolean canCreateResource = logged.hasPermissionFor(TeamworkPermissions.resource_canCreate);
          boolean canCreateIssue = true;//logged.hasPermissionFor(TeamworkPermissions.issue_canCreate); //puoi sempre creare una issue, tanto se non hai un task su cui metterla non salva
          if (canCreateProject||canCreateResource||canCreateIssue) {

            opener = new ButtonJS("");
            opener.iconChar = "P";
            opener.additionalCssClass = "first";
            opener.label = I18n.get("ADD")+"...";

            DivOnMouseover add = new DivOnMouseover(opener);

            //add task
            if (canCreateProject){
              PageSeed newTask = new PageSeed(request.getContextPath() + "/applications/teamwork/task/taskNew.jsp");
              newTask.addClientEntry("ADD_TYPE", "ADD_TASK");
              newTask.setCommand(Commands.ADD);
              ButtonSupport bl = ButtonLink.getBlackInstance("+ " + I18n.get("ADD_TASK"), 720, 800, newTask);
              add.addButton(bl);
            }

            //issues
            if (canCreateIssue) {
              ButtonJS addIssue = IssueBricks.getBlackEditor(0, Commands.ADD, "&ISSUE_TASK=");
              addIssue.label = "+ " + I18n.get("ADD_ISSUE");
              add.addButton(addIssue);
            }

            // add resources
            if (canCreateResource) {
              PageSeed newPerson = pageState.pageFromRoot("resource/resourceEditor.jsp");
              newPerson.setCommand(Commands.ADD);
              newPerson.addClientEntry("RESOURCE_TYPE", Person.class.getName());
              ButtonLink bl = new ButtonLink(newPerson);
              bl.label = "+ " + I18n.get("ADD_PERSON");
              add.addButton(bl);

              PageSeed newCompany = new PageSeed(request.getContextPath() + "/applications/teamwork/resource/resourceEditor.jsp");
              newCompany.setCommand(Commands.ADD);
              newCompany.addClientEntry("RESOURCE_TYPE", Company.class.getName());
              bl = new ButtonLink("+ " + I18n.get("ADD_COMPANY"), newCompany);
              add.addButton(bl);
            }

       add.toHtml(pageContext);
        }
      %>
    </div>

  </div>
</div>

<%

      JspHelper helper = new JspHelper("/commons/layout/partTemplateDrawer.jsp");
      helper.parameters.put("template", template );
      helper.parameters.put("ws_page", wsp);
      helper.toHtml(pageContext);


/*
      pageState.addClientEntry("WS_PAGEID", pageId);
      template.urlToInclude = "/" + template.getJspFileLocation();
      template.toHtml(pageContext);
*/

    }
  }
%>
