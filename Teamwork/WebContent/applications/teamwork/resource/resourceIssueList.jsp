<%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8" %>
<%@ page import="com.twproject.resource.Resource,
                 com.twproject.task.IssueBricks,
                 com.twproject.waf.TeamworkHBFScreen,
                 com.twproject.waf.html.ResourceHeaderBar,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.constants.Fields,
                 org.jblooming.waf.html.core.JspHelper,
                 org.jblooming.waf.html.display.Img,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState, com.twproject.resource.businessLogic.ResourceController" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);
  // su resourceList il main object è la resource, ma la issueAction usa FLT_ISSUE_ASSIGNED_TO
  if (!JSP.ex(pageState.mainObjectId)) {
    pageState.mainObjectId = pageState.getEntry("FLT_ISSUE_ASSIGNED_TO").stringValueNullIfEmpty() + "";
  } else {
    pageState.addClientEntry("FLT_ISSUE_ASSIGNED_TO", pageState.mainObjectId);
    IssueBricks.addOpenStatusFilter(pageState);
  }

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    //final ScreenArea body = new ScreenArea(new ResourceController(), request);
    final ScreenArea body = new ScreenArea(request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {

    Resource resource = Resource.load(pageState.mainObjectId);
    pageState.setMainObject(resource);

    // Form -----------------------------------------------------------------------------------------------
    PageSeed self = pageState.thisPage(request);
    self.mainObjectId = resource.getId();
    self.setCommand(Commands.EDIT);
    String parId = pageState.getEntry(Fields.PARENT_ID).stringValueNullIfEmpty();
    self.addClientEntry(Fields.PARENT_ID, JSP.w(parId));

    Form f = new Form(self);
    f.encType = Form.MULTIPART_FORM_DATA;
    f.alertOnChange = true;
    f.id="issueMultiEditor";
    pageState.setForm(f);

    //f.start(pageContext);

//----------------------------------------------- MAIN COLUMN START ---------------------------------------------------------
%>
<div class="mainColumn">
  <%
    //---------------------------------------- HEAD BAR -------------------------------------------
    pageState.addClientEntry("RESOURCE_TABSET", "RESOURCE_ISSUES_TAB");
    ResourceHeaderBar head = new ResourceHeaderBar(resource);
    //head.pathToObject.mainClass = Resource.class;
    head.toHtml(pageContext);
    //-----------------HEAD BAR END

    //---------------------------------------------- ISSUE LIST START ---------------------------------------------------------
    new JspHelper("/applications/teamwork/issue/partIssueList.jsp").toHtml(pageContext);
    //---------------------------------------------- ISSUE LIST END ---------------------------------------------------------

//----------------------------------------------- MAIN COLUMN END ---------------------------------------------------------
  %>
</div>
<%
    //---------------------------------------- SIDE BAR -------------------------------------------
    JspHelper side = new JspHelper("part/partResourceSideBar.jsp");
    side.parameters.put("RESOURCE", resource);
    side.toHtml(pageContext);

//---------------------------------------------- RIGHT COLUMN END ---------------------------------------------------------
   // f.end(pageContext);
  }
%>
