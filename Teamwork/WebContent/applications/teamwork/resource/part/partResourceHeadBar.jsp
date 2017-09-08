<%@ page import="com.twproject.resource.Person, com.twproject.resource.Resource, com.twproject.waf.html.ResourceHeaderBar, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, org.jblooming.waf.html.container.TabSet, org.jblooming.waf.view.PageSeed, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.container.Tab, com.twproject.task.IssueStatus, org.jblooming.waf.constants.Fields, com.twproject.task.TaskStatus, org.jblooming.waf.html.core.HtmlIncluder, com.twproject.task.IssueBricks, org.jblooming.waf.html.display.Img, org.jblooming.waf.html.button.ButtonJS, com.twproject.security.TeamworkPermissions, org.jblooming.operator.Operator, com.twproject.operator.TeamworkOperator" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);
  ResourceHeaderBar resourceHeaderBar = (ResourceHeaderBar) JspIncluderSupport.getCurrentInstance(request);
  Resource resource= resourceHeaderBar.resource;

  boolean isNew = resource.isNew();
  boolean isPerson = (resource instanceof Person);


  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
  boolean canWrite = resource.hasPermissionFor(logged, TeamworkPermissions.resource_canWrite);

  boolean canEditImage = (canWrite || resource.equals(logged.getPerson() ))&& !isNew && pageState.href.indexOf("resourceEditor")>0 ;

%>
<div class="profileImage<%= canWrite && !isNew ? " canWrite": ""%>" <%if (canEditImage) {%> onclick="openProfileImageEditor('<%=resource.getId()%>')" style="cursor: pointer" <%}%>><%
  Img img = resource.bricks.getAvatarImage("");
  img.id = "personalAvatar";
  img.toHtml(pageContext);

  if(canEditImage) {
    %><div class="imageUploaderOpener"><span class="teamworkIcon " style="cursor: pointer" >e</span></div><%
  }
%>
</div>

<script>$("#RESOURCE_MENU").addClass('selected');</script><%


  resourceHeaderBar.pathToObject.toHtml(pageContext);

%><div class="pathCodeWrapper clearfix"><div class="pathCode"><%=(resource.isNew() ? "" : "R#" + resource.getMnemonicCode())+"#"%></div></div><%

  // BEGIN TABSET ----------------------------------------------

  TabSet tabset = new TabSet("RESOURCE_TABSET",pageState);

//  tabset.pre.add(new HtmlIncluder("<span class='PathCode' title='"+I18n.get("REFERENCE_CODE")+"'>"+(resource.isNew() ? "" : "R#" + resource.getMnemonicCode())+"#</span>"));

  {
    PageSeed ps = pageState.pageFromRoot("resource/resourceEditor.jsp");
    ps.command = Commands.EDIT;
    ps.mainObjectId = resource.getId();
    ButtonLink bl = new ButtonLink(I18n.get("RESOURCE_GENERAL_TAB"), ps);
    bl.enabled = !isNew;
    bl.hasFocus = pageState.thisPage(request).href.indexOf(ps.href) >= 0;
    Tab tab = new Tab("RESOURCE_GENERAL_TAB", bl);
    tabset.addTab(tab);
  }

  if (isPerson) {
    {
      PageSeed ps = pageState.pageFromRoot("resource/resourceSecurity.jsp");
      ps.command = Commands.EDIT;
      ps.mainObjectId =resource.getId();
      ButtonLink bl = new ButtonLink(I18n.get("RESOURCE_SECURITY_TAB"), ps);
      bl.id = "hintFirstSecurity";
      bl.enabled=!isNew && (canWrite|| resource.equals(logged.getPerson()));
      bl.hasFocus = pageState.thisPage(request).href.indexOf(ps.href) >= 0;
      Tab tab = new Tab("RESOURCE_SECURITY_TAB", bl);
      tabset.addTab(tab);
    }

    {
      PageSeed ps = pageState.pageFromRoot("resource/resourceOptions.jsp");
      ps.command = "EDIT_OPT";
      ps.mainObjectId = resource.getId();
      ButtonLink bl = new ButtonLink(I18n.get("GENERAL_OPTIONS"), ps);
      bl.id = "hintFirstOption";
      bl.enabled=!isNew && resource.getMyself()!=null && (canWrite || resource.equals(logged.getPerson()));
      bl.hasFocus = pageState.thisPage(request).href.indexOf(ps.href) >= 0;
      Tab tab = new Tab("GENERAL_OPTIONS_TAB", bl);
      tabset.addTab(tab);
    }
  }

  {
    PageSeed ps = pageState.pageFromRoot("resource/resourceAssignments.jsp");
    ps.command = Commands.EDIT;
    ps.mainObjectId = resource.getId();
    ButtonLink bl = new ButtonLink(I18n.get("RESOURCE_ASSIG_TAB"), ps);
    bl.enabled=!isNew;
    Tab tab = new Tab("RESOURCE_ASSIG_TAB", bl);
    tabset.addTab(tab);
  }

  {
    PageSeed ps = pageState.pageFromRoot("resource/resourceIssueList.jsp");
    ps.mainObjectId=resource.getId();
    IssueBricks.addOpenStatusFilter(ps);
    ButtonLink bl = new ButtonLink(I18n.get("ISSUES"), ps);
    bl.enabled=!isNew;
    bl.hasFocus = pageState.thisPage(request).href.indexOf(ps.href) >= 0;
    Tab tab = new Tab("RESOURCE_ISSUES_TAB", bl);
    tabset.addTab(tab);
  }

  {
    PageSeed psDocumentList = pageState.pageFromRoot("resource/resourceDocumentList.jsp");
    psDocumentList.setCommand("LIST_DOCS");
    psDocumentList.addClientEntry("RES_ID",resource.getId());
    ButtonLink bl = new ButtonLink(I18n.get("DOCUMENTS"), psDocumentList);
    bl.enabled = !isNew;
    bl.hasFocus = pageState.thisPage(request).href.indexOf(psDocumentList.href) >= 0;
    Tab tab = new Tab("DOCUMENTS_TAB", bl);
    tabset.addTab(tab);
  }

  {
    PageSeed ps = pageState.pageFromRoot("resource/resourceSubscriptions.jsp");
    ps.command = Commands.EDIT;
    ps.mainObjectId = resource.getId();
    ButtonLink bl = new ButtonLink(I18n.get("TASK_SUBSCRIPTIONS_TAB"), ps);
    bl.enabled = !isNew;
    bl.hasFocus = pageState.thisPage(request).href.indexOf(ps.href) >= 0;
    Tab tab = new Tab("TASK_SUBSCRIPTIONS_TAB", bl);
    tabset.addTab(tab);
  }

  tabset.drawBar(pageContext);

  tabset.end(pageContext);

  // END TABSET ----------------------------------------------

   %>
