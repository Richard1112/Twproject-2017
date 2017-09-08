<%@ page import="com.opnlb.website.forum.ForumEntry, com.twproject.forum.TeamworkForumEntry, com.twproject.forum.businessLogic.ForumController,
com.twproject.operator.TeamworkOperator, com.twproject.task.Task, com.twproject.waf.TeamworkHBFScreen,
com.twproject.waf.html.TaskHeaderBar, org.jblooming.oql.OqlQuery, org.jblooming.utilities.DateUtilities, org.jblooming.waf.ScreenArea,
org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar,
org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.input.TinyMCE, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n,
org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List" %>
<%


  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new ForumController(), request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {
    //this is set by action
    Task task = (Task) pageState.attributes.get("REFERRAL_OBJECT");

    TeamworkForumEntry thread = (TeamworkForumEntry) pageState.getMainObject();

    PageSeed ps = pageState.thisPage(request);
    ps.command = "";
    ps.mainObjectId = thread.getId();
    ps.addClientEntry("TASK_ID", task.getId());
    Form form = new Form(ps);
    form.alertOnChange=true;
    form.start(pageContext);
    pageState.setForm(form);
%>
<div class="mainColumn"><%
  pageState.addClientEntry("TASK_TABSET","TASK_TLOG_TAB");

  TaskHeaderBar head = new TaskHeaderBar(task);
  head.pathToObject.destination=pageState.pageInThisFolder("taskForumList.jsp",request);
  head.pathToObject.alternativeCEForMainObject="TASK_ID";
  head.toHtml(pageContext);
%>
  <div class="discussionWrap discussionThread lreq10" style="min-height:315px;padding-top: 10px"><%

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    boolean canAdminister = logged != null && (logged.hasPermissionAsAdmin() || logged.equals(thread.getAuthor()));

    PageSeed edTh1 = pageState.pageInThisFolder("taskForumEditor.jsp", request);
    edTh1.mainObjectId = thread.getId();
    edTh1.addClientEntry("TASK_ID", task.getId());
    edTh1.command = Commands.EDIT;
    ButtonLink editLink1 = new ButtonLink("", edTh1);
    editLink1.toolTip = I18n.get("EDIT");
    editLink1.iconChar="e";
    editLink1.enabled = canAdminister;
  %><div style="position:relative;" class="linkEnabled postEntry">

    <div class="postAuthorImage">
      <%((TeamworkOperator)thread.getAuthor()).getPerson().bricks.getAvatarImage("medium").toHtml(pageContext);%>
    </div>

    <div style="vertical-align: top; display:inline-block">
      <h2><%=thread.getTitle()%></h2>

      <div class="postMeta">

        <label><b><%=thread.getAuthor().getDisplayName()%></b> <%=DateUtilities.dateToRelative(thread.getCreationDate())%></label>
        <% if (canAdminister) {
        %><span class="bullet">&#8226;</span><%
          editLink1.toHtmlInTextOnlyModality(pageContext);
        }
      %>
      </div>
      <div class="postDescription"><%=thread.getContent()%></div>
      </div>
    </div>
    <%

      if (thread.getChildrenSize() > 0){
    %><h3><%=I18n.get("FORUM_ANSWERS")%> (<%=thread.getChildrenSize()%>)</h3><%


        String hql = "from " + ForumEntry.class.getName() + " as fe where fe.parent=:parent order by fe.creationDate";
        OqlQuery oql = new OqlQuery(hql);
        oql.getQuery().setEntity("parent", thread);

        List<ForumEntry> fes = oql.list();

        PageSeed edTh= pageState.pageInThisFolder("taskForumEditor.jsp",request);
        edTh.mainObjectId=thread.getId();
        edTh.addClientEntry("TASK_ID", task.getId());
        edTh.command= Commands.EDIT;
        edTh.addClientEntry("THREAD_ID",thread.getId());

        ButtonLink editLink = new ButtonLink("",edTh);
        editLink.toolTip= I18n.get("EDIT");
        editLink.iconChar="e";
        editLink.enabled=canAdminister;


        for (ForumEntry child : fes) {
          canAdminister = logged != null && (logged.hasPermissionAsAdmin() || logged.equals(child.getAuthor()));
          edTh.mainObjectId = child.getId();
          editLink = new ButtonLink("", edTh);
          editLink.toolTip = I18n.get("EDIT");
          editLink.iconChar="e";
          editLink.enabled = canAdminister;
    %>
    <div class="linkEnabled replyEntry">
      <div class="replyDescription">
        <div class="postAuthorImage">
          <%((TeamworkOperator) child.getAuthor()).getPerson().bricks.getAvatarImage("small").toHtml(pageContext);%>
        </div>
        <div style="vertical-align: top; display:inline-block; width: calc(100% - 80px)">
          <div class="postMeta">

            <label><%=child.getAuthor().getDisplayName()%> - <%=DateUtilities.dateToRelative(child.getLastModified())%>
            </label>
            <% if (canAdminister) {
            %><span class="bullet">&#8226;</span><%
              editLink.toHtmlInTextOnlyModality(pageContext);
            }
          %></div>
          <p><%=child.getContent()%></p>
        </div>
      </div>
    </div>
    <%
      }
    } else {
    %><%
      }


    %><br><br><h3><%=I18n.get("ADD_ANSWER")%></h3><div class="commEditPlace"><%
     TinyMCE tmce = new TinyMCE("", "REPLY_MESSAGE", "", "100%", "150px", pageState);
      tmce.showHTMLButton = true;
      tmce.required=true;
      tmce.textArea.preserveOldValue=true;
      tmce.toHtml(pageContext);

      ButtonBar buttonBar = new ButtonBar();
      buttonBar.buttonAreaHtmlClass = "buttonArea notFix";
      ButtonSubmit postSb = ButtonSubmit.getSaveInstance(pageState.getForm(), I18n.get("REPLY_THREAD"));
      postSb.additionalCssClass = "first";
      postSb.variationsFromForm.command = "REPLY";
      buttonBar.addToLeft(postSb);

      buttonBar.toHtml(pageContext);

    %></div>
  </div>
</div><%

    JspHelper side = new JspHelper("part/partTaskSideBar.jsp");
    side.parameters.put("TASK",task);
    side.toHtml(pageContext);

    form.end(pageContext);

  }
%>
