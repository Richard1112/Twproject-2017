<%@ page import="com.opnlb.website.forum.ForumEntry, com.twproject.forum.businessLogic.ForumController, com.twproject.operator.TeamworkOperator,
com.twproject.task.Task, com.twproject.waf.TeamworkHBFScreen, com.twproject.waf.html.TaskHeaderBar,
org.jblooming.agenda.CompanyCalendar, org.jblooming.oql.OqlQuery, org.jblooming.persistence.PersistenceHome, org.jblooming.utilities.DateUtilities,
org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonJS,
org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.html.input.TextField,
org.jblooming.waf.html.layout.HtmlColors, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed,
org.jblooming.waf.view.PageState, java.util.List" %>
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

    PageSeed ps = pageState.thisPage(request);
    ps.command = "";
    ps.addClientEntry("TASK_ID", task.getId());
    Form form = new Form(ps);
    form.start(pageContext);
    pageState.setForm(form);

%>

<div class="mainColumn">
<%
  pageState.addClientEntry("TASK_TABSET","TASK_TLOG_TAB");
  TaskHeaderBar head = new TaskHeaderBar(task);
  head.pathToObject.destination=pageState.thisPage(request);
  head.pathToObject.alternativeCEForMainObject = "TASK_ID";
  head.toHtml(pageContext);

  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  ForumEntry fe = task.getForumEntry();

  TextField.hiddenInstanceOfFormToHtml("POST_ID", pageState.getForm(), pageContext);

  pageState.addClientEntry("THREAD_ID", PersistenceHome.NEW_EMPTY_ID + "");
  TextField.hiddenInstanceOfFormToHtml("THREAD_ID", pageState.getForm(), pageContext);


  new DeletePreviewer("POST_DEL",ForumController.class, pageState);


%><div class="discussionWrap discussionList lreq10" style="padding-top: 10px"><%

  if (fe!=null && fe.getChildrenSize() > 0) {

    String hql = "from " + ForumEntry.class.getName() + " as fe where fe.parent=:parent order by fe.postedOn desc";

    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setEntity("parent", fe);

    List<ForumEntry> fes = oql.list();

    boolean canAdminister;
    for (ForumEntry post : fes) {
      canAdminister = logged!=null && (logged.hasPermissionAsAdmin() || logged.equals(post.getAuthor())) ;

      long now = System.currentTimeMillis();
      long diff = now - post.getLastPostOnBranch().getTime();

      //one day 10%
      int var = (int) (diff / (CompanyCalendar.MILLIS_IN_HOUR * 6));
      var = var > 30 ? 30 : var;
      String finalBG = HtmlColors.modifyLight("#cccccc", -var * 2);
%>
    <div class="listData discussionLine" <%--style="border-bottom: 1px solid <%=finalBG%>;"--%> postId="<%=post.getId()%>">

      <div class="postAuthorImage">
        <%((TeamworkOperator)post.getAuthor()).getPerson().bricks.getAvatarImage("medium").toHtml(pageContext);%>
      </div>

      <div style="vertical-align: top; display:inline-block">
          <% PageSeed full = pageState.pageInThisFolder("taskForumThread.jsp", request);
                full.mainObjectId = post.getId();
                full.addClientEntry("TASK_ID", task.getId());
                full.command = "LIST_POSTS";
                ButtonLink listLink = ButtonLink.getTextualInstance("<h2>" + post.getTitle() + "</h2>", full);

                listLink.toHtml(pageContext);
              %>
          <div class="postMeta">

            <label>
              <b><%=post.getAuthor() != null ? post.getAuthor().getDisplayName() : "anonymous"%></b> - <%=DateUtilities.dateToRelative(post.getCreationDate())%>
            </label><%

            PageSeed delPost = pageState.pageInThisFolder("taskForumEditor.jsp", request);
            delPost.command = Commands.DELETE_PREVIEW;
            delPost.mainObjectId = post.getId();
            delPost.addClientEntry("TASK_ID", task.getId());

            ButtonJS delPrev = new ButtonJS("","deletePreviewPost('"+post.getId()+"');");
            delPrev.iconChar = "d";
            delPrev.additionalCssClass="delete";

            if(canAdminister)
            {
          %><span class="bullet">&#8226;</span><%
              delPrev.toHtmlInTextOnlyModality(pageContext);
            }

          %>
          </div>
          <div class="postDescription"><%=JSP.ex(post.getContent()) ? post.getContent() : ""%></div>
          <div class="discussionFooter">
            <%
                if (post.getChildrenSize()>0) {
                  listLink.iconChar="Q";
                  listLink.label = "<b>" + post.getChildrenSize() + "</b> " + I18n.get("FORUM_ANSWERS");
                } else {
                  listLink.label=I18n.get("ADD_ANSWER");
                }
              listLink.toHtml(pageContext);
            %>
            &nbsp;&nbsp;<label><em title="<%=DateUtilities.dateAndHourToFullString(post.getLastPostOnBranch())%>">(<%=I18n.get("FORUM_LASTPOST")%>
            <%=DateUtilities.dateToRelative(post.getLastPostOnBranch())%> <%=I18n.get("POSTED_BY")%> <%=JSP.w(post.getLastPosterOnBranch())%>)</em></label>
            </div>
      </div>
    </div><%
  }
  } else {
    %><div class="boxInfo" style="text-align: center">
    <div class="hint" style="text-align: center"><%=I18n.get("TASK_NO_DIARY")%></div><%

    {
      //-----------------DISCUSSION
      PageSeed addThread=pageState.pageFromRoot("task/taskForumEditor.jsp");
      addThread.command = Commands.ADD;
      addThread.addClientEntry("TASK_ID",task.getId());
      ButtonLink addThreadLink= new ButtonLink(addThread);
      addThreadLink.label = I18n.get("POST_NEW_THREAD");
      addThreadLink.iconChar="P";
      //addThreadLink.additionalCssClass="small";
      addThreadLink.toHtml(pageContext);

    }

    %></div><%
  }

%>

</div></div><%

    JspHelper side = new JspHelper("part/partTaskSideBar.jsp");
    side.parameters.put("TASK", task);
    side.toHtml(pageContext);

    form.end(pageContext);

%>

<script>
  function deletePreviewPost(postId){
    deletePreview("POST_DEL", postId, function(response){  // callback function
      if (response && response.ok){
        $(".discussionLine[postId="+postId+"]").fadeOut(500, function () {$(this).remove();});
      }
    });
  }

</script>
<%
  }
%>
