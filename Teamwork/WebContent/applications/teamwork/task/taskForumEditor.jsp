<%@ page
    import="com.opnlb.website.forum.ForumEntry, com.twproject.forum.TeamworkForumEntry, com.twproject.forum.businessLogic.ForumController, com.twproject.operator.TeamworkOperator,
  com.twproject.task.Task, com.twproject.waf.TeamworkHBFScreen, com.twproject.waf.html.TaskHeaderBar, org.jblooming.utilities.DateUtilities, org.jblooming.waf.ScreenArea,
   org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.ButtonBar,
    org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.input.TinyMCE, org.jblooming.waf.html.state.Form,
     org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.Iterator" %>
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

    TeamworkForumEntry post = (TeamworkForumEntry) pageState.getMainObject();


    PageSeed ps = pageState.thisPage(request);
    ps.command = Commands.EDIT;
    ps.addClientEntry("TASK_ID", task.getId());
    ps.addClientEntry(pageState.getEntry("THREAD_ID"));
    ps.mainObjectId =post.getId();

    Form form = new Form(ps);
    form.alertOnChange=true;
    form.start(pageContext);
    pageState.setForm(form);
%>

<div class="mainColumn">
  <%
    pageState.addClientEntry("TASK_TABSET","TASK_TLOG_TAB");
    TaskHeaderBar head = new TaskHeaderBar(task);
    head.pathToObject.destination=pageState.pageInThisFolder("taskForumList.jsp",request);
    head.pathToObject.alternativeCEForMainObject="TASK_ID";
    head.toHtml(pageContext);

    TeamworkForumEntry threadRoot = TeamworkForumEntry.load(pageState.getEntry("THREAD_ID").intValueNoErrorCodeNoExc());

    TeamworkForumEntry youngestBrother = null;
    if (threadRoot != null && threadRoot.getChildrenSize() > 0) {
      Iterator brothers = threadRoot.getChildrenIterator();
      while (brothers.hasNext()) {
        TeamworkForumEntry brother = (TeamworkForumEntry) brothers.next();
        if (youngestBrother == null)
          youngestBrother = brother;
        else if (brother.getCreationDate().getTime() > youngestBrother.getCreationDate().getTime())
          youngestBrother = brother;
      }
    }


  %>
  <div class="<%=(post.isNew() ? "new" : "" )%> discussionWrap discussionEditor" style="padding-top: 10px">
    <table class="table" cellpadding="0" border="0"> <%


    /*
   ________________________________________________________________________________________________________________________________________________________________________


   display  threadRoot

   ________________________________________________________________________________________________________________________________________________________________________

   */
    if (threadRoot!=null) {
  %> <tr><td colspan="2">
    <div class="postEntry">
      <div class="postAuthorImage">
        <%((TeamworkOperator)threadRoot.getAuthor()).getPerson().bricks.getAvatarImage("medium").toHtml(pageContext);%>
      </div>
      <div style="vertical-align: top; display:inline-block; width: calc(100% - 80px)">
        <h2><%=threadRoot.getTitle()%></h2>
        <div class="postMeta"><label>by <b><%=threadRoot.getAuthor()!=null ? threadRoot.getAuthor().getDisplayName() : "anonymous"%></b>,
          &nbsp;on&nbsp;<%=DateUtilities.dateToString(threadRoot.getPostedOn()).toLowerCase()%></label>
        </div>
        <div class="linkEnabled postDescription"><%=threadRoot.getContent()%></div>
       </div>
    </div>
  </td>
  </tr> <%
    }

 /*
________________________________________________________________________________________________________________________________________________________________________


  display most recent brother

________________________________________________________________________________________________________________________________________________________________________

  */
    if (youngestBrother!=null && post.isNew()) {
  %><tr><td colspan="2">
    <div class="linkEnabled replyEntry">
      <div class="postMeta"><div class="postAuthorImage">
        <%((TeamworkOperator)youngestBrother.getAuthor()).getPerson().bricks.getAvatarImage("").toHtml(pageContext);%>
      </div>
        <label>last post by&nbsp;<%=youngestBrother.getAuthor()!=null ? youngestBrother.getAuthor().getDisplayName() : "anonymous"%>,
          &nbsp;on&nbsp;<%=DateUtilities.dateToRelative(youngestBrother.getPostedOn())%></label>
      </div>
      <div class="replyDescription"><%=youngestBrother.getContent()%></div>
    </div>
  </td>
  </tr> <%
    }


    %><tr><td><%
      if (threadRoot==null) {
        %><h2><%=(post.isNew() ? I18n.get("POST_NEW_THREAD") : I18n.get("POST_EDIT_THREAD"))%></h2><%
      }
    %>
      <div class="postAuthorImage">
      <%  if (youngestBrother!=null) {

        ((TeamworkOperator)youngestBrother.getAuthor()).getPerson().bricks.getAvatarImage("medium").toHtml(pageContext);
      } else {

        ((TeamworkOperator)post.getAuthor()).getPerson().bricks.getAvatarImage("medium").toHtml(pageContext);
      }


      %>
    </div>
      <div style="vertical-align: top; display:inline-block;width: calc(100% - 80px); ">

        <%

          //title available for thread root only
          if (threadRoot==null){
            TextField tf = new TextField("", "POST_SUBJECT", "<br>", 50, false);
            tf.script = "style=width:100%";
            //tf.fieldClass = "formElements bold";
            tf.innerLabel=I18n.get("POST_SUBJECT");
            tf.required=true;
            tf.toHtmlI18n(pageContext);
            %><br><%
        }

        TinyMCE tmce = new TinyMCE("", "POST_MESSAGE", "<br>", "100%", "250px", pageState);
        tmce.showHTMLButton = true;
        tmce.required=true;
        tmce.textArea.preserveOldValue=true;
        tmce.toHtml(pageContext);
      %>

      </div>
    </td></tr>




  </table><br><%


    ButtonBar buttonBar =new ButtonBar();

    if (post!=null && !post.isNew()) {


      DeletePreviewer deletePreviewer = new DeletePreviewer("FORUM_DEL", ForumController.class, pageState);
      deletePreviewer.normalizeInstanceToSuperclass = ForumEntry.class;
      ButtonSupport delPrev = deletePreviewer.getDeleteButton(I18n.get("DELETE"), post.getId() + "");
      delPrev.additionalCssClass = "big delete";
      buttonBar.addButton(delPrev);
    }

    ButtonSubmit postSb = ButtonSubmit.getSaveInstance(pageState.getForm(), I18n.get("SAVE"));
    if (post!=null && !post.isNew())
      postSb.label=I18n.get("SAVE");
    postSb.additionalCssClass="big first";
    postSb.variationsFromForm.command=Commands.SAVE;
    buttonBar.addToLeft(postSb);


    buttonBar.toHtml(pageContext);

  %></div></div><%


    JspHelper side = new JspHelper("part/partTaskSideBar.jsp");
    side.parameters.put("TASK",task);
    side.toHtml(pageContext);

    form.end(pageContext);


  }
%>
