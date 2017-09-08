<%@ page import="com.twproject.fulltext.waf.FullTextSearchControllerAction,
                 com.twproject.messaging.stickyNote.StickyNote,
                 com.twproject.operator.TeamworkOperator,
                 com.twproject.security.TeamworkPermissions,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.ScreenBasic,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.constants.OperatorConstants,
                 org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSupport,
                 org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  String text=pageState.getEntry("TEXT").stringValueNullIfEmpty();

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;

    ScreenBasic.preparePage(new FullTextSearchControllerAction(pageState), pageContext);
    pageState.perform(request, response).toHtml(pageContext);
  } else {

    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    PageSeed self = pageState.thisPage(request);
    self.setCommand(Commands.FIND);

    int ps =  pageState.getEntry(OperatorConstants.OP_PAGE_SIZE).intValueNoErrorCodeNoExc();
    if (ps==0)
      ps = 10;
    self.addClientEntry(OperatorConstants.OP_PAGE_SIZE,ps);
    
    Form f = new Form(self);
    pageState.setForm(f);
    f.start(pageContext);

   %><script>$("#TOOLS_MENU").addClass('selected');</script>



<%---------------------------------------------- MAIN COLUMN START ---------------------------------------------------------%>

<div class="mainColumn">
  <div class="inlineContainerWrapper">
    <div class="container">
    <h1><%=I18n.get("TEAMWORK_SEARCH")%></h1>
      <div class="box">
    <table cellpadding="3" class="table">
      <tr>
        <td><%

          TextField search = new TextField("TEXT", "", "TEXT", "", 50, false);
          search.addKeyPressControl(13, "fastSearch($(this));","onkeyup" );
          search.fieldClass="formElements formElementsBig";
          pageState.setFocusedObjectDomId(search.id);
          search.toHtml(pageContext);

        %></td><td align="left"><%
        ButtonJS searchBT = new ButtonJS(I18n.get("FLD_SEARCH"),"fastSearch($('#TEXT'))");
        //ButtonSubmit searchBT = ButtonSubmit.getSearchInstance(f, pageState);
        searchBT.additionalCssClass="big first";
        searchBT.style="margin-bottom:0";
        //searchBT.variationsFromForm.command="FIND_TEXT";
        searchBT.toHtml(pageContext);

      %></td>
      </tr>
    </table>
      </div>

    <jsp:include page="partTextSearch.jsp" />

    </div>
    <div class="container" style="width: 20%">

      <h2><%=I18n.get("TEAMWORK_ADVANCED_SEARCH")%></h2>
      <div class=" noprint">

        <%=I18n.get("TEAMWORK_ADVANCED_SEARCH_HELP")%>:<br><br><%

        PageSeed ts = pageState.pageFromRoot("task/taskList.jsp");
        ts.addClientEntry("NAME_DESCRIPTION",text);
        ts.command=Commands.FIND;
        ButtonLink task = new ButtonLink(I18n.get("TASK_MENU"), ts);
        task.iconChar="t";
        task.toHtmlInTextOnlyModality(pageContext);
        %><br><%
        PageSeed al = pageState.pageFromRoot("agenda/agendaList.jsp");
        al.addClientEntry("OBJECT_DESCRIPTION",text);
        al.command=Commands.FIND;

        ButtonLink agenda = new ButtonLink(I18n.get("AGENDA"), al);
        agenda.iconChar="C";
        agenda.toHtmlInTextOnlyModality(pageContext);
      %><br><%
        PageSeed is = pageState.pageFromRoot("issue/issueList.jsp");
        is.addClientEntry("FLT_ISSUE_DESCRIPTION",text);
        is.command=Commands.FIND;
        ButtonLink issue = new ButtonLink(I18n.get("ISSUES_MENU"), is);
        issue.iconChar="i";
        issue.toHtmlInTextOnlyModality(pageContext);
      %><br><%

        PageSeed rl = pageState.pageFromRoot("resource/resourceList.jsp");
        rl.addClientEntry("NAME_SURNAME",text);
        rl.command=Commands.FIND;
        ButtonLink res = new ButtonLink(I18n.get("RESOURCE_MENU"), rl);
        res.iconChar="M";
        res.toHtmlInTextOnlyModality(pageContext);
      %><br><%
        PageSeed dl = pageState.pageFromRoot("document/documentList.jsp");
        dl.addClientEntry("NAME_DESCRIPTION",text);
        dl.command=Commands.FIND;
        ButtonLink doc = new ButtonLink(I18n.get("DOCUMENTS"), dl);
        doc.iconChar="c";
        doc.toHtmlInTextOnlyModality(pageContext);
      %><br><%
        PageSeed wl = pageState.pageFromRoot("task/worklog/worklogList.jsp");
        wl.addClientEntry("WORKLOG_ACTION",text);
        wl.command=Commands.FIND;
        ButtonLink work = new ButtonLink(I18n.get("WORKLOGS"), wl);
        work.iconChar="A";
        work.toHtmlInTextOnlyModality(pageContext);
      %></div>

       <%
         if (JSP.ex(text)) {

       %><div class="separator">&nbsp;</div>
      <h2><%=I18n.get("TEAMWORK_ACTION_SEARCH")%></h2>
      <div class=" noprint"><%=I18n.get("TEAMWORK_SEARCH_ACTION HELP")%>

        <br><br><%

          PageSeed pss = new PageSeed("");
          pss.setCommand(Commands.ADD);
          pss.addClientEntry("messageBody",text);
          ButtonSupport cstickyL = new StickyNote().bricks.getEditorInBlack(I18n.get("SEND_STICKY"),pss, pageState);
          cstickyL.iconChar="&igrave;";
          cstickyL.toHtmlInTextOnlyModality(pageContext);

        %><br><%

          ButtonJS addBl = new ButtonJS(I18n.get("ADD_ISSUE"),"openIssueEditorInBlack('0','"+Commands.ADD+"','ISSUE_DESCRIPTION='+encodeURIComponent($('#TEXT').val()));");
          addBl.iconChar="P";
          addBl.toHtmlInTextOnlyModality(pageContext);

          %><br><%

          if (logged.hasPermissionFor(TeamworkPermissions.project_canCreate)) {

            PageSeed newTask = new PageSeed(request.getContextPath() + "/applications/teamwork/task/taskNew.jsp");
            newTask.addClientEntry("ADD_TYPE", "ADD_TASK");
            newTask.addClientEntry("TASK_NAME",text);
            newTask.setCommand(Commands.ADD);
            ButtonSupport bl = ButtonLink.getBlackInstance(I18n.get("ADD_TASK"), 720, 800, newTask);
            bl.iconChar="P";
            bl.toHtmlInTextOnlyModality(pageContext);

          }

        %></div><div class="separator">&nbsp;</div><%
      }
       %>

    </div>
  </div>
</div>

<%---------------------------------------------- MAIN COLUMN END ---------------------------------------------------------%>

<%
    f.end(pageContext);
  }

%>
