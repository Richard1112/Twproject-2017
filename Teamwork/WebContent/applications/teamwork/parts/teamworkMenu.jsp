<%@ taglib prefix="pd4ml" uri="http://pd4ml.com/tlds/pd4ml/2.5" %>
<%@ page buffer="16kb" %><%@ page import="com.opnlb.website.waf.WebSiteConstants,
                 com.twproject.agenda.AgendaBricks,
                 com.twproject.operator.TeamworkOperator,
                 com.twproject.resource.Person,
                 com.twproject.security.TeamworkArea,
                 org.jblooming.operator.Operator,
                 org.jblooming.utilities.DateUtilities,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.constants.Fields,
                 org.jblooming.waf.html.button.ButtonImg,
                 org.jblooming.waf.html.button.ButtonJS,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.display.Hint,
                 org.jblooming.waf.html.display.Img,
                 org.jblooming.waf.html.input.TextField,
                 org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jblooming.waf.html.container.DivOnMouseover, org.jblooming.waf.html.core.TextEmbedder, org.jblooming.messaging.MessagingSystem, com.twproject.task.Assignment" %><%
  PageState pageState = PageState.getCurrentPageState(request);

  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
  Img log = new Img("logo.png", "Home");

  String homePage = Operator.getOperatorOption(pageState.getLoggedOperator(), WebSiteConstants.HOME_PAGE);
  if(logged!=null && !logged.hasPermissionAsAdmin() && "firstStart.page".equalsIgnoreCase(homePage))
    homePage = "/getsThingsDone.page";

  PageSeed homePs = pageState.pageFromRoot(homePage);

  ButtonLink home = new ButtonLink(homePs);

  ButtonImg bi = new ButtonImg(home, log);

  Hint.HintWriter hintWriter = new Hint.HintWriter();
  hintWriter.addHint("HINT_SEARCH", "#HINT_SEARCH_span", 300,350,  JSP.ex(logged.getOption("HINT_FIRST_ASSIG_ROLE")) && JSP.ex(logged.getOption("HINT_FIRST_ASSIG")) && JSP.ex(logged.getOption("HINT_FIRST_SUBTASK")) && JSP.ex(logged.getOption("HINT_FIRST_DOC")) && JSP.ex(logged.getOption("HINT_FIRST_COST")), pageState);
  hintWriter.toHtml(pageContext);

  if (logged != null) {

    if (Fields.TRUE.equals(ApplicationState.applicationParameters.get("TEAMWORK_ASP_INSTANCE"))) {
      Person p = logged.getPerson();
      TeamworkArea a = TeamworkArea.load(p.getArea().getId());
      if (a!=null && a.getExpiry()!=null ){
        PageSeed buynow = new PageSeed(request.getContextPath() + "/applications/teamwork/buyTwproject.jsp");
        ButtonLink scbl = new ButtonLink(I18n.get("THIS_IS_A_DEMO_EXPIRES_ON_%%", DateUtilities.dateToString(a.getExpiry(),"dd MMMM yyyy")), buynow);
        scbl.toolTip = I18n.get("GRACE_DAYS_LEFT_DESCR");
%><div class="stripLicenseInfo"><%scbl.toHtmlInTextOnlyModality(pageContext);%>&nbsp;&nbsp;&nbsp;&nbsp;<a href="mailto:info@twproject.com?subject=I need more time for my demo"><span class="teamworkIcon">S</span> <%=I18n.get("DEMO_NEED_MORE_TIME")%></a></div><%
  }

} else if(JSP.ex((String)ApplicationState.applicationParameters.get("LIC_EXPIRES"))) {
  String licExp = (String)ApplicationState.applicationParameters.get("LIC_EXPIRES");

  PageSeed buynow = new PageSeed(request.getContextPath() + "/applications/teamwork/buyTwprojectLicense.jsp");
  ButtonLink scbl = new ButtonLink(I18n.get("LICENSE_EXPIRES_IN_%%", licExp), buynow);
%><div class="stripLicenseInfo"><% scbl.toHtmlInTextOnlyModality(pageContext);%>&nbsp;&nbsp;&nbsp;&nbsp;<a href="mailto:info@twproject.com?subject=License is about to expire"><span class="teamworkIcon">S</span> <%=I18n.get("CONTACT_US")%></a></div><%

  }

%>
<div class="twHeader noprint">
  <div id="headerWrapper ">
    <div class="twLogo withArrowRight"><%bi.toHtml(pageContext);%></div>
    <div id="mainNav" class="noprint mainNav">
      <table  border="0" cellspacing="0" cellpadding="0"  >
        <tr>
          <td id="DASHBOARD_MENU" ttitle="<%=I18n.get("HOME_MENU_HELP")%>" class="withArrowRight">
            <div style="position: relative"><jsp:include page="partMenuCustomPages.jsp"/></div></td>

          <td id="TASK_MENU" ttitle="<%=I18n.get("INTRO_TW_PROJECT_ENTRY_POINT")%>">
            <jsp:include page="partMenuProjects.jsp"/>
          </td>

          <td id="TIMESHEET_MENU" ttitle="<%=I18n.get("TIMESHEET_MENU_HELP")%>">
            <jsp:include page="partMenuTimesheet.jsp"/>
          </td>

          <td id="PLAN_MENU" ttitle="<%=I18n.get("PLAN_MENU_HELP")%>" valign="bottom">
            <jsp:include page="partMenuPlan.jsp"/></td>

          <td id="AGENDA_MENU" ttitle="<%=I18n.get("INTRO_AGENDA")%>">
            <a href="<%=AgendaBricks.getAgendaView(pageState).toLinkToHref()%>" class="button" ><%=I18n.get("AGENDA_MENU")%></a>

          </td>
          <td id="ISSUES_MENU" ttitle="<%=I18n.get("INTRO_ISSUE_DESCR")%>">
            <jsp:include page="partMenuIssues.jsp"/>
          </td>

          <td id="RESOURCE_MENU" ttitle="<%=I18n.get("RESOURCE_MENU_HELP")%>">
            <a href="<%=pageState.pageFromRoot("resource/resourceList.jsp").toLinkToHref()%>" class="button" ><%=I18n.get("RESOURCE_MENU")%></a>
          </td>

          <td id="TOOLS_MENU" ttitle="<%=I18n.get("TOOLS_MENU_HELP")%>">
            <jsp:include page="partMenuTools.jsp"/>
          </td>

        </tr>
      </table>
    </div>

    <div class="menuTools">

      <div id="HINT_SEARCH_span" class="menuSearch"><%

        //set the default ce
        String fastSearch="";
        /*if (pageState.href.indexOf("/worklog/")>=0)
          fastSearch="W:";
        else if (pageState.href.indexOf("/task/")>=0)
          fastSearch="T:";
        else if (pageState.href.indexOf("/resource/")>=0)
          fastSearch="R:";
        else if (pageState.href.indexOf("/issue/")>=0)
          fastSearch="I:";
        else if (pageState.href.indexOf("/agenda/")>=0)
          fastSearch="A:";
        else if (pageState.href.indexOf("/board/")>=0)
          fastSearch="B:";
        else if (pageState.href.indexOf("/document/")>=0)
          fastSearch="D:";
        else
          fastSearch="";*/

        pageState.addClientEntry("search",fastSearch+(JSP.w(pageState.getEntry("search").stringValueNullIfEmpty())));

        TextField tf = new TextField("search", "");
        tf.fieldClass="searchInput";
        tf.label = "";
        tf.fieldSize = 15;
        tf.addKeyPressControl(13, "fastSearch($(this));","onkeyup" );

        tf.toHtml(pageContext);

        ButtonJS opener = new ButtonJS("");
        opener.label="";
        opener.iconChar="[";
        opener.additionalCssClass="fastSearchkeysButton";

        DivOnMouseover domo = new DivOnMouseover(opener);
        domo.id="keyDiv";
        domo.additionalDropDownClass="fSKeys";
        domo.addButton(new TextEmbedder("<span class='menuTitle'>"+I18n.get("SEARCH_IN")+":</span>"));


        ButtonJS all = new ButtonJS(I18n.get("ALL"), "changeFastSearchKey('ALL');");
        all.additionalCssClass = "ALL";
        domo.addButton(all);

        ButtonJS worklog = new ButtonJS(I18n.get("WORKLOG"), "changeFastSearchKey('W');");
        worklog.additionalCssClass = "W";
        domo.addButton(worklog);

        ButtonJS task = new ButtonJS(I18n.get("TASKS"), "changeFastSearchKey('T');");
        task.additionalCssClass = "T";
        domo.addButton(task);

        ButtonJS resource = new ButtonJS(I18n.get("RESOURCES"), "changeFastSearchKey('R');");
        resource.additionalCssClass = "R";
        domo.addButton(resource);

        ButtonJS issue = new ButtonJS(I18n.get("ISSUES"), "changeFastSearchKey('I');");
        issue.additionalCssClass = "I";
        domo.addButton(issue);

        ButtonJS agenda = new ButtonJS(I18n.get("AGENDA"), "changeFastSearchKey('A');");
        agenda.additionalCssClass = "A";
        domo.addButton(agenda);

        ButtonJS board = new ButtonJS(I18n.get("BOARDS"), "changeFastSearchKey('B');");
        board.additionalCssClass = "B";
        domo.addButton(board);

        ButtonJS document = new ButtonJS(I18n.get("DOCUMENTS"), "changeFastSearchKey('D');");
        document.additionalCssClass = "D";
        domo.addButton(document);

        domo.toHtmlInTextOnlyModality(pageContext);

        ButtonJS search = new ButtonJS("fastSearch($('#search'))");
        search.additionalCssClass="searchButton";
        search.label = "";
        search.iconChar = "L";
        search.toHtmlInTextOnlyModality(pageContext);



      %></div>
      <jsp:include page="partMenuPersonal.jsp"/>

      <div id="notifications">
        <div id="messageListOpener" class="messages button textual icon" onclick="loadMessageList($(this));">
          <span class="teamworkIcon">b</span>
          <%
            int unreadMessageCount = MessagingSystem.getUnreadMessageCount(logged, "LOG");
          %><span class="notificationNumber" style="display:<%=unreadMessageCount<=0?"none":"inline-block"%>"><%=unreadMessageCount%></span><%
        %>
        </div>
      </div>

      <div id="messageListBox" class="divomo divDark" style="position: absolute;left:-1000px; top:-1000px;">

        <div class="divomoArrow" ></div>
        <span class="menuTitle"><%=I18n.get("NOTIFICATION")%></span>

        <div id="messageListPlace" class="messageListPlace"> </div>
        <div class="showAllMessages" onclick="$('#messageListBox').trigger('hideDivOnMouseover');"><%new ButtonLink(I18n.get("SHOW_ALL"),pageState.pageFromRoot("messaging/messageListFull.jsp")).toHtmlInTextOnlyModality(pageContext);%></div>
      </div>


        <%Assignment countedAssignment = Assignment.getCountedAssignment(logged);%>
        <div class="menuTimeCounter" style="display:none;"><a href="javascript:openBlackPopup('<%=pageState.pageFromRoot("/task/timeCounter.jsp")%>');" id="tcMenuCounter"></a></div>


    </div>
  </div>
</div>
<script type="text/javascript">
  // unread messages managament
  $("#messageListBox").on("hideDivOnMouseover",function(){
    $.getJSON(contextPath + "/applications/teamwork/messaging/messageAjaxController.jsp",{CM:"READALL"},function(response){
      jsonResponseHandling(response);
      if (response.ok){
        $("#messageListOpener .notificationNumber").fadeOut().html("0");
      }
    });
  });

  function followMessageLink(el) {
    var href = el.find(".message_link a:first").attr("href");
    if (href)
      self.location.href = href;
  }


  function saveThisPageAsPreferred(){
    location.href=contextPath+"/applications/teamwork/tools/manageFavorites.jsp?CM=POSTLINK&URL="+encodeURIComponent(location.href)+"&TITLE="+encodeURIComponent(document.title);
  }

  (function tmStartup() {
    var counted =<%=countedAssignment==null?"undefined":countedAssignment.jsonify()%>;
    //console.debug("counted", counted);
    if (counted) {
      var format = '%0h:%0m:%0s';
      var ndo = $("#tcMenuCounter");
      ndo.attr("title", counted.taskName).data("time", counted.countingStartedMillis).activateTimeCounter(format);
      ndo.parent().show();

    }


    registerEvent("timeCounterEvent",function(e,data){
      //console.debug("timeCounterEvent",data);
      var ndo = $("#tcMenuCounter");
      if(data.type=="start"){
        if (data.response.assignment) {
          ndo.attr("title", data.response.assignment.taskName).data("time", data.response.assignment.countingStartedMillis).activateTimeCounter(format);
          ndo.parent().show();
        }
      } else if (data.type=="stop"){
        ndo.stopTimeCounter();
        ndo.parent().hide();
      }
    })

  })()

</script>

<%
  }
%>
