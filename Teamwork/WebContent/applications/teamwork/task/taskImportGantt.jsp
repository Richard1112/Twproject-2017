<%@ page
  import="com.twproject.security.TeamworkPermissions,
          com.twproject.waf.TeamworkHBFScreen,
          org.jblooming.operator.Operator,
          org.jblooming.waf.ScreenArea,
          org.jblooming.waf.html.button.ButtonJS,
          org.jblooming.waf.html.input.TextField,
          org.jblooming.waf.html.state.Form,
          org.jblooming.waf.settings.ApplicationState,
          org.jblooming.waf.settings.I18n,
          org.jblooming.waf.view.PageSeed,
          org.jblooming.waf.view.PageState" %>
<%

  //String GANT_ONLINE_SERVICE_URL = ApplicationState.platformConfiguration.development ? "http://olpc009" : "http://gantt.twproject.com";
  String GANT_ONLINE_SERVICE_URL = "https://gantt.twproject.com";

  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);

  } else {


    Operator logged = pageState.getLoggedOperator();

    logged.testPermission(TeamworkPermissions.project_canCreate);


  %> <h1><%=I18n.get("IMPORT_FROM_JSON")%></h1>

<hr>
<span class="hint">I have the code:</span>
<code class="import"> e.g.:M-259019F992532F45503EFB5BB16D0C4F53BF03240A6E604B31462322C0924E39</code>
<%
  PageSeed self=pageState.thisPage(request);
  self.mainObjectId = pageState.mainObjectId;
  self.setCommand("GETJSONFROMGANTT");
  Form f = new Form(self);
  f.id = "GNTFRM";
  pageState.setForm(f);
  f.start(pageContext);

%>
<br>

  <table class="table">
    <tr>
      <td nowrap valign="top" colspan="2"><%

        TextField code = new TextField("TWGANTT_CODE", "<br>");
        code.fieldSize = 60;
        code.label = I18n.get("TWGANTT_CODE");
        code.required = true;
        code.toHtml(pageContext);

      %><hr></td>
      </tr><tr>
      <td id="impBtn" style="width: 1%;white-space: nowrap"><%
        ButtonJS start = new ButtonJS("importFromService($(this));");
        //start.id = "btnStart";
        start.label = I18n.get("GANTT_IMPORT_ONE") + "<br><span class='textSmall'>" + I18n.get("GANTT_IMPORT_ONE_HELP") + "</span>";
        start.additionalCssClass = "big first";
        start.style = "height:50px;line-height: 130%";
        start.toHtml(pageContext);
      %></td><td><%


        ButtonJS startAll = new ButtonJS("importFromService($(this),true);");
        //startAll.id = "btnStart";
        startAll.label = I18n.get("GANTT_IMPORT_ALL") + "<br><span class='textSmall'>" + I18n.get("GANTT_IMPORT_ALL_HELP") + "</span>";
        startAll.additionalCssClass = "big first";
        startAll.style = "height:50px;line-height: 130%";
        startAll.toHtml(pageContext);
    %></td>

    </tr>
    <tr>
      <td id="importStep2" colspan="3" style="display: none;">
        <h3><%=I18n.get("PROJECTS_TO_IMPORT")%>:</h3><br>

        <div id="prjList"></div>
        <br>
        <%new ButtonJS("PROCEED", "proceedImport($(this));").toHtmlI18n(pageContext);%>
        <%new ButtonJS("RESET", "cancelImport($(this));").toHtmlI18n(pageContext);%>
      </td>
    </tr>
    <tr><td id="importResults" colspan="3"></td></tr>
  </table>



<%f.end(pageContext);%>



<script>

  var projectsToImport;

  $("#TASK_MENU").addClass('selected');


  function importFromService(el,getAll) {
    if (canSubmitForm("GNTFRM")) {
      $("#impBtn").hide();
      var prjKey = $("#TWGANTT_CODE").val();
      showSavingMessage();
      $.ajax({
        url:     "<%=GANT_ONLINE_SERVICE_URL%>/data/" + prjKey + (getAll ? "/all" : ""),
        dataType:"jsonp",
        jsonp:   "__jsonp_callback",
        success: function (response) {
          jsonResponseHandling(response);
          if (response.ok) {
            $("#impBtn");
            projectsToImport = response.projects;
            var ndo = $("#prjList");
            for (var i = 0; i < projectsToImport.length; i++) {
              ndo.append("<h3>" + (i + 1) + ") " + projectsToImport[i].tasks[0].name + "</h3>");
            }
            $("#importStep2").show();
          }
          hideSavingMessage();

        }
      });
    }
  }

  function cancelImport(el) {
    //$("#TWGANTT_CODE").val("");
    $("#uploadJsonFile,#impBtn").fadeIn();
    $("#prjList").empty();
    $("#importStep2").hide();
    projectsToImport = undefined;
  }

  function proceedImport(el) {
    if (projectsToImport) {
      showSavingMessage();
      $.ajax("gantt/ganttAjaxController.jsp", {
        dataType:"json",
        data:    {CM:"IMPORTMULTIPROJECTS", prjs:JSON.stringify(projectsToImport)},
        type:    "POST",

        success:function (response) {
          hideSavingMessage();
          //console.debug(response);
          var ndo = $("#importResults");
          ndo.empty();
          jsonResponseHandling(response);
          if (response.ok) {
            $("#importStep2,#impBtn").hide();
            if (response.projects && response.projects.length > 0) {
              // procede to the editor: create a button
              ndo.append("<br><h2><%=I18n.get("PROJECTS_CORRECTLY_IMPORTED")%>:</h2><br>");
              for (var i=0;i<response.projects.length;i++){
                var task = response.projects[i].tasks[0];
                ndo.append("<a href='taskOverview.jsp?CM=ED&TASK_ID=" + task.id + "' ><h3>"+(i+1)+") "+ task.name + "</h3></a>");
              }
            } else {
              ndo.append("<h2><%=I18n.get("PROJECT_SEEMS_EMPTY")%></h2>");
            }
          }
        }

      });
    }
  }

</script>
<%

  }
%>
