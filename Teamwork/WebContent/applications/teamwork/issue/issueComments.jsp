<%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8" %>
<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.task.Issue, com.twproject.task.IssueHistory, com.twproject.waf.TeamworkPopUpScreen, net.sf.json.JSONArray, org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.core.JST, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, com.twproject.security.TeamworkPermissions, net.sf.json.JSONObject" %> <%


  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;

    final ScreenArea body = new ScreenArea(request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

    String issueId = pageState.getEntry("issueId").stringValueNullIfEmpty();
    Issue issue = Issue.load(issueId + "");

    if (issue == null)
      return;

    if (!issue.hasPermissionFor(logged, TeamworkPermissions.issue_canRead))
      return;


    JSONArray issHists = new JSONArray();

    for (IssueHistory ih : issue.getComments()) {
      JSONObject ihjs = ih.jsonify();
      ihjs.element("canDelete",false);
      issHists.add(ihjs);
    }

    if (issHists.size()>0){
      JSONObject o = issHists.getJSONObject(issHists.size() - 1);
      if (o.getString("ownerId").equals(logged.getId()+""))
        o.element("canDelete",true);
    }

%>

<h1><%=I18n.get("ISSUE_COMMENTS")%></h1>

<div class="commentList"><table class="table edged" id="issHistTable" ></table></div>
<div class="commEditPlace"></div>

<%----------------------------------------------- WL and NOTES EDITOR ---------------------------------------------------------%>
<jsp:include page="partIssueNotes.jsp"/>


<div style="display: none;" id="issueTemplates">
  <%=JST.start("ISSHISTPL")%>
  <tr histId="(#=obj.id#)" class="issueEditorObjects">
    <td valign="top" nowrap colspan="9">
      <div class="commentHeader">
         <img class="face small" src="(#=obj.ownerAvatarUrl#)">
          <b>(#=obj.lastModifier#)</b> - (#=dateToRelative(new Date(obj.creationDate))#)
      </div>

      <div class="commentBody">
      (#=obj.oldStatusName?"<%=I18n.get("ISSUE_STATUS")%>: "+obj.statusName+"<br>":""#)
      (#=obj.assigneeName?"<%=I18n.get("ISSUE_ASSIGNED_TO")%>: "+obj.assigneeName+"<br>":""#)
      (#=obj.taskName?"<%=I18n.get("ISSUE_TASK")%>: "+obj.taskName+"<br>":""#)
      (#=obj.comment#)
      </div>
      </td><td align="top" style="text-align: center; width:40px;vertical-align:top">
      <%
      ButtonJS delData = new ButtonJS("deleteNotes($(this))");
      delData.enabled=issue.hasPermissionFor(logged,TeamworkPermissions.issue_canWrite);
      delData.confirmRequire = true;
      delData.label = "";
      delData.iconChar = "d";
      delData.additionalCssClass = "delete issHistDelBtn";
      delData.toHtmlInTextOnlyModality(pageContext);
    %>
    </td>
  </tr>
  <%=JST.end()%>
</div>

<script>

  var issueId="<%=issue.getId()%>";
  $(function () {

    showNotesEditor();

    $("#issueTemplates").loadTemplates().remove();

    $.JST.loadDecorator("ISSHISTPL", function (histRow, hist) {
      //console.debug("decorator ISSHISTPL",histRow,hist);
      //if (hist.taskId || hist.assigneeId || hist.statusId || hist.creator!="<%=logged.getFullname()%>" || !hist.last ) {
      if (!hist.canDelete) {
        histRow.find(".issHistDelBtn").html("");
      }
    });

    drawHistory();

  });


  <%-------------------------------------------------------------  NOTES AND HISTORY ------------------------------------------------------------------------%>

  function drawHistory() {
    var issHists =<%=issHists%>;
    //console.debug("drawHistory",issHists);
    var ndo = $("#issHistTable");
    for (var i = issHists.length - 1; i >= 0; i--) {
      issHists[i].last=(i==issHists.length - 1); //in modo da abilitare il delete del messaggio solo sull'ultima riga
      var commentRow = $.JST.createFromTemplate(issHists[i], "ISSHISTPL");
      ndo.prepend(commentRow);
    }
  }


  function showNotesEditor() {
    var commEditor = $("#insertComment").clone(false);
    commEditor.attr("histId", "-1").removeProp("id").show();
    $(".commEditPlace").append(commEditor);
    commEditor.show().find("textarea").focus();
  }


  function saveComment(el) {
    var editor = el.closest("[histId]");
    var histId = editor.attr("histId");
    showSavingMessage();
    var request = {CM: "SVHISTNOTES", histId: histId, notes: editor.find($("textarea")).val(), issueId: issueId};
    $.getJSON(contextPath+"/applications/teamwork/issue/issueAjaxControllerJson.jsp", request, function (response) {
      jsonResponseHandling(response);
      if (response.ok) {
        editor.find("textarea").val("");
        if (response.history) {
          var ndo = $("#issHistTable");
          var histRow = $.JST.createFromTemplate(response.history, "ISSHISTPL");
          ndo.append(histRow);
          histRow.effect("highlight", { color: "#F9EFC5" }, 1500);
          closeBlackPopup();
        }
      }
      hideSavingMessage();
    });
  }


  function deleteNotes(el) {
    var row = el.closest("[histId]");
    var request = {CM: "DLHISTNOTES", histId: row.attr("histId")};
    $.getJSON(contextPath+"/applications/teamwork/issue/issueAjaxControllerJson.jsp", request, function (response) {
      jsonResponseHandling(response);
      if (response.ok) {
        row.remove();
        location.reload();
      }
      hideSavingMessage();
    });
  }

</script>

<%
  }

%>
