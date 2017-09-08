<%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8" %>
<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.task.Issue, com.twproject.task.IssueHistory, com.twproject.waf.TeamworkPopUpScreen, net.sf.json.JSONArray, org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.core.JST, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, org.jblooming.waf.html.input.TextArea, org.jblooming.utilities.JSP, org.jblooming.utilities.StringUtilities" %> <%


  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;

    final ScreenArea body = new ScreenArea(request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {


    //si controlla subito se la chiave è buona se non lo è si esce subito zitti-zitti
    String key=pageState.getEntry("key").stringValue();
    String requesterEmail= StringUtilities.generateEmailFromKey(key);
    if (!JSP.ex(requesterEmail)){
      return;
    }

    //se la issue non è arrivata o non è dell'utente estratto dalla key si esce
    Issue issue = Issue.load(pageState.getEntry("issueId").intValueNoErrorCodeNoExc() + "");
    if (issue == null || !requesterEmail.equals(issue.getExtRequesterEmail()))
      return;


    JSONArray issHists = new JSONArray();
    for (IssueHistory ih : issue.getComments()) {
      issHists.add(ih.jsonify());
    }
%>

<h1><%=I18n.get("ISSUE_COMMENTS")%></h1>

<div class="commentList"><table class="table edged" id="issHistTable" ></table></div>
<div class="commEditPlace"></div>



<%-- ------------------------------------------------------------- COMMENT INSERTION START ----------- è clonato e inserito dopo le righe della history ----------------------------------------------------- --%>
<div id="insertComment" style="display:none" class="notesEditor">
  <%
    TextArea ta= new TextArea(I18n.get("ADD_COMMENT"),"HIS_NOTES","</td><td>",50,2,"style='height:125px;'");
    ta.setAutosize(125,400,15);

    ta= new TextArea("","HIS_COMMENT","",50,2,"");
    ta.script="placeHolder='" + I18n.get("ADD_COMMENT") + "'";
    ta.setAutosize(125,400,15);
    ta.maxlength=0;
    ta.toHtml(pageContext);
  %><div style="margin-top: 10px"><%
  ButtonJS savenotes=new ButtonJS("saveComment($(this))");
  savenotes.label=I18n.get("ADD");
  savenotes.additionalCssClass="first small";
  savenotes.toHtml(pageContext);
%></div>
</div>



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
      delData.enabled=true;
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
      //si toglie il delete se non è inserita dal requester o se non è l'ultima o se è una history vera
      if (hist.taskId || hist.assigneeId || hist.statusId || hist.extRequesterEmail!="<%=requesterEmail%>" || !hist.last ) {
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
    for (var i = issHists.length - 1; i >= 0; i--) { // in ordine inverso
      issHists[i].last=(i==issHists.length - 1);
      ndo.prepend($.JST.createFromTemplate(issHists[i], "ISSHISTPL"));
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
    var request = {CM: "SVHISTNOTES",notes: editor.find($("textarea")).val(), issueId: issueId,key:"<%=key%>"};
    $.getJSON("publicAjaxControllerJson.jsp", request, function (response) {
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
    var request = {CM: "DLHISTNOTES", histId: row.attr("histId"),key:"<%=key%>"};
    $.getJSON("publicAjaxControllerJson.jsp", request, function (response) {
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
