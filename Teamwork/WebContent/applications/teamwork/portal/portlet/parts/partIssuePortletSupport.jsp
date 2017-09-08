<%@ page import="com.twproject.task.Issue, com.twproject.task.IssueBricks, org.jblooming.waf.html.core.JST, org.jblooming.waf.html.input.ColorValueChooser, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState" %>
<%

    PageState pageState= PageState.getCurrentPageState(request);

    ColorValueChooser cvc = IssueBricks.getStatusChooser("dummy" , "WP_MYISS_STS", pageState);
    cvc.displayValue=false;
    cvc.height=15;
    cvc.width=15;
    cvc.showOpener = false;
    cvc.onChangeScript="myIssChangeStatus(hidden);";
    cvc.init(pageContext);

%>
<style type="text/css">

    <% for (String gr:Issue.getGravities()){ %>

    .issueRow[gravity="<%=gr%>"] td.status{
      background-color: <%=IssueBricks.getGravityColor(gr)%>;
      opacity: .9;
      border-bottom: 1px double #ccc;
      border-top: 1px double #fff;
    }

    <% } %>

</style>


<div id="wp_myInsIssueTemplates" style="display: none;">
    <%---------------------------  MY INSERTED ISSUE TEMPLATE ----------------------------------------------------------%>

    <%=JST.start("MYINSISSLINE")%>
    <tr class="issueRow alternate (#=obj.shouldCloseBy<new Date().getTime()?'expired':''#)" gravity="(#=obj.gravityId#)" issueId="(#=id#)">
      <td class="status" ></td>
      <td style="width: 30px; padding-top: 5px;" valign="top" align="center"><%
        cvc.fieldName="ISSUE_STATUS";
        pageState.addClientEntry("ISSUE_STATUS","(#=obj.statusId#)");
        cvc.label="";
        cvc.toHtml(pageContext);
      %></td>
      <td valign="top" style="max-width: 300px;">
        <div title="(id:(#=id#)) (#=obj.taskName#)" onclick="openIssueEditorInBlack((#=id#));" class="issueDesc" style="overflow: hidden;word-wrap: normal;cursor: pointer">(#=obj.description#)</div>
      </td>
      <td valign="top">
        <%if (I18n.isActive("CUSTOM_FEATURE_ALWAYS_SHOW_TASK_PATH")){
        %><div class="pathSmall">(#=obj.taskPath#)</div><%
        }%>
        <span class="textSmall" title="<%=I18n.get("ISSUE_TASK")%>">(#=obj.taskName#)</span>
      </td>
      <td valign="top" nowrap>
        (#if (obj.assignedById!=null && obj.assignedById!=obj.assigneeId){#)
        <img src="(#=obj.assignedByAvatarUrl#)" align="absmiddle" class="face small" title="(#=obj.assignedByName#)"><span class="teamworkIcon fromTo">:</span>
        (#}#)

        (#if (obj.assigneeId!="-1"){#)
        <img src="(#=obj.assigneeAvatarUrl#)" align="absmiddle" class="face small" title="(#=obj.assigneeName#)"/> <span class="textSmall"></span>
        (#}#)
      </td>
      <td valign="top" nowrap>
        (#if (obj.shouldCloseBy){#)
        <span class="textSmall (#=obj.shouldCloseBy<new Date().getTime()?'warning warningIcon':''#)" title="<%=I18n.get("ISSUE_CLOSE_BY")%>" > (#=new Date(obj.shouldCloseBy).format()#)</span>
        (#}#)
      </td>

      </tr>
    <%=JST.end()%>
</div>

<div id="wp_myIssueTemplates"  style="display: none;">
  <%---------------------------  MY ISSUE TEMPLATE ----------------------------------------------------------%>
  <%=JST.start("MYISSLINE")%>
  <tr class="alternate issueRow (#=obj.shouldCloseBy<new Date().getTime()?'expired':''#)" gravity="(#=obj.gravityId#)" issueId="(#=id#)" assId="(#=obj.assignmentId#)">
    <td class="status"></td>
    <td style="width: 30px; padding-top: 5px;" valign="top" align="center"><%
      cvc.fieldName="ISSUE_STATUS";
      pageState.addClientEntry("ISSUE_STATUS","(#=obj.statusId#)");
      cvc.toHtml(pageContext);
    %></td>
    <td style="max-width: 300px;">
      <div class="issueDesc linkEnabled" style="overflow: hidden;word-wrap: normal;cursor: pointer" title="(id:(#=id#)) (#=obj.taskName#)" onclick="openIssueEditorInBlack((#=id#));">(#=obj.description#)</div>
    </td>
    <td valign="top" class="textSmall columnTaskName">
      <%if (I18n.isActive("CUSTOM_FEATURE_ALWAYS_SHOW_TASK_PATH")){
      %><div class="pathSmall">(#=obj.taskPath#)</div><%
      }%>

      <span  title="<%=I18n.get("ISSUE_TASK")%>">(#=obj.taskName#)</span>
    </td>
    <td valign="top" class="textSmall columnTaskCode"><span title="<%=I18n.get("ISSUE_TASK")%>">(#=obj.taskCode#)</span></td>

    <td valign="top" nowrap>
      (#if (obj.assignedById!=null && obj.assignedById!=obj.assigneeId){#)
        <img src="(#=obj.assignedByAvatarUrl#)" align="absmiddle" class="face small" title="(#=obj.assignedByName#)"><span class="teamworkIcon fromTo">:</span>
      (#}#)

      (#if (obj.assigneeId!="-1"){#)
      <img src="(#=obj.assigneeAvatarUrl#)" align="absmiddle" class="face small" title="(#=obj.assigneeName#)"/> <span class="textSmall"></span>
      (#}#)
    </td>
    <td nowrap>
      (#if (obj.shouldCloseBy){#)
      <span class="textSmall (#=obj.shouldCloseBy<new Date().getTime()?'warning warningIcon':''#)" title="<%=I18n.get("ISSUE_CLOSE_BY")%>"> (#=new Date(obj.shouldCloseBy).format()#)</span>
      (#}#)
    </td>
    <td valign="top" style="padding-top: 5px">
      <span class="button noprint textual icon"  title="<%=I18n.get("ADD_WORKLOG")%>" onclick="btnShowWorklog($(this));return false; "><span class="teamworkIcon">w</span></span>
    </td>
  </tr>
  <%=JST.end()%>
</div>


<script type="text/javascript">

  function myIsslineDecorator (issRow, iss) {
    issRow.find(".linkEnabled").activateTeamworkLinks(true);
  }
  $.JST.loadDecorator("MYINSISSLINE", myIsslineDecorator );
  $.JST.loadDecorator("MYISSLINE", myIsslineDecorator );


  function myIssChangeStatus(el) {
    var issueRow = el.closest("[issueId]");
    var issueId = issueRow.attr("issueId");
    if (el.isValueChanged()) {
      showSavingMessage();

      var request = {CM: "CHISSSTS", OBJID: issueId};
      request[el.prop("name")] = el.val();

      $.getJSON(contextPath + "/applications/teamwork/issue/issueAjaxControllerJson.jsp", request, function (response) {
        jsonResponseHandling(response);
        if (response.ok) {
          el.updateOldValue();
          // eventually shows wl insertion
          if (response.assId) {
            var issueText = "<%=I18n.get("CLOSED_ISSUE_PREFILLED_ACTION")%>" + (issueRow.find("div.issueDesc").text());
            openWorklogEditorPopup(issueRow, {assId:response.assId, text:issueText, issueId:response.issue.id,title:"<%=I18n.get("ISSUE")%>: I#"+ response.issue.id+"#"});
          } else {
            hideWorklogEditorIfYouCan();
          }
          //eventually show notes
          if (response.histId) {
            showNotes(response.histId);
          } else {
            $("#insertStatusNotes").fadeOut();
          }

          $("body").trigger("issueEvent",[{type:"save",response:response}])
        }
        hideSavingMessage();
      });
    }
  }


  function showNotes(histId) {
    $("#floatWindow").append($("#insertStatusNotes"));
    $("#insertStatusNotes").attr("histId", histId).fadeIn();
  }


  function saveStatusNotes() {
    var theDiv = $("#insertStatusNotes");
    var histId = theDiv.attr("histId");
    showSavingMessage();
    var request = {CM: "SVHISTNOTES", histId: histId, notes: theDiv.find("textarea").val()};
    $.getJSON(contextPath + "/applications/teamwork/issue/issueAjaxControllerJson.jsp", request, function (response) {
      jsonResponseHandling(response);
      if (response.ok) {
        $("#insertStatusNotes").fadeOut();
        theDiv.find("textarea").updateOldValue();
      }
      hideSavingMessage();
    });
  }

  function btnShowWorklog(el) {
    var issueRow = el.closest("[issueId]");
    var issueText;
    openWorklogEditorPopup(el,{assId:issueRow.attr("assId"), issueId:issueRow.attr("issueId"),title:"<%=I18n.get("ISSUE")%>: I#"+issueRow.attr("issueId")+"#"});
  }



</script>
