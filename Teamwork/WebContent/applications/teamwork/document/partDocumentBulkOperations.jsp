<%@ page import="org.jblooming.waf.html.core.JspHelper,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.view.PageState,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.html.button.ButtonJS,
                 com.twproject.task.Issue,
                 org.jblooming.waf.html.input.TagBox,
                 org.jblooming.waf.html.input.SmartCombo,
                 com.twproject.security.TeamworkPermissions,
                 com.twproject.task.TaskBricks, com.twproject.resource.ResourceBricks, com.twproject.resource.Resource, com.twproject.document.TeamworkDocument, org.jblooming.operator.Operator, com.twproject.operator.TeamworkOperator, org.jblooming.waf.html.input.CheckField" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
  JspHelper bulkOp = (JspHelper) JspIncluderSupport.getCurrentInstance(request);

%>


<div id="bulkOp" style="display:none;">

  <div id="bulkRowSel"></div>

  <div><%
    ButtonJS changeTaskAll = new ButtonJS("openBulkAction('moveToTaskAll');");
    changeTaskAll.label = I18n.get("DOCUMENT_MOVE_TO_TASK_ALL");
    changeTaskAll.iconChar = "y";
    changeTaskAll.toHtmlInTextOnlyModality(pageContext);

  %>&nbsp;&nbsp;<%
    ButtonJS changeResAll = new ButtonJS("openBulkAction('moveToResAll');");
    changeResAll.label = I18n.get("DOCUMENT_MOVE_TO_RESOURCE_ALL");
    changeResAll.iconChar = "y";
    changeResAll.toHtmlInTextOnlyModality(pageContext);

  %>&nbsp;&nbsp;<%

  ButtonJS addTagsAll = new ButtonJS(I18n.get("ADD_TAGS"), "openBulkAction('addTagsAll');");
  addTagsAll.iconChar="B";
  addTagsAll.toHtmlInTextOnlyModality(pageContext);

  %></div>

</div>
<%----------------------------------------------- END BULK OPERATIONS ---------------------------------------------------------%>

<%------------------------------------------------ BULK BOXES START ---------------------------------------------------------%>
<div id="moveToTaskAll" style="display:none;" class="bulkData">
  <h1><%=I18n.get("DOCUMENT_MOVE_TO_TASK_ALL")%>
  </h1>
  <table width="100%">
    <tr>
      <td valign='top' width="330" nowrap><%
        SmartCombo tasks = TaskBricks.getTaskCombo("DOCUMENT_MOVE_TO_TASK", false, TeamworkPermissions.document_canCreate, pageState);
        tasks.fieldSize = 35;
        tasks.label = I18n.get("EDITOR_CHOOSE");
        tasks.separator = "<br>";
        tasks.required = true;
        tasks.preserveOldValue = false;
        tasks.toHtml(pageContext);
      %></td>
    </tr>
    <tr>
      <td><br><%
        ButtonJS doMove = new ButtonJS(I18n.get("PROCEED"), "performBulkAction('BULK_MOVE_TO_TASK',$(this));");
        doMove.additionalCssClass = "first";
        doMove.toHtml(pageContext);
      %></td>
    </tr>
  </table>
</div>

<div id="moveToResAll" style="display:none;" class="bulkData">
  <h1><%=I18n.get("DOCUMENT_MOVE_TO_RESOURCE_ALL")%>
  </h1>
  <table width="100%">
    <tr>
      <td valign="top" width="330" nowrap><%
        SmartCombo res = ResourceBricks.getResourceCombo("DOCUMENT_MOVE_TO_RES", TeamworkPermissions.document_canCreate, null, Resource.class,pageState);
        res.label = I18n.get("EDITOR_CHOOSE");
        res.separator = "<br>";
        res.preserveOldValue = false;
        res.fieldSize = 35;
        res.toHtml(pageContext);
      %></td>
    </tr>
    <tr>
      <td><br><%
        doMove = new ButtonJS(I18n.get("PROCEED"), "performBulkAction('BULK_MOVE_TO_RES',$(this));");
        doMove.additionalCssClass = "first";
        doMove.toHtml(pageContext);
      %></td>
    </tr>
  </table>
</div>

<div id="addTagsAll" style="display:none;" class="bulkData">
  <h1><%=I18n.get("ADD_TAGS")%></h1>
  <table width="100%">
    <tr>
      <td valign='top'><%
        TagBox tags = new TagBox("DOCUMENT_TAGS", TeamworkDocument.class,logged.getPerson().getArea());
        tags.label = "";
        tags.toolTip = I18n.get("TAGS_COMMA_SEPARATED");
        tags.separator = "<br>";
        tags.preserveOldValue = false;
        tags.script = "style='width:100%;'";
        tags.toHtml(pageContext);
      %></td>
    </tr>
    <tr><td>
      <%
        new CheckField("REPLACE_EXISTING","&nbsp;",false).toHtmlI18n(pageContext);
      %></td>
    </tr>
    <tr>
      <td><%
        ButtonJS doAddTag = new ButtonJS(I18n.get("PROCEED"), "performBulkAction('BULK_ADD_TAGS',$(this));");
        doAddTag.additionalCssClass = "first";
        doAddTag.toHtml(pageContext);
      %></td>

    </tr>
  </table>
</div>

<%------------------------------------------------ BULK BOXES END ---------------------------------------------------------%>

<script>


  function delRow(el){
    //console.debug("delRow",el)
    var docRow = $(el).closest("[docId]");
    var docId = docRow.attr("docId");
    deletePreview("DOC_DEL", docId, function (response) {  // callback function
      if (response && response.ok) {
        docRow.fadeOut(500, function () {$(this).remove();});
      }
    });
  }


  function getCheckedIds() {
    var ret = [];
    $("#docsTable .selector:checked").each(function () {
      ret.push($(this).closest("[docId]").attr("docId"));
    });
    return ret;
  }

  function performBulkAction(command, el) {
    //console.debug("performBulkAction",command);
    var ids = getCheckedIds();
    if (ids.length > 0) {
      var request = {CM:command,docIds:ids.join(",")};
      if (el) {
        var bulkDiv = el.closest(".bulkData");
        bulkDiv.fillJsonWithInputValues(request);
      }

      showSavingMessage();

      $.getJSON(contextPath+"/applications/teamwork/document/documentAjaxController.jsp", request, function(response) {
        jsonResponseHandling(response);
        if (response.ok) {
          location.reload();
          closeBlackPopup(response);
        }
        hideSavingMessage();
      });

    }
  }

</script>


