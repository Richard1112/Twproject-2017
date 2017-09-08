<%@ page import="com.twproject.security.TeamworkPermissions, com.twproject.task.Issue, com.twproject.task.IssueBricks, com.twproject.task.TaskBricks, com.twproject.task.businessLogic.IssueController, com.twproject.waf.TeamworkHBFScreen, net.sf.json.JSONObject, org.jblooming.page.Page, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.core.JST, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.DataTable, org.jblooming.waf.html.input.ColorValueChooser, org.jblooming.waf.html.input.LoadSaveFilter, org.jblooming.waf.html.input.RadioButton, org.jblooming.waf.html.input.SmartCombo, org.jblooming.waf.html.layout.HtmlColors, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.state.PersistentSearch, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List, org.jblooming.security.License" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    //no command do a default search
    if (pageState.getCommand() == null) {
      if (!PersistentSearch.feedFromDefaultSearch("ISSUEFILTER", pageState))
        // when not set use my issues
        pageState.addClientEntry(Fields.FLD_FILTER_NAME, "PF_MY_OPEN_ISSUES");
      pageState.command = Commands.FIND;
    }


    final ScreenArea body = new ScreenArea(new IssueController(), request);
    body.areaHtmlClass="lreq10 lreqPage";
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {
    Page issues = pageState.getPage();

    String defOrg=pageState.getEntryOrDefault("orgType","STATUS").stringValueNullIfEmpty();

    PageSeed self = pageState.thisPage(request);
    self.command = Commands.FIND;
    Form form = new Form(self);
    form.alertOnChange = true;
    form.id = "issueMultiEditor";
    pageState.setForm(form);
    form.start(pageContext);

    String savedFilterName = pageState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty();

%>

<div class="mainColumn">
<script>$("#ISSUES_MENU").addClass('selected');</script>


<h1 class="filterTitle" defaultTitle="<%=I18n.get("ISSUE_ORGANIZER")%>">
  <%=JSP.ex(savedFilterName)?I18n.get("ISSUE_ORGANIZER")+": "+I18n.get(savedFilterName):I18n.get("ISSUE_ORGANIZER")%>
</h1>


<%----------------------------------------------------------------------------  START FILTER ----------------------------------------------------------------------------%>
<div class="filterBar withButtons clearfix">
  <%
    new JspHelper("/applications/teamwork/issue/partIssueFilter.jsp").toHtml(pageContext);

    LoadSaveFilter lsfb = new LoadSaveFilter("ISSUEFILTER", pageState.getForm());
    lsfb.label = I18n.get("WANT_TO_SAVE_FILTER");
    lsfb.drawEditor = true;
    lsfb.drawButtons = false;
    lsfb.id="issueLSF";

  %>


  <div class="filterButtons">
    <div class="filterButtonsElement filterAdd">
      <span class="button" id="filterSelectorOpener" title="<%=I18n.get("ADD_FILTER")%>" onclick="bjs_showMenuDiv('filterSelectorBox', 'filterSelectorOpener');"><span class="teamworkIcon">f</span></span>
    </div>
    <div class="filterButtonsElement filterSearch"><%ButtonSubmit.getSearchInstance(form, pageState).toHtml(pageContext);%></div>

    <div class="filterActions">
      <div class="filterButtonsElement filterSave"><%lsfb.toHtml(pageContext);%></div>
      <div class="filterButtonsElement filterHelp"><%
        DataTable.getQBEHelpButton(pageState).toHtmlInTextOnlyModality(pageContext);%></div>
    </div>

  </div>
</div>

<script src="<%=request.getContextPath()%>/commons/js/filterEngine.js"></script>
<%----------------------------------------------------------------------------  END FILTER ----------------------------------------------------------------------------%>

<div class="optionsBar clearfix">
  <div class="filterElement">
    <label><%=I18n.get("ISSUE_ORGANIZE_BY")%></label><br>
    <%
      RadioButton rb;
      rb = new RadioButton(I18n.get("TASK"), "orgType", "TASK", "", null, false, "changeCriteria($(this));");
      rb.preserveOldValue=false;
      rb.toHtml(pageContext);

      rb = new RadioButton(I18n.get("ASSIGNEE"), "orgType", "ASSIGNEE", "", null, false, "changeCriteria($(this));");
      rb.preserveOldValue=false;
      rb.toHtml(pageContext);

      rb = new RadioButton(I18n.get("ISSUE_STATUS"), "orgType", "STATUS", "", null, false, "changeCriteria($(this));");
      rb.preserveOldValue=false;
      rb.toHtml(pageContext);

      rb = new RadioButton(I18n.get("ISSUE_GRAVITY"), "orgType", "GRAVITY", "", null, false, "changeCriteria($(this));");
      rb.preserveOldValue=false;
      rb.toHtml(pageContext);

    %>
  </div>
  <div id="adders">
    <div id="TASK_ADDER" class="adder filterElement"><%
      SmartCombo tc = TaskBricks.getTaskCombo("TASK_ADD", false, TeamworkPermissions.issue_canCreate, pageState);
      tc.label="+ "+I18n.get("ADD_COLUMN_OF_TASK")+": ";
      tc.onValueSelectedScript="wantNewElem($('#TASK_ADD').val(),$('#TASK_ADD_txt').val(),'',$('#TASK_ADD_txt').val())";
      tc.preserveOldValue=false;
      tc.separator = "<br>";
      tc.fieldSize = 25;
      tc.toHtml(pageContext);
    %></div>

    <div id="STATUS_ADDER" class="adder filterElement"><label><%="+ "+I18n.get("ADD_COLUMN_OF_STATUS")%>: </label><br><%
      ColorValueChooser stad = IssueBricks.getStatusChooser("STATUS_ADD", "STATUS", pageState);
      stad.onChangeScript="wantNewElem(data.code,data.value,data.color,data.index)";  // last parameter shoud be the position
      stad.preserveOldValue=false;
      stad.height=26;
      stad.showOpener = true;

      stad.toHtml(pageContext);
    %></div>


    <div id="ASSIGNEE_ADDER" class="adder filterElement"><%
      SmartCombo assignee = TaskBricks.getAssignableResourceCombo(null,"ASS_ADD",false,pageState);
      //SmartCombo assignee = ResourceBricks.getResourceCombo("ASS_ADD", TeamworkPermissions.assignment_manage, null, Resource.class, pageState);
      assignee.label="+ "+I18n.get("ADD_COLUMN_FOR_ASSIGNEE")+": ";
      assignee.readOnly = false;
      assignee.separator = "<br>";
      assignee.onValueSelectedScript="wantNewElem($('#ASS_ADD').val(),$('#ASS_ADD_txt').val(),'',$('#ASS_ADD_txt').val())";
      assignee.preserveOldValue=false;
      assignee.fieldSize = 25;
      assignee.toHtml(pageContext);
    %></div>

    <div id="GRAVITY_ADDER" class="adder filterElement"><label><%="+ "+I18n.get("ADD_COLUMN_OF_GRAVITY")%>: </label><br><%
      ColorValueChooser gad = IssueBricks.getGravityChooser("GRAVITY_ADD", "GRAVITY", false, false, pageState);
      gad.onChangeScript="wantNewElem(data.code,data.value,data.color,data.index)";
      gad.preserveOldValue=false;
      gad.height=26;
      gad.toHtml(pageContext);
    %></div>
  </div>
</div>
<%--<div class="hint"><%=I18n.get("ISSUE_ORGANIZER_EXPLAIN")%></div>--%>

<jsp:include page="partIssueNotes.jsp"/>
<%-- --------------------------------------------------------------------  ISSUE TEMPLATE ------------------------------------------------------------------------------------------------------------------- --%>
<div id="templates" style="display:none;background-color:#e5e5ff; width:800px; padding:3px;">
  <%=JST.start("ISSUE")%>
  <div id="##id##" issueId="##id##" task="##taskId##" assignee="##assigneeId##" gravity="##gravityId##" status="##statusId##" class="issue">
    <div class="title dragHandler"></div>
    <div class="rightCol">
      <div class="status"><span style="color:##statusColor##;" class="teamworkIcon">&#xa9;</span>&nbsp;##statusName##&nbsp;&nbsp;</div>
      <span class="code">I###icodid###</span>
      <div class="body">##description##</div>

      <span class="task" title="##taskName##">T###tcodid###</span>
      <span class="assignee" title="##assigneeName##">R###rcodid###</span>
    </div>

  </div><%=JST.end()%>

  <%=JST.start("ISSUE_ZOOM")%>
  <div id="ZOOM_##id##" issueId="##id##" task="##taskId##" assignee="##assigneeId##" gravity="##gravityId##" status="##statusId##" class="issueZoom">
    <div ><h2>I###icodid###</h2></div>
    <div >##descr##</div>
    <div >T###tcodid### - ##taskName##</div>

    <div class="assignee" >R###rcodid### - ##assigneeName##</div>
    <div class="gravity" style="background-color:##gravityColor##"><span>##gravityName##</span></div>
    <div class="status" style="background-color:##statusColor##;"><span>##statusName##</span></div>

  </div><%=JST.end()%>
</div>

<%--  -------------------------------------------------------------------- BARREL AND MAIN TABLE --------------------------------------------------------------------%>
<div id="issuesBarrel" style="display:none; border:3px red dashed; padding:10px; margin:10px; height:50px; overflow:auto;"></div>
<div class="hScrollingBox">
  <table id="board" class="table board fixHead" width="100%" align="center" border="0" cellspacing="0" organizeBy="<%=defOrg%>"></table>
</div>
<%---------------------------------------------------------------------- ISSUE ZOOM --------------------------------------------------------------------%>
<div id="issueZoom" style="display:none;"></div>

<%--  -------------------------------------------------------------------- CSS --------------------------------------------------------------------%>
<style type="text/css">

  .colScrollDiv{
    width: 100%;
    height: 100%;
    overflow-y: auto;
    overflow-x: hidden;
    min-height: calc(100vh - 200px);
    /*
        position: absolute;
        left: 0;
        top: 0;
    */

  }

  .hScrollingBox{
    width: 100%;
    overflow-x: auto;
    overflow-y: scroll;
    /*height: calc(100vh - 250px);*/

  }

  [gravity="<%=Issue.GRAVITY_BLOCK%>"] .title,
  [gravity="<%=Issue.GRAVITY_BLOCK%>"] .title a{
    background-color: <%=IssueBricks.getGravityColor(Issue.GRAVITY_BLOCK)%>;
    color: <%=HtmlColors.contrastColor(IssueBricks.getGravityColor(Issue.GRAVITY_BLOCK))%>
  }

  [gravity="<%=Issue.GRAVITY_CRITICAL%>"] .title,
  [gravity="<%=Issue.GRAVITY_CRITICAL%>"] .title a{
    background-color: <%=IssueBricks.getGravityColor(Issue.GRAVITY_CRITICAL)%>;
    color: <%=HtmlColors.contrastColor(IssueBricks.getGravityColor(Issue.GRAVITY_CRITICAL))%>
  }

  [gravity="<%=Issue.GRAVITY_HIGH%>"] .title,
  [gravity="<%=Issue.GRAVITY_HIGH%>"] .title a{
    background-color: <%=IssueBricks.getGravityColor(Issue.GRAVITY_HIGH)%>;
    color: <%=HtmlColors.contrastColor(IssueBricks.getGravityColor(Issue.GRAVITY_HIGH))%>
  }

  [gravity="<%=Issue.GRAVITY_MEDIUM%>"] .title,
  [gravity="<%=Issue.GRAVITY_MEDIUM%>"] .title a{
    background-color: <%=IssueBricks.getGravityColor(Issue.GRAVITY_MEDIUM)%>;
    color: <%=HtmlColors.contrastColor(IssueBricks.getGravityColor(Issue.GRAVITY_MEDIUM))%>
  }

  [gravity="<%=Issue.GRAVITY_LOW%>"] .title,
  [gravity="<%=Issue.GRAVITY_LOW%>"] .title a  {
    background-color: <%=IssueBricks.getGravityColor(Issue.GRAVITY_LOW)%>;
    color: <%=HtmlColors.contrastColor(IssueBricks.getGravityColor(Issue.GRAVITY_LOW))%>
  }

</style>

</div>

<%---------------------------------------------- RIGHT COLUMN START ---------------------------------------------------------%>

<jsp:include page="partIssueSidebBar.jsp"/>

<%------------------------------------------------ RIGHT COLUMN END ---------------------------------------------------------%>


<%
  form.end(pageContext);


// ----------------------------------------------------------- ALL ISSUES ARE PLACED IN JS MEMORY --------------------------------------------------------------------
  JSONObject jIssues=new JSONObject();
  if (JSP.ex(issues)){
    for (Object[] o : (List<Object[]>)issues.getAllElements()) {
      Issue issue = Issue.load((String) o[0]);//((Object[])iterator.next())[0];
      if (issue==null) {
        continue;
      }
      jIssues.element(issue.getId()+"",issue.jsonify());
    }
  }

  boolean  filterByRes=JSP.ex(pageState.getEntry("FLT_ISSUE_ASSIGNED_TO"));
%>


<%--------------------------------------------------------------------- START SCRIPT ----------------------------------------------------------------------%>
<script type="text/javascript">

var issues=<%=jIssues%>;
var columnsByHand= new Object();

var sortByRes=<%=filterByRes%>;

$(document).ready(function () {
  $("#ISSUES_MENU").addClass('selected');

  $(window).resize(function(){

//    $("#board").height($(window).height() - $("#board").position().top - 25);
    $(".hScrollingBox").width($(window).width()-25)
  }).resize();

  //load templates and remove container
  $("#templates").loadTemplates().remove();

  var barrel = $("#issuesBarrel");
  $.each(issues, function(issueId, issue) {
    issue.dom = $.JST.createFromTemplate(issue, "ISSUE").appendTo(barrel).activateTeamworkLinks(true).emoticonize()
  });


  $("div.issue").click(function() {
    zoomIssue($(this));
  });

  changeCriteria($("[name=orgType][value=<%=defOrg%>]"));

  //issue update events from black popup editor
  registerEvent("issueEvent", function (e, data) {
    //console.debug("issueEvent",data)
    if (data.type=="delete") {
      //console.debug("issueRemoved");
      issues[data.response.deletedId].dom.fadeOut(1000, function () {
        delete issues[data.response.deletedId];
        $(this).remove();
      });
    } else if (data.type=="save"&&data.response.issue) {
      //remove the boxino
      var issue=data.response.issue;
      $("#"+issue.id).remove();
      //re-create the box
      issue.dom = $.JST.createFromTemplate(issue,"ISSUE").appendTo($("#issuesBarrel")).draggable({revert: 'invalid'}).activateTeamworkLinks(true).emoticonize();
      //update the issue in memory
      issues[issue.id]=issue;
      //re-arrange the issue
      spreadIssues();
    }
  });
});

function changeCriteria(el){
  $("#board").attr("organizeBy",el.val());
  $("#adders .adder").hide();
  $("#"+el.val()+"_ADDER").show();
  spreadIssues();
}


function spreadIssues(){
  var board = $("#board");
  var organizeBy = board.attr("organizeBy");
  if (!organizeBy)
    organizeBy = "STATUS";

  var orderedIssues=[];
  $.each(issues,function(issueId,issue){
    //first move all issues from board to barrel
    issue.dom.appendTo("#issuesBarrel");
    orderedIssues.push(issue);
  });


  //sort issues
  orderedIssues.sort(function(a,b){
    var ret= a.gravityOrder-b.gravityOrder;
    if (ret==0){
      ret= sortByRes?a.orderByRes- b.orderByRes:a.orderByTask- b.orderByTask;
    }
    return ret;
  });

  //remove all columns from board
  board.find("tr, thead").remove();

  // create an object for columns
  var cols= new Object();

  //add columns to table
  var trheader= $("<thead/>");
  var trh = $("<tr>").prop("id","trHead").height(20);
  trheader.append(trh);
  var tr=$("<tr>").prop("id","trBody");

  var prop=organizeBy.toLowerCase();

  //and now loop for every issue and move it
  $.each(orderedIssues,function(i,issue){
    //find the column
    var col = cols[issue[prop+"Id"]];
    if (!col){
      // data for building the column
      var value = {id:issue[prop+"Id"],name:issue[prop+"Name"],color:issue[prop+"Color"],order:issue[prop+"Order"]};
      col=appendColumn(trh,tr,value,organizeBy);
      cols[value.id]=col;
    }
    col.find(".colScrollDiv:first").append(issue.dom);
  });

  // then add byHand columns
  var addColsByType = columnsByHand[organizeBy];
  if (addColsByType){
    $.each(addColsByType,function(orgBy,colValue){
      //add if not already there
      if (!cols[colValue.id])
        appendColumn(trh,tr,colValue,organizeBy);
    });
  }

  //append rows
  board.append(trheader);
  board.append(tr);
}

function appendColumn(trh,tr,value,organizeBy){
  var th=$("<th>").html(value.name);
  if (value.color){
    th.css("background-color",value.color);
  } else {
    th.css("background-color","#EEEEEE");
  }
  th.css("border","1px solid #CCC");
  th.attr("order",value.order);


  //append the column in the right position
  var appended=false;
  trh.children().each(function(){
    if ($(this).attr("order")>value.order){
      appended=true;
      $(this).before(th);
      return false;
    }
  });
  if (!appended)
    trh.append(th);


  var td = $("<td>").attr("colId", value.id).addClass("col").html("").attr("order",value.order?value.order:0);
  var div=$("<div>").addClass("colScrollDiv sortableCol");
  td.append(div);
  //td.sortable ({
  div.sortable ({
    connectWith:".sortableCol",
    stop:  function (ev, ui) {
      updateIssue(ui.item);
    },
    over: function(event, ui) {
      if (ui.sender) {
        widget = ui.sender.data("ui-sortable");
        widget.scrollParent = $(this);
        widget.overflowOffset = $(this).offset();
        $(".sortableCol").height($(".col").height());
      }
    }

  }).disableSelection();

  td.css("border-left","1px solid #CCC");

  //append the column in the right position
  var appended=false;
  tr.children().each(function(){
    if ($(this).attr("order")>value.order){
      appended=true;
      $(this).before(td);
      return false;
    }
  });
  if (!appended)
    tr.append(td);

  return td;
}


function updateIssue(issue){
  // console.debug("dropped issue:" + issue.prop("id") + " on col:" + column.attr("colId"));

  var column=issue.closest("td");
  var board = $("#board");
  var organizeBy = board.attr("organizeBy");

  if (<%=!License.assertLevel(10)%>) return;

//  console.debug(organizeBy, column.attr("colid"), issue.attr(organizeBy));

  //update the issue value on dom
  issue.attr(organizeBy,column.attr("colId"));

  showSavingMessage();


  // dato che se si ordina la nuova gravità è data dalle issue vicine si calcola la nuova gravità
  //compute new gravity
  var prevIssue = issue.prev();
  var nextIssue = issue.next();
  var oldG = issue.attr("gravity");
  var newG = oldG;

  var prevG = prevIssue.attr("gravity");
  var nextG = nextIssue.attr("gravity");

  //move to bottom
  if (nextG == undefined) {
    newG = prevG > oldG ? oldG : prevG;

    //move to top
  } else if (prevG == undefined) {
    newG = nextG < oldG ? oldG : nextG;

    //move in the middle
  } else {
    //not compatible place
    if (!(nextG < oldG && prevG > oldG)) {
      newG = nextG;
    }
    // else do nothing
  }
  issue.attr("gravity",newG);

  var issueId = issue.prop("id");
  var request={
    CM:("STATUS"==organizeBy?"CHISSSTS":"SV"),
    OBJID:issueId,
    ISSUE_STATUS:issue.attr("status"),
    ISSUE_TASK:issue.attr("task"),
    ISSUE_GRAVITY:issue.attr("gravity"),
    ASSIGNEE:issue.attr("assignee")
  };

  //se possono essere sortabili
  var sortFlavour;
  if ($("#FLT_ISSUE_ASSIGNED_TO").val()) {
    sortFlavour = "BY_RESOURCE";
  } else if ($("#FLT_ISSUE_TASK").val()) {
    sortFlavour = "BY_TASK";
  }
  if (sortFlavour) { <%--si passano solo quelle della stessa gravità dato che sono sempre ordinate gravità + ordinamento manuale--%>
    var sortedIds = [];
    column.find("[issueId][gravity="+newG+"].issue").each(function (i) {
      sortedIds.push($(this).attr("issueId"));
    });
    request.SORT_FLAVOUR= sortFlavour;
    request.issues= sortedIds.join(",");
  }


  $.getJSON("issueAjaxControllerJson.jsp", request,function(response){
    jsonResponseHandling(response);
    if (response.ok){
      //refresh the object
      var iss=response.issue;
      //inject the new html in the old issue
      issue.html($.JST.createFromTemplate(iss,"ISSUE").activateTeamworkLinks(true).emoticonize().html());
      //set dom object in the new json issue
      iss.dom=issue;
      //update issue collection in memory
      issues[issueId]=iss;

      // eventually shows wl insertion
      if (response.assId) {
        var issueText = issueText = issue.find("div.body").text().trim().substring(0, 50);
        openWorklogEditorPopup(issue,{assId:response.assId,text: issueText, issueId:issueId,title:"<%=I18n.get("ISSUE")%>: I#"+issueId+"#"});
      } else {
        hideWorklogEditorIfYouCan();
      }


      //eventually show notes
      if(response.histId){
        showNotes(response.histId);
      } else {
        $("#insertHistNotes").fadeOut();
      }
    }
    hideSavingMessage();

  });

}


function saveStatusNotes(){
  var theDiv=$("#insertStatusNotes");
  var histId=theDiv.attr("histId");
  showSavingMessage();
  var request= {CM:"SVHISTNOTES",histId:histId,notes:theDiv.find("textarea").val()};
  $.getJSON("issueAjaxControllerJson.jsp",request,function(response){
    jsonResponseHandling(response);
    if (response.ok) {
      $("#insertStatusNotes").fadeOut();
      theDiv.find("textarea").updateOldValue();
    }
    hideSavingMessage();
  });
}


function showNotes(histId){
  $("#insertStatusNotes").attr("histId",histId).fadeIn();
}


function wantNewElem(id,label,color,order){
  var board = $("#board");
  var organizeBy = board.attr("organizeBy");
  if (label && id){
    if (board.find("td[colId="+id+"]").size()==0 ){
      // use the matching array
      var colValue = {id:id,name:label,color:color,order:order};
      var colsValues=columnsByHand[organizeBy];
      if (!colsValues){
        colsValues=new Array();
        columnsByHand[organizeBy]=colsValues;
      }
      colsValues.push(colValue);
      spreadIssues();
      //changeCriteria($("[name=orgType][value=<%=defOrg%>]"));
    }
  }
}

var oldOpenedIssueBox= null;
function zoomIssue(issueBox){
  //console.debug("zoomIssue",issueBox,issueBox.attr("issueId"));
  var ndo=$("#issueZoom");
  var issueId = issueBox.attr("issueId");

  if(!ndo.is(":visible") || issueId != oldOpenedIssueBox){
    removeZoom(issueBox);
    oldOpenedIssueBox = issueId;
    ndo.empty().append($.JST.createFromTemplate(issues[ issueId],"ISSUE_ZOOM")).show();
    setTimeout(function(){
      $(document).on("click.issueBox", function(e){
        if(!$(e.target).is("#issueZoom")){
          removeZoom(issueBox);
        }
      });
    },200)
  } else {
    oldOpenedIssueBox = null;
    removeZoom(issueBox);
  }
}

function removeZoom(issueBox){
  $("#issueZoom").hide().empty();
  $(document).off("click.issueBox");
}

function addIssue() {
  var editUrl=contextPath+"/applications/teamwork/issue/issueEditor.jsp?CM=AD";
  openBlackPopup(editUrl, 1024, $(window).height() - 50);
}


</script>
<%
  }
%>
