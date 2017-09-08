<%@ page pageEncoding="UTF-8"%><%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.resource.Person, com.twproject.task.Issue,
net.sf.json.JSONArray, org.jblooming.agenda.CompanyCalendar, org.jblooming.oql.OqlQuery, org.jblooming.waf.html.core.JST,
org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState,
java.util.Date, java.util.List, org.jblooming.waf.settings.ApplicationState"%><%

  PageState pageState = PageState.getCurrentPageState(request);

  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
  Person loggedPerson = logged.getPerson();


  String hql = "from " + Issue.class.getName() + " as issue where issue.assignedTo = :myself and issue.task is null " +  //only my to do
      "and (issue.status.behavesAsOpen=true or issue.lastStatusChangeDate>=:dianzi) " + // only open ones or close in 12 hours
      "and (issue.shouldCloseBy is null or issue.shouldCloseBy<=:doman) " +
      " order by issue.status, issue.orderFactorByResource asc";

  OqlQuery oql = new OqlQuery(hql);
  oql.getQuery().setEntity("myself", loggedPerson);
  oql.getQuery().setTimestamp("dianzi", new Date(System.currentTimeMillis() - CompanyCalendar.MILLIS_IN_12_HOUR));
  oql.getQuery().setTimestamp("doman", new Date(System.currentTimeMillis() + CompanyCalendar.MILLIS_IN_DAY));
  List<Issue> todosl = oql.list();

  JSONArray todos= new JSONArray();

  for (Issue t:todosl){
    todos.add(t.jsonify());
  }


    /*PageSeed myTodo = pageState.pageFromRoot("/issue/issueList.jsp");
    myTodo.addClientEntry(Fields.FLD_FILTER_NAME, "PF_MY_OPEN_TODOS");
    myTodo.command= Commands.FIND;*/

%>
<style type="text/css">
  .todoLine{
    border-bottom:1px solid #CCCCCC;
    background: #F7F7F7;
    position: relative;
    min-height: 25px;
    padding:0;
  }

  .todoLine:first-of-type {
    border-top:1px solid #CCCCCC;
  }
  .todoLine.done .body{
    text-decoration:line-through;
  }

  .doIt span.teamworkIcon {
    position:relative;
    cursor:pointer;
  }

  .todoLine.done .body{
    color: #656565

  }

  .confirmBox{
    background-color: #fff
  }

  .todoLine .body {
    word-wrap: break-word;
  }

  .todos.small input.formElements {
    padding: 10px;
    background-image: url("<%=ApplicationState.contextPath%>/img/crossed-bg.png")

  }

  div.issueDrag.dragHandler {
    opacity: .6;
  }


</style>

<div class="portletBox todos small todo-list" style="position: relative; overflow: auto">
  <h1><%=I18n.get("MYISSUES_ROOT")%></h1>


  <div class="container">
    <input type="text" style="font-size:16px;width:100%;margin-bottom: 10px" placeholder="<%=I18n.get("ADD_ISSUE_TEXT_FIELD")%>" onkeypress="saveTodo(event,$(this));" class="formElements">

    <div id="wp_todo_list"></div>

    <div id="todotempl" style="display:none">
      <%=JST.start("TODOLINE")%>
      <div issueId="(#=obj.id#)" assignee="(#=obj.assigneeId#)" gravity="(#=obj.gravityId#)" status="(#=obj.statusId#)" isOpen="(#=obj.isOpen#)" orderByRes="(#=obj.orderByRes#)" class="todoLine (#=obj.isOpen?'':'done'#)">
        <div class="issueDrag dragHandler" style="width:25px;height:95%; opacity:1; position: absolute;left:0"></div>
        <div class="doIt" style="position: absolute;left:30px; top:5px"><span onclick="doUndo($(this));"  class="teamworkIcon">(#=obj.isOpen?'&iexcl;':';'#)</span></div>
        <div style="position: absolute;right:2px; top:5px;">
          <div class="teamworkIcon delete" onclick="removeTodo($(this));" style="cursor:pointer">d</div>
        </div>
        <div class="body" style="padding: 4px 20px 4px 52px">(#=obj.description#)</div>

      </div>
      <%=JST.end()%>
    </div>
  </div>

</div>

<script type="text/javascript">

  //    $(function () {
  $("#todotempl").loadTemplates().remove();
  var ndo=$("#wp_todo_list");
  var todos=<%=todos%>;


  for (var i=0; i<todos.length;i++){
    var todo=$.JST.createFromTemplate(todos[i],"TODOLINE");
    ndo.append(todo);
  }

  $("#wp_todo_list").sortable({
    handle:".dragHandler",
    axis:"y",
//    scroll: false,
    update: function(table, row) {
      var sortedIds = [];
      $(this).children().each(function() {
        sortedIds.push($(this).attr("issueId"));
      });

      var request = {CM:"SORT_ISSUES",SORT_FLAVOUR:"BY_RESOURCE",issues:sortedIds.join(",")};
      showSavingMessage();
      $.getJSON(contextPath + "/applications/teamwork/issue/issueAjaxControllerJson.jsp", request, function(response) {
        jsonResponseHandling(response);
        if (response.ok) {
          //do nothing
        }
        hideSavingMessage();
      });
    },
    start:   function (e, ui) {
      ui.placeholder.height(ui.helper.outerHeight());
    }
  }).disableSelection();
  //});

  function removeTodo(el) {
    if (el.confirm(function() {
      var row = el.closest("[issueId]");
      var request = {CM:"DL",OBJID:row.attr("issueId")};
      showSavingMessage();
      $.getJSON(contextPath + "/applications/teamwork/issue/issueAjaxControllerJson.jsp", request, function(response) {
        jsonResponseHandling(response);
        if (response.ok) {
          row.fadeOut(300, function() {
            $(this).remove();
          });
        }
        hideSavingMessage();
      });
    }));
  }


  function saveTodo(ev,el){
    if (ev.keyCode==13 && el.val()!=""){
      var row = el.closest("[issueId]");
      var request = {CM:"ADDTODO",descr:el.val()};
      showSavingMessage();
      $.getJSON(contextPath + "/applications/teamwork/issue/issueAjaxControllerJson.jsp", request, function(response) {
        jsonResponseHandling(response);
        if (response.ok) {
          if(response.todo){
            var domTodo=insertSorted(response.todo);
            domTodo.effect("highlight", { color: "#F9EFC5" }, 2000);
            el.val("");
          }
        }
        hideSavingMessage();
      });
    }
  }


  function doUndo(el){
    var row = el.closest("[issueId]");

    var request = {CM:"TDDONE",OBJID:row.attr("issueId"),done:row.is(".done")?"no":"yes"};
    showSavingMessage();
    $.getJSON(contextPath + "/applications/teamwork/issue/issueAjaxControllerJson.jsp", request, function(response) {
      jsonResponseHandling(response);
      if (response.ok) {
        if (response.todo){
          row.remove();
          var domTodo=insertSorted(response.todo);
          domTodo.effect("highlight", { color: "#F9EFC5" }, 2000);
        }
      }
      hideSavingMessage();
    });
  }


  function insertSorted(jsTodo){
    var ndo=$("#wp_todo_list");
    var foundOne=false;
    var todo=$.JST.createFromTemplate(jsTodo,"TODOLINE");
    ndo.find("[issueId]").each(function(){
      var row=$(this);
      var isOpen = row.attr("isOpen")=="true";
      if(isOpen<jsTodo.isOpen){
        row.before(todo);
        foundOne=true;
        return false;
      } else if (isOpen==jsTodo.isOpen){
        if (parseInt(row.attr("orderByRes"))>=parseInt(jsTodo.orderByRes)){
          row.before(todo);
          foundOne=true;
          return false;
        }
      }
    });
    if (!foundOne){
      ndo.append(todo);
    }
    return todo;
  }

</script>
