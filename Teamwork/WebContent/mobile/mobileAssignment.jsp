<%--<%@ page import="org.jblooming.waf.html.core.JST" %>--%>
<%--&lt;%&ndash;--------------------------------------------------------  PAGES DEFINITION  -------------------------------------------------------&ndash;%&gt;--%>

<%--&lt;%&ndash; -----------------------------------  my assignment page -------------------------------------- &ndash;%&gt;--%>
<%--<div data-role="page" id="myAssignments" title="<%=I18n.get("MY_ASSIGNMENTS")%>">--%>
  <%--<div data-role="content" class="scroll">--%>
    <%--<div id="assignmentListPlace"></div>--%>
  <%--</div>--%>
<%--</div>--%>


<%--&lt;%&ndash;--------------------------------------------------------  TEMPLATES  -------------------------------------------------------&ndash;%&gt;--%>
<%--<div class="_mobileTemplates">--%>
  <%--&lt;%&ndash; ---------------------------------  MYASSIG_ROW TEMPLATE ------------------------------------------------------------------------------------------------------------------- &ndash;%&gt;--%>
  <%--<%=JST.start("MYASSIG_ROW")%>--%>
  <%--<div data-role="swiper" class="listRow" assigId="(#=id#)" taskId="(#=taskId#)" resId="(#=resId#)">--%>
    <%--<table class="" width="100%" style="padding: 5px 10px" valign="middle">--%>
      <%--<tr>--%>
        <%--<td valign="top" width="80%"><h2>(#=obj.taskName#) <small>((#=obj.taskCode#))</small></h2></td>--%>
        <%--<td valign="top" align="right"><small>(#=obj.roleCode#)</small></td>--%>
      <%--</tr>--%>
    <%--</table>--%>
    <%--&lt;%&ndash; SWIPE BOX &ndash;%&gt;--%>

    <%--<div class="swipeBox">--%>

      <%--&lt;%&ndash;todo: add actions&ndash;%&gt;--%>
      <%--<div class="swipeButton"><span class="teamworkIcon">$</span></div>--%>
      <%--<div class="swipeButton"><span class="teamworkIcon">w</span></div>--%>
      <%--<div class="swipeButton"><span class="teamworkIcon play">a</span></div>--%>

    <%--</div>--%>

  <%--</div>--%>



  <%--<%=JST.end()%>--%>

<%--</div>--%>
<%--&lt;%&ndash;--------------------------------------------------------  DECORATORS  -------------------------------------------------------&ndash;%&gt;--%>
<%--<script>--%>
  <%--$.JST.loadDecorator("MYASSIG_ROW", function(domEl, task) {--%>
    <%--//domEl.find("#progressPlace").append($.JST.createFromTemplate({width:50,perc:task.progress}, "PERCENTILE"));--%>

  <%--});--%>




<%--&lt;%&ndash;--------------------------------------------------------  MY ASSIGNMENT PAGE FUNCTIONS  -------------------------------------------------------&ndash;%&gt;--%>
  <%--function myAssignmentsEnter(event, data, fromPage, isBack) {--%>
    <%--console.debug("myAssignments", data,isBack);--%>
    <%--if (isBack)--%>
      <%--return;--%>
    <%--var page = $(this);--%>
    <%--//page.find("[data-role=title]").html(data.label);--%>

    <%--var ndo = $("#assignmentListPlace");--%>
    <%--ndo.empty();--%>

    <%--var filter = {"CM": "GETMYASSIGNMENTS"};--%>
    <%--$.extend(filter,data);--%>


    <%--callController(filter, function (response) {--%>
      <%--console.debug(response)--%>
      <%--var assCount=0;--%>
      <%--if (response.tasks){--%>
        <%--for (var i = 0; i < response.tasks; i++) {--%>
          <%--updateApplicationCacheElement(applicationCache.tasks, response.tasks[i]);--%>
        <%--}--%>
      <%--}--%>
      <%--if (response.expiredAssignments) {--%>
        <%--for (var i = 0; i < response.expiredAssignments.length; i++) {--%>
          <%--var ass = response.expiredAssignments[i];--%>
          <%--ndo.append($.JST.createFromTemplate(ass, "MYASSIG_ROW"));--%>
          <%--updateApplicationCacheElement(applicationCache.assignments, ass);--%>
          <%--assCount++;--%>
        <%--}--%>
      <%--}--%>
      <%--if (response.assignments) {--%>
        <%--for (var i = 0; i < response.assignments.length; i++) {--%>
          <%--var ass = response.assignments[i];--%>
          <%--ndo.append($.JST.createFromTemplate(ass, "MYASSIG_ROW"));--%>
          <%--updateApplicationCacheElement(applicationCache.assignments, ass);--%>
          <%--assCount++;--%>
        <%--}--%>

        <%--enableSwipe();--%>

      <%--}--%>
      <%--if (assCount<=0) {--%>
        <%--ndo.append($.JST.createFromTemplate({}, "NO_ELEMENT_FOUND"));--%>
      <%--}--%>
    <%--});--%>
  <%--}--%>


  <%--function getAssignmentById(assigId) {--%>
    <%--for (i in applicationCache.assignments) {--%>
      <%--if (applicationCache.assignments[i].id == assigId)--%>
        <%--return applicationCache.assignments[i];--%>
    <%--}--%>
    <%--return false;--%>
  <%--}--%>


<%--</script>--%>
