<%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8" %>
<%@ page import="com.twproject.operator.TeamworkOperator,
                 com.twproject.resource.Resource,
                 com.twproject.waf.TeamworkPopUpScreen,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.html.core.JspHelper,
                 org.jblooming.waf.view.PageState, com.twproject.task.Task, org.jblooming.ontology.PersistentFile, com.twproject.security.TeamworkPermissions" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {

    int taskId=pageState.getEntry("TASK_ID").intValueNoErrorCodeNoExc();
    Task task=Task.load(taskId+"");
    task.testPermission(logged, TeamworkPermissions.task_canWrite);


    String imgUrl=task.bricks.getImageUrl();


%>
<div id="holder" class="uploadizeDrop justOpened imageUploader" style="width: 400px; height: 400px; overflow: hidden;">
  <%if (JSP.ex(imgUrl)){%><img src="<%=imgUrl%>"><%}%>
</div>
<%
    JspHelper imageUploader = new JspHelper("/commons/layout/partImageUploader.jsp");
    imageUploader.parameters.put("imgUrl", imgUrl);
    imageUploader.parameters.put("fixAspectRatio", false);
    imageUploader.toHtml(pageContext);
%>

<script>



function saveImage() {
  var imgData = $("#imageContent").val();
  if (imgData && imgData.length > 1000) {
    showSavingMessage();
    var request = {CM: "SVIMG", "TASK_ID": "<%=taskId%>", imgData: imgData};
    $.ajax("taskAjaxController.jsp", {
      dataType: "json",
      data:     request,
      type:     "POST",

      success: function (response) {
        hideSavingMessage();
        jsonResponseHandling(response);
        if (response.ok) {

          if($("#taskImage img", top.document).length) {
            $("#taskImage img", top.document).attr("src", $("#holder img").attr("src"));
          } else {
            $("#taskImage", top.document).append($("<img>").attr("src", $("#holder img").attr("src")).css({width:"100%"})).css({display:"block"})
          }
          //Reload the page to display the image
          //top.location.reload();

          closeBlackPopup(response);
        }
      }
    });
  } else {
    closeBlackPopup();
  }
}

function deleteImage() {
  showSavingMessage();
  var request = {CM: "RMIMG", "TASK_ID": "<%=taskId%>"};
  $.getJSON("taskAjaxController.jsp", request, function (response) {
    hideSavingMessage();
    jsonResponseHandling(response);
    if (response.ok) {

      if($("#taskImage img", top.document).length) {
        $("#taskImage img", top.document).remove();
        $("#taskImage", top.document).hide();
      }

      //Reload the page to display the image
      //top.location.reload();
      closeBlackPopup(response);
    }
  });
}

</script>


<%
  }
%>


