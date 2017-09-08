<%@ page import="com.twproject.task.Task,
                 com.twproject.waf.TeamworkHBFScreen,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.view.PageState,
                 java.util.ArrayList,
                 java.util.List, java.util.Collections, com.twproject.utilities.TeamworkComparators" %><%


PageState pageState = PageState.getCurrentPageState(request);
pageState.setPopup(true);
if (!pageState.screenRunning) {
  pageState.screenRunning = true;
  final ScreenArea body = new ScreenArea(request);
  TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
  lw.register(pageState);
  pageState.perform(request, response).toHtml(pageContext);
} else {

%>

<div id="ndo" style="position:absolute; display: inline-block; width: 831px;height: 831px; background-color: #ddd"></div>


<script src="glanceIt.js"></script>
<script>
  $(function(){

    //si generano un po di diva a rava
    for (var i=0;i<5;i++){
      for (var j=0;j<5;j++){
        var div=$("<div>").addClass("glanceIt").css({top:i*170,left:j*170});
        $("#ndo").append(div);
      }
    }



    glanceIt($("#ndo"),$(".glanceIt"),function(image,imageIndex){
      image.html(imageIndex);
    })
  })



</script>

<style type="text/css">
  .glanceIt{
    position: absolute;
    width: 150px;
    height: 150px;
    border: 1px solid red;
    font-size: 80px;
    text-align: center;
    line-height: 200px;
  }
</style>


<%}%>
