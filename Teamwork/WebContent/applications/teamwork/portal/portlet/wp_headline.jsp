<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.security.TeamworkPermissions, org.jblooming.waf.view.PageState" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
%>


<div class="wpHeadLineContainer">
  <div class="wpHLBox-wrapper">
    <div id="wphMilesOver" class="wpHLBox wpHLGrave waiting"></div><div id="wphTasksOver" class="wpHLBox wpHLGrave waiting"></div><div id="wphIssuesOver" class="wpHLBox wpHLGrave waiting"></div><div id="wphProjectBudgetOverflow" class="wpHLBox wpHLGrave waiting"></div><div id="wphMilesForthcoming" class="wpHLBox waiting"></div><div id="wphTasksForthcoming" class="wpHLBox waiting"></div><div id="wphIssuesForthcoming" class="wpHLBox waiting"></div><div id="wphOpenProjects" class="wpHLBox waiting"></div><div id="wphWorklog" class="wpHLBox waiting"></div><div id="wphTaskDiscussions" class="wpHLBox waiting"></div><%if (logged.hasPermissionFor(TeamworkPermissions.task_canRead)) {%><div id="wphProjectCreated" class="wpHLBox waiting"></div><div id="wphProjectClosed" class="wpHLBox waiting"></div><div id="wphAssigCreated" class="wpHLBox waiting"></div><%}%></div>
  <!-- Add Arrows -->
  <div class="wpHLBox-next" style="display: none"></div>
  <div class="wpHLBox-prev" style="display: none"></div>
</div>

<style>

  .wpHeadLineContainer{
    position: relative;
    display: block;
    width: 100%;
    vertical-align: top;
    padding: 0;
    background: #356A8C;
    /*background: #42789B;*/
  }

  .wpHLBox-wrapper {
    position: relative;
    overflow:hidden;
    width: 100%;
    opacity: 1;
    white-space: nowrap;
  }

  .wpHLBox{
    min-width: 280px;
    height: 75px;
    border-radius: 4px;
    display: inline-block;
    margin: 10px 6px;
    position: relative;
    vertical-align: top;
    text-align: left;
    font-size: 18px;
    opacity:1;
  }

  .wpHLBox:first-of-type{
    margin-left: 8px;
  }


  .wpHLBox:last-of-type{
    margin-right: 8px;
  }


  .wpHLBox.waiting{
    opacity:0;
  }

  .wpHLBox a {
    display: block;
    position: relative;
    height: 100%;
    padding: 7px 15px;
    background: rgba(255, 255, 255, 0.1);
    border-radius: 4px;
    color:#fff;
    overflow: hidden;

  }

  .wpHLBox:hover a{
    text-decoration: none;
    color: #2F97C6 !important;
    background-color:#fff;
  }

  .wpHLBox.wpHLGrave:hover a .wphlNumber{
      border-color: transparent;
  }

  .wpHLBox h2{
    font-size: 20px;
    margin: 0;
    padding-right: 50px;
    text-overflow: ellipsis;
    overflow: hidden;
    font-weight: 300;
  }

  .wpHLBox p{
    font-size: 11px;
    white-space: normal;
    margin: 0;
    padding: 2px 0 0;
  }

  .wpHLBox .wphlNumber{
    font-weight: 300;
    position:absolute;
    right: 5px;
    top: 12px;
    min-width: 25px;
    padding: 0 6px;
    line-height: 23px;
    text-align: center;
    font-size: 35px;
  }

  /*.wpHLBox.wpHLGrave .wphlNumber {
      color: rgb(254, 255, 244);
      background-color: rgba(255, 36, 26, 0.85);
      height: 100%;
      padding-top: 10px;
      padding-right: 17px;
      margin-right: -20px;
      top:0
  }*/

  .wpHLBox.wpHLGrave a{
      padding-left: 17px;
  }

  .wpHLBox.wpHLGrave a:before {
      content: "";
      position: absolute;
      width: 10px;
      height: 100%;
      left: 0;
      top:0;
      background-color: rgba(255, 36, 26, 0.85);
  }


/*  .wpHLBox.wpHLGrave .wphlNumber:before {
      font-family: "icons" !important;
      content: "!";
      color: red;
      font-size: 70%;
      border-radius: 40px;
      background-color: #fff;
  }*/

  .wpHLBox-next {
    position: absolute;
    top:0;
    right: 0;
    width: 80px;
    height: 100%;
    cursor: pointer;
    background-image: linear-gradient(to right, rgba(53, 106, 140,0) 25%, #356B8C 50%, #356B8C 100%);
  }

  .wpHLBox-prev {
    position: absolute;
    top:0;
    left: 0;
    width: 80px;
    height: 100%;
    cursor: pointer;
    background-image: linear-gradient(to right, #356B8C 25%, #356B8C 50%, rgba(53, 106, 140,0) 100%);
  }
  .wpHLBox-prev:after,
  .wpHLBox-next:after{
      cursor: pointer;
      transition: all .5s;
      font-family: "icons";
      position: absolute;
      font-size: 200%;
      color: #fff;
      top: 0;
      bottom: 0;
      margin: auto;
      width: 20px;
      height: 20px;
  }
  .wpHLBox-next:after {
    content: "}";
    right: 10px;
  }

  .wpHLBox-prev:after {
    content: "{";
    left: 10px;
  }

  .wpHLBox-prev:hover:after, .wpHLBox-next:hover:after {
    color: #ffffff;
  }

  @media only screen and (max-width: 1024px) {


    .wpHLBox .wphlNumber {
      font-size: 30px;
    }
    .wpHLBox h2 {
      padding-right: 55px;
    }

  }


    .neg .wpHLBox a {
        background-color: #fff;
    }

    .neg .wpHLBox a{
        color: #444;
    }

    .neg .wpHLGrave .wphlNumber {
        color: #ff5524;
    }
    .neg .wpHLBox:hover a {
        background-color: rgba(255, 255, 255, 0.8);
    }

</style>

<script>

  if ($("#portletList").size()<=0)
    $(".wpHeadLineContainer").insertBefore("#twInnerContainer");


  var killEmpty=function(){
    var el=$(this);
    if (el.text().length<10){
      $(this).remove();
    } else {
      el.removeClass("waiting");
    }
  };

  $.fn.slideHeadLine = function(){

    var $headLine = $(this);
    var headLine = $headLine[0];
    var $headLineSlider = $(".wpHLBox-wrapper", $headLine);

    var next = $(".wpHLBox-next", $headLine);
    var prev = $(".wpHLBox-prev", $headLine);
    var elementWidth = $(".wpHLBox", $headLineSlider).outerWidth();

    var headLineWidth = $headLineSlider.outerWidth();
    var headLineLeft = $headLineSlider.scrollLeft();

    next.on("click", function(){
      var step = headLineLeft + headLineWidth - elementWidth;
      var isAtTheEnd = ( (headLineLeft + headLineWidth) >= $headLineSlider[0].scrollWidth-10);
      step = isAtTheEnd ? $headLineSlider[0].scrollWidth : step;

      $headLineSlider.animate({scrollLeft: step},500, checkPrevNextVisibility);
    });

    prev.on("click", function(){
      var step = headLineLeft - headLineWidth + elementWidth;
      var isAtTheStart = ( headLineLeft <= 10 );
      step = isAtTheStart ? 0 : step;

      $headLineSlider.animate({scrollLeft: step},500, checkPrevNextVisibility);
    });

    var checkPrevNextVisibility = function() {

      headLineWidth = $headLine.width();
      headLineLeft = $headLineSlider.scrollLeft();

      if ( (headLineLeft + headLineWidth) >= $headLineSlider[0].scrollWidth-10){
        next.fadeOut();
      }else{
        next.fadeIn();
      }

      if ( headLineLeft <= 0 ){
        prev.fadeOut();
      } else {
        prev.fadeIn();
      }
    };

    $(window).on("resize.headLine", checkPrevNextVisibility).resize();

  };

  $(function(){

    setTimeout(function(){
      $(".wpHeadLineContainer").slideHeadLine();
    },1000);

  });

  $(".wpHeadLineContainer .wpHLBox").each(function(i){
    var box = $(this);
    box.load(contextPath+"/applications/teamwork/portal/portlet/parts/partWPHeadline.jsp",{CM:box.prop("id"),"_":new Date().getTime()+"."+i},killEmpty)
  });
</script>

