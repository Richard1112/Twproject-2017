<%@ page import="org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.view.PageState, org.jblooming.utilities.JSP" %>

<%
  response.setHeader("Content-Type", "text/javascript");
  PageState pageState = PageState.getCurrentPageState(request);

  boolean requestMobile = JSP.ex(pageState.getEntry("isMobile"));

  if (false) {%><script><%}%>

/*
*
* TWPROJECT javascript framework
*
*/
var serverURL = "<%=ApplicationState.serverURL%>";
var contextPath = "<%=ApplicationState.contextPath%>";
var buildNumber = "<%=ApplicationState.getBuild()%>";

<%-------------------------------------------------------------------- COMMON DESKTOP AND MOBILE-------------------------------------------------------------------------------------------%>

//************************************************************************************************** utilities.js
<jsp:include page="utilities.js"></jsp:include>


//**************************************************************************************************  date.js -- WARNING: prima di i18nJs.jsp
<jsp:include page="date.js"></jsp:include>

//************************************************************************************************** forms.js
<jsp:include page="forms.js"></jsp:include>

//************************************************************************************************** layout.js
<jsp:include page="layout.js"></jsp:include>

//************************************************************************************************** ajaxFunctions.js
<jsp:include page="ajaxFunctions.js"></jsp:include>

//************************************************************************************************** jquery/JST/jquery.JST.js
<jsp:include page="jquery/JST/jquery.JST.js"></jsp:include>

//************************************************************************************************** jquery/confirm/jquery.confirm.js
<jsp:include page="jquery/confirm/jquery.confirm.js"></jsp:include>

//************************************************************************************************** ../layout/smartCombo/partSmartCombo.js
<jsp:include page="../layout/smartCombo/partSmartCombo.js"></jsp:include>

//**************************************************************************************************  ../layout/colorValueChooser/partColorValueChooser.js
<jsp:include page="../layout/colorValueChooser/partColorValueChooser.js"></jsp:include>

//**************************************************************************************************  ../layout/comboBox/partComboBox.js
<jsp:include page="../layout/comboBox/partComboBox.js"></jsp:include>

//**************************************************************************************************  ../layout/partButtonsOnMouseover.js
<jsp:include page="../layout/partButtonsOnMouseover.js"></jsp:include>

//**************************************************************************************************  ../layout/partDivOnMouseover.js
<jsp:include page="../layout/partDivOnMouseover.js"></jsp:include>

//**************************************************************************************************  ../layout/partUploader.js
<jsp:include page="../layout/partUploader.js"></jsp:include>

//**************************************************************************************************  ../layout/dataTable/partDataTable.js
<jsp:include page="../layout/dataTable/partDataTable.js"></jsp:include>

//**************************************************************************************************  ../layout/dateDurationInput/partDateDurationInput.js
<jsp:include page="../layout/dateDurationInput/partDateDurationInput.js"></jsp:include>

//**************************************************************************************************  ../layout/deletePreviewer/partDeletePreview.js
<jsp:include page="../layout/deletePreviewer/partDeletePreview.js"></jsp:include>

//**************************************************************************************************  jquery/dateField/jquery.dateField.js
<jsp:include page="jquery/dateField/jquery.dateField.js"></jsp:include>

//**************************************************************************************************  jquery/jquery.timers.js
<jsp:include page="jquery/jquery.timers.js"></jsp:include>

//**************************************************************************************************  jquery/uploadize/jquery.uploadize.js
<jsp:include page="jquery/uploadize/jquery.uploadize.js"></jsp:include>

<%if (ApplicationState.platformConfiguration.development) {%>
//**************************************************************************************************  profiling.js
<jsp:include page="profiling.js"></jsp:include>
<%}%>


//************************************************************************************************** ON DOCUMENT LOAD GENERIC

window.onbeforeunload = function(){return alertOnUnload();};
//again because... something does not work.. RB and MB don't remember..


$(":input[oldValue]").livequery(function () {
  $(this).updateOldValue();
});

$(document).on('blur', '.validated', function () {$(this).validateField()});

$('textarea.autosize').livequery(function () {
  $(this).on('keyup focus',enlargeTextArea);
  enlargeTextArea.apply(this,[true]);
});

// Disable caching of AJAX responses */
// set default jquery ajax encoding

$.ajaxSetup({
  cache      : false,
  contentType: "application/x-www-form-urlencoded; charset=UTF-8"
});

<%-------------------------------------------------------------------- DESKTOP ONLY -------------------------------------------------------------------------------------------%>
<% if(!requestMobile){%>

//**************************************************************************************************  dialogs.js
<jsp:include page="dialogs.js"></jsp:include>

//**************************************************************************************************  twprojectLayout.js
<jsp:include page="twprojectLayout.js"></jsp:include>

//**************************************************************************************************  tableLayout.js
<jsp:include page="tableLayout.js"></jsp:include>

//**************************************************************************************************  jquery/jquery.mb.balloon.js
<jsp:include page="jquery/jquery.mb.balloon.js"></jsp:include>

//**************************************************************************************************  jquery/tinyTimers/jquery.tinyTimers.js
<jsp:include page="jquery/tinyTimers/jquery.tinyTimers.js"></jsp:include>

//**************************************************************************************************  jquery/folio/jquery.folio.js
<jsp:include page="jquery/folio/jquery.folio.js"></jsp:include>

//**************************************************************************************************  ../layout/container/partContainer.js
<jsp:include page="../layout/container/partContainer.js"></jsp:include>

//**************************************************************************************************  ../layout/tabSet/partTabSet.js
<jsp:include page="../layout/tabSet/partTabSet.js"></jsp:include>

//**************************************************************************************************  ../layout/hint/partHint.js
<jsp:include page="../layout/hint/partHint.js"></jsp:include>

//**************************************************************************************************  ../layout/loadSaveFilter/partLoadSaveFilter.js
<jsp:include page="../layout/loadSaveFilter/partLoadSaveFilter.js"></jsp:include>

//**************************************************************************************************  ../layout/designer/partDesignerDetail.js
<jsp:include page="../layout/designer/partDesignerDetail.js"></jsp:include>

//**************************************************************************************************  ../scheduler/partScheduleComposer.js
<jsp:include page="../scheduler/partScheduleComposer.js"></jsp:include>

//**************************************************************************************************  ../../applications/teamwork/document/partUrlFileStorage.js
<jsp:include page="../../applications/teamwork/document/partUrlFileStorage.js"></jsp:include>

//**************************************************************************************************  ../../applications/teamwork/parts/woklog/partWorklogEditor.js.jsp
<jsp:include page="../../applications/teamwork/task/worklog/partWorklogEditor.js.jsp"></jsp:include>

//**************************************************************************************************  ../../applications/teamwork/parts/woklog/financial/expenseEditor.js.jsp
<jsp:include page="../../applications/teamwork/task/financial/expenseEditor.js.jsp"></jsp:include>


//************************************************************************************************** ON DOCUMENT LOAD DESKTOP
$(function () {

  jQuery.balloon.init();

  $(".linkEnabled").activateTeamworkLinks(true).emoticonize();
  $(".rightColumnInner").initRightPanel();

  $("#messageListPlace").on("scroll", function (e) {
    if ($(this).scrollTop() + $(this).height() > this.scrollHeight - 200)
      $(this).find(".loadMore .button").click();
    e.stopPropagation();
  });

  /*  Manage ButtonBar fixed*/
  $.buttonBar.init();
  /*  Manage THEAD and TFOOT fixed*/
  $.tableHF.init();

  if ($(".bottomBar").length) {
    $("body").css({paddingBottom: $(".bottomBar").outerHeight()});
  }



  //Prevent drop file on pages by default
  $("body").off("dragover").on("dragover",function (e) {
    e.originalEvent.dataTransfer.dropEffect = 'none';
    e.preventDefault();
    return false;
  }).on("drop",function(e){
    e.preventDefault();
    return false;
  });

  $(window).resize(function(){

    $("body").css({minHeight: $(window).height()});
    var sb = $(".scrollingBox");

    if(sb.length){
      //sb.height($(window).outerHeight()-sb.offset().top - 40).width($(window).outerWidth()-($("body").is(".pinned")?240:10)-sb.offset().left);
      sb.height($(window).outerHeight()-sb.offset().top - 40).width(0).width($(".mainColumn").width());
      $("body").css({overflowY: "hidden"})
    }


    if($("#board").length)
      $("#board").height($(window).height()-$("#board").position().top-25);

    if($(".hScrollingBox").length){
      $(".overflowActive").remove();
      $(".hScrollingBox").children().eq(0).css({paddingRight:0});
      if( $(".hScrollingBox").children().width() > $(".hScrollingBox").width()){
        var overflowActive = $("<div/>").addClass("overflowActive");
        overflowActiveWidth= 30;
        $(".hScrollingBox").children().eq(0).css({paddingRight:overflowActiveWidth});
        overflowActive.css({width:overflowActiveWidth, height:$(".hScrollingBox").outerHeight(), top:$(".hScrollingBox").offset().top, left:$(".hScrollingBox").outerWidth()+$(".hScrollingBox").offset().left-overflowActiveWidth});
        $(".hScrollingBox").before(overflowActive);
      }
    }


  }).resize();


});



<%-------------------------------------------------------------------- MOBILE ONLY -------------------------------------------------------------------------------------------%>
<%} else{%>

//**************************************************************************************************  twprojectLayout.m.js
<jsp:include page="twprojectLayout.m.js"></jsp:include>
<jsp:include page="jquery/calendarPicker/jquery.calendarPicker.js"></jsp:include>
<jsp:include page="jquery/jqueryBaHashchange.js"></jsp:include>

//************************************************************************************************** ON DOCUMENT LOAD MOBILE
//$(function () {});

<%}%>


  <% if (false) {%></script><%}%>
