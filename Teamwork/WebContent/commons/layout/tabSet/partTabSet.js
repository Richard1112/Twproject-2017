
function clickTab(tabId) {
  //si toglie il selected a tutti a si rimette a lui
  $(".tabSelected:not(#"+tabId+")").removeClass("tabSelected");
  var tab = $("#"+tabId);
  tab.addClass("tabSelected");

  //si setta l'hidden value
  $(":input[name="+tab.attr("tabset")+"]").val(tabId);

  //si nascondono tutti i tabBox tranne e si visualizza lui
  $(".tabBox:not([tabId="+tabId+"])").hide();
  $(".tabBox[tabId="+tabId+"]").show();

	$(window).resize();
}

$(document).ready(function() {
  // show tabset with errors
  var errSpan = $("span.errImg");
  if (errSpan.length > 0) {
    var theTabDiv = errSpan.eq(0).closest(".tabBox");
    if (theTabDiv.length > 0)
      clickTab(theTabDiv.attr("tabId"));
  }
});
