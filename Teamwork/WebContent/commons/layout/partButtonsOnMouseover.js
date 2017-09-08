  var _timerIdForButtonsOMO = new Object();

  function bjs_showButtonsOMO(divId, bjsId) {
    $(".buttonsomo").hide();

    $("#"+divId).show();
    nearBestPosition(bjsId, divId, true);
    if (_timerIdForButtonsOMO[divId] !== null)
      clearTimeout(_timerIdForButtonsOMO[divId]);
  }

  function bjs_hideButtonsOMO(divid) {
    //console.debug("bjs_hideButtonsOMO")
    $("#"+divid).hide();
  }

  function div_hideButtonsOMOWithTimeout(divId) {
    _timerIdForButtonsOMO[divId] = setTimeout("$('#"+divId+"').hide()", 500);
  }

  function div_showButtonsOMO(divId) {
    if (_timerIdForButtonsOMO !== null)
      clearTimeout(_timerIdForButtonsOMO[divId]);
    $("#"+divId).show();
  }
