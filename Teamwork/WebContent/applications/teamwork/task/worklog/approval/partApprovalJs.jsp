<%@ page import="org.jblooming.waf.constants.Commands, org.jblooming.waf.view.PageState" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
%>

<script type="text/javascript">
  $(document).ready(function () {

    $("#multi :checkbox").bind("click", function () {refreshBulk($(this))});
    $("#multi").find(":text.wlTime").bind("blur", saveWL);
    $("#multi").find(":text.wlDescr").bind("blur", saveDescr);

    $("#multi table.isDrag").draggable({
      start:    function () {$("body").unselectable();},
      end:      function () {$("body").clearUnselectable();},
      revert:   'invalid',
      cursor:   "crosshair",
      distance: 10,
      zIndex:   10000,
      axis:     "x"
    });

    $("#multi .cellDrop").each(function (i) {
      var resId = $(this).parents("tr[assId]:first").attr("assId");
      var accFil = ".wldrag" + resId;
      $(this).droppable({
        hoverClass: "dropOver",
        accept:     accFil,
        tolerance:  "pointer",
        drop:       function (event, ui) {
          var dragged = ui.draggable;
          dragged.appendTo($(this));
          dragged.attr("style", "position:relative");
          updateDataAfterDrop($(this), dragged);
        }
      });
    });

    refreshTotals();

  });

  function goToMillis(time) {
    $("[name=FOCUS_MILLIS]").val(time);
    $("#<%=pageState.getForm().id%>").submit();
  }

  function refreshTotals() {
    $("#multi").find("th.dayHeader[day]").each(function (i) {refreshColumn($(this).attr("day"));});
  }

  function refreshColumn(day) {
    var tot = 0;
    $("td[day=" + day + "]").find(":text.wlTime").each(function () {
      var mill = millisFromHourMinute($(this).val());
      tot = tot + (mill ? mill : 0);
    });
    $("td[day=" + day + "].tdTotal").text(getMillisInHoursMinutes(tot));
  }

  function rowSel(ck) {
    var tr = $(ck).parents("tr").eq(0);
    tr.find(":checkbox").prop('checked', $(ck).prop('checked'));
  }

  function selAll(ck) {
    $("#multi :checkbox").prop('checked', $(ck).prop('checked'));
  }

  function colSel(ck) {
    var day = $(ck).parents("th.dayHeader:first").attr("day");
    var plan = $("#multi");
    plan.find("td[day=" + day + "]").find(":checkbox").prop('checked', $(ck).prop('checked'));
  }

  function updateDataAfterDrop(dropEl, dragEl) {
    showSavingMessage();
    var newDay = dropEl.attr("day");
    var wlId = dragEl.attr("wlId");
    $.getJSON("approvalAjaxController.jsp", "<%=Commands.COMMAND%>=CHANGEDAY&" + "wlId=" + wlId + "&day=" + newDay, function (response) {
      jsonResponseHandling(response);
      if (response.ok) {
        refreshTotals(newDay);
      }
      hideSavingMessage();
    });
  }

  function removeWorklog(butt, wlId) {
    showSavingMessage();
    var day = $(butt).parents("td[day]:first").attr("day");
    $.getJSON("approvalAjaxController.jsp", {CM: "REMOVEWL", wlId: wlId}, function (response) {
      jsonResponseHandling(response);
      if (response.ok) {
        //remove the line
        $(butt).parents("table[wlId]:first").remove();
        refreshColumn(day);
      }
      hideSavingMessage();
    });
  }

  function saveWL() {
    //console.debug("saveWL")
    var theEl = $(this);
    //save only if something is changed
    if (theEl.isValueChanged() && theEl.isFullfilled()) {
      showSavingMessage();
      var wlId = theEl.parents("table[wlId]:first").attr("wlId");

      $.getJSON("approvalAjaxController.jsp", {CM: "SAVEWL", wlId: wlId, dur: theEl.val()}, function (response) {

        jsonResponseHandling(response);

        if (response.ok) {
          //reset variables
          theEl.val(response.normValue);
          theEl.updateOldValue();
          refreshColumn(theEl.parents("td[day]:first").attr("day"));
        }
        refreshTotals();
        hideSavingMessage();
      });
    }
  }

  function saveDescr() {
    var theEl = $(this);
    //save only if something is changed
    if (theEl.isValueChanged()) {
      showSavingMessage();
      var wlId = theEl.parents("table[wlId]:first").attr("wlId");
      $.getJSON("approvalAjaxController.jsp", {CM: "SAVEDESCR", wlId: wlId, descr: theEl.val()}, function (response) {
        jsonResponseHandling(response);
        if (response.ok) {
          //reset variables
          theEl.updateOldValue();
        }
        hideSavingMessage();
      });
    }
  }

  function changeStatus() {
    var theEl = $(this);
    if (theEl.isValueChanged()) {
      showSavingMessage();
      var wlId = theEl.parents("table[wlId]:first").attr("wlId");
      $.getJSON("approvalAjaxController.jsp", {CM: "SAVESTATUS", wlId: wlId, sts: theEl.val()}, function (response) {
        jsonResponseHandling(response);
        if (response.ok) {
          theEl.updateOldValue();
        }
        hideSavingMessage();
      });
    }
  }


  function performBulkAction(command, el) {
    var form = $("#Wl_PLAN");
    var ids = getCheckedIds();
    if (ids.length > 0 && (el.closest(".bulkData").find(":input").isFullfilled())) {
      //change command
      form.find("[name=CM]").val(command);

      // inject hidden ids field on form
      var hid = $("<input type='hidden' name='wlIds'>");
      hid.val(ids.join(","));
      form.append(hid);

      if (el) {
        // move div "moveToTaskAll" to form in order to submit ce
        form.append(el.closest(".bulkData").hide());
      }
      form.attr("alertOnChange", false);
      form.submit();
    }
  }

  function getCheckedIds() {
    var ret = [];
    $("#multi [wlid] :checked").each(function () {
      ret.push($(this).closest("[wlId]").attr("wlId"));
    });
    return ret;
  }


</script>
