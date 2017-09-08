<%@ page import="org.jblooming.waf.settings.I18n" %>
<%if (false) {%>
<script><%}%>


/**
 *
 * @param el che scatena l'azione
 * @param request ={
*   wlId:worklog_id_se_in_edit,
*   assId:assignment_id_se_in_add,
*   text_testo_action_precompilato,
*   issueId:issue_Id_se_ce,
*   title:title_da mettere_all_editor,
*   millis:millis data del giorno su cui si inserisce
* }
 * @param callback
 * @returns {*|jQuery|HTMLElement}
 */
function createWorklogEditor(el, request, callback) {
  //console.debug("createWorklogEditor", el, request,callback);

  if (hideWorklogEditorIfYouCan()) {

    var isInIframe = isIframe();
    var wlBox = $("<div id='insertWL' class='offScreen' style='width: 300px;'></div>");

    if (isInIframe)
      wlBox.addClass("inIframe");

    wlBox.data("originElement", el);

    wlBox.load(contextPath + "/applications/teamwork/task/worklog/partWorklogEditor.jsp", request, function (html) {

      bindWorklogEvents();

      if (request.date)
        wlBox.find("#WORKLOG_INSERTIONDATE").val(request.date);
      else if (request.millis)
        wlBox.find("#WORKLOG_INSERTIONDATE").val(new Date(request.millis).format()); //attenzione questo millis deve essere nel TZ del server
      else
        wlBox.find("#wlDateInput").show();



      if (request.title)
        wlBox.find("#wlEditorSubTitle").html(request.title);
      else
        wlBox.find("#wlEditorSubTitle").remove();

      if (request.text)
        wlBox.find("#WORKLOG_ACTION").val(request.text);

      //wlBox.css({opacity: 0, marginTop: -500}).show();

      $("#insertWL :input").updateOldValue();

      //si appende sul body
      $("body").append(wlBox);
      if (typeof (callback) == "function")
        callback(wlBox);

    });

    return wlBox;
  }

}


function openWorklogEditorCloseToElement(el, request, callback) {
  //console.debug("openWorklogEditorCloseToElement", el, request, callback)

  createWorklogEditor(el, request, function (box) {

    box.find("#wlEditorSubTitle,#wlEditorTitle").remove();

    el.showBalloon(null, {
      balloon:     box.removeClass("offScreen").css({padding: "10px 15px"}),
      oncursor:    false,
      bgcolor:     "#FFF",
      bordercolor: "#CCC",
      textcolor:   "#000",
      addoverlay:  false,
      storeData:   false,
      autoHide:    false,
      addclose:    true

    });

    if (typeof (callback) == "function")
      callback(box);
    else
      $("#WORKLOG_DURATION").focus();

  });
}

function openWorklogEditorPopup(el, request, callback) {

  var wlBox = createWorklogEditor(el, request, function (box) {
    createModalPopup(box.width()+60, box.height()).append(box).parent().off("close"); // si rumuove l'evento close sul background
    box.removeClass("offScreen");

    if (typeof (callback) == "function")
      callback(box);
    else
      $("#WORKLOG_DURATION").focus();

  });
}

function hideWorklogEditorIfYouCan() {

  if (!$("#insertWL :input[oldValue]").isValueChanged() || confirm("<%=I18n.get("LOOSE_CHANGES")%>")) {
    var box = $("#insertWL");
    if (box.parent().is(".bwinPopupd"))
      getBlackPopup().trigger("destroy");
    else
      $(".mbBalloonOpener").destroyBalloon();

    $(document).off("click.wleditor");
    $("#floatWindow").off("keydown");
    return true;
  } else {
    return false;
  }
}

function _deleteWorklog(el) {
  var wlBox = $("#insertWL");
  if (wlBox.data("originElement")) {
    removeWorklog(wlBox.data("originElement"), el.closest("[wlId]").attr("wlId"), null, true)
  }
  hideWorklogEditorIfYouCan()
}

function _saveWorklog(el) {
  if ($("#insertWL :input").isFullfilled()) {
    var request = {
      CM:                    "SV",
      wlId:                  el.closest("[wlId]").attr("wlId"),
      assId:                 el.closest("[assId]").attr("assId"),
      issueId:               el.closest("[issueId]").attr("issueId"),
      WORKLOG_DURATION:      $("#WORKLOG_DURATION").val(),
      WORKLOG_ACTION:        $("#WORKLOG_ACTION").val(),
      WORKLOG_INSERTIONDATE: $("#WORKLOG_INSERTIONDATE").val()
    };

    var box = $("#insertWL");
    for (var i = 1; i <= 4; i++) {
      request["WORKLOG_CUSTOM_FIELD_" + i] = box.find("[name=WORKLOG_CUSTOM_FIELD_" + i + "]").val();
    }

    showSavingMessage();
    $.getJSON(contextPath + "/applications/teamwork/task/worklog/worklogAjaxController.jsp", request, function (response) {
      jsonResponseHandling(response);
      if (response.ok) {
        var wlBox = $("#insertWL");
        wlBox.fadeOut(200);
        if (wlBox.data("originElement"))
          wlBox.data("originElement").trigger("worklogSaved", [response]);

        $("#insertWL :input").updateOldValue();
        hideWorklogEditorIfYouCan();

        //svuota i campi per evitare doppi wl
        wlBox.find(":input").val("");

        $("body").trigger("worklogEvent", [
          {type: "save", response: response}
        ]);
      }
      hideSavingMessage();
    });
  }
}

function bindWorklogEvents() {
  //bind click per chiudere finestra
  $(document).off("click.wleditor").on("click.wleditor", function (e) {
    if ($(e.target).hasClass("wlDayCell"))
      return false;
    if ($(e.target).closest("#insertWL").size() <= 0) {
      hideWorklogEditorIfYouCan();
    }
  });

  var editorInputs = $("#insertWL input,#insertWL textarea").not("#WORKLOG_INSERTIONDATE");
  var idx = 0;
  $("#floatWindow").on("keydown", function (e) {

    var keyCode = e.keyCode || e.which;
    if (keyCode == 13 && !$("#WORKLOG_ACTION").is(":focus")) {
      $(".saveWlbtn").click()
    } else if (keyCode == 9) {
      e.preventDefault();
      idx++;
      if (idx == editorInputs.length)
        idx = 0;
      editorInputs.eq(idx).focus();
      return false;
    }

  });
}


<%if (false) {%></script>
<%}%>
