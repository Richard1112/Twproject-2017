<%@ page import="org.jblooming.waf.settings.I18n" %>


<div id="lineActions" class="divomo lineActions"  style="display:none;">
  <div class="divomoArrow"></div>
  <div>

    <%if (I18n.isActive("CUSTOM_FEATURE_RECONCILE_PLAN")){%>
      <span class="button noprint textual"  onclick="reconcilePlan($(this));"><%=I18n.get("RECONCILE_PLAN")%></span>
    <%}%>
    <%if (I18n.isActive("CUSTOM_FEATURE_FILL_PLAN_0")){%>
      <span  class="button noprint textual" onclick="fillPlan0($(this));"><%=I18n.get("FILL_PLAN_0")%></span>
    <%}%>
  </div>
</div>


<div id="planNotesEditor" class="microEdit mbBalloon" day="" assId="" style="display:none;">

  //todo: Dovrebbe essere gestito come un balloon vero...
  <%--<div style="top: -12px; left: 10px;" class="arrow n"></div>--%>
  <%--<div style="top: -12px; left: 10px;border-color:#fff" class="arrow border n"></div>--%>

  <div style="position:absolute;top:0;right: 0"><span class="teamworkIcon meClose" title="<%=I18n.get("CLOSE")%>" onclick="closeMicroEditor();" style="cursor: pointer; font-size: 15px">x</span></div>
  <textarea class="formElements formElementsMedium" name="description" rows="5" cols="5" style="margin:2px 0; padding:2px;width:100%;min-height:60px;" placeholder="<%=I18n.get("NOTES")%>"></textarea>
  <div style="text-align:right;padding: 2px 0" class="planNoteButtons">
    <span class="button textual delete" onclick="ajaxDeleteNotes();" style="cursor: pointer"><%=I18n.get("DELETE")%></span>
    &nbsp;&nbsp;
    &nbsp;&nbsp;
    <span class="button textual edit" onclick="ajaxSaveNotes($(this));" style="cursor: pointer;"><%=I18n.get("SAVE")%></span>
  </div>
</div>


<script type="text/javascript">

function openLineAction(el){
  $(".lineActions:visible").remove();
    var lineActions = $("#lineActions").clone();
    lineActions.mouseleave(function(){
      $(this).oneTime(500,"chiuddivomo",function(){$(this).fadeOut(200,function(){$(this).remove();});});
    }).mouseenter(function(){
      $(this).stopTime("chiuddivomo");
    }).click(function(){$(this).remove();});

    el.after(lineActions);
    lineActions.show();
}



//   ---------------------------------------------   FOCUS  ---------------------------------------------
function cellFocus(e) {
//  this.select();
  var theCell = $(this);

  //highlight col e row
  var theTd = theCell.closest("td");
  theTd.prevAll(":last").addClass("showHead");
  theTd.closest("tbody").find(".total[t=" + theTd.attr("t") + "],.dayHeader[t=" + theTd.attr("t") + "]").addClass("showHead");

  if(!theCell.is("[readonly]")){
    theTd.addClass("sel");
    theCell.one("blur",function(){
      theTd.removeClass("sel");
    });
  }

  if (typeof(showExplain) == "function") {
    $(document).stopTime("showExplain");
    $(document).oneTime(800, "showExplain", function() {
      showExplain(theCell);
    });
  }

}


<%---------------------------------------------   KEYBOARD  ---------------------------------------------%>
function cellKey(event) {
  var theCell = $(this);
  var theTd = theCell.parent();

  var newTd;

  var ret = true;
  switch (event.keyCode) {

    case 37: //left arrow
      newTd = theTd.prev();
      ret = false;
      break;

    case 38: //up arrow
      var cells = $(".tdCell[t=" + theTd.attr("t") + "]");
      var pos = cells.index(theTd.get(0));
      newTd = cells.eq(pos > 0 ? pos - 1 : pos);
      ret = false;
      break;
    case 40: //down arrow
      var cells = $(".tdCell[t=" + theTd.attr("t") + "]");
      var pos = cells.index(theTd.get(0));
      newTd = cells.eq(pos + 1);
      ret = false;
      break;
    case 36: //home
      newTd = theTd.prevAll(".tdCell:last");
      ret = false;
      break;
    case 35: //end
      newTd = theTd.nextAll(".tdCell:last");
      ret = false;
      break;

    case 9: //tab
    case 13: //enter
    case 39: //right arrow
      newTd = theTd.next();
      ret = false;
      break;
  }

  if (newTd && newTd.size() > 0)
    newTd.find(".cell,.cellErr").focus();

  return ret;
}

<%---------------------------------------------   saveCell  ---------------------------------------------%>
function saveCell(e) {
  var theCell = $(this);
  var theTd = theCell.parent();
  var theTr = theTd.parent();
  var theTbody = theTd.closest("tbody");

  var itsOK = true;

  //if nothing changes -> do nothing and be happy
  if (theCell.isValueChanged()) {
    // check if the inserted text is ok
    if (!theCell.val() || isValidDurationMillis(theCell.val())) {
      showSavingMessage();
      var req = {
        CM:"SV_CELL",
        "assId":theTr.attr("assId"),
        "day":theCell.parent().attr("t"),
        "wp":this.value,
        scaleType:$("[name=SCALE_TYPE]").val(),
        focusMillis:$("[name=FOCUS_MILLIS]").val()

      };
      $.getJSON('planAjaxController.jsp', req, function(response) {
        if (response.ok) {
          //console.debug(response);
          theCell.val(response.value);
          theCell.updateOldValue();

          //make cell with 0 value less evident
          if (millisFromString(response.value,true)>0)
            theCell.parent().removeClass("zero_panned").addClass("hasPlan");
          else {
            theCell.parent().addClass("zero_panned").removeClass("hasPlan");

          }

          //update totals if any
          updateCellAndColumns(theTr,response);


          //reset classes
          //theCell.parent().attr("class", "day tdCell");

          //check if exceded
          if (response.cellClass)
            theCell.parent().addClass(response.cellClass);

          updateRowPlanValues(theTr,response.estimatedWorklog, response.totPlanned);

          theCell.removeClass("cellErr");
          theCell.addClass("cell");

        } else {
          theCell.removeClass("cell");
          theCell.addClass("cellErr");
        }
        hideSavingMessage();
      });

    } else {
      itsOK = false;
    }
  }

  //highlight col e row
  var theTd = theCell.closest("td");
  theTd.prevAll(":last").removeClass("showHead");
  $(".showHead[t=" + theTd.attr("t") + "]").removeClass("showHead");

  if (itsOK) {
    theCell.removeClass("cellErr");
    theCell.addClass("cell");

  } else {
    theCell.removeClass("cell");
    theCell.addClass("cellErr");
  }

  return itsOK;
}

<%---------------------------------------------------------------------------------  UPDATE ASSIG ESTIMATION ---------------------------------------------------------------------%>
function updateEstimation(el) {
  // check if the inserted text is ok
  if (el.isValueChanged() && isValidDurationMillis(el.val())) {
    showSavingMessage();
    var theTr = el.closest("[assid]");

    var req = {
      CM:"UPD_EST",
      "assId":theTr.attr("assId"),
      "estim":el.val(),
      focusMillis:$("[name=FOCUS_MILLIS]").val(),
      scaleType:$("[name=SCALE_TYPE]").val()

    };
    $.getJSON('planAjaxController.jsp', req, function(response) {
      jsonResponseHandling(response);
      if (response.ok) {
        //console.debug(response);
        el.val(getMillisInHoursMinutes(response.estimatedWorklog)).updateOldValue();
        //update totals if any
        updateCellAndColumns(theTr,response);
        updateRowPlanValues(theTr,response.estimatedWorklog, response.totPlanned);
      } else {
        el.val(el.getOldValue());
      }

      hideSavingMessage();
    });

  }
}



function reconcilePlan(el){
  var theTr= el.closest("tr");
  var firstTd=theTr.find("td[title]:first");
  if (confirm(firstTd.prop("title")+"\n<%=I18n.get("RECONCILE_PLAN_CONFIRM")%>")){

    var req = {
      CM:"RECONCILE",
      "assId":theTr.attr("assId"),
      focusMillis:$("[name=FOCUS_MILLIS]").val(),
      scaleType:$("[name=SCALE_TYPE]").val()

    };
    $.getJSON('planAjaxController.jsp', req, function(response) {
      jsonResponseHandling(response);
      if (response.ok) {

        //update totals if any
        updateCellAndColumns(theTr,response);

        //update plan cell
        for (day in response.assignementsDetailLine) {
          var plan = response.assignementsDetailLine[day][2];
          var planCell = theTr.find(".tdCell[t=" + day + "]");
          planCell.find("input").val(plan>=0?getMillisInHoursMinutes(plan):"");
        }

        updateRowPlanValues(theTr,response.estimatedWorklog, response.totPlanned);


      } else {
        el.val(el.getOldValue());
      }

      el.updateOldValue();
      hideSavingMessage();
    });

  }
}

function fillPlan0(el){
  var theTr= el.closest("tr");
  var firstTd=theTr.find("td[title]:first");
  if (confirm(firstTd.prop("title")+"\n<%=I18n.get("FILL_PLAN_0_CONFIRM")%>")){

    var req = {
      CM:"FILLPLAN0",
      "assId":theTr.attr("assId"),
      focusMillis:$("[name=FOCUS_MILLIS]").val(),
      scaleType:$("[name=SCALE_TYPE]").val()
    };
    $.getJSON('planAjaxController.jsp', req, function(response) {
      jsonResponseHandling(response);
      if (response.ok) {

        //update totals if any
        updateCellAndColumns(theTr,response);

        //fill with 0 unplanned days
        theTr.find("input.cell").each(function(){
          var cell=$(this);
          if(!cell.is("[readonly],.dayH") && cell.val()==""){
            cell.val("0:00").updateOldValue();
            //make cell with 0 value less evident
            cell.parent().addClass("zero_panned");
          }
        });


      } else {
        el.val(el.getOldValue());
      }

      el.updateOldValue();
      hideSavingMessage();
    });

  }
}


function openWorklogList(el){
  var theTr= el.closest("[assid]");
  openBlackPopup(contextPath+"/applications/teamwork/task/worklog/worklogAssignmentList.jsp?CM=FN&ASS_ID="+theTr.attr("assid"),$(window).width()-300,$(window).height()-100,function(){
    var req = {
      CM:"REFRESHASS",
      "assId":theTr.attr("assId"),
      focusMillis:$("[name=FOCUS_MILLIS]").val(),
      scaleType:$("[name=SCALE_TYPE]").val()
    };
    $.getJSON('planAjaxController.jsp', req, function(response) {
      jsonResponseHandling(response);
      if (response.ok) {
        var theTbody=theTr.closest("tbody");
        //update total load
        var totalRow = theTr.closest("[resId]").find(".totalRow:first");
        for (day in response.totalLoadAndWork) {
          var workLoad = response.totalLoadAndWork[day][0];
          //wl overflow
          var totCell = totalRow.find(".total[t=" + day + "]");
          var exceeded = parseInt(totCell.attr("whpd")) >= 0 && workLoad > parseInt(totCell.attr("whpd"));
          totCell.html(getMillisInHoursMinutes(workLoad));
          if (exceeded) {
            theTbody.find("[t=" + day + "]").addClass("exceeded");
          } else {
            theTbody.find("[t=" + day + "]").removeClass("exceeded");
          }
        }

        //fill worklog done
        theTr.find(".totDone").html(getMillisInHoursMinutes(response.totalWorkDone))

        //fill toBeDone
        var totEstim=theTr.find(".estimOnPlanned input:first").val();
        theTr.find(".toBeDone").html(getMillisInHoursMinutes(millisFromString(totEstim)-response.totalWorkDone));


      } else {
        el.val(el.getOldValue());
      }

      el.updateOldValue();
      hideSavingMessage();
    });
  });
}


function updateRowPlanValues(theTr,estimatedWorklog,totPlanned){
  var totDone=parseInt(theTr.find("[totDone]:first").attr("totDone"));

  //update to be planned
  theTr.find("[toBeDone]:first").attr("toBeDone",estimatedWorklog-totDone).html(getMillisInHoursMinutes(estimatedWorklog-totDone));

  //update plan in the future
  theTr.find("[totPlanned]:first").attr("totPlanned",totPlanned).html(getMillisInHoursMinutes(totPlanned));

  //check for overplanned
  if (estimatedWorklog-totDone < totPlanned)
    theTr.find(".estimOnPlanned").addClass("overPlanned");
  else
    theTr.find(".estimOnPlanned").removeClass("overPlanned");
}


function updateCellAndColumns(theTr,response){
  //console.debug("updateCellAndColumns",response);
  var theResArea=theTr.closest("[resId]"); //tbody o tr

  var resId=theResArea.attr("resId");

  theResArea=$("[resId="+resId+"]");

  for (day in response.totalLoadAndWork) {
    var workLoad = response.totalLoadAndWork[day][0];

    var whpd=parseInt(theTr.find("[t=" + day + "]:first").attr("whpd"));


    if(workLoad > whpd+2){
      theResArea.find("[t=" + day + "]").addClass("exceeded").removeClass("underplanned");

    } else if(workLoad < whpd){
      theResArea.find("[t=" + day + "]").removeClass("exceeded").addClass("underplanned");
    } else {
      theResArea.find("[t=" + day + "]").removeClass("exceeded").removeClass("underplanned");
    }
    theResArea.find(".totalRow [t=" + day + "]").html(getMillisInHoursMinutes(workLoad));
  }

}

function noteButtonClick() {
  //console.debug("noteButtonClick");
  $(this).closest("td").dblclick(); // ri rimanda al doppio click sul td
}

function noteCellDBLClick(){
  //console.debug("noteCellDBLClick");
  var td=$(this);
  var cell=td.find(":text.cell");

  var editor = $("#planNotesEditor");
  editor.css({position:"absolute"});

  if (editor.is(":visible")){
    editor.hide();
    editor.data("openerCell").parent().removeClass("notesInEdit");
    editor.removeData("openerCell");
  }

  if (cell.val()!=""){
    var theTd=cell.parent();
    var theTr=theTd.closest("[assid]");

    // get notes from server
    var req = {
      CM:"GETNOTES",
      "assId":theTr.attr("assId"),
      "day":theTd.attr("t")
    };
    $.getJSON('planAjaxController.jsp', req, function(response) {
      jsonResponseHandling(response);
      if (response.ok) {
        editor.find("textarea").val(response.notes).attr("enabled");
        if (response.canWrite){
          editor.find(".planNoteButtons").show();
          editor.find("textarea").removeAttr("readonly")
        }else{
          editor.find(".planNoteButtons").hide();
          editor.find("textarea").attr("readonly","true")
        }
        editor.show();

        cell.after(editor);
        nearBestPosition(cell,editor);
        editor.find(":input:first").focus();

        editor.data("openerCell",cell);
        cell.parent().addClass("notesInEdit");

        $(document).off("click.cellBblClick").on("click.cellBblClick",function(e){
          if(!$(e.target).is(editor) && !$(e.target).parents().is(editor)  && !$(e.target).is(cell))
            editor.hide();
        })
      }
    });
  }
}

function closeMicroEditor(){
  var editor=$("#planNotesEditor");
  editor.hide();
  editor.data("openerCell").parent().removeClass("notesInEdit");
  editor.removeData("openerCell")
}


function ajaxSaveNotes(){
  var editor=$("#planNotesEditor");
  var cell=editor.data("openerCell");
  var theTd=cell.parent();
  var theTr=theTd.closest("[assid]");

  var req = {
    CM:"SAVENOTES",
    "assId":theTr.attr("assId"),
    "day":theTd.attr("t"),
    "notes":editor.find("textarea").val()
  };
  $.getJSON('planAjaxController.jsp', req, function(response) {
    jsonResponseHandling(response);
    if (response.ok) {
      closeMicroEditor();

      if (req.notes && req.notes!="")
        theTd.addClass("hasNotes");
      else
        theTd.removeClass("hasNotes");
    }
  });
}


function ajaxDeleteNotes(){
  $("#planNotesEditor textarea").val("");
  ajaxSaveNotes();
}

</script>
