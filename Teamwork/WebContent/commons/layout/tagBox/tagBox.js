$(document).ready(function() {
	$("[taggableClass]").livequery(tagBoxBind);
});

function tagBoxBind(e) {
	$(this).bind("focus", tagBoxFocus).bind("blur", tagBoxBlur).bind("keydown", tagBoxKey);
}

function tagBoxFocus(e) {
	var theEl = $(this);
	// check if the result box exists
	var theDiv = $("#" + theEl.prop("id") + "_DIV");
	if (theDiv.size() <= 0) {
		//create the div
		var boxDiv=$("<div id='" + theEl.prop("id") + "_DIV" + "' class='tbDiv' style='width:" + theEl.width() + "; '></div>");
		theEl.after(boxDiv);
		boxDiv.css({left:theEl.position().left});
		boxDiv.css("z-index","1000");
		theDiv = $("#" + theEl.prop("id") + "_DIV");
	}
	if (theEl.attr("autoStart")=="1")
		tagBoxRefreshDiv(theEl, theDiv);
}

function tagBoxBlur(e) {
	var theEl = $(this);
	var theDiv = $("#" + theEl.prop("id") + "_DIV");
	// reformat string

	var splitted = theEl.val().split(",");
  var resArr=[];
	var res="";
	for(i = 0; i < splitted.length; i++){
    var tag=splitted[i].trim();
		if (tag!="" && resArr.indexOf(tag)<0){
      resArr.push(tag);
		}
	}
	theEl.val(resArr.join(", "));

	theDiv.fadeOut(200, function() {
		$(this).remove()
	});
}


function tagBoxKey(e) {

	var theEl = $(this);
	var theDiv = $("#" + theEl.prop("id") + "_DIV");
	var rows = theDiv.find("div");
	var rowNum = rows.index(theDiv.find("div.tagBoxSel"));

	var ret=true;
	switch (e.which) {
		case 38: //up arrow
			rowNum = (rowNum < 1 ? 0 : rowNum - 1 );
			tagBoxHLSCR(theDiv, rows.eq(rowNum), true);
			break;

		case 40: //down arrow
			rowNum = (rowNum < rows.size() - 1 ? rowNum + 1 : rows.size() - 1 );
			tagBoxHLSCR(theDiv, rows.eq(rowNum), false);
			break;

		case 13: //enter
			var theRow = rows.eq(rowNum);
			tagBoxClickRow(theRow);
			ret=false;
			break;

		default:
			$(document).stopTime("tagBoxRefresh");
			$(document).oneTime(400, "tagBoxRefresh", function() {
				tagBoxRefreshDiv(theEl, theDiv);
			});
			break;
	}
	return ret;
}


function tagBoxRefreshDiv(theEl, theDiv) {
	var lastComma = theEl.val().lastIndexOf(",");
	var search = theEl.val().substr(lastComma + 1).trim();

	theDiv.hide();
	if (search != "" || theEl.attr("autoStart") == "1") {
		theDiv.load(contextPath + document.jspFillerName, "CL=" + encodeURIComponent(theEl.attr("taggableClass")) + "&AID=" + theEl.attr("areaId") + "&FN=" + encodeURIComponent(search) + "&MR=" + theEl.attr("maxResult") + "&TPN=" + theEl.attr("tagPropertyName"), function() {
			theDiv.data("theEl",theEl);

			if(!theDiv.isEmptyElement())
				theDiv.fadeIn();

			theDiv.find("div:first").addClass("tagBoxSel");
			theDiv.find("div").bind("click", function() {
				tagBoxClickRow($(this));
			});
		});
	} else{

	}
}

function tagBoxHLSCR(theDiv, theRowJQ, isUp) {
	if (theRowJQ.size() > 0) {
		var div = theDiv.get(0);
		var theRow = theRowJQ.get(0)
		if (isUp) {
			if (theDiv.scrollTop() > theRow.offsetTop) {
				theDiv.scrollTop(theRow.offsetTop);
			}
		} else {
			if ((theRow.offsetTop + theRow.offsetHeight) > (div.scrollTop + div.offsetHeight)) {
				div.scrollTop = theRow.offsetTop + theRow.offsetHeight - div.offsetHeight;
			}
		}
		theDiv.find("div.tagBoxSel").removeClass("tagBoxSel");
		theRowJQ.addClass("tagBoxSel");
	}
}

function tagBoxClickRow(el) {
	//console.debug("tagBoxClickRow") ;
	var theRow = el;//$(this);
	var theEl = theRow.closest(".tbDiv").data("theEl");

	if (!theEl || !theEl.length)
		return;

	var lastComma = theEl.val().lastIndexOf(",");
	var newVal = (theEl.val().substr(0, lastComma + 1) +" "+ theRow.text()).trim();

	theEl.val(newVal);
	theRow.parent().remove();
	theEl.focus();
}
