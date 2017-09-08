function scDropDownRowClicked(row){
	//console.debug("scDropDownRowClicked");
	var dropDown=row.parents(".cbDropDown:first");
	var hiddField=dropDown.nextAll("input[type=hidden]:first");
	var txtField=dropDown.prevAll("input:text:first");
	txtField.val(row.attr('selectText')).trigger("change");
	hiddField.val(row.attr('selectValue')).trigger("change");
	txtField.blur();
}

function createDropDown(txtField, ifrWidth, ifrHeight) {
	var hiddField=txtField.nextAll("input[type=hidden]:first");
	var dropDown=txtField.nextAll(".cbDropDown:first");

	if (dropDown.size()==0) {
		dropDown = $("<div>");
		dropDown.addClass("cbDropDown");
		//var css = {left: txtField.position().left}; // 9/3/2016 Mattie Rob sembra non servire e scassa il mobile
		var css = {};
		if (ifrWidth)
			css.width= ifrWidth;
		if (ifrHeight )
			css.height=ifrHeight;
		dropDown.css(css);

		if ($("body").is(".mobile")){
			enableComponentOverlay(txtField,dropDown);
		}
		dropDown.attr( "fieldId",hiddField.prop("id"));

    var isIos = $("body").is(".isIos") ;
		dropDown.on(isIos  ? "click" : "mousedown",function(ev){ //si usa il mousedown
			txtField.removeClass("doNotFinalize");
			if ($(ev.target).closest(".scTr").size()>0){
				scDropDownRowClicked($(ev.target).closest(".scTr"));
			} else if ($(ev.target).closest(".addEntityLine").size()>0) { // click on add entity line
				return false;
			} else {
				//stop finalization
				txtField.addClass("doNotFinalize");
			}

			ev.stopPropagation();
			ev.preventDefault();

		}).on("mouseup",function(ev){
      txtField.removeClass("doNotFinalize");
    });
		txtField.after(dropDown);
	}
	if (!$("body").is(".mobile"))
		dropDown.keepItVisible(txtField);
}

function refreshDropDown(dropDown, txtField,key) {

	//console.debug("scRefreshDropDown",key);
	var txt=txtField.val();
	row = 0;
	var hiddField = dropDown.nextAll("input[type=hidden]:first");
	if (!hiddField || hiddField.length==0)
		return;
	//console.debug("scRefreshDropDown",txt,key,hiddField.val(),hiddField.attr("jspPart"));

	//txt = txt.replace(/&backslash;/g, '\\');  //commentata il 12/1/2014
	dropDown.load(contextPath+"/commons/layout/smartCombo/"+hiddField.attr("jspPart"),
			{id: dropDown.attr("fieldId"),
				filter: txt,
				key:key,
				hiddenValue:hiddField.val(),
				ststrnz: (new Date()).getTime()}, function() {
				dropDown.show();

      //nel caso di mobile si attiva iscroll
      if ($("body").is(".mobile.isIos")){
        if (dropDown.data("iscroll")) {
          dropDown.data("iscroll").destroy();
        }
        var overlay_iscroll = new IScroll(dropDown.get(0), {click: true});
        dropDown.data("iscroll", overlay_iscroll);
      };
	});
}

function finalizeOperation(dropDown, required, addAllowed) {
	//console.debug("scFinalizeOperation");

	//return false;

	var txtField = dropDown.prevAll("input:text:first");
	if (txtField.is(".doNotFinalize")){
		txtField.removeClass("doNotFinalize");
		return;
	}

	var hidField = dropDown.nextAll("input[type=hidden]:first");
	if (required) {
		if (addAllowed) {
			if (txtField.val() == '') {
				txtField.addClass('formElementsError');
			} else {
				txtField.removeClass('formElementsError');
			}
		} else {
			if (hidField.val() == '') {
				txtField.addClass('formElementsError');
			} else {
				txtField.removeClass('formElementsError');
			}
		}
	} else {
		if (!addAllowed && (txtField.val() != '' && hidField.val() == '')) {
			txtField.addClass('formElementsError');
		} else {
			txtField.removeClass('formElementsError');
		}
	}

	//hide image linking to entity on new selection
	var linkedEnt = txtField.nextAll("._lnk:first");
	if (linkedEnt) {
		if (hidField.isValueChanged() ) {
			linkedEnt.hide();
		} else {
			linkedEnt.show();
		}
	}

	if ($("body").is(".mobile")){
		disableComponentOverlay();
	}

	dropDown.remove();

	var fieldId = hidField.prop("id");
	try {
		// riscritta per poter chiamare il letSubmit.. con un contesto
		//if (eval("typeof(letSubmit" + fieldId + ")") == 'function') {
		//  eval("letSubmit" + fieldId + "()");
		//}
		if (typeof window["letSubmit" + fieldId]=='function')
			window["letSubmit" + fieldId].apply(hidField.get(0));

	} catch (e) {
		//console.error(e)
	}

}

function scrollDropDown(dropDown, inc) {
	dropDown.scrollTop(inc);
}

function stopKeyEvent(event) {
	var ret=true;
	switch (event.keyCode) {
		case 13: //enter
			stopBubble(event);
			ret= false;
			break;
	}
	return ret;
}

var row = 0;
var theRow = false;

function manageKeyEvent(txtField, oEvent, required, addAllowed) {
	//console.debug("manageKeyEvent",txtField.attr('id'),oEvent);
	var dropDown=txtField.nextAll(".cbDropDown:first");
	var totalRow=dropDown.find("tr[selectValue]").size()+1;
	var hidField = txtField.nextAll("input[type=hidden]:first");

	var refreshQueue;
	var ret = true;
	switch (oEvent.keyCode) {

		case 38: //up arrow
			var nextRowId = row > 1 ? "ROW_" + row : "ROW_1";
			if (row > 1) {
				row--;
				var rowId = "ROW_" + row;
				theRow = dropDown.find("#"+rowId);
				thePrevRow = dropDown.find("#"+nextRowId);
				if (theRow.position().top<0)
					dropDown.scrollTop(dropDown.scrollTop()-theRow.outerHeight()-3);
				theRow.addClass("trSel");
				if (thePrevRow.size()>0)
					thePrevRow.removeClass("trSel");
				return false;
			}
			break;

		case 40: //down arrow
			var prevRowId = "ROW_" + (row);
			if (row < totalRow - 1) {
				row++;
				var rowId = "ROW_" + row;
				theRow = dropDown.find("#"+rowId);
				thePrevRow = dropDown.find("#"+prevRowId);

				if (theRow.position().top>(dropDown.height()-theRow.outerHeight()))
					dropDown.scrollTop(dropDown.scrollTop()+theRow.outerHeight()+3);

				theRow.addClass("trSel");
				if (thePrevRow.size()>0)
					thePrevRow.removeClass("trSel");
				return false;
			}
			break;

		case 33: //page up
			var prevRowId = row > 1 ? "ROW_" + row : "ROW_1";
			if (row > 1) {
				row = 1;
				var rowId = "ROW_" + row;
				theRow = dropDown.find("#"+rowId);
				thePrevRow = dropDown.find("#"+prevRowId);
				dropDown.scrollTop(0);
				theRow.addClass("trSel");
				if (thePrevRow.size()>0){
					thePrevRow.removeClass("trSel");
				}
				return false;
			}
			break;

		case 34: //page down
			var nextRowId = row > 1 ? "ROW_" + row : "ROW_1";
			if (row < totalRow - 1) {
				row = totalRow - 1;
				var rowId = "ROW_" + row;

				theRow = dropDown.find("#"+rowId);
				thePrevRow = dropDown.find("#"+nextRowId);

				dropDown.scrollTop(20000);
				theRow.addClass("trSel");
				if (thePrevRow.size()>0){
					thePrevRow.removeClass("trSel");
				}
				return false;
			}
			break;

		// in those cases do nothing, do not need to refresh combo, only caret movement
		case 37: //left arrow
		case 39: //right arrow
		case 36: //home
		case 35: //end
		case 27: //esc
		case 16: //shift
		case 17: //ctrl
		case 18: //alt
		case 20: //caps lock
		case 255: // ???
			break;


		case 9:  //tab
		case 13: //enter
			if (theRow) {
				stopBubble(oEvent);
				txtField.val($(theRow).attr('selectText'));
				hidField.val($(theRow).attr('selectValue'));
				txtField.get(0).blur();

				row = 0;
				theRow = false;


				//17/06/2013 bicch & chel: si prova a dare il focus all'input dopo
				var allvis=$(":input:visible");
				var nextField=allvis.get(allvis.index(txtField.get(0))+1);
				if (nextField)
					nextField.focus();
				//console.debug("daiiffocoaipprossimo ",allvis.index(txtField.get(0)),allvis);

				return false;
				// break; move out of "if" 24/1/13  in order to avoid field invalidation when moving using tab
			}
			break;


		// in this case must refresh combo
		case 8: //backspace
		case 46: //delete
		default:
			hidField.val("");  // WARNING added on 12/12/2007 in order to manage correctly combo.addAllowed
			row = 0;
			//var cleanedText = txtField.val().replace(/\\/g, '&backslash;');  commentata il 12/1/2014
			var cleanedText = txtField.val();
			dropDown.stopTime("refreshQueue");
			dropDown.oneTime(500,"refreshQueue",function(){
				refreshDropDown($(this),txtField,"");
			});
			break;
	}
	return false;
}


function removeSmartComboEntry(smartComboName) {
	//console.debug("removeSmartComboEntry",smartComboName)
	$("#"+smartComboName).val('');
	$("#"+smartComboName+"_txt").val('');
}

function setSmartComboEntry(smartComboName, code, descr) {
	$("#"+smartComboName).val(code);
	$("#"+smartComboName+"_txt").val(descr);
}

