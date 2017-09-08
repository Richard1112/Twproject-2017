var _comboBoxData = _comboBoxData || {};

function cb_inputBlur(input) {
  //console.debug("blur");
	var comboDiv = input.data("comboDiv");
	//comboDiv.remove();
	input.oneTime(200, "cb_inputBlur", function () {
    //console.debug("esecuzione blur");
/*
    if (comboDiv.data("opener").get(0) == input.get(0)) {
      comboDiv.appendTo("body");
    }
*/
		comboDiv.remove()
  });
}

var currentRow;  //global to be shared between keydown and keyup :-(
function cb_keyDown(element, keyCode) {
  var comboType = element.attr("comboType");
  var comboDiv = element.data("comboDiv");
  currentRow = undefined;
  if (!comboDiv.is(":visible")) {
    comboDiv.show();
    var newRow = cb_findRow(element.val(), comboDiv);
    if (newRow)
      currentRow = newRow;
    keyCode = "fuckyou"; //in order skip  find
  }

  if (!currentRow) {
    currentRow = comboDiv.find(".trSel:first");
    if (currentRow.size() == 0)
      currentRow = comboDiv.find(".comboRow:first");
  }
  comboDiv.find(".trSel").removeClass("trSel");

  switch (keyCode) {
    case 38: //up arrow
      var newRow = currentRow.prev();
      if (newRow.size() > 0) {
        currentRow = newRow;
      }
      break;

    case 40: //down arrow
      var newRow = currentRow.next();
      if (newRow.size() > 0) {
        currentRow = newRow;
      }
      break;
  }
  currentRow.addClass("trSel");
  comboDiv.scrollTop(currentRow.get(0).offsetTop - comboDiv.height() / 2);
}


function cb_keyUp(element, keyCode) {
  var comboType = element.attr("comboType");
  var comboDiv = element.data("comboDiv");

  switch (keyCode) {
    case 38, 40 : //up arrow down arrow
      break;

    case 13: //enter
      currentRow.click();
      break;

    default:
      var newRow = cb_findRow(element.val(), comboDiv);
      if (newRow) {
        currentRow = newRow;
        comboDiv.find(".trSel").removeClass("trSel");
        currentRow.addClass("trSel");
        comboDiv.scrollTop(currentRow.get(0).offsetTop - comboDiv.height() / 2);
      }
      break;
  }
}

function cb_findRow(string, comboDiv) {
  var inputValue = string.toLowerCase();
  var currentRow;
  if (inputValue && inputValue.length > 0) {
    comboDiv.find("[comboValue]").each(function () {
      currentRow = $(this);
      if (currentRow.attr("comboValue").toLowerCase().indexOf(inputValue) >= 0) {
        return false;
      }
    });
  }
  return currentRow;
}


function cb_initializeCombo(element) {
  var type = element.attr("comboType");
  var comboData = _comboBoxData[type];
  //console.debug("cb_initializeCombo",element,comboData);

	$(".comboBoxDiv").remove();

  var comboDiv = $("<div>").addClass("comboBoxDiv").attr({comboType: comboData.type, style: comboData.style, onSelectScript: comboData.onSelectScript});
  var table = $("<table>").addClass("table comboTable");
  for (var i = 0; i < comboData.values.length; i++) {
    var tr = $("<tr>").addClass("alternate comboRow").prop("id", comboData.type + "_ROW_" + i).attr("comboValue", comboData.values[i]);
    var td = $("<td>").append(comboData.values[i]);
    tr.append(td);
    table.append(tr);
  }
  if (i == 0) {
    var tr = $("<tr>").addClass("alternate comboRow").prop("id", comboData.type + "_ROW_" + i).attr("comboValue", comboData.values[i]);
    var td = $("<td>").append(i18n.COMBO_NO_VALUES);
    tr.append(td);
    table.append(tr);
  }

  comboDiv.append(table);

	comboDiv.on("mouseleave",function(){
		comboDiv[0].timer = setTimeout(function(){
			comboDiv.remove();
		},1000);
	}).on("mouseenter",function(){
			clearTimeout(comboDiv[0].timer);
	});

  //bind events
  comboDiv.find(".comboRow").click(function (e) {
    var row = $(this);
    var input = comboDiv.data("opener");
    input.stopTime("cb_inputBlur");
    input.val(row.attr("comboValue"));

    //execute onSelectScript using input as "this" for the executon context
    evalInContext(comboDiv.attr("onSelectScript"), input);
	  comboDiv.remove();

  }).mouseenter(function () {
   // comboDiv.find(".trSel").removeClass("trSel");
    //$(this).addClass("trSel");
  }).mouseleave(function () {
    //$(this).removeClass("trSel");
  });

  comboDiv.scroll(function () {
    //console.debug("scroll");
    var input = $(this).data("opener");
    input.stopTime("cb_inputBlur");
  });

  element.data('comboDiv', comboDiv);
  comboDiv.data('opener', element);
  comboDiv.find(".trSel").removeClass("trSel");
  element.after(comboDiv);
  comboDiv.show();
  nearBestPosition(element, comboDiv);
	comboDiv.keepItVisible();

  cb_keyUp(element, "fake");

  //select text
  element.get(0).select();
}

function cb_clickOpener(arrow) {
  var input = arrow.prevAll(':input:first');
  var comboType = input.attr("comboType");
  var comboDiv = $("#" + comboType + "_DIV");
  if (comboDiv.size() > 0 && comboDiv.is(":visible"))
    comboDiv.remove();
  else
    input.focus();
}
