
  var _colorValueData = _colorValueData || {};

  function cvc_clickColValSel(colorSquare, event) {
    //console.debug("cvc_clickColValSel");
    var component=colorSquare.closest("[cvcType]");
    var hiddenInput = component.find(":input:first");
    if (hiddenInput.is("[readonly]"))
      return;

    var isMultiSelect=component.is(".cvcMultiSelect");
    var isDisplayValue=component.is(".cvcDisplayValue");

    //se è già aperta si esce subito
    if (component.find(".cvcDropDown").size()>0){
	    component.find(".cvcDropDown").remove();
	    return;
    }

    var type = component.attr("cvcType");
    var cvcData=_colorValueData[type];
    var colors=cvcData.values;

    //si costruisce la lista
    var divList = $("<div>").addClass("cvcDropDown").css("z-index", 100000);

    //si prendono i valori settati
    var val=hiddenInput.val();
    var selectedCodes= val?val.split(","):[""];

    if (typeof (cvcData.callback)=="function") {
      var callback=cvcData.callback;
      component.off("cvcChange").on("cvcChange",function(ev,data){
        //console.debug("calling back",data);
        callback($(this).find(":input:first"),data);
      });
    }


    //per tutti i codici/colori in lista
    //for (var code in colors){
    for (var i=0;i<colors.length;i++){
      var codeColor=colors[i];
      //console.debug("codeColor",codeColor)
      var code=codeColor.code;

      var colorLine=$("<div>").addClass("cvcLine").html(codeColor.value).attr("code",code);
      var icon=$("<span>").addClass("teamworkIcon").css({"color":codeColor.color}).html("&#169; ");
      colorLine.prepend(icon);

      colorLine.on("click touchend",function(ev){

        ev.stopPropagation();
	      ev.preventDefault();
        var selCode = $(this).attr("code");
        if(isMultiSelect) {
          if (!selCode || selCode == "") {
            selectedCodes = [];
            divList.find(".cvcLine").removeClass("selected");
          } else {
            var pos = selectedCodes.indexOf("");
            if (pos >= 0)
              selectedCodes.splice(pos, 1);
            if ($(this).toggleClass("selected").is(".selected")) {
              selectedCodes.push(selCode);
            } else {
              selectedCodes.splice(selectedCodes.indexOf(selCode), 1);
            }

          }
          hiddenInput.val(selectedCodes.join(","));
          hiddenInput.clearErrorAlert();

        } else {
          hiddenInput.val(selCode);
          hiddenInput.clearErrorAlert();
          component.find(".cvcDropDown").remove();
        }
        cvc_redraw(component);

        //callback
        //console.debug(hiddenInput.val(),colors,cvc_getValueFromCode(hiddenInput.val(),colors))
        component.trigger("cvcChange",[cvc_getValueFromCode(hiddenInput.val(),colors)]);
      });

      //è selezionato?
      if (selectedCodes.indexOf(code)>=0)
        colorLine.addClass("selected");

      divList.append(colorLine);
    }

    //si bindano gli enventi di chiusura
    component.bind("mouseleave",function(){$(this).oneTime(200,"delDrop", function(){$(this).find(".cvcDropDown").remove();})});
    component.bind("mouseenter",function(){component.stopTime("delDrop")});

    colorSquare.append(divList);

    divList.keepItVisible(colorSquare);

    stopBubble(event);
    return false;
  }

  function cvc_redraw(component){
    //console.debug("cvc_redraw",component.attr("cvcType"));
    var val=component.find(":input:first").val();
    var selectedCodes= val?val.split(","):[""];
    component.find(".cvcSelBox").remove();

    var isMultiSelect=component.is(".cvcMultiSelect");
    var isDisplayValue=component.is(".cvcDisplayValue");

    var colorValueData = _colorValueData[component.attr("cvcType")];

    //console.debug("colorValueData",colorValueData,selectedCodes)
    for (var i = 0; i < selectedCodes.length; i++) {
      //var ccv = colorValueData[selectedCodes[i]];
      var ccv = cvc_getValueFromCode(selectedCodes[i],colorValueData.values);
      if (ccv) {
        var div = $("<div>").addClass("cvcSelBox").attr("code", ccv.code).attr("title",ccv.value);
        if (!isMultiSelect && isDisplayValue) {
          div.css({color: ccv.textColor, "background-color": ccv.color});
        } else {
          var span = $("<span>").addClass("teamworkIcon").css("color", ccv.color).html("&#169;");
          div.append(span);
        }

        if (isDisplayValue) {
	        var valueSpan = $("<span/>").addClass("cvcDescription").html(ccv.value);
          div.append(valueSpan);
        }

        component.find(".cvcStatuses").append(div);
      }
    }
  }

  function cvc_selectFirstElement(component){
    var type=component.attr("cvcType").toUpperCase();
    component.find(":input[type=hidden]").val(_colorValueData[type].values[0].code);
  }

  function cvc_getValueFromCode(code,colorValueData){
    var ret;
    for (var i=0;i<colorValueData.length;i++){
      if (code==colorValueData[i].code) {
        ret =colorValueData[i];
        break;
      }
    }
    return ret;
  }
