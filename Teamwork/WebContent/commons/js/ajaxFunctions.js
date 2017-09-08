//-- START AJAX ---------------------------------------------------------------------------

function executeCommand(command, data) {
  $.ajax({
    url: contextPath + "/command.jsp?CM=" + command,
    data: data,
    dataType: "text"
    //success:function(resp){alert(">"+resp+"<")}
  });
}

/**
 *
 * @param formId
 * @param domIdToReload
 * @deprecated
 */
function ajaxSubmit(formId, domIdToReload) {
	var form = $("#" + formId);
	var settings = {
		success:function (response) {
			hideSavingMessage();
			if (domIdToReload)
				$('#' + domIdToReload).replaceWith(response);
		},
		data:form.serialize()
	};
	showSavingMessage();
	$.ajax(form.attr("action"), settings);
}



/**
 *  shows messages from controller and in case of errors set respons to false
 *  shows client entriesErrors
 *  domScope is used to define where your input are in case of not unique field e.g.: a list with multiple inpout field named "code"
 *   errors =[{ceName:ceErrorCode},...]
 **/

function jsonResponseHandling(response, domScope) {

  //primo: si girano le clientEntries per vedere se ci sono errori
	var ceMessage = "";
	for (var i=0; response.clientEntryErrors && i< response.clientEntryErrors.length ;i++) {
		var err = response.clientEntryErrors[i];
		var field = domScope ? domScope.find(":input[name=" + err.name + "]") : $(":input[name=" + err.name + "]");
		if (field.length > 0)
			field.createErrorAlert(err.error);
		else
			ceMessage += "\"" + err.label + "\":&nbsp;" + err.error + "<br>";
		response.ok=false; // se ci sono errori sulle ce è sempre picche!
	}

	if (ceMessage.trim().length>0){
    showFeedbackMessage("ERROR",ceMessage);
	}

	//secondo: si mandano i messaggi
	if (response.messagesFromController) {
		for (var j=0; response.messagesFromController && j<response.messagesFromController.length;j++) {
			var m = response.messagesFromController[j];
      showFeedbackMessage(m.type, m.message, m.title);
		}
	}
}

// END AJAX ----------------------------------------------------------------------------
