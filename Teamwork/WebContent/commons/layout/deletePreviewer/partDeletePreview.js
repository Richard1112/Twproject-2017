  function deletePreview(deletePreviewerId, delendoId,callback) {
//    console.debug("deletePreview", deletePreviewerId, delendoId);
    var request={"DEL_PREV_ID":deletePreviewerId,OBJID:delendoId,CM:"DELETEPREVIEW",ISCALLBACK:callback==null?"no":"yes"};  //se c'è la callback non dovrà fare redirect dopo il delete
    var diag=createModalPopup(550,300,callback);
    $.get(contextPath+"/commons/layout/deletePreviewer/deletePreviewAjaxController.jsp", request, function (response) {
      diag.append(response);
      $(window).resize();
    });
  }

  function performDelete(deletePreviewerId, delendoId, useCallback) {
    //console.debug("performDelete", deletePreviewerId, delendoId,useCallback);
    $("#deleteErrorFeedback").hide();

    var request={"DEL_PREV_ID":deletePreviewerId,OBJID:delendoId,CM:"DL"};
    var diag=getBlackPopup();
    diag.find(":radio:checked").each(function(){
      var el=$(this);
      request[el.prop("name")]=el.val();
    });

    $.getJSON(contextPath+"/commons/layout/deletePreviewer/deletePreviewAjaxController.jsp", request, function (response) {
      jsonResponseHandling(response);
      if (response.ok) {
        if(!useCallback && response.REDIRECT_TO ){
          location.href=response.REDIRECT_TO;
        } else {
          closeBlackPopup(response); //in questo modo si passa la response al callback che può eventualmente cancellare la riga
        }
      } else if (response.stackTrace){
        //mostra errore
        $("#referrers").hide();
        var stTrace = $("#deleteErrorFeedback code");
        stTrace.empty();
        if (response.stackTrace)
          stTrace.append(response.stackTrace.substr(0,Math.min(300,response.stackTrace.length))+"...");
        $("#deleteErrorFeedback").show();
      }
      hideSavingMessage();
    });
  }