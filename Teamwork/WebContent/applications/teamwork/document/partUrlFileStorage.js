function openFileNav(tfId, docId) {
  //console.debug("openFileNav", tfId, docId, location)
  //remove the error
  $('#' + tfId + 'error').remove();

  var url = contextPath + "/applications/teamwork/document/popUpFileStorage.jsp?POP=yes";
  url += "&OPENER_FIELD_ID=" + tfId;

  var fieldContent = $('#' + tfId).val();
  if (fieldContent) {

    var string = fieldContent.substring(2);
    var valori = string.split(":");
    if (valori != null && valori.length > 0) {
      var fileStorageId = valori[0];
      url += "&OBJID=" + valori[0];
      url += "&PATH=" + valori[1];
    }

  }
  openBlackPopup(url, $(window).width() - 100, $(window).height() - 50);

}
