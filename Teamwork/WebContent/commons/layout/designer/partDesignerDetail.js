function addDetailLine(href, tableId, detailName) {
  //console.debug("addDetailLine",href, tableId, detailName);
  var table = $("#" + tableId);
  var id = parseInt(table.attr('maxRow')) + 1;
  table.attr('maxRow', id);
  var newHref = href + '&detailName=' + detailName + '&rowLine=' + id;

  $.get(newHref, function (ret) {
    //console.debug("get",ret)
    table.append(ret);
  });
}