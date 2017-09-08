$(function () {
  //console.debug("dataTableSetup");
  $("[data-table]").each(function () {
    var tbl = $(this);
    //move order by field inside the form in order to get saveable with filters
    $("#"+tbl.attr("formId")).append($("#OB_"+tbl.attr("id")));

    //bind
    if (tbl.is("[bindReturnKey]")){
      $("#"+tbl.attr("formId")).keyup(function(ev){
        if(ev.keyCode==13 && $(ev.target).closest(".filterActiveElements").length ) // solo sugli elementi nel filtro
          dataTableSearchClick(tbl.attr("id"));
      });
    }

    dataTableRefreshPaginator(tbl);
    dataTableRefreshOrderHeaders(tbl);
    //console.debug(tbl.find(".dataTableBody tr").length)
    if (tbl.find(".dataTableBody[noData]").length) {
      dataTableRefresh(tbl);
    } else {
      $(function(){sinchDataTableComponents(tbl)});
    }
  });
});

function dataTableChangeOrder(el,dataTableId) {
  //console.debug("dataTableChangeOrder");
  if (!canILeave()) return;
  var orderState = parseInt(el.attr("orderState")) + 1;
  orderState = orderState % 3;
  el.attr("orderState", orderState);

  var dataTable = $("#"+dataTableId);

  $("[name=OB_" + dataTableId + "]").val((orderState == 1 ? el.attr("orderingHql") + " ASC" : (orderState == 2 ? el.attr("orderingHql") + " DESC" : "")));
  dataTableRefresh(dataTable);
}


function dataTableRefresh(dataTable,resetPaginator) {
  //console.debug("dataTableRefresh", typeof (dataTable),resetPaginator);
  if (typeof(dataTable) != "object")
    dataTable = $("#" + dataTable);

  var dataTableId = dataTable.prop("id");
  if (resetPaginator)
    $("#" + dataTableId + "_PGtf").val(0); // altrimenti il search ti lascia alla pagina corrente

  //si recupera il form
  var form = $("#" + dataTable.attr("formId"));

  var data = {};
  /*form.find(".filterActiveElements :input").each(function () {
    var inp = $(this);
    data[inp.prop("name")] = inp.val();
  });*/
  form.find(".filterActiveElements").fillJsonWithInputValues(data);


  var cmp = form.find("input[name='CM']").val();
  data.CM = "FN";
  if(cmp!=""){
	  data.CM = cmp;
  }
  data["DATA_TBL_ID"] = dataTableId;

  //ci si mettono sempre i dati del paginator e dell'order in quanto potrebbero essere fuori dalla form
  data["_FP_PG_N"] = $("#"+dataTableId + "_PGtf").val();
  data["_FP_PG_S"] = $("#_FP_PG_S").val();
  data["OB_"+dataTableId] = $("[name=OB_"+dataTableId+"]").val();

  var searchString="";
  for (var n in data){
    //console.debug("data[n]",data[n])
    if (data[n] && data[n].length>0)
      searchString+=encodeURIComponent(n)+"="+encodeURIComponent(data[n])+"&";
  }


  if(history.pushState){
    history.pushState("","",window.location.pathname+"?"+searchString);
  }else{ //this is required for ie9
    window.location.href =  window.location.pathname+"?"+searchString;
   return;
  }


  var settings = {
    success:function(response){sinchDataTableComponents(dataTable, response)},
    data: data
  };
  showSavingMessage();

  //dataTable.css({opacity:.4});

  var paginators = $("[dataTableId=" + dataTable.prop("id") + "]");
  paginators.find(".paginatorSearching").show();
  paginators.find(".paginatorNotFound,.paginatorFoundN,.pagSize").hide();

  $.ajax(contextPath + "/commons/layout/dataTable/dataTableAjaxController.jsp", settings);
}

function sinchDataTableComponents(dataTable, response) {
  //console.debug("sinchDataTableComponents",dataTable.length);
  if (response) {

    //dataTable.css({opacity:1});

    dataTable.children("tfoot").remove();
    dataTable.children("tbody").replaceWith(response);
    $.tableHF.refreshTfoot();
  }
  var tBody = dataTable.children("tbody");
  var tHead = dataTable.children("thead");

  if (tBody.attr("totalnumberofelements") == "0"){
    $(dataTable).after($(".paginatorNotFound"));
	  $(".paginatorNotFound").hide();
    $(".paginatorNotFound:first").show().addClass("hint");
    $("#workSpace").hide();
  } else {
    $("#workSpace").show();
  }


  dataTableRefreshPaginator(dataTable);
  dataTableRefreshOrderHeaders(dataTable);

  //rise a load event on dataTable
  dataTable.trigger('load.dataTable');

  //se nell'head c'è un filtro attivo si svuota
  tHead.find(".tableFilterElementBox").removeClass("filterOn").find("input").val("");


  //si ritorna in cima alla pagina
  //console.debug("scroll",window.scrollX,window.scrollY)
  window.scrollTo(window.scrollX,0);

  //chiama, se è definita, una funzione js sulla pagina principale con il numero di elementi trovati
  if (typeof dataTableCallback == "function") {
    dataTableCallback(parseInt(tBody.attr("totalnumberofelements")));
  }
  hideSavingMessage();

  $(window).resize();

}

function dataTableRefreshOrderHeaders(dataTable) {
  //console.debug("dataTableRefreshOrderHeaders");
  var tHead = dataTable.find(".dataTableHead");
  var tBody = dataTable.find(".dataTableBody");
  var orderBy=tBody.attr("orderBy");
  // reset sort
  tHead.find(".tableHeadEl[orderingHql]").attr("orderState", 0);

  if (orderBy && orderBy.length>0) {
    var order = orderBy.split(" "); // task.orderState ASC
    var el = tHead.find("[orderingHql^='" + order[0] + "']");
    var newOrder = 1;
    if (order.length > 1) {
      newOrder = ("ASC" == order[1].toUpperCase()) ? 1 : (("DESC" == order[1].toUpperCase()) ? 2 : 1);
    }
    //console.debug("neworder",newOrder)
    el.attr("orderState", newOrder);
  }
}


function dataTableRefreshPaginator(dataTable) {
  //console.debug("dataTableRefreshPaginator");
  var bd = dataTable.find("tbody");
  var totalNumberOfElements = parseInt(bd.attr("totalNumberOfElements"));
  var pageSize = parseInt(bd.attr("pageSize"));

  var pageNumber = parseInt(bd.attr("pageNumber"));
  //console.debug(totalNumberOfElements, pageSize, pageNumber);
  var paginators = $("[dataTableId=" + dataTable.prop("id") + "]");

  paginators.find(".paginatorSearching").hide();

  //---------------------- trovato niente ----------------------------------
  if (!totalNumberOfElements || totalNumberOfElements <= 0) {
    paginators.find(".paginatorSearching, .paginatorFound1,.paginatorFoundN,.pagSize").hide();
    paginators.find(".paginatorNotFound:first").show();
    paginators.find(".paginatorPages").empty();
    //---------------------- trovato 1 ----------------------------------
  } else if (totalNumberOfElements == 1) {
    paginators.find(".paginatorSearching, .paginatorNotFound,.paginatorFoundN,.pagSize").hide();
    paginators.find(".paginatorFound1").show();
    paginators.find(".paginatorPages").empty();
    //---------------------- trovati N ----------------------------------
  } else {
    paginators.find(".paginatorSearching, .paginatorNotFound,.paginatorFound1").hide();
    paginators.find(".paginatorFoundN .totalNumberOfElements").html(totalNumberOfElements);
    paginators.find(".paginatorFoundN,.pagSize").show();
    var newPages = $("<span>").addClass("paginatorPages");
    var quantPagesShow = 10;
    var minPage = 0;
    var maxPage = 0;
    var lastPageNumber = parseInt((totalNumberOfElements-1) / pageSize); // è zero based

    if (pageNumber > lastPageNumber - quantPagesShow / 2)
      minPage = lastPageNumber - quantPagesShow;
    else
      minPage = pageNumber - quantPagesShow / 2;

    if (minPage <= 0)
      minPage = 0;

    maxPage = minPage + quantPagesShow;
    if (maxPage >= lastPageNumber)
      maxPage = lastPageNumber;

    paginators.find(".paginatorPages").css({visibility: "visible"});

    if (pageNumber >= 1) {
      newPages.append("<span class='dataTablePrev' style=font-weight:700 onclick='dataTableGoToPage($(this)," + (pageNumber) + ")'><span class='teamworkIcon sm'>{</span>&nbsp;"+i18n.PREV+"</span>");
    }

    if (minPage > 1) {
      newPages.append("<span onclick='dataTableGoToPage($(this),1)'>1...</span>");
    }

    if (minPage!=maxPage) {
      for (var i = minPage; i <= maxPage; i++) {
        newPages.append("<span onclick='dataTableGoToPage($(this)," + (i + 1) + ")'>" + (i == pageNumber ? "<b>" + (i + 1) + "</b>" : (i + 1)) + "</span>");
      }
    }

    if (maxPage < lastPageNumber) {
      newPages.append("<span onclick='dataTableGoToPage($(this)," + (lastPageNumber + 1) + ")'>..." + (lastPageNumber + 1) + "</span>");
    }

    if (pageNumber < lastPageNumber) {
      newPages.append("<span class='dataTableNext' style=font-weight:700 onclick='dataTableGoToPage($(this)," + (pageNumber + 2) + ")'>"+i18n.NEXT+"&nbsp;<span class='teamworkIcon sm'>}</span></span>");
    }
    paginators.find(".paginatorPages").replaceWith(newPages);

    if(!maxPage)
      paginators.find(".paginatorPages").css({visibility: "hidden"});
    else
      paginators.find(".paginatorPages").css({visibility: "visible"});
  }
}


function dataTableGoToPage(el, pageNumber) {
  //console.debug("dataTableGoToPage",pageNumber);
  if (!canILeave()) return;
  var dataTableId = el.closest("[dataTableId]").attr("dataTableId");
  $("#" + dataTableId + "_PGtf").val(pageNumber);
  dataTableRefresh($("#" + dataTableId));
}

function dataTableSearchClick(dataTableId){
  if (!canILeave()) return;

  $(".paginatorNotFound").hide();
  var filtTitle = $(".filterTitle");
  filtTitle.html(filtTitle.attr("defaultTitle"));
  dataTableRefresh(dataTableId, true);
}

function dataTableChangePageSize(dataTable){
  //check if something is changed
  if (!canILeave()) return;
  dataTableRefresh(dataTable,true);
}

function generatePluginLink(link, height, width){

  var dataTable = $("table.dataTable[formid]");
  var form = $("#" + dataTable.attr("formId"));

  var data = {};
  form.find(".filterActiveElements").fillJsonWithInputValues(data);
  data.CM = "FN";


  var searchString="";
  for (var n in data){
    if (data[n] && data[n].length>0)
      searchString+=encodeURIComponent(n)+"="+encodeURIComponent(data[n])+"&";
  }


  if(history.pushState){
    history.pushState("","",window.location.pathname+"?"+searchString);
  }else{ //this is required for ie9
    window.location.href =  window.location.pathname+"?"+searchString;
    return;
  }

  if(width>0 || height>0){
    openBlackPopup(link+"?"+searchString,width+"px",height+"px");
  }else{
    location.href=link+"?"+searchString;
  }
}