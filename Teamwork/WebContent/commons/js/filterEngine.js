(function(){

//$(function(){
	//chiusura di un filtrino
	var filterClose=function(){
		//console.debug("close filter")
		var el=$(this);
		var fe=el.closest(".filterElement");
		var filter=el.closest(".filterBar");
		filter.find(".filterInactiveElements").append(fe);
		fe.data("selector").show();
		//si disabilita l'input
		fe.find(":input").prop("disabled",true).val("").prop("checked",false); // WARNING: devono essere svuotati altrimenti la history back re-inserisce i valori
	};

//	var formElementsFocus = false;

	//selezione di un filtrino
	function filterSelect(){
		var el=$(this);
		var filterActive=$(".filterBar").find(".filterActiveElements");
		var fe = el.data("filterElement");
		filterActive.append(fe);
		//si abilita l'input
		fe.find(":input").prop("disabled",false);
		bjs_hideDivOnMouseover("filterSelectorBox");

		el.fadeOut();
	};

	var filter=$(".filterBar");
	if (filter.length==0)
		return;

	//si costruisce il selettore dei filtri
	var selectors=$("<div>").addClass("divomo").attr({id:"filterSelectorBox"}).hide();
	var selectorsArrow=$("<div>").addClass("divomoArrow");


	// si looppano gli elementi
  filter.find (".filterElement").each(function(){
		var fe=$(this);

		//si aggiunge la "x" su tutti i filterElement
    var x = $("<span>").addClass("close").html("x").click(filterClose);
    fe.append(x);

		//si cerca la label
		//var lbl=fe.find("label");   //in modo da prendere solo la prima
		var lbl=fe.find("label:first");

		//si crea un elemento selettore
		var selEl=$("<a/>").addClass("button noprint textual filterSelectorElement").html(lbl.text()).data("filterElement",fe).click(filterSelect);
		fe.data("selector",selEl);

		//si ricopiano gli attributi data- aggiuntivi anche sul selettore
		var attrs=fe.get(0).attributes;
		for (var i=0;i<attrs.length;i++){
			var attr=attrs[i];
			if (attr.nodeName.startsWith("data-")){
				selEl.attr(attr.nodeName,attr.value);
			}
		}

		selectors.append(selEl);

		//si disabilitano gli input
		fe.find(":input").prop("disabled",true);

    //si mettono copiano i default
    if (fe.is (".filterDefault"))
      selEl.addClass("filterElementDefault");
	});

  //si sortano gli elementi del filtro in ordine alfabetico
  var fels=selectors.find("a.filterSelectorElement");
  fels.sort(function(a,b){
    var an= $(a).text();
    var bn= $(b).text();
    return an>bn ? 1: (an<bn ?-1 :0) ;
  });
  fels.detach().appendTo(selectors);

  selectors.prepend(selectorsArrow);


	//si aggiunge il display per il selettore
	filter.find(".filterButtons .filterAdd").append(selectors);


  //si mettono quelli di default già visibili simulando un click
  filter.find(".filterElementDefault").click();


  //si cercano tutti gli input text e hidden
	filter.find("input[type=hidden],input[type=text],select").each(function(){
		var inp=$(this);
		if (inp.val() && inp.val().trim()!="" && (inp.val()!="no" || !inp.is("[data-checkfield]"))){
			inp.closest(".filterElement").each(function(){
				var tds = $(this).data("selector");
				if (tds)
					tds.click();
			})
		}

	});
})();
//});
