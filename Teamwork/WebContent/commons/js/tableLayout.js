
/*----------------------------------------------------------------- manage table headers, table sections and table footers*/
$.tableHF = {
	defaults: {},

	init: function(){
		setTimeout(function(){
			$.tableHF.refreshThead();
			$.tableHF.refreshTSection();
			$.tableHF.refreshTfoot();
		},100);

		$(window).on("scroll.tableHF",function(){
			$.tableHF.manageThead();
			$.tableHF.manageTSection();
			$.tableHF.manageTFoot();
		});

		$(window).on("resize.tableHF",function(){
			$.tableHF.refreshThead();
			$.tableHF.refreshTSection();
			$.tableHF.refreshTfoot();
		});

		$(".scrollingBox").on("scroll.tableHF",function(){
			$.tableHF.manageThead();
		});
	},

	manageThead: function() {

		return $(".fixHead thead").not(".headerHolder").each(function(){
			var thead = this;

			thead.tbl = $(this).parent("table");
			var tblClass = thead.tbl.attr("class");

			var scrollingBox=thead.tbl.closest(".scrollingBox");

			if (scrollingBox.size()) {
				thead.isOut=scrollingBox.scrollTop()>0;
			} else {
				thead.isOut = ($(window).scrollTop() > $(thead).offset().top) && ((thead.tbl.offset().top + thead.tbl.height()) > $(window).scrollTop());
			}

			if (thead.isOut && !thead.isCloned){
				$(".tableHeaderCloned").remove();

				thead.isCloned = true;
				thead.headerHolder = $("<table/>").addClass("tableHeaderCloned  clone top " + tblClass).css({width: thead.tbl.outerWidth(), marginTop:0});

				thead.headerHolder.hide();

				var theadClone = $(thead).clone();
				theadClone.addClass("headerHolder");
				thead.headerHolder.css({position:(scrollingBox.size()?"absolute":"fixed"), top:0, left:(scrollingBox.size()?0:$(thead).offset().left)});
				thead.headerHolder.append(theadClone);
				var headerHolderTh = thead.headerHolder.find("th");

				$("th", $(thead)).each(function(i) {
					var orig = $(this);
					var clone = headerHolderTh.eq(i);
					clone.data("orig",orig);
					var w = isMozilla ? orig.width() : orig.outerWidth();
					clone.width( w );
					if(! clone.is(".dayTHeader"))
						clone.css({background:"transparent"});

					var span = clone.find("span[onclick]");
					span.attr("onclick", "").on("click", function(e){
						$("html, body").animate({ scrollTop: $(thead).offset().top }, 300, function(){ orig.find("span").click()});
						e.preventDefault();
						e.stopPropagation();
					});

					var check = clone.find("input[type='checkbox']");
					check.attr("onclick", "").on("click",function(){
						orig.find("input[type=checkbox]").click();
					});
				});

				thead.tbl.before(thead.headerHolder);
				//$("body").append(headerHolder);
				thead.headerHolder.show();

			} else if (!thead.isOut && thead.isCloned) {
				//console.debug("destroy");

				thead.isCloned = false;
				thead.headerHolder.remove();
			}

      if (thead.isCloned) {
  			if (scrollingBox.size()){
          thead.headerHolder.css({top: scrollingBox.scrollTop()});
        } else {
          thead.headerHolder.css({left: $(thead).offset().left - $(window).scrollLeft()});
        }
      }


		})
	},

	refreshThead: function() {

		$(".tableHeaderCloned").remove();
		$(".fixHead thead").not(".headerHolder").each(function(){
			var thead = this;
			thead.isCloned = false;
		});
		$.tableHF.manageThead();
	},

	manageTSection:function(){

		$(this).stopTime("manageTSection").oneTime(50, "manageTSection", function(){

			//console.debug("manageTSection");

			$(".fixHead .tableSection").not(".tableSectionHolder").each(function(){

				var tSection = this;

				tSection.tbl = $(this).parents("table");

				var tblClass = tSection.tbl.attr("class");

				tSection.isOut = (( $(window).scrollTop() > $(tSection).offset().top - $(".headerHolder").height() - ($(".tableSectionCloned:visible").height()/2) )) && ((tSection.tbl.offset().top + tSection.tbl.height()) > $(window).scrollTop());

				if (tSection.isOut && !tSection.isCloned){

					$(".tableSectionCloned").hide();
					tSection.isCloned = true;
					tSection.sectionHolder = $("<table/>").addClass("tableSectionCloned  clone top " + tblClass).css({width: tSection.tbl.outerWidth(), marginTop:0});

					tSection.sectionHolder.data("orig",tSection);
					tSection.sectionHolder.hide();

					var tSectionClone = $(tSection).clone();
					tSectionClone.addClass("tableSectionHolder");
					tSection.sectionHolder.css({position:"fixed", top:$(".headerHolder").height() || 0, left:$(tSection).offset().left-$(window).scrollLeft()});
					tSection.sectionHolder.append(tSectionClone);
					var sectionHolderTd = tSection.sectionHolder.find("td");

					$(tSection).css("opacity",.4);

					$("td", $(tSection)).each(function(i) {
						var orig = $(this);
						var clone = sectionHolderTd.eq(i);
						clone.data("orig",orig);
						clone.css({background:"transparent"});
					});

					tSection.tbl.before(tSection.sectionHolder);
					//$("body").append(sectionHolder);
					tSection.sectionHolder.show();

				} else if (!tSection.isOut && tSection.isCloned) {

					tSection.isCloned = false;
					tSection.sectionHolder.remove();

					$(tSection).css("opacity",1);

					var allTSections = $(".tableSectionCloned");
					var lastTSectionClonedElement = allTSections.eq(allTSections.length - 1);
					var orig = lastTSectionClonedElement.data("orig");

					lastTSectionClonedElement.show();
				} else if (tSection.isCloned){
          tSection.sectionHolder.css({left:$(tSection).offset().left-$(window).scrollLeft()});

        }


			})
		})
	},

	refreshTSection: function(){
		$(".tableSectionHolder").remove();

		$(".fixHead .tableSection").not(".tableSectionHolder").each(function() {
			var tSection = this;
			tSection.isCloned = false;
		});

		$.tableHF.manageTSection();
	},

	manageTFoot: function() {
		return $(".fixFoot tfoot").not(".footerHolder").each(function(){
			var tfoot= this;
			tfoot.tbl = $(this).parent("table");
			var tblClass = tfoot.tbl.attr("class");
			var b = $("#moveBar").length ? $("#moveBar").outerHeight() : 0;
			b = $(".bbCloned").length ? $(".bbCloned").outerHeight() : b;

			tfoot.isOut = ($(window).scrollTop() + $(window).height() - $(tfoot).height() - b < $(tfoot).offset().top) && (tfoot.tbl.offset().top < $(window).scrollTop() + $(window).height() - b);

			if (tfoot.isOut && !tfoot.isCloned) {
				tfoot.isCloned = true;
				tfoot.footerHolder = $("<table/>").addClass("tableFooterCloned clone bottom " + tblClass).css({width: tfoot.tbl.outerWidth()});

				tfoot.footerHolder.hide();

				var tfootClone = $(tfoot).clone();
				tfootClone.addClass("footerHolder");
				tfoot.footerHolder.css({position:"fixed", bottom: b, left:$(tfoot).offset().left});
				tfoot.footerHolder.append(tfootClone);
				var footerHolderTD = tfoot.footerHolder.find("td");
				$("td", $(tfoot)).each(function(i){
					var orig = $(this);
					var clone = footerHolderTD.eq(i);
					var w = isMozilla ? orig.width() : orig.outerWidth();
					clone.width( w );
					if(! clone.is(".color"))
						clone.css({background:"transparent"});
				});

				tfoot.tbl.after(tfoot.footerHolder);


				$("body").css({marginBottom : tfoot.footerHolder.height() +100});

				if(tfoot.footerHolder.height()>10)
					tfoot.footerHolder.show();
				else
					tfoot.footerHolder.hide();

			} else if(!tfoot.isOut && tfoot.isCloned) {
				tfoot.isCloned = false;
				tfoot.footerHolder.remove();
			}
		});
	},

	refreshTfoot: function(anim) {
		$(".tableFooterCloned").remove();
		$(".fixFoot tfoot").not(".footerHolder").each(function(){
			var tfoot = this;
			tfoot.isCloned = false;
		});
		$.tableHF.manageTFoot(anim);
	}

};

//prepende un elemento per il filtraggio dei dati della tabella a "ndo"
function createTableFilterElement(ndo,rowSelector,cellSelector){
	var ret=$("<div>").addClass("tableFilterElementBox");
	var input_id = "tableFilterField_" + Math.floor(Math.random()*10000);
	var input=$("<input id='" + input_id + "' type='text' class='tableFilterField formElements formElementsSmall tableFilterElementBox-input' autocomplete='off'>");
	input.attr("rowSelector",rowSelector).attr("cellSelector",cellSelector).attr("onkeyup","tableFilter($('#"+input_id+"'))");
	ret.append(input);
	var search = $("<span class='button noprint textual icon tableFilterElementBox-search' data-search><span class='teamworkIcon' >L</span></span>");

	$("body").on("click.tableFilterElementBox", ".tableFilterElementBox-search" ,function(e){

		if ($(this).parents().is(".headerHolder")) {
			var th_orig = $(this).parents("th").data("orig");
			openTableFilter($(".tableFilterElementBox-search", th_orig),e);
		} else {
			openTableFilter($(this),e);
		}
	});

	$("body").on("focus", ".tableFilterElementBox-input", function(){
		if($(".headerHolder").length){
			$("html, body").css({ scrollTop: $(".fixHead thead").not(".headerHolder").offset().top }, 100, function(){
				setTimeout(function(){
					$(".tableFilterElementBox-input").focus();
				},500);
			});
		}
	});

	$("body").on("scroll", function(){
			$(".tableFilterElementBox-input").blur();
	});

	ret.append(search);
	var close = $("<span class='button noprint textual icon tableFilterElementBox-close' data-close><span class='teamworkIcon' >x</span></span>");

	$("body").on("click.tableFilterElementBox", ".tableFilterElementBox-close", function() {
		$(this).prevAll(':input').val('').keyup();
		$(this).closest('.tableFilterElementBox').removeClass('filterOn');
	});

	ret.append(close);
	ndo.prepend(ret);
	return ret;

}

function openTableFilter(el,event){

	if ($(".headerHolder").length) {
		$("html, body").animate({ scrollTop: $(".fixHead thead").not(".headerHolder").offset().top }, 300, function(){
			el.prev().focus();
		});
	}else{
		setTimeout(function(){
			el.prev().focus();
		},300);
	}
	event.stopPropagation();
	var searchDiv = el.closest(".tableFilterElementBox");
	searchDiv.addClass("filterOn");

	//si blocca la dimensione di tutti i th per evitare sfarfallii. Si calcola in percentuale
	var tableW=searchDiv.closest("table").width();
	var s=[];
	var ths = searchDiv.closest("tr").find("th");
	ths.each(function(){
		var th=$(this);
		s.push(th.outerWidth()/tableW*100);
	});

	for (var i=0;i<ths.length;i++){
		ths.eq(i).css("width",s[i]+"%");
	}

}


function tableFilter(filterTextField){
	var rowSelector=filterTextField.attr("rowSelector");
	var cellSelector=filterTextField.attr("cellSelector");
	var val=filterTextField.val();
	filterTextField.stopTime("tableFilterTimer");
	//console.debug("tableFilter",val);
	if (val.length>0){
		filterTextField.oneTime(400,"tableFilterTimer",function(){
			var vals=val.toLowerCase().split(' ');
			$(rowSelector).each(function(){
				var row=$(this);
				var cellTxt=row.find(cellSelector).text();
        row.find(cellSelector).find("input").each(function(){
          cellTxt+=" "+$(this).val();
        }) ;
        cellTxt=cellTxt.toLowerCase();
        var containsAll=true;
				for (var i=0;i<vals.length;i++){
					if (cellTxt.indexOf(vals[i])<0){
						containsAll=false;
						break;
					}
				}
				if (!containsAll)
					row.addClass("tableFilteredRow");
				else
					row.removeClass("tableFilteredRow");
			});
		});

	} else {
		$(rowSelector).removeClass("tableFilteredRow");
	}
}
