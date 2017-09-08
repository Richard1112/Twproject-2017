var isAndroid,isIOs;
var currentPage;
var currentPageHash;
var currentPageIsBack;

$(function() {
	//console.debug("APPLICATION BEGIN",location.href,location.hash);

	detectDevice();

	//trucco per capire se siamo su file locale o su embedded, se in locale il deviceready lo mando a mano
	var isApp = document.URL.indexOf('http://') === -1 && document.URL.indexOf('https://') === -1;

	document.addEventListener("deviceready", onDeviceReady, false);
	document.addEventListener("resume", onResume, false);  //quando l'applicazione viene "risvegliata"
	document.addEventListener("backbutton", onPressBack, false);  //quando si preme il back

	if (!isApp)//|| !isIOs ) // dato che non uso phonegap lo lancio a mano
		onDeviceReady();

	/*
	 $(document).on("touchend", "input[type=checkbox], input[type=radio] ", function(){
	 this.checked = !this.checked;
	 })
	 */

});

function onDeviceReady() {
	//setta la classe isIos o isAndroid per nascondere le bizzarrie degli store
	if (isIOs) {
		$("body").addClass("isIos");
	} else if(isAndroid) { //il caso di default lo mettiamo a android cosi che i link siano abilitati
		$("body").addClass("isAndroid");
	} else {
		$("body").addClass("isWin");
	}

	var oldHash=location.hash;
	//remove hashtag
	location.hash = ""; // se non lo svuoto nella prima pagina non ci entro
	setupAndBindings();

	startup(oldHash); // see mobileHome.jsp
}


function onResume(){
	$(window).hashchange(); //genera evento per rinfrescare la pagina
}

function onPressBack(e){
	e.preventDefault();
	if (!currentPage || currentPage.attr("id")=="home" || location.href.indexOf("firstPage")>=0){
		navigator.app.exitApp();
	} else {
		backPage();
	}
}

function detectDevice() {
	isAndroid = (/Android/i).test(navigator.userAgent);
	isIOs = (/(iPod|iPad|iPhone)/i).test(navigator.userAgent);

	if (isIOs || isAndroid)
		$("body").addClass("mobile");

	if(window.navigator.standalone)
		$("body").addClass("standalone");

}


/**
 * carica le parti di default di una pagina, se queste non sono definite nella pagina da disegnare vengono usate queste
 */
function getPageDefaults() {
	var defP = $("[data-role=defaultPage]");
	var pageDefaults = {};
	pageDefaults.header = defP.find("[data-role=header]");
	pageDefaults.header = pageDefaults.header.size() > 0 ? pageDefaults.header : undefined;

	pageDefaults.content = defP.find("[data-role=content]");
	pageDefaults.content = pageDefaults.content.size() > 0 ? pageDefaults.content : undefined;

	pageDefaults.footer = defP.find("[data-role=footer]");
	pageDefaults.footer = pageDefaults.footer.size() > 0 ? pageDefaults.footer : undefined;

	pageDefaults.height = $(window).height();
	return pageDefaults;
}

function adaptLayout(){

	//carica i page defaults
	var pageDefaults = getPageDefaults();

	//sottrae le altezze di altri elementi fixed
	var h = pageDefaults.height;
	if (currentPage){
		currentPage.find("[data-position=fixed]").not(":hidden").each(function() {
			h -= $(this).outerHeight();
		});
		var cont = currentPage.find("[data-role=content]");
		cont.css("height",h);

		//alert(h);


		refreshIscroll();
	}
}

function refreshIscroll(){
	currentPage.find("[data-role=content].scroll,[data-role=content].hscroll ").refreshIscroll();
}


$(window).on("resize", adaptLayout);

function setupAndBindings() {

	//load templates
	$("._mobileTemplates").loadTemplates().remove();

	//set offline by default
	$("body").addClass("offline");

	//carica i page defaults
	var pageDefaults = getPageDefaults();


	//carica header e footer sulle pagine a cui mancano e calcola la dimensione da dare al [data-role=content]
	var pageHeight;
	$("[data-role=page]").each(function() {
		var page = $(this);

		var cont = page.find("[data-role=content]");
		var h = pageDefaults.height;

		var head=page.find("[data-role=header]");
		//se devo inserire l'header
		if (!page.is(".noHeader") && head.size()<=0) {
			head=pageDefaults.header.clone();
			page.prepend(head);
		}

		//se devo inserire il footer
		var foot=page.find("[data-role=footer]");
		if (!page.is(".noFooter") && foot.size()<=0) {
			foot=pageDefaults.footer.clone();
			page.append(foot);
		}


		//sottrae le altezze di altri elementi fixed
		page.find("[data-position=fixed]").each(function() {
			h -= $(this).outerHeight();
		});

		cont.css("height",h);
		page.hide();

	});

	//bind iscroll per i contenuti scrollabili, l'iscroll è salvato sui data della pagina
	// inserisce nel content
	$("[data-role=content].scroll").each(function() {
		var el = $(this);
		//console.debug("scroll ",el)
		el.wrapInner("<div class='iscroll'>");

		var params = {
			preventDefault:false,
			scrollbars: true,
			bounce: false,
			click: false,
      disablePointer: true,
      disableTouch:false,
      disableMouse:false
		};

		if (el.is(".zoom")){
			params.zoom=true;
			params.scrollY=true;
			params.scrollX=true;
		}
		var iscr = new IScroll(el.get(0),params);
		el.closest("[data-role=page]").data("iscroll", iscr);
	});

	$("[data-role=content].hScroll").each(function() {
		var el = $(this);
		el.wrapInner("<div class='iscroll' style='position:absolute;'>");
		var params={
			scrollY:false,
			scrollX:true,
			bounce:false,
			scrollbars:true,
			fadeScrollbars:true,
			click:true
		};//,HWCompositing:false};
		if (el.is(".zoom")){
			params.zoom=true;
			params.scrollY=true;
			params.scrollX=true;
		}

		var iscr = new IScroll(el.get(0),params);
		el.closest("[data-role=page]").data("iscroll", iscr);
	});

	//bind hashChange
	$(window).hashchange(function(e) {
		// Alerts every time the hash changes!
		__changePage(location.hash);
	});


	//bind page events
	//si presuppone di avere per ogni pagina due funzioni nomePaginaEnter e nomePagineLeave a cui verrà passato l'evento e i dati
	$("[data-role=page]").each(function() {
		var pg = $(this);
		var id = pg.attr("id");
		try {
			var fEenter = eval(id + "Enter");
			if (typeof(fEenter) == "function")
				pg.bind("enterpage", fEenter);
		} catch(e) {
		}

		try {
			var fLeave = eval(id + "Leave");
			if (typeof(fLeave) == "function")
				pg.bind("leavepage", fLeave);
		} catch(e) {
		}
	});
}



/**
 *
 * @param page è l'id della pagina
 * @param params sono un oggetto es: #ricerca/vicino oppure #ricerca/txt=pippo
 */
function goToPage(page, params, noHistory) {
	//console.debug("goToPage",page,params);
	var url = page;
	if (params) {
		for (var key in params)
			url += "/" + encodeURIComponent(key) + "=" + encodeURIComponent(params[key]);
	}

	if(noHistory){

		__changePage("#"+url)
	}else{
		window.location.hash = url;
	}

	manageMainMenuView(true);

}


function __changePage(hash) {
	if($("input").is(":focus")){
		$("input").blur();
		return;
	}

	//si "clicca" su body per far perdere eventuali focus e chiudere in particolare il dateField
	$("body").click();

	if (!hash)
		return;

	//converte l'hash in pagina e dati #ricerca/vicino  #ricerca/str=pippo
	var p = hash.split("/");
	var page = p[0];

	var data;

	if (p.length > 1) {
		data = {};
		for (var i = 1; i < p.length; i++) {
			var el = p[i];
			var pos = el.indexOf("=");
			if (pos > 0) {
				var s = decodeURIComponent(el.substr(pos + 1));
				data[el.substr(0, pos)] = Number(s)||s;  // in questo modo se è un numero ho un numero e non una stringa
			} else {
				data[el] = true; // mette un true per comodità
			}
		}
	}

	//console.debug("hash page: "+page,data);
	var newPg = $(page).show();

	var oldPage = currentPage;
//  var oldPage = currentPage;

	if (oldPage && oldPage.size() > 0) {
		oldPage.trigger("leavepage", [data,newPg,false]);

		if(	currentPageHash == page	){
			oldPage.removeClass("onScreen");
			newPg.addClass("samePage");
		}else{
			if(currentPageIsBack){
				oldPage.addClass("pageBack");
				newPg.addClass("pageBack");
			}

			if(!newPg.is(".editor")){
				oldPage.delay(200).animate({left:0},1,function(){
					oldPage.removeClass("onScreen samePage").addClass("outScreen");
				})
      }
		}
	}

	//se la pagina ha un titolo lo copia nell'header
	if (!newPg.is(".noHeader") && newPg.is("[title]"))
		newPg.find("[data-role=title]").html(newPg.attr("title"));

	$("[btnPage]").removeClass("active");
	$("[btnPage=" + page + "]").addClass("active");


	// identify back button
	var isBack=false;
	window.hashCache=window.hashCache||[];
	var pos = window.hashCache.lastIndexOf(hash);
	if (pos>-1 && pos==window.hashCache.length-2){
		window.hashCache.splice(pos);
		isBack=true;
	}
	window.hashCache.push(hash);
	//console.debug("window.hashCache",window.hashCache);
	newPg.trigger("enterpage", [data,oldPage,isBack]);


	setTimeout(function(){
		newPg.addClass("onScreen");
		//refresh iscroll
		newPg.refreshIscroll();
	},50);

	if(oldPage && oldPage.length && currentPageHash != page)
		setTimeout(function(){
			oldPage.removeClass("outScreen").hide();
			oldPage.removeClass("pageBack");
			newPg.removeClass("pageBack");
		},1000);

	currentPage = newPg;
	currentPageHash = page;
	currentPageIsBack = false;
}


function backPage() {
	if(!$("body").data("componentOverlay")){
		window.history.back();
		currentPageIsBack = true;
	}
}


function touchScrollSmartCombo(){
	var el = $(".cbDropDown");
	var scrollStartPos = 0;

	$("body").on("touchstart", el, function(event){
		scrollStartPos = el.scrollTop() + event.originalEvent.touches[0].pageY;
		//event.preventDefault();
		//console.debug(scrollStartPos);
	});

	$("body").on("touchmove", el, function(event){
		var top = scrollStartPos - event.originalEvent.touches[0].pageY;
		$(".cbDropDown").scrollTop(top);

		//console.debug(top, $(".cbDropDown").scrollTop());
		//event.preventDefault();
	});
}


function enableComponentOverlay(textField,overlayElement) {

	$(".componentOverlayOn").remove();

	var overlay = $("<div/>").addClass("componentOverlayOn");
	textField.before(overlay);

	var labelBox = $("<div/>").addClass("cbDropDownLabel").on("click",function(){
		textField.blur();
	});
	var label = $("[for=" + textField[0].id + "]").html();
	labelBox.html(label);
	overlay.append(labelBox);

	overlayElement.addClass("componentOverlayElement");

	if (!textField.is("[readonly]") || !textField.is("[disabled]"))
		textField.addClass("componentOverlayTF");

	$("label[for=" + textField.prop("id") + "]").addClass("componentOverlayLBL");
	$("body").data("componentOverlay", {textField: textField, overlayElement: overlayElement});

	//hide bottom-bar
	currentPage.find("[data-role=footer]").hide();

	//store the y position to be restored once the overlay is closed
	currentPage.data("yPos", currentPage.data("iscroll").y);

	//scroll to top
	currentPage.data("iscroll").scrollTo(0, 0);

	//destroy iscroll
	currentPage.data("iscroll").destroy();

	$("body").css({overflow:"hidden"});

	adaptLayout();
//	touchScrollSmartCombo();

}

function disableComponentOverlay() {
	//return;
	var co=$("body").data("componentOverlay");
	if (co) {
		co.overlayElement.removeClass("componentOverlayElement");
		co.textField.removeClass("componentOverlayTF");
		$("label[for=" + co.textField.prop("id") + "]").removeClass("componentOverlayLBL");
		$("body").oneTime(50,"RCO",function(){$(this).removeData("componentOverlay")}); // dalayed perchè altrimenti quando si preme il back button non si accorge di dover chiudere
	}
	$(".componentOverlayOn").remove();

	//Show the bottom-bar
	currentPage.find("[data-role=footer]").show();

	var params = {
		preventDefault:false,
		scrollbars: true,
		bounce: false,
		//click: true,
		click: false,
		probeType:3,
		startY: currentPage.data("yPos"),
		preventDefaultException: { tagName:/.*/}
	};

	//initialize the iscroll
	var iscr = new IScroll(currentPage.find("[data-role=content].scroll,[data-role=content].hscroll ").get(0),params);
	currentPage.data("iscroll", iscr);
	$("body").css({overflow:"auto"});
	adaptLayout();

}


function textAreaAdjust(o) {
	o.style.height = "1px";
	o.style.height = (25+o.scrollHeight)+"px";
}


function manageMainMenuView(forceClose){
	if(typeof forceClose == "undefined")
		forceClose = false;
	if( $("[data-role=mainMenu]").is(".mainMenuOpen") || forceClose) {
		$("[data-role=mainMenu]").removeClass("mainMenuOpen");
		$("[data-role=page]").removeClass("mainMenuIsOpen");

		$(document).off("touchstart.mainMenuOpen");

	}else {
		$("[data-role=mainMenu]").addClass("mainMenuOpen");
		$("[data-role=page].onScreen").addClass("mainMenuIsOpen");

		setTimeout(function(){
			$(document).on("touchstart.mainMenuOpen", function(e){
				e = e || e.originalEvent;
				if(!$(e.target).parents().is(".mainMenuBox") && !$(e.target).is(".mainMenuBox")){
					manageMainMenuView(true);
					e.stopPropagation();
					return false;
				}
			});
		},5);

		setMenuSel();

	}
}

function setMenuSel() {
	var page = window.location.hash.replace("#","").split("/")[0];
	$(".mainMenuBox button").removeClass("selected");
	$(".mainMenuBox button[data-role*=" + page + "]").addClass("selected");
};


jQuery.fn.refreshIscroll = function() {
	//console.debug("refresh iscroll");
	var el = $(this);
	var ct = el.closest("[data-role=page]");
	if (ct.data("iscroll"))
		setTimeout(function () {
			ct.data("iscroll").refresh();
		}, 0);
	return this;
};

/*
 * enableSwipe()
 * http://labs.rampinteractive.co.uk/touchSwipe/demos/index.html
 *
 *
 * Add the swipe behaviour on a list row element
 *
 * the list row element must have data-role="swiper"
 * and must contain a div with class="swipeBox" with the actions inside it
 *
 * The enableSwipe() function should be called once the list is drawn in the page
 *
 *
 *
 * */
function enableSwipe(){

	$("[data-role=swiper]").each (function(){

		if(this.swipeInited)
			return;

		var el = $(this);
		var swipeBox = $(".swipeBox",el);

		swipeBox.on("click", function(e){

			if(el.is(".swipeIsActive")){
				e.preventDefault();
				e.stopPropagation();
			}

		});

		el.swipe({
			swipeStatus:function(event, phase, direction, distance , duration , fingerCount, pinchZoom, fingerData) {
				if (direction == "left" && distance > 100){
					$("[data-role=swiper]").not(el).removeClass("swipeIsActive");
					el.addClass("swipeIsActive");
				}else if(direction == "right" && distance > 100){
					$("[data-role=swiper]").removeClass("swipeIsActive");
				}

			},
			threshold:50,
			fingers:'all'
		});

		this.swipeInited = true;

	});
}

document.addEventListener("touchmove", ScrollStart, false);
document.addEventListener("touchend", Scrolled, false);

function ScrollStart(e) {
	//start of scroll event for iOS
	$("body").addClass("scrolling");


	/*
	 * !!!! Molto importante: !!!!
	 * su iOs dobbiamo fermare il comportamento di default per lo scroll e delegarlo tutto a iScroll.
	 * Non rimuovere il preventDefault().
	 * ************************************************/

 	if($("body").is(".isIos"))
		e.preventDefault();

	/*************************************************/
}

function Scrolled(e) {
	$("body").removeClass("scrolling");
}

$.fn.unselectable = function () {
	this.each(function () {
		$(this).addClass("unselectable").attr("unselectable", "on");
	});
	return $(this);
};

$.fn.clearUnselectable = function () {
	this.each(function () {
		$(this).removeClass("unselectable").removeAttr("unselectable");
	});
	return $(this);
};

/**
 *
 * FormElements error
 *
 **/

jQuery.fn.clearErrorAlert = function () {
	this.each(function () {
		var el = $(this);
		el.removeAttr("invalid").removeClass("formElementsError");
		$("#" + el.prop("id") + "error").remove();
	});
	return this;
};

// Used to grow textareas to fit theur conteent
function fit_content(elem_id, restore)
{
	var elem = document.getElementById(elem_id);
	$(elem).css({overflow:"auto"});
	if(!restore) {

		if (elem.expanded) {

			elem.expanded = false;
			fit_content(elem_id, true);
			return;

		} else if (elem.clientHeight < elem.scrollHeight) {
			elem.orig_height = elem.clientHeight;
			//console.debug(elem.clientHeight, elem.scrollHeight);
			$(elem).css({height: elem.scrollHeight, maxHeight: elem.scrollHeight});
			elem.expanded = true;
		} else {
		}
	}else {
		$(elem).css({height: elem.orig_height});
		elem.expanded = false;
	}

	refreshIscroll();

}

$(function(){
	$("body").on("focus", ".formElementsError", function(){
		$(this).clearErrorAlert();
	})
});







