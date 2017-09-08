jQuery.fn.activateTeamworkLinks = function (showImages) {
	//first activate standard link
	this.activateLinks(showImages);
	var issueRE = /[Ii]#([\w\u00c0-\uFFFF\u0021-\u002f]+)#/g;
	var taskRE = /[Tt]#([\w\u00c0-\uFFFF\u0021-\u002f]+)#/g;
	var resourceRE = /[Rr]#([\w\u00c0-\uFFFF\u0021-\u002f]+)#/g;
	var boardRE = /[Bb]#([\w\u00c0-\uFFFF\u0021-\u002f]+)#/g; // boards
	var eventRE = /[EeMm]#([\w\u00c0-\uFFFF\u0021-\u002f]+)#/g; // event or meeting

	this.each(function () {
		var el = $(this);
		var html = el.html();

		html = html.replace(issueRE, "<a href='#' onclick=\"openIssueEditorInBlack('$1','GUESS');return stopBubble(event);\">I#$1#</a>", "g");
		html = html.replace(taskRE, "<a href='#' onclick=\"window.location='" + contextPath + "/applications/teamwork/task/taskOverview.jsp?CM=GUESS&OBJID=$1';return stopBubble(event);\">T#$1#</a>", "g");
		html = html.replace(resourceRE, "<a href='#' onclick=\"window.location='" + contextPath + "/applications/teamwork/resource/resourceEditor.jsp?CM=GUESS&OBJID=$1';return stopBubble(event);\">R#$1#</a>", "g");
		html = html.replace(eventRE, "<a href='#' onclick=\"window.location='" + contextPath + "/applications/teamwork/agenda/agendaEditor.jsp?CM=GUESS&OBJID=$1';return stopBubble(event);\">E#$1#</a>", "g");
		html = html.replace(boardRE, "<a href='#'onclick=\"openBoardInBlack('$1','GUESS');return stopBubble(event);\">B#$1#</a>", "g");

		el.html(html);

	});
	return this;
};

function changeFastSearchKey(key){
	var el = $("#search");
	var actualVal = validString = el.val();

	if(actualVal.indexOf(":") == 1) {
		var validString = actualVal.substring(2);
	}

	if(key == "ALL")
		actualVal = validString;
	else
		actualVal = key + ":" + validString;

	el.get(0).actualKey = key;

	el.val(actualVal).focus().setCursorPosition(actualVal.length);

	$(".fSKeys .selected").removeClass("selected");
	$(".fSKeys ." + key).addClass("selected");
};

$("#search").on("keydown",function(){
	$(".fSKeys").hide();
});

function fastSearch(el){

	if(!el.is(":visible")) {
		var self = el.get(0);
		el.closest(".twHeader").addClass("searchOpen");

		el.fadeIn(300).focus();

		if( $(window).width() < 1450)
			$("#mainNav").animate({opacity:.1},100);

		$(".fastSearchkeysButton").fadeIn(500, function(){
			var el = $(this);
			setTimeout(function(){
				//el.click();
			},100)
		}).css({display:"inline-block"});
		el.setCursorPosition(el.val().length);
		setTimeout(function(){
			$(document).on("click.hidesearch", function(e){
				if(!$(e.target).is("#search") && ! $(e.target).parents().is(".fSKeys") && ! $(e.target).parents().is(".menuSearch")){
					$("#mainNav").animate({opacity:1},100);

					el.fadeOut(300, function(){});
					el.closest(".twHeader").removeClass("searchOpen");

					$(".fastSearchkeysButton").fadeOut(300);
					$(document).off("click.hidesearch");
				}
			});
		},400);

		var fastSearch="";
		if (location.href.indexOf("/worklog/")>=0)
			fastSearch="W:";
		else if (location.href.indexOf("/task/")>=0)
			fastSearch="T:";
		else if (location.href.indexOf("/resource/")>=0)
			fastSearch="R:";
		else if (location.href.indexOf("/issue/")>=0)
			fastSearch="I:";
		else if (location.href.indexOf("/agenda/")>=0)
			fastSearch="A:";
		else if (location.href.indexOf("/board/")>=0)
			fastSearch="B:";
		else if (location.href.indexOf("/document/")>=0)
			fastSearch="D:";
		else
			fastSearch="";


		self.actualKey = self.actualKey || fastSearch.replace(":","");
		$(".fSKeys").removeClass("selected");

		if(self.actualKey.length){
			$(".fSKeys ."+self.actualKey).addClass("selected");
		} else
			$(".fSKeys .ALL").addClass("selected");

		return;
	}

	var searchText=el.val();

	var url=contextPath;

	//hack for import from Gantt
	if (searchText.toUpperCase().startsWith("GANTT:")){
		window.location.href=url+"/applications/teamwork/task/taskImportGantt.jsp?TWGANTT_CODE="+(searchText.substr(6));
		return;
	}

	// shortcut management T#xxx# I#xxx# etc.
	var myregexp = /([IiTtRrBbEeMm])#([\w\u00c0-\uFFFF\u0021-\u002f]+)#/i;
	var match = myregexp.exec(searchText);
	if (match != null) {
		var type = match[1].toUpperCase();
		var val=match[2];


		if ("I" == type){
			openIssueEditorInBlack(val,'GUESS');

		} else if ("T" == type){
			window.location.href=url+"/task/"+encodeURIComponent(val);

		} else if ("R" == type){
			window.location.href=url+"/resource/"+encodeURIComponent(val);

		} else if ("E" == type || "M" == type ){
			window.location.href=url+"/"+type+"/"+encodeURIComponent(val);

		} else if ("B" == type  ){
			openBoardInBlack(val,'GUESS');
		}

		// T:xxx I:xxx etc. management
	} else  {
		var searchEncoded = encodeURIComponent(searchText.substr(2));
		if (searchText.toUpperCase().startsWith("T:")) {
			window.location.href=url+"/applications/teamwork/task/taskList.jsp?CM=FN&NAME_DESCRIPTION_NOTE_CODE="+searchEncoded+"&search="+searchEncoded;
		} else if (searchText.toUpperCase().startsWith("R:")) {
			window.location.href=url+"/applications/teamwork/resource/resourceList.jsp?CM=FN&NAME_SURNAME="+searchEncoded+"&search="+searchEncoded;

		} else if (searchText.toUpperCase().startsWith("I:")) {
			window.location.href=url+"/applications/teamwork/issue/issueList.jsp?CM=FN&FLT_ISSUE_DESCRIPTION="+searchEncoded+"&search="+searchEncoded;

		} else if (searchText.toUpperCase().startsWith("D:")) {
			window.location.href=url+"/applications/teamwork/document/documentList.jsp?CM=FN&NAME_DESCRIPTION="+searchEncoded+"&search="+searchEncoded;

		} else if (searchText.toUpperCase().startsWith("B:")) {
			window.location.href=url+"/applications/teamwork/board/boardList.jsp?CM=FN&SEARCH="+searchEncoded+"&search="+searchEncoded;

		} else if (searchText.toUpperCase().startsWith("W:")) {
			window.location.href=url+"/applications/teamwork/task/worklog/worklogList.jsp?CM=FN&WORKLOG_ACTION="+searchEncoded+"&search="+searchEncoded;

		} else if (searchText.toUpperCase().startsWith("A:") || searchText.toUpperCase().startsWith("M:") ) {
			window.location.href=url+"/applications/teamwork/agenda/agendaList.jsp?CM=FN&OBJECT_DESCRIPTION="+searchEncoded+"&search="+searchEncoded;

		} else {
			window.location.href=url+"/applications/teamwork/search/fullTextSearch.jsp?CM=FN&TEXT="+encodeURIComponent(searchText)+"&search="+searchText;
		}
	}
}



//------------------------------------------------ MANAGE RIGHT PANEL   --------------------------------------------------------

/*COOKIES
 * -----------------------------------------------------------------*/
$.mbCookie = {
	set: function(name,value,days,domain) {
		if (!days) days=7;
		domain= domain ?  "; domain="+domain : "";
		var date = new Date(), expires;
		date.setTime(date.getTime()+(days*24*60*60*1000));
		expires = "; expires="+date.toGMTString();
		document.cookie = name + "="+value+expires+"; path=/" + domain;
	},
	get: function(name) {
		var nameEQ = name + "=";
		var ca = document.cookie.split(';');
		for(var i=0;i < ca.length;i++) {
			var c = ca[i];
			while (c.charAt(0)==' ') c = c.substring(1,c.length);
			if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
		}
		return null;
	},
	remove: function(name) {
		$.mbCookie.set(name,"",-1);
	}
};

/*----------------------------------------------------------------- manage right panel */

$.rightPanel = {

	defaults: {
		isPinned: false
	},

	init: function(){

		$.rightPanel.isPinned = eval($.mbCookie.get("rightPanelPinned"));
		$.rightPanel.menu = this;
		$.rightPanel.container = $.rightPanel.menu.closest('.rightColumn');

		if(!this.length){
			$("body").removeClass("pinned");
			setTimeout(function(){
				$('.rightColumn').fadeIn(700);
			},500);

			return;
		}

		$.rightPanel.tools = $(".tools", $.rightPanel.container);
		$.rightPanel.simbol = function(){return $.rightPanel.container.is(".expanded") ? "}" : "{"};
		$.rightPanel.hamburger = $("<span/>").addClass("teamworkIcon hamburger").html($.rightPanel.simbol());

		$.rightPanel.hamburger.attr({title:i18n.OPEN_THIS_MENU});

		$.rightPanel.tools.append($.rightPanel.hamburger);

		$.rightPanel.hamburger.on("click", function(){
			if($.rightPanel.isOpen){
				$.rightPanel.close();
			}else{
				$.rightPanel.open();
			}
		});

		$.rightPanel.pinnedModeLabel = function(){return !$.rightPanel.isPinned ? i18n.PIN_THIS_MENU : i18n.UNPIN_THIS_MENU};
		$.rightPanel.pinnedMode = $("<div/>").addClass("pinnedMode button textual");
		$.rightPanel.pinnedModeContent = $("<span/>").html($.rightPanel.pinnedModeLabel());
		$.rightPanel.pinnedMode.append($.rightPanel.pinnedModeContent);
		$.rightPanel.menu.prepend($.rightPanel.pinnedMode);
		$.rightPanel.origHeight = $.rightPanel.container.height();

		if($.rightPanel.isPinned){
			$("body").addClass("pinned");
			$.rightPanel.hamburger.hide();
		}

		$.rightPanel.pinnedMode.on("click", function(){
			$.rightPanel.pin();
		});

		$(window).on("resize.rightPanel scroll.rightPanel", function(e){

			$("body").css({minHeight: "auto"});

			var minHeight = $(window).height() - $(".twHeader").outerHeight() + $(window).scrollTop() ;
			var hasVScroll = $("body").outerHeight() > $(window).height();

			if(hasVScroll)
				minHeight = "100%";

			$.rightPanel.container.css({minHeight: minHeight});

			/*Manage pinned mode under a screen resolution of 1400*/
			if(e.type == "resize")
				if($(window).width() <= 1400){

					if(!$.rightPanel.wasPinned)
						$.rightPanel.wasPinned = $.rightPanel.isPinned;

					$.rightPanel.isPinned = false;
					$("body").removeClass("pinned");
					$.rightPanel.hamburger.show();
					$.rightPanel.close();
					$('.mainColumn').removeClass('narrow');
					$(".rightColumn .pinnedMode").hide();

				}else if($.rightPanel.wasPinned){

					$("body").addClass("pinned");
					$.rightPanel.isPinned = true;
					$.rightPanel.hamburger.hide();
					$.rightPanel.wasPinned = false;

				}

			if($(window).width() > 1400){
				$(".rightColumn .pinnedMode").show();

			}

			$.rightPanel.container.fadeIn(700);

		}).resize();
	},

	open: function(){
		$.rightPanel.isOpen = true;
		$.rightPanel.container.addClass("expanded");
		$.rightPanel.hamburger.html($.rightPanel.simbol());
		$.rightPanel.hamburger.attr({title:i18n.CLOSE_THIS_MENU});

		setTimeout(function(){
			$(document).on("click.rightPanel",function(e){
				var t = $(e.target);
				var canClose = (!t.is(".rightColumn") && !t.parents().is(".rightColumn")) && !$.rightPanel.isPinned;
				if(canClose){
					$.rightPanel.close();
				}
			})
		},200)
	},

	close: function(){
		$.rightPanel.isOpen = false;
		$.rightPanel.container.removeClass("expanded");
		$.rightPanel.hamburger.html($.rightPanel.simbol());
		$.rightPanel.hamburger.attr({title:i18n.OPEN_THIS_MENU});

		$(document).off("click.rightPanel");
	},

	pin: function(){
		if(!$.rightPanel.isPinned){
			$("body").addClass("pinned");
			$.rightPanel.isPinned = true;
			$.rightPanel.hamburger.hide();

			$.mbCookie.set("rightPanelPinned",true);
		}	else {
			$("body").removeClass("pinned");
			$.rightPanel.hamburger.show();
			$.rightPanel.close();
			$.rightPanel.isPinned = false;
			$('.mainColumn').removeClass('narrow');
			$.mbCookie.remove("rightPanelPinned");
		}

		$(window).resize();

		$.rightPanel.pinnedModeContent.html($.rightPanel.pinnedModeLabel());
	},

	unpin: function(save){
		$("body").removeClass("pinned");
		$.rightPanel.hamburger.show();
		$.rightPanel.close();
		$.rightPanel.isPinned = false;
		$('.mainColumn').removeClass('narrow');
		if(save)
			$.mbCookie.remove("rightPanelPinned");
	}
};

$.fn.initRightPanel = jQuery.rightPanel.init;


//------------------------------------------------ TEAMWORK SPECIFIC FUNCTIONS   --------------------------------------------------------
function openIssueEditorInBlack(issueId, command, params) {
	if (!command)
		command = "ED";
	var editUrl = contextPath + "/applications/teamwork/issue/issueEditor.jsp?CM=" + command + "&OBJID=" + issueId;
	if (params)
		editUrl = editUrl +"&"+ params;
	openBlackPopup(editUrl, 960, function(){return $(window).height() - 50}, function () {
		$("#" + issueId).effect("highlight", { color:"#FFFAE4" }, 1500);
	});
}

function openBoardInBlack(boardId, command, params, callback) {
	if (!command)
		command = "ED";
	var editUrl = contextPath + "/applications/teamwork/board/board.jsp?CM=" + command + "&OBJID=" + boardId+"&POP=1";
	if (params)
		editUrl = editUrl + params;
	openBlackPopup(editUrl, function(){return $(window).width()-50}, function(){return $(window).height()}, callback, null, "black");
}

/* Message menu*/
function loadMessageList(el){
	if (!el.parent().is(".arrowHover")){
		$("#messageListPlace")[0].canScroll = true;
		$("#messageListPlace").empty().load(contextPath + "/applications/teamwork/messaging/messageList.jsp", function (ret) {
      if (ret.length>200)
        $(".showAllMessages").show();
      else
        $(".showAllMessages").hide();
			bjs_showMenuDiv("messageListBox", "messageListOpener", false);
		});
	}
}

function loadMessageListMore(el,topMillis, isInpage) {
	var listPlace = el.closest(".messageListPlace");

	if(!listPlace[0].canScroll && !isInpage)
		return;

	listPlace[0].canScroll = false;

	var ce = isInpage ? "&isInPage=true" : "";

	$.get(contextPath + "/applications/teamwork/messaging/messageList.jsp?topMillis="+topMillis+ce,function(res){
		el.parent().remove();
		listPlace.append(res);
		listPlace[0].canScroll = true;
	})
}


$.fn.activateTimeCounter = function(format){
	if(!format)
		format = '%0h<span class="counterLabel">h</span> %0m<span class="counterLabel">m</span><span class="sec"> %0s<span class="counterLabel">s</span></span>';
  var el = $(this);
  var millis = el.data("time");
	var d = new Date(millis).toString();
	el.tinyTimer({ from: d , format: format });
  return el;
};

$.fn.stopTimeCounter = function(){
  var tt = $(this).data("tinyTimer");
  if (tt)
    tt.stop();
  return $(this).removeData("tinyTimer");
};

