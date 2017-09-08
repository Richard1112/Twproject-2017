jQuery.fn.containerBuilder = function () {
	return this.each(function () {
		var container = $(this);

		var id = container.prop("id");
		var sendDataToServer = container.is("[saveStatus]");
		var suffix = container.attr("cmdSuffix");
		var status = container.attr("status");
		var containment = container.attr("containment");

		if (container.hasClass("draggable")) {
			container.bind("mousedown", function () {
				container.bringContainerToFront(".container");
			});

			var params = {
				handle:       ".containerTitle",
				stack:        ".container.draggable",
				start:        function () {$("body").unselectable();},
				stop:         function (event, ui) {
					$("body").clearUnselectable();
					if (sendDataToServer) {
						executeCommand("MOVE" + suffix, "DOM_ID=" + id + "&X=" + parseInt(container.position().left) + "&Y=" + parseInt(container.position().top));
					}
					params.originalTop = container.position().top;
					params.originalLeft = container.position().left;
				},
				originalTop:  container.position().top,
				originalLeft: container.position().left
			};

			function kipItVisible() {

				if (typeof containment == "undefined" || typeof containment == "object")
					return;

				container.css({left: params.originalLeft, top: params.originalTop});

				if (container.position().left + container.outerWidth() > $(containment).width())
					container.css({left: $(window).width() - container.outerWidth() - 20});


				if (container.position().top + container.outerHeight() > $(containment).height())
					container.css({top: $(window).height() - container.outerHeight() - 20});


				if (container.position().left <= 0)
					container.css({left: 0});


				if (container.position().top <= 0)
					container.css({top: 0});
			};

			kipItVisible();

			$(window).on("resize.container", function () {
				kipItVisible();
			});


			if (!containment) {
				var dH = ($(document).height() - (container.outerHeight() + 10));
				var dW = ($(document).width() - (container.outerWidth() + 10));
				containment = [10, 10, dW, dH]; //[x1, y1, x2, y2]
			}

			params.containment = containment;
			container.draggable(params);
		}

		if (container.hasClass("resizable")) {

			if (!container.is("[status=ICONIZED]")) {
				var newH = container.outerHeight() - container.find(".containerTitle:first").outerHeight() - 5;
				container.css("height", container.outerHeight());
				container.find(".containerBody:first").css("height", newH);
			}

			container.resizable({
				containment: "parent",
				start:       function () {$("body").unselectable();},
				stop:        function () {
					$("body").clearUnselectable();
					if (sendDataToServer) {
						executeCommand("RESIZE" + suffix, "DOM_ID=" + id + "&W=" + container.outerWidth() + "&H=" + container.outerHeight());
					}

					var containment = container.attr("containment");
					if (!containment) {
						var dH = ($(document).height() - (container.outerHeight() + 10));
						var dW = ($(document).width() - (container.outerWidth() + 10));
						containment = [10, 10, dW, dH]; //[x1, y1, x2, y2]
					}
					container.draggable('option', 'containment', containment);
				},
				resize:      function (e, ui) {
					var container = $(this);

					var newH = container.outerHeight() - container.find(".containerTitle:first").outerHeight() - 5;
					container.find(".containerBody:first").css({height: newH});
				}
			});
		}

		if (container.hasClass("centeredOnScreen")) {
			container.centerOnScreen();
			if (status != "HIDDEN")
				container.show();
		}

		if (container.hasClass("collapsable")) {
			// everything is done by css
		}

		if (container.hasClass("closeable")) {
			container.find(".stsHide").show();
		}

		if (container.hasClass("iconizable")) {
			container.find(".stsIconize").show();

			if (status == "ICONIZED") {
				container.one("click", function () {
					container.trigger("restore");
				});
			}
		}

		//events on container

		container.bind("iconize", function (e) {
			container.attr("status", "ICONIZED");


			executeCommand("ICONIZE" + suffix, "DOM_ID=" + id);

			setTimeout(function () {
				container.one("click", function () {
					container.trigger("restore");
				});

			}, 10);

			e.stopPropagation();
		}).bind("hide", function (e) {
			e.stopPropagation();
			container.hide();
			container.attr("status", "HIDDEN");
			if (sendDataToServer)
				executeCommand("HIDE" + suffix, "DOM_ID=" + id);

		}).bind("show", function (e) {
			e.stopPropagation();
			container.show();
			container.attr("status", "DEFAULT");
			if (sendDataToServer)
				executeCommand("SHOW" + suffix, "DOM_ID=" + id);

		}).bind("collapse", function (e) {
			e.stopPropagation();
			container.attr("status", "COLLAPSED");
			if (sendDataToServer)
				executeCommand("COLLAPSE" + suffix, "DOM_ID=" + id);

		}).bind("toggleCollapse", function (e) {
			e.stopPropagation();
			if (container.is("[status=COLLAPSED]"))
				container.trigger("restore");
			else
				container.trigger("collapse");

		}).bind("restore", function (e) {
			e.stopPropagation();
			container.attr("status", "DEFAULT");
			if (container.hasClass("resizable")) {
				var newH = container.outerHeight() - container.find(".containerTitle:first").outerHeight() - 5;
				container.css("height", container.outerHeight());
				container.find(".containerBody:first").css("height", newH);
			}

			if (sendDataToServer)
				executeCommand("RESTORE" + suffix, "DOM_ID=" + id);

		}).bind("toggle", function (e) {
			e.stopPropagation();
			if ($(this).attr("status") == "HIDDEN")
				container.trigger("show");
			else
				container.trigger("hide");
		}).bind("toggleAnimate", function (e) {
			e.stopPropagation();
			if ($(this).attr("status") == "HIDDEN")
				container.trigger("slideDown");
			else
				container.trigger("slideUp");
		});

	});
};

$.fn.bringContainerToFront = function (selector) {
	//return $(this);

	return this.each(function(){
		var zi = 10;
		var elements = selector ? $(selector) : $("*");
		elements.each(function () {
			if ($(this).css("position") != "static") {
				var cur = parseInt($(this).css('zIndex'));
				zi = cur > zi ? parseInt($(this).css('zIndex')) : zi;
			}
		});
		$(this).css('z-index', zi += 10);

		if(!$(this).is(".alwaysOnTop"))
			$(".alwaysOnTop").bringContainerToFront();
	})

};

