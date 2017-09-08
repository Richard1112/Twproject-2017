/*******************************************************************************
 *
 * confirm
 * Author: mbicocchi
 * Creation date: 30/04/15
 *
 ******************************************************************************/

// ------------------ jQuery.confirm
$.fn.confirm = function (action, message) {
	var timer = 6000;
	if (typeof(action) != "function")
		return;

	$(".confirmBox").remove();

	this.each(function () {
		var el = $(this);
		var div = $("<div>").addClass("confirmBox").
				html(message ? message : i18n.DO_YOU_CONFIRM);
		div.css({"min-width":el.outerWidth(), "min-height":el.outerHeight()});
		div.oneTime(timer, "autoHide", function () {
			$(this).fadeOut(100, function () {
				el.show();
				$(this).remove();
			});
		});

		var no = $("<span>").addClass("confirmNo").html(i18n.NO).click(function (ev) {
			ev.stopPropagation();
			el.show();
			$(this).parent().hide().stopTime("autoHide").remove();
		});

		var yes = $("<span>").addClass("confirmYes").html(i18n.YES).click(function (ev) {
			ev.stopPropagation();
			$(this).parent().hide().stopTime("autoHide").remove();
			el.show();
			action.apply(el.get(0));
		});

		div.append("&nbsp;&nbsp;")
				.append(yes)
				.append("&nbsp;/&nbsp;")
				.append(no);

		el.after(div);

		var l = el.position().left + el.outerWidth() - div.outerWidth() + 10;

		if( (l + div.outerWidth()) > $(window).width() ){
			var diff = (l + div.outerWidth()) - $(window).width();
			l = l - diff;
		}

		div.css({marginTop: - ( div.offset().top - el.offset().top + 10 ), left: l });

		var lOffset = div.offset().left;
		if(lOffset < 0 ){
			l = 0;
		}
		div.css({left: l });

	});
	return this;

};
