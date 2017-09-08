function Folio(element) {

	element.data("folio", this);
	this.element = element;
	var oldPosition = this.element.css("position");
	if (!oldPosition || (oldPosition != "relative" && oldPosition != "absolute" && oldPosition != "fixed"  ))
		this.element.css("position", "relative");

	// real dimension on screen
	this.pixelWidth = element.width();
	this.pixelHeight = element.height();

	this.folioCanScroll = true;

	// real position on screen
	if (element.position()) {
		this.pixelLeft = element.position().left;
		this.pixelTop = element.position().top;
	}

	//virtual dimensions
	this.width = this.pixelWidth;  // by default as real
	this.height = this.pixelHeight;
	this.top = 0;
	this.left = 0;

	this.scrollBar; //filled if exists

	this.inPercent = false; // settato a true usa in le posizioni in percentuale invece che assolute

	this.addElement = function (domElement, top, left, width, height) {
		domElement.hide();
		domElement.css("position", "absolute");
		domElement.data("paintable", {top: top, left: left, width: width, height: height});
		this.element.append(domElement);
	};

	this.getPixelHeight = function () {
		return this.height / this.pixelHeight;
	};

	this.getPixelHeightPercent = function () {
		return this.height / 100;
	};
	this.getPixelWidth = function () {
		return this.width / this.pixelWidth;
	};
	this.getPixelWidthPercent = function () {
		return this.width / 100;
	};

	this.getScaleHeight = function () {
		return this.pixelHeight / this.height;
	};
	this.getScaleHeightPercent = function () {
		return 100 / this.height;
	};

	this.getScaleWidth = function (forcePixel) {
		return this.pixelWidth / this.width;
	};
	this.getScaleWidthPercent = function () {
		return 100 / this.width;
	};

	this.scrollTop = function (topInVirtualSize) {
		var chiScroll = this.scrollBar ? this.scrollBar : this.element;
		chiScroll.scrollTop((this.pixelHeight / this.height) * topInVirtualSize);
	};
	this.scrollLeft = function (leftInVirtualSize) {
		this.element.get(0).scrollLeft = (this.pixelWidth / this.width) * leftInVirtualSize;
	};

	this.getVirtualLeft = function (leftInPixel) {
		return this.left + leftInPixel * this.getPixelWidth();
	};

	this.getVirtualTop = function (topInPixel) {
		return this.top + topInPixel * this.getPixelHeight();
	};


	this.redraw = function () {

		var scaleFactorW;
		var scaleFactorH;
		var unit = "";

		//refresh size
		this.pixelWidth = element.width();
		this.pixelHeight = element.height();

		if (this.inPercent) {
			unit = "%";
			scaleFactorW = 100 / this.width;
			scaleFactorH = 100 / this.height;

		} else {
			unit = "px";
			// compute scale factors
			scaleFactorW = this.pixelWidth / this.width;
			scaleFactorH = this.pixelHeight / this.height;
		}

		var folio = this;
		this.element.children().each(function () {

			var el = $(this);
			var pasu = el.data("paintable");
			if (pasu) {
				var top = ((pasu.top - folio.top) * scaleFactorH);
				var left = ((pasu.left - folio.left) * scaleFactorW);
				var widht = (pasu.width * scaleFactorW);
				var height = (pasu.height * scaleFactorH);

				el.css({top: top + unit, left: left + unit, width: widht + unit, height: height + unit});
				el.show();
			}
		});
	};

	this.createScrollbar = function (container) {
		var self = this;
		var scrollBar = $("<div/>").addClass("folio_scrollbar");
		this.scrollBar = scrollBar;

		var scrollContent = $("<div>").css({height: this.element.get(0).scrollHeight, position: "relative", width: "100%"}).addClass("folio_scrollbar_content");
		scrollBar.append(scrollContent);

		container.append(scrollBar);
		scrollBar.on("scroll", function () {
			var scrolled = $(this).scrollTop();
			self.element.scrollTop(scrolled);
		});

		function setScrollHeight(el) {
			var spanEventContainerH = $("#spanEvents").outerHeight() + 40;
			//console.debug(spanEventContainerH);
			scrollContent.css({height: el.element.get(0).scrollHeight + spanEventContainerH})
		}

		self.element.on('mousewheel MozMousePixelScroll', function (event) {

			if (!self.folioCanScroll)
				return;

			setScrollHeight(self);

			event.preventDefault();
			var delta = event.originalEvent.wheelDelta || -event.originalEvent.detail;

			var scrollTo = scrollBar.scrollTop() - (delta/4);
			scrollBar.scrollTop(scrollTo);
			return false;
		})
	};

	this.resize = function (params) {
		if (params)
			this.element.css(params);
		this.pixelWidth = element.width();
		this.pixelHeight = element.height();

		if (this.scrollBar)
			this.scrollBar.find("div").css({height: this.element.get(0).scrollHeight});
	}
}
