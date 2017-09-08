jQuery.fn.imageUploadize = function (opt) {

	var options = {
		fieldName:                  "file",
		maxSize:                    5 * 1024 * 1024, //5MB
		acceptFileTypes:            /(\.|\/)(gif|jpe?g|png)$/i,
		multi:                      false,
		preview:                    false,
		fileAreaSelector:           false, // jquery selector to identify where to list file during upload phase. If false use the element itself
		fileAreaAppend:             false, // append or insert on file list
		showPlaceHolder             :true, //shows the "Drop here placehoder"

		onLoadCallback:function (response) { },
		onStart:function(){},
		onDrop:function(){},
		validateFile:  function (file) {

			if (options.maxSize && file.size > options.maxSize)
				return i18n.UPLOAD_MAX_SIZE_EXCEEDED;
			else if (options.acceptFileTypes && !(options.acceptFileTypes.test(file.type) || options.acceptFileTypes.test(file.name)))
				return 'file type not accepted';
			else
				return true;

		},
		url:           '__I_WANT_404__'  // you have to change it
	};

	for (var k in opt)
		options[k] = opt[k];

	var test_dnd = 'draggable' in document.createElement('span');
	var test_formdata = !!window.FormData;

	//se non posso fare post nascondo tutto
	if (!test_formdata) {
		$(this).hide();
		return this;
	}

/*
	if(typeof options.onStart == "function")
		options.onStart();
*/

	var updateProgress = function (progress, perc) {
		var progressBar = progress.find(".uploadizeProgress");
		progressBar.css("width", perc + "%").html(perc + " %");
	};

	var setFileEndMessage = function (progress, resp, el) {
		resp = resp ? resp : {ok:false, message:i18n.ERROR_UPLOADING};
		progress.data("resp", resp);

		if(!resp.ok)
			resp.message = i18n.ERROR_UPLOADING;

		var progressBar = progress.find(".uploadizeProgress");
		progressBar.css("width", "100%").html(resp.message).addClass(resp.ok ? "uploadizeOK" : "uploadizeError");

		progress.oneTime(resp.ok ? 1500 : 3000, function () {
			$(this).fadeOut(500, function () {
				var resp = $(this).data("resp");
				$(this).remove();

				//TODO: REMOVE __ ONLY FOR SAFARI 5
				if(!window.FileReader){
					var image = new Image();
					image.src = repositoryURL+"/"+resp.docHref;
					image.style.maxWidth = "100%"; // a fake resize
					el.prepend(image);
				}

				if (resp && resp.ok && typeof (options.onLoadCallback) == "function") {
					options.onLoadCallback(resp);
					$(".uploadizeInputFile").hide();
				}
			});
		});
	};

	var previewfile = function (el, file) {

//		Check if the file is an image
		var isValidImage = (/(gif|jpg|jpeg|png)$/i).test(file.type);

		if(window.FileReader && isValidImage){
			var reader = new FileReader();
			reader.onload = function (event) {

				var image = new Image();
				image.src = event.target.result;
				image.mimetype = file.type;

				var elW = el.width();
				el.prepend(image);

				var origImgW = $(image).width();
				var origImgH = $(image).height();

				$(image).one("load",function(){

					// needed to correct the orientation if image is get from touch devices
					canvasResize(file, {
						width: 1280,
						height: 0,
						crop: false,
						quality: 100,
						//rotate: 90,
						callback: function(data, width, height) {

							$(image).data("src", data);

							if(!options.url){
								if (typeof (options.onLoadCallback) == "function") {
									resp = {ok:true};
									options.onLoadCallback(resp);
									$(".uploadizeInputFile").hide();
								}
							}

						}
					});

				//Bugfix for IE.
					var h = (image.height * elW) / $(this).width();

					$(this).width(elW);
					$(this).height(h);

					//introduced new attributes for the original image size
					this.origWidth = origImgW;
					this.origHeight = origImgH;

				})
			};
			reader.readAsDataURL(file);
		}
	};


	var readfiles = function (el, files) {
		//debugger;
		for (var i = 0; i < files.length; i++) {

			//appned progress element
			var progress = $("<div class='uploadizeFileBox'>").html(files[i].name).click(function () {
				$(this).data("xhr").abort();
			});
			var progressBar = $("<div class='uploadizeProgress'>");
			progress.append(progressBar);

			if (options.fileAreaSelector) {
				if (options.fileAreaAppend)
					$(options.fileAreaSelector).append(progress);
				else
					$(options.fileAreaSelector).prepend(progress);
			} else {
				if (options.fileAreaAppend)
					el.append(progress);
				else
					el.prepend(progress);
			}

			if (typeof (options.validateFile) == "function") {
				var message = options.validateFile(files[i]);
				if (message !== true) {
					setFileEndMessage(progress, {ok:false, message:message}, el);
					continue;
				}
			}


			if (options.preview && 	window.FileReader)
				previewfile(el, files[i]);

			// no multi -> break
			if (!options.multi)
				break;
		}
	};

	this.each(function () {
		var el = $(this);
		el.attr("title", i18n.DROP_HERE);
		el.data("canUpload", true);

		el.unselectable();
		el.find("*").unselectable();

		//if area is "disabled" do nothing
		if (el.is("[disabled]"))
			return true;

		//inject input type file
		var inputFile = $("<input id='uploadizerInputFile' type='file'" + (options.multi ? "multiple" : "") + ">")
				.addClass("uploadizeInputFile")
				.change(function () {
					readfiles(el, this.files);
					if(typeof options.onDrop == "function")
						options.onDrop();
				}).click(function(){
					el.data("canUpload", false);
				});

		el.append(inputFile);

		if(typeof options.onStart == "function")
			options.onStart(el);

		if (test_dnd) {

			// todo: unbind event after upload
			el.find(".uploadizeInputFile").css({position:"absolute", width:"100%", height:"100%", left:0, top:0, opacity:0}).show();

			var counter = 0;

			el.on("dragenter",function (e) {

				if(!el.data("canUpload")){
					$(this).removeClass('hover');
					return false;
				}

				if (el.is("[disabled]"))
					return true;

				counter++;

				e.preventDefault();
				e.stopPropagation();

				$("#holder img").hide();
				$(this).addClass('hover');

				return false;

			}).on("dragleave dragend",function (e) {

				counter--;

				e.preventDefault();
				e.stopPropagation();

				if(counter == 0){
					$(this).removeClass('hover');
					$("#holder img").show();
					return false;
				}

			}).on("dragover",function (e) {
				e.preventDefault();
				e.stopPropagation();
				return false;

			}).on("drop",function (e) {

				counter=0;
				e.preventDefault();
				e.stopPropagation();

				if(!el.data("canUpload")){
					$(this).removeClass('hover');
					return false;
				}

				if(el.get(0).counter && el.get(0).counter==1){
					$(this).find("img").remove();
				}

				$(this).removeClass('hover').find(".uploadizeDropHere").remove();

				$(".uploadizeInputFile").hide();
				e.preventDefault();
				readfiles(el, e.originalEvent.dataTransfer.files);
				el.get(0).counter = el.get(0).counter ? el.get(0).counter++ : 1;
//			      $(this).off("drop");

			}).on("dblclick",function (e) {

				if(!el.data("canUpload")){
					$(this).removeClass('hover');
					return false;
				}

						/*
						 if(el.is(".fullscreen"))
						 return;
						 */

				e.preventDefault();
				e.stopPropagation();

				$(this).find(".uploadizeInputFile").css({position:"absolute", width:"100%", height:"100%", left:0, opacity:0}).show();
				$(this).find("img").remove();
				$(this).find(".uploadizeDropHere").toggle();
				$(this).find(".uploadizeInputFile").click();

/*
				if(typeof options.onDrop == "function")
					options.onDrop();
*/
			})
					.unselectable()

			/*.one("click", function (e) {

				e.preventDefault();
				e.stopPropagation();

				$(this).find(".uploadizeDropHere").toggle();
				//$(this).find(".uploadizeInputFile").click();
			})*/;

		} else {
			inputFile.show();
		}
	});
	return this;
};


$.fn.unselectable=function(){
	return this.each(function(){
		$(this).addClass("unselectable").css({
			"-moz-user-select": "none",
			"user-select": "none"
		}).attr("unselectable","on");
	});
};







