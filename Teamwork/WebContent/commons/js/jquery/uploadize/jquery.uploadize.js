jQuery.fn.uploadize	= function (opt) {

	var options = {
		fieldName:                  "file",
		maxSize:                    5 * 1024 * 1024, //5MB
		//acceptFileTypes:            /(\.|\/)(gif|jpe?g|png)$/i,
		//onFinishProcess
		//onStartProcess
		multi:                      true,
		preview:                    false,
		fileAreaSelector:           false, // jquery selector to identify where to list file during upload phase. If false use the element itself
		fileAreaAppend:             false, // append or insert on file list
		additionalRequestParameters:false, //map of parameters to add to request {CM:"SV","TASK_ID":2}
		showPlaceHolder             :true, //shows the "Drop here placehoder"
		activeElement               : "body",

		onLoadCallback:function (response) {
			//console.debug(response);
		},
		validateFile:  function (file) {
			//console.debug("validateFile", file);
			if (options.maxSize && file.size > options.maxSize)
				return i18n.UPLOAD_MAX_SIZE_EXCEEDED;
			else if (options.acceptFileTypes && !(options.acceptFileTypes.test(file.type) || options.acceptFileTypes.test(file.name)))
				return i18n.FILE_TYPE_NOT_ALLOWED;
			else
				return true;

		},
		url:           '__I_WANT_404__',  // server side url to manage uploads, you have to change it
    resolvePasteElement: function(event){
      return $(".isUploadizer");
    }
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


	var updateProgress = function (progress, perc) {
		var progressBar = progress.find(".uploadizeProgress");
		progressBar.css("width", perc + "%").html(perc + " %");
	};

	var setFileEndMessage = function (el,progress, resp) {
		if (typeof (options.onFinishProcess) == "function") {
			//console.debug("onFinishProcess");
			options.onFinishProcess.call(el,resp);
		}
		resp = resp ? resp : {ok:false, message:i18n.ERROR_UPLOADING};
		if(!resp.ok)
			resp.message = i18n.ERROR_UPLOADING;

		progress.data("resp", resp);

		var progressBar = progress.find(".uploadizeProgress");
		progressBar.css("width", "100%").html(resp.message).addClass(resp.ok ? "uploadizeOK" : "uploadizeError");

		progress.oneTime(resp.ok ? 1500 : 3000, function () {
			$(this).fadeOut(500, function () {
				var resp = $(this).data("resp");
				$(this).remove();
				if (resp && resp.ok && typeof (options.onLoadCallback) == "function") {
					options.onLoadCallback.call(el,resp)
				}
			});
		});
	};


	var previewfile = function (el, file) {
		var reader = new FileReader();
		reader.onload = function (event) {
			var image = new Image();
			image.src = event.target.result;
			image.width = 250; // a fake resize
			el.append(image);
		};

		reader.readAsDataURL(file);
	};


	var readfiles = function (el, files) {
		//debugger;
		//		console.debug("readFiles", files);

		if (typeof (options.onStartProcess) == "function") {
			//console.debug("onStartProcess");
			options.onStartProcess.call(el,files);
		}


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
					setFileEndMessage(el,progress, {ok:false, message:message});
					continue;
				}
			}

			var xhr = new XMLHttpRequest();
			progress.data("xhr", xhr); // link progress to xhr request

			xhr.open('POST', options.url);

			xhr.upload.progress = progress;
			xhr.upload.xhr = xhr;

			// LOAD: finish loading file
			xhr.onload = function (event) {
				xhr = event.currentTarget;
				//console.debug("load",xhr.readyState,xhr.status,event);

				var message = i18n.FILE_UPLOAD_COMPLETED;
				var resp = {};
				if (xhr.status != 200) {
					resp.message = i18n.ERROR_UPLOADING + ": " + xhr.status;
					resp.ok = false;
				} else {
					resp = JSON.parse(xhr.response);
					if (resp.clientEntryErrors) {
						resp.message = resp.clientEntryErrors[0].error;
						resp.ok = false;
					}
					resp.message = resp.message ? resp.message : i18n.FILE_UPLOAD_COMPLETED;
				}
				setFileEndMessage(el,xhr.upload.progress, resp);
			};

			//ABORT
			xhr.upload.addEventListener("abort", function (event) {
				//console.debug("abort",event)
				setFileEndMessage(el,event.currentTarget.progress, {ok:false, message:i18n.UPLOAD_ABORTED});
			}, false);


			//ERROR on response ---> never called
			xhr.addEventListener("error", function (event) {
				var xhr = event.currentTarget;
				//console.error("error",event)
				setFileEndMessage(el,xhr.upload.progress, {ok:false, message:i18n.ERROR_UPLOADING});
			}, false);

			//ERROR on response ---> never called too
			xhr.upload.addEventListener("error", function (event) {
				//console.error("upload.error",event)
				setFileEndMessage(el,event.currentTarget.progress, {ok:false, message:i18n.ERROR_UPLOADING});
			}, false);

			xhr.upload.onprogress = function (event) {
				if (event.lengthComputable) {
					var complete = (event.loaded / event.total * 100 | 0);
					updateProgress(event.currentTarget.progress, complete);
				}
			};

			var formData = new FormData();
			formData.append(options.fieldName, files[i], files[i].name);
//			console.debug("formData",files[i])
			if (options.additionalRequestParameters) {
				for (var key in options.additionalRequestParameters)
					formData.append(key, options.additionalRequestParameters[key]);
			}

			if (options.preview)
				previewfile(el, files[i]);

			xhr.send(formData);

			// no multi -> break
			if (!options.multi)
				break;
		}
	};


  //NON BUTTARE VIA è da mettere dove viene
  $(document).pasteImageReader(function(result) {
    //console.debug("pasteImageReader: callback")
    var el=options.resolvePasteElement(result.event);
    if (el.size()>0) {
      //console.debug("pasteImageReader: execute");
      readfiles(el, [result.file])
    }
  });


  this.each(function () {
		var el = this;
		var $el = $(this);
		$el.addClass("isUploadizer");
		$el.prop("title", i18n.DROP_HERE);

			//if area is "disabled" do nothing
		if ($el.is("[disabled]"))
			return true;

		//inject input type file
		var inputFile = $("<input type='file'" + (options.multi ? "multiple" : "") + ">")
				.addClass("uploadizeInputFile")
				.change(function () {
					readfiles($el, this.files);
				});
		$el.after(inputFile);

		if (test_dnd) {

			document.dragIsLeft = false;
			el.counter = 0;

			var activeEl = $(options.activeElement);

			$el.attr("data-dropel", options.activeElement);

			activeEl.on("dragenter",function (e) {

				if ($el.is("[disabled]"))
					return true;

				el.counter++;

				clearTimeout(this.leave);

				//console.debug(e.type,e.target, counter);

				e.preventDefault();

				activeEl.addClass("uploadizeDragOver");

				return false;

			}).on("dragover",function (e) {

				if ($el.is("[disabled]"))
					return true;

				e.originalEvent.dataTransfer.dropEffect = 'copy';

				e.preventDefault();
				return false;

			}).on("dragend dragleave",function (e) {

				if ($el.is("[disabled]"))
					return true;


				el.counter--;
				e.preventDefault();

				if($(e.target).is(activeEl)){
					this.leave = setTimeout(function(){
						activeEl.removeClass("uploadizeDragOver");
					},100);
				} else {

					if(el.counter == 0)
						activeEl.removeClass("uploadizeDragOver");
				}

				return false;

			}).on("drop",function (e) {

				activeEl.removeClass("uploadizeDragOver");
				e.originalEvent.dataTransfer.dropEffect = 'none';
				el.counter = 0;

				if ($el.is("[disabled]"))
					return false;

				e.preventDefault();

				$el.css({visibility:"visible"});
				$el.find(".uploadizeDropHere").remove();
				readfiles($el, e.originalEvent.dataTransfer.files);
			});

			$el.on("click", function (e) {
				$el.next(".uploadizeInputFile").click();
				e.preventDefault();
				return false;
			});

		} else {
			inputFile.show();
		}

	});
	return this;
};

var disableUploadize = function(){
		$(".isUploadizer").each(function(){
			$(this).attr("disabled", "disabled");
		})
};

var enableUploadize = function(){
	$(".isUploadizer").each(function(){
		$(this).removeAttr("disabled");
	})
};

$(document).on("mouseover", ".uploadizeDragOver", function(){
	$(this).removeClass("uploadizeDragOver");
});

$.fn.pasteImageReader = function(options) {
  //console.debug("pasteImageReader")
	if (typeof options === "function") {
		options = {
			callback: options
		};
	}
	options = $.extend({}, {callback: $.noop,matchType: /image.*/}, options);

	return this.each(function() {
		var $this, element;
		element = this;
		$this = $(this);
		return $this.off("paste").on('paste', function(event) {
      //console.debug("pasteImageReader: paste",event.originalEvent.clipboardData);

			var clipboardData;
			clipboardData = event.originalEvent.clipboardData;

			if(!clipboardData || !clipboardData.items){

				$(".uploadizeDrop").addClass("pasting");
				setTimeout(function(){
					var imgSrc = $(".uploadizeDrop img").attr("src") || "";
					$(".uploadizeDrop").empty();
					$(".uploadizeDrop").removeClass("pasting");

					if(!imgSrc.length) return;

					if(imgSrc.indexOf("webkit-fake-url:")>=0 ){
						alert("Your browser doesn't support copy/paste of images.");
						return;
					}

					function dataURItoBlob(dataURI,mime) {
						var BASE64_MARKER = ';base64,';
						var base64Index = dataURI.indexOf(BASE64_MARKER);
						dataURI = dataURI.substring(base64Index + BASE64_MARKER.length);
						// convert base64 to raw binary data held in a string
						// doesn't handle URLEncoded DataURIs
						// separate out the mime component
						//dataURI= dataURI.substring(dataURI.indexOf(',')+1,dataURI.length);
						// write the ArrayBuffer to a blob, and you're done
						var byteString = window.atob(dataURI);
						// write the bytes of the string to an ArrayBuffer
						//var ab = new ArrayBuffer(byteString.length);
						var ia = new Uint8Array(byteString.length);
						for (var i = 0; i < byteString.length; i++) {
							ia[i] = byteString.charCodeAt(i);
						}
						return new Blob([ia], { type: mime });
					}
					var blob = dataURItoBlob(imgSrc, "image/jpg");
					blob.name = "pasteBoard_" + new Date().getTime() + ".jpg";

					return options.callback.call(element, {
						dataURL: imgSrc,
						event: event,
						file: blob
					});

				},10);

				return;
			}

			setTimeout(function(){$(".uploadizeDrop").empty();},10);

			var files=[];
			for (var i=0; i<clipboardData.items.length; i++){
				var item=clipboardData.items[i];
				var type = item.type;
				//console.debug("type", type)

				if (type.match(options.matchType)) {
					var file = item.getAsFile();
        //else
        //  file=new Blob("aaaaaaaaaa", {type: type});
					file.name="clipboard_"+new Date().format("yyyyMMddHHmmss")+"."+(type.split("/")[1]);

					//console.debug("file.name",file.name);
					var reader = new FileReader();
					reader.onload = function(evt) {
						return options.callback.call(element, {
							dataURL: evt.target.result,
							event: evt,
							file: file
						});
					};
					reader.readAsDataURL(file);
					break;
				}
			}
		});
	});
};




