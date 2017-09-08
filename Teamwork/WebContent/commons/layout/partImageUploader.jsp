<%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8" %>
<%@ page import="org.jblooming.utilities.JSP,
                 org.jblooming.waf.html.button.ButtonJS,
                 org.jblooming.waf.html.core.JspHelper,
                 org.jblooming.waf.settings.I18n" %>

<%
  JspHelper imageUploader = (JspHelper) JspHelper.getCurrentInstance(request);
  String imgUrl = (String) imageUploader.parameters.get("imgUrl");
  boolean fixAspectRatio = (Boolean) imageUploader.parameters.get("fixAspectRatio");

%>

<!-- IMAGE UPLOADER -->
<link rel=stylesheet href="<%=request.getContextPath()%>/commons/js/jquery/imageUploader/jquery.Jcrop.min.css" type="text/css">
<script src="<%=request.getContextPath()%>/commons/js/jquery/imageUploader/jquery.Jcrop.js"></script>

<link rel=stylesheet type="text/css" href="<%=request.getContextPath()%>/commons/js/jquery/imageUploader/jquery.imageUploadize.css"/>
<script src="<%=request.getContextPath()%>/commons/js/jquery/imageUploader/jquery.imageUploadize.js"></script>
<script type="text/javascript" src="<%=request.getContextPath()%>/commons/js/jquery/imageUploader/binaryajax.js"></script>
<script type="text/javascript" src="<%=request.getContextPath()%>/commons/js/jquery/imageUploader/exif.js"></script>
<script type="text/javascript" src="<%=request.getContextPath()%>/commons/js/jquery/imageUploader/canvasResize.js"></script>
<style type="text/css">
  .uploadizeDrop{
    /*background-image: none;*/
    background-size: 50px!important;
  }

  #CROP_IMAGE_HELP.inside {
    display: inline-block;
    width: 290px;
    padding-top: 5px;
    position: absolute;
    top: 210px;
    left: 0;
    bottom: 0;
    right: 0;
    margin: auto;
    height: 170px;
    text-align: center;
    line-height: 1.5;
  }

  .buttonBar {
    margin-top: 10px;
  }
</style>

<div class="buttonBar"><%
  ButtonJS save = new ButtonJS(I18n.get("SAVE"), "saveImage($(this));");
  save.additionalCssClass = "first";
  save.toHtml(pageContext);


  if (JSP.ex(imgUrl)) {
    ButtonJS delete = new ButtonJS(I18n.get("DELETE"), "deleteImage($(this));");
    delete.additionalCssClass = "delete";
    delete.confirmRequire = true;
    delete.toHtml(pageContext);
  }
%></div>
<%
  String orUpload = "<a href=\"javascript:void(0)\" onclick=\"$('.uploadizeInputFile').click()\">"+I18n.get("OR_UPLOAD_IMAGE_HELP")+"</a>";
%>
<div id="CROP_IMAGE_HELP" class="textSmall"><%=I18n.get("CROP_IMAGE_HELP", orUpload)%></div>

<script>


$(function () {

  $("#holder").addClass("fullscreen").imageUploadize({
    url:            false,
    preview:        true,
    acceptFileTypes:  /(\.|\/)(gif|jpe?g|png)$/i,
    onStart: function(el){

      if($(".btn-crop").length >0)
        return;

      el.find(".uploadizeInputFile").attr({accept:".jpg, .jpeg, .png, .gif"});
    },
    onDrop: function (e) {

      if($(".btn-crop").length >0)
        return;

      var jCrop_api = $("#holder").data("jcrop_api"); //.removeClass("justOpened")

      if (jCrop_api)
        jCrop_api.destroy();

      $("#holder img").remove();
      $("#CROP_IMAGE_HELP").hide();

    },
    onLoadCallback: function (response) {
      if (response.ok) {

        /*
         var jCrop_api = $("#holder").data("jcrop_api");
         if (jCrop_api)
         jCrop_api.destroy();
         */

        if($("#holder > img").length > 1){
          $("#holder > img:last").remove();
        }
        $("#CROP_IMAGE_HELP").hide();

        startCrop();

      } else {
        $("#CROP_IMAGE_HELP").show();
      }
    }
  });

  if ($("#holder img").size() == 0) {
//    $("#holder").append($("#CROP_IMAGE_HELP").addClass("inside"))
    $("#twInnerContainerPopup .buttonBar").after($("#CROP_IMAGE_HELP").removeClass("inside"))
  } else {
    $("#twInnerContainerPopup .buttonBar").after($("#CROP_IMAGE_HELP").removeClass("inside"))

  }

});


function startCrop() {

  var holder = $("#holder");

  holder.data("canUpload", true);

  var droppedImage = holder.find("img:first");

  //resize dell'immagine droppata
  var imageRatio = droppedImage.width() / droppedImage.height();
  var holderRatio = holder.width() / holder.height();
  if (imageRatio < holderRatio) {
    droppedImage.height(holder.height());
    droppedImage.width("auto");
  } else {
    droppedImage.width(holder.width());
    droppedImage.height("auto");
  }


  holder.cropImage({
    aspectRatio: <%=fixAspectRatio?"holderRatio":"undefined"%>,

    onStart: function () {
      $("#holder").css({zIndex: 10000});
      var overlay = $("<div/>").addClass("crop_overlay").css({position: "fixed", background: "rgba(0,0,0,0.9)", width: "100%", height: "100%", zIndex: 9000, top: 0});
      $("body").prepend(overlay);
      $(".uploadizeFileBox").remove();
      $("body").on("dblclick", ".jcrop-tracker", function (e) {
        e.preventDefault();
        e.stopPropagation();
        $("#cropButton").click();
      });
      $("#holder").data("canUpload", false);
    },

    onCrop: function (img, b64) {
      var w = img.width;
      var h = img.height;
      $("#imageWidth").val(w);
      $("#imageHeight").val(h);
      $("#imageContent").remove();
      var cIdataHidden = $("<input/>").attr({type: "hidden", id: "imageContent", name: "imageContent"}).val(b64);
      holder.removeClass("fullscreen").css({position: "relative", zIndex: 1}).after(cIdataHidden);
      $(".crop_overlay").remove();
      $("#holder").data("canUpload", true);

      $("#twInnerContainerPopup .buttonBar").after($("#CROP_IMAGE_HELP").removeClass("inside"))
      $("#CROP_IMAGE_HELP").show();

    }
  });
}


/*Crop Image methods -----------------------------------------------------*/


$.fn.cropImage = function (opt) {
  var holder = $(this);
  var defaults = {
    minSize:    [100, 100],
    maxSize:    [holder.width(), holder.height()],
    cropWidth:  600,
    cropHeight: 600,
    setSelect:  [ 0, 0, holder.width() / 2, holder.height() / 2 ],
    onCrop:     function () {},
    onStart:    function () {}
  };

  $.extend(defaults, opt);

  var imageToCrop = this.is("img") ? this : this.children("img:first");

  var cropButton = $("<span class='button small first btn-crop' style='color:white'/>").attr({id: "cropButton"}).html("<%=I18n.get("CROP_IMAGE")%>");
  imageToCrop.after(cropButton);

  imageToCrop.attr("src", imageToCrop.data("src"));

  cropButton.on("click", function () {
    var cropResult = $("<canvas/>").attr({id: "cropResult", width: 0, height: 0}).css({background: "#fff"});
    imageToCrop.after(cropResult);

    var canvas = cropResult.get(0);

    canvas.width = imageToCrop.data("w");
    canvas.height = imageToCrop.data("h");
    imageToCrop.css({background: "#fff"});

    if (defaults.cropWidth) {
      var ratio = imageToCrop.data("h") / imageToCrop.data("w");
      canvas.width = defaults.cropWidth;
      canvas.height = defaults.cropWidth * ratio;

    } else if (defaults.cropHeight) {
      var ratio = imageToCrop.data("h") / imageToCrop.data("w");
      canvas.width = defaults.cropHeight / ratio;
      canvas.height = defaults.cropHeight;

    }

    if (!imageToCrop.length) {
      alert("image broken");
      return;
    }

    var image = imageToCrop.get(0),
        $image = $(image),

        imageW = image.width,
        imageH = image.height;

    // Fix for IE 10+ that is unable to get the correct image size
    if (isExplorer) {
      imageW = image.origWidth;
      imageH = image.origHeight;
    }

    var ratioY = imageH / $image.height(),
        ratioX = imageW / $image.width();

    var getX = Math.round(imageToCrop.data("x") * ratioX),
        getY = Math.round(imageToCrop.data("y") * ratioY),
        getWidth = Math.round(imageToCrop.data("w") * ratioX),
        getHeight = Math.round(imageToCrop.data("h") * ratioY);

    var context = canvas.getContext("2d");

    try {
      context.drawImage(image, getX, getY, getWidth, getHeight, 0, 0, canvas.width, canvas.height);
    } catch (e) {
      return;
    }

    var imageType = image.mimetype || "image/jpg";

    //: imageType must be "image/jpg"
    var croppedImageData = canvas.toDataURL(imageType, 1);

    imageToCrop.attr({
      src: croppedImageData
    }).css({
      width:  "100%",
      height: "auto"
    });

    $(canvas).remove();
    cropButton.remove();
    holder.data("jcrop_api").destroy();

    if (defaults.onCrop)
      defaults.onCrop(canvas, croppedImageData);

    return false;

  });


  imageToCrop.Jcrop({
    //onChange: updatePreview,
    onSelect:    function (c) { // show all coords
      imageToCrop.data({
        x:  c.x,
        y:  c.y,
        x2: c.x2,
        y2: c.y2,
        w:  c.w,
        h:  c.h
      });
    },
    bgFade:      true,
    bgOpacity:   .2,
    setSelect:   defaults.setSelect,
    minSize:     defaults.minSize,
    maxSize:     defaults.maxSize,
    aspectRatio: defaults.aspectRatio

  }, function () {
    holder.data("jcrop_api", this);
  });

  if (defaults.onStart)
    defaults.onStart();
};


</script>



