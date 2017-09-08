  function showBaloon(target, type, message){
    return target.showBalloon(null,{
      balloon: message,
      oncursor:false,
      bgcolor     : "#FFF8E0",
      bordercolor: "#2F97C6",
      textcolor : "#333",
      addoverlay: true,
      type: type
    });
  }

  $.fn.hintBaloon=function(message,type,width,height,createSkip){
    var ret=[];
    return this.each(function() {
      var target=$(this);
      var msg = $("<div/>").html(message).css({padding:"10px 10px 40px","max-width":width});

      if (createSkip) {
        var skip = $("<div>").html(i18n.HINT_SKIP).addClass('hintSkip');
        msg.append(skip);
        skip.click(function(ev) {
          skipHint(target,type);
        });
      }
      var hint = showBaloon(target, type, msg);
    });
  };


  function skipHint(target, type){
    var type=type;
    var req={CM:"SKIPHINT",type:type};
    $.getJSON(contextPath+"/applications/teamwork/resource/resourceAjaxController.jsp",req);
    target.hideBalloon();
  }
