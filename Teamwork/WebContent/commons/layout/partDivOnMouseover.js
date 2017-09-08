  var divomoHoverTimer = 2000;
  var divomoCloseAfter = 1000;

  function bjs_showMenuDiv(divId, bjsId, centered) {
    //console.debug(typeof (bjsId),bjsId,typeof (bjsId)!="object")
    var opener=bjsId;
    if (typeof (opener)!="object")
      opener = $("#" + bjsId);

    if(opener.is("[disabled]"))
      return;

    $(document).off("click.divomo");
    clearTimeout(opener[0].delayOpen);
    opener.off("mouseleave");

    if(opener.parent().is(".arrowHover")){
      $(".arrowHover").removeClass("arrowHover");
      bjs_hideDivOnMouseover(divId);
      return;
    }

    if(typeof centered == "undefined")
      centered = true;

    $(".divomo").hide();

    var divomo = $("#" + divId);

    divomo.appendTo("body").slideDown(100, function(){
      $(this).css({overflow:"visible"});
    });

    setTimeout(function(){
      opener.off("mouseleave.divomo").on("mouseleave.divomo", function(){
	      clearTimeout(el._timerIdForDivOnMouseOver);
	      div_hideDivOnMouseoverWithTimeout(divId);
        clearTimeout(opener[0].delayOpen);
      });

      divomo.off("mouseenter.divomo").off("mouseleave.divomo");
      divomo.on("mouseenter.divomo",function(){
        clearTimeout(this._timerIdForDivOnMouseOver);
      }).on("mouseleave.divomo", function(){
        div_hideDivOnMouseoverWithTimeout(divId);
        clearTimeout(opener[0].delayOpen);
      });

      $(document).on("click.divomo",function(e){
        var isSelf = $(e.target).is(".divomo") || $(e.target).parents(".divomo").length >0;
        if(!isSelf)
          $("body").oneTime(10,"bjs_hideDivOnMouseover",function(){bjs_hideDivOnMouseover(divId)});
      });

      $(".divomo .button").not(".maintain").one("click",function(){
        bjs_hideDivOnMouseover(divId);
      });
      opener.parent().addClass("arrowHover");
    },50);

    nearBestPosition(opener, divId, centered);

    $(".divomoArrow",divomo).css({left:(opener.offset().left - divomo.offset().left)+ (opener.outerWidth()/2) - 12});

    setTimeout(function(){
      $(window).on("resize.divomo", function(){
        nearBestPosition(opener, divId, centered);
        $(".divomoArrow", divomo).css({left:(opener.offset().left - divomo.offset().left)+ (opener.outerWidth()/2) - 12});
      });
    },300);

    var el = divomo.get(0);
    clearTimeout(el._timerIdForDivOnMouseOver);
  }

  function bjs_showMenuDivOnMouseOver(divId, bjsId, centered) {

    var opener = $("#" + bjsId);

    if(opener.is("[disabled]"))
      return;

    if(opener.parent().is(".arrowHover")){
      return;
    }
    opener.off("mouseleave").on("mouseleave", function(){
      div_hideDivOnMouseoverWithTimeout(divId);
      clearTimeout(opener[0].delayOpen);
    });

    if(typeof centered == "undefined")
      centered = true;

    opener[0].delayOpen = setTimeout(function(){
      $(".divomo").hide();
      $("#"+divId).appendTo("body").slideDown(100, function(){
        $(this).css({overflow:"visible"});
        $(this).on("mouseenter",function(){
          var el = $("#"+divId).get(0);
          clearTimeout(el._timerIdForDivOnMouseOver);
        }).on("mouseleave", function(){
          div_hideDivOnMouseoverWithTimeout(divId);
          clearTimeout(opener[0].delayOpen);
        });
      });

      setTimeout(function(){
        $(".divomo .button").not(".maintain").one("click",function(){
          bjs_hideDivOnMouseover(divId);
        });
        opener.parent().addClass("arrowHover");
      },50);

      nearBestPosition(bjsId, divId, centered);
      $(".divomoArrow", $("#"+divId)).css({left:($("#"+bjsId).offset().left - $("#"+divId).offset().left)+ (opener.outerWidth()/2) - 12});

      setTimeout(function(){
        $(window).on("resize.divomo", function(){
          nearBestPosition(bjsId, divId, centered);
          $(".divomoArrow", $("#"+divId)).css({left:($("#"+bjsId).offset().left - $("#"+divId).offset().left)+ (opener.outerWidth()/2) - 12});
        });
      },300);

      opener.parent().addClass("arrowHover");

      var el = $("#"+divId).get(0);
      clearTimeout(el._timerIdForDivOnMouseOver);

    },divomoHoverTimer)

  }

  function bjs_hideDivOnMouseover(divid) {
    $(".arrowHover").removeClass("arrowHover");
    $("#"+divid).slideUp(100,function(){$(this).trigger("hideDivOnMouseover")});
    $(document).off("click.divomo");
    $(window).off("resize.divomo");

  }

  function div_hideDivOnMouseoverWithTimeout(divId) {
    var el = $("#"+divId).get(0);
    el._timerIdForDivOnMouseOver = null;

    el._timerIdForDivOnMouseOver = setTimeout("bjs_hideDivOnMouseover('"+divId+"')", divomoCloseAfter);
  }

  function div_showDivOnMouseover(divId) {
    var el = $("#"+divId).get(0);
    clearTimeout(el._timerIdForDivOnMouseOver);
    $("#"+divId).show();
  }
