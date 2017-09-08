<%@ page import="com.twproject.waf.TeamworkHBFScreen, org.jblooming.waf.ScreenArea, org.jblooming.waf.html.display.Hint, org.jblooming.waf.view.PageState, org.jblooming.waf.settings.I18n, org.jblooming.waf.settings.ApplicationState" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {
%>

<div class="t" id="1" style="background-color:#F9EFC5; "><h1>in vetta</h1>
  <div id="7" class="t d" style="position:relative;left:650px;top:-40px;width:200px;height:30px;background-color:#ff00cc;"></div>
  <div id="11" class="t d" style="position:relative;left:350px;top:-40px;width:200px;height:30px;background-color:#ff00cc;"></div>
</div>


<div class="t" id="3" style="background-color:#DB2727; width:200px; float:left; height:400px;"><h1>a sinistra<br></h1>
  <div id="12" class="t d" style="position:relative;left:0px;top:20px;width:200px;height:80px;background-color:#ff00cc;"></div>
  <div id="10" class="t d" style="position:relative;left:0px;top:100px;width:200px;height:80px;background-color:#ff00cc;"></div>
</div>

<div class="t" id="2" style="background-color:green; width:200px; float:right; height:400px;"><h1>a destra<br></h1>
  <div class="t" style="background-color:blue;color:white;">dentro ad un'altro<br></div>


  <div id="14" class="t d" style="position:relative;left:0px;top:20px;width:200px;height:80px;background-color:#ff00cc;"></div>
  <div id="15" class="t d" style="position:relative;left:0px;top:100px;width:200px;height:80px;background-color:#ff00cc;"></div>

</div>


<div class="t" id="c" style="background-color:#cccccc; float: left; width:780px;">zona centrale
  <div class="t" id="5" style="background-color:blue;color:white;width:300px;"><h1>uno dove capita
    <span id="s1" style="position:relative"></span></h1></div>
</div>

<p align="right"><span class="t" id="s2" style="position:relative">QUI</span></p>
<div class="t" id="8" style="background-color:#F9EFC5; clear:both;"><h1>in fondo<br></h1>
  <div id="9" class="t d" style="position:relative;left:250px;top:-40px;width:200px;height:30px;background-color:#ff00cc;"></div>
  <div id="13" class="t d" style="position:relative;left:650px;top:-40px;width:200px;height:30px;background-color:#ff00cc;"></div>
</div>

<p align="center">^</p>


<style type="text/css">
  .hintBaloon {
    position: absolute;
    /*border-radius:5px;*/
    padding: 10px;
    z-index: 90000;
    background-color: #eee;
    border-color: #eee;
  }

  .hintBaloon .arrow {
    position: absolute;
    width: 0px;
    height: 0px;

    border-color: transparent;
    border-style: solid;
    border-width: 8px;
  }

  .hintBaloon .arrow.n {
    border-bottom-color: inherit;
    border-bottom-width: 16px;
    border-top-width: 0;
  }

  .hintBaloon .arrow.s {
    border-top-color: inherit;
    border-top-width: 16px;
    border-bottom-width: 0;
  }

  .hintBaloon .arrow.e {
    border-left-color: inherit;
    border-left-width: 16px;
    border-right-width: 0;
  }

  .hintBaloon .arrow.w {
    border-right-color: inherit;
    border-right-width: 16px;
    border-left-width: 0;
  }

</style>

<script type="text/javascript">


  $.fn.hintBaloon=function(message,type,width,height,createSkip){
    var ret=[];
    this.each(function() {
      //console.debug("hintBaloon",message,type,width,height,createSkip);

      var aW=45; // arrow square width
      var aH=45; // arrow square height
      var target=$(this);

      width=width?width:120;
      height=height?height:80;

      //set target as relative
      if (target.css("position")!="absolute" && target.css("position")!="relative")
        target.css("position","relative");

      var $body = $("body");
      var cL=$body.width()/2;
      var cT=$body.height()/2;

      var tT=target.offset().top;
      var tL=target.offset().left;
      var tW=target.outerWidth();
      var tH=target.outerHeight();

      //get center of target
      var center={top:tT+tH/2,left:tL+tW/2};
      var pos;
      if (Math.abs(center.top-cT) > Math.abs(center.left-cL))
      //up or down?
        pos= center.top > cT?"up":"down";
      else
      //left or right
        pos=center.left> cL?"left":"right";

      var posV= center.top > cT?"up":"down";
      var posH=center.left> cL?"left":"right";

      var hint=$("<div>").append(message).attr("hintType",type).css({width:width,height:height});
      var arrow=$("<div>").addClass("hintBaloonArrow");
      hint.append(arrow);

      ret.push(hint);

      var topH,leftH;
      var topA,leftA;
      //up or down
      if (pos=="down" || pos=="up" ){
        if (pos == "down") {
          topH = target.outerHeight() + aH;
          topA = -aH;
          arrow.addClass("n");
        } else {
          topH = -aH - height;
          topA = height;
          arrow.addClass("s");
        }
        leftH=cL-tL-width/2;
        if(leftH>tW-aW)
          leftH=tW-aW;
        else if(leftH<width+aW && leftH+width<aW)
          leftH=-width+aW;

        leftA= (Math.max(leftH, 0)+Math.min(width+leftH,tW))/2 -leftH  -aW/2;
        if(leftA+aW>width)
          leftA=width-aW;
        leftA=leftA<0?0:leftA;

        // left or right
      } else{
        if (pos=="right"){
          leftH=target.outerWidth()+aW;
          leftA=-aW;
          arrow.addClass("w");
        } else {
          leftH=-width-aW;
          leftA=width;
          arrow.addClass("e");
        }
        topH=cT-tT-height/2+aH;
        if(topH>tH-aH)
          topH=tH-aH;
        else if(topH<height+aH  && topH+height<aH)
          topH=-height+aH;

        topA= (Math.max(topH, 0)+Math.min(height+topH,tH))/2 -topH  -aH/2;

        if(topA+aH>height)
          topA=height-aH;
        topA=topA<0?0:topA;
      }
      hint.css({top:topH,left:leftH}).addClass("hintBaloon");
      arrow.css({top:topA,left:leftA});
      target.append(hint);


      //inject skipper
      if (createSkip) {
        var skip = $("<div>").html("<%=I18n.get("HINT_SKIP")%>").addClass('hintSkip').click(function(ev) {
          skipHint($(this).closest("[hintType]"));
          ev.stopPropagation();
          return false;
        });
        hint.append(skip);
      }

      ret.push(hint.get(0));
    });
    return $(ret);
  };


  function skipHint(hint){
    var type=hint.attr("hintType");
    var req={CM:"SKIPHINT",type:type};
    $.getJSON(contextPath+"/applications/teamwork/resource/resourceAjaxController.jsp",req);
    hint.fadeOut(200,function(){$(this).remove();});
  }



  $(function () {
    var d = $("<div>").css({backgroundColor: "red", width: "50px", height: "40px"});

    $(".d").each(function () {$(this).hintBaloon(d.clone(), "", {borderColor: "#80ff80", backgroundColor: "#ffff80", padding: 5})})

    $("#1").hintBaloon("sdfsdg gf fsdg fsdg ", "", {borderColor: "#80ff80", backgroundColor: "#ffff80", padding: 5})

    var v = $("<div>").css({"border-top": "1px solid red", position: "absolute", width: "100%", height: "1px", top: $("body").height() / 2});
    var h = $("<div>").css({"border-left": "1px solid red", position: "absolute", height: "100%", width: "1px", top: 0, left: $("body").width() / 2});
    $("body").append(v).append(h);

  });


</script>

<style type="text/css">



  .hintBaloon{
    position:absolute;
    background-color: #FAFFAA;
    padding: 20px;
    font-size: 16px;
    color:#444;
    border-radius:10px;
    z-index:90000;
    box-shadow: 2px 2px 6px #778d8f;
    -moz-box-shadow: 2px 2px  6px #778d8f;
    -webkit-box-shadow: 2px 2px  6px #778d8f;
    -o-box-shadow: 2px 2px  6px #778d8f;
    text-align:left;
    font-size: 16px;
    line-height: 22px;
    font-family: Courier, "Courier New", monospace
  }


  .hintBaloonArrow{
    position:absolute;
    background-color: #ccff00;
    width:45px;

    border-color:#ccff00;
    border-style:solid;
    border-width:0;
  }

  .hintBaloonArrow.n{
    <%--background: transparent url('<%=ApplicationState.contextPath%>/applications/teamwork/images/arrow-n.png') no-repeat top left;--%>
    margin-left:5px;
    height: 45px;
  }
  .hintBaloonArrow.s{
    <%--background: transparent url('<%=ApplicationState.contextPath%>/applications/teamwork/images/arrow-s.png') no-repeat top left;--%>
    margin-left:5px;
    height: 45px;
  }
  .hintBaloonArrow.e{
    <%--background: transparent url('<%=ApplicationState.contextPath%>/applications/teamwork/images/arrow-e.png') no-repeat top left;--%>
    margin-top:5px;
    height: 45px;
  }
  .hintBaloonArrow.w{
    <%--background: transparent url('<%=ApplicationState.contextPath%>/applications/teamwork/images/arrow-w.png') no-repeat top left;--%>
    margin-top:5px;
    height: 45px;
  }

  .hintSkip {
    position:absolute;
    bottom:10px;
    right:20px;
    font-size: 13px;
    color:#28B0DF;
    cursor:pointer
  }
</style>

<%
    /*
        Hint.HintWriter hintWriter = new Hint.HintWriter();
        if ( pageState.href.contains("resourceEditor") ) {
          hintWriter.addHint("qui", "#1,.d", 300, 200, true, pageState);
        }
        hintWriter.toHtml(pageContext);
        */
  }
%>

