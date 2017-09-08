<%@ page import="org.jblooming.waf.settings.I18n" %>
<%----------------------------------------------------------  PAGES DEFINITION  ---------------------------------------------------------%>


<!-- -----------------------------------  sticky list page -------------------------------------- -->
<div data-role="page" id="stickyList" Title="<%=I18n.get("MESSAGES")%>" class="stickyList">
  <div data-role="content" class="scroll">
    <div id="stickyListPlace"></div>
  </div>
  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell left inputBox col12">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
  </div>
</div>

<%-- -----------------------------------  notifications page -------------------------------------- --%>
<div data-role="page" id="notifications" title="<%=I18n.get("NOTIFICATION")%>">
  <div data-role="content" class="scroll">
    <div id="notificationsListPlace"></div>
  </div>
  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell left inputBox col12">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
  </div>
</div>

<%----------------------------------------------------------  TEMPLATES  ---------------------------------------------------------%>
<div class="_mobileTemplates">

  <%-- ---------------------------------  STICKY_MESSAGE TEMPLATE ------------------------------------------------------------------------------------------------------------------- --%>
  <%=JST.start("STICKY_MESSAGE")%>
    <div class="listRow sticky" stickyId="(#=id#)" style="color:(#=obj.contrastColor#); border-left:12px solid (#=obj.color#) ">
      <div width="100%">
        <div class="stickyHeader"
             style="position: relative;">
          <small>(#=dateToRelative(obj.creationMillis)#)</small>
          <div style="(#=obj.authorId?'':'display:none'#); font-size: 70%;"><%=I18n.get("MESSAGE_FROM")%>
            <span resourceId="(#=obj.authorId#)" onclick="viewResource($(this))">(#=obj.authorName#)</span></div>
          <h2>(#=obj.title#)</h2>
          <span style="position: absolute; top:0; right: 3px;" class="teamworkIcon medium"
                onclick="removeSticky($(this));">d</span>
        </div>
        <div class="stickyMessage">(#=obj.message#)</div>
      </div>
    </div>
  <%=JST.end()%>
</div>

<%----------------------------------------------------------  DECORATORS  ---------------------------------------------------------%>
<script>
</script>

<%----------------------------------------------------------  STICKY PAGES FUNCTIONS  ---------------------------------------------------------%>
<script>

  function stickyListEnter(event, data, fromPage, isBack){
    var ndo = $("#stickyListPlace").empty();
    if (applicationCache.stickies.length > 0) {
      for (var i in applicationCache.stickies) {
        ndo.append($.JST.createFromTemplate(applicationCache.stickies[i], "STICKY_MESSAGE",true));
      }
    } else {
      ndo.append($.JST.createFromTemplate({}, "NO_ELEMENT_FOUND"));
    }
  }

  function notificationsEnter(event, data, fromPage, isBack) {
    var ndo = $("#notificationsListPlace").empty();

    ndo.load(contextPath + "/applications/teamwork/messaging/messageList.jsp", function (ret) {
      if (ret.length > 200)
        $(".showAllMessages").show();
      else
        $(".showAllMessages").hide();


      /**
       * infinite scroll
       */
      var myScroll = currentPage.data("iscroll");

      if(!myScroll.eventBinded) {
        myScroll.on('scrollEnd', function () {
          if(this.y < this.maxScrollY + 100){
//            console.debug('scrollEnd',this.y, this.maxScrollY);
            $(".loadMore .button").click();
          }
        });
        myScroll.eventBinded = true;
      }
      refreshIscroll();
    });
  }

  /* -------------------------------------------------------------------------------------  STICKY NOTES FUNCTIONS -------------------------------------------------*/
  function checkStickyNotes(){
    var filter = { "CM": "GETSTICKY"};
    callController(filter, function(response) {
      if (response.stickies && response.stickies.length>0){
        applicationCache.stickies=response.stickies;
      }else{
        applicationCache.stickies=[];
      }
      updateStickyCounter();
    });
  }

  function checkNotifications(){
    var filter = { "CM": "CHECKNOTIFICATIONS"};
    callController(filter, function(response) {
      updateNotificationCounter(response.unreadNotifications);
    });
  }

  function updateNotificationCounter(quant) {
    if (quant) {
      $("#notificationCounterPlace").empty().append($.JST.createFromTemplate({count:quant}, "STICKY_COUNT"));
    } else {
      $("#notificationCounterPlace").empty();
    }
  }

  function updateStickyCounter() {
    $("#stickyCount").html(applicationCache.stickies.length )
  }

  function loadMessageListMore(el,topMillis) {
    var listPlace = el.closest("#notificationsListPlace");

    $.get(contextPath + "/applications/teamwork/messaging/messageList.jsp?topMillis="+topMillis,function(res){
      el.parent().remove();
      listPlace.append(res);
      listPlace[0].canScroll = true;

      refreshIscroll();

    })
  }

  function removeAllStickies(){
    var ids=$.map(applicationCache.stickies, function(stk, i){
      return (stk.id);
    });

    var filter = { "CM": "RMSTK","IDS":ids.join()};
    callController(filter, function(response) {
      applicationCache.stickies=[];
      backPage();
      updateStickyCounter();
    });
  }

  function removeSticky(el){
    var row=el.closest("[stickyId]");

    var stkId = row.attr("stickyId");
    var filter = { "CM": "RMSTK","IDS":stkId};
    callController(filter, function(response) {
      row.remove();
      applicationCache.stickies=$.map(applicationCache.stickies, function(stk, i){
        return (stk.id==stkId?null:stk);
      });

      if (applicationCache.stickies.length<=0){
        backPage();
      }
      updateStickyCounter();
    });

  }

  function followMessageLink(el) {
    var href = el.find(".message_link a:first").attr("href");
    if (href)
      self.location.href = href;
  }
</script>



