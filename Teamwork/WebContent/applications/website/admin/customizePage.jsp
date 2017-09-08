<%@ page import="com.opnlb.website.page.WebSitePage, com.opnlb.website.portlet.Portlet, com.opnlb.website.portlet.businessLogic.PortletAction, com.opnlb.website.template.Template, com.opnlb.website.util.TemplateManager, net.sf.json.JSONObject, org.jblooming.PlatformRuntimeException, org.jblooming.ontology.PlatformComparators, org.jblooming.operator.Operator, org.jblooming.security.Permission, org.jblooming.waf.ScreenBasic, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.Collections, java.util.List, java.util.Set" %>
<%
  PageState pageState= PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    ScreenBasic screenBasic = ScreenBasic.preparePage(pageContext);
    screenBasic.getBody().areaHtmlClass="lreq20 lreqPage";
    pageState.perform(request,response).toHtml(pageContext);

  } else {
    Operator logged=pageState.getLoggedOperator();
    WebSitePage wsPage=WebSitePage.load(pageState.getEntry("PAGEID").stringValueNullIfEmpty()+"");

    if (wsPage==null || logged==null )
      throw new PlatformRuntimeException("Page not found");


    boolean generalConfig=pageState.getEntry("GENCONF").checkFieldValue();

    if (generalConfig)
      logged.testIsAdministrator();

    if (!wsPage.isCustomizable() && !generalConfig)
      throw new SecurityException("This page is not customizable.");

    //-----------------------------------------------------------  START SHOW PORTLET ---------------------------------------------------------------------

    List<Portlet> portlets = PortletAction.getInstalledPortlets();
    Collections.sort(portlets, new PlatformComparators.NameComparator());

    %>
<script>$("#HOME_MENU").addClass('selected');</script>


    <style type="text/css">
      .portletPlaceHolder{
        background-color: #FFFBE8;
        border:none;
        }
      .portletList {
        width:100%;
        padding:10px;
        background-color: #EEEEEE;
      }

      .portletList div.portlet {
        width: 228px;
        padding: 3px;
        margin: 2px;
        -moz-border-radius:3px;
        -webkit-border-radius:3px;
        *float:left;
        display:inline-block;
        color: #FFFFFF;
        background: #2f97c6 url("<%=request.getContextPath()%>/img/grip.png") no-repeat 5px center;
        background-color: #2F97C6;

      }

      .portletList div.portlet .title {
        padding:3px 3px 3px 18px;
        width: 200px;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
        color: #FFFFFF;
        font-size: 11px;
        font-weight: bold;
        padding: 3px 3px 3px 20px;
        text-transform: uppercase;
      }

      .portletDrag {
        box-shadow: 1px 1px 4px #666;
        -moz-box-shadow: 1px 1px 4px #666;
        -o-box-shadow: 1px 1px 4px #666;
        -webkit-box-shadow: 1px 1px 6px #666;
        background: #9FF8FF !important;
        border: 1px solid #8BD9DF !important;}


      .area {
        border: 0.223em dotted rgb(255, 150, 5);
        min-height: 60px;
        padding: 3px;
        margin: 0;
      }

      .contentOverlay{
        position: absolute;
        top:0;
        left:0;
        width: 100%;
        height: 100%;
        background-color: rgba(255,255,255,.3);
      }

      .portletList .content {
        display: none;
      }

      .templateContainer .content {
        display: block;
      }

      .templateContainer.hideContent  .content{
        display: none;
      }

      .templateContainer .portlet {
        float: none;
          
      }
      .portlet .title {
        cursor:move;
      }
      .templateContainer .title {
        text-transform:uppercase;
        font-size:11px;
        font-weight:bold;
        color:#fff;
        padding:5px 10px;
        margin-bottom: 5px;
        -webkit-border-radius:3px;
        background: #2F97C6;


      }

      .area .closeButton, .portlet .closeButton{
        cursor: pointer;
        float:right;
        padding: 5px;
        text-transform: lowercase;
        font-size: 12px;
        color: #FFFFFF;
      }

      .noCustom .closeButton{
        display:none;
      }

      .noCustom .title{
        cursor:default;
      }
      
      .noCustom{
        background-image: url(<%=request.getContextPath()%>/img/forbidden.gif);
        background-repeat:no-repeat;
        background-position: center;
      }

      .canCustom{border: 1px dotted red;}
      table.containerContent td {padding:0 !important}
      .containerTitle {padding-left:5px !important}

      .ui-sortable-placeholder{
        height: 50px;
      }

    </style>

<h1><%=I18n.get("CHOSEN_PAGE")+ ": <b>" + wsPage.getName() + "</b>  " + " - " + (generalConfig ? I18n.get("GENERAL_CONFIGURATION") : I18n.get("PERSONAL_CONFIGURATION")+": "+logged.getDisplayName())%>


</h1>
<div style="padding: 15px 0">

  <%

    ButtonJS save = new ButtonJS(I18n.get("SAVE"), "saveAll()");
    save.additionalCssClass = "first big";
    save.toHtml(pageContext);

    if (!generalConfig) {
      ButtonJS buttonReset = new ButtonJS(I18n.get("RESET_DEFAULTS"), "resetUserDefaults();");
      buttonReset.confirmRequire = true;
      buttonReset.additionalCssClass = "big";
      buttonReset.toHtml(pageContext);
    }

    if (logged.hasPermissionAsAdmin()) {
      PageSeed pEdit = pageState.pageInThisFolder("pageEditor.jsp", request);
      pEdit.mainObjectId = wsPage.getId();
      pEdit.command = Commands.EDIT;
      ButtonLink bl = new ButtonLink(I18n.get("EDIT_PAGE"), pEdit);
      bl.additionalCssClass = "big";
      bl.toHtml(pageContext);

    }

    PageSeed tp = pageState.thisPage(request);
    tp.addClientEntry("PAGEID", wsPage.getId());
    if (generalConfig) {
      if (wsPage.isCustomizable()) {
        tp.addClientEntry("GENCONF", false);
        ButtonLink personalPage = new ButtonLink(I18n.get("PERSONAL_CONFIGURATION"), tp);
        personalPage.additionalCssClass = "big";
        personalPage.toHtmlInTextOnlyModality(pageContext);
      }
    } else {
      if (logged.hasPermissionAsAdmin()) {
        tp.addClientEntry("GENCONF", true);
        ButtonLink genPage = new ButtonLink(I18n.get("GENERAL_CONFIGURATION"), tp);
        genPage.additionalCssClass = "big";
        genPage.toHtmlInTextOnlyModality(pageContext);

      }
    }


  %>
</div>
<div id="portletList" class="portletList sortBox" pageId="<%=wsPage.getId()%>"><%
    for (Portlet portlet:portlets){
      Set<Permission> portletPermissions = portlet.getPermissions();
      boolean portletIsVisible=true;
      if (portletPermissions!=null && portletPermissions.size()>0) {
        portletIsVisible = false;
        for (Permission permission:portlet.getPermissions()) {
          if (logged.hasPermissionFor(permission)) {
            portletIsVisible = true;
            break;
          }
        }
      }

      if (portletIsVisible){
        %><div id="PRTL_<%=portlet.getId()%>" portletId="<%=portlet.getId()%>" class="portlet">
            <span class="closeButton teamworkIcon" onclick="closePortlet($(this))">x</span>
            <div class="title" title="<%=portlet.getName()%>"><%=portlet.getName()%></div>
            <div style="position: relative;">
            <div class="content"></div>
            <div class="contentOverlay"></div>
            </div>
            </div><%
      }
    }%></div><br style="clear:both;"><%


  Template chosenTemplate = wsPage.getDefaultTemplate();
  String url = "/" + chosenTemplate.getTemplateFile().getFileLocation();


  %><div class="templateContainer"><jsp:include page="<%=url%>"/></div><%

  JSONObject customChoices= TemplateManager.getUserContents(wsPage, logged);
  JSONObject defaultChoices=  TemplateManager.getDeafultContents(wsPage);

  Form f= new Form(tp);
  f.alertOnChange=true;
  f.start(pageContext);
  %><input type="hidden" oldValue="1" id="layout" name="layout" value="0"> <%
  f.end(pageContext);

// ----------------------------------------------  START JAVASCRIPT ----------------------------------------------------------------------
%>
<script type="text/javascript">
  var generalConfig =<%=generalConfig%>;
  var defaultChoices=<%=defaultChoices%>;

  $(function() {
    var placeHolder = $("<div class='portletPlaceHolder'/>").css({height:1});
    $(".portletList").before(placeHolder);
    $(window).scroll(function () {
      if ($(window).scrollTop() > 160) {
        $(".portletPlaceHolder").css({height:$(".portletList").outerHeight()});
        $(".portletList").css({position:"fixed", top:0,width:$(".templateContainer").width(),left:$(".templateContainer").offset().left, "z-index":"3000", "-moz-box-shadow":"#999 0px 1px 7px"})
      } else {
        $(".portletPlaceHolder").css({height:1});
        $(".portletList").css({position:"relative",left:0,"-moz-box-shadow":"none", "z-index":0})
      }
    });

    var areas = $('.templateContainer [areaname]');
    areas.addClass("area");
    $('.area[custom=yes]').addClass("dropBox");
    if (generalConfig)
      $('.area[custom=no]').addClass("canCustom").addClass("dropBox");
    else
      $('.area[custom=no]').addClass("noCustom");

    areas.each(function() {
      $(this).prop("title", $(this).attr("areaname"));
    });

    sortPortletList ();

    //smist portlet on areas
    smistPortlet(<%=customChoices%>);

    //bind sort, drag, drop
    $(".sortBox").sortable({
      revert: true,
      connectWith:".dropBox",
      handle:".title",
      tolerance:"pointer",
      forceHelperSize:true,
      start: function (event, ui) {
        $("body").unselectable();
        $(ui.item).addClass("portletDrag");
        $(".templateContainer").addClass("hideContent");
      },

      //receive: function(event, ui) { alert("receive: drop from another list")},
      stop: function(event, ui) {
        $(ui.item).removeClass("portletDrag");
        $(ui.item).effect("highlight", { color: "#9FF8FF" }, 1000);
        $(".templateContainer").removeClass("hideContent");
        sortExecuted(event,ui);

      }//,
      //stop: sortExecuted

    });
    //bind sort, drag, drop
    $(".dropBox").sortable({
      revert: true,
      connectWith:".dropBox",
      handle:".title",
      tolerance:"pointer",
      forceHelperSize:true,
      start: function (event, ui) {
        $("body").unselectable();
        $(".templateContainer").addClass("hideContent");
      },
      //receive: function(event, ui) { alert("receive: drop from another list")},
      stop: function(event, ui){
        $(".templateContainer").removeClass("hideContent");
        sortExecuted(event, ui);
      }
    });

  });

  function smistPortlet(customChoices) {
    //console.debug("smistPortlet",customChoices,defaultChoices,generalConfig);

    var portletOut={};

    $("[areaname]").each(function(){
      var area=$(this);
      var areaName = area.attr("areaName");
      var portlets=customChoices[areaName];
      if (area.attr("custom")=="yes"){
        if (generalConfig || !portlets || portlets.length==0)
          portlets=defaultChoices[areaName];

      } else {
        var portlets=defaultChoices[areaName];
      }

      if (portlets) {
        $.each(portlets, function (i, portlet) {
          if (portletOut[portlet.id])
            return;

          area.append($("#PRTL_" + portlet.id));
          //console.debug(area,portlet.id)
          loadPortlet(portlet.id);

          portletOut[portlet.id]=portlet.id;
        });
      }

    });

  }

  function sortExecuted(event, ui) {
    //console.debug("sortExecuted");
    var portlet = ui.item;

    // first eventually load portlet
    loadPortlet(portlet.attr("portletId"), function() {
      $("#layout").val(3);
    });
    if (portlet.parents("#portletList").size() > 0)
      portlet.effect("highlight", { color: "#9FF8FF" }, 2000);

    sortPortletList();
  }

  function sortPortletList(){
    var pl=$("#portletList");
    var pts=pl.find("[portletid]");
    pts.sort(function (p1,p2){
      //console.debug(p1,p2)
      var an = $(p1).find(".title").attr("title").toUpperCase();
      var bn = $(p2).find(".title").attr("title").toUpperCase();

      if(an > bn) {
        return 1;
      } else if(an < bn) {
        return -1;
      } else {
        return 0;
      }
    });

    pts.detach().appendTo(pl);
  }

  function loadPortlet(portletId, callback) {
    //console.debug("loadPortlet");
    var portTmpl = $("#PRTL_" + portletId);
    if (portTmpl.attr("loaded") == "OK") {
      if (typeof(callback) == "function")
        callback(portTmpl);

    } else {
      //load the template using ajax
      portTmpl.attr("loaded", "LOADING");
      var req = {"<%=Commands.COMMAND%>":"LOPO","PID":portletId};
      portTmpl.find("div.content").load("ajax/ajaxCustomizationController.jsp", req, function() {
        portTmpl.attr("loaded", "OK");
        if (typeof(callback) == "function")
          callback(portTmpl);
      });
    }
  }


  function closePortlet(button) {
    var portlet = button.parents(".portlet:first");
    $("#portletList").append(portlet);
    sortPortletList();
    portlet.effect("highlight", { color: "#ffffff" }, 2000);
  }

  function saveAll() {
    showSavingMessage();

    var req = {"<%=Commands.COMMAND%>":"<%=generalConfig?"SAVEDEFAULT":"SAVEFORUSER"%>",
      pageId:"<%=wsPage.getId()%>" };

    var areas;
    if (generalConfig)
      areas=$(".area");
    else
      areas=$(".area[custom=yes]");

    areas.each(function() {
      var area = $(this);
      var pids = "";
      area.find(".portlet").each(function(i) {
        var portlet = $(this);
        pids = pids + (i == 0 ? "" : "_") + portlet.attr("portletId");
      });
      req["AREA_" + area.attr("areaname")] = pids;
    });

    $.getJSON("ajax/ajaxCustomizationController.jsp", req, function(response) {
      jsonResponseHandling(response);
      hideSavingMessage();
      if (response.ok) {
        showFeedbackMessage("OK", "<%=I18n.get("PAGE_CORRECTLY_SAVED")%>");
        $("#layout").updateOldValue();
      }
    });


  }

  function resetUserDefaults() {
    var req = {"<%=Commands.COMMAND%>":"RESETUSRDEF",
      pageId:"<%=wsPage.getId()%>" };

    showSavingMessage();

    //put all portlet on list
    $("#portletList").append($(".area .portlet"));

    $.getJSON("ajax/ajaxCustomizationController.jsp", req, function(response) {
      jsonResponseHandling(response);
      if (response.ok) {
        hideSavingMessage();
        smistPortlet(response.mapping);
      }
    });


  }

</script>
<%
  }
%>
