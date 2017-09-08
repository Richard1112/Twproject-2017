<%@ page buffer="16kb" %><%@ page import="org.jblooming.operator.Operator,
                 org.jblooming.scheduler.Scheduler,
                 org.jblooming.security.PlatformPermissions,
                 org.jblooming.system.SystemConstants,
                 org.jblooming.tracer.Tracer,
                 org.jblooming.utilities.HttpUtilities,
                 org.jblooming.utilities.JSP,
                 org.jblooming.utilities.StringUtilities,
                 org.jblooming.waf.SessionState,
                 org.jblooming.waf.configuration.LoaderSupport,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.constants.Fields,
                 org.jblooming.waf.constants.OperatorConstants,
                 org.jblooming.waf.constants.SettingsConstants,
                 org.jblooming.waf.html.core.HtmlBootstrap,
                 org.jblooming.waf.html.core.JST,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.html.display.FeedbackFromController, org.jblooming.waf.html.display.HeaderFooter, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.settings.PersistenceConfiguration, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.Iterator, java.util.Locale, org.jblooming.security.License" %><%
    HeaderFooter headerFooter = (HeaderFooter) JspIncluderSupport.getCurrentInstance(request);

    PageState pageState = PageState.getCurrentPageState(request);

/*________________________________________________________________________________________________________________________________________________________________________


  HEADER

________________________________________________________________________________________________________________________________________________________________________

*/
    if (Fields.TRUE.equals(request.getAttribute(HeaderFooter.HEADER))) {
        headerFooter.toolTip = "Twproject";
%><!DOCTYPE HTML>
<html>
<head>
  <meta http-equiv="X-UA-Compatible" content="IE=EDGE"/>
  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
  <meta id="Viewport" name="viewport" content="initial-scale=1, maximum-scale=1.5, minimum-scale=.5, user-scalable=yes">
  <meta name="apple-mobile-web-app-capable" content="yes"/>

  <link rel="Shortcut Icon" type="image/ico" href="<%=request.getContextPath()%>/favicon.ico">
    <title><%=headerFooter.toolTip%></title>

    <%=headerFooter.meta != null ? headerFooter.meta : ""%>
  <link rel=stylesheet href="<%=request.getContextPath()%>/commons/skin/default/platform.css?_=<%=ApplicationState.getBuild()%>" type="text/css">
  <%--hack for japanese--%>
  <%if(SessionState.getLocale().getLanguage().equals(Locale.JAPANESE.getLanguage())){ %>
    <link rel=stylesheet href="<%=request.getContextPath()%>/commons/skin/japanese.css?_=<%=ApplicationState.getBuild()%>" type="text/css">
  <%}%>


  <link rel=stylesheet href="<%=request.getContextPath()+"/commons/skin/default/print.css?_="+ApplicationState.getBuild()%>" type="text/css" media="print">
  <%if (I18n.isActive("CUSTOM_FEATURE_CUSTOM_CSS")){
  String customCSSFile = ApplicationState.applicationSettings.get("customCSSFile");
  if(JSP.ex(customCSSFile)){
  %><link rel=stylesheet href="<%=request.getContextPath()+ "/applications/teamwork/" + customCSSFile%>" type="text/css"><%
  }
  }%>

  <script src="<%=request.getContextPath()+"/commons/js/jquery/jquery-2.1.3.min.js"%>?<%=ApplicationState.getBuild()%>"></script>
  <script src="<%=request.getContextPath()+"/commons/js/jquery/ui/jquery-ui-1.10.4.min.js"%>?_=<%=ApplicationState.getBuild()%>"></script>
  <script src="<%=request.getContextPath()+"/commons/js/jquery/UI-touch-punch.js"%>?_=<%=ApplicationState.getBuild()%>"></script>
  <script src="<%=request.getContextPath()+"/commons/js/jquery/jquery.livequery.1.1.1.min.js"%>?_=<%=ApplicationState.getBuild()%>"></script>

  <script src="<%=request.getContextPath()+"/commons/js/platform.js.jsp"%>?_=<%=ApplicationState.getBuild()%>"></script>
  <script src="<%=request.getContextPath()+"/commons/js/i18nJs.jsp"%>?_=<%=ApplicationState.getBuild()%>"></script>

  <script>
      if ('ontouchstart' in window && window.outerWidth<600 && readCookie("browseTwAsDefault") != 1 && <%=pageState.getLoggedOperator()!=null%>)
       self.location.href = contextPath + "/mobile";
  </script>
<style type="text/css">
*{
font-family:SimSun;
}
</style>
</head><%
    /*
    ________________________________________________________________________________________________________________________________________________________________________


      BODY

    ________________________________________________________________________________________________________________________________________________________________________

    */
    if (headerFooter.printBody) {
      String bodyClass="";
      if (HttpUtilities.getCookie(request, "rightPanelPinned")!=null){
        bodyClass+="pinned ";
      }
      if(pageState.isPopup())
        bodyClass+="isPopup";



      // --------------------------------------------- SOFT ERROR MANAGED BY FCF -----------------------------------------------------
      Throwable t= (Throwable) session.getAttribute("_SOFTERROR");
      if (t!=null){
        session.removeAttribute("_SOFTERROR");
        PageState.getCurrentPageState(request).addMessageError(I18n.get("ERROR_SECURITY_EXCEPTION")+"<br>("+JSP.w(t.getMessage())+")");
      }


%><body  <%
    final String jsbeString = Operator.getOperatorOption(pageState.getLoggedOperator(),OperatorConstants.FLD_LOGOUT_TIME);
    int logoutTime = 0;
    if (JSP.ex(jsbeString))
        logoutTime = Integer.parseInt(jsbeString);

    PageSeed logout = PageSeed.getConfiguredInstance(SettingsConstants.ROOT_LOGIN);
    logout.setCommand(Commands.LOGOUT);


    %>class="level<%=License.getLicense().level%> <%=JSP.w(bodyClass)%>" level="<%=License.getLicense().level%>" <%

    //CLOSING BODY
    %>><div id="floatWindow"></div>


<script >
  var minutesToLock = <%=logoutTime%>;
  $(function() {
    <%if (logoutTime>0) { %>
    //log out handling
    $("body").everyTime(60 * 1000, "autoLogoff", function() {
      if (minutesToLock > 0) {
        minutesToLock--;
      } else {
        $("body").stopTime("autoLogoff");
        muteAlertOnChange = true;
        if (window != top)
          window.close();
        else
          window.location = "<%=JSP.javascriptEncode(logout.toLinkToHref()) %>";
      }

    }).bind("mousemove",function(){
        minutesToLock=<%=logoutTime%>;
      });   //in case of logout time bind a mousemove on body

  <%} %>

    // keep alive
    $("body").everyTime(600000, "pingServer", function() {

    <%//// add - graziella per evitare il ping  dopo il log out
    if (request.getSession().getAttribute("CMD_LOG_OUT_PARAM_SESSION_KEY") != null) { %>
      $("body").stopTime("pingServer");
    <%}else{%>
      $.get('<%=ApplicationState.contextPath%>/command.jsp');
    <%}%>
    });

  });

  // keep as top
  if (<%=headerFooter.keepAsTop%>){
    if (window != top)
      parent.location.href = this.location.href;
  }


</script>
<%
/*
________________________________________________________________________________________________________________________________________________________________________


  FOOTER

________________________________________________________________________________________________________________________________________________________________________

*/
    }
} else {

    // second frull for bootstrap validate
    for (Iterator iterator = pageState.htmlBootstrappers.iterator(); iterator.hasNext();) {
        HtmlBootstrap htmlBootstrap = (HtmlBootstrap) iterator.next();
        if (!htmlBootstrap.validate(pageState))
            throw new NullPointerException("Invalid validate for object " + htmlBootstrap.getClass().getName() + " of id '" + htmlBootstrap.getDiscriminator() + "'");
    }


    // i18n edit in place
    String i18nEditEnabled="false";
    PageSeed edit = new PageSeed(ApplicationState.contextPath + "/commons/administration/i18nEdit.jsp");
    edit.addClientEntry(Fields.APPLICATION_NAME, pageState.getApplication().getName());
    edit.setCommand(Commands.EDIT);
    if (I18n.EDIT_STATUS_EDIT.equals(I18n.getEditStatus()) && pageState.getLoggedOperator() != null && I18n.getI18nEditingOperatorId() == pageState.getLoggedOperator().getId()) {
      i18nEditEnabled="true";
    }


    String ADMIN_MESSAGE = ApplicationState.applicationSettings.get(SystemConstants.ADMIN_MESSAGE);
    if (!pageState.isPopup() && JSP.ex(ADMIN_MESSAGE) && pageState.getLoggedOperator() != null && pageState.getLoggedOperator().hasPermissionFor(PlatformPermissions.schedule_manage)) {
        if (Scheduler.isRunning()) {
            ApplicationState.applicationSettings.remove(SystemConstants.ADMIN_MESSAGE);
        } else {
          %> <hr> <p align="center"><big class="warning"><%=ADMIN_MESSAGE%></big></p> <hr> <%
        }
    }
  %>
  <div id="_errorTemplates" style="display:none;">
  <%=JST.start("errorTemplate")%>
  <div style="padding:15px 12px" class="FFC_(#=type#) FFC_Global" type="(#=type#)">
    <table cellpadding="0" cellspacing="0" align="center" style="width:100%;">
      <tr>
        <td valign="top" width="1" nowrap><img src="<%=request.getContextPath()%>/img/(#=type.toLowerCase()#).png"></td>
        <td valign="middle" class="_errorTemplateMessage" style="padding-left: 3px">(#if(obj.title){#)
          <b>(#=title#)</b><br>
          (#}#)(#=message#)<br></td>
        <td width="1" valign="top" onclick="$(this).parents('div:first').fadeOut(function(){$(this.remove())})"><span class="teamworkIcon" style="cursor:pointer">x</span></td>
      </tr>
    </table>
  </div>
  <%=JST.end()%>
  </div>

<script>
  $(function() {
    if (<%=pageState.isPopup()%>) {
      window.focus();
    }

    <%--------------  message from controller JST ------%>
    $("#_errorTemplates").loadTemplates().remove();
    var _messagesFromController =<%=FeedbackFromController.getPageStateMessages(pageState)%>;
    if (_messagesFromController.length > 0) {
      for (var i=0;i<_messagesFromController.length;i++) {
        showFeedbackMessage(_messagesFromController[i]);
      }
    }

    <%--------------  i18n edit ------%>
    if (<%=i18nEditEnabled%>){
      $("span[i18n]").css("border", "1px solid red").bind("click", function(e) {
        if (e.shiftKey) {
          window.open("<%=edit.toLinkToHref()%>&<%=Fields.FORM_PREFIX%>code=" + $(this).attr("i18n"), 'i18nwindow', 'height=800,width=1200,scrollbars=yes').focus();
          e.preventDefault();
          return false;
        }
      });
    }

    <%--------------  focused object ------%>
    if (<%=pageState.getFocusedObjectDomId()==null?0:1%>)
      $("#<%=pageState.getFocusedObjectDomId()%>").focus();

    <%-------------- level ------------------%>
    showUpgradeMessage();

  });


  function showUpgradeMessage() {
    var levpg = $(".lreqPage:first");
    if (levpg.size() > 0) {

      var lv =<%=License.getLicense().level%>;
      var reqLev = levpg.hasClass("lreq30") ? 30 : levpg.hasClass("lreq20") ? 20 : levpg.hasClass("lreq10") ? 10 : 0;
      if (lv > 0 && reqLev > lv) {
        var str = reqLev == 0 ? "FULL" : reqLev == 5 ? "FREE" : reqLev == 10 ? "BASIC" : reqLev == 20 ? "ADVANCED" : reqLev == 30 ? "ENTERPRISE" : "TRIAL";

        var wrp = $("<div>").addClass("modalPopup upgradeMessage").css("background", "none");
        var inn = $("<div>").addClass("bwinPopupd").css({"max-width": "360px", "min-height": "200px", top: "20%"}).click(function () {
          location.href = contextPath + "/applications/teamwork/buyTwprojectLicense.jsp?rqlv=" + reqLev;
        });
        inn.append($("<h2>").html('<%=I18n.get("UPGRADE_TITLE")%>'));
        inn.append($("<p>").html('<%=I18n.get("UPGRADE_YOUR_PLAN_%%")%>'.replace("%%", str)));
        //inn.append("<br><br>");
        inn.append($("<p align='center'>").append($("<span>").addClass("button first").html('Twproject shop')));
        wrp.append(inn)
        levpg.after(wrp)
      }
    }
  }
</script><%


  /* ------------------------------------------- htmlDebug START  -------------------------------------------*/
  if (false && ApplicationState.platformConfiguration.development && !headerFooter.isPartFooter) {
    %><div class="noprint"> <small><%
      if (request.getAttribute("time") != null) {
        %>Server side generated in <%=System.currentTimeMillis() - ((Long) request.getAttribute("time"))%> ms.<%
      }
    %></small> </div> <%
  }

if (PersistenceConfiguration.getDefaultPersistenceConfiguration().useHibStats) {
  %><hr>&nbsp;&nbsp;&nbsp;<%=StringUtilities.replaceAllNoRegex(Tracer.traceHibernateEnd(),"\n","&nbsp;&nbsp;&nbsp;")%><hr><%
}

/* ---------------------------------------- htmlDebug END ------------------------------------------- */

  synchronized (this) {
    if (!ApplicationState.platformConfiguration.loadedWithPageContextSettings) {
      LoaderSupport.withPageContextSetup(pageContext);
    }
  }


%></body></html><%
}
%>
