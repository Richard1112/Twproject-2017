<%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8" %>
<%@ page import="org.jblooming.security.License,
                 com.twproject.setup.TeamworkConfiguration,
                 org.jblooming.agenda.CompanyCalendar,
                 org.jblooming.operator.Operator,
                 org.jblooming.oql.OqlQuery,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.ScreenBasic,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.container.TabSet,
                 org.jblooming.waf.html.display.PercentileDisplay,
                 org.jblooming.waf.settings.ApplicationState,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState, org.jblooming.utilities.StringUtilities, com.twproject.security.LicenseUpdater, org.jblooming.waf.html.input.TextArea, org.jblooming.waf.html.button.ButtonJS" %>
<%
    PageState pageState = PageState.getCurrentPageState(request);

    pageState.getLoggedOperator().testIsAdministrator();

    if (!pageState.screenRunning) {
        ScreenBasic.preparePage(pageContext);
        pageState.perform(request, response).toHtml(pageContext);
    } else {
        boolean teamwork_host_mode = ApplicationState.isHostMode;
%>
<script>$("#ADMINISTRATION_ROOT_MENU").addClass('selected');</script>
<%


    PageSeed configPage = pageState.pageFromRoot("administration/teamworkGlobalSettings.jsp");
%>

<div class="mainColumn adminIntro">
<div class="container inlineContainerWrapper boxLicenseInfo">
  <%------------------------------------------------------ CONFIGURATION LEVEL & LICENSE -----------------------------------------------------%>

    <%-------------------------------------------------------------------------  CONFIGURATION LEVEL ----------------------------------------------------------------%>
    <div class="container" style="width: 420px;">

      <div class="inverse" style="position:relative; height: 130px;">
        <%
          TeamworkConfiguration.evaluateLevel(pageState);
          double totalScore = 0;
          double score = 0;
        %>
        <div id="CONFIGURATION_HELP" style="display: none;">
          <h2>What is configured?</h2><%
          for (TeamworkConfiguration.ConfigurationStep cs : TeamworkConfiguration.configurationSteps.values()) {
            if (cs.done)
              score += cs.points;
            totalScore += cs.points;
        %><span class="checklistEl <%=cs.done ? "done" : ""%>"><%=cs.description%> <label><%=cs.points%></label></span> <br><%
          }
          double perc = (score / totalScore) * 100;
        %>
        </div>
        <table cellpadding="0" cellspacing="0">
          <tr>
            <td>
              <div id="CONFIGURATION_LEVEL" style="padding: 15px"></div>
              <script>$(function(){drawCircle("CONFIGURATION_LEVEL", <%=perc%>, 50, 5, circleColors);})</script>
            </td>
            <td colspan="2">
              <label style="padding-left: 20px; font-size: 1.4em; font-weight: 200">Your current<br>configuration level</label>
              <span class="teamworkIcon inverse helpOpener" onclick="openPopUp($('#CONFIGURATION_HELP'),500,600)">?</span>
            </td>
          </tr>
        </table>
      </div>
    </div>

    <%------------------------------------------------------------------------- LICENSE ----------------------------------------------------------------%>
    <div class="container">
      <ul class="licenseBox">
        <%
          License license=License.fromFile();

          //warn if close to limit
          String hql = "select count(op) from " + Operator.class.getName() + " as op where op.enabled = :truth";
          OqlQuery oql = new OqlQuery(hql);
          oql.getQuery().setBoolean("truth", Boolean.TRUE);
          long totOp = (Long) oql.uniqueResult();
          long daysRemaining = (license.expires.getTime() - System.currentTimeMillis()) / CompanyCalendar.MILLIS_IN_DAY;
        %>
        <li>Licensed to <strong><%=license.customerCode%></strong></li>
        <li>License level <strong><%=license.getLevelName()%></strong></li>
        <li class="<%=license.licenses<totOp+3?"warning":""%>">User active/licensed <strong><%=totOp%> of <%=license.licenses%></strong></li>
        <li class="<%=daysRemaining < 10?"warning":""%>">Expiration date <strong><%=JSP.w(license.expires)%></strong></li>
      </ul>
    </div>



    <%------------------------------------------------------------------------- REGISTER LICENSE ----------------------------------------------------------------%>
    <%if (!teamwork_host_mode) {%>
    <div class="container">

      <ul>
        <li><%ButtonLink.getBlackInstance("Register license", 500, 600, pageState.pageFromRoot("/administration/register.jsp")).toHtmlInTextOnlyModality(pageContext);%>
          <label>insert here your updated Twproject license</label>
        </li>

        <li><%
          PageSeed cu = new PageSeed("https://shop.twproject.com/utils/checkUpdates.jsp");
          cu.addClientEntry("TW_CURRENT_VERSION", ApplicationState.getApplicationVersion());
          cu.command = "CHECK_UPDATES";
          ButtonLink.getBlackInstance("Check for updates", 500, 500, cu).toHtmlInTextOnlyModality(pageContext);
        %><label>is your Twproject up-to-date?</label>
        </li>

        <%
          pageState.addClientEntry("lopf", StringUtilities.encryptBase64(license.getLicenseWrapped(), LicenseUpdater.key));  //double encryption
          TextArea ta = new TextArea("","lopf","",100,10,"style='display:none;'");
          ta.toHtml(pageContext);



        %><li><%new ButtonJS("Check license online","licUpd();").toHtmlInTextOnlyModality(pageContext);%>
        <label>Click above if you want to resync your license with the one generated with your last purchase.</label></li>


      </ul>
      <script>
        function licUpd(){

          var request={CM:"CKNEWLIC"};
          request.lopf=$("#lopf").val();
          showSavingMessage();
          $.ajax({
            url: "https://shop.twproject.com/utils/licenseUpdate.jsp",
            data:request,
            dataType:"jsonp",
            jsonp:   "__jsonp_callback",
            success: function (response) {
              if (response) {
                jsonResponseHandling(response);
                if (response.ok) {
                  //si deve salvare la licenza
                  if (response.nl){

                    if (response.nl!=request.lopf){

                      var req={CM:"SVNEWLIC",nl:response.nl};
                      showSavingMessage();
                      jsonResponseHandling(response);
                      $.getJSON("administration/adminAjaxController.jsp",req,function(response){
                        jsonResponseHandling(response);
                        if (response.ok){
                          alert("License updated correctly.\nYour last purchased license has been installed correctly.");
                          location.href = "administration/administrationIntro.jsp";
                        }
                      })
                    } else {
                      alert("Your license is up-to-date.\n If you have just completed a purchase wait a few minutes and then retry.");
                    }
                  }
                }
                hideSavingMessage();

              } else {
                showFeedbackMessage("ERROR", "Uknown error refreshing license. Please contact us.", "License updater")
              }
            }
          });
        }
      </script>
    </div>
    <%}%>

<%------------------------------------------------------------------------- SHOP BUTTON ----------------------------------------------------------------%>
<div class="container">
  <%
    PageSeed licLink = pageState.pageFromRoot("buyTwprojectLicense.jsp");
    ButtonLink licLinkB = new ButtonLink("Go to Twproject shop", licLink);
    licLinkB.additionalCssClass = "big first cart";
    licLinkB.toHtml(pageContext);
  %>
  <p style="text-align: center; padding: 10px 0"><label><a href="mailto:info@twproject.com?su=Twproject licensing" title="write us">Question about licensing? Contact Us.</a> </label></p>
</div>
</div>
<div class="inlineContainerWrapper">


<div class="container">

  <%------------------------------------------------------------------------- "TWPROJECT CONFIGURATION" ------------------------------------------------------------------%>
  <div class="panel">
    <h2>Twproject configuration</h2>
    <ul>
      <li><%
        TabSet.pointToTab("genTabSet", "i18nTS", configPage);
        configPage.addClientEntry("highlight", "tr.international");
        new ButtonLink("Language, dates, currency", configPage).toHtmlInTextOnlyModality(pageContext);
      %><label>set default language, working days, currency and date formats</label>
      </li>

      <li><%
        TabSet.pointToTab("genTabSet", "userTS", configPage);
        configPage.addClientEntry("highlight", "tr.userDefaults");
        new ButtonLink("User defaults", configPage).toHtmlInTextOnlyModality(pageContext);
      %><label>user default, hourly cost, working hours, etc.</label>
      </li>


      <li><%
        TabSet.pointToTab("genTabSet", "pmTS", configPage);
        configPage.addClientEntry("highlight", "table.projectDefault tr");
        new ButtonLink("Projects defaults", configPage).toHtmlInTextOnlyModality(pageContext);
      %><label>roles names, alerts, codes, milestones, etc.</label>
      </li>

      <li><%
        TabSet.pointToTab("genTabSet", "pmISS", configPage);
        configPage.addClientEntry("highlight", "tr.issueDefaults");
        new ButtonLink("Issues defaults", configPage).toHtmlInTextOnlyModality(pageContext);
      %><label>enable codes, types</label>
      </li>

      <li><%
        new ButtonLink("Holidays", pageState.pageFromCommonsRoot("/administration/holidays.jsp")).toHtmlInTextOnlyModality(pageContext);
      %><label>configure company/national holidays</label>
      </li>
    </ul>
  </div>
  <hr>


  <%------------------------------------------------------------------------- "DASHBOARDS" ------------------------------------------------------------------%>
  <div class="panel">
    <h2>Dashboards</h2>
    <ul>
      <li class="lreq20 lreqLabel"><%
        new ButtonLink("Pages", pageState.pageFromApplications("website/admin/pageList.jsp")).toHtmlInTextOnlyModality(pageContext);
      %><label>create/manage pages</label>
      </li>
      <li class="lreq20 lreqLabel">
        <%
          new ButtonLink("default home page", pageState.pageFromRoot("portal/choosePage.jsp")).toHtmlInTextOnlyModality(pageContext);
        %><label>set a default entry page for each user.</label>
      </li>


      <li class="lreq20 lreqLabel"><%
        new ButtonLink("Portlets", pageState.pageFromApplications("website/admin/portletList.jsp")).toHtmlInTextOnlyModality(pageContext);
      %><label>install/un-install, restrict access, modify</label>
      </li>

      <li class="lreq20 lreqLabel"><%
        new ButtonLink("Templates", pageState.pageFromApplications("website/admin/templateList.jsp")).toHtmlInTextOnlyModality(pageContext);
      %><label>create/manage templates (aka page grids)</label>
      </li>

    </ul>
  </div>
  <hr>

  <%------------------------------------------------------------------------- "Subscription & Messaging" ------------------------------------------------------------------%>
  <div class="panel">
    <h2>Subscription & Messages</h2>
    <ul>
      <li><%
        new ButtonLink("Events", new PageSeed(request.getContextPath() + "/applications/teamwork/messaging/eventManager.jsp")).toHtmlInTextOnlyModality(pageContext);
      %><label>list/manage pending event (usually empty)</label>
      </li>
      <li><%
        new ButtonLink("Subscriptions", new PageSeed(request.getContextPath() + "/applications/teamwork/messaging/listenerManager.jsp")).toHtmlInTextOnlyModality(pageContext);
      %><label>list/manage user's subscriptions to Twproject events (lots of records here)</label>
      </li>
      <li><%
        new ButtonLink("Messages", new PageSeed(request.getContextPath() + "/applications/teamwork/messaging/messageManager.jsp")).toHtmlInTextOnlyModality(pageContext);
      %><label>list/manage message queue</label>
      </li>
    </ul>
  </div>
  <hr>



</div>
<div class="container">


  <%----------------------------------------------------------------------- SERVER CONFIGURATION ----------------------------------------------------------------%>
  <div class="panel">
    <h2>Server configuration</h2>
    <ul>
      <li><%
        TabSet.pointToTab("genTabSet", "bcts", configPage);
        configPage.addClientEntry("highlight", "tr.httpSettings");
        new ButtonLink("HTTP server", configPage).toHtmlInTextOnlyModality(pageContext);
      %><label>set domain name, port, protocol</label>
      </li>

      <li><%
        configPage.addClientEntry("highlight", "tr.repositorySettings");
        new ButtonLink("File system", configPage).toHtmlInTextOnlyModality(pageContext);
      %><label>repository path and exposed files</label>
      </li>

    </ul>
  </div>

  <hr>

  <%------------------------------------------------------------------------- "EMAIL" ------------------------------------------------------------------%>
  <div class="panel">
    <h2>Email configuration</h2>
    <ul>
      <li><%
        TabSet.pointToTab("genTabSet", "mailTS", configPage);
        configPage.addClientEntry("highlight", "tr.mailSettings");
        new ButtonLink("Twproject e-mail", configPage).toHtmlInTextOnlyModality(pageContext);%>
        <label>enable Twproject e-mail notification</label>
      </li>

      <li><%
        configPage.addClientEntry("highlight", "tr.mailSMTP");
        new ButtonLink("Send e-mail - SMTP", configPage).toHtmlInTextOnlyModality(pageContext);%>
        <label>enable Twproject e-mail notification</label>
      </li>

      <li><%
        configPage.addClientEntry("highlight", "tr.mailPOP3");
        new ButtonLink("Receive e-mail - POP3/IMAP", configPage).toHtmlInTextOnlyModality(pageContext);%>
        <label>enable Twproject receiving e-mail</label>
      </li>

      <li class="lreq30 lreqLabel"><%
        TabSet.pointToTab("genTabSet", "pmISS", configPage);
        configPage.addClientEntry("highlight", "table.mailboxes tr,table.mailboxes th");
        new ButtonLink("Projects mailboxes", configPage).toHtmlInTextOnlyModality(pageContext);
      %><label>configure a mailbox where to receive issues-tickets</label>
      </li>


    </ul>
  </div>
  <hr>



  <%------------------------------------------------------------------------- "CUSTOMIZATION" ------------------------------------------------------------------%>
  <div class="panel">
    <h2>Customization</h2>
    <ul>
      <li class="lreq30 lreqLabel lreqActive"><%
        ButtonLink cfed = new ButtonLink("Custom fields", pageState.pageFromRoot("/administration/customFieldsEditor.jsp"));
        cfed.additionalCssClass = "";
        cfed.toHtmlInTextOnlyModality(pageContext);
      %><label>define additional fields for tasks/project, issues/todo,<br>resources, worklog, costs, budget etc.</label>
      </li>

      <li class="lreq30 lreqLabel lreqActive"><%
        new ButtonLink("Excel", pageState.pageFromRoot("/administration/excelExportsEditor.jsp")).toHtmlInTextOnlyModality(pageContext);
      %><label>define export fields</label>
      </li>


      <li class="lreq30 lreqLabel"><%
        new ButtonLink("Forms, plugins and reports", pageState.pageFromCommonsRoot("administration/pluginAdmin.jsp")).toHtmlInTextOnlyModality(pageContext);
      %><label>reload/manage plugins and reports.</label>
      </li>


      <li class="lreq30 lreqLabel lreqActive"><%
        new ButtonLink("Business processes", pageState.pageFromCommonsRoot("flowork/backoffice/deployList.jsp")).toHtmlInTextOnlyModality(pageContext);
      %><label>upload new processes, manage, control Teamwork's business process engine</label>
      </li>

      <li class="lreq30 lreqLabel"><%
        new ButtonLink("Labels", pageState.pageFromCommonsRoot("/administration/i18nManager.jsp")).toHtmlInTextOnlyModality(pageContext);
      %><label>change interface' labels</label>
      </li>

      <li class="lreq30 lreqLabel"><%
        PageSeed custlabel = pageState.pageFromCommonsRoot("/administration/i18nManager.jsp");
        custlabel.addClientEntry("SEARCH_TEXT", "CUSTOM_FEATURE_");
        custlabel.addClientEntry("_FP_LANGUAGESEN", "yes");
        new ButtonLink("Advanced customization", custlabel).toHtmlInTextOnlyModality(pageContext);
      %><label>enable/disable Twproject customizations</label>
      </li>
    </ul>
  </div>




</div>
<div class="container">



  <%------------------------------------------------------------------------- "Security " ------------------------------------------------------------------%>
  <div class="panel">
    <h2>Security</h2>
    <ul>

      <li class="lreq30 lreqLabel lreqActive"><%
        new ButtonLink("Roles", pageState.pageFromRoot("security/roleList.jsp")).toHtmlInTextOnlyModality(pageContext);
      %><label>create/customize role names, permissions, default notification</label>
      </li>

      <li class="lreq30 lreqLabel"><%
        new ButtonLink("Areas", pageState.pageFromRoot("security/area.jsp")).toHtmlInTextOnlyModality(pageContext);%> -
        <%
          ButtonLink newAreaWizard = new ButtonLink("New area creation wizard", pageState.pageFromRoot("security/security.jsp"));
          newAreaWizard.additionalCssClass = "edit";
          newAreaWizard.toHtmlInTextOnlyModality(pageContext);
        %>
        <label>rename/manage default area </label>
      </li>


      <li><%
        TabSet.pointToTab("genTabSet", "secTS", configPage);
        configPage.addClientEntry("highlight", "#security tr");
        new ButtonLink("Security policies", configPage).toHtmlInTextOnlyModality(pageContext);
      %><label>password expiry, length, cookies, logout, etc.</label>
      </li>


      <li class="lreq30 lreqLabel lreqActive"><%
        TabSet.pointToTab("genTabSet", "secTS", configPage);
        configPage.addClientEntry("highlight", "tr.AUTH");
        new ButtonLink("Authentication", configPage).toHtmlInTextOnlyModality(pageContext);
      %><label>configure how Twproject validate users: internally, HTTP, LDAP.</label>
      </li>

      <li class="lreq30 lreqLabel lreqActive"><%
        TabSet.pointToTab("genTabSet", "secTS", configPage);
        configPage.addClientEntry("highlight", "#ldapConnection tr,tr.LDAP");
        new ButtonLink("LDAP integration", configPage).toHtmlInTextOnlyModality(pageContext);
      %><label>configure Twproject for using your LDAP data.</label>
      </li>


      <li class="lreq30 lreqLabel lreqActive"><%
        new ButtonLink("LDAP user import", pageState.pageFromRoot("administration/importLdapUsers.jsp")).toHtmlInTextOnlyModality(pageContext);
      %><label>import/update Twproject users from LDAP</label>
      </li>
    </ul>
  </div>
  <hr>


  <%------------------------------------------------------------------------- "MONITORING" ------------------------------------------------------------------%>
  <div class="panel">
    <h2>Monitoring</h2>
    <ul>

      <li><%
        new ButtonLink("Scheduler", pageState.pageFromCommonsRoot("scheduler/scheduleManager.jsp")).toHtmlInTextOnlyModality(pageContext);
      %><label>configure, check, run scheduled jobs</label>
      </li>

      <li><%
        PageSeed ps = pageState.pageFromCommonsRoot("scheduler/jobList.jsp");
        ps.command = Commands.FIND;
        new ButtonLink("Jobs", ps).toHtmlInTextOnlyModality(pageContext);
      %><label>create/manage schedulable jobs</label>
      </li>


      <li class="lreq30 lreqLabel"><%
        new ButtonLink("Deleted object logs", pageState.pageFromCommonsRoot("administration/deleteLog/deleteLogList.jsp")).toHtmlInTextOnlyModality(pageContext);
      %><label>Inspect deletion history logs</label>
      </li>

      <li><%
        ButtonLink.getBlackInstance("Permission's cache manager", 468, 600, pageState.pageFromCommonsRoot("/administration/permissionsCacheManager.jsp")).toHtmlInTextOnlyModality(pageContext);
      %><label>check/rebuild objects tree structure</label>
      </li>

      <li><%
        TabSet.pointToTab("genTabSet", "logTS", configPage);
        configPage.addClientEntry("highlight", "#ct_logAndMonitoring");
        new ButtonLink("Logging", configPage).toHtmlInTextOnlyModality(pageContext);
      %><label>configure/see logs</label>
      </li>


      <%
        if (!ApplicationState.isHostMode) {%>
      <li><%
        ButtonLink.getBlackInstance("System check", 768, 1024, pageState.pageFromRoot("/administration/systemCheck.jsp")).toHtmlInTextOnlyModality(pageContext);
      %><label>updates, license, memory, and system variables</label>
      </li>
      <%}%>

      <li><%
        ButtonLink.getBlackInstance("Tree structure check", 768, 1024, pageState.pageFromCommonsRoot("/administration/treeCheck.jsp")).toHtmlInTextOnlyModality(pageContext);
      %><label>check/rebuild objects tree structure</label>
      </li>

      <li><%
        ButtonLink.getBlackInstance("Java Email Server (JES) monitor", 600, 800, pageState.pageFromRoot("/administration/JESManager.jsp")).toHtmlInTextOnlyModality(pageContext);
      %><label>check status, stop, restart JES (not configured by default)</label>
      </li>

    </ul>
  </div>
  <hr>

  <%------------------------------------------------------------------------- "Misc and Tools" ------------------------------------------------------------------%>
  <div class="panel">
    <h2>Misc and Tools</h2>
    <ul>
      <li><%
        TabSet.pointToTab("genTabSet", "indexingTS", configPage);
        configPage.addClientEntry("highlight", "tr.index");
        new ButtonLink("Index configuration", configPage).toHtmlInTextOnlyModality(pageContext);
        PageSeed indexi = pageState.pageFromRoot("administration/indexingTeamwork.jsp");

        ButtonLink indexManage = new ButtonLink("Index management", indexi);
        indexManage.additionalCssClass = "edit";
        indexManage.toHtmlInTextOnlyModality(pageContext);

      %><label>full-text index configuration, stemmers, etc.</label></li>

      <li><%
        TabSet.pointToTab("genTabSet", "varTS", configPage);
        configPage.addClientEntry("highlight", "tr.various");
        new ButtonLink("Various", configPage).toHtmlInTextOnlyModality(pageContext);
      %><label>Db case conversion and setup flags.</label>
      </li>

      <li><%
        new ButtonLink("Check permission on projects", pageState.pageFromRoot("administration/securityTest.jsp")).toHtmlInTextOnlyModality(pageContext);
      %><label>helps to verify Twproject security configuration on projects</label>
      </li>


      <li><%
        PageSeed hits = pageState.pageFromRoot("administration/hitList.jsp");
        new ButtonLink("Hits", hits).toHtmlInTextOnlyModality(pageContext);
      %><label>manage/rebuild. Hits are used to track user activity.</label>
      </li>


    </ul>
  </div>


</div>
</div>
</div>



<script src="<%=request.getContextPath()+"/commons/js/circles.min.js"%>?_=<%=ApplicationState.getBuild()%>"></script>
<script>
  var circleColors = ['#356A8C', '#ffffff'];
  var animDuration = 0;

  var circles = [];
  function drawCircle(id, perc, radius, lineWidth, colors, duration) {
    if (!id)
      return;
    perc=perc?perc: 0;
    radius=radius?radius:70;
    lineWidth=lineWidth?lineWidth:10;
    duration=duration?duration:600;

    if (!colors)
      colors = circleColors;
    else if (typeof colors == "string")
      colors = [circleColors[0], colors];

    $(function () {
      var circle = Circles.create({
        id      : id,
        radius  : radius,
        width   : lineWidth,
        value   : Math.floor(perc),
        duration: duration,
        text    : function (value) {return value + '<small>%</small>';},
        colors  : colors
      });

      circle._origRadius = circle._radius;
      circles.push(circle);

    })
  }

  $(window).on("resize.circles", function () {
    var i;
    if (window.innerWidth < 1280)
      for (i = 0; i < circles.length; i++) {
        circles[i].updateRadius(window.innerWidth / 25);
      }
    else
      for (i = 0; i < circles.length; i++) {
        circles[i].updateRadius(circles[i]._origRadius);
      }
  });

</script>

<%

    }
%>
