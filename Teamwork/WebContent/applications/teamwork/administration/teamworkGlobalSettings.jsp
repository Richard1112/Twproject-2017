<%@ page import="com.twproject.operator.TeamworkOperator,
                 com.twproject.setup.SettingsControllerAction,
                 org.jblooming.ldap.LdapUtilities,
                 org.jblooming.messaging.MailHelper,
                 org.jblooming.oql.OqlQuery,
                 org.jblooming.system.SystemConstants,
                 org.jblooming.tracer.Tracer,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.ScreenBasic,
                 org.jblooming.waf.constants.Fields,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.container.ButtonBar,
                 org.jblooming.waf.html.container.Container,
                 org.jblooming.waf.html.container.Tab,
                 org.jblooming.waf.html.container.TabSet,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.ApplicationState,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState,
                 javax.mail.*,
                 javax.naming.directory.DirContext,
                 java.util.ArrayList,
                 java.util.List, org.jblooming.waf.html.button.ButtonJS"%><%@ page pageEncoding="UTF-8" %><%

  PageState pageState = PageState.getCurrentPageState(request);

  pageState.getLoggedOperator().testIsAdministrator();
  
  if (!pageState.screenRunning) {

    ScreenBasic.preparePage(new SettingsControllerAction(), pageContext);
    pageState.perform(request, response).toHtml(pageContext);
  } else {

    PageSeed ps = pageState.thisPage(request);
    ps.addClientEntry("CONFIRM_POP3", Fields.FALSE);
    ps.addClientEntry("CONFIRM_LDAP_AUTH", Fields.FALSE);
    Form form = new Form(ps);
    pageState.setForm(form);
    form.start(pageContext);

 %><script>$("#ADMINISTRATION_ROOT_MENU").addClass('selected');</script>
<style>
    table.edged > tbody > tr > td{
        font-size: 90%;
        padding: 4px;
    }
    h2 {
        margin-top: 20px;
    }
    .tabBox > h2:first-of-type {
        margin-top: 10px;
    }

</style>
<%
  ButtonLink adminLink = new ButtonLink(I18n.get("ADMINISTRATION_ROOT_MENU") + " /",pageState.pageFromRoot("administration/administrationIntro.jsp"));
%>
<div class="mainColumn"><%adminLink.toHtmlInTextOnlyModality(pageContext);%><h1><%=I18n.get("GLOBAL_SETTINGS")%></h1><%

    TabSet ts = new TabSet("genTabSet", pageState);

    Tab base = new Tab("bcts", "base");
    ts.addTab(base);

    Tab pm = new Tab("pmTS", "projects");
    ts.addTab(pm);

    Tab iss = new Tab("pmISS", "issues");
    ts.addTab(iss);

    Tab mailTS = new Tab("mailTS", "e-mail");
    ts.addTab(mailTS);

    Tab user = new Tab("userTS", "user");
    ts.addTab(user);

    Tab indexing = new Tab("indexingTS", "indexing");
    ts.addTab(indexing);

    Tab ii18n = new Tab("i18nTS", "internationalization");
    ts.addTab(ii18n);

    Tab sec = new Tab("secTS", "security");
    ts.addTab(sec);

    Tab var = new Tab("varTS", "various");
    ts.addTab(var);

    Tab log = new Tab("logTS","log settings");
    ts.addTab(log);


  if (SettingsControllerAction.ldapAuthHasJustBeenSet(pageState)) {

      Container conf = new Container();
      conf.title = "Confirm LDAP as authentication source";
      conf.draggable = true;
      conf.centeredOnScreen = true;
      conf.closeable = true;
      conf.setCssPostfix("warn");
      conf.width = "50%";
      conf.start(pageContext);
      %>You are about to activate LDAP authentication. You must have created/imported the LDAP users in Twproject before enabling LDAP authentication.<br><%

  String prov_url  = pageState.getEntry(LdapUtilities.PROVIDER_URL).stringValueNullIfEmpty();
  String sec_principal = pageState.getEntry(LdapUtilities.SECURITY_PRINCIPAL).stringValueNullIfEmpty();
  String sec_auth = pageState.getEntry(LdapUtilities.SECURITY_AUTHENTICATION).stringValueNullIfEmpty();
  String sec_credentials = pageState.getEntry(LdapUtilities.SECURITY_CREDENTIALS).stringValueNullIfEmpty();
  String baseDN = pageState.getEntry(LdapUtilities.BASE_DN).stringValueNullIfEmpty();
  String oldBaseDN = ApplicationState.getApplicationSetting(LdapUtilities.BASE_DN);
  if (JSP.ex(baseDN))
    ApplicationState.getApplicationSettings().put(LdapUtilities.BASE_DN,baseDN);

  List<TeamworkOperator> enabledInLdap = new ArrayList();

  try {
   DirContext ctx  = LdapUtilities.getContext(prov_url,sec_auth, sec_principal,sec_credentials);

      //how many of the current tw logins are in teamwork
      String hql = "from "+ TeamworkOperator.class.getName()+" as op where op.enabled = :truth and op.hidden=:falsity";
      OqlQuery oql = new OqlQuery(hql);
      oql.getQuery().setBoolean("truth",true);
      oql.getQuery().setBoolean("falsity",false);
      List<TeamworkOperator> ops = oql.list();

      for (TeamworkOperator top : ops) {
        if (LdapUtilities.getLdapUser(top.getLoginName(),ctx)!=null)
          enabledInLdap.add(top);
      }

      if (enabledInLdap.size()==0) {
        %><font size="3">No Twproject user found in LDAP: you should NOT activate LDAP authentication now.</font><%
      } else {
        %><br>Twproject users enabled in LDAP: <div style="max-height: 600px; overflow-y: auto;"><%
        for (TeamworkOperator top : enabledInLdap) {
          %><%=top.getPerson().getDisplayName()%> <%
        }
        %></div><br><font size="3">You may enable LDAP safely.</font><%
      }

    } catch(Throwable e){

      %>LDAP connection refused: <%=e.getMessage()%>.<%
      Tracer.platformLogger.error(e);
  }

   if (JSP.ex(oldBaseDN))
     ApplicationState.getApplicationSettings().put(LdapUtilities.BASE_DN,oldBaseDN);

  ButtonBar bb = new ButtonBar();

  if (enabledInLdap.size()==0) {
   PageSeed imp = pageState.pageInThisFolder("importLdapUsers.jsp",request);
   ButtonLink bimp = new ButtonLink("go to LDAP imports",imp);
   bb.addButton(bimp);
  }
  ButtonSubmit saveInstance = ButtonSubmit.getSaveInstance(pageState);
  saveInstance.additionalCssClass="first big";
  saveInstance.variationsFromForm.addClientEntry("CONFIRM_LDAP_AUTH", Fields.TRUE);
  saveInstance.enabled = enabledInLdap.size()>0;  
  saveInstance.label = "do enable LDAP authentication";
  bb.addButton(saveInstance);
  bb.toHtml(pageContext);

      conf.end(pageContext);
  }


  ButtonBar bb = new ButtonBar();
  pageState.setButtonBar(bb);



  ButtonSubmit saveInstance = ButtonSubmit.getSaveInstance(pageState);
  saveInstance.additionalCssClass=" first big svBtn";
  bb.addButton(saveInstance);

  PageSeed administrationIntro = pageState.pageFromRoot("administration/administrationIntro.jsp");
  ButtonLink ai = new ButtonLink(I18n.get("ADMINISTRATION_ROOT_MENU"),administrationIntro);
  ai.additionalCssClass="big";
  bb.addButton(ai);

  ts.drawBar(pageContext);

  base.start(pageContext);
%>
<h2>Base configuration</h2>
<table cellpadding="3" id="help" class="table edged">
  <tr>
    <th class="tableHead large">Parameter</th>
    <th width="1%" class="tableHead">Current value</th>
    <th width="1%" class="tableHead">Sample value</th>
    <th class="tableHead">Description</th>
  </tr>
  <jsp:include page="../../../commons/administration/partSettingsBase.jsp"/>
</table>
<table width="100%">
  <tr>
    <td>
      <h2>Using Twproject with a domain name</h2>
<a name="domain"></a>
The value should be the URL which the DNS resolves to Twproject's server IP.<br>
A technical point for the administrator: if you want to use Twproject both from your intranet and from Internet, you
should consider the problem of using an address valid from both networks.
Typically, you use a public DNS name in order to reach your Twproject from anywhere; and to use the same addresses in
your intranet, the internal DNS must have the public entry too.
</td>
  </tr></table>
<%


  base.end(pageContext);
//------------------------------------------------------------PM------------------------------------------------------------
    pm.start(pageContext);
%><h2>Project management configuration</h2>
  <table cellpadding="3" id="help" class="table projectDefault edged">
  <tr>
    <th class="tableHead large">Parameter</th>
    <th width="1%" nowrap class="tableHead">Current value</th>
    <th width="1%" nowrap class="tableHead">Sample value</th>
    <th class="tableHead">Description</th>
  </tr>
  <jsp:include page="partSettingsTeamwork.jsp"/>
</table>
<%

  pm.end(pageContext);


//------------------------------------------------------------ISSUES------------------------------------------------------------
  iss.start(pageContext);
  %><jsp:include page="partSettingsIssues.jsp"/><%
  iss.end(pageContext);


//------------------------------------------------------------E_MAIL------------------------------------------------------------

  mailTS.start(pageContext);%>
<h2>Projects mailboxes</h2>
<table cellpadding="3" id="help" class="table edged">
  <tr>
    <th class="tableHead large">Parameter</th>
    <th width="1%" nowrap class="tableHead">Current value</th>
    <th width="1%" nowrap class="tableHead">Sample value</th>
    <th class="tableHead">Description</th>
  </tr>
  <jsp:include page="../../../commons/administration/partSettingsMail.jsp"/>
</table>
<%
  mailTS.end(pageContext);


//------------------------------------------------------------USER------------------------------------------------------------

  user.start(pageContext);%>
<h2>Operator default options</h2>
<table cellpadding="3" id="help" class="table edged">
  <tr>
    <th class="tableHead large">Parameter</th>
    <th width="10%" nowrap class="tableHead">Current value</th>
    <th width="1%" nowrap class="tableHead">Sample value</th>
    <th class="tableHead">Description</th>
  </tr>
  <jsp:include page="../../../commons/administration/partSettingsOperator.jsp"/>
</table>
<%
  user.end(pageContext);

//--------------------------------------------------------------indexing------------------------------------------------------------
  indexing.start(pageContext);%>
<h2>Indexing configuration</h2>
<table cellpadding="3" id="help" class="table edged">
  <tr>
    <th class="tableHead large">Parameter</th>
    <th width="1%" nowrap class="tableHead">Current value</th>
    <th width="1%" nowrap class="tableHead">Sample value</th>
    <th class="tableHead">Description</th>
  </tr>
  <jsp:include page="partSettingsIndexingTeamwork.jsp"/>
</table>
<%
  indexing.end(pageContext);



  //--------------------------------------------------------------   i18n --------------------------------------------------------------
  ii18n.start(pageContext);%>
<h2>Internationalization & agenda</h2>
<table cellpadding="3" id="help" class="table edged">
  <tr>
    <th class="tableHead large">Parameter</th>
    <th width="1%" nowrap class="tableHead">Current value</th>
    <th width="1%" nowrap class="tableHead">Sample value</th>
    <th class="tableHead">Description</th>
  </tr>
  <jsp:include page="../../../commons/administration/partSettingsI18n.jsp"/>
</table>
<h2>A note on currency handling</h2>
<a name="curr"></a>
The CURRENCY_FORMAT parameter allows setting how to group numbers and which symbol to use;
<br>

it does NOT allow choosing a decimal and grouping separator. <br>
So ###,##0.00€ is valid, but ###.##0,00€ is not. <br>
The decimal and grouping separator is set by your general locale, which is set in the parameter SELECT_LANG

<%
  ii18n.end(pageContext);

//--------------------------------------------------------------   SEC ------------------------------------------------------------ --
 sec.start(pageContext);
%>
<h2>Security</h2>
<table cellpadding="3" id="help" class="table edged">
  <tr>
    <th class="tableHead large">Parameter</th>
    <th width="1%" nowrap class="tableHead">Current value</th>
    <th width="1%" nowrap class="tableHead">Sample value</th>
    <th class="tableHead">Description</th>
  </tr>
  <jsp:include page="../../../commons/administration/partSettingsSecurity.jsp"/>
  <jsp:include page="partSettingsLDAP.jsp"/>

</table>
  <%
  sec.end(pageContext);


//--------------------------------------------------------------    VARIOUS ------------------------------------------------------------
  var.start(pageContext);%>
  <h2>Advanced configuration - change only if sure</h2>
<table cellpadding="3" id="help" class="table edged">
  <tr>
    <th class="tableHead large">Parameter</th>
    <th width="1%" nowrap class="tableHead">Current value</th>
    <th width="1%" nowrap class="tableHead">Sample value</th>
    <th class="tableHead">Description</th>
  </tr>
  <jsp:include page="../../../commons/administration/partSettingsVarious.jsp"/>
</table>


<%
  var.end(pageContext);

//------------------------------------------------------------ LOG SETTINGS --------------------------------------------------------------

  log.start(pageContext);
  %><jsp:include page="partLogSettings.jsp"/><%
  log.end(pageContext);
//--------------------------------------------------------------Buttons------------------------------------------------------------

    ts.end(pageContext);

    bb.toHtml(pageContext);

    %></div><%

    form.end(pageContext);

    %>

  <script type="text/javascript">
    $(function(){
      $("body").oneTime(500,function(){
        $("<%=pageState.getEntry("highlight").stringValueNullIfEmpty()%>").addClass("highlight").first().each(function(){
          $("body").scrollTop($(this).position().top);
        });//("highlight",2500);
      });
    });

  </script>

<%
  }
%>