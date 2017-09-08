<%@ page import="org.jblooming.system.SystemConstants,
                 org.jblooming.utilities.JSP,
                 org.jblooming.utilities.StringUtilities,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.input.CheckField,
                 org.jblooming.waf.html.input.ComboBox,
                 org.jblooming.waf.html.input.TextField, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jblooming.waf.settings.I18n, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.button.ButtonJS" %><%

  PageState pageState = PageState.getCurrentPageState(request);

  if (!Commands.SAVE.equals(pageState.command)) {
    pageState.addClientEntry(SystemConstants.FLD_MAIL_FROM, ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_FROM));
    pageState.addClientEntry(SystemConstants.FLD_MAIL_SMTP, ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_SMTP));
    pageState.addClientEntry(SystemConstants.FLD_MAIL_SMTP_PORT, ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_SMTP_PORT));
    pageState.addClientEntry(SystemConstants.FLD_MAIL_SUBJECT, ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_SUBJECT));

    pageState.addClientEntry(SystemConstants.FLD_MAIL_USE_AUTHENTICATED, ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_USE_AUTHENTICATED));
    pageState.addClientEntry("MAIL_SMTP_USE_TLS", ApplicationState.getApplicationSetting("MAIL_SMTP_USE_TLS"));
    pageState.addClientEntry(SystemConstants.FLD_MAIL_USER, ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_USER));
    pageState.addClientEntry(SystemConstants.FLD_MAIL_PWD, ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_PWD));
    pageState.addClientEntry(SystemConstants.FLD_MAIL_PROTOCOL, ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_PROTOCOL));

    pageState.addClientEntry(SystemConstants.FLD_POP3_USER, ApplicationState.getApplicationSetting(SystemConstants.FLD_POP3_USER));
    pageState.addClientEntry(SystemConstants.FLD_POP3_PSW, ApplicationState.getApplicationSetting(SystemConstants.FLD_POP3_PSW));
    pageState.addClientEntry(SystemConstants.FLD_POP3_HOST, ApplicationState.getApplicationSetting(SystemConstants.FLD_POP3_HOST));
    pageState.addClientEntry(SystemConstants.FLD_POP3_PORT, ApplicationState.getApplicationSetting(SystemConstants.FLD_POP3_PORT));
    pageState.addClientEntry("MAIL_POP3_USE_TLS", ApplicationState.getApplicationSetting("MAIL_POP3_USE_TLS"));
    pageState.addClientEntry(SystemConstants.FLD_EMAIL_DOWNLOAD_PROTOCOL, ApplicationState.getApplicationSetting(SystemConstants.FLD_EMAIL_DOWNLOAD_PROTOCOL));

  }
%>
<tr class="mailSettings"><td><%

    TextField textField = new TextField(SystemConstants.FLD_MAIL_FROM, SystemConstants.FLD_MAIL_FROM, "</td><td>", 30, false);
    textField.toolTip = SystemConstants.FLD_MAIL_FROM;
    textField.toHtmlI18n(pageContext);
  %></td>
  <td>teamworkmail@yourdomain.com</td>
  <td>This address will be used to send messages, if SMTP is configured. If POP3 (or IMAP,...) is configured, this should be the e-mail
    address corresponding to the POP3 account.
    In order to send messages by mail, and receive email notifications, this must be set.
  </td>
</tr>
<tr class="mailSettings"><td><%
    TextField defaultSubject = new TextField(SystemConstants.FLD_MAIL_SUBJECT, SystemConstants.FLD_MAIL_SUBJECT, "</td><td>", 30, false);
    defaultSubject.toolTip = SystemConstants.FLD_MAIL_SUBJECT;
    defaultSubject.toHtmlI18n(pageContext);
  %></td>
  <td>Twproject notification:</td>
  <td>This text will be added as prefix in the subject of each notification sent from Twproject by e-mail.
  </td>
</tr>
<tr><th colspan="4" align="center" class="tableHead secondRow"><small>SMTP - sending</small></th></tr>
<tr class="mailSMTP">
  <td><%
    String smtp = ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_SMTP);

    textField = new TextField(SystemConstants.FLD_MAIL_SMTP, SystemConstants.FLD_MAIL_SMTP, "</td><td>", 30, false);
    textField.toolTip = SystemConstants.FLD_MAIL_SMTP;
    textField.toHtmlI18n(pageContext);
  %></td>
  <td>smtp.yourserver.com</td>
  <td>In order to send messages by mail, and receive email notifications, this must be set.<br>
    After having saved a new value, you MUST test it. <a href="javascript:testSMTP($(this));">Click her to test SMTP</a></td>
</tr>

<tr class="mailSMTP">
  <td nowrap><%
    textField = new TextField(StringUtilities.replaceAllNoRegex(SystemConstants.FLD_MAIL_SMTP_PORT, "_", " ").toLowerCase(), SystemConstants.FLD_MAIL_SMTP_PORT, "</td><td>", 30, false);
    textField.toolTip = SystemConstants.FLD_MAIL_SMTP_PORT;
    textField.fieldSize = 4;
    textField.toHtmlI18n(pageContext);
  %>
  </td>
  <td>25</td>
  <td>If left empty, the standard port (25 or 465 for smtps) will be used.</td>
</tr>


<tr class="mailSMTP">
  <td nowrap><%

    CheckField cf = new CheckField(SystemConstants.FLD_MAIL_USE_AUTHENTICATED, SystemConstants.FLD_MAIL_USE_AUTHENTICATED, "</td><td>", true);
    cf.toolTip = SystemConstants.FLD_MAIL_USE_AUTHENTICATED;
    cf.toHtmlI18n(pageContext);%>
  </td>
  <td>&nbsp;</td>
  <td>For details, see  <a href="http://twproject.com/support/e-mail-configuration/" target="_blank">email configuration</a>.</td>
</tr>
<tr class="mailSMTP">
  <td nowrap><%
    CheckField cfa = new CheckField("MAIL_SMTP_USE_TLS", "MAIL_SMTP_USE_TLS", "</td><td>", true);
    cfa.toolTip = "MAIL_SMTP_USE_TLS";
    cfa.toHtmlI18n(pageContext);%>
  </td>
  <td>&nbsp;</td>
  <td>Check this value if your mail provider require to start TLS.</td>
</tr>
<tr class="mailSMTP"><td><%

    textField = new TextField(SystemConstants.FLD_MAIL_USER, SystemConstants.FLD_MAIL_USER, "</td><td>", 30, false);
    textField.toolTip = SystemConstants.FLD_MAIL_USER;
    textField.toHtmlI18n(pageContext);
  %></td>
  <td>&nbsp;</td>
  <td></td>
</tr>
<tr class="mailSMTP"><td><%

    textField = new TextField("PASSWORD",SystemConstants.FLD_MAIL_PWD, SystemConstants.FLD_MAIL_PWD, "</td><td>", 20, false);
    textField.toolTip = SystemConstants.FLD_MAIL_PWD;
    textField.toHtmlI18n(pageContext);
  %></td>
  <td>&nbsp;</td>
  <td></td>
</tr>
<tr class="mailSMTP"><td><%

  ComboBox mailProtocol = new ComboBox(SystemConstants.FLD_MAIL_PROTOCOL,SystemConstants.FLD_MAIL_PROTOCOL,SystemConstants.FLD_MAIL_PROTOCOL,pageState);
  mailProtocol.separator =  "</td><td>";
  mailProtocol.toolTip = SystemConstants.FLD_MAIL_PROTOCOL;
  mailProtocol.addValue("smtp");
  mailProtocol.addValue("smtps");
  mailProtocol.toHtmlI18n(pageContext);

  %></td>
<td>smtp</td>
  <td>Use smtps for example to use Gmail as SMTP server.</td></tr>


<%-----------------------     POP3/IMAP --------------------------------------------------------------------------------------------------------------%>
<tr><th colspan="4" align="center" class="tableHead secondRow"><small>POP3/IMAP - receiving</small></th></tr>
<tr class="mailPOP3"><td><%
    textField = new TextField(SystemConstants.FLD_POP3_HOST, SystemConstants.FLD_POP3_HOST, "</td><td>", 30, false);
    textField.toolTip = SystemConstants.FLD_POP3_HOST;
    textField.toHtmlI18n(pageContext);
  %></td>
  <td>pop3.yourserver.com</td>
  <td>In order to enable Twproject to receive messages by mail, this must be set.<br>
    After having saved a new value, you MUST to test it. <a href="javascript:testPOP3($(this));">Click her to test POP3</a></td>
</tr>

<tr class="mailPOP3">
  <td nowrap><%
    cfa = new CheckField("MAIL_POP3_USE_TLS", "MAIL_POP3_USE_TLS", "</td><td>", true);
    cfa.toolTip = "MAIL_POP3_USE_TLS";
    cfa.label= I18n.get("MAIL_SMTP_USE_TLS");
    cfa.toHtml(pageContext);%>
  </td>
  <td>&nbsp;</td>
  <td>Check this value if your mail provider require to start TLS.</td>
</tr>

<tr class="mailPOP3"><td><%

    textField = new TextField(SystemConstants.FLD_POP3_USER, SystemConstants.FLD_POP3_USER, "</td><td>", 30, false);
    textField.toolTip = SystemConstants.FLD_POP3_USER;
    textField.toHtmlI18n(pageContext);

  %></td>
  <td>&nbsp;</td>
  <td>In order to enable Twproject to receive messages by mail, this must be set.</td>
</tr>
<tr class="mailPOP3">
  <td><%

    textField = new TextField("PASSWORD",SystemConstants.FLD_POP3_PSW, SystemConstants.FLD_POP3_PSW, "</td><td>", 20, false);
    textField.toolTip = SystemConstants.FLD_POP3_PSW;
    textField.toHtmlI18n(pageContext);

  %></td>
  <td>&nbsp;</td>
  <td>In order to enable Twproject to receive messages by mail, this must be set. <%

    if (JSP.ex(ApplicationState.getApplicationSetting(SystemConstants.FLD_POP3_HOST))) {
       PageSeed log = pageState.pageFromRoot("administration/showLog.jsp");
           log.addClientEntry("LOG","email.log");
           ButtonLink.getBlackInstance("see e-mail downloaded logs",600,800, log).toHtmlInTextOnlyModality(pageContext);
    }

  %></td>
</tr>
<tr class="mailPOP3"><td><%

  ComboBox mailDownProtocol = new ComboBox(SystemConstants.FLD_EMAIL_DOWNLOAD_PROTOCOL,SystemConstants.FLD_EMAIL_DOWNLOAD_PROTOCOL,SystemConstants.FLD_EMAIL_DOWNLOAD_PROTOCOL,pageState);
  mailDownProtocol.separator =  "</td><td>";
  mailDownProtocol.toolTip = SystemConstants.FLD_EMAIL_DOWNLOAD_PROTOCOL;
  mailDownProtocol.addValue("pop3");
  mailDownProtocol.addValue("pop3s");
  mailDownProtocol.addValue("imap");
  mailDownProtocol.addValue("imaps");
  mailDownProtocol.toHtmlI18n(pageContext);

  %></td>
<td>pop3</td>
  <td>Use pop3s for example to use Gmail as POP3 server.</td>
</tr>
<tr class="mailPOP3">
  <td><%

    textField = TextField.getIntegerInstance(SystemConstants.FLD_POP3_PORT);
    textField.toolTip = SystemConstants.FLD_POP3_PORT;
    textField.fieldSize = 5;
    textField.toHtmlI18n(pageContext);

  %></td>
  <td>110</td>
  <td>E.g: pop3=110, pop3s=995, imap=143, imaps=993</td>
</tr>

<%
  ButtonBar bb = pageState.getButtonBar();
  ButtonJS testPOP3= new ButtonJS("Test POP3/IMAP","testPOP3($(this));");
  testPOP3.style="display:none;";
  testPOP3.additionalCssClass="first big testPOP3";
  testPOP3.id="testPOP3";
  bb.addButton(testPOP3);

  ButtonJS testSMTP= new ButtonJS("Test SMTP","testSMTP($(this));");
  testSMTP.style="display:none;";
  testSMTP.additionalCssClass="first big testSMTP";
  bb.addButton(testSMTP);

%>

<script>
  $(function(){
    $(".mailPOP3 :input,.mailSMTP :input").on("blur click keyup",function(){
      if ($(".mailPOP3 :input,.mailSMTP :input").isValueChanged() && $("#POP3_HOST").val()!="" && $("#MAIL_SMTP").val()!="") {
        $(".svBtn").hide();
      } else {
        $(".svBtn").show();
      }
      if ($(".mailPOP3 :input").isValueChanged() && $("#POP3_HOST").val()!="") {
        $(".testPOP3").show();
      } else {
        $(".testPOP3").hide();
      }

      //se i server non sono scritti si nasconde il test
      if ($(".mailSMTP :input").isValueChanged() && $("#MAIL_SMTP").val()!="") {
        $(".testSMTP").show();
      } else {
        $(".testSMTP").hide();
      }
    }).updateOldValue();

  })


  function testPOP3(el){
    var request={CM:"TSTPOP3"};
    $(".mailPOP3 :input[name]").each(function(){
      request[$(this).attr("name")]=$(this).val();
    });
    showSavingMessage();

    $.getJSON("adminAjaxController.jsp",request,function(response){
      hideSavingMessage();
      jsonResponseHandling(response);
      if (response.ok){
        $(".mailPOP3 :input").updateOldValue().eq(0).click();
      } else {
        $(".mailPOP3 :input:first").data("_oldvalue", "--scf++").click();
      }

    })
  }


  function testSMTP(el){
    var request={CM:"TSTSMTP"};
    $(".mailSMTP :input[name]").each(function(){
      request[$(this).attr("name")]=$(this).val();
    });
    showSavingMessage();

    $.getJSON("adminAjaxController.jsp",request,function(response){
      hideSavingMessage();
      jsonResponseHandling(response);
      if (response.ok){
        $(".mailSMTP :input").updateOldValue().eq(0).click();
      } else {
        $(".mailSMTP :input:first").data("_oldvalue", "--scf++").click();
      }

    })
  }



</script>
