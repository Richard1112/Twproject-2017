<%@ page import="com.twproject.setup.businessLogic.CreateEnvironmentActionController,
                 com.twproject.waf.TeamworkLoginScreen,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.constants.OperatorConstants,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.input.Combo,
                 org.jblooming.waf.html.input.TextField,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.ApplicationState,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState" %><%

  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new CreateEnvironmentActionController(pageState), request);
    TeamworkLoginScreen lw = new TeamworkLoginScreen(body);
    lw.register(pageState);
    pageState.getHeaderFooter().toolTip="Twproject project managament software free demo";
    pageState.perform(request, response);
    if (Commands.SAVE.equals(pageState.command) && pageState.validEntries() ) {

      PageSeed createCompany = pageState.pageFromRoot("createCompany.jsp");
      createCompany.command= "ACCOUNT_SET";

      response.sendRedirect(createCompany.toLinkToHref());

      return;
    }
    pageState.toHtml(pageContext);

  } else {

   
    PageSeed psc = pageState.thisPage(request);
    psc.setCommand(Commands.SAVE);
    Form form = new Form(psc);
    //form.alertOnChange = false;
    pageState.setForm(form);

  form.start(pageContext);
%>

<script type="text/javascript">
var RecaptchaOptions = {
   theme : 'white'
};
</script>

<div class="demoBox">

<table align="center" border="0" style="margin-bottom: 10px">

<tr>
  <td valign="top" class="welcomeText">
      <p>
  This simple procedure will create <b>your demo working environment</b>.
  You will be the "manager" of a working environment, in which it will be possible to create users and projects.


      <br>Have fun,<br>
  <p style="text-align: right"><small><i>Twproject staff</i></small></p>

  <div style="position: absolute; bottom: 45px"><%

    PageSeed loginPage = pageState.pageFromRoot("index.jsp");
    new ButtonLink("I already have a login", loginPage).toHtmlInTextOnlyModality(pageContext);
  %> <br> <%
    PageSeed j = new PageSeed("https://twproject.com");
    new ButtonLink("Back to site", j).toHtmlInTextOnlyModality(pageContext);
  %>
  </div>
</td>
<td valign="top">
  <table border="0" cellpadding="2" cellspacing="0" align="center" width="100%">
    <tr>
      <td nowrap>
        <%
          TextField login = new TextField(OperatorConstants.FLD_LOGIN_NAME, "<br>");
          login.label = "e-mail";
          login.fieldSize = 25;
          login.fieldClass = "formElements formElementsBig";
          login.required = true;
          login.toHtml(pageContext);
        %></td>
    </tr>

    <tr>
      <td nowrap><%
        TextField tf = new TextField("password",OperatorConstants.FLD_PWD, "<br>",20);
        tf.required = true;
        tf.label = OperatorConstants.FLD_PWD;
        tf.fieldClass = "formElements formElementsBig";
        tf.addKeyPressControl(13, "obj('PASSWORD2').focus();", "onkeyup");
        tf.script = tf.script + " autocomplete=\"off\"";
        tf.toHtmlI18n(pageContext);
      %></td>
    </tr>
    <tr>

      <td nowrap><%
        tf = new TextField("password",OperatorConstants.FLD_PWD_RETYPE, "<br>",20);
        tf.required = true;
        tf.label = OperatorConstants.FLD_PWD_RETYPE;
        tf.fieldClass = "formElements formElementsBig";
        tf.addKeyPressControl(13, "obj('"+OperatorConstants.FLD_SELECT_LANG+"').focus();", "onkeyup");
        tf.script = tf.script + " autocomplete=\"off\"";
        tf.toHtmlI18n(pageContext);
      %></td>
    </tr>

    <tr>
      <td nowrap><%
        Combo cb = I18n.getLocaleCombo(OperatorConstants.FLD_SELECT_LANG , pageState);
        cb.label="Select a language";
        cb.fieldSize=20;
        cb.separator="<br>";
        cb.initialSelectedCode = ApplicationState.SYSTEM_LOCALE.toString();

        cb.toHtml(pageContext);
      %></td>
    </tr>
  </table>
  <table width="100%" cellspacing="0" cellpadding="5" ><tr><td width="2%">
    <br>
    <small class="infoText">You may receive a follow up e-mail and occasional Twproject release announcement at the e-mail above, to which you may opt out any time.</small>
  </td></tr>
    <tr><td width="2%" align="left">
      <%
        //ButtonBar bb = new ButtonBar();
        ButtonSubmit createAccount= new ButtonSubmit(form);
        createAccount.alertOnRequired=true;
        createAccount.additionalCssClass="big first";
        createAccount.label="create an account";
        createAccount.toHtml(pageContext);

      %></td></tr></table>

</td>
</tr>
</table>
</div>
  <%
  form.end(pageContext);%>

</div>


<%
  }
%>
