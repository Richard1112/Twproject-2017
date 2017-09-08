<%@ page buffer="164kb" %><%@ page import="com.twproject.security.businessLogic.TeamworkLoginController,
                 com.twproject.waf.TeamworkLoginScreen,
                 org.jblooming.security.businessLogic.LoginController,
                 org.jblooming.system.SystemConstants,
                 org.jblooming.utilities.HttpUtilities,
                 org.jblooming.utilities.JSP,
                 org.jblooming.utilities.StringUtilities,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.constants.Fields,
                 org.jblooming.waf.constants.OperatorConstants,
                 org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.display.Img, org.jblooming.waf.html.input.Combo, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.ClientEntry, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.io.File, java.io.FileInputStream, java.util.Properties, org.jblooming.utilities.RecaptchaV2, org.jblooming.security.License" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  boolean justSetupped = Fields.TRUE.equals(ApplicationState.applicationSettings.get("JUST_SETUPPED"));
  if (!pageState.screenRunning) {
    if (justSetupped)
      pageState.setAttribute("JUST_SETUPPED",Fields.TRUE);
    pageState.screenRunning = true;
    LoginController lc = new TeamworkLoginController(request.getContextPath() + "/applications/teamwork/index.jsp");
    final ScreenArea body = new ScreenArea(lc, request);
    TeamworkLoginScreen lw = new TeamworkLoginScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);

  } else {

    if (justSetupped && !JSP.ex(pageState.getEntry(OperatorConstants.FLD_LOGIN_NAME).stringValueNullIfEmpty()))
      pageState.addClientEntry(OperatorConstants.FLD_LOGIN_NAME,"administrator");



    PageSeed psc = new PageSeed(request.getContextPath() + "/applications/teamwork/security/login.jsp");
    psc.setCommand(Commands.SAVE);
    Form form = new Form(psc);
    form.id="LOGINFORMXXX";
    form.alertOnChange = false;
    form.encType = Form.MULTIPART_FORM_DATA;
    pageState.setForm(form);

    form.start(pageContext);

    boolean isASP = Fields.TRUE.equals(ApplicationState.applicationParameters.get("TEAMWORK_ASP_INSTANCE"));

    int maxLoginAttemptFailed = Integer.parseInt(ApplicationState.getApplicationSetting(OperatorConstants.FLD_MAX_LOGIN_FAILED, "0"));
    Integer loginAttemptFailed = (Integer) pageState.sessionState.getAttribute("loginAttemptFailed");

%>

      <h4 style="font-weight: 300; text-transform: uppercase"><%=I18n.get("LOGIN_TITLE")%></h4>
      <div class="helpOpener">
        <span class="teamworkIcon" onclick="var isVisible = $('.help').is(':visible'); !isVisible? $('.help').fadeIn():$('.help').fadeOut(); $(this).html(isVisible?'?':'x');" style="cursor:pointer">?</span>
      </div>
        <div class="help">

          <table align="center" border="0">
            <tr><td>
              <h2 style="margin: 0 0 10px"><%=I18n.get("TEAMWORK_HELP_LINKS")%></h2>
            </td></tr>
            <%
              if (justSetupped) {
            %>
            <tr>
              <td><div style="float: left"><%new Img("warning.png","","27","27").toHtml(pageContext);%></div><i><%=I18n.get("TW_HELP_HOWLOGIN")%></i></td>
            </tr>
            <%
              }

            %>
            <tr>

              <td><h4><%=I18n.get("TW_HELP_TEAMWORK")%></h4>
                <h5><%=I18n.get("TW_HELP_INFOSITE")%><br><a href="http://twproject.com" class="" target="_blank" title="Twproject web site">twproject.com</a></h5>
                <h5><%=I18n.get("TW_HELP_DOWN_GUIDE")%><br><a href="http://twproject.com/support" target="_blank" title="Twproject support site">twproject.com/support/</a></h5>
                <h5><%=I18n.get("TW_HELP_ASK")%><br><a href="http://twproject.com/support/submit-a-ticket/" target="_blank" title="Twproject contacts">http://twproject.com/support/submit-a-ticket//</a><h5>

                    <%

                 try {
               %><small><i><%=I18n.get("APP_VERSION")%> <%=ApplicationState.getApplicationVersion()%>&nbsp;<%
                } catch (Throwable t) {
                %>Twproject version could not be determined. <%
                  }

                  try {
                    String custCode = License.getLicense().customerCode;
                    if (JSP.ex(custCode))
                      custCode = StringUtilities.replaceAllNoRegex(custCode,"@","_");
                %><%=I18n.get("TEAMWORK_LICENSED_TO")%> "<%=custCode%>"</i></small><%
               } catch (Throwable t) {
               %>Twproject license could not be determined. <%
                 }
               %>

              </td>
            </tr>

          </table>
        </div>



      <table align="center" border="0" style="margin-bottom: 10px">
        <tr>
          <td height="40" nowrap><%

            TextField tf = new TextField("TEXT","",OperatorConstants.FLD_LOGIN_NAME,"",20,false);
            tf.addKeyPressControl(13,"obj('"+ OperatorConstants.FLD_PWD +"').focus();","onkeyup");
            tf.tabIndex=1;
            tf.required = true;
            tf.innerLabel=I18n.get(OperatorConstants.FLD_LOGIN_NAME);
            tf.fieldClass="formElements formElementsBig light";
            pageState.setFocusedObjectDomId(tf.id);
            tf.toHtml(pageContext);

          %></td></tr>
        <tr><td height="40" nowrap><%

          tf = new TextField("PASSWORD","",OperatorConstants.FLD_PWD,"",20,false);
          tf.addKeyPressControl(13,"obj('"+ pageState.getForm().getUniqueName() +"').submit();", "onkeyup");
          tf.innerLabel=I18n.get(OperatorConstants.FLD_PWD);
          tf.fieldClass="formElements formElementsBig light";
          tf.tabIndex=2;
          tf.toHtml(pageContext);

        %></td></tr>
        <%
          if(loginAttemptFailed!=null && loginAttemptFailed> maxLoginAttemptFailed && maxLoginAttemptFailed>0){

            %> <tr><td height="40" nowrap><%
              RecaptchaV2 recaptcha = new RecaptchaV2("6LeGqCITAAAAANwJVFJhjMxha7m_OqJXs7U-Cbiz", "6LeGqCITAAAAAKfLkKiLYsvcEOwBGpGe1ciygt1j");
              recaptcha.toHtml(pageContext);
            %></td></tr><%

         }

          if (justSetupped) {

        %><tr><td height="40"><div class="language">
        <h4><%=I18n.get("SELECT_LANGUAGE_AND_LOCALE")%></h4>
        <%
          Combo cb = I18n.getLocaleCombo(OperatorConstants.FLD_SELECT_LANG , pageState);
          cb.initialSelectedCode = ApplicationState.SYSTEM_LOCALE.toString();
          cb.separator ="";
          cb.additionalStyle ="formElementsBig";
          cb.label ="";
          cb.toHtml(pageContext);

        %><br><br><small><%=I18n.get("ENTER_OTHER_COUNTRY")%></small></div></td></tr><%

        }
      %>
        <tr>
          <td height="70" nowrap >
            <div class="hideOnMobile"><%new ButtonLink("<big>"+I18n.get("GO_MOBILE")+"</big>",pageState.pageFromWebApp("mobile")).toHtmlInTextOnlyModality(pageContext);%></div>

            <%
              ButtonSubmit bs = new ButtonSubmit(pageState.getForm());
              bs.label = I18n.get("PROCEED");
              bs.additionalCssClass = "first big access full";
              bs.toHtml(pageContext);

            %></td></tr><%

        ClientEntry ce = pageState.getEntry(OperatorConstants.FLD_PWD);
        if (ce!=null && ce.errorCode!=null && ce.errorCode.equals("ERR_EXPIRED")) {

      %><tr><td height="30" ><%

        tf = new TextField("PASSWORD",OperatorConstants.FLD_LOGIN_NEW_PWD,"",20,false);
        tf.innerLabel=I18n.get("PASSWORD");
        tf.toHtml(pageContext);

      %></td></tr>
        <tr><td height="30"><%

          tf = new TextField("PASSWORD",OperatorConstants.FLD_LOGIN_NEW_PWD_RETYPE,"",20,false);
          tf.innerLabel=I18n.get("PASSWORD");
          tf.toHtml(pageContext);

        %></td></tr><%
        }

        String auth_type = ApplicationState.getApplicationSetting(SystemConstants.AUTHENTICATION_TYPE);

        if (SystemConstants.ENABLE_AUTHENTICATION_TYPE.ENABLE_LDAP_AUTHENTICATION.toString().equals(auth_type)||
            SystemConstants.ENABLE_AUTHENTICATION_TYPE.ENABLE_LDAP_AUTHENTICATION_WITH_FALLBACK_ON_STANDARD.toString().equals(auth_type)) {
      %><tr> <td colspan="2" nowrap><%=I18n.get("LDAP_ENABLED")%></td> </tr><%
        }

        if (isASP) {
          ButtonLink bl = new ButtonLink("create a demo account", pageState.pageFromRoot("createUserEnvironment.jsp"));
          bl.additionalCssClass="full big";

          %><tr> <td colspan="2" nowrap><%bl.toHtml(pageContext);%></td> </tr><%
        }
        if (!JSP.ex(auth_type) || SystemConstants.ENABLE_AUTHENTICATION_TYPE.ENABLE_STANDARD_AUTHENTICATION.toString().equals(auth_type)){
          if (JSP.ex(ApplicationState.getApplicationSetting(SystemConstants.FLD_MAIL_SMTP)) ) {
      %><tr> <td nowrap colspan="2" height='50'><%
        ButtonSubmit forgot = new ButtonSubmit(form);
        forgot.label = I18n.get("FORGOT_PWD");
        forgot.variationsFromForm.command = "FORGOT_PWD";
        forgot.variationsFromForm.href = pageState.pageFromRoot("forgotPassword.jsp").href;
        forgot.toHtmlInTextOnlyModality(pageContext);

      %></td></tr><%
      } else {
      %><tr> <td nowrap colspan='2' height='50'><small>"Forgot my password" functionality is disabled until<br> you configure e-mail (SMTP) in Twproject.</small></td> </tr>
        <%
            }
          }
        %></table>

<%
    form.end(pageContext);

  }%>
