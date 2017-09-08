<%@ page import="org.jblooming.system.SystemConstants, org.jblooming.utilities.StringUtilities, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.OperatorConstants, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.TextField, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.view.PageState" %><%

  PageState pageState = PageState.getCurrentPageState(request);
  if (!Commands.SAVE.equals(pageState.command)) {

    pageState.addClientEntry(SystemConstants.ENABLE_REDIR_AFTER_LOGIN, ApplicationState.getApplicationSetting(SystemConstants.ENABLE_REDIR_AFTER_LOGIN));
    pageState.addClientEntry(SystemConstants.ALLOW_EMPTY_STRING_PSW, ApplicationState.getApplicationSetting(SystemConstants.ALLOW_EMPTY_STRING_PSW));
    pageState.addClientEntry(SystemConstants.DISABLE_COOKIE_LOGIN, ApplicationState.getApplicationSetting(SystemConstants.DISABLE_COOKIE_LOGIN));
    pageState.addClientEntry(SystemConstants.FLD_PASSWORD_EXPIRY, ApplicationState.getApplicationSetting(SystemConstants.FLD_PASSWORD_EXPIRY));
    pageState.addClientEntry(SystemConstants.FLD_PASSWORD_MIN_LEN, ApplicationState.getApplicationSetting(SystemConstants.FLD_PASSWORD_MIN_LEN));
    pageState.addClientEntry(OperatorConstants.FLD_LOGOUT_TIME, ApplicationState.getApplicationSetting(OperatorConstants.FLD_LOGOUT_TIME));
    pageState.addClientEntry(OperatorConstants.FLD_MAX_LOGIN_FAILED, ApplicationState.getApplicationSetting(OperatorConstants.FLD_MAX_LOGIN_FAILED));
  }
%>

<tbody id="security">
  <tr>
    <td><%

      CheckField cf = new CheckField(StringUtilities.replaceAllNoRegex(SystemConstants.ENABLE_REDIR_AFTER_LOGIN, "_", " ").toLowerCase(), SystemConstants.ENABLE_REDIR_AFTER_LOGIN, "</td><td>", true);
      cf.toHtml(pageContext);

    %></td>
    <td>yes / no</td>
    <td>Whether to redir after login to a pending URL (may generate exception)</td>
  </tr>


  <tr>
    <td><%

      cf = new CheckField(StringUtilities.replaceAllNoRegex(SystemConstants.ALLOW_EMPTY_STRING_PSW, "_", " ").toLowerCase(), SystemConstants.ALLOW_EMPTY_STRING_PSW, "</td><td>", true);
      cf.toHtml(pageContext);

    %></td>
    <td>yes / no</td>
    <td>Password empty allowed</td>
  </tr>
  <tr>
    <td><%

      cf = new CheckField(StringUtilities.replaceAllNoRegex(SystemConstants.DISABLE_COOKIE_LOGIN, "_", " ").toLowerCase(), SystemConstants.DISABLE_COOKIE_LOGIN, "</td><td>", true);
      cf.toHtml(pageContext);

    %></td>
    <td>yes / no</td>
    <td>Disable login cookie.<br>If disabled users must re-insert username and password every time</td>
  </tr>

<tr>
   <td><%

     TextField textField = new TextField(StringUtilities.replaceAllNoRegex(SystemConstants.FLD_PASSWORD_EXPIRY, "_", " ").toLowerCase(), SystemConstants.FLD_PASSWORD_EXPIRY, "</td><td>", 2, false);
     textField.toolTip = SystemConstants.FLD_PASSWORD_EXPIRY;
     textField.toHtml(pageContext);
   %>
  </td>
  <td>0 (never)</td>
  <td>Password expiry in days.</td>
</tr>

<tr>
  <td><%

     textField = new TextField(StringUtilities.replaceAllNoRegex(SystemConstants.FLD_PASSWORD_MIN_LEN, "_", " ").toLowerCase(), SystemConstants.FLD_PASSWORD_MIN_LEN, "</td><td>", 2, false);
     textField.toolTip = SystemConstants.FLD_PASSWORD_MIN_LEN;
     textField.toHtml(pageContext);
   %>
  </td>
  <td>0 (not set)</td>
  <td>Password minimal length.</td>
</tr>

<tr>
  <td><%

     textField = new TextField(StringUtilities.replaceAllNoRegex(OperatorConstants.FLD_LOGOUT_TIME, "_", " ").toLowerCase(), OperatorConstants.FLD_LOGOUT_TIME, "</td><td>", 3, false);
     textField.toolTip = OperatorConstants.FLD_LOGOUT_TIME;
     textField.toHtml(pageContext);
   %>
  </td>
  <td>29 (minutes)</td>
  <td>After how many minutes of inactivity the application will log out the user. If 0, session will never expire; this is the suggested setting.</td>
</tr>
  <tr>
  <td><%

     textField = new TextField(StringUtilities.replaceAllNoRegex(OperatorConstants.FLD_MAX_LOGIN_FAILED, "_", " ").toLowerCase(), OperatorConstants.FLD_MAX_LOGIN_FAILED, "</td><td>", 3, false);
     textField.toolTip = OperatorConstants.FLD_MAX_LOGIN_FAILED;
     textField.toHtml(pageContext);
   %>
  </td>
  <td>0 (not set)</td>
  <td>After how many failed login attempt the system ask for captcha.</td>
</tr>
</tbody>