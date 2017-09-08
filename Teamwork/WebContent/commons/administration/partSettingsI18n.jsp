<%@ page import="org.jblooming.system.SystemConstants,
                 org.jblooming.utilities.JSP,
                 org.jblooming.utilities.StringUtilities,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.constants.OperatorConstants,
                 org.jblooming.waf.html.display.Img,
                 org.jblooming.waf.html.input.CheckField,
                 org.jblooming.waf.html.input.Combo,
                 org.jblooming.waf.html.input.TextField,
                 org.jblooming.waf.settings.ApplicationState,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageState,
                 java.io.File" %><%@ page pageEncoding="UTF-8" %><%

  PageState pageState = PageState.getCurrentPageState(request);
  String pl = ApplicationState.getApplicationSetting(SystemConstants.PRINT_LOGO);

  if (!Commands.SAVE.equals(pageState.command)) {
    pageState.addClientEntry(SystemConstants.FRIDAY_IS_WORKING_DAY, ApplicationState.getApplicationSetting(SystemConstants.FRIDAY_IS_WORKING_DAY));
    pageState.addClientEntry(SystemConstants.SATURDAY_IS_WORKING_DAY, ApplicationState.getApplicationSetting(SystemConstants.SATURDAY_IS_WORKING_DAY));
    pageState.addClientEntry(SystemConstants.SUNDAY_IS_WORKING_DAY, ApplicationState.getApplicationSetting(SystemConstants.SUNDAY_IS_WORKING_DAY));
    pageState.addClientEntry(SystemConstants.CURRENCY_FORMAT, ApplicationState.getApplicationSetting(SystemConstants.CURRENCY_FORMAT));
    pageState.addClientEntry(SystemConstants.FLD_SERVER_TIME_ZONE, ApplicationState.getApplicationSetting(SystemConstants.FLD_SERVER_TIME_ZONE));
    pageState.addClientEntry(SystemConstants.PRINT_LOGO, pl);
  }


%>
<tr class="international"><td><%

    Combo cb = I18n.getLocaleCombo(OperatorConstants.FLD_SELECT_LANG , pageState);
    cb.initialSelectedCode = ApplicationState.SYSTEM_LOCALE.toString();

  %><%cb.toHtml(pageContext);%></td>
  <td>en</td>
  <td>&nbsp;</td>
</tr>
<tr class="international"><td><%

    cb = I18n.getTimeZoneCombo(SystemConstants.FLD_SERVER_TIME_ZONE , pageState);
    cb.initialSelectedCode = ApplicationState.SYSTEM_TIME_ZONE.getID();
    cb.fieldSize=60;

  %><%cb.toHtml(pageContext);%></td>
  <td>(MGMT)</td>
  <td>&nbsp;</td>
</tr>
<tr class="international"><td><%

    CheckField cf = new CheckField(StringUtilities.replaceAllNoRegex(SystemConstants.FRIDAY_IS_WORKING_DAY, "_", " ").toLowerCase(), SystemConstants.FRIDAY_IS_WORKING_DAY, "</td><td>", true);
    cf.toolTip = SystemConstants.FRIDAY_IS_WORKING_DAY;
    cf.toHtml(pageContext);%>
  </td>
  <td>yes</td>
  <td>Whether Friday is a working day.</td>
</tr>
<tr class="international"><td><%

    cf = new CheckField(StringUtilities.replaceAllNoRegex(SystemConstants.SATURDAY_IS_WORKING_DAY, "_", " ").toLowerCase(), SystemConstants.SATURDAY_IS_WORKING_DAY, "</td><td>", true);
    cf.toolTip = SystemConstants.SATURDAY_IS_WORKING_DAY;
    cf.toHtml(pageContext);%>
  </td>
  <td>no</td>
  <td>Whether Saturday is a working day.</td>
</tr>
<tr class="international">
  <td><%

    cf = new CheckField(StringUtilities.replaceAllNoRegex(SystemConstants.SUNDAY_IS_WORKING_DAY, "_", " ").toLowerCase(), SystemConstants.SUNDAY_IS_WORKING_DAY, "</td><td>", true);
    cf.toolTip = SystemConstants.SUNDAY_IS_WORKING_DAY;
    cf.toHtml(pageContext);%>
  </td>
  <td>no</td>
  <td>Whether Sunday is a working day.</td>
</tr>
<tr class="international"><td><%

    TextField textField = new TextField(StringUtilities.replaceAllNoRegex(SystemConstants.CURRENCY_FORMAT, "_", " ").toLowerCase(), SystemConstants.CURRENCY_FORMAT, "</td><td>", 12, false);
    textField.toolTip = SystemConstants.CURRENCY_FORMAT;
    textField.toHtml(pageContext);
  String formatted ="";
  try {
   formatted = JSP.currency(1234567);
  } catch (Throwable t) {
    formatted = "<span class=\"warning\"><b>This format is not legal in Java currency.</b></span>";
  }

   %></td>
  <td>###,##0.00€ </td>
  <td>The currency format applied in costs. Do NOT change separators, only groupings! Money will appear like this: <%=formatted%><br>
    <a href="#curr">See detail.</a></td>
</tr>
<tr class="printLogo">
  <td><%

    textField = new TextField(StringUtilities.replaceAllNoRegex(SystemConstants.PRINT_LOGO, "_", " ").toLowerCase(), SystemConstants.PRINT_LOGO, "</td><td>", 20, false);
    textField.toolTip = SystemConstants.PRINT_LOGO;
    textField.toHtml(pageContext);
  %></td>
  <td>printLogo.gif</td>
  <td><%
    if (pl != null && pl.trim().length()>0) {
      new Img(pl, "").toHtml(pageContext);%><br><%
    }
  %>The logo file must be put in the folder <%=ApplicationState.webAppFileSystemRootPath+File.separator%>img</td>
</tr>