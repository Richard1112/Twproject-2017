<%@ page import="org.jblooming.oql.QueryHelper,
                 org.jblooming.system.SystemConstants,
                 org.jblooming.utilities.StringUtilities,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.constants.OperatorConstants,
                 org.jblooming.waf.html.input.CheckField,
                 org.jblooming.waf.html.input.TextField" %>
<%@ page import="org.jblooming.waf.settings.ApplicationState" %>
<%@ page import="org.jblooming.waf.view.PageState" %>
<%
  
  PageState pageState = PageState.getCurrentPageState(request);
  if (!Commands.SAVE.equals(pageState.command)) {
    pageState.addClientEntry(QueryHelper.QBE_CONVERT_TO_UPPER, ApplicationState.getApplicationSetting(QueryHelper.QBE_CONVERT_TO_UPPER));
    pageState.addClientEntry(SystemConstants.SETUP_DB_UPDATE_DONE, ApplicationState.getApplicationSetting(SystemConstants.SETUP_DB_UPDATE_DONE));
    pageState.addClientEntry(SystemConstants.SETUP_NOTIFIED_ADMIN_WIZARDS, ApplicationState.getApplicationSetting(SystemConstants.SETUP_NOTIFIED_ADMIN_WIZARDS));
    pageState.addClientEntry(SystemConstants.AUDIT, ApplicationState.getApplicationSetting(SystemConstants.AUDIT));
    pageState.addClientEntry("COMBO_ROWS_TO_FETCH", ApplicationState.getApplicationSetting("COMBO_ROWS_TO_FETCH"));
    pageState.addClientEntry(OperatorConstants.OP_PAGE_SIZE, ApplicationState.getApplicationSetting(OperatorConstants.OP_PAGE_SIZE));
  }

%>
<tr class="various"><td><%

    CheckField cf = new CheckField(StringUtilities.replaceAllNoRegex(QueryHelper.QBE_CONVERT_TO_UPPER, "_", " ").toLowerCase(), QueryHelper.QBE_CONVERT_TO_UPPER, "</td><td>", true);
    cf.toolTip = QueryHelper.QBE_CONVERT_TO_UPPER;
    cf.toHtml(pageContext);

%></td>
  <td>yes</td>
  <td>
    This is useful for case sensitive databases, like Oracle, but for the others may be set to false and improve
    performance.
  </td>
</tr>

<tr class="various"><td><%

  TextField textField = new TextField("COMBO_ROWS_TO_FETCH", "COMBO_ROWS_TO_FETCH", "</td><td>", 2, false);
    textField.toolTip = "COMBO_ROWS_TO_FETCH";
    textField.toHtmlI18n(pageContext);

   %></td>
  <td>20</td>
  <td>Default rows fetched on smart combos.</td>
</tr>

<tr class="various"><td><%

  textField = new TextField(OperatorConstants.OP_PAGE_SIZE, OperatorConstants.OP_PAGE_SIZE, "</td><td>", 2, false);
  textField.toolTip = OperatorConstants.OP_PAGE_SIZE;
  textField.toHtmlI18n(pageContext);

%></td>
  <td>20</td>
  <td>Default page size.</td>
</tr>



<tr class="various"><td><%

    cf = new CheckField(StringUtilities.replaceAllNoRegex(SystemConstants.SETUP_DB_UPDATE_DONE, "_", " ").toLowerCase(), SystemConstants.SETUP_DB_UPDATE_DONE, "</td><td>", true);
    cf.toolTip = SystemConstants.SETUP_DB_UPDATE_DONE;
    cf.toHtml(pageContext);

%></td>
  <td>&nbsp;</td>
  <td>If absent or "no", at start-up the application does a database schema verification, and eventually an update.</td>
</tr>
<tr class="various">
   <td><%

     cf = new CheckField(StringUtilities.replaceAllNoRegex(SystemConstants.SETUP_NOTIFIED_ADMIN_WIZARDS, "_", " ").toLowerCase(), SystemConstants.SETUP_NOTIFIED_ADMIN_WIZARDS, "</td><td>", true);
     cf.toolTip = SystemConstants.SETUP_NOTIFIED_ADMIN_WIZARDS;
     cf.toHtml(pageContext);

   %></td>
  <td>&nbsp;</td>
  <td>At launch the application has created default data (user, types )</td>
</tr>

