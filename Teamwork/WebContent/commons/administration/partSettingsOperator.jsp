<%@ page import="com.twproject.resource.ResourceBricks,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.constants.Fields,
                 org.jblooming.waf.constants.OperatorConstants,
                 org.jblooming.waf.html.input.CheckField,
                 org.jblooming.waf.html.input.ComboBox,
                 org.jblooming.waf.html.input.TextField,
                 org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.view.PageState" %><%

  PageState pageState = PageState.getCurrentPageState(request);
  if (!Commands.SAVE.equals(pageState.command)) {
    pageState.addClientEntry(OperatorConstants.FLD_CURRENT_SKIN, ApplicationState.getApplicationSetting(OperatorConstants.FLD_CURRENT_SKIN));
    pageState.addClientEntry(OperatorConstants.FLD_WORKING_HOUR_BEGIN, ApplicationState.getApplicationSetting(OperatorConstants.FLD_WORKING_HOUR_BEGIN));
    pageState.addClientEntry(OperatorConstants.FLD_WORKING_HOUR_END, ApplicationState.getApplicationSetting(OperatorConstants.FLD_WORKING_HOUR_END));
    pageState.addClientEntry("SHOW_USER_SCORES", JSP.ex(ApplicationState.getApplicationSetting("SHOW_USER_SCORES")) ? ApplicationState.getApplicationSetting("SHOW_USER_SCORES") : Fields.TRUE);

    pageState.addClientEntry(OperatorConstants.FLD_WORKING_HOUR_TOTAL, ApplicationState.getApplicationSetting(OperatorConstants.FLD_WORKING_HOUR_TOTAL));
    pageState.addClientEntry("ASSIG_COST", ResourceBricks.getDefaultHourlyCost());

  }

%>
<tr class="userDefaults">
  <td><%

    TextField textField = TextField.getDoubleInstance("ASSIG_COST");
    textField.fieldSize=6;
    textField.toolTip = "ASSIG_COST";
    textField.toHtmlI18n(pageContext);%>
  </td>
  <td><%=JSP.w(20d)%></td>
  <td>Default hourly cost on assignments; if set on the resource, this last is what will be proposed on the assignment editor when the assignee is chosen.</td>
</tr>

<tr class="userDefaults"><td><%

  ComboBox wht = ComboBox.getTimeInstance(OperatorConstants.FLD_WORKING_HOUR_TOTAL, OperatorConstants.FLD_WORKING_HOUR_TOTAL, pageState);
  wht.separator = "</td><td>";
  wht.toHtmlI18n(pageContext);

%></td>
  <td>08:00</td>
  <td>This will determine the default number of hour per day.</td>
</tr>

<tr class="userDefaults"><td><%

  ComboBox whb = ComboBox.getTimeInstance(OperatorConstants.FLD_WORKING_HOUR_BEGIN, OperatorConstants.FLD_WORKING_HOUR_BEGIN, pageState);
  whb.separator = "</td><td>";
  whb.toHtmlI18n(pageContext);

%></td>
  <td>09:00</td>
  <td>Default working start hour.</td>
</tr>
<tr class="userDefaults">
  <td><%

    ComboBox whe = ComboBox.getTimeInstance(OperatorConstants.FLD_WORKING_HOUR_END, OperatorConstants.FLD_WORKING_HOUR_END, pageState);
    whe.separator = "</td><td>";
    whe.toolTip = OperatorConstants.FLD_WORKING_HOUR_END;
    whe.toHtmlI18n(pageContext);

  %></td>
  <td>18:00</td>
  <td>Default work end hour.</td>
</tr>

<tr class="userDefaults">
  <td><%

    CheckField opSco = new CheckField("SHOW_USER_SCORES","</td><td>",true);
    opSco.toolTip = "SHOW_USER_SCORES";
    opSco.toHtmlI18n(pageContext);

  %></td>
  <td>yes</td>
  <td>Show user's scores where possible.</td>
</tr>
