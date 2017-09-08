<%@ page import="org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState" %><%
  PageState pageState = PageState.getCurrentPageState(request);

%>
<table cellspacing=5 class="legendaPlan">
<tr>
  <td align="center"><i><%=I18n.get("PLAN_LEGENDA")%>:</i></td>
  <td class="dayT color"></td><td><%=I18n.get("TODAY")%>&nbsp;&nbsp;&nbsp;</td>
  <td class="dayH color"></td><td><%=I18n.get("PLAN_HOLIDAYS_LEGENDA")%>&nbsp;&nbsp;&nbsp;</td>
  <td class="color notAvailable"></td><td><%=I18n.get("PLAN_IN_VACATION_LEGENDA")%>&nbsp;&nbsp;&nbsp;</td>
  <td class="color exceeded"></td><td><%=I18n.get("PLAN_CAPACITY_EXCEEDED_LEGENDA")%>&nbsp;&nbsp;&nbsp;</td>

  <td align="center">&nbsp;&nbsp;&nbsp;<i><%=I18n.get("PLAN_BAR_LEGENDA")%>:</i></td>
  <td class="color highlightPeriod"></td>
  <td><%=I18n.get("PLAN_FOCUSED_PERIOD_LEGENDA")%>&nbsp;&nbsp;&nbsp;</td>
</tr>
</table>
