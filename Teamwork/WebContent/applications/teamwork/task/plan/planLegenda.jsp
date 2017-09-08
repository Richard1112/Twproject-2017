<%@ page import="org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState" %><%
  PageState pageState = PageState.getCurrentPageState(request);

%>
<table align="left" cellspacing=5 class="legendaPlan">
  <tr>
  <td align="center"><i><%=I18n.get("PLAN_LEGENDA")%>:</i></td>
  <td class="color dayH">&nbsp;</td><td><%=I18n.get("PLAN_HOLIDAYS_LEGENDA")%>&nbsp;&nbsp;&nbsp;</td>
  <td class="color routine"></td><td><%=I18n.get("PLAN_NOT_ALL_IN_ONE_LEGENDA")%>&nbsp;&nbsp;&nbsp;</td>
  <td class="color outOfScope"></td><td><%=I18n.get("PLAN_OUT_TASK_SCOPE_LEGENDA")%>&nbsp;&nbsp;&nbsp;</td>
  <td class="color notAvailable"></td><td><%=I18n.get("PLAN_IN_VACATION_LEGENDA")%>&nbsp;&nbsp;&nbsp;</td>
  <td class="color exceeded"></td><td><%=I18n.get("PLAN_CAPACITY_EXCEEDED_LEGENDA")%>&nbsp;&nbsp;&nbsp;</td>
  <td class="color overPlanned"></td><td><%=I18n.get("PLAN_OVERPLANNED_LEGENDA")%>&nbsp;&nbsp;&nbsp;</td>
  <%if (I18n.isActive("CUSTOM_FEATURE_SHOW_UNDERPLANNED")){%>
  <td class="color underplanned"></td><td><%=I18n.get("PLAN_UNDERPLANNED_LEGENDA")%>&nbsp;&nbsp;&nbsp;</td>
  <%}%>


  <td class="color hasPlan" style="position: relative;"><span class="noteEditorButton" title="<%=I18n.get("NOTES")%>"></span></td><td class="color hasPlan hasNotes" style="position: relative;"><span class="noteEditorButton" title="<%=I18n.get("NOTES")%>"></span></td><td align="center"><%=I18n.get("NOTES")%></td>


<%--
  <td align="center">&nbsp;&nbsp;&nbsp;<i><%=I18n.get("PLAN_BAR_LEGENDA")%>:</i></td>
  <td class="color focused">&nbsp;</td><td><%=I18n.get("PLAN_FOCUSED_PERIOD_LEGENDA")%>&nbsp;&nbsp;&nbsp;</td>
  <td class="color highlightPeriod">&nbsp;</td><td><%=I18n.get("PLAN_TASKS_LEGENDA")%>&nbsp;&nbsp;&nbsp;</td>
--%>
</tr>
</table>
