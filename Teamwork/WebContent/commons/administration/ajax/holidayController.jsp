<%@ page import="net.sf.json.JSONObject, org.jblooming.agenda.CompanyCalendar, org.jblooming.operator.Operator, org.jblooming.utilities.DateUtilities, org.jblooming.waf.JSONHelper, org.jblooming.waf.view.ClientEntry, org.jblooming.waf.view.PageState, java.util.Map" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  Operator logged = pageState.getLoggedOperator();
  JSONHelper jsonHelper = new JSONHelper();
  JSONObject json = jsonHelper.json;
  try {

    if ("SVHOL".equals(pageState.command)) {
      if (logged.hasPermissionAsAdmin()) {

        Map<String, ClientEntry> entryMap = pageState.getClientEntries().getEntriesStartingWithStripped("DAY_");

        CompanyCalendar cc = new CompanyCalendar();

        for (String day : entryMap.keySet()) {
          cc.setTime(DateUtilities.dateFromString(day, "yyyy_MM_dd"));
          int val = entryMap.get(day).intValueNoErrorCodeNoExc();
          if (val == 0)
            cc.removeHoliday();
          else if (val == 1)
            cc.setHoliday(false);
          else if (val == 2)
            cc.setHoliday(true);

        }
        cc.storeHolidays();
      }

    }
  } catch (Throwable t) {
    jsonHelper.error(t);
  }
  jsonHelper.close(pageContext);
%>