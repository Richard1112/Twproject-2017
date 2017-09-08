<%@ page import="org.jblooming.oql.OqlQuery, org.jblooming.utilities.JSP,
org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields,
org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n,
org.jblooming.waf.settings.I18nEntryPersistent, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.*"%><html>
<head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/></head><%
  PageState pageState = PageState.getCurrentPageState(request);
  pageState.getLoggedOperator().testIsAdministrator();

  response.setContentType("application/vnd.ms-excel");
  response.setHeader("Content-Disposition", "attachment; filename=\"labelsI18n.xls\"");

  I18n i18nManager = ApplicationState.i18n;
  String searchText = pageState.getEntry("SEARCH_TEXT").stringValue();

  boolean searchTextEnabled = JSP.ex(searchText);

  boolean suspectSearchEnabled = pageState.getEntry("SUSPECT").checkFieldValue();
  boolean searchFromDBOnlyEnabled = !ApplicationState.platformConfiguration.development && pageState.getEntry("FROMDBONLY").checkFieldValue();

  boolean searchEnabled = searchTextEnabled || suspectSearchEnabled || searchFromDBOnlyEnabled;

  // load from db
    Set<String> entryFromDB = new HashSet();
    if (searchFromDBOnlyEnabled) {
      List<I18nEntryPersistent> en = new OqlQuery("from " + I18nEntryPersistent.class.getName()).list();
      for (I18nEntryPersistent i : en)
        entryFromDB.add(i.getCode());
    }

  boolean filterForLanguageAvailable = false;

  String focusedApplicationName = pageState.getEntry(Fields.FORM_PREFIX + "APPLICATION").stringValue();

  if (focusedApplicationName == null) {
    focusedApplicationName = "";
  }

  SortedMap entries;
  if (!JSP.ex(focusedApplicationName.length())) {
    entries = i18nManager.codeEntries;
  } else {
    entries = I18n.getEntriesForApplication(focusedApplicationName);
  }

  %><body><table class="table"> <%

  for (Iterator iterator = entries.keySet().iterator(); iterator.hasNext(); ) {
    String code = (String) iterator.next();

    PageSeed edit = new PageSeed("i18nEdit.jsp");
    if (pageState.getEntry(Fields.APPLICATION_NAME).stringValueNullIfEmpty() != null)
      edit.addClientEntry(Fields.APPLICATION_NAME,pageState.getEntry(Fields.APPLICATION_NAME).stringValueNullIfEmpty());
    edit.setCommand(Commands.EDIT);
    edit.addClientEntry(Fields.FORM_PREFIX + "code", code);

    I18n.I18nEntry i18nEntry = (I18n.I18nEntry) (entries.get(code));


    if (searchTextEnabled && !i18nEntry.matches(searchText))
      continue;

    if (suspectSearchEnabled && !i18nEntry.isSuspect())
      continue;

    if (!ApplicationState.platformConfiguration.development && searchFromDBOnlyEnabled && !entryFromDB.contains(i18nEntry.code))
      continue;

      %> <tr> <%
      for (Iterator iterator1 = i18nEntry.applicationEntries.keySet().iterator(); iterator1.hasNext(); ) {
        String applicationName = (String) iterator1.next();

        I18n.ApplicationEntry applicationEntry = (I18n.ApplicationEntry) i18nEntry.applicationEntries.get(applicationName);

       %> <td><%=applicationName%></td>

        <td><%=code%></td><%


    for (Iterator iterator2 = i18nManager.supportedLanguages.iterator(); iterator2.hasNext(); ) {
      String lang = (String) iterator2.next();

      if (!filterForLanguageAvailable || pageState.getEntry(Fields.FORM_PREFIX + "LANGUAGES" + lang).stringValue() != null) {
  %>

    <td><%=lang%></td>

    <td><% String entry = (String) applicationEntry.entries.get(lang);
      if (entry != null) {
    %><%=entry%><%
    } else {
    %> &nbsp;<%
      }
    %></td><%

        }
      }
    }
  %>
  </tr><%
    
  }
%>
</table></body></html>