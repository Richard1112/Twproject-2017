<%@ page import="com.opnlb.fulltext.IndexingConstants, com.opnlb.fulltext.waf.IndexingBricks, org.jblooming.utilities.StringUtilities, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.input.ComboBox, org.jblooming.waf.html.input.TextField, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.view.PageState" %><%

  PageState pageState = PageState.getCurrentPageState(request);

  if (!Commands.SAVE.equals(pageState.command)) {
    pageState.addClientEntry(IndexingConstants.INDEX_PATH, ApplicationState.getApplicationSetting(IndexingConstants.INDEX_PATH));
    pageState.addClientEntry(IndexingConstants.ANALYZER_LANGUAGE, ApplicationState.getApplicationSetting(IndexingConstants.ANALYZER_LANGUAGE));
  }

  boolean teamwork_host_mode =ApplicationState.isHostMode;


%>
<tr class="index">
   <td><%

     TextField textField = new TextField(StringUtilities.replaceAllNoRegex(IndexingConstants.INDEX_PATH, "_", " ").toLowerCase(), IndexingConstants.INDEX_PATH, "</td><td>", 30, false);
     textField.toolTip = IndexingConstants.INDEX_PATH;
     textField.readOnly=teamwork_host_mode;
     textField.toHtmlI18n(pageContext);%>
  </td>
  <td><%

  if (System.getProperty("os.name").indexOf("Windows")>-1) {
    %>c:\demo\index <%
  } else {
    %>usr/local/myapp/index<%
  }
  %>

  </td>
  <td>Folder where all Lucene index files will be saved. You must restart the web app to make indexing effective.<br>

    <%
    ButtonLink bl = new ButtonLink("Reindex entities.",pageState.pageFromRoot("administration/indexingTeamwork.jsp"));
    bl.toHtmlInTextOnlyModality(pageContext);
  %>

  </td>

</tr>
<tr class="index">
   <td nowrap><%
     ComboBox cb = new ComboBox(IndexingConstants.ANALYZER_LANGUAGE, StringUtilities.replaceAllNoRegex(IndexingConstants.ANALYZER_LANGUAGE, "_", " ").toLowerCase(), "STEMMER", pageState);
     cb.values = IndexingBricks.getStemmers();
     cb.fieldSize=15;
     cb.separator="</td><td nowrap>";
     cb.divWidth=150;
     cb.toolTip = IndexingConstants.ANALYZER_LANGUAGE;
     cb.toHtmlI18n(pageContext);%>
  </td>
  <td>English
  </td>
  <td>Language used by Snowball analyzer: examples are "English", "German", "Spanish", "French", "Italian". <%


    %><br><b>You must restart the web app, and then reindex the whole set of objects, to make the change effective.</b>
    </td>

</tr>
