<%@ page import="org.apache.log4j.Level,
                 org.jblooming.system.SystemConstants,
                 org.jblooming.tracer.Tracer,
                 org.jblooming.utilities.HashTable,
                 org.jblooming.utilities.HttpUtilities,
                 org.jblooming.utilities.file.FileUtilities,
                 org.jblooming.utilities.file.fileStorage.FileStorageUtilities,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.constants.Fields,
                 org.jblooming.waf.html.button.ButtonImg,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.button.ButtonSupport,
                 org.jblooming.waf.html.container.Container,
                 org.jblooming.waf.html.display.Img,
                 org.jblooming.waf.html.input.CheckField,
                 org.jblooming.waf.html.input.RadioButton,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.settings.PlatformConfiguration,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState,
                 java.io.File,
                 java.io.FileFilter,
                 java.util.HashSet,
                 java.util.Iterator,
                 java.util.Map, org.apache.log4j.Logger" %>
<%!

  public class LogFileFilter implements FileFilter {

    private final String[] fileExtension = new String[]{".log"};

    public boolean accept(File file) {
      boolean retVal = false;
      if (file.isFile()) {
        for (String tmp : fileExtension) {
          if (file.getName().toLowerCase().endsWith(tmp)) {
            retVal = true;
          }
        }
      }
      return retVal;
    }
  }

%><%

  PageState pageState = PageState.getCurrentPageState(request);

    if (Commands.SAVE.equals(pageState.getCommand())) {
      Map<String, String> logSettings = new HashTable();

      Tracer.platformLogger.setLevel(Level.toLevel(pageState.getEntry(SystemConstants.FLD_LOG_PLATFORM_LEVEL).intValue()));
      Tracer.hibernateLogger.setLevel(Level.toLevel(pageState.getEntry(SystemConstants.FLD_LOG_HIB_LEVEL).intValue()));
      Tracer.i18nLogger.setLevel(Level.toLevel(pageState.getEntry(SystemConstants.FLD_LOG_I18N_LEVEL).intValue()));
      Tracer.jobLogger.setLevel(Level.toLevel(pageState.getEntry(SystemConstants.FLD_LOG_JOB_LEVEL).intValue()));
      Tracer.emailLogger.setLevel(Level.toLevel(pageState.getEntry(SystemConstants.FLD_LOG_EMAIL_LEVEL).intValue()));
      Tracer.emailLogger.setLevel(Level.toLevel(pageState.getEntry(SystemConstants.FLD_LOG_EMAIL_LEVEL).intValue()));

      //JES
      Logger.getLogger("JESLogger").setLevel(Level.toLevel(pageState.getEntry(SystemConstants.FLD_LOG_EMAIL_LEVEL).intValue()));

      PlatformConfiguration.logOnConsole = pageState.getEntry(SystemConstants.FLD_LOG_ON_CONSOLE).checkFieldValue();
      PlatformConfiguration.logOnFile = pageState.getEntry(SystemConstants.FLD_LOG_ON_FILE).checkFieldValue();

      logSettings.put("logOnFile", PlatformConfiguration.logOnFile ? Fields.TRUE : Fields.FALSE);
      logSettings.put("logOnConsole", PlatformConfiguration.logOnConsole ? Fields.TRUE : Fields.FALSE);

      logSettings.put("hibernateLogLevel", "" + Tracer.hibernateLogger.getLevel().toString());
      logSettings.put("platformLogLevel", "" + Tracer.platformLogger.getLevel().toString());
      logSettings.put("i18nLogLevel", "" + Tracer.i18nLogger.getLevel().toString());
      logSettings.put("jobLogLevel", "" + Tracer.jobLogger.getLevel().toString());
      logSettings.put("emailLogLevel", "" + Tracer.emailLogger.getLevel().toString());


      String logPropetiesPath = HttpUtilities.getFileSystemRootPathForRequest(request) + File.separator + "WEB-INF" + File.separator + PlatformConfiguration.logSettingsFileName;
      String value = "## LOG\n\n ## levels may DEBUG, WARN, ERROR\n\n";
      for (Iterator iterator = new HashSet(logSettings.keySet()).iterator(); iterator.hasNext();) {
        String key = (String) iterator.next();
        String prop = (String) logSettings.get(key);
        value += key + "=" + prop + "\n";
      }
      if (value != null) {
        FileUtilities.writeToFile(logPropetiesPath, value, "UTF-8"); //was in old times value.replace('\\','/'), who knows why ? ppolsinelli@open-lab.com
      }
    } else {
      pageState.addClientEntry(SystemConstants.FLD_LOG_PLATFORM_LEVEL, Tracer.platformLogger.getLevel().toInt() + "");
      pageState.addClientEntry(SystemConstants.FLD_LOG_HIB_LEVEL, Tracer.hibernateLogger.getLevel().toInt() + "");
      pageState.addClientEntry(SystemConstants.FLD_LOG_I18N_LEVEL, Tracer.i18nLogger.getLevel().toInt() + "");
      pageState.addClientEntry(SystemConstants.FLD_LOG_JOB_LEVEL, Tracer.jobLogger.getLevel().toInt() + "");
      pageState.addClientEntry(SystemConstants.FLD_LOG_EMAIL_LEVEL, Tracer.emailLogger.getLevel().toInt() + "");
      pageState.addClientEntry(SystemConstants.FLD_LOG_ON_FILE, PlatformConfiguration.logOnFile);
      pageState.addClientEntry(SystemConstants.FLD_LOG_ON_CONSOLE, PlatformConfiguration.logOnConsole);
    }

    PageSeed self = pageState.thisPage(request);
    self.setCommand(Commands.SAVE);
    if (pageState.getEntry(Fields.APPLICATION_NAME).stringValueNullIfEmpty() != null)
      self.addClientEntry(Fields.APPLICATION_NAME, pageState.getEntry(Fields.APPLICATION_NAME).stringValueNullIfEmpty());
  


    String root = request.getSession().getServletContext().getRealPath("/").replaceAll("\\\\", "/");
    String logPath = root + "WEB-INF/log/";

%><h2><%=I18n.get("LOG_CONFIGURATION")%></h2>
<div class="container level_2" id="ct_logAndMonitoring">
<table class="logSettings table edged">
  <tr>
    <td colspan="2">
        <%Level levels[] = {Level.ALL, Level.DEBUG, Level.INFO, Level.WARN, Level.ERROR, Level.FATAL}; %>
  </td>
  </tr>
  <tr>
    <td>"platform" <%=I18n.get("LOG_LEVEL")%>:</td>
    <td><%
      for (int i = 0; i < levels.length; i++) {
        RadioButton rbs = new RadioButton(levels[i].toString().toLowerCase(), SystemConstants.FLD_LOG_PLATFORM_LEVEL, levels[i].toInt() + "",
            "&nbsp;", "", false, "");
        rbs.translateLabel = false;
        rbs.toHtml(pageContext);
    %>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<%
      }
    %></td>
  </tr>
  <tr>
    <td>"persistence" <%=I18n.get("LOG_LEVEL")%>:</td>
    <td><%
      for (int i = 0; i < levels.length; i++) {
        RadioButton rbs = new RadioButton(levels[i].toString().toLowerCase(), SystemConstants.FLD_LOG_HIB_LEVEL, levels[i].toInt() + "",
            "&nbsp;", "", false, "");
        rbs.translateLabel = false;
        rbs.toHtml(pageContext);
    %>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<%
      }

    %></td>
  </tr>

  <tr>
    <td>"i18n" <%=I18n.get("LOG_LEVEL")%>:</td>
    <td><%
      for (int i = 0; i < levels.length; i++) {
        RadioButton rbs = new RadioButton(levels[i].toString().toLowerCase(), SystemConstants.FLD_LOG_I18N_LEVEL, levels[i].toInt() + "",
            "&nbsp;", "", false, "");
        rbs.translateLabel = false;
        rbs.toHtml(pageContext);
    %>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<%
      }
    %></td>
  </tr>

  <tr>
    <td>"job" <%=I18n.get("LOG_LEVEL")%>:</td>
    <td><%
      for (int i = 0; i < levels.length; i++) {
        RadioButton rbs = new RadioButton(levels[i].toString().toLowerCase(), SystemConstants.FLD_LOG_JOB_LEVEL, levels[i].toInt() + "",
            "&nbsp;", "", false, "");
        rbs.translateLabel = false;
        rbs.toHtml(pageContext);
    %>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<%
      }
    %></td>
  </tr>

  <tr>
    <td>"email" <%=I18n.get("LOG_LEVEL")%>:</td>
    <td><%
      for (int i = 0; i < levels.length; i++) {
        RadioButton rbs = new RadioButton(levels[i].toString().toLowerCase(), SystemConstants.FLD_LOG_EMAIL_LEVEL, levels[i].toInt() + "",
            "&nbsp;", "", false, "");
        rbs.translateLabel = false;
        rbs.toHtml(pageContext);
    %>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<%
      }
    %></td>

  </tr>
  <tr>
    <td colspan="2">&nbsp;</td>
  </tr>
  <tr>
    <td colspan="2"><b><%=I18n.get("LOG_OUT_MODALITY")%>
    </b>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<%

      CheckField rbs = new CheckField(SystemConstants.FLD_LOG_ON_FILE, "&nbsp;", false);
      rbs.label = I18n.get("LOG_ON_FILE");
      rbs.toHtml(pageContext);
    %>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<%
      rbs = new CheckField(SystemConstants.FLD_LOG_ON_CONSOLE, "&nbsp;", false);
      rbs.label = I18n.get("LOG_ON_CONSOLE");
      rbs.toHtml(pageContext);

    %></td>
  </tr>
  
</table>
<%

  if (PlatformConfiguration.logOnFile) {
    //Container logFiles = new Container();
    //logFiles.title = I18n.get("LOG_FILES_IN") + ":  " + logPath;
    //logFiles.start(pageContext);%>

  <h2><%=I18n.get("LOG_FILES_IN") + ":  " + logPath%></h2><%

  File logs = new File(logPath);

  File[] logFls = logs.listFiles(new LogFileFilter());
  if (logFls != null && logFls.length > 0) {
%>


<table class="table edged" cellpadding="3" cellspacing="1" border="0">
  <tr>
    <th align="left"class="tableHead"><%=I18n.get("FILE_NAME")%></th>
    <th align="center"class="tableHead"><%=I18n.get("FILE_LENGTH")%></th>
    <th align="center"class="tableHead"><%=I18n.get("FILE_VIEW")%></th>
    <th align="center"class="tableHead"><%=I18n.get("FILE_DOWNLOAD")%></th>
    <th align="center"class="tableHead"><%=I18n.get("FILE_ZIP")%></th>
  </tr>
  <%
    int cont = 1;
    for (File tmpFile : logFls) {
  %>
  <tr><%
  %>
    <td><%=tmpFile.getName()%>
    </td>
    <td align="center">(<%=FileStorageUtilities.convertFileSize(tmpFile.length())%>)</td>
    <td align="center">
      <%
        PageSeed showLog = pageState.pageFromRoot("administration/showLog.jsp");
        showLog.addClientEntry("LOG", tmpFile.getName());

        ButtonSupport bjs=ButtonLink.getBlackInstance("show logs", 768, 1024, showLog);
        bjs.iconChar="c";
        bjs.toolTip=I18n.get("VISUALIZE_LOG");
        bjs.label = "";
        bjs.toHtmlInTextOnlyModality(pageContext);
      %>
    </td>
    <td align="center">
      <%
        showLog.command="down";
        bjs = new ButtonLink(showLog);
        bjs.toolTip=I18n.get("LOG_DOWNLOAD_NO_ZIP");
        bjs.iconChar = "Z";
        bjs.label = "";
        bjs.toHtmlInTextOnlyModality(pageContext);
      %>
    </td>
    <td align="center">
      <%
        showLog.command="zip";
        bjs = new ButtonLink(showLog);
        ButtonImg btn = new ButtonImg(bjs, new Img( "mime/application_x-tar.png", I18n.get("LOG_DOWNLOAD_ZIP"),"20",""));
        bjs.label = "";
        btn.toHtml(pageContext);
      %>
    </td>

    </tr>
  <%
      cont++;
    }

  %></table>
<%

} else {
%><%=I18n.get("NO_LOG_FILES")%><%
  }
  //logFiles.end(pageContext);
%></div><br><%
  }

  

%></div>
<div id="logViewer"></div>
