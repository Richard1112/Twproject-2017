<%@ page import="org.jblooming.ontology.PersistentFile,
                  org.jblooming.system.SystemConstants,
                  org.jblooming.utilities.CodeValueList,
                  org.jblooming.utilities.JSP,
                  org.jblooming.waf.constants.Commands,
                  org.jblooming.waf.html.button.ButtonLink,
                  org.jblooming.waf.html.button.ButtonSupport,
                  org.jblooming.waf.html.input.Combo, org.jblooming.waf.html.input.TextField, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.io.File" %><%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!Commands.SAVE.equals(pageState.command)) {
    pageState.addClientEntry(SystemConstants.FLD_REPOSITORY_URL, ApplicationState.getApplicationSetting(SystemConstants.FLD_REPOSITORY_URL));
    pageState.addClientEntry("DEFAULT_STORAGE_TYPE", ApplicationState.getApplicationSetting("DEFAULT_STORAGE_TYPE"));
    pageState.addClientEntry(SystemConstants.STORAGE_PATH_ALLOWED, ApplicationState.getApplicationSetting(SystemConstants.STORAGE_PATH_ALLOWED));
    pageState.addClientEntry(SystemConstants.PUBLIC_SERVER_NAME, ApplicationState.getApplicationSetting(SystemConstants.PUBLIC_SERVER_NAME));
    pageState.addClientEntry(SystemConstants.PUBLIC_SERVER_PORT, ApplicationState.getApplicationSetting(SystemConstants.PUBLIC_SERVER_PORT));
    pageState.addClientEntry(SystemConstants.HTTP_PROTOCOL, ApplicationState.getApplicationSetting(SystemConstants.HTTP_PROTOCOL));
    pageState.addClientEntry(SystemConstants.UPLOAD_MAX_SIZE, ApplicationState.getApplicationSetting(SystemConstants.UPLOAD_MAX_SIZE));
  }

  boolean teamwork_host_mode = ApplicationState.isHostMode;

  CodeValueList pfTypes= new CodeValueList();
  pfTypes.add(PersistentFile.TYPE_FILESTORAGE, I18n.get("TYPE_FILESTORAGE"));
  pfTypes.add(PersistentFile.TYPE_ENCRYPTED_FILESTORAGE,I18n.get("TYPE_ENCRYPTED_FILESTORAGE"));
  pfTypes.add(PersistentFile.TYPE_DB,I18n.get("TYPE_DB"));
  Combo pfTypesc= new Combo("DEFAULT_STORAGE_TYPE","",null,30,pfTypes,null);
  pfTypesc.label="";

  ButtonSupport gaesk = ButtonLink.getBlackInstance("Generate AES keys", 250, 500, pageState.pageFromCommonsRoot("administration/generateAESKey.jsp"));

%>

<tr class="repositorySettings"><td><%

     TextField textField = new TextField(SystemConstants.FLD_REPOSITORY_URL, SystemConstants.FLD_REPOSITORY_URL, "</td><td>", 30, false);
     textField.toolTip = SystemConstants.FLD_REPOSITORY_URL;
     textField.readOnly = teamwork_host_mode;
     textField.toHtmlI18n(pageContext);%>
  </td>
  <td><%

  if (System.getProperty("os.name").indexOf("Windows")>-1) {
    %>c:\demo\repository <%
  } else {
    %>usr/local/teamwork/repository<%
  }

  %></td>
  <td>Folder where all files uploaded will be saved.<%

    String FLD_REPOSITORY_URL = ApplicationState.getApplicationSetting(SystemConstants.FLD_REPOSITORY_URL);
    if (FLD_REPOSITORY_URL != null && !new File(FLD_REPOSITORY_URL).exists()) {
  %><br><big><span class="warning"><b><%=SystemConstants.FLD_REPOSITORY_URL%> does
    not seem to exist: <%=FLD_REPOSITORY_URL%>!</b></span></big><%
  }
    %></td>
</tr>

<tr class="repositorySettings">
  <td valign="top" class="lreq30"><label>repository default type</label></td>
  <td valign="top" class="lreq30"><%pfTypesc.toHtml(pageContext);%></td>
  <td valign="top" class="lreq30"><%=I18n.get("TYPE_FILESTORAGE")%></td>
  <td class="lreq30 lreqLabel">
    Uploaded files can be placed in three different types of repositories:
    <ul>
      <li>
        <b><%=I18n.get("TYPE_FILESTORAGE")%></b>: upload files on the folder specified in the "repository url" parameter
      </li>
      <li>
        <b><%=I18n.get("TYPE_DB")%></b>: upload files as BLOB on database. This option will increase you db size; consider the backup time.
      </li>
      <li>
        <b><%=I18n.get("TYPE_ENCRYPTED_FILESTORAGE")%></b>: upload as above, but files will be encrypted.<br>
        <%if (!I18n.isActive("CUSTOM_FEATURE_AES_CRYPTO_KEY")){%>
        You can set a new one here:  <%gaesk.toHtmlInTextOnlyModality(pageContext);%><br>
        Keep a copy of your key in a safe place.<br>
        <i><b>WARNING: if you change the key, you will LOST files previously encrypted!</b></i>
        <%}%>
      </li>
    </ul>
    <i>Note: existing files will use its original type and will not be touched. </i>
  </td>
</tr>


<tr class="repositorySettings"><td><%
     textField = TextField.getIntegerInstance(SystemConstants.UPLOAD_MAX_SIZE);
     textField.fieldSize=4;
     textField.toolTip = SystemConstants.UPLOAD_MAX_SIZE;
     textField.toHtmlI18n(pageContext);%>
  </td>
  <td>20</td>
  <td>Max size in MB of files uploaded.</td>
</tr>



<tr class="repositorySettings">
  <td  class="lreq30"><%

    textField = new TextField(SystemConstants.STORAGE_PATH_ALLOWED, SystemConstants.STORAGE_PATH_ALLOWED, "</td><td class=\"lreq30\">", 30, false);
    textField.toolTip = SystemConstants.STORAGE_PATH_ALLOWED;
    textField.readOnly = teamwork_host_mode;
    textField.toHtmlI18n(pageContext);

  %></td>  <td class="lreq30"><%
     if (System.getProperty("os.name").indexOf("Windows")>-1) {
    %>c:\demo\projects,d:\shared\files<%
  } else {
    %>/usr/local/demo/projects,/usr/local/demo/dataExchange<%
  }
  %></td>
  <td  class="lreq30 lreqLabel">Use comma separated paths.<%

  String STORAGE = ApplicationState.getApplicationSetting(SystemConstants.STORAGE_PATH_ALLOWED);
  if (STORAGE != null && STORAGE.indexOf(";") > -1) {
    %><big><span class="warning"><b>Must use comma ","
    as separator for storage path (<%=SystemConstants.STORAGE_PATH_ALLOWED%>),
    not semi-colon ";"!</b></span></big><br><%
  }
  else
  if (STORAGE != null && STORAGE.indexOf(",") == -1 && !new File(STORAGE).exists()) {
    %><br><big><span class="warning"><b><%=SystemConstants.STORAGE_PATH_ALLOWED%>
    contains a non existent root:
    <%=STORAGE%>!</b></span></big><%
  }

  %></td>
</tr>
<tr class="httpSettings">
  <td><%
   String psn = ApplicationState.getApplicationSetting(SystemConstants.PUBLIC_SERVER_NAME);

    textField = new TextField(SystemConstants.PUBLIC_SERVER_NAME, SystemConstants.PUBLIC_SERVER_NAME, "</td><td>", 20, false);
    textField.toolTip = SystemConstants.PUBLIC_SERVER_NAME;
    textField.toHtmlI18n(pageContext);%>

 </td>
  <td>servername.yourdomain.com</td>
  <td><a href="#domain">For details, see below.</a>
    After having saved a new value, you may test it. Warn: it may take some time: <%
    PageSeed test = pageState.pageFromCommonsRoot("administration/testSocket.jsp");
    test.addClientEntry("SOCKET_SERVER", psn);
    test.addClientEntry("SOCKET_PORT", 80);
    ButtonLink testBL = new ButtonLink("click to test server", test);
    testBL.popup_height = "100";
    testBL.popup_width = "400";
    testBL.target = "_blank";
    testBL.toHtmlInTextOnlyModality(pageContext);
  %></td>
</tr>
<tr class="httpSettings">
  <td><%

    textField = new TextField(SystemConstants.PUBLIC_SERVER_PORT, SystemConstants.PUBLIC_SERVER_PORT, "</td><td>", 4, false);
    textField.toolTip = SystemConstants.PUBLIC_SERVER_PORT;
    textField.readOnly=teamwork_host_mode;
    textField.toHtmlI18n(pageContext);%>
 </td>
  <td><%=request.getServerPort()%></td>
  <td>Normally, leave this value empty. This has to be set up by hand ONLY if the http port used locally by the server is different from
    the port which is used to contact the server from the client's browsers. Otherwise itis automatically configured.<%
     String psp = ApplicationState.getApplicationSetting(SystemConstants.PUBLIC_SERVER_PORT);
    if (psp==null || psp.trim().length()==0) {
      %>Current port used on the server is <%=request.getServerPort()%>.<%
    }
    %></td>
</tr>

<tr class="httpSettings">
  <td><%

    String protocol = request.getRequestURL().substring(0, request.getRequestURL().indexOf("//"));

    CodeValueList cvl = new CodeValueList();
    cvl.add("", "leave it as it is");
    cvl.add("http","http");
    cvl.add("https","https (http+SSL)");

    Combo cb = new Combo(SystemConstants.HTTP_PROTOCOL, "</td><td>", null, 20, cvl, null);
    cb.readOnly=teamwork_host_mode;
    cb.toolTip = SystemConstants.HTTP_PROTOCOL;
    cb.toHtmlI18n(pageContext);
%>
 </td>
  <td>http</td>
  <td>Normally, leave this value empty. This has to be set up by hand ONLY if the http protocol used locally by the server is different from
    the port which is used to contact the server from the client's browsers. Otherwise it is automatically configured.<br><%
    String protocolValue = ApplicationState.getApplicationSetting(SystemConstants.HTTP_PROTOCOL);
    if (JSP.ex(protocol)) {
      %>Protocol for this request is <%=protocol%>. <%
    }
    if (JSP.ex(protocolValue)) {
      %>Protocol in memory value is <%=protocolValue%>.<%
    }
    %> Server complete URL used is <%=ApplicationState.serverURL%>.</td>
</tr>
