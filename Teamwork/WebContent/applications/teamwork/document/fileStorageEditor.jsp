<%@ page import=" com.dropbox.client2.session.AppKeyPair,
                  com.dropbox.client2.session.RequestTokenPair,
                  com.dropbox.client2.session.Session,
                  com.dropbox.client2.session.WebAuthSession,
                  com.twproject.document.TeamworkDocument,
                  com.twproject.document.businessLogic.FileStorageController,
                  com.twproject.operator.TeamworkOperator,
                  com.twproject.security.SecurityBricks,
                  com.twproject.security.TeamworkPermissions,
                  com.twproject.waf.TeamworkHBFScreen,
                  org.jblooming.remoteFile.Document,
                  org.jblooming.remoteFile.FileStorage,
                  org.jblooming.remoteFile.RemoteFile,
                  org.jblooming.remoteFile.RemoteFileDropBox,
                  org.jblooming.system.SystemConstants,
                  org.jblooming.tracer.Tracer,
                  org.jblooming.utilities.CodeValueList, org.jblooming.utilities.JSP,
                  org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink,
                  org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.ButtonBar,
                  org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.html.display.PathToObject, org.jblooming.waf.html.input.Combo,
                  org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed,  org.jblooming.waf.view.PageState, java.util.HashMap, java.util.Map"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new FileStorageController(), request);
    body.areaHtmlClass="lreq30 lreqPage";
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);

  } else {

    TeamworkOperator logged=(TeamworkOperator) pageState.getLoggedOperator();


%><script>$("#TOOLS_MENU").addClass('selected');</script><%

    //TeamworkDocument document = (TeamworkDocument) pageState.getMainObject();
    FileStorage document = (FileStorage) pageState.getMainObject();


    String connType="";
    boolean rootExists = false;
    if (!document.isNew()) {
      RemoteFile rfs = RemoteFile.getInstance(document);
      connType=document.getConnType()+"";
      rootExists = rfs.exists();
    }

    boolean canWrite = document.hasPermissionFor(logged, TeamworkPermissions.fileStorage_canWrite);
    boolean canCreate = document.hasPermissionFor(logged, TeamworkPermissions.fileStorage_canCreate);

    PageSeed self = pageState.thisPage(request);
    self.mainObjectId = document.getId();
    self.setCommand(Commands.SAVE);
    Form f = new Form(self);
    pageState.setForm(f);
  f.start(pageContext);

%><%

    PathToObject pto = new PathToObject(document);
    PageSeed back = new PageSeed("fileStorageList.jsp");
    back.setCommand(Commands.FIND);
    ButtonLink dicList = new ButtonLink(I18n.get("FILE_STORAGE_LIST"), back);
    pto.rootDestination = dicList;
    pto.destination = pageState.thisPage(request);
    pto.destination.setCommand(Commands.EDIT);
    pto.toHtml(pageContext);


%>
<table border="0">
  <tr>
    <td><%
    TextField tf = new TextField("DOCUMENT_CODE", "<br>");
    tf.readOnly=!canWrite;
    tf.fieldSize = 12;
    tf.toHtmlI18n(pageContext);
    %>&nbsp;<%=document.isNew() ? "" : "(id:&nbsp;"+document.getId()+")"%><%
  %></td><td><%

    tf = new TextField("DOCUMENT_NAME", "<br>");
    tf.readOnly=!canWrite;
    tf.fieldSize = 50;
    tf.required = true;
    tf.toHtmlI18n(pageContext);

  %></td><td class="<%=SecurityBricks.isSingleArea()?"displayNone":""%>"><%

      Combo cb = SecurityBricks.getAreaCombo("AREA",document.isNew() ? TeamworkPermissions.fileStorage_canCreate : TeamworkPermissions.fileStorage_canWrite,pageState);
      cb.disabled=!canWrite;
      cb.separator="<br>";
      cb.toHtmlI18n(pageContext);


  %></td>
  </tr>
  <tr>
    <td colspan="2"><%

      tf = new TextField("DOCUMENT_URL_TO_CONTENT", "<br>");
      tf.readOnly=!canWrite;
      tf.fieldSize = 80;
      //tf.required = true;
      tf.toHtmlI18n(pageContext);

    if (!document.isNew() && !rootExists) {
      %>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="warning"><%=I18n.get("FILE_DOES_NOT_EXIST")%></span><%
    }

  %></td><td><%
    if (true||document.isNew()) {
    CodeValueList cvl = new CodeValueList();
    cvl.addChoose(pageState);
    for (Document.ConnectionType connectionType : Document.enabledConnectionTypes) {
      cvl.add(connectionType.toString(), I18n.get(connectionType.toString()));
    }
    Combo cbT = new Combo("connectionType", "<br>", "", 255, cvl, null);
    cbT.setJsOnChange = "enableInputFileStorage(this.value);";
    cbT.disabled=!canWrite;
    cbT.label=I18n.get("CONNECTION_TYPE");
    cbT.toHtml(pageContext);
  } else {
    TextField.hiddenInstanceToHtml("connectionType",document.getConnType()+"",pageContext);

    %><%=I18n.get("connectionType")%><br><%=I18n.get(document.getConnType() + "")%><%
  }


  %></td></tr>
  </table>
 <hr>
<table><%

  String spa = ApplicationState.getApplicationSetting(SystemConstants.STORAGE_PATH_ALLOWED);
  String displayDropbox = "none";
  String displayConnectionData = "none";
  if(JSP.ex(connType)){
    if("DROPBOX".equals(connType)){
      displayDropbox = "block";
      displayConnectionData = "none";
    }else{
      displayDropbox = "none";
      displayConnectionData = "block";
    }
  }

  if (spa == null || spa.trim().length()==0) {
   %><tr class="rfsHelp"><td colspan="4"><span class="warning"><%=JSP.wHelp("STORAGE_PATH_ALLOWED value not found or not set: set it in tools -> administration -> global settings")%></span></td></tr><%
  } else {
     %><tr class="rfsHelp"><td colspan="4"><%=JSP.wHelp(I18n.get("FILE_STORAGE_PATH_HELP")) %>: <%=spa%></td></tr><%
  }

  %>
  <tr><td id="dropboxButton" style="display:<%=displayDropbox%>" colspan="3"><%
    if(!document.isNew() && "DROPBOX".equals(document.getConnType().name())){
    try {
      Map configDB = new HashMap();
      configDB = RemoteFileDropBox.generateConfigMap(configDB);

      WebAuthSession was = new WebAuthSession(new AppKeyPair(configDB.get("consumer_key") + "", configDB.get("consumer_secret") + ""), Session.AccessType.DROPBOX);
      String callbackParam = RemoteFileDropBox.CALLBACK_URL+ (document.isNew() ? "" : "?docId=" + document.getId());
      WebAuthSession.WebAuthInfo info = was.getAuthInfo(callbackParam);
      String url = info.url;
      RequestTokenPair rtp = info.requestTokenPair;
      session.setAttribute("DBRT", rtp.key);
      session.setAttribute("DBRTS", rtp.secret);
      ButtonLink authorizeDB = new ButtonLink(new PageSeed(url));
      authorizeDB.inhibitParams = true;
      authorizeDB.label = I18n.get("ENABLE_DROPBOX");
      authorizeDB.additionalCssClass = "enableBtn";
      authorizeDB.enabled = !document.isNew();
      authorizeDB.toHtml(pageContext);
    } catch (Throwable e) {
      e.printStackTrace();
      Tracer.platformLogger.error("dropbox error retriving request token -  probably the service is down");
    }
    }  
  %></td></tr>
  <tr id="conn_data" style="display:<%=displayConnectionData%>">
    <td colspan="3">
      <h2><%=I18n.get("CONNECTION_DATA")%></h2><table class="table"><tr>
      <td id="conn_host"><%
            tf = new TextField("connectionHost", "<br>");
            tf.label="<span>"+I18n.get("connectionHost")+"</span>";
            tf.fieldSize = 20;
            tf.toHtml(pageContext);

        %></td><td id="conn_user"><%

            tf = new TextField("connectionUser", "<br>");
            tf.label="<span>"+I18n.get("connectionUser")+"</span>";
            tf.fieldSize = 20;
            tf.toHtml(pageContext);

        %></td><td id="conn_pwd"><%

          tf = new TextField("connectionPwd", "<br>");
          tf.type="password";
          tf.label="<span>"+I18n.get("connectionPwd")+"</span>";
          tf.fieldSize = 20;
          tf.toHtmlI18n(pageContext);

        %></td>
    </tr></table>
      </td>
    </tr>
  </table>
  <script type="text/javascript">
    function enableInputFileStorage(val) {
      if (val == 'SVN' || val == 'SVN_Http' || val == 'SVN_Https' || val == 'FTP' || val == 'SFTP') {
        $("#conn_data").show();
        $('#conn_host').find("span").text("host");
        $('#conn_user').find("span").text("user");
        $('#conn_pwd').find("span").text("password");
        $("tr.rfsHelp").hide();

      } else if (val == 'S3') {
        $("#conn_data").show();
        $('#conn_host').find("span").text("bucket");
        $('#conn_user').find("span").text("access key");
        $('#conn_pwd').find("span").text("secret key");
        $("tr.rfsHelp").hide();
      } else if(val == "DROPBOX"){
        $("#conn_data").hide();
        $('#dropboxButton').show();
        $("tr.rfsHelp").hide();

      }else{

        $("#conn_data").hide();
        $("tr.rfsHelp").show();
      }
    }

    $(function(){
      enableInputFileStorage("<%=connType%>");
    });
  </script>

  <%

    ButtonBar bb2 = new ButtonBar();

      ButtonSubmit save = ButtonSubmit.getSaveInstance(f, I18n.get("SAVE"));
      save.enabled = canWrite;
      save.additionalCssClass = "big first";
      bb2.addButton(save);

    if (!document.isNew()) {

      bb2.loggableIdentifiableSupport = document;  

      PageSeed explore = pageState.pageInThisFolder("explorer.jsp", request);
      explore.mainObjectId = document.getId();


      ButtonSupport nome = ButtonLink.getBlackInstance(I18n.get("EXPLORE"), explore);
      //nome.enabled = rootExists && FileStorageUtilities.validUrlToContent(document.getContent());
      nome.enabled = rootExists;
      nome.additionalCssClass = "big";
      nome.target="explorer";


      bb2.addButton(nome);
    }

    DeletePreviewer deletePreviewer = new DeletePreviewer("PF_DEL", FileStorageController.class, pageState);
    ButtonSupport delPrev = deletePreviewer.getDeleteButton(I18n.get("DELETE"), document.getId() );
    delPrev.enabled = !document.isNew() && canCreate;
    delPrev.additionalCssClass="big delete";
    bb2.addButton(delPrev);


    bb2.toHtml(pageContext);

    f.end(pageContext);
  }
%>
