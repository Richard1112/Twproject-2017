<%@ page
        import="com.dropbox.client2.session.*, org.jblooming.remoteFile.FileStorage, org.jblooming.remoteFile.RemoteFileDropBox, org.jblooming.tracer.Tracer, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.view.PageState, java.util.HashMap, java.util.Map" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  try {
    String requestToken = session.getAttribute("DBRT") + "";
    String requestTokenSecret = session.getAttribute("DBRTS") + "";
    Map configDB = new HashMap();
    configDB = RemoteFileDropBox.generateConfigMap(configDB);
    WebAuthSession was = new WebAuthSession(new AppKeyPair(configDB.get("consumer_key") + "", configDB.get("consumer_secret") + ""), Session.AccessType.DROPBOX);


    String accessTokens = was.retrieveWebAccessToken(new RequestTokenPair(requestToken, requestTokenSecret));
    AccessTokenPair accessToken = was.getAccessTokenPair();

    /* Authenticator auth = new Authenticator(configDB);
    auth.consumer.setTokenWithSecret(requestToken, requestTokenSecret);
    auth.retrieveAccessToken("");*/
    String docId = pageState.getEntry("docId").stringValueNullIfEmpty();

      FileStorage f = FileStorage.load(docId);
      if (f != null) {
        f.setConnectionPwd(accessToken.key + "_" + accessToken.secret);
        f.setConnectionUser(accessToken.key + "_" + accessToken.secret);
        f.store();

    }
    response.sendRedirect(ApplicationState.contextPath + "/applications/teamwork/document/fileStorageEditor.jsp?CM=ED&OBJID=" + docId);

  } catch (Throwable e) {
    Tracer.platformLogger.error("Drobpbox error retriving access token -  probably the service is down");
  }


%>