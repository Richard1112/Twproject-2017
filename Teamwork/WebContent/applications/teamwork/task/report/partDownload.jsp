<%@page import="org.jblooming.remoteFile.Document.ConnectionType"%>
<%@page import="org.jblooming.system.SystemConstants"%>
<%@page import="org.jblooming.waf.settings.ApplicationState"%>
<%@page import="org.jblooming.remoteFile.FileStorage"%>
<%@ page import=" org.jblooming.persistence.PersistenceHome,
                  org.jblooming.remoteFile.BasicDocumentBricks,
                  org.jblooming.remoteFile.Document,
                  org.jblooming.remoteFile.RemoteFile,
                  org.jblooming.utilities.HttpUtilities,
                  org.jblooming.utilities.StringUtilities,
                  org.jblooming.utilities.file.FileUtilities, org.jblooming.waf.view.PageState,
                  java.io.BufferedInputStream, java.io.InputStream, java.net.URLEncoder"%><%

  response.resetBuffer();
  PageState pageState = PageState.getCurrentPageState(request);
  Document document = new FileStorage();
  document.setConnType(ConnectionType.FS);
  document.setContent(ApplicationState.getApplicationSetting(SystemConstants.FLD_REPOSITORY_URL));
  String path = pageState.getEntry("PATH").stringValueNullIfEmpty();
  String ck = pageState.getEntry("CK").stringValueNullIfEmpty();
  if (!BasicDocumentBricks.checkChecksum(pageState.mainObjectId, path, ck))
    return;

  RemoteFile rfs = RemoteFile.getInstance(document);
  rfs.setTarget(path);

  response.setContentType(HttpUtilities.getContentType(rfs.getName()));

  String filename = rfs.getDisplayName();
  String filenameEncoded =  URLEncoder.encode(filename, "UTF8");
  filenameEncoded = StringUtilities.replaceAllNoRegex(StringUtilities.replaceAllNoRegex(filenameEncoded, "+", "_"), " ", "_");
  if (pageState.isPopup())
    response.setHeader("content-disposition", "attachment; filename=" + filenameEncoded);
  else
    response.setHeader("content-disposition", "inline; filename=" + filenameEncoded);


  InputStream remoteInputStream = rfs.getRemoteInputStream();
  BufferedInputStream fr = new BufferedInputStream(remoteInputStream);


  // write data to stream and close it
  FileUtilities.writeStream(fr, response.getOutputStream());

  fr.close();



%>