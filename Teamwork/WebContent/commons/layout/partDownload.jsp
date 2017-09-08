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
  Document document = (Document) PersistenceHome.findByPrimaryKey(Document.class, pageState.mainObjectId);
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