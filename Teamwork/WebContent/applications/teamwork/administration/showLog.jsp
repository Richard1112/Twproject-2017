<%@ page import="org.jblooming.utilities.HttpUtilities,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.view.PageState,
                 java.io.*,
                 java.util.zip.ZipEntry, java.util.zip.ZipOutputStream" %><%!

  public void zipRemoteFile(String log, File file, ZipOutputStream zipout) throws IOException {


    ZipEntry zipEntry = new ZipEntry(log);
    zipout.putNextEntry(zipEntry);
    InputStream remoteInputStream = new FileInputStream(file);
    BufferedInputStream fr = new BufferedInputStream(remoteInputStream);
    int b;
    byte[] buf = new byte[1024];
    int len;
    while ((len = fr.read(buf)) > 0) {
      zipout.write(buf, 0, len);
    }
    fr.close();
    zipout.closeEntry();
    remoteInputStream.close();

  }
%><%



  PageState pageState = PageState.getCurrentPageState(request);

  pageState.getLoggedOperator().testIsAdministrator();

  String log = pageState.getEntry("LOG").stringValueNullIfEmpty();
  if (!JSP.ex(log))
    log = "platform.log";

  String logFolder = HttpUtilities.getFileSystemRootPathForRequest(request) + File.separator + "WEB-INF" + File.separator + "log" ;
  File logFolderF= new File(logFolder);
  String pathname = logFolder+ File.separator + log;
  File pllog = new File(pathname);

  if (!pllog.getCanonicalPath().startsWith(logFolderF.getCanonicalPath()))
    throw new org.jblooming.security.SecurityException("Invalid Log path!");

  if (pllog.exists()) {
    String command = pageState.getCommand();
    if ("zip".equals(command)) {

/*
________________________________________________________________________________________________________________________________________________________________________


zip

________________________________________________________________________________________________________________________________________________________________________

*/
      response.setContentType("application/zip");
      response.setHeader("Content-Disposition", "attachment; filename=\"" + pllog.getName() + ".zip\"");

      //get selected files
      ZipOutputStream zipout = new ZipOutputStream(response.getOutputStream());
      zipout.setComment("Twproject FileStorage Service");
      zipRemoteFile(log, pllog, zipout);

      try {
        zipout.finish();
        response.getWriter().flush();
      } catch (java.util.zip.ZipException e) {
      }
/*
________________________________________________________________________________________________________________________________________________________________________


show

________________________________________________________________________________________________________________________________________________________________________

*/
    } else {
      response.resetBuffer();
      response.setCharacterEncoding("ISO-8859-1");

      if ("down".equals(command)){
        response.setContentType("text/html");
        response.setHeader("Content-Disposition", "attachment; filename=\"" + log);
      } else {
        response.getWriter().print("<html><big>"+log+":</big><br><code><pre>");
      }

      InputStream remoteInputStream = new FileInputStream(pllog);
      BufferedInputStream fr = new BufferedInputStream(remoteInputStream);
      int b;
      while ((b = fr.read()) != -1) response.getWriter().write(b);
      fr.close();

      if (!"down".equals(command)){
        response.getWriter().print("</pre></code></html>");
      }

    }
/*
________________________________________________________________________________________________________________________________________________________________________


nothing to show

________________________________________________________________________________________________________________________________________________________________________

*/
  } else {
%>Log file <%=log%> does not exist at location: <%=pathname%>.<%
  }

%>