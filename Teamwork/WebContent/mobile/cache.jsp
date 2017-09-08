<%@ page contentType="text/cache-manifest" %><%@ page import="org.jblooming.waf.settings.ApplicationState, org.jblooming.utilities.file.FileUtilities, java.io.File"
%>CACHE MANIFEST
<%
  // get all files in /mobile path


  String mobileRoot = ApplicationState.webAppFileSystemRootPath + "/mobile";

  for (File file : FileUtilities.getFilesRecursively(new File(mobileRoot))) {
    // skip svn files etc.
    if (!file.isHidden() && file.getCanonicalPath().indexOf(".svn")<0) {
      if (" .js .css .html .jpg .png  ".indexOf((" " + FileUtilities.getFileExt(file.getName()) + " ").toLowerCase()) >= 0) {
        String nomeGiust = file.getCanonicalPath().substring(mobileRoot.length()+1).replace('\\', '/');
        System.out.println(nomeGiust);
        out.println(nomeGiust);
      }
    }

  }

%>
