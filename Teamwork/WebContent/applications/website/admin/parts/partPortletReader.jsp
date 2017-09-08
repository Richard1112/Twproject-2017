<%@ page import="org.jblooming.utilities.HttpUtilities,
                 org.jblooming.utilities.file.FileUtilities,
                 java.io.File"%><%@ page pageEncoding="UTF-8" %><%

  String jsp = request.getParameter("val");
  String path = HttpUtilities.getFileSystemRootPathForRequest(request) ;
  String testo = "";

  if (jsp != null) {
    if (new File(path + jsp).exists())
      testo = FileUtilities.readTextFile(path + jsp);
  }
%><%=testo%>