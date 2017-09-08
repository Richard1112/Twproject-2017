<%@ page import="org.jblooming.utilities.HttpUtilities, org.jblooming.waf.settings.ApplicationState, org.jblooming.utilities.file.FileUtilities" %><%

  // force non dev mode
  boolean oldMode=ApplicationState.platformConfiguration.development;

  ApplicationState.platformConfiguration.development=false;

  String index= HttpUtilities.getPageContent(ApplicationState.serverURL+"/mobile/mobile.jsp");
  FileUtilities.writeToFile(ApplicationState.webAppFileSystemRootPath+"/mobile/Licorize.html",index);

  ApplicationState.platformConfiguration.development=oldMode;
%>Client generated: navigate here "file:///<%=ApplicationState.webAppFileSystemRootPath.replace('\\','/')+"/mobile/Licorize.html"%>"