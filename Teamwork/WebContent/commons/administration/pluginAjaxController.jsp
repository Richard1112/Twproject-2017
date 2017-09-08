<%@ page import="net.sf.json.JSONObject, org.jblooming.operator.Operator, org.jblooming.utilities.file.FileUtilities, org.jblooming.waf.JSONHelper, org.jblooming.waf.html.input.Uploader, org.jblooming.waf.html.input.Uploader.UploadHelper, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.view.PageState, java.io.File, java.io.FileInputStream, java.io.FileOutputStream" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  Operator logged = pageState.getLoggedOperator();
  JSONHelper jsonHelper = new JSONHelper();
  JSONObject json = jsonHelper.json;
  try {

    if ("LDPLG".equals(pageState.command)) {
      if (logged.hasPermissionAsAdmin()) {
        UploadHelper uh = Uploader.getHelper("file", pageState);
        File temporaryFile = uh.temporaryFile;
        if (temporaryFile != null && temporaryFile.exists()) {
          FileInputStream fis = new FileInputStream(temporaryFile);
          FileOutputStream fos = new FileOutputStream(ApplicationState.webAppFileSystemRootPath + File.separator + "applications" + File.separator + "teamwork" + File.separator + "plugins" + File.separator + uh.originalFileName);

          FileUtilities.copy(fis, fos);
        }
      }

    }
  } catch (Throwable t) {
    jsonHelper.error(t);
  }
  jsonHelper.close(pageContext);
%>