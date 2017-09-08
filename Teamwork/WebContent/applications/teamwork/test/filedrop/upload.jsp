<%@ page import="net.sf.json.JSONObject,
                 org.jblooming.utilities.file.FileUtilities,
                 org.jblooming.waf.JSONHelper,
                 org.jblooming.waf.html.input.Uploader,
                 org.jblooming.waf.html.input.Uploader.UploadHelper,
                 org.jblooming.waf.view.PageState,
                 java.io.File,
                 java.io.FileInputStream, java.io.FileOutputStream" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);


  JSONHelper jsonHelper = new JSONHelper();
  JSONObject json = jsonHelper.json;


  try {

    UploadHelper uh = Uploader.getHelper("file", pageState);
    System.out.println(uh.temporaryFile.getCanonicalPath() + " " + uh.originalFileName);


    File temporaryFile = uh.temporaryFile;
    if (temporaryFile != null && temporaryFile.exists()) {
      FileInputStream fis = new FileInputStream(temporaryFile);
      FileOutputStream fos = new FileOutputStream(temporaryFile.getParentFile().getCanonicalPath() + File.separator + uh.originalFileName);

      FileUtilities.copy(fis, fos);
      fis.close();
      fos.close();

    }


  } catch (Throwable t) {
    jsonHelper.error(t);
  }

  jsonHelper.close(pageContext);


%>