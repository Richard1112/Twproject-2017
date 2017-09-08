<%@ page import ="org.jblooming.ontology.PersistentFile,
                  org.jblooming.tracer.Tracer,
                  org.jblooming.utilities.HttpUtilities,
                  org.jblooming.utilities.JSP,
                  org.jblooming.utilities.file.FileUtilities,
                  org.jblooming.waf.SessionState,
                  org.jblooming.waf.html.core.JspIncluderSupport,
                  org.jblooming.waf.html.display.Img,
                  org.jblooming.waf.html.display.MultimediaFile,
                  org.jblooming.waf.settings.ApplicationState,
                  org.jblooming.waf.view.PageSeed,
                  org.jblooming.waf.view.PageState,
                  java.io.File"%><%@page pageEncoding="UTF-8"%><%

  PageState pageState = PageState.getCurrentPageState(request);
  SessionState sessionState = pageState.sessionState;
  MultimediaFile mmFile = (MultimediaFile) JspIncluderSupport.getCurrentInstance(request);

  PersistentFile psFile = mmFile.pf;
  String type = psFile.getType();

  if (psFile != null) {
      String fileExt = FileUtilities.getFileExt(psFile.getOriginalFileName());
      boolean isImage = FileUtilities.isImageByFileExt( fileExt );
      boolean isApplet = FileUtilities.isAppletByFileExt(fileExt);
      boolean isJsp = FileUtilities.isJspByFileExt(fileExt);
      boolean isFlash = FileUtilities.isFlashByFileExt( fileExt );
      boolean isQuick = FileUtilities.isQuickByFileExt( fileExt );
      boolean isMsMovie = FileUtilities.isWindowsMovieByFileExt(fileExt);
      boolean isVideo = isFlash || isQuick || isMsMovie;
      String width = mmFile.width;
      String height = mmFile.height;
      if(!JSP.ex(width) && isVideo)
        width = "480";
      if(!JSP.ex(height) && isVideo)
        height = "280";


      // -------------------------------------- DOC or ZIP --------------------------------------------------
      if (FileUtilities.isDocByFileExt( fileExt ) || FileUtilities.isMsApplicationExtension(fileExt) || FileUtilities.isArchiveByFileExt(fileExt)) {
        PageSeed myself = psFile.getPageSeed(true);
        String label =psFile.getOriginalFileName();
        if (mmFile.label!=null)
          label = mmFile.label;
        %><span><a href="<%=myself.toLinkToHref()%>" <%=mmFile.generateToolTip()%>><%=label%></a></span><%

      // -------------------------------------- JSP (portlet) --------------------------------------
      } else if (isJsp) {
        File jspFile = new File(HttpUtilities.getFileSystemRootPathForRequest(request)+ File.separator+psFile.getFileLocation());
        String path = "/" + psFile.getFileLocation();
        if (jspFile.exists()) {
          // DEVELOPMENT :: we like crashes!....
          if (ApplicationState.platformConfiguration.development) {
            %><jsp:include page="<%=path%>"><jsp:param name="AREANAME" value="<%=mmFile.areaName%>"/></jsp:include><%

          // ... but users do not! PRODUCTION
          } else {
            try {
              %><jsp:include page="<%=path%>"><jsp:param name="AREANAME" value="<%=mmFile.areaName%>"/></jsp:include><%
            } catch (Throwable a) {
              %><%="<small>" + pageState.getI18n("NO_PREVIEW_AVAILABLE") + " ::<br>" + path + "" + a + "</small><br>"%><%
              Tracer.platformLogger.error("WEBSITE_ERROR displaying :"+path, a);
            }
          }
        }

      //--------------------------------------  IMAGE --------------------------------------
      } else if (isImage) {
        Img image = new Img(psFile, "");
        if(JSP.ex(mmFile.id))
          image.id = mmFile.id;
        image.width = mmFile.width;
        image.height = mmFile.height;
        image.style = mmFile.style;
        image.toolTip = JSP.w(mmFile.toolTip);
        image.script = mmFile.script;
        image.toHtml(pageContext);


      //-------------------------------------- EVERYTHING ELSE --------------------------------------
      } else {
        Tracer.platformLogger.error("PartMultimediaFile. Unsupported persistent file type: "+psFile.serialize());
        %>Unsupported type<%

      }


  } else {
    %><%=pageState.getI18n("FILE_NULL")%><%
  }

%>
