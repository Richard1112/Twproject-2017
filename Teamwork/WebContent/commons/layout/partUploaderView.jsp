<%@ page import="org.jblooming.tracer.Tracer,
                 org.jblooming.waf.constants.Fields,
                 org.jblooming.waf.html.input.Uploader,
                 org.jblooming.waf.view.PageState" %><%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8"%><%

  PageState pageState = PageState.getCurrentPageState(request);
  boolean treatAsAttachment = pageState.getEntry("TREATASATTACH").checkFieldValue();
  try {
      Uploader.displayFile(treatAsAttachment, pageState, response);
  } catch (Throwable a) {
    Tracer.platformLogger.error("Error viewing uploaded file: "+pageState.getEntry(Fields.FILE_TO_UPLOAD).stringValueNullIfEmpty(),a);
  }

%>