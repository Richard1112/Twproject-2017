<%@ page import="com.opnlb.website.template.Template,
                org.jblooming.waf.settings.ApplicationState,
                org.jblooming.waf.view.PageState"%><%@ page pageEncoding="UTF-8" %><%

  PageState pageState = PageState.getCurrentPageState(request);
  String tid = pageState.getEntry("tid").stringValueNullIfEmpty()+"";
  Template t=Template.load(tid);
  String path = "/" + t.getTemplateFile().getFileLocation();

%><jsp:include page="<%=path%>"/>
