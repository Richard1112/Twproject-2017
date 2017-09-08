<%@ page import=" com.twproject.document.TeamworkDocument,
                 com.twproject.waf.html.DocumentDrawer,
                 org.jblooming.waf.view.PageState" %><%


  PageState pageState = PageState.getCurrentPageState(request);

  if ("DRAWDOCLINE".equals(pageState.command)) {
    TeamworkDocument doc= TeamworkDocument.load(pageState.getEntry("docId").intValueNoErrorCodeNoExc()+"");
    if (doc !=null){
      DocumentDrawer dd = new DocumentDrawer(doc.getTask()==null?doc.getResource():doc.getTask());
      dd.drawDocument(doc, pageContext);
    }
  }
%>
