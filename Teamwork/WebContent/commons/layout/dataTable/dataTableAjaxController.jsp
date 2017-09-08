<%@ page import="org.jblooming.tracer.Tracer, org.jblooming.utilities.JSP, org.jblooming.waf.html.display.DataTable, org.jblooming.waf.view.PageState" %><%
  PageState pageState=PageState.getCurrentPageState(request);

  String dataTableId=pageState.getEntry("DATA_TBL_ID").stringValueNullIfEmpty();
  if (JSP.ex(dataTableId)){
    DataTable dataTable=(DataTable)pageState.sessionState.getAttribute(dataTableId);
    if (dataTable!=null){
      //si chiama il controller
      dataTable.controllerClass.newInstance().perform(request, response);

      // e si disegnano le righe
      dataTable.drawTableRows(pageContext);

    } else {
      Tracer.platformLogger.warn("Trying to refresh a dataTable, but ID \""+dataTableId+"\" not found in session");
      // se non c'è è probabilmente scaduta la sessione, si prova a ricaricare la pagina
      %><script>window.location.reload(true)</script><%
    }


  } else {
    Tracer.platformLogger.error("Trying to refresh a dataTable without the ID");
  }

%>