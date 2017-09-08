<%@ page import ="org.jblooming.waf.html.core.JspIncluderSupport,
                   org.jblooming.waf.html.table.ListHeader"%><%

  ListHeader.ListHeaderButton button = (ListHeader.ListHeaderButton) JspIncluderSupport.getCurrentInstance(request);

  if (button.getLabel() != null){
    if (button.drawOrderBy && button.getLaunchedJsOnActionListened() != null) {
      String js =
              " " + button.getActionListened()+
              "= \"if (event.keyCode==" + button.getKeyToHandle() +
              ") {" + button.getLaunchedJsOnActionListened()+
              "}\"";
      %><span <%=button.generateLaunchJs()%> title="<%=(button.getToolTip()!=null?button.getToolTip():"")%>"><a href="#" <%=js%> id="<%=button.getId()%>"><%=button.getLabel()%></a></span><%
    } else {
      %><%=button.getLabel()%><%
    }
  }

%>