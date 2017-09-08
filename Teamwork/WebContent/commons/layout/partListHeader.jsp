<%@ page import="org.jblooming.waf.html.core.JspIncluder,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.html.input.TextField,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.html.table.ListHeader,
                 java.util.List"%><%

  ListHeader lh = (ListHeader)JspIncluderSupport.getCurrentInstance(request);

  final List headers = lh.getHeaders();
  for (Object o : headers) {
    if (o instanceof ListHeader.Header) {
      ListHeader.Header h= (ListHeader.Header) o;
      %><th class="tableHead" <%=h.align!=null ? "align=\""+h.align+"\"" : ""%>  <%=h.width!=null ? "nowrap width=\""+h.width+"\"" : "nowrap"%>><%h.toHtml(pageContext);%></th><%
    } else if (o instanceof JspIncluder) {
      JspIncluder j= (JspIncluder) o;
      %><th class="tableHead" nowrap ><%j.toHtml(pageContext);%></th><%
    }
  }
  TextField hid = new TextField("hidden","",Form.FLD_FORM_ORDER_BY+lh.id,"",0,false);
  //emulating form' generation of hiddenfields
  hid.preserveOldValue=false;
  hid.id = lh.form.getUniqueName()+Form.FLD_FORM_ORDER_BY+lh.id;

  %><%hid.toHtml(pageContext);%>