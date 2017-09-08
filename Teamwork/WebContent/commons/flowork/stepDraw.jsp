<%@ page import="org.jblooming.designer.Designer,
                 org.jblooming.designer.DesignerField,
                 org.jblooming.waf.html.core.JspIncluder,
                 org.jblooming.waf.view.PageState"%>
<%
  Designer designer = (Designer) Designer.getCurrentInstance(request);

  PageState pageState = PageState.getCurrentPageState(request);
  String stepName = pageState.getEntry("STEP_ID").stringValue();

  if (Designer.DRAW_FORM.equals(request.getAttribute(JspIncluder.ACTION))) {

      int mqbcmd=2;

      %> <table><tr><%
      int i=0;
      for (DesignerField df : designer.designerFields.values()) {
          if (i>=mqbcmd) {
            i=0;
            %></tr> <tr><%
          }
          i++;
          df.usedForSearch = false;
          %><td height="20px"><%df.toHtml(pageContext);%></td><%

      }
      %> </tr> </table> <%



  }
%>