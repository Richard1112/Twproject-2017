<%@ page import="org.jblooming.waf.html.button.ButtonSupport,
                 org.jblooming.waf.html.container.ButtonBar,
                 org.jblooming.waf.html.container.HeadBar,
                 org.jblooming.waf.html.core.JspIncluder,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.view.PageState"%><%

  HeadBar headBar = (HeadBar) JspIncluderSupport.getCurrentInstance(request);
  PageState pageState = PageState.getCurrentPageState(request);


%>
<div class="headBar noprint">
     <%
    if (headBar.buttonList.size() > 0) {
      for (JspIncluder button : headBar.buttonList) {

        if (button instanceof ButtonSupport) {
          ((ButtonSupport)button).toHtmlInTextOnlyModality(pageContext);
        } else if (button instanceof ButtonBar.Separator) {
          %><span class="separator" style="width:<%=((ButtonBar.Separator)button).width%>px">&nbsp;</span><%
        } else if (button instanceof ButtonBar.Label) {
          %><span><%=((ButtonBar.Label)button).label%>&nbsp;</span><%
        } else
          button.toHtml(pageContext);
      }
    }
    
%> </div>
