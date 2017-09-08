<%@ page import="org.jblooming.utilities.JSP,
                 org.jblooming.waf.html.button.ButtonSupport,
                 org.jblooming.waf.html.container.ButtonBar,
                 org.jblooming.waf.html.container.ButtonBox,
                 org.jblooming.waf.html.core.JspIncluder,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.view.PageState"%><%

  ButtonBox buttonBox = (ButtonBox) JspIncluderSupport.getCurrentInstance(request);
  PageState pageState = PageState.getCurrentPageState(request);


%><div class="<%=buttonBox.boxClass%> noprint"><%
if (JSP.ex(buttonBox.title)){
  %><h2><%=buttonBox.title%></h2><%
}

if (buttonBox.buttonList.size() > 0) {
      for (JspIncluder button : buttonBox.buttonList) {

        if (button instanceof ButtonSupport) {
          if (buttonBox.useButtonTextual)
            ((ButtonSupport)button).toHtmlInTextOnlyModality(pageContext);
          else
            ((ButtonSupport)button).toHtml(pageContext);            
        } else if (button instanceof ButtonBar.Separator) {
          %><div class="separator">&nbsp;</div><%
        } else if (button instanceof ButtonBar.Label) {
          %><span ><%=((ButtonBar.Label)button).label%>&nbsp;</span><%
        } else
          button.toHtml(pageContext);
      }
    }
    
%> </div>
