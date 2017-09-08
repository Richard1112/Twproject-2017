<%@ page import="org.jblooming.waf.html.button.ButtonSupport,
                 org.jblooming.waf.html.container.ButtonBar,
                 org.jblooming.waf.html.container.DoubleButton,
                 org.jblooming.waf.html.container.SideBar,
                 org.jblooming.waf.html.core.JspIncluder,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.view.PageState"%><%

  SideBar sideBar = (SideBar) JspIncluderSupport.getCurrentInstance(request);
  PageState pageState = PageState.getCurrentPageState(request);


%>
<div class="sideBar noprint">
     <%
    if (sideBar.buttonList.size() > 0) {
      for (JspIncluder button : sideBar.buttonList) {

        if (button instanceof ButtonSupport) {
          ((ButtonSupport)button).toHtmlInTextOnlyModality(pageContext);

        } else if (button instanceof DoubleButton) {
          DoubleButton db=(DoubleButton) button;

          %><div><%
            if (db.button1 instanceof ButtonSupport)
              ((ButtonSupport)db.button1).toHtmlInTextOnlyModality(pageContext);
            else
              db.button1.toHtml(pageContext);

            %><div style="margin-top:-40px; float:right"><%
          if (db.button2 instanceof ButtonSupport)
            ((ButtonSupport)db.button2).toHtmlInTextOnlyModality(pageContext);
          else
            db.button2.toHtml(pageContext);
          %></div> </div><%
        } else if (button instanceof ButtonBar.Separator) {
          %><span class="separator" style="width:<%=((ButtonBar.Separator)button).width%>px">&nbsp;</span><%
        } else if (button instanceof ButtonBar.Label) {
          %><span><%=((ButtonBar.Label)button).label%>&nbsp;</span><%
        } else
          button.toHtml(pageContext);
      }
    }
    
%> </div>
