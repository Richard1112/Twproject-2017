<%@ page import="org.jblooming.utilities.JSP,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.html.display.Hint,
                 org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState"%><%

  PageState pageState = PageState.getCurrentPageState(request);
  Hint hint = (Hint) JspIncluderSupport.getCurrentInstance(request);
%>
<script type="text/javascript">
  $(function() {

    setTimeout(function(){
      var hintPlace = $("<%=hint.placementSelector%>");
      var hint = hintPlace.hintBaloon("<%=JSP.javascriptEncode(JSP.convertLineFeedToBR(hint.message))%>", "<%=hint.type%>", <%=hint.width%>, <%=hint.height%>,true);

      hintPlace.one("click.hint",function() {
        hintPlace.hideBalloon();
       });

    },1000)

  });
</script>
