<%@ page import="org.jblooming.utilities.JSP"%>
<%@ page import="org.jblooming.waf.html.core.JspIncluderSupport"%>
<%@ page import="org.jblooming.waf.html.display.PercentileDisplay, org.jblooming.waf.html.layout.HtmlColors"%><%
  PercentileDisplay pd = (PercentileDisplay) JspIncluderSupport.getCurrentInstance(request);

  int w=(int)pd.percentile;
  if (pd.percentile>100)
    w=(int)((100/pd.percentile)*100);

  int h = Integer.parseInt(pd.height.replaceAll("px", ""));

    String bgColor = pd.percentile > 100 ? pd.percentileOverflowColor : pd.backgroundColor;
    String barColor = JSP.ex(pd.percentileColor) ? pd.percentileColor : HtmlColors.modifyLight(bgColor, -60);%>
<div style="width:<%=pd.width%>;height:<%=pd.height%>;position: relative" title="<%=JSP.w(pd.toolTip)%>" >
        <div class="perc" style="width:100%;height:<%=pd.height%>;background-color:<%=bgColor%>;position:relative; border-radius:4px;display: inline-block; overflow:hidden;">
          <div style="background-color:<%=barColor%>;width:<%=w%>%;height:<%=pd.height%>;"></div>
          <div style="width: 100%;position: absolute; top:0;line-height: 100%; background:none; padding: 2px; text-align: center;font-size:<%=h-4%>px;color:<%=HtmlColors.contrastColor(pd.percentile<40?bgColor:barColor)%>"><%=JSP.perc(pd.percentile,pd.fractionDigits)+"%"%></div>
        </div>
  <div class="teamworkIcon" title="<%=JSP.w(pd.toolTip)%>" style=" vertical-align: top; cursor:default; position: absolute;right: -18px;top:0">!</div>
  </div>


