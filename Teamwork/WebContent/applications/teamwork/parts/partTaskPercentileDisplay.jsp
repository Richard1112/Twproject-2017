<%@ page import="org.jblooming.utilities.JSP"%>
<%@ page import="org.jblooming.waf.html.core.JspIncluderSupport"%>
<%@ page import="org.jblooming.waf.html.display.PercentileDisplay, org.jblooming.waf.html.layout.HtmlColors, org.htmlparser.tags.Html, com.twproject.task.Task"%>
<%
  PercentileDisplay pd = (PercentileDisplay) JspIncluderSupport.getCurrentInstance(request);
  String errMessage= (String) pd.parameters.get("errMessage");
  double errValue= (Double) pd.parameters.get("errValue");

  int w=(int)pd.percentile;
  if (pd.percentile>100)
    //w=(int)((100/pd.percentile)*100);
    w=100;


  int h = Integer.parseInt(pd.height.replaceAll("px", ""));

    String bgColor = pd.percentile > 100 ? pd.percentileOverflowColor : pd.backgroundColor;
    String barColor = JSP.ex(pd.percentileColor) ? pd.percentileColor : HtmlColors.modifyLight(bgColor, -100);%>
<div class="progressBar" style="width:<%=pd.width%>;height:<%=pd.height%>;position: relative" >

        <div data-balloon ="<strong><%=JSP.w(pd.toolTip)%></strong> <%=(errValue>0.1 && "yes".equals(pd.parameters.get("drawCheckup"))) ? "<br>" + "<small>" + JSP.w(errMessage)+ "</small>" : ""%>" data-timer="3000" data-forceposition="left" class="perc" style="width:100%;height:<%=pd.height%>;background-color:<%=bgColor%>;position:relative; display: inline-block; overflow:hidden;">
          <div style="background-color:<%=barColor%>;width:<%=w%>%;height:<%=pd.height%>;"></div>
          <div style="cursor: pointer;width: 100%;position: absolute; top:0;line-height: 100%; background:none; padding: 2px 4px; text-align: left;font-size:<%=h-4%>px;color:<%=HtmlColors.contrastColor(pd.percentile<40?bgColor:barColor)%>"><%=JSP.perc(pd.percentile,pd.fractionDigits)+"%"%></div>
        </div>
<%--
        <% if (errValue>0.1 && "yes".equals(pd.parameters.get("drawCheckup"))) {%>
          <div class="teamworkIcon info" data-balloon ="<%=errMessage%>" data-timer="3000" title="<%=errMessage%>" style="vertical-align: top; cursor:default; position: absolute;right: -18px;top:-2px">|</div>
        <%}%>
--%>
  </div>


