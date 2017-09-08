<%@ page import="com.twproject.task.Task, org.jblooming.utilities.JSP, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, java.util.Map, org.jblooming.ontology.Pair, org.jblooming.waf.html.layout.HtmlColors" %><%
  JspHelper taskCheckup =  (JspHelper)JspHelper.getCurrentInstance(request);

  Task task=(Task)taskCheckup.parameters.get("task");
  Pair<String,Double> checkup = task.checkup("\n");
  double max=checkup.second;
  String color= HtmlColors.getGreenToRed(max);
  String message=checkup.first;
  if (checkup.second<=0)
    message=I18n.get("CK_ALL_OK");

  if ("VU".equals(taskCheckup.parameters.get("mode"))) {
    int w=taskCheckup.parameters.containsKey("width")?(Integer)taskCheckup.parameters.get("width"):60;
    int h=taskCheckup.parameters.containsKey("height")?(Integer)taskCheckup.parameters.get("height"):30;
    %>
      <div class="vumeter" style="background: url(<%=request.getContextPath()%>/img/vu_meter_bg.svg) no-repeat;background-position:center center;background-size: contain; height: <%=h%>px; width:<%=w%>px; position: relative" title="<%=message%> (<%=JSP.perc(max)%>)">
      <img src="<%=request.getContextPath()%>/img/vu_meter_lancet.svg" style="height:90%;position: relative; margin: auto; display: block;transform-origin:50% 90%; transform: rotate(<%=max*180-90%>deg)"/>
      </div>
    <%

  } else if ("ROUND".equals(taskCheckup.parameters.get("mode"))){
    int size=taskCheckup.parameters.containsKey("size")?(Integer)taskCheckup.parameters.get("size"):20;
    %><div style="width:<%=size%>px; height:<%=size%>px; background-color:<%=color%>; -moz-border-radius:<%=size%>px; -webkit-border-radius:<%=size%>px; -o-border-radius:<%=size%>px; border-radius:<%=size%>px;" class="checkupElement" title="<%=message%> (<%=JSP.perc(max)%>)" ></div><%

  } else {
    double boundedMax =max<0.05?0.05:(max>0.95?0.95:max);

    int w=taskCheckup.parameters.containsKey("width")?(Integer)taskCheckup.parameters.get("width"):100;
    int h=taskCheckup.parameters.containsKey("height")?(Integer)taskCheckup.parameters.get("height"):15;


      double level = h - (h - (h * boundedMax) - 6);%><div class="thermoWrap"><div class="thermo vert" style="width:<%=w%>px;border:1px solid #fff; height:<%=h%>px; display:inline-block; position:relative; background-color:rgba(152,164,164,0.41); opacity:1;border-radius:0px; " title="<%=message%> (<%=JSP.perc(max)%>)">
       <div style="left:0;bottom:0;padding:0;width:100%;height:<%=level > 90 ? 90 : level%>px; position:absolute;  overflow: hidden; border-top: 3px solid #000">
        <img src="<%=ApplicationState.contextPath%>/img/checkupBarVert.jpg" style="bottom:0;left:0;width:100%;height:<%=h%>px;position:absolute; opacity:1;border-radius:0px;">
       </div>
    <%
          for (int i =1;i<10;i++){
            %><div class="tick" style="height:1px;width:<%=w%>px;position:absolute;left:0;top:<%=i*h/10%>px;background-color:rgba(255,255,255,0.3);"></div><%
          }
        %>
    </div></div><%

  }


%>
