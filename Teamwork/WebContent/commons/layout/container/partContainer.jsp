<%@page pageEncoding="UTF-8" %><%@ page import="
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.SessionState,
                 org.jblooming.waf.html.container.Container,
                 org.jblooming.waf.html.core.JspIncluder,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.html.layout.HtmlColors,
                 org.jblooming.waf.html.state.ScreenElementStatus, org.jblooming.waf.settings.I18n"%><%

  Container container = (Container) JspIncluderSupport.getCurrentInstance(request);
  SessionState sessionState = SessionState.getCurrentSessionState();

 if (Container.BOX_START.equals(request.getAttribute(Container.ACTION))) { // ------------------------------------------------------ Container START

    StringBuffer containerStyle = new StringBuffer();
    StringBuffer containerBodyStyle = new StringBuffer();

    //set absolute position correctly
    container.absolutePosition= container.absolutePosition || container.draggable || container.centeredOnScreen;

    //recover container status
    ScreenElementStatus screenElementStatus=null;
    if(container.saveStatus)
      screenElementStatus= sessionState.screenElementsStatus.get(container.getId());


    if (screenElementStatus == null) {
      screenElementStatus = new ScreenElementStatus(container.getId());
      screenElementStatus.status = container.status;
      if (container.absolutePosition) {
          screenElementStatus.y = container.top;
          screenElementStatus.x = container.left;

        screenElementStatus.w = container.width;
        screenElementStatus.h = container.height;
      }
    }
    


    if (screenElementStatus.w != null) {
      screenElementStatus.w += (!screenElementStatus.w.endsWith("%") && !screenElementStatus.w.endsWith("px") ? "px" : ""); // add px if no specified
      containerStyle.append("width:" + screenElementStatus.w + "; ");
    }

    if (screenElementStatus.h != null) {
      screenElementStatus.h += (!screenElementStatus.h.endsWith("%") && !screenElementStatus.h.endsWith("px") ? "px" : ""); // add px if no specified
      containerBodyStyle.append("height:" + screenElementStatus.h + "; ");
    }

    //force overflow
    if (screenElementStatus.h != null || container.resizable){
      container.overflow = JSP.ex(container.overflow) ? container.overflow : "auto"; //in case height is specified overflow is "auto" if not differently specified
    }

    if (container.overflow!=null)
      containerBodyStyle.append("overflow:"+container.overflow+";");

    if (container.absolutePosition) {
        containerStyle.append("left:").append(screenElementStatus.x).append("px;");
        containerStyle.append("top:").append(screenElementStatus.y).append("px;");
    }

    if (container.contentAlign != null)
      containerBodyStyle.append("text-align:" + container.contentAlign + ";");

    if (JSP.ex(container.color)) {
      containerStyle.append("background:" + container.color + ";");
      containerBodyStyle.append("color:" + HtmlColors.contrastColor(container.color) + ";");
    }

    String containerClass="";
    containerClass+=container.draggable?"draggable ":"";
    containerClass+=container.collapsable?"collapsable ":"";
    containerClass+=container.closeable?"closeable ":"";
    containerClass+=container.iconizable?"iconizable ":"";
    containerClass+=container.absolutePosition?"absolutePosition ":"";
    containerClass+=container.centeredOnScreen?"centeredOnScreen ":"";
    containerClass+=container.resizable?"resizable ":"";

%>
<div id="<%=container.getContainerId()%>" class="container <%=container.getCssPostfix()%> <%=container.getCssLevel()%> <%=containerClass%>" status="<%=screenElementStatus.status%>" <%=container.saveStatus?"saveStatus":""%> style="<%=containerStyle%>" cmdSuffix="<%=container.commandSuffix%>" <%=JSP.ex(container.containment) ? "containment=\"" + container.containment + "\"" : ""%>>
  <% if (JSP.ex(container.title) || container.closeable|| container.iconizable || container.collapsable ) {
  //containers without title are like old BOX %>
    <div id="<%=container.getContainerTitleId()%>" class="containerTitle <%=JSP.ex(container.title)?"":"noTitle"%> <%=container.getCssPostfix()%> <%=container.getCssLevel()%>">
      <div class="titleIcon" style="padding-right:3px"><%
        if (container.icon != null)
          container.icon.toHtml(pageContext);

        if (container.embeddedInTitle != null) {
          container.embeddedInTitle.toHtml(pageContext);
        }
      %></div>
      <div class="title" <%=container.collapsable?"onclick=\"$(this).closest('.container').trigger('toggleCollapse');\"":""%> style="<%=container.collapsable ? "cursor:pointer" :""%>"><%=JSP.w(container.title) %></div>
    </div>
    <div class="stsButtons" align="right">
      <%
        // add left buttons if any
        if (container.titleRightElements != null && container.titleRightElements.size() > 0) {
          for (JspIncluder button : container.titleRightElements) {
      %><div class="titleRight" style="float:left"><%button.toHtml(pageContext);%></div><%
        }
      }
      //container status buttons
    %>
      <span class="stsButton stsIconize teamworkIcon" onclick="$(this).closest('.container').trigger('iconize');" style="float:left;cursor:pointer;">&egrave;</span>
      <span class="stsButton stsRestore teamworkIcon" onclick="$(this).closest('.container').trigger('restore');" style="float:left;cursor:pointer;">E</span>
      <span class="stsButton stsCollapse teamworkIcon" onclick="$(this).closest('.container').trigger('collapse');" style="float:left;cursor:pointer;">Q</span>
      <span title="<%=I18n.get("CLOSE")%>" class="stsButton stsHide teamworkIcon" onclick="$(this).closest('.container').trigger('hide');" style="float:left;cursor:pointer;">x</span>
    </div>
  <%}%>
  <div id="<%=container.getContainerBodyId()%>" class="containerBody" style="<%=containerBodyStyle%>"><%
  } else if (Container.BOX_END.equals(request.getAttribute(Container.ACTION))) { // ------------------------------------------------------ Container END
  %></div>
</div>

<script type="text/javascript">
  $(function() {
    $("#<%=container.getId()%>").containerBuilder();
  });
</script>
<%

  }
%>
