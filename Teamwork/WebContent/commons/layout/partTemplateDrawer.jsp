<%@ page import="com.opnlb.website.content.Content,
                 com.opnlb.website.page.WebSitePage,
                 com.opnlb.website.template.Template,
                 com.twproject.operator.TeamworkOperator,
                 net.sf.json.JSONArray,
                 net.sf.json.JSONObject,
                 org.jblooming.oql.OqlQuery,
                 org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.view.PageState, java.util.ArrayList, java.util.List, org.jblooming.waf.settings.I18n, com.opnlb.website.util.TemplateManager" %>
<%@page pageEncoding="UTF-8" %>


<%
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged= (TeamworkOperator) pageState.getLoggedOperator();

  JspHelper templateDrawer = (JspHelper) JspHelper.getCurrentInstance(request);

  Template template = (Template) templateDrawer.parameters.get("template");
  WebSitePage wsp= (WebSitePage) templateDrawer.parameters.get("ws_page");
  String tpFile= "/" + template.getTemplateFile().getFileLocation();

  JSONObject customChoices= TemplateManager.getUserContents(wsp,logged);
  JSONObject defaultChoices=  TemplateManager.getDeafultContents(wsp);

%>
<style>
  .portletPlaceHolder{
    min-height: 200px;
    background-color: #f8f8f8;
  }
</style>

<jsp:include page="<%=tpFile%>"/>
<script>
  var customChoices=<%=customChoices%>;
  var defaultChoices=<%=defaultChoices%>;

  var portletOut={};

  $("[areaname]").each(function(){
    var area=$(this);
    var areaName = area.attr("areaName");
    var portlets=customChoices[areaName];
    if (area.attr("custom")=="yes"){
      if (!portlets || portlets.length==0)
        portlets=defaultChoices[areaName];

    } else {
      var portlets=defaultChoices[areaName];
    }

    for (var i=0;portlets && i<portlets.length ; i++) {
      if (portletOut[portlets[i].id])
        return;

      portletOut[portlets[i].id]=portlets[i].id;
      drawPortlet(portlets[i],areaName,area);
    }

  });

  function drawPortlet(portlet,areaName,area){
    var div=$("<div>").prop("id",areaName+portlet.id).addClass("portletBoxPlaceholder portletBox").html("<h2><%=I18n.get("LOADING")%> "+portlet.name+"...</h2>");
    area.append(div);
    $.get(contextPath+"/"+portlet.file,portlet.parameters,function(ret){
      var c=$(ret).hide();
      //div.replaceWith(c);
      div.html(c).removeClass("portletBoxPlaceholder portletBox").addClass("portletContainer").data("portlet",portlet);
      c.find(".linkEnabled").activateTeamworkLinks(true);
      c.fadeIn(200);
    });
  }


  function refreshPortlet(aRandomElementInThePortlet,addParams){
    //console.debug("refreshPortlet",addParams);
    var portletDiv=aRandomElementInThePortlet.closest(".portletContainer");
    if (portletDiv.data("portlet")){
      var portlet=portletDiv.data("portlet");
      portlet.parameters=portlet.parameters||{};
      if (addParams)
        $.extend(portlet.parameters,addParams);

      //console.debug(portlet.parameters)
      $.get(contextPath+"/"+portlet.file,portlet.parameters,function(ret){
        portletDiv.html($(ret));
        portletDiv.find(".linkEnabled").activateTeamworkLinks(true);
      });
    }
  }

</script>


