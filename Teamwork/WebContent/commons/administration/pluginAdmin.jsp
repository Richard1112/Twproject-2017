<%@ page buffer="16kb" %>
<%@ page import="com.twproject.waf.settings.ReportBricks,
                 net.sf.jasperreports.engine.JRParameter,
                 net.sf.jasperreports.engine.JasperReport,
                 net.sf.jasperreports.engine.util.JRLoader,
                 org.jblooming.operator.Operator,
                 org.jblooming.system.SystemConstants,
                 org.jblooming.utilities.JSP, org.jblooming.waf.PagePlugin, org.jblooming.waf.PageQuark, org.jblooming.waf.PluginBricks, org.jblooming.waf.ScreenBasic, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.input.Uploader, org.jblooming.waf.settings.Application, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.Map, java.util.Set" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    ScreenBasic screenBasic = ScreenBasic.preparePage(pageContext);
    screenBasic.getBody().areaHtmlClass="lreq30 lreqPage";
    pageState.perform(request, response).toHtml(pageContext);
    pageState.getLoggedOperator().testIsAdministrator();

  } else {
  %><script>$("#ADMINISTRATION_ROOT_MENU").addClass('selected');</script><%
    Container network = new Container();
    network.title = "Plugin admin";
    network.start(pageContext);

    if ("RELOAD".equals(pageState.command)) {
      ApplicationState.applicationParameters.remove(SystemConstants.PAGE_PLUGINS);
      for (Application a : ApplicationState.platformConfiguration.applications.values()) {
        a.configureNeedingPageContext(pageContext);
      }
    }

    Operator logged = pageState.getLoggedOperator();
    logged.testIsAdministrator();
    Map<String, Set<PageQuark>> pqs = PluginBricks.getPageQuarks();


    %><table class="table"><tr><th class="tableHead">name i18n</th><th class="tableHead">name</th><th class="tableHead">class</th><th class="tableHead">file</th></tr><%

    for (String groupName : pqs.keySet()) {

       %><tr><th colspan="99" class="tableHead secondRow"><%=groupName%></th></tr><%

       Set<PageQuark> pqqs = pqs.get(groupName);
       for (PageQuark pqq : pqqs) {
       
        %><tr class="alternate" >
        <td><%=pageState.getI18n(JSP.w(pqq.getName()))%></td>
        <td><%=JSP.w(pqq.getName())%></td>
        <td><%if (pqq instanceof PagePlugin) {
          PagePlugin pp = (PagePlugin)pqq;
          if (pp.willBeActiveWhenMainObjectIsOfClass!=null) {
            %><%=JSP.w(pp.willBeActiveWhenMainObjectIsOfClass.getSimpleName())%><%
          } else {
            %><i>delegated to impl</i><%
          }
        } else {
            %>global<%
          }%></td>
        <td><%=JSP.w(pqq.getFile().getFileLocation())%></td>
        </tr><%
      }
    }




%></table>

<div id="holder" class="uploadizeDrop"><i>Drop plugins here</i></div>

<%

  if (ReportBricks.getAllReports().size()>0){
    %><h2>Additional (i)Reports</h2>
<table class="table edged">

  <tr><th class="tableHead">type</th><th class="tableHead">name i18n</th><th class="tableHead">name</th><th class="tableHead">permission required</th><th class="tableHead">file / parameters</th></tr><%
    for (ReportBricks.Report r:ReportBricks.getAllReports()){
      try {
      JasperReport jasperReport = (JasperReport) JRLoader.loadObjectFromFile(r.file.getPath());
      JRParameter[] params = jasperReport.getParameters();
      %><tr>
          <td><%=r.type%></td>
          <td><%=r.getBestName()%></td>
          <td><%=r.name%></td>
          <td><%=r.requiredPermission%></td>
          <td><%=r.file.getCanonicalPath().substring(ApplicationState.webAppFileSystemRootPath.length())%><br><b style="font-size: 10px;"><%
            for (JRParameter p:params){
              if (p.isSystemDefined())
                continue;
                %><%=p.getName()%>&nbsp;<%
            }
          %></b></td></tr><%
      } catch (Throwable t){
        %><tr>
          <td><%=r.type%></td>
          <td><%=r.getBestName()%></td>
          <td><%=r.name%></td>
          <td><%=r.requiredPermission%></td>
          <td><%=r.file.getCanonicalPath().substring(ApplicationState.webAppFileSystemRootPath.length())%><br><b style="font-size: 10px; color: red;">Error on report: <%=t.getMessage()%></b></td></tr><%
      }
    }

    %></table><%
  }


  PageSeed ps = pageState.thisPage(request);
  ps.command = "RELOAD";
%>

<script>
  $(function(){
    $("#holder").uploadize({
      url:"pluginAjaxController.jsp",
      maxSize:<%=Uploader.getMaxUploadSizeInByte()%>,
      additionalRequestParameters:{CM:"LDPLG"},
      onLoadCallback:function(response){
        if (response.ok){
          window.location.href="<%=ps.toLinkToHref()%>";
        }
      }
      //fileAreaSelector:"#documentList"
    });
  });
</script>


<%

    ButtonBar bb = new ButtonBar();
    ButtonLink link = new ButtonLink("reload plugins", ps);
    //link.confirmRequire=true;
    bb.addButton(link);
    bb.toHtml(pageContext);

    network.end(pageContext);
  }
%>