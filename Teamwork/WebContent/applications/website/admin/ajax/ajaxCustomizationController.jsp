<%@ page import="com.opnlb.website.content.Content, com.opnlb.website.page.WebSitePage, com.opnlb.website.portlet.Portlet, net.sf.json.JSONObject, org.jblooming.PlatformRuntimeException, org.jblooming.operator.Operator, org.jblooming.oql.OqlQuery, org.jblooming.tracer.Tracer, org.jblooming.utilities.JSP, org.jblooming.utilities.StringUtilities, org.jblooming.waf.JSONHelper, org.jblooming.waf.view.ClientEntry, org.jblooming.waf.view.PageState, java.util.ArrayList, java.util.Hashtable, java.util.List, java.util.Map, com.opnlb.website.util.TemplateManager" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  Operator logged = pageState.getLoggedOperator();


  // ---------------------------------------------------- HTML PART ---------------------------------------

  if ("LOPO".equals(pageState.command)) {

    String id = pageState.getEntry("PID").stringValueNullIfEmpty() + "";
    Portlet portlet = Portlet.load(id);
    if (portlet != null) {
      portlet.toHtml(pageContext);
    } else {
%>Error loading portlet id:<%=id%><%
    }



    // ---------------------------------------------------- JSON PART ---------------------------------------

  } else {
    JSONHelper jsonHelper = new JSONHelper();
    JSONObject json = jsonHelper.json;
    try {

      if ("RESETUSRDEF".equals(pageState.command)) {
        pageState.initializeEntries("row");
        WebSitePage wsPage = WebSitePage.load(pageState.getEntry("pageId").stringValueNullIfEmpty() + "");

        if (wsPage == null || logged == null)
          throw new PlatformRuntimeException("Page not found");

        //first remove all content for page X user
        String hql = "delete from " + Content.class.getName() + " where operator=:ope and page=:pag";
        OqlQuery oql = new OqlQuery(hql);
        oql.getQuery().setEntity("ope", logged);
        oql.getQuery().setEntity("pag", wsPage);
        oql.getQuery().executeUpdate();


        JSONObject defaultChoices=  TemplateManager.getDeafultContents(wsPage);

        json.element("mapping", defaultChoices);


      } else if ("SAVEDEFAULT".equals(pageState.command)) {
        pageState.initializeEntries("row");

        logged.testIsAdministrator();

        WebSitePage wsPage = WebSitePage.load(pageState.getEntry("pageId").stringValueNullIfEmpty() + "");

        if (wsPage == null || logged == null)
          throw new PlatformRuntimeException("Page not found");

        //first remove all content for page X user
        String hql = "delete from " + Content.class.getName() + " where defaultConfiguration=true and page=:pag";
        OqlQuery oql = new OqlQuery(hql);
        oql.getQuery().setEntity("pag", wsPage);
        int quant = oql.getQuery().executeUpdate();

        Map<String, ClientEntry> entryMap = pageState.getClientEntries().getEntriesStartingWithStripped("AREA_");
        for (String areaName : entryMap.keySet()) {

          String pids_ser = entryMap.get(areaName).stringValueNullIfEmpty();
          if (JSP.ex(pids_ser)) {
            List<String> pids = StringUtilities.splitToList(pids_ser, "_");
            int i = 0;
            for (String pid : pids) {
              Portlet p = Portlet.load(pid);
              if (p!=null){
                Content c = new Content();
                c.setPage(wsPage);
                c.setArea(areaName);
                c.setOrder(i);
                c.setDefaultConfiguration(true);
                c.setPortlet(p);
                c.store();
                i++;
              } else {
                Tracer.platformLogger.error("Page configuration: missing portlet id: "+pid);
              }
            }
          }
        }


      } else if ("SAVEFORUSER".equals(pageState.command)) {
        pageState.initializeEntries("row");
        WebSitePage wsPage = WebSitePage.load(pageState.getEntry("pageId").stringValueNullIfEmpty() + "");

        if (wsPage == null || logged == null)
          throw new PlatformRuntimeException("Page not found");

        //first remove all content for page X user
        String hql = "delete from " + Content.class.getName() + " where operator=:ope and page=:pag";
        OqlQuery oql = new OqlQuery(hql);
        oql.getQuery().setEntity("ope", logged);
        oql.getQuery().setEntity("pag", wsPage);
        int quant = oql.getQuery().executeUpdate();

        Map<String, ClientEntry> entryMap = pageState.getClientEntries().getEntriesStartingWithStripped("AREA_");
        for (String areaName : entryMap.keySet()) {

          String pids_ser = entryMap.get(areaName).stringValueNullIfEmpty();
          if (JSP.ex(pids_ser)) {
            List<String> pids = StringUtilities.splitToList(pids_ser, "_");
            int i = 0;
            for (String pid : pids) {

              Portlet p = Portlet.load(pid);
              if (p!=null){
                Content c = new Content();
                c.setPage(wsPage);
                c.setOperator(logged);
                c.setArea(areaName);
                c.setOrder(i);
                c.setDefaultConfiguration(false);
                c.setPortlet(p);
                c.store();
              }
              i++;
            }
          }

        }
      }

    } catch (Throwable t) {
      jsonHelper.error(t);
    }
    jsonHelper.close(pageContext);

  }
%>