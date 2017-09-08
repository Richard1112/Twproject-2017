<%@ page import="com.twproject.waf.TeamworkHBFScreen, org.jblooming.agenda.CompanyCalendar, org.jblooming.operator.Operator, org.jblooming.oql.OqlQuery, org.jblooming.security.License, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.text.SimpleDateFormat, java.util.Locale, java.util.TimeZone, org.jblooming.waf.settings.I18n" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  boolean wantsToBuy = "WANT_TO_BUY".equals(pageState.command);

  if (!pageState.screenRunning) {

    pageState.screenRunning = true;

    final ScreenArea body = new ScreenArea(request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);

  } else {

    // reset errore generato da mobile, per chi c'è andato per sbaglio
    String filterError = (String) ApplicationState.applicationParameters.get("FILTER_ERROR");
    if (JSP.ex(filterError) && filterError.indexOf("terpri") > 0) {
      ApplicationState.applicationParameters.remove("FILTER_ERROR");
    }

    License license=License.fromFile();

    String hql = "select count(op) from "+ Operator.class.getName()+" as op where op.enabled = true";
    long totOp = (Long)new OqlQuery(hql).uniqueResult();
    long exceededUsers=totOp-license.licenses;
    exceededUsers=exceededUsers>0?exceededUsers:0;

    //warn if close to limit
    SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy", Locale.UK);
    sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    long daysRemaining = (license.expires.getTime() - System.currentTimeMillis()) / CompanyCalendar.MILLIS_IN_DAY;

    PageSeed buy;
    int reqLevel=-1;
    if ("ckLic".equals(pageState.command)){
      if (JSP.ex((String) ApplicationState.getApplicationSettings().get("AMAZON_INSTANCE_ID")))
        buy = new PageSeed("https://shop.twproject.com/shop/thanksRenewAmazon.jsp");
      else
        buy = new PageSeed("https://shop.twproject.com/shop/thanksRenew.jsp");

    } else {
      buy = new PageSeed("https://shop.twproject.com/shop/renew.jsp");

      buy.addClientEntry("twVersion", ApplicationState.getApplicationVersion());  // è anche il testimone per il supporto alla licenza wrappata

      buy.addClientEntry("version", license.version);
      buy.addClientEntry("customer", license.customerCode);
      buy.addClientEntry("isEnt", license.enterprise);
      buy.addClientEntry("isMob", license.mobile);
      buy.addClientEntry("isDemo", license.demo);
      buy.addClientEntry("expMill", license.expires.getTime()); // deprecated
      buy.addClientEntry("expires" , DateUtilities.dateToString(license.expires, "dd/MM/yyyy"));

      buy.addClientEntry("clients", license.licenses);
      buy.addClientEntry("lic", license.licenseKey);
      buy.addClientEntry("level", license.level);
      reqLevel = pageState.getEntry("rqlv").intValueNoErrorCodeNoExc();
      buy.addClientEntry("reqLevel", reqLevel);

      buy.addClientEntry("qta", exceededUsers);
      buy.addClientEntry("type", license.demo ? "DEMO" : (daysRemaining > 3000 ? "NONEXP" : "EXPIRING"));
      buy.addClientEntry("activeUsers", totOp);

      PageSeed retURL = new PageSeed(ApplicationState.serverURL + "/applications/teamwork/buyTwprojectLicense.jsp");
      retURL.command = "ckLic";

      buy.addClientEntry("returnURL", retURL.toLinkToHref());

      //se c'è si passa l'amazon id
      if (JSP.ex((String) ApplicationState.getApplicationSettings().get("AMAZON_INSTANCE_ID"))) {
        buy.addClientEntry("ec2Id", (String) ApplicationState.getApplicationSettings().get("AMAZON_INSTANCE_ID"));
        buy.addClientEntry("type", "AMAZON");
      }

      //se c'è si passa l'amazon instanceType
      if (JSP.ex((String) ApplicationState.getApplicationSettings().get("AMAZON_INSTANCE_TYPE")))
        buy.addClientEntry("ec2Ty", (String) ApplicationState.getApplicationSettings().get("AMAZON_INSTANCE_TYPE"));

      //se c'è si passa l'amazon zone
      if (JSP.ex((String) ApplicationState.getApplicationSettings().get("AMAZON_ZONE")))
        buy.addClientEntry("ec2Zo", (String) ApplicationState.getApplicationSettings().get("AMAZON_ZONE"));

      //se c'è si passa la dimensione del volume
      if (ApplicationState.getApplicationSettings().containsKey("AMAZON_VOLUME"))
        buy.addClientEntry("ec2Vo", ApplicationState.getApplicationSettings().get("AMAZON_VOLUME") + "");


      //se c'è un'azione scelta a mano dall'utente la passiamo
      buy.addClientEntry("manualAction", pageState.getEntry("manualAction"));  //addUsers, changePlan, addTime
    }

%>

<style type="text/css">
  .FFC_WARNING {
    display: none;
  }
</style>
<div id="wp_buyTeamwork" class="mainColumn" style="width: 100%; padding:0">
  <%if (reqLevel>0){%>
  <i><%=I18n.get("UPGRADE_YOUR_PLAN_%%",License.getLevelName(reqLevel))%></i>
  <%} else if (JSP.ex(filterError)){%>
  <i><%=filterError%></i>
  <%}%>

<iframe src="<%=buy.toLinkToHref()%>" style="border:none;width: 100%;height:1600px;"></iframe></td>

</div>
<%
  }
%>