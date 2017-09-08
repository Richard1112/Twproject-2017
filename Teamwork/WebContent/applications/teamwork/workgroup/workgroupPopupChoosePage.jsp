<%@ page import="com.opnlb.website.page.WebSitePage,
                 com.opnlb.website.waf.WebSiteConstants,
                 com.twproject.operator.TeamworkOperator,
                 com.twproject.resource.Person,
                 com.twproject.resource.Resource,
                 com.twproject.waf.TeamworkPopUpScreen,
                 org.jblooming.oql.QueryHelper,
                 org.jblooming.persistence.PersistenceHome,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.button.ButtonSupport,
                 org.jblooming.waf.html.container.ButtonBar,
                 org.jblooming.waf.html.input.Collector,
                 org.jblooming.waf.html.input.LoadSaveFilter,
                 org.jblooming.waf.html.input.TextField,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.state.PersistentSearch,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState,
                 java.util.List,
                 java.util.Set,
                 java.util.TreeMap" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);

    /*
      ------------------------------------------------------------------
                         CONTROLLER
      ------------------------------------------------------------------
     */
    if (Commands.FIND.equals(pageState.getCommand())) {
      TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
      boolean recoveredFromSavedFilter = PersistentSearch.feedFromSavedSearch(pageState);

      String hql = "select distinct resource from " + Person.class.getName() + " as resource ";
      QueryHelper qhelp = new QueryHelper(hql);
      String filter = pageState.getEntry("PEOPLE").stringValueNullIfEmpty();
      if (filter != null) {
        qhelp.addQBEORClauses(
            filter,
            qhelp.getOrElement("resource.personName", "personName", QueryHelper.TYPE_CHAR),
            qhelp.getOrElement("resource.personSurname || resource.personName", "surnameName", QueryHelper.TYPE_CHAR),
            qhelp.getOrElement("resource.personName || resource.personSurname", "nameSurname", QueryHelper.TYPE_CHAR),
            qhelp.getOrElement("resource.personSurname", "personSurname", QueryHelper.TYPE_CHAR)
        );


      }

      qhelp.addOQLClause(" resource.myself is not null and resource.myself.enabled = :truth", "truth", Boolean.TRUE);
      qhelp.addOQLClause(" resource.hidden = false");

      TreeMap<String, String> ctm = Collector.chosen(pageState.mainObjectId + "_EC", pageState);
      TreeMap<String, String> candTm = new TreeMap<String, String>();

      List<Resource> cand = (List<Resource>) qhelp.toHql().list();
      if (cand != null && cand.size() == 1) {
        Resource aCand = cand.get(0);
        ctm.put(aCand.getId().toString(), aCand.getDisplayName());

      } else if (cand != null && cand.size() > 1) {
        for (Resource aCand : cand) {
          if (ctm == null || !ctm.keySet().contains(aCand.getId().toString()))
            candTm.put(aCand.getId().toString(), aCand.getDisplayName() +
                (JSP.ex(aCand.getMyself().getOption(WebSiteConstants.HOME_PAGE)) ? "<b> / " + JSP.w(aCand.getMyself().getOption(WebSiteConstants.HOME_PAGE)) + "</b>" : ""));

        }
      }
      Collector.clearClientEntry(pageState.mainObjectId + "_EC", pageState);
      Collector.make(pageState.mainObjectId + "_EC", candTm, ctm, pageState);

    } else if (Collector.isCollectorCommand(pageState.mainObjectId + "_EC", pageState.getCommand())) {
      Collector.move(pageState.mainObjectId + "_EC", pageState);
      // ripulisco
      TreeMap<String, String> ctm = Collector.chosen(pageState.mainObjectId + "_EC", pageState);
      TreeMap<String, String> ctm2 = new TreeMap<String, String>();
      Set<String> ids = ctm.keySet();
      for (String id : ids) {
        if (JSP.ex(ctm.get(id)) && ctm.get(id).indexOf("<b> /") != -1)
          ctm2.put(id, ctm.get(id).substring(0, ctm.get(id).indexOf("<b> /")).trim());
        else
          ctm2.put(id, JSP.w(ctm.get(id)));
      }
      TreeMap<String, String> candTm = Collector.candidates(pageState.mainObjectId + "_EC", pageState);
      TreeMap<String, String> candTm2 = new TreeMap<String, String>();
      Set<String> idsCand = candTm.keySet();
      for (String id : idsCand) {
        if (JSP.ex(candTm.get(id)) && candTm.get(id).indexOf("<b> /") == -1) {
          Person p = (Person) PersistenceHome.findByPrimaryKey(Person.class, id);
          String pageName = p.getMyself().getOption(WebSiteConstants.HOME_PAGE);
          candTm2.put(id, JSP.w(p.getDisplayName()) + (JSP.ex(pageName) ? "<b> / " + pageName + "</b>" : ""));
        } else
          candTm2.put(id, JSP.w(candTm.get(id)));
      }
      Collector.clearClientEntry(pageState.mainObjectId + "_EC", pageState);
      Collector.make(pageState.mainObjectId + "_EC", candTm2, ctm2, pageState);

    } else if (Commands.SAVE.equals(pageState.getCommand())) {
      WebSitePage objPage = (WebSitePage) PersistenceHome.findByPrimaryKey(WebSitePage.class, pageState.mainObjectId);
      String pageName = objPage.getName() + ".page";
      TreeMap<String, String> ctm = Collector.chosen(pageState.mainObjectId + "_EC", pageState);
      Set<String> ids = ctm.keySet();
      for (String id : ids) {
        Resource resource = (Resource) PersistenceHome.findByPrimaryKey(Resource.class, id);
        resource.getMyself().putOption(WebSiteConstants.HOME_PAGE, pageName);
      }
      TreeMap<String, String> candTm = Collector.candidates(pageState.mainObjectId + "_EC", pageState);
      Set<String> idsCand = candTm.keySet();
      for (String id : idsCand) {
        Resource resource = (Resource) PersistenceHome.findByPrimaryKey(Resource.class, id);
        if (pageName.equals(resource.getMyself().getOption(WebSiteConstants.HOME_PAGE)))
          resource.getMyself().getOptions().remove(WebSiteConstants.HOME_PAGE);
      }

    } else if ("POPULATE".equals(pageState.getCommand())) {

      WebSitePage objPage = (WebSitePage) PersistenceHome.findByPrimaryKey(WebSitePage.class, pageState.mainObjectId);
      String pageName = objPage.getName() + ".page";

      String hql = "select distinct resource.myself from " + Person.class.getName() + " as resource ";
      QueryHelper qhelp = new QueryHelper(hql);
      qhelp.addOQLClause(" resource.myself is not null and resource.myself.enabled = :truth", "truth", Boolean.TRUE);
      qhelp.addOQLClause(" resource.hidden = false");
      List<TeamworkOperator> opList = qhelp.toHql().list();
      TreeMap<String, String> ctm = new TreeMap<String, String>();
      TreeMap<String, String> candTm = new TreeMap<String, String>();
      for (TeamworkOperator op : opList) {
        if (pageName.equals(op.getOption(WebSiteConstants.HOME_PAGE)))
          ctm.put(op.getPerson().getId().toString(), op.getPerson().getDisplayName());
        else
          candTm.put(op.getPerson().getId().toString(), op.getPerson().getDisplayName() +
              (JSP.ex(op.getOption(WebSiteConstants.HOME_PAGE)) ? "<b> / " + JSP.w(op.getOption(WebSiteConstants.HOME_PAGE)) + "</b>" : ""));
      }
      Collector.make(pageState.mainObjectId + "_EC", candTm, ctm, pageState);
    }
    // -------------------------------------------------------------------------------------------------

    pageState.perform(request, response).toHtml(pageContext);

  } else {

  //close the black frame
  if (Commands.SAVE.equals(pageState.getCommand()) && pageState.validEntries()) {
        %><script>closeBlackPopup();</script><%      
  }


    PageSeed seed = pageState.thisPage(request);
    seed.setPopup(true);
    seed.setCommand(Commands.FIND);
    seed.mainObjectId = pageState.mainObjectId;

    Form form = new Form(seed);
    pageState.setForm(form);
    form.start(pageContext);

    WebSitePage objPage = (WebSitePage) PersistenceHome.findByPrimaryKey(WebSitePage.class, pageState.mainObjectId);
    pageState.setMainObject(objPage);

    String pageName = pageState.getEntry("PAGE_NAME").stringValueNullIfEmpty();
    TextField.hiddenInstanceToHtml("PAGE_NAME", pageName, pageContext);    // perche ad ogni submit del collctor la perdo

%><h2><%=I18n.get("SELECT_PERSON_FOR_PAGE") + " \"" + pageName + "\""%></h2>
<table class="table">
  <tr>
    <td>
      <table border="0" cellpadding="3" cellspacing="0">
        <tr>
          <td height="30">
            <%
              TextField tf = new TextField(I18n.get("PEOPLE"), "PEOPLE", "</td><td>", 20, false);
              tf.addKeyPressControl(13, "obj('" + pageState.getForm().getUniqueName() + "').submit();", "onkeyup");
              tf.toHtmlI18n(pageContext);
            %>
          </td>
          <td height="30">
            <%

            %></td>
        </tr>
      </table>
      <%

        ButtonBar bb2 = new ButtonBar();

        LoadSaveFilter lsf = new LoadSaveFilter("category", pageState.getForm());
        bb2.addButton(lsf);

        ButtonSubmit search = ButtonSubmit.getSearchInstance(form, pageState);//new ButtonSubmit(form);
        bb2.addButton(search);
        bb2.toHtml(pageContext);

      %>
      <table class="table" border="0" cellpadding="3" cellspacing="0">
        <tr>
          <td><%
            Collector collector = new Collector(pageState.mainObjectId + "_EC", 300, pageState.getForm());
            collector.CANDIDATES_LABEL = I18n.get("WORKGROUP_CANDIDATES") + " / home page";
            collector.CHOSEN_LABEL = I18n.get("WORKGROUP_CHOSEN");
            collector.NO_CANDIDATES = I18n.get("WORKGROUP_NO_CANDIDATES");
            collector.NO_CHOSEN = I18n.get("WORKGROUP_NO_CHOSEN");
            collector.toHtml(pageContext);
          %></td>
        </tr>
      </table>

    </td>
  </tr>
</table>
<%
    ButtonBar bb = new ButtonBar();
    ButtonSupport wp = ButtonSubmit.getSaveInstance(pageState);
    bb.addButton(wp);
    bb.toHtml(pageContext);
    form.end(pageContext);
  }
%>