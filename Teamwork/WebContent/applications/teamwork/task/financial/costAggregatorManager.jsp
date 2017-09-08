<%@ page import="com.twproject.operator.TeamworkOperator,
                 com.twproject.resource.Person,
                 com.twproject.security.TeamworkPermissions,
                 com.twproject.task.financial.CostAggregator,
                 com.twproject.waf.TeamworkHBFScreen,
                 org.jblooming.oql.QueryHelper,
                 org.jblooming.persistence.objectEditor.FieldFeature,
                 org.jblooming.persistence.objectEditor.ObjectEditor, org.jblooming.persistence.objectEditor.businessLogic.ObjectEditorController, org.jblooming.security.Area, org.jblooming.utilities.CodeValueList, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.SecurityConstants, org.jblooming.waf.html.input.SmartCombo, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, java.util.ArrayList, java.util.List"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  pageState.setPopup(true);

  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
  logged.testPermission(TeamworkPermissions.classificationTree_canManage);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    ObjectEditor objectEditor = new ObjectEditor(I18n.get("COST_CENTER"), CostAggregator.class, pageContext);

    List<Area> areas = new ArrayList(logged.getAreasForPermission(TeamworkPermissions.classificationTree_canManage));
    if (areas.size() == 0)
      throw new org.jblooming.security.SecurityException(SecurityConstants.I18N_PERMISSION_LACKING, TeamworkPermissions.classificationTree_canManage);


    // list part
    {
      objectEditor.queryHelper = new QueryHelper("from " + CostAggregator.class.getName() + " as cc ");
      objectEditor.queryHelper.addOQLClause("cc.area in (:areas)");
      objectEditor.queryHelper.addParameter("areas", areas);
      objectEditor.defaultOrderBy=" cc.code";
      
      objectEditor.mainHqlAlias = "cc";
      objectEditor.addDisplayField("code", I18n.get("CODE"));
      objectEditor.addDisplayField("description", I18n.get("DESCRIPTION"));

      FieldFeature area = FieldFeature.getComboInstance(
              "area",
              I18n.get("AREA"),
              CodeValueList.getI18nInstanceForIdentifiables(areas, pageState));
      objectEditor.addDisplayField(area);

      if (!logged.hasPermissionAsAdmin() && pageState.getEntry("area").stringValueNullIfEmpty() == null) {
        pageState.addClientEntry("area", areas.get(0).getId() + "");
      }

      objectEditor.addDisplayField("type", I18n.get("TYPE"));

      FieldFeature ffoEd = FieldFeature.getIdentifiableInstance(
              "manager",
              I18n.get("MANAGER"),
              Person.class,
              new String[]{"personName", "personSurname"});

      String hql = " select p.id, p.personName ||' '|| p.personSurname from " + Person.class.getName() + " as p";
      QueryHelper queryHelperForFiltering = new QueryHelper(hql);
      String baseFilter = " (p.personName ||' '|| p.personSurname like :" + SmartCombo.FILTER_PARAM_NAME + " or p.personSurname ||' '|| p.personName like :" + SmartCombo.FILTER_PARAM_NAME + ")" +
              " and p.area in (:areas) ";
      queryHelperForFiltering.addOQLClause(baseFilter);
      queryHelperForFiltering.addToHqlString(" order by p.code ");

      queryHelperForFiltering.addParameter("areas", areas);
      String whereForId = " where p.id = :" + SmartCombo.FILTER_PARAM_NAME;

      SmartCombo lookup = new SmartCombo("manager", hql, null, whereForId);
      lookup.searchAll = true;
      lookup.queryHelperForFiltering = queryHelperForFiltering;
      ffoEd.smartCombo = lookup;
      objectEditor.addDisplayField(ffoEd);
    }

    //edit part
    {
      FieldFeature codeEd = new FieldFeature("code", I18n.get("CODE"));
      codeEd.required = true;
      objectEditor.addEditField(codeEd);
      objectEditor.addEditField("description", I18n.get("DESCRIPTION"));
      objectEditor.addEditField("type", I18n.get("TYPE"));

      FieldFeature area = FieldFeature.getComboInstance(
              "area",
              I18n.get("AREA"),
              CodeValueList.getI18nInstanceForIdentifiables(areas, pageState));
      area.required = true;
      objectEditor.addEditField(area);

      FieldFeature ffoEd = FieldFeature.getIdentifiableInstance(
              "manager",
              I18n.get("MANAGER"),
              Person.class,
              new String[]{"personName", "personSurname"});

      String hql = " select p.id, p.personName ||' '|| p.personSurname from " + Person.class.getName() + " as p";
      QueryHelper queryHelperForFiltering = new QueryHelper(hql);
      String baseFilter = " (p.personName ||' '|| p.personSurname like :" + SmartCombo.FILTER_PARAM_NAME + " or p.personSurname ||' '|| p.personName like :" + SmartCombo.FILTER_PARAM_NAME + ")" +
              " and p.area in (:areas) ";
      queryHelperForFiltering.addOQLClause(baseFilter);
      queryHelperForFiltering.addToHqlString(" order by p.code ");

      queryHelperForFiltering.addParameter("areas", areas);
      String whereForId = " where p.id = :" + SmartCombo.FILTER_PARAM_NAME;

      SmartCombo lookup = new SmartCombo("manager", hql, null, whereForId);
      lookup.searchAll = true;
      lookup.queryHelperForFiltering = queryHelperForFiltering;
      ffoEd.smartCombo = lookup;
      objectEditor.addEditField(ffoEd);

      if (!logged.hasPermissionAsAdmin() && pageState.getEntry("area").stringValueNullIfEmpty() == null) {
        pageState.addClientEntry("area", areas.get(0).getId() + "");
      }

    }

    ScreenArea body = null;
    ObjectEditorController oec = new ObjectEditorController(objectEditor);
    body = new ScreenArea(oec, request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);

  } else {

%><script>$("#TASK_MENU").addClass('selected');</script><%
    pageState.getMainJspIncluder().toHtml(pageContext);

  }
%>