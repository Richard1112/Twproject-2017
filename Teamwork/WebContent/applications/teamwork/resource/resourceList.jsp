<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.rank.EntityGroupRank, com.twproject.rank.RankUtilities, com.twproject.resource.Company, com.twproject.resource.Person,
                 com.twproject.resource.Resource, com.twproject.resource.ResourceBricks, com.twproject.resource.businessLogic.ResourceController, com.twproject.security.TeamworkPermissions, com.twproject.waf.TeamworkHBFScreen,
                 com.twproject.waf.settings.ReportBricks, net.sf.json.JSONObject, org.jblooming.company.DepartmentType, org.jblooming.ontology.Identifiable, org.jblooming.security.Area, org.jblooming.utilities.CodeValueList, org.jblooming.utilities.JSP, org.jblooming.waf.Bricks, org.jblooming.waf.EntityViewerBricks, org.jblooming.waf.PluginBricks, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.*, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.container.DivOnMouseover, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.DataTable,
                 org.jblooming.waf.html.input.*, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.*" %>
<%@ page import="org.jblooming.waf.html.container.ButtonsOnMouseover, org.jblooming.designer.DesignerField, com.twproject.task.Issue, com.twproject.worklog.Worklog, com.twproject.task.Assignment, com.twproject.task.Task" %>
<%


  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new ResourceController(), request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);

  } else {

    PageSeed self = pageState.thisPage(request);
    self.setCommand(Commands.FIND);
    Form f = new Form(self);
    f.addKeyPressControl(13, "dataTableRefresh('RESLST');", "onkeyup");

    pageState.setForm(f);
    f.start(pageContext);


    DataTable dataTable= new DataTable("RESLST",f, new JspHelper("/applications/teamwork/resource/rowResourceList.jsp"), ResourceController.class,pageState );
    dataTable.addHeader("");
    dataTable.addHeader(I18n.get("NAME"), "resource.name");
    dataTable.addHeader(I18n.get("CODE"), "resource.code");
    dataTable.addHeader("");
    dataTable.addHeader(I18n.get("SCORE"));

    dataTable.addHeader(I18n.get("DESCRIPTION"));
    dataTable.addHeader(I18n.get("TELEPHONE"));
    dataTable.addHeader(I18n.get("MOBILE"));
    dataTable.addHeader(I18n.get("EMAIL"));




    if ("EXPORT_RESOURCES".equals(pageState.command)) {
%><script>window.open(contextPath + '/applications/teamwork/resource/resourceExport.jsp', 'resourceExport')</script><%
  }

  String savedFilterName = pageState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty();



%>
<%---------------------------------------------- MAIN COLUMN START ---------------------------------------------------------%>
<script>
  $("#RESOURCE_MENU").addClass('selected');

  //chiamata ogni volta che si rinfresca la tabella
  function dataTableCallback(totalNumberOfElements){
    if (totalNumberOfElements>0) {
      $("#btn_print_2,#btn_print,#btn_excel").removeAttr("disabled").show();
    }else {
      $("#btn_print_2,#btn_print,#btn_excel").attr("disabled", true).hide();
    }
  }


</script>
<div class="mainColumn">



  <h1 class="filterTitle" defaultTitle="<%=I18n.get("RESOURCE_LIST")%>">
    <%=JSP.ex(savedFilterName)?I18n.get(savedFilterName):I18n.get("RESOURCE_LIST")%>
  </h1>

<%
  //----------------------------------------------------------------------------  START FILTER ----------------------------------------------------------------------------

  CodeValueList cvl = new CodeValueList();
  cvl.add("PERSON",I18n.get("PERSONS")); //ATTENZIONE PERSON e COMPANY sono usati per il filtro nel discriminator
  cvl.add("COMPANY",I18n.get("COMPANIES"));
  cvl.addChoose(pageState);
  Combo comboTypeSearch = new Combo("RESOURCE_TYPE", "<br>", "fieldLSF formElements", 20, cvl, null);
  //comboTypeSearch.setJsOnChange="showHidePersonCompany($(this));";
  comboTypeSearch.labelClass = "labelLSF";

  TextField loginName = new TextField(I18n.get("FLD_LOGIN_NAME"), "FLD_LOGIN_NAME", "<br>", 20, false);
  loginName.searchField = true;

  cvl = CodeValueList.getI18nInstance(pageState, "HAVING_LOGIN", "WITHOUT_LOGIN");
  cvl.addChoose(pageState);
  Combo withLogin = new Combo("BY_LOGIN", "<br>", "fieldLSF formElements", 20, cvl, null);
  withLogin.labelClass = "labelLSF";

  cvl = CodeValueList.getI18nInstance(pageState, "IS_ENABLED", "IS_DISABLED");
  cvl.addChoose(pageState);
  Combo loginEnabled = new Combo("BY_ENABLED", "<br>", "fieldLSF formElements", 20, cvl, null);
  loginEnabled.labelClass = "labelLSF";

  cvl = CodeValueList.getI18nInstance(pageState, "IS_HIDDEN", "IS_NOT_HIDDEN");
  cvl.addChoose(pageState);
  Combo hidden = new Combo("BY_HIDDEN", "<br>", "fieldLSF formElements", 20, cvl, null);
  hidden.labelClass = "labelLSF";

  String hql = "select p.id, p.description from " + DepartmentType.class.getName() + " as p ";
  String whereForFiltering = "where p.description like :" + SmartCombo.FILTER_PARAM_NAME + " order by p.description";
  String whereForId = "where p.id = :" + SmartCombo.FILTER_PARAM_NAME;
  SmartCombo companyType = new SmartCombo("COMPANY_TYPE", hql, whereForFiltering, whereForId);
  companyType.fieldSize = 20;
  companyType.separator = "<br>";

  TextField nameSurname = new TextField("NAME_SURNAME", "NAME_SURNAME", "<br>", 22, false);
  pageState.setFocusedObjectDomId(nameSurname.id);
  nameSurname.searchField = true;

  TextField phone = new TextField("TELEPHONE", "PHONE", "<br>", 14, false);
  phone.searchField = true;

  TextField anagData = new TextField("ANAGRAPHICAL_DATA", "ANAGRAPHICAL_DATA", "<br>", 22, false);
  anagData.searchField = true;

  DateField lastModified = new DateField("LAST_MODIFIED");
  lastModified.separator = "<br>";
  lastModified.setSearchField(true);

  TextField jobDescr = new TextField("JOB_DESCRIPTION", "JOB_DESCRIPTION", "<br>", 22, false);
  jobDescr.searchField = true;

  TagBox tags= new TagBox("RESOURCE_TAGS",Resource.class,logged.getPerson().getArea());
  tags.label=I18n.get("TAGS");
  tags.separator= "<br>";
  tags.fieldSize=25;
  tags.preserveOldValue=false;

  Set<Area> areas = logged.getAreasForPermission(TeamworkPermissions.resource_canRead);
  cvl = CodeValueList.getI18nInstanceForIdentifiables(areas, pageState);
  cvl.addChoose(pageState);
  Combo area = new Combo("AREA", "<br>", "", 30, null, cvl, "");
  area.required = false;

  cvl= new CodeValueList();
  cvl.add("",I18n.get("ALL"));
  cvl.add(Fields.TRUE,I18n.get("COMPANY"));
  cvl.add(Fields.FALSE,I18n.get("DEPARTMENT")+" / "+I18n.get("PERSON"));
  Combo rootOrStandalone= new Combo("ROOT_OR_STANDALONE","<br>",null,15,cvl,"");
  rootOrStandalone.label=I18n.get("COMPANY_OR_DEPARTMENT");

  CheckField recentlyUsed= new CheckField("RECENTLY_USED","<br>",true);
  recentlyUsed.preserveOldValue=false;

  SmartCombo companyCombo = ResourceBricks.getCompanyCombo("COMPANY", TeamworkPermissions.resource_canRead, null, pageState);
  companyCombo.label = I18n.get("COMPANY");
  companyCombo.separator = "<br>";
  companyCombo.fieldSize = 20;

  SmartCombo all_staff_of = ResourceBricks.getPersonCombo("ALL_STAFF_OF", TeamworkPermissions.resource_canRead, false, null, pageState);
  all_staff_of.label = I18n.get("ALL_STAFF_OF");
  all_staff_of.fieldSize = 20;
  all_staff_of.separator = "<br>";

  TextField notes = new TextField(I18n.get("NOTES"), "NOTES", "<br>", 25, false);
  notes.addKeyPressControl(13, "obj('" + pageState.getForm().getUniqueName() + "').submit();", "onkeyup");
  notes.searchField = true;

  ButtonSupport qbe = ButtonLink.getBlackInstance(JSP.wHelp(I18n.get("HELP")), 700, 800, pageState.pageFromCommonsRoot("help/qbe.jsp"));
  qbe.toolTip = I18n.get("HELP_QBE");

  LoadSaveFilter lsfb = new LoadSaveFilter(Resource.RESOURCE, f);
  lsfb.label=I18n.get("WANT_TO_SAVE_FILTER");
  lsfb.drawButtons = false;
  lsfb.drawEditor = true;
%>

<div class="filterBar withButtons clearfix">
  <div class="filterActiveElements"></div>

  <div class="filterInactiveElements">
    <div class="filterElement filterDefault"><%nameSurname.toHtmlI18n(pageContext);%></div>
    <div class="filterElement"><%comboTypeSearch.toHtmlI18n(pageContext);%></div>
    <div class="filterElement"><%phone.toHtmlI18n(pageContext);%></div>
    <div class="filterElement" data-person><%loginName.toHtmlI18n(pageContext);%></div>
    <div class="filterElement" data-person><%withLogin.toHtmlI18n(pageContext);%></div>
    <div class="filterElement" data-person><%loginEnabled.toHtmlI18n(pageContext);%></div>

    <div class="filterElement" data-company><%companyType.toHtmlI18n(pageContext);%></div>
    <div class="filterElement"><%anagData.toHtmlI18n(pageContext);%></div>
    <div class="filterElement"><%jobDescr.toHtmlI18n(pageContext);%></div>
    <div class="filterElement"><%tags.toHtml(pageContext);%></div>
    <div class="filterElement"><%area.toHtmlI18n(pageContext);%></div>
    <div class="filterElement"><%lastModified.toHtmlI18n(pageContext);%></div>
    <div class="filterElement" data-company><%rootOrStandalone.toHtmlI18n(pageContext);%></div>
    <div class="filterElement"><%companyCombo.toHtmlI18n(pageContext);%></div>
    <div class="filterElement"><%all_staff_of.toHtml(pageContext);%></div>
    <div class="filterElement"><%notes.toHtmlI18n(pageContext);%></div>
    <div class="filterElement"><%recentlyUsed.toHtmlI18n(pageContext);%></div>

    <div class="filterElement" data-person><%hidden.toHtmlI18n(pageContext); %></div>

    <%
      // ------------------------------------------------------------------------------------------- custom fields -------------------------------------------------------------------------------------------
      if (ResourceBricks.hasCustomField()) {
        for (int i = 1; i < 7; i++) {
          DesignerField dfStr = DesignerField.getCustomFieldInstance("RESOURCE_CUSTOM_FIELD_", i, null, false, false, true, pageState);
          if (dfStr != null) {
            dfStr.usedForSearch = true;
            dfStr.preserveOldValue = false;
            dfStr.separator = "<br>";
            %><div class="filterElement"><%dfStr.toHtml(pageContext);%></div><%
          }
        }
      }
    %>
  </div>

  <div class="filterButtons">
      <div class="filterButtonsElement filterAdd"><span class="button" id="filterSelectorOpener" title="<%=I18n.get("ADD_FILTER")%>" onclick="bjs_showMenuDiv('filterSelectorBox', 'filterSelectorOpener');"><span class="teamworkIcon">f</span></span></div>
    <div class="filterButtonsElement filterSearch"><%dataTable.getSearchButton().toHtml(pageContext);%></div>

    <div class="filterActions">
      <div class="filterButtonsElement filterSave"><%lsfb.toHtml(pageContext);%></div>
      <div class="filterButtonsElement filterHelp"><%DataTable.getQBEHelpButton(pageState).toHtmlInTextOnlyModality(pageContext);%></div>
    </div>

  </div>
</div>
<script src="<%=request.getContextPath()%>/commons/js/filterEngine.js"></script>
<%
//-----------------------------------------------------------------------------   END FILTER

//  INIZIO TABELLA -------------------------------------------------------------------------

  dataTable.drawPaginator(pageContext);
  dataTable.drawTable(pageContext);
  dataTable.drawPaginatorPagesOnly(pageContext);
//----------------------------------------------------------------------------   FINE TABELLA

     %></div>
<%
//-------------------------------------------------------------------------  MAIN COLUMN END

//RIGHT COLUMN START  ----------------------------------------------------------------------
%>
<div class="rightColumn noprint"><div class="tools">

    <%
      if (logged.hasPermissionFor(TeamworkPermissions.resource_canCreate)) {

        ButtonJS opener = new ButtonJS("");
        opener.iconChar = "P";
        opener.additionalCssClass = "menuArrow square first";
        opener.toolTip = I18n.get("ADD");
        opener.label = I18n.get("ADD")+"...";

        DivOnMouseover add = new DivOnMouseover(opener);

        PageSeed newPerson = pageState.pageFromRoot("resource/resourceEditor.jsp");
        newPerson.setCommand(Commands.ADD);
        newPerson.addClientEntry("RESOURCE_TYPE", Person.class.getName());
        ButtonLink bl = new ButtonLink(newPerson);
        bl.label="+ "+I18n.get("ADD_PERSON");
        add.addButton(bl);

        PageSeed newCompany = new PageSeed(request.getContextPath() + "/applications/teamwork/resource/resourceEditor.jsp");
        newCompany.setCommand(Commands.ADD);
        newCompany.addClientEntry("RESOURCE_TYPE", Company.class.getName());
        bl = new ButtonLink("+ "+I18n.get("ADD_COMPANY"), newCompany);
        add.addButton(bl);


        //inject custom Wizard
        add.addButtons(PluginBricks.createPageQuarkItemsButtons("WIZARD", pageState));


      %><div class="toolsElement"><%add.toHtml(pageContext);%></div><%
    }

  ButtonJS imp = new ButtonJS("");
  imp.toolTip = I18n.get("RESORCES_IMPORT_EXPORT");
  imp.label = "";
  imp.iconChar="I";
  imp.additionalCssClass="square toolsElement";
  DivOnMouseover domo = new DivOnMouseover(imp);

  if (logged.hasPermissionFor(TeamworkPermissions.resource_canCreate)) {
    PageSeed importResource = new PageSeed(request.getContextPath() + "/applications/teamwork/resource/resourceImport.jsp");
    importResource.setCommand(Commands.ADD);
    ButtonLink i = new ButtonLink(I18n.get("IMPORT_RESOURCES"), importResource);
    domo.addButton(i);
  }

  ButtonExportXLS excel = new ButtonExportXLS(pageState.getForm(), ResourceController.class, Commands.FIND);
  excel.addImportClasses(Task.class.getName(), Assignment.class.getName(), Resource.class.getName(), Worklog.class.getName(), Person.class.getName(), Issue.class.getName());
  excel.outputFileName = "resourceList";
  excel.label = I18n.get("EXPORT_EXCEL");
  excel.additionalCssClass="lreq20 lreqLabel";

  excel.id="btn_excel";

  if (I18n.isActive("CUSTOM_EXPORT_EXCEL_RESOURCE")) {
    JSONObject properties = JSONObject.fromObject(I18n.get("CUSTOM_EXPORT_EXCEL_RESOURCE"));
    excel.addFieldsToExport(properties);
  } else {
    excel.addBSHFieldToExport("obj.getDisplayName()", "resource name");
    excel.addBSHFieldToExport("(obj.getAnagraphicalDatas().size()>0?obj.getAnagraphicalDataOrdered().get(0).getAddress():\"\")", "address");
    excel.addBSHFieldToExport("(obj.getAnagraphicalDatas().size()>0?obj.getAnagraphicalDataOrdered().get(0).getZip():\"\")", "postal code");
    excel.addBSHFieldToExport("(obj.getAnagraphicalDatas().size()>0?obj.getAnagraphicalDataOrdered().get(0).getCity():\"\")", "city");
    excel.addBSHFieldToExport("(obj.getAnagraphicalDatas().size()>0?obj.getAnagraphicalDataOrdered().get(0).getEmail():\"\")", "e-mail");
    excel.addBSHFieldToExport("(obj.getAnagraphicalDatas().size()>0?obj.getAnagraphicalDataOrdered().get(0).getTelephone():\"\")", "telephone");
    excel.addBSHFieldToExport("(obj.getAnagraphicalDatas().size()>0?obj.getAnagraphicalDataOrdered().get(0).getFax():\"\")", "fax");
    excel.addBSHFieldToExport("(obj.getAnagraphicalDatas().size()>0?obj.getAnagraphicalDataOrdered().get(0).getMobile():\"\")", "mobile");
    excel.addBSHFieldToExport("(obj.getAnagraphicalDatas().size()>0?obj.getAnagraphicalDataOrdered().get(0).getUrl():\"\")", "url");
    excel.addBSHFieldToExport("obj.getNotes()", "notes");
  }
  domo.addButton(excel);

  %><div class="toolsElement"><%domo.toHtml(pageContext); %></div><%


  ButtonJS print = new ButtonJS("");
  print.iconChar = "p";
  print.toolTip = I18n.get("PRINT");
  print.label = "";
  print.id = "btn_print";
  DivOnMouseover pr = new DivOnMouseover(print);

  ButtonSupport p = ButtonSubmit.getBlackInstance(pageState.getForm(), pageState.pageFromRoot("resource/resourceListPrint.jsp").href);
  p.label = I18n.get("PRINT");
  p.id = "btn_print_2";
  pr.addButton(p);

  //reports
  TreeSet<ReportBricks.Report> reports = ReportBricks.getReports("resource");
  if (reports.size() > 0) {
    pr.addSeparator();
    f.url.addClientEntry("reportName", "");
    f.url.addClientEntry("outType", "");
    for (ReportBricks.Report r : reports) {
      ButtonSubmit pri = new ButtonSubmit(pageState.getForm());
      pri.variationsFromForm.command = Commands.FIND;
      pri.variationsFromForm.href = pageState.pageFromRoot("resource/resourceIReportHelper.jsp").href;
      pri.variationsFromForm.addClientEntry("reportName", r.name);
      pri.target = "_blank";
      pri.label = r.getBestName();
      pri.preserveFormStatus = true;
      pri.additionalCssClass="lreq30 lreqLabel";
      pr.addButton(pri);
    }
  }

  //inject custom reports
  pr.addButtons(PluginBricks.createPageQuarkItemsButtonsForDataTable("REPORTS", pageState));

%><div class="toolsElement"><%pr.toHtml(pageContext); %></div></div><%

  //----------------------------------------------------------------------------  START RIGHT COLUMN INNER ----------------------------------------------------------------------------

%><div class="rightColumnInner"><%

  //----------------------------------------------------------------------------  START BUTTON BOX ----------------------------------------------------------------------------
%>
  <div class="noprint filters"><h2><%=I18n.get("MY_SAVED_FILTERS")%></h2><%

    lsfb.drawEditor =false;
    lsfb.drawButtons=true;
    lsfb.toHtml(pageContext);

    String cmd = pageState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty();


    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_RES_MY_DEPARTMENT");
      ButtonLink bl = new ButtonLink(I18n.get("PF_RES_MY_DEPARTMENT"), ps);
      bl.hasFocus=ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);
    }


    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_RES_TEAMWORK_USERS");
      ButtonLink bl = new ButtonLink(I18n.get("PF_RES_TEAMWORK_USERS"), ps);
      bl.hasFocus=ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);
    }

    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_RES_COMPANIES");
      ButtonLink bl = new ButtonLink(I18n.get("PF_RES_COMPANIES"), ps);
      bl.hasFocus=ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);
    }

    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_RES_PEOPLE");
      ButtonLink bl = new ButtonLink(I18n.get("PF_RES_PEOPLE"), ps);
      bl.hasFocus=ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);
    }

    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_LAST_MODIFIED");
      ButtonLink bl = new ButtonLink(I18n.get("PF_LAST_MODIFIED"), ps);
      bl.hasFocus=ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);

    }

    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_RECENTLY_USED");
      ButtonLink bl = new ButtonLink(I18n.get("PF_RECENTLY_USED"), ps);
      bl.hasFocus=ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtmlInTextOnlyModality(pageContext);

    }
  %></div>

  <div class="noprint recent">
    <h2><%=I18n.get("INTRO_RESOURCE_SEEN_RECENTLY")%></h2>
    <%
      List<EntityGroupRank> ranks = RankUtilities.getRanked(RankUtilities.computeWeightForEntities(RankUtilities.getRecentHitsOfOperator(logged.getIntId(),Person.class.getName(), new Date())), 3);
      ranks.addAll(RankUtilities.getRanked(RankUtilities.computeWeightForEntities(RankUtilities.getRecentHitsOfOperator(logged.getIntId(),Company.class.getName(), new Date())), 3));
      Collections.sort(ranks);

      for (EntityGroupRank egr:ranks){
        Identifiable is = egr.getEntity();
        EntityViewerBricks.EntityLinkSupport edi = Bricks.getLinkSupportForEntity(is, pageState);
        edi.getButton().iconChar="";
        %><p><% edi.bs.toHtmlInTextOnlyModality(pageContext);%></p><%
      }
    %>
  </div>
</div></div>
<%---------------------------------------------- RIGHT COLUMN END ---------------------------------------------------------%>


<%
    f.end(pageContext);
  }
%>
