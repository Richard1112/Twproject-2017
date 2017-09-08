<%@ page import="com.twproject.agenda.AgendaBricks, com.twproject.agenda.businessLogic.AgendaController, com.twproject.messaging.board.Board, com.twproject.resource.Person, com.twproject.resource.ResourceBricks, com.twproject.security.TeamworkPermissions, com.twproject.waf.TeamworkHBFScreen, org.jblooming.agenda.EventType, org.jblooming.ontology.SerializedList, org.jblooming.operator.Operator, org.jblooming.oql.OqlQuery, org.jblooming.security.Area, org.jblooming.security.Permission, org.jblooming.utilities.CodeValueList, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.container.DivOnMouseover, org.jblooming.waf.html.container.HeadBar, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.DataTable, org.jblooming.waf.html.input.*, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.ArrayList, java.util.HashSet, java.util.List, java.util.Set, org.jblooming.waf.constants.Fields" %>
<%
    PageState pageState = PageState.getCurrentPageState(request);
    if (!pageState.screenRunning) {
        pageState.screenRunning = true;
        final ScreenArea body = new ScreenArea(new AgendaController(), request);
        TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
        lw.register(pageState);
        pageState.perform(request, response).toHtml(pageContext);
    } else {
        Operator logged = pageState.getLoggedOperator();
        Person loggedPerson = Person.getLoggedPerson(pageState);
        PageSeed self = pageState.thisPage(request);
        self.setCommand(Commands.FIND);
        Form f = new Form(self);
        f.addKeyPressControl(13, "dataTableRefresh('multi');", "onkeyup");
        pageState.setForm(f);
        f.start(pageContext);

%><script>$("#AGENDA_MENU").addClass('selected');</script><%



    DataTable dataTable= new DataTable("AGELST",f,new JspHelper("/applications/teamwork/agenda/rowAgendaList.jsp"), AgendaController.class,pageState);
    dataTable.addHeaderFitAndCentered("<input type='checkbox' onclick='masterCheckBox($(this));'>");
    dataTable.addHeaderFitAndCentered(I18n.get("EDIT_SHORT"));
    dataTable.addHeader(I18n.get("START"), "schedule.start");
    dataTable.addHeader(I18n.get("END"), "schedule.end");
    dataTable.addHeader(I18n.get("START_HOUR"));
    dataTable.addHeader(I18n.get("END_HOUR"));
    dataTable.addHeader(I18n.get("AGENDA_SUMMARY"));
    dataTable.addHeader(I18n.get("DESCRIPTION"));
    dataTable.addHeader(I18n.get("AGENDA_AUTHOR"), "event.author.id");
    dataTable.addHeader(I18n.get("LOCATION"));
    dataTable.addHeader(I18n.get("EVENT_TYPE"));
    dataTable.addHeaderFitAndCentered(I18n.get("DELETE_SHORT"));
    dataTable.drawPageFooter=true;
    dataTable.tableAdditionalAttributes="cellpadding=2 cellspacing=1 border=0";


  LoadSaveFilter lsfb = new LoadSaveFilter(I18n.get("WANT_TO_SAVE_FILTER"), "EVENT", dataTable.form);
  lsfb.toolTip=I18n.get("MY_SAVED_FILTERS");
  lsfb.drawButtons = false;
  lsfb.drawEditor = true;


  String savedFilterName = pageState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty();

%>
<%------------------------------------------------ MAIN COLUMN START ---------------------------------------------------------%>
<div class="mainColumn">
<%----------------------------------------------- HEADER START ---------------------------------------------------------%>

  <h1 class="filterTitle" defaultTitle="<%=I18n.get("AGENDA_LIST")%>">
    <%=JSP.ex(savedFilterName)?I18n.get(savedFilterName):I18n.get("AGENDA_LIST")%>
  </h1>
<%---------------------------------------------- HEADER END ---------------------------------------------------------%>

<%----------------------------------------------------------------------------  START FILTER ----------------------------------------------------------------------------%>
<%
  CodeValueList radios= new CodeValueList();

  if(logged.hasPermissionAsAdmin()) {
    radios.add("ANY", I18n.get("FILTER_ANY"));
  }
  radios.add("NONE",I18n.get("FILTER_NONE"));
  radios.add("WORK",I18n.get("FILTER_WORK"));
  radios.add("PERSONAL",I18n.get("FILTER_IS_PERSONAL"));
  radios.add("ONLY_MINE",I18n.get("FILTER_ONLY_ME_IN_IT"));
  radios.add("AUTHOR",I18n.get("FILTER_AUTHOR"));
  radios.add("UNAVAIL",I18n.get("FILTER_UNAVAIL"));
  Combo filter = new Combo(I18n.get("FILTER"), "FILTER", "<br>", "", 25, "", radios, "");

  TextField descr = new TextField(I18n.get("OBJECT_DESCRIPTION"), "OBJECT_DESCRIPTION", "<br>", 30, false);

  TextField location = new TextField(I18n.get("LOCATION"), "LOCATION", "<br>", 20, false);

  Set<Area> areaSet = new HashSet();
  areaSet.add(loggedPerson.getArea());
  areaSet = loggedPerson.getMyself().getAreasForPermission(TeamworkPermissions.resource_canRead);
  Combo eventType=null;
  if (JSP.ex(areaSet)) {
    String hql = "select e.id, e.description from " + EventType.class.getName() + " as e";
    hql = hql + " where e.area in (:areas)";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setParameterList("areas", areaSet);
    List<Object[]> res = oql.list();

    CodeValueList cvl = new CodeValueList();
    for (Object[] resEntry: res) {
      cvl.add(resEntry[0] + "", resEntry[1] + "");
    }
    cvl.addChoose(pageState);
    eventType = new Combo("EVENT_TYPE", "<br>","", 25,"",cvl, "");
    eventType.label=I18n.get("EVENT_TYPE");
    eventType.required=false;
  }

  SmartCombo author = ResourceBricks.getPersonCombo("AUTHOR", false, null, pageState);
  author.label = I18n.get("AGENDA_AUTHOR");
  author.fieldSize = 30;
  author.separator = "<br>";


  SmartCombo target = ResourceBricks.getPersonCombo("TARGET", false, null, pageState);
  target.label = I18n.get("TARGETS");
  target.fieldSize = 20;
  target.separator = "<br>";


  String boardHql = "from " + Board.class.getName() + " as board ";
  boardHql = boardHql + "where board.active = :truth";
  OqlQuery oql = new OqlQuery(boardHql);
  oql.getQuery().setBoolean("truth", Boolean.TRUE);
  List<Board> boardList = oql.list();
  List<Board> visibleBoards = new ArrayList();
  for (Board board : boardList) {
    if (board.hasPermissionFor(logged, TeamworkPermissions.board_canRead))
      visibleBoards.add(board);
  }
  CodeValueList boardCvl = CodeValueList.getI18nInstanceForIdentifiables(visibleBoards, pageState);
  // don't pop up to remind that user makes changes
  boardCvl.addChoose(pageState);
  Combo boards = new Combo("BOARD", "", "", 10, boardCvl, "");
  boards.separator = "<br>";

  DateField periodStart = new DateField("VALIDITY_PERIOD_START");
  periodStart.separator = "<br>";
  periodStart.setSearchField(true);

  DateField periodEnd = new DateField("VALIDITY_PERIOD_END");
  periodEnd.separator = "<br>";
  periodEnd.setSearchField(true);


  CheckField isMeeting = new CheckField("IS_MEETING", "&nbsp;", false);
%>


  <div class="filterBar clearfix">
    <div class="filterActiveElements"></div>

    <div class="filterInactiveElements">
      <div class="filterElement"><%filter.toHtml(pageContext);%></div>
      <div class="filterElement filterDefault"><%descr.toHtml(pageContext);%></div>
      <div class="filterElement"><%location.toHtml(pageContext);%></div>
      <%if (eventType != null) {%>
      <div class="filterElement"><%eventType.toHtml(pageContext);%></div>
      <%}%>
      <div class="filterElement"><%author.toHtml(pageContext);%></div>
      <div class="filterElement"><%target.toHtml(pageContext);%></div>
      <div class="filterElement"><%boards.toHtmlI18n(pageContext);%></div>
      <div class="filterElement"><%periodStart.toHtmlI18n(pageContext);%></div>
      <div class="filterElement"><%periodEnd.toHtmlI18n(pageContext);%></div>
      <div class="filterElement"><%isMeeting.toHtmlI18n(pageContext);%></div>


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
<%----------------------------------------------------------------------------  END FILTER ----------------------------------------------------------------------------%>

<div id="bulkOp" style="display:none;">
    <div id="bulkRowSel"></div>
    <%
        ButtonJS buttonJS = new ButtonJS(I18n.get("DEL_SELECTED"), "deleteSelected()");
        buttonJS.confirmRequire = true;
        buttonJS.confirmQuestion = I18n.get("FLD_CONFIRM_DELETE");
        buttonJS.iconChar = "&#xa2;";
        buttonJS.additionalCssClass = " bulk";
        buttonJS.label = I18n.get("DEL_SELECTED");

        buttonJS.toHtmlInTextOnlyModality(pageContext);
    %>
</div>


  <div style="position: relative">
    <%dataTable.drawPaginator(pageContext);%>
  </div>
  <%
    //---------------------------------  INIZIO TABELLA ----------------------------------------------
    dataTable.drawTable(pageContext);
    //---------------------------------  FINE TABELLA ----------------------------------------------

    dataTable.drawPaginatorPagesOnly(pageContext);

    f.end(pageContext);

%></div>

<div class="rightColumn noprint">

    <div class="tools"><%


  PageSeed ps = new PageSeed("agendaEditor.jsp");
  ps.setCommand(Commands.ADD);
  ButtonSubmit sub = new ButtonSubmit(pageState.getForm());
  sub.label=I18n.get("AGEADD");
  sub.variationsFromForm = ps;
  sub.iconChar = "P";
  sub.additionalCssClass = "first";


  %><div class="toolsElement"><%sub.toHtml(pageContext);%></div><%

%></div><%


  %><div class="rightColumnInner"><%

  ps= AgendaBricks.getAgendaView(pageState);
  sub = new ButtonSubmit(pageState.getForm());
  sub.label=I18n.get("AGENDA");
  sub.variationsFromForm = ps;
  sub.iconChar = "C";
  sub.toHtmlInTextOnlyModality(pageContext);


  SerializedList<Permission> permissions = new SerializedList();
  permissions.add(TeamworkPermissions.resource_canRead);
  //permissions.add(TeamworkPermissions.assignment_manage);
  TextField.hiddenInstanceToHtml("PERM_REQUIRED", permissions.serialize(), pageContext);

  %><div class="noprint filters">
  <h2><%=I18n.get("MY_SAVED_FILTERS")%></h2>
  <%

    lsfb.drawEditor = false;
    lsfb.drawButtons = true;
    lsfb.toHtml(pageContext);


  %></div><%



%></div></div>


<script type="text/javascript">


    function masterCheckBox(el){
        if (el.is(":checked")){
            $(".ck :checkbox").prop("checked",true);
            $("tr[evid]").addClass("selected");
        } else {
            $(".ck :checkbox").prop("checked",false);
            $("tr[evid]").removeClass("selected");
        }
        refreshBulk(el);
    }

    function deleteSelected(){
        var ids=[];
        $(".ck :checkbox:checked").each(function(){
            ids.push($(this).parents("tr:first").attr("evId"));
        });

        var req={CM:"DELSEEV",ids:JSON.stringify(ids)};
        $.getJSON("agendaAjaxController.jsp",req,function(response){
            jsonResponseHandling(response);
            if (response.ok==true){
                for (var i=0;i<response.deleted.length;i++){
                    $("tr[evId="+response.deleted[i]+"]").fadeOut(100,function(){$(this).remove()});
                }
            }
        });

    }

</script>

<%
    }
%>
