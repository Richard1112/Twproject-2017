<%@ page import="com.opnlb.website.news.businessLogic.NewsController, com.opnlb.website.security.WebSitePermissions, org.jblooming.operator.Operator, org.jblooming.waf.ScreenBasic, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.DataTable, org.jblooming.waf.html.input.DateField, org.jblooming.waf.html.input.LoadSaveFilter, org.jblooming.waf.html.input.RadioButton, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.display.DeletePreviewer" %>
<%@ page pageEncoding="UTF-8" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);

  //verify permissions
  Operator logged = pageState.getLoggedOperator();

  // operator MUST be responsible for at least one category
  if (logged == null || !logged.hasPermissionFor(WebSitePermissions.news_canWrite))
    throw new SecurityException(I18n.get("PERMISSION_LACKING"));

  if (!pageState.screenRunning) {
    ScreenBasic.preparePage(new NewsController(), pageContext);
    pageState.perform(request, response).toHtml(pageContext);

  } else {

%><%---------------------------------------------- MAIN COLUMN START ---------------------------------------------------------%>


<div class="mainColumn">

  <h1 class="filterTitle">
  <%=I18n.get("NEWS_LIST")%>
</h1>
<%

  new DeletePreviewer("NEWS_DEL",NewsController.class, pageState);



  PageSeed self = pageState.thisPage(request);
  self.setCommand(Commands.FIND);
  Form f = new Form(self);
  pageState.setForm(f);
  f.start(pageContext);

%>
<script>$("#TOOLS_MENU").addClass('selected');</script>

<div class="optionsBar clearfix filterActiveElements">
    <div class="filterElement"><%

    TextField tfName = new TextField("TEXT", I18n.get("NAME_DESCRIPTION"), "TITLES_TEXT", "<br>", 30, false);
    tfName.addKeyPressControl(13, "obj('" + f.getUniqueName() + "').submit();", "onkeyup");

  %><%tfName.toHtml(pageContext);%>
  </div><div class="filterElement"><%
      DateField df = new DateField("START");
      df.separator = "<br>";
      df.setSearchField(true);

    %><%df.toHtmlI18n(pageContext);%>
  </div><div class="filterElement"><%

      df = new DateField("END");
      df.separator = "<br>";
      df.setSearchField(true);

    %><%df.toHtmlI18n(pageContext);%>
  </div><div class="filterElement centered"></div>
    <div class="filterElement centered"><%=I18n.get("VISIBLE")%>: <%

      RadioButton rbIns = new RadioButton(I18n.get("YES"), "VISIBLE", Fields.TRUE, "", null, false, "");
      rbIns.toHtml(pageContext);

      rbIns = new RadioButton(I18n.get("NO"), "VISIBLE", Fields.FALSE, "", null, false, "");
      rbIns.toHtml(pageContext);

      if (pageState.getEntry("VISIBLE").stringValue() == null) {
        pageState.addClientEntry("VISIBLE", "ALL");
      }
      rbIns = new RadioButton(I18n.get("ALL"), "VISIBLE", "ALL", "", null, false, "");
      rbIns.toHtml(pageContext);

    %></div>
</div><%

    ButtonBar bb = new ButtonBar();
    PageSeed edit = new PageSeed("newsEditor.jsp");
    edit.setCommand(Commands.ADD);
    ButtonLink add = new ButtonLink(edit);
    add.label = I18n.get("ADD");
    add.additionalCssClass = "first";
    bb.addButton(add);



    DataTable dataTable = new DataTable("NEWSMGR", f, new JspHelper("/applications/website/admin/news/rowNewsList.jsp"), NewsController.class, pageState);
    dataTable.addHeader(I18n.get("TITLE"), "news.title");
    dataTable.addHeader(I18n.get("START"), "news.startingDate");
    dataTable.addHeader(I18n.get("END"), "news.endingDate");
    dataTable.addHeader(I18n.get("VISIBLE"), "news.visible");
    dataTable.addHeaderFitAndCentered(I18n.get("IMAGE"));
    dataTable.addHeaderFitAndCentered(I18n.get("DELETE_SHORT"));
    bb.addButton(dataTable.getSearchButton());

    bb.addSeparator(20);

    LoadSaveFilter lsf = new LoadSaveFilter("NEWS", f);
    bb.addButton(lsf);

    bb.toHtml(pageContext);


    dataTable.drawPaginator(pageContext);
    dataTable.drawTable(pageContext);
  dataTable.drawPaginatorPagesOnly(pageContext);

    %></div>
<%---------------------------------------------- MAIN COLUMN END ---------------------------------------------------------%>

<%---------------------------------------------- RIGHT COLUMN START ---------------------------------------------------------%>

  <div class="rightColumn noprint"></div>

<script>
  function delRow(el) {
    var issueRow = $(el).closest("[newsid]");
    var issueId = issueRow.attr("newsid");
    deletePreview("NEWS_DEL", issueId, function(response) {  // callback function
      if (response && response.ok){
        issueRow.fadeOut(500, function () {$(this).remove();});
      }
    });
  }
</script>


<%

    f.end(pageContext);
    pageState.setFocusedObjectDomId(tfName.id);

  }


%>