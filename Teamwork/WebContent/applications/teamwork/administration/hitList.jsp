<%@ page
  import="com.twproject.operator.TeamworkOperator, com.twproject.rank.businessLogic.HitControllerAction, org.jblooming.ontology.IdentifiableSupport, org.jblooming.persistence.PersistenceBricks, org.jblooming.utilities.CodeValueList, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenBasic, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.DataTable, org.jblooming.waf.html.input.Combo, org.jblooming.waf.html.input.LoadSaveFilter, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  if (!logged.hasPermissionAsAdmin())
    throw new SecurityException("Not admin");

  if (!pageState.screenRunning) {
    ScreenBasic.preparePage(new HitControllerAction(), pageContext);
    pageState.perform(request, response).toHtml(pageContext);
  } else { %>

<%
  ButtonLink adminLink = new ButtonLink(I18n.get("ADMINISTRATION_ROOT_MENU") + " /",pageState.pageFromRoot("administration/administrationIntro.jsp"));
%>
<%adminLink.toHtmlInTextOnlyModality(pageContext);%>
<h1>Hits</h1><%

    PageSeed pageSeed = pageState.thisPage(request);
    pageSeed.setCommand(Commands.FIND);

    Form f = new Form(pageSeed);
    pageState.setForm(f);
    f.start(pageContext);

    Container cont = new Container();
  cont.level=3;
    cont.title = "hit accounts administration";
    cont.start(pageContext);


%>
<script>$("#ADMINISTRATION_ROOT_MENU").addClass('selected');</script>
<table class="table">
  <tr>
    <td width="1%"><%
      CodeValueList valueList = PersistenceBricks.getPersistentEntities(IdentifiableSupport.class);
      valueList.sort();
      valueList.addChoose(pageState);
      Combo ep = new Combo("ENTITY_CLASS", "</td><td>", null, 0, valueList, null);
      ep.label = "entity";
      ep.toHtml(pageContext);
    %></td>
    <td><%
      TextField tf = new TextField("ENTITY_ID", "</td><td>");
      tf.label = "id";
      tf.toHtml(pageContext);
    %></td>
  </tr>
</table>
<%
    DataTable dataTable = new DataTable("HITLLH", f, new JspHelper("/applications/teamwork/administration/rowHitList.jsp"), HitControllerAction.class, pageState);
    dataTable.addHeader("entity");
    dataTable.addHeader("entityClass", "hit.entityClass");
    dataTable.addHeader("entityId", "hit.entityId");
    dataTable.addHeader("operatorId", "hit.operatorId");
    dataTable.addHeader("weight", "hit.weight");
    dataTable.addHeader("when", "hit.when");


    ButtonBar bb = new ButtonBar();


    bb.addButton(dataTable.getSearchButton());

    LoadSaveFilter lsf = new LoadSaveFilter("HITL", f);
    bb.addButton(lsf);

    ButtonSupport qbe = ButtonLink.getBlackInstance(JSP.wHelp(I18n.get("HELP")), 700, 800, pageState.pageFromCommonsRoot("help/qbe.jsp"));
    qbe.toolTip = I18n.get("HELP_QBE");
    bb.addButton(qbe);

    ButtonSubmit rep = ButtonSubmit.getSearchInstance(f, pageState);
    rep.variationsFromForm.command = "REPAIR";
    rep.label = "repair - may take a while";
    bb.addButton(rep);

    bb.toHtml(pageContext);

    cont.end(pageContext);

    dataTable.drawPaginator(pageContext);
    dataTable.drawTable(pageContext);
    dataTable.drawPaginatorPagesOnly(pageContext);

    f.end(pageContext);
  }
%>