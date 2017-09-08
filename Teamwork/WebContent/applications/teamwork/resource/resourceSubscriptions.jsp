<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.resource.Resource, com.twproject.resource.businessLogic.ResourceController, com.twproject.waf.TeamworkHBFScreen, com.twproject.waf.html.ResourceHeaderBar, org.jblooming.messaging.Listener, org.jblooming.ontology.Identifiable, org.jblooming.operator.businessLogic.OptionController, org.jblooming.oql.QueryHelper, org.jblooming.persistence.PersistenceHome, org.jblooming.security.Securable, org.jblooming.utilities.JSP, org.jblooming.utilities.ReflectionUtilities, org.jblooming.waf.Bricks, org.jblooming.waf.EntityViewerBricks, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonImg, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.display.Img, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List, org.jblooming.waf.html.input.CheckField, com.twproject.security.TeamworkPermissions" %><%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new ResourceController(), request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);

    pageState.toHtml(pageContext);

  } else {
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    Resource resource = (Resource) pageState.getMainObject();
    PageSeed seed = pageState.thisPage(request);
    seed.setMainObjectId(resource.getId());
    seed.addClientEntry("RESOURCE_TYPE", ReflectionUtilities.deProxy(resource.getClass().getName()));  
    seed.setCommand(Commands.EDIT);

    boolean canWrite = resource.hasPermissionFor(logged, TeamworkPermissions.resource_canWrite) ||resource.equals(logged.getPerson());


    Form form = new Form(seed);
    pageState.setForm(form);
    form.alertOnChange = true;
    form.encType = Form.MULTIPART_FORM_DATA;
    form.start(pageContext);


%>
<div class="mainColumn"><%

  //---------------------------------------- HEAD BAR -------------------------------------------
    pageState.addClientEntry("RESOURCE_TABSET","TASK_SUBSCRIPTIONS_TAB");
    ResourceHeaderBar head = new ResourceHeaderBar(resource);
  head.pathToObject.destination=pageState.pageInThisFolder("resourceEditor.jsp",request);
  head.pathToObject.destination.command=Commands.EDIT;
  head.toHtml(pageContext);



  List<Listener> listeners=null;
  if (resource.getMyself()!=null){
    String hql = "from " + Listener.class.getName();
    QueryHelper query = new QueryHelper(hql);
    query.addOQLClause("ownerx=:owx", "owx", resource.getMyself().getId().toString());
    query.addOQLClause("eventType is not null");
    listeners = query.toHql().list();
  }
  if (JSP.ex(listeners)) {

%><table id="multi" class="table fixHead fixFoot dataTable" border="0" id="allsubs">

  <thead class="dataTableHead"><tr>
    <th class="tableHead">
    <%
    CheckField cf = new CheckField("","chall","",false);
    cf.toolTip=I18n.get("SELECT_DESELECT_ALL");
    cf.script=" onclick=\"selUnselAll($(this));\"";
      cf.toHtml(pageContext);
    %>
    </th>
    <th class="tableHead"><%=I18n.get("TYPE")%></th>
    <th class="tableHead" id="headTaskName"><%=I18n.get("NAME")%></th>
    <th class="tableHead"><%=I18n.get("LISTENER_EVENT")%></th>
    <th class="tableHead"><%=I18n.get("LISTENER_MEDIA")%></th>
    <th class="tableHead">&nbsp;</th>
    </tr></thead>
    <tbody>
  <%


    for (Listener listener : listeners) {

      String theClass = listener.getTheClass();
      theClass = ReflectionUtilities.deProxy(theClass);
      Class<? extends Identifiable> claz = (Class<? extends Identifiable>) Class.forName(theClass);
      Identifiable identi = PersistenceHome.findByPrimaryKey(claz, listener.getIdentifiableId());

      if (identi != null) {
        EntityViewerBricks.EntityLinkSupport edi = Bricks.getLinkSupportForEntity(ReflectionUtilities.getUnderlyingObject(identi), pageState);
        if (edi != null) {
          if (((Securable) identi).hasPermissionFor(logged, edi.readPermission)) {
              %><tr class="data alternate" subId="<%=listener.getId()%>">
                  <td><input type="checkbox" onclick="refreshBulk($(this));" class="selector"></td>
                  <td><span class="teamworkIcon"><%=edi.bs != null?edi.bs.iconChar:""%></span></td>
                  <td subTaskCell><%
              if (edi.bs != null) {
                edi.bs.iconChar="";
                edi.bs.toHtmlInTextOnlyModality(pageContext);
              }
              %></td>
              <td subEVTCell><%=I18n.get(listener.getEventType())%></td>
              <td><%=listener.getMedia().toLowerCase()%></td>
              <td align="center"><%


                PageSeed deleteMessage = pageState.thisPage(request);
                deleteMessage.setMainObjectId(listener.getId());
                deleteMessage.setCommand(Commands.DELETE);

                ButtonJS removeRowAndRefresh = new ButtonJS("_removeRow($(this).closest('tr[subId]'))");
                removeRowAndRefresh.confirmRequire = true;
                removeRowAndRefresh.confirmQuestion = I18n.get("FLD_CONFIRM_DELETE");
                removeRowAndRefresh.iconChar="d";
                removeRowAndRefresh.additionalCssClass="delete";
                removeRowAndRefresh.label="";
                removeRowAndRefresh.toolTip="" + listener.getId();
                removeRowAndRefresh.enabled=canWrite;
                removeRowAndRefresh.toHtmlInTextOnlyModality(pageContext);
              %></td>
            </tr>
            <%
          }
        }
      }
    }

  %>
    </tbody>
  <tfoot><tr><td id="bulkPlace" colspan="99"></td></tr></tfoot>
</table>

  <div id="bulkOp" style="display:none;">
    <div id="bulkRowSel"></div>

    <div><%
      ButtonJS removeAll = new ButtonJS(I18n.get("ISSUE_REMOVE_ALL"), "removeAll();");
      removeAll.confirmRequire = true;
      removeAll.iconChar = "&#xa2;";
      removeAll.confirmQuestion = I18n.get("FLD_CONFIRM_DELETE");
      removeAll.label = I18n.get("ISSUE_REMOVE_ALL");
      removeAll.toHtmlInTextOnlyModality(pageContext);
    %></div>
  </div>


<%

  } else {
%><h2 class="hint" style="text-align: center"><%=I18n.get("NO_LISTENERS_SUBSCRIBED")%></h2><%
  }


  %></div><%
    //---------------------------------------- HEAD BAR -------------------------------------------
    JspHelper side = new JspHelper("part/partResourceSideBar.jsp");
    side.parameters.put("RESOURCE", resource);
    side.toHtml(pageContext);



    %>

<script type="text/javascript">

  //inject the table search
  createTableFilterElement($("#headTaskName"),".dataTable [subid]","[subTaskCell],[subEVTCell]");


  function removeAll(){
    showSavingMessage();
    $(".selector:checked").each(function() {
      _removeRow($(this).closest("tr[subId]"));
    });
    hideSavingMessage();
  }

  function _removeRow(row){
    var rowid=row.attr("subId");
    if(rowid != null && rowid != ""){
      executeCommand('CALLCONTR', 'CTCL=<%=OptionController.class.getName()%>&CTRM=<%=Commands.DELETE%>&OBID='+rowid);
      row.fadeOut(10,function(){
        $(this.remove());
        refreshBulk($("#multi .dataTableHead"));
      });
    }
  }


</script>


<%

    form.end(pageContext);

  }
  %>
