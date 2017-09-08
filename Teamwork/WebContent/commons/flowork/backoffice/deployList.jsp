<%@ page import="org.jblooming.flowork.FlowPermissions, org.jblooming.flowork.FlowUploader, org.jblooming.flowork.FloworkApplication, org.jblooming.flowork.PlatformJbpmSessionFactory, org.jblooming.operator.Operator,
                 org.jblooming.oql.OqlQuery, org.jblooming.tracer.Tracer, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenBasic, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields,
                 org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.container.Container,
                 org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.input.Uploader, org.jblooming.waf.html.input.Uploader.UploadHelper, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState,
                 org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, org.jbpm.JbpmContext, org.jbpm.db.GraphSession, org.jbpm.graph.def.ProcessDefinition, org.jbpm.jpdl.JpdlException, org.jbpm.jpdl.xml.Problem, java.io.FileInputStream, java.io.InputStream, java.io.InputStreamReader, java.util.*" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  JbpmContext jbpmSession = PlatformJbpmSessionFactory.getJbpmContext(pageState);
  GraphSession gs = jbpmSession.getGraphSession();

  final Operator logged = pageState.getLoggedOperator();
  boolean isFlowManager = logged != null && (logged.hasPermissionFor(FlowPermissions.canManageFlows));
  if (logged == null || !isFlowManager)
    throw new SecurityException("No permission for accessing page " + request.getRequestURI());

  if (!pageState.screenRunning) {
    ScreenBasic screenBasic = ScreenBasic.preparePage(null, pageContext);
    //screenBasic.getBody().areaHtmlClass="lreq30 lreqPage";
    pageState.perform(request, response);

/*
________________________________________________________________________________________________________________________________________________________________________


  controller

________________________________________________________________________________________________________________________________________________________________________

*/
    if (Commands.SAVE.equals(pageState.getCommand())) {
      pageState.initializeEntries("table");

      String errorDescription = "";

      UploadHelper uh = Uploader.getHelper(Fields.FORM_PREFIX + "filename", pageState);

      if (uh != null) {

        InputStream is = new FileInputStream(uh.temporaryFile);

        ProcessDefinition processDefinition = null;

        FlowUploader jpdlReader = new FlowUploader(new InputStreamReader(is));
        try {
          processDefinition = jpdlReader.readProcessDefinition();
          //processDefinition = ProcessDefinition.parseXmlInputStream(is);
        } catch (JpdlException e) {
          Tracer.platformLogger.error(e);
        }
        if (is!=null)
          is.close();

        if (jpdlReader.getProblems().size() > 0) {
          for (Problem prb : jpdlReader.getProblems()) {
            if (prb.getLevel() == Problem.LEVEL_WARNING){
              errorDescription = errorDescription + "<span class=\"warning\">warning: " + prb.getDescription() + "</span><br>";
              if (prb.getException()!=null && JSP.ex(prb.getException().getMessage()))
                  errorDescription = errorDescription +"<small>"+prb.getException().getMessage()+"</small><br>";
            }else{
              errorDescription = errorDescription + "<span class=\"warning\">error: " + prb.getDescription() + "</span><br>";
              if (prb.getException()!=null && JSP.ex(prb.getException().getMessage()))
                  errorDescription = errorDescription +"<small>"+prb.getException().getMessage()+"</small><br>";
            }
          }
        }

        if (jpdlReader.getProblems().size() == 0 || !Problem.containsProblemsOfLevel(jpdlReader.getProblems(), Problem.LEVEL_ERROR)) {
          // deploy the process
          jbpmSession.deployProcessDefinition(processDefinition);
        }
      } else {
        errorDescription = "select a valid flow definition file";
      }

      pageState.addClientEntry("errorDescription", errorDescription);

    } else if (Commands.DELETE.equals(pageState.getCommand())) {
      pageState.initializeEntries("table");

      String hql = "select pd from " + ProcessDefinition.class.getName() + " as pd where pd.id=:myid";
      OqlQuery query = new OqlQuery(hql);
      query.getQuery().setLong("myid", Long.parseLong(pageState.mainObjectId.toString()));
      ProcessDefinition pd = (ProcessDefinition) query.uniqueResult();

      FloworkApplication flw = (FloworkApplication) ApplicationState.platformConfiguration.getDefaultApplication();
      flw.removeDefinition(pd,pageState);
      pageState.mainObjectId = null;
      pageState.setCommand(null);
    }

    pageState.toHtml(pageContext);

  } else {

    %><script>$("#ADMINISTRATION_ROOT_MENU").addClass('selected');</script><%
  ButtonLink adminLink = new ButtonLink(I18n.get("ADMINISTRATION_ROOT_MENU") + " /",pageState.pageFromRoot("administration/administrationIntro.jsp"));
  adminLink.toHtmlInTextOnlyModality(pageContext);
%><h1>Flow administration</h1><%
/*
________________________________________________________________________________________________________________________________________________________________________


  view

________________________________________________________________________________________________________________________________________________________________________

*/

    Container df = new Container();
    df.title = I18n.get("FLUX_PUBLISHED");
    df.overflow = "auto";
    df.start(pageContext);
%>
<table align="center" border="0" class="table dataTable">
 <thead class="dataTableHead">
  <tr>
    <th class="tableHead"> id</th>
    <th class="tableHead"> version</th>
    <th class="tableHead"> name</th>
    <th class="tableHead"> definition </th>
    <th class="tableHead"> inspection </th>
    <th class="tableHead lreq30 lreqLabel"> management </th>
    <th class="tableHead lreq30 lreqHide"> del. </th>

  </tr>
 </thead>
  <%


    Collection allDefs = gs.findAllProcessDefinitions();
    Collection updDefs = gs.findLatestProcessDefinitions();
    if (allDefs != null && allDefs.size() > 0) {

      List allDefinitions = new ArrayList(allDefs);
      Collections.sort(allDefinitions,
        new Comparator() {
          public int compare(Object b, Object a) {

            return ((ProcessDefinition) b).getName().compareToIgnoreCase((((ProcessDefinition) a).getName()));
          }
        });


      PageSeed psGraph = new PageSeed("nodesPlus.jsp");

      PageSeed psNodes = new PageSeed("nodes.jsp");
      ButtonLink struct = new ButtonLink(psNodes);
      struct.label = I18n.get("STRUCTURE");

      PageSeed psFlow = new PageSeed("instances.jsp");
      ButtonLink flow = new ButtonLink(psFlow);
      flow.label = I18n.get("INSTANCES");

     /* PageSeed psAssoc = new PageSeed("swimlane.jsp");
      ButtonLink assoc = new ButtonLink(psAssoc);
      assoc.label = I18n.get("ASSOCIATED_OPERATORS");*/

      /*PageSeed psS = new PageSeed("starters.jsp");
      ButtonLink stBut = new ButtonLink(psS);
      stBut.label = "starters";*/

      /*
      PageSeed destroy = pageState.thisPage(request);
      destroy.setCommand("DESTROY");
      ButtonLink des = new ButtonLink(destroy);
      des.label = "destroy";
      des.confirmRequire = true;
      */

      for (Object o :allDefinitions) {
        ProcessDefinition definition = (ProcessDefinition) o;

        String hql =
          "select count(pi.id) " +
            "from org.jbpm.graph.exe.ProcessInstance as pi " +
            "where pi.processDefinition.id = :processDefinitionId and pi.end is null";

        OqlQuery query = new OqlQuery(hql);
        query.getQuery().setLong("processDefinitionId", definition.getId());

        long sizeInst = (Long) query.uniqueResult();
        if (!updDefs.contains(definition) && sizeInst == 0)
          continue;
        psNodes.setMainObjectId(definition.getId());
        psFlow.setMainObjectId(definition.getId());
        psGraph.setMainObjectId(definition.getId());

        flow.label = I18n.get("INSTANCES")+" (" + sizeInst + ")";
        //psAssoc.setMainObjectId(definition.getId());

        /*        
        // get startes count
        int stnum=0;
        Starter starter = (Starter) PersistenceHome.findUniqueNullIfEmpty(Starter.class, "definitionName", definition.getName());
        if (starter != null && starter.getSwimlanes()!=null) {
          stnum=starter.getSwimlanes().size();
          stBut.toolTip="";
          flow.enabled = true;
          flow.toolTip="";
        } else {
          stBut.toolTip=I18n.get("NO_STARTER_DEFINED");
          //flow.enabled = false;
          flow.toolTip=I18n.get("NO_STARTER_DEFINED");
        }
        stBut.label = "starters ("+stnum+")";
        */

        ButtonSupport blGraph = ButtonLink.getBlackInstance(I18n.get("GRAPH"),768,1024,psGraph);

  %>
<tr class="alternate" >
  <td><%=definition.getId()%><%=definition.getFileDefinition() != null ? definition.getFileDefinition().getName() : ""%></td>
  <td><%=definition.getVersion()%></td>
  <td title="<%=definition.getName()%>"><%=definition.getName()%><div style="font-size:smaller;"><%=JSP.w(definition.getDescription())%></div></td>
  <td align="center"><%struct.toHtmlInTextOnlyModality(pageContext);%></td>
  <td align="center"><%blGraph.toHtmlInTextOnlyModality(pageContext);%></td>
  <td align="center" class="lreq30"><%flow.toHtmlInTextOnlyModality(pageContext);%></td>

  <td align="center" class="lreq30 lreqHide"><%
    PageSeed edit = pageState.thisPage(request);
    edit.mainObjectId = definition.getId();
    edit.setCommand(Commands.DELETE);
    ButtonJS editLink = new ButtonJS("window.location.href='"+edit.toLinkToHref()+"'");
    editLink.confirmRequire=true;
    editLink.iconChar="d";
    editLink.additionalCssClass="delete";
    editLink.label="";
    editLink.toolTip="Careful! Will delete latest version.";
    editLink.toHtmlInTextOnlyModality(pageContext);
  %></td>

</tr>
  <%
    }
  } else {
  %> <tr><td colspan="4"><%=I18n.get("NO_FLOW_DEPLOYED")%></td></tr> <%
  }
%>
</table>
<%
  df.end(pageContext);
%>
<br><div  class="lreq30 lreqLabel"> <h3><%=I18n.get("LOAD_FLUX")%></h3><%

  PageSeed me = pageState.thisPage(request);
  me.addClientEntry(pageState.getEntry(Fields.APPLICATION_NAME));
  me.setCommand(Commands.FIND);
  Form form = new Form(me);
  form.encType = Form.MULTIPART_FORM_DATA;
/*  Uploader uploader = new Uploader(Fields.FORM_PREFIX + "filename", form, pageState);
  uploader.label = "flux file:";
  uploader.separator = "</td><td>";
  uploader.size = 70;*/
  TextField uploader = new TextField("file",Fields.FORM_PREFIX + "filename","</td><td>",70);
  uploader.label = I18n.get("FLUX_FILE");

  form.start(pageContext);

  uploader.toHtml(pageContext);
  %>
    </td></tr></table> <%

    ButtonBar bb = new ButtonBar();
    ButtonSubmit save = ButtonSubmit.getSaveInstance(form, I18n.get("PROCEED"));
    save.additionalCssClass="first big";
    bb.addButton(save);
    bb.toHtml(pageContext);

    form.end(pageContext);

    //dfnew.end(pageContext);
%></div><%


  String errorDescription = pageState.getEntry("errorDescription").stringValueNullIfEmpty();
  if (errorDescription != null) {
    Container dfer = new Container();
    dfer.title = "<big>Errors and warnings:</big>";
    dfer.setCssPostfix("warn");
    dfer.width = "70%";
    dfer.centeredOnScreen=true;
    dfer.closeable=true;
    dfer.collapsable=true;
    dfer.draggable=true;
    dfer.start(pageContext);
        %><big><b><%=errorDescription%></b></big><%
    dfer.end(pageContext);
  }

  }
%>
