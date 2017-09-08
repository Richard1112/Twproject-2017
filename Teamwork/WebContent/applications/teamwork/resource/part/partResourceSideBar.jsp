<%@ page import="com.twproject.document.DocumentBricks, com.twproject.document.TeamworkDocument, com.twproject.operator.TeamworkOperator, com.twproject.resource.Company, com.twproject.resource.Person, com.twproject.resource.Resource, com.twproject.security.TeamworkPermissions, com.twproject.task.IssueStatus, com.twproject.task.TaskStatus, com.twproject.waf.settings.ReportBricks, org.jblooming.anagraphicalData.AnagraphicalData, org.jblooming.system.SystemConstants, org.jblooming.utilities.JSP, org.jblooming.waf.Bricks, org.jblooming.waf.PageQuark, org.jblooming.waf.PluginBricks, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.DivOnMouseover, org.jblooming.waf.html.container.SideBar, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.display.Hint, org.jblooming.waf.html.input.Uploader, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List, java.util.Set, java.util.TreeSet, org.jblooming.waf.html.container.ButtonsOnMouseover" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);

  JspHelper jspHelper = (JspHelper) JspIncluderSupport.getCurrentInstance(request);
  Resource resource = (Resource) jspHelper.parameters.get("RESOURCE");
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  boolean isNew=resource.isNew();
  boolean isPerson = (resource instanceof Person);

  boolean canWrite = resource.hasPermissionFor(logged,TeamworkPermissions.resource_canWrite);

%><div class="rightColumn noprint"><%


if (!isNew) {
    %><div class="tools"><%

  //editor, security e login
  if (request.getRequestURI().indexOf("/resourceEditor")>0 || request.getRequestURI().indexOf("/resourceSecurity")>0 ||request.getRequestURI().indexOf("/resourceOption")>0 ) {

    boolean canAdd = resource.hasPermissionFor(logged,TeamworkPermissions.resource_canCreate);
    if (!isPerson ) {
      ButtonJS opener = new ButtonJS("");
      opener.iconChar = "P";
      opener.additionalCssClass = "first";
      opener.label = I18n.get("ADD")+"...";

      DivOnMouseover add = new DivOnMouseover(opener);

      PageSeed ps = pageState.pageFromRoot("resource/resourceEditor.jsp");
      ps.setCommand(Commands.ADD);
      ps.setMainObjectId(resource.getId());
      ps.addClientEntry("RESOURCE_TYPE", Person.class.getName());
      ButtonLink addPerson = new ButtonLink("+ " + I18n.get("ADD_PERSON"), ps);
      addPerson.enabled = canAdd;
      add.addButton(addPerson);

      ps = pageState.pageFromRoot("resource/resourceEditor.jsp");
      ps.setCommand(Commands.ADD);
      ps.setMainObjectId(resource.getId());
      ps.addClientEntry("RESOURCE_TYPE", Company.class.getName());
      ButtonLink addCompany = new ButtonLink("+ " + I18n.get("ADD_COMPANY"), ps);
      addCompany.enabled = canAdd;
      add.addButton(addCompany);

      //inject custom Wizard
      add.addButtons(PluginBricks.createPageQuarkItemsButtons("WIZARD", pageState));

      %><div class="toolsElement"><%add.toHtml(pageContext);%></div><%
    } else {
      PageSeed ps = pageState.pageFromRoot("resource/resourceEditor.jsp");
      ps.setCommand(Commands.ADD);
      if (resource.getParent()!=null)
        ps.setMainObjectId(resource.getParent().getId());
      ps.addClientEntry("RESOURCE_TYPE", Person.class.getName());
      ButtonLink addPerson = new ButtonLink("+ "+I18n.get("ADD_COLLEAGUE"), ps);
      addPerson.additionalCssClass = "first";
      addPerson.enabled = canAdd;
      %><div class="toolsElement"><%addPerson.toHtml(pageContext);%></div><%

    }
  //issue
  } else if (request.getRequestURI().indexOf("/resourceIssue")>0){
    ButtonJS bjs= new ButtonJS(I18n.get("ADD_ISSUE"),"addIssue()");
    bjs.additionalCssClass="first";
    bjs.iconChar="P";
    bjs.toHtml(pageContext);

  //document
  } else if (request.getRequestURI().indexOf("/resourceDocument")>0){
      // document list
      if (resource.hasPermissionFor(logged,TeamworkPermissions.resource_canWrite)) {

        ButtonJS addOpener = new ButtonJS("");
        addOpener.toolTip = I18n.get("ADD");
        addOpener.label = addOpener.toolTip + "...";
        addOpener.additionalCssClass = "first";
        addOpener.iconChar = "P";
        DivOnMouseover add = new DivOnMouseover(addOpener);

        // ADD DOCUMENT UPLOAD
        PageSeed ps = pageState.pageFromRoot("resource/resourceDocumentEditor.jsp");
        ps.setCommand(Commands.ADD);
        ps.addClientEntry("RES_ID", resource.getId());
        ps.addClientEntry("DOCUMENT_TYPE", TeamworkDocument.IS_UPLOAD);
        ButtonSupport addDoc = ButtonLink.getBlackInstance("+ "+I18n.get("ADD_DOCUMENT_UPLOAD"),700,1000, ps);
        add.addButton(addDoc);

        // ADD DOCUMENT FILE STORAGE
        //if (logged.hasPermissionFor(TeamworkPermissions.fileStorage_canRead)) {
        ps = pageState.pageFromRoot("resource/resourceDocumentEditor.jsp");
        ps.setCommand(Commands.ADD);
        ps.addClientEntry("RES_ID", resource.getId());
        ps.addClientEntry("DOCUMENT_TYPE", TeamworkDocument.IS_FILE_STORAGE);
        addDoc = ButtonLink.getBlackInstance("+ "+I18n.get("ADD_DOCUMENT_REMOTE"),700,1000, ps);
        addDoc.enabled=logged.hasPermissionFor(TeamworkPermissions.fileStorage_canRead);
        add.addButton(addDoc);
        //}

        // ADD DOCUMENT URL
        ps = pageState.pageFromRoot("resource/resourceDocumentEditor.jsp");
        ps.setCommand(Commands.ADD);
        ps.addClientEntry("RES_ID", resource.getId());
        ps.addClientEntry("DOCUMENT_TYPE", TeamworkDocument.IS_URL);
        addDoc = ButtonLink.getBlackInstance("+ "+I18n.get("ADD_DOCUMENT_URL"),600,700, ps);
        add.addButton(addDoc);

        // ADD CONTENT
        ps = pageState.pageFromRoot("resource/resourceDocumentEditor.jsp");
        ps.setCommand(Commands.ADD);
        ps.addClientEntry("RES_ID", resource.getId());
        ps.addClientEntry("DOCUMENT_TYPE", TeamworkDocument.IS_CONTENT);
        addDoc = ButtonLink.getBlackInstance("+ "+I18n.get("ADD_DOCUMENT_CONTENT"),700,700, ps);
        add.addButton(addDoc);

        add.toHtml(pageContext);
      }




  }




  //------------------------------------- PRINT  -------------------------------------------------------------
  ButtonJS opener = new ButtonJS("");
  opener.iconChar = "p";
  opener.toolTip = I18n.get("PRINT");
  opener.label = "";
  DivOnMouseover printButtons = new DivOnMouseover(opener);



    if (pageState.href.indexOf("resourceAssignments")>=0){
      PageSeed print = pageState.pageFromRoot("resource/resourcePrintAssignments.jsp");
      print.mainObjectId = resource.getId();
      print.setCommand(Commands.EDIT);
      ButtonSupport bl = ButtonLink.getBlackInstance("", print);
      bl.label=I18n.get("PRINT");
      printButtons.addButton(bl);

    } else {
      PageSeed print = pageState.pageFromRoot("resource/resourceEditorPrint.jsp");
      print.mainObjectId = resource.getId();
      print.setCommand(Commands.EDIT);

      ButtonSupport bl = ButtonLink.getBlackInstance("", print);
      bl.label=I18n.get("PRINT");
      printButtons.addButton(bl);
    }



  if (isPerson && !resource.isNew()) {
    PageSeed vcardPS = pageState.pageInThisFolder("vcard.jsp", request);
    vcardPS.mainObjectId = resource.getId();
    ButtonLink vcard = new ButtonLink(I18n.get("VCARD"), vcardPS);
    vcard.target ="_blank";
    printButtons.addButton(vcard);
  }

  // freeze
  PageSeed redirTo = pageState.pageFromRoot("resource/resourceEditor.jsp");
  redirTo.mainObjectId = resource.getId();
  redirTo.setCommand(Commands.EDIT);
  redirTo.command = "CREATE_SNAPSHOT";
  PageSeed printFreeze = new PageSeed("/applications/teamwork/resource/resourceEditorPrint.jsp");
  printFreeze.mainObjectId = resource.getId();
  printFreeze.setCommand(Commands.EDIT);
  printFreeze.addClientEntry("PRINT_ASSIGNMENTS", Fields.TRUE);
  printFreeze.addClientEntry("PRINT_ISSUE", Fields.TRUE);
  ButtonLink freeze = ButtonLink.getPDFFreezeButton(printFreeze, redirTo, "resource_" + resource.getId());
  String spa = ApplicationState.getApplicationSetting(SystemConstants.FLD_REPOSITORY_URL);
  freeze.enabled = !resource.isNew() && canWrite && JSP.ex(spa);
  freeze.label = I18n.get("TASK_FREEZE");
  printButtons.addButton(freeze);


  //reports
  TreeSet<ReportBricks.Report> reports = ReportBricks.getReports("resource");
  if (reports.size()>0){
    printButtons.addSeparator();
    pageState.getForm().url.addClientEntry("reportName","");
    pageState.getForm().url.addClientEntry("outType","");
    for (ReportBricks.Report r:reports){
      ButtonSubmit pri = new ButtonSubmit(pageState.getForm());
      pri.variationsFromForm.command = Commands.EDIT;
      pri.variationsFromForm.href = pageState.pageFromRoot("resource/resourceIReportHelper.jsp").href;
      pri.variationsFromForm.addClientEntry("reportName",r.name);
      pri.target = "_blank";
      pri.label = r.getBestName();
      pri.preserveFormStatus = true;
      pri.additionalCssClass="lreq30 lreqLabel";
      printButtons.addButton(pri);
    }
  }

  //inject custom reports
  printButtons.addButtons(PluginBricks.createPageQuarkItemsButtonsForDataTable("REPORTS", pageState));

%><div class="toolsElement"><%printButtons.toHtml(pageContext);%></div>
</div><%
  }

  if (!isNew ){


    //-------------------------------  HINTS MANAGEMENT ---------------------------------------------------
    Hint.HintWriter hintWriter = new Hint.HintWriter();
    if ( pageState.href.contains("resourceEditor") ) {
      hintWriter.addHint("HINT_FIRST_SECURITY", "#hintFirstSecurity", 300, 200, !logged.equals(resource.getMyself()) && resource.getMyself() == null, pageState);
      hintWriter.addHint("HINT_FIRST_OPTION", "#hintFirstOption", 300, 150,  logged.equals(resource.getMyself()) && JSP.ex(logged.getDefaultEmail()), pageState);
    }
    hintWriter.toHtml(pageContext);


  }

  %></div>
