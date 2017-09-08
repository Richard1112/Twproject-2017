<%@ page
        import="com.twproject.exchange.msproject.businessLogic.ProjectImportExportControllerAction, com.twproject.operator.TeamworkOperator,
        com.twproject.security.TeamworkPermissions, com.twproject.task.Task, 
        com.twproject.waf.TeamworkHBFScreen, net.sf.mpxj.ProjectFile,
        net.sf.mpxj.mpp.MPPReader, net.sf.mpxj.mpx.MPXReader, org.jblooming.persistence.PersistenceHome,
        org.jblooming.tracer.Tracer, org.jblooming.utilities.CodeValueList, org.jblooming.utilities.JSP,
        org.jblooming.utilities.file.FileUtilities, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands,
        org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar,
        org.jblooming.waf.html.display.PathToObject, org.jblooming.waf.html.input.Combo, org.jblooming.waf.html.input.Uploader,
        org.jblooming.waf.html.input.Uploader.UploadHelper, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n,
         org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.io.File, java.util.Locale, com.twproject.waf.TeamworkPopUpScreen" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    body.areaHtmlClass="lreq20 lreqPage";
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);

  } else {

    ProjectImportExportControllerAction.ImportResult ir = null;

    Task task = null;
    if (pageState.mainObjectId != null) {
      task = (Task) PersistenceHome.findByPrimaryKey(Task.class, pageState.mainObjectId);
    }
    Throwable exc = null;
    if (Commands.SAVE.equals(pageState.command)) {

      UploadHelper instance = Uploader.getHelper("MS_FILE_TO_IMPORT", pageState);
      if (instance!=null) {
      File temporaryFile = instance.temporaryFile;
      if (temporaryFile != null && temporaryFile.exists()) {
        ProjectFile mpx = null;
        try {
          if (FileUtilities.getFileExt(instance.originalFileName.toLowerCase()).equals(".mpx")) {
            MPXReader mpxReader = new MPXReader();
            String loc = pageState.getEntryOrDefault("MPXJ_LOCALE",ApplicationState.getApplicationSetting("MPXJ_LOCALE")).stringValueNullIfEmpty();
            if (JSP.ex(loc))
              mpxReader.setLocale(new Locale(loc));
            mpx = mpxReader.read(temporaryFile);
          }
          else {
            MPPReader mppReader = new MPPReader();
            mpx = mppReader.read(temporaryFile);
          }

          ir = ProjectImportExportControllerAction.importNode(mpx, task, (TeamworkOperator) pageState.getLoggedOperator(),pageState);
        } catch (Throwable e) {
          exc = e;
          pageState.addMessageError("Error importing " + instance.originalFileName+"<br>"+e.getMessage());
        }
      }
    }
    }
    %><script>$("#TASK_MENU").addClass('selected');</script><%

    PageSeed self = pageState.thisPage(request);
    self.mainObjectId = pageState.mainObjectId;
    self.setCommand(Commands.SAVE);
    Form f = new Form(self);
    f.encType = Form.MULTIPART_FORM_DATA;
    f.alertOnChange = true;
    pageState.setForm(f);

    f.start(pageContext);




    if (pageState.mainObjectId != null) {
      /*
      ________________________________________________________________________________________________________________________________________________________________________


            path to object

      ________________________________________________________________________________________________________________________________________________________________________

      */

      PathToObject pto = new PathToObject(task);
      pto.canClick = TeamworkPermissions.task_canRead;

      PageSeed back = new PageSeed("taskList.jsp");
      back.setCommand(Commands.FIND);
      ButtonLink taskList = new ButtonLink(I18n.get("TASK_LIST") + " /", back);
      pto.rootDestination = taskList;
      pto.destination = pageState.pageInThisFolder("taskOverview.jsp",request);
      pto.destination.setCommand(Commands.EDIT);
      pto.toHtml(pageContext);

    } else {
      %> <h1><%=I18n.get("IMPORT_FROM_MSPROJECT")%></h1><%
    }


    %><hr>
<table class="table"><tr><td nowrap valign="top"><%

  if (ir == null) {
    Uploader u = new Uploader("MS_FILE_TO_IMPORT", pageState);
    u.label = I18n.get("MS_FILE_TO_IMPORT");
    u.separator="<br>";
    u.required=true;
    u.toHtml(pageContext);
    %></td>

  <td valign="top"><%
    CodeValueList cvl = ProjectImportExportControllerAction.getLocalizations();
    cvl.addChoose(pageState);
    pageState.getEntryOrDefault("MPXJ_LOCALE",ApplicationState.getApplicationSetting("MPXJ_LOCALE"));
    Combo cb = new Combo("MPXJ_LOCALE", "<br>", null, 20, cvl, null);
    cb.toHtmlI18n(pageContext);
  %><br><small>This language option is used only in case of import of MPX files, not in case of MPP.</small></td>


  </tr><tr><td valign="top" colspan="2"><br><%=I18n.get("IMPORT_FROM_MSPROJECT_DISCLAIM")%><br>
      <small>Microsoft Project is (c) Microsoft Corporation.</small><%
  }

   if (exc!=null) {
       %><br><span class="warning"><%=I18n.get("IMPORT_FROM_MSPROJECT_FAILED")%>:<%=exc.getMessage()%></span></td><td><%
   }  else if (ir != null) {
      %><%=I18n.get("IMPORT_FROM_MSPROJECT_SUCCESS")%>:</td><td><%
      PageSeed edit = pageState.pageInThisFolder("taskEditor.jsp", request);
      edit.command = Commands.EDIT;
      edit.mainObjectId = ir.root.getId();
      ButtonLink tasked = new ButtonLink(ir.root.getName(), edit);
      tasked.target="_top";
      tasked.toHtml(pageContext);
    }

    %></td></tr></table><%
    //ButtonLink.get
    ButtonBar bb2 = new ButtonBar();

    ButtonSubmit saveInstance = ButtonSubmit.getSaveInstance(f, I18n.get("GO"));
    saveInstance.enabled = ir==null;
    saveInstance.additionalCssClass="big first";
    bb2.addButton(saveInstance);
    bb2.toHtml(pageContext);



    f.end(pageContext);

  }
%>