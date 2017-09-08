<%@ page import="com.twproject.security.TeamworkPermissions,
                com.twproject.waf.TeamworkPopUpScreen,
                org.jblooming.operator.Operator,
                org.jblooming.remoteFile.FileStorage,
                org.jblooming.remoteFile.RemoteFile,
                org.jblooming.remoteFile.businessLogic.ExplorerController,
                org.jblooming.waf.ScreenArea,
                org.jblooming.waf.constants.Commands,
                org.jblooming.waf.html.display.Explorer,
                org.jblooming.waf.html.state.Form,
                org.jblooming.waf.view.PageSeed,
                org.jblooming.waf.view.PageState, java.io.File"%>
<%

  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new ExplorerController() , request);
    body.areaHtmlClass="lreq30";
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {

    FileStorage fileStorage = FileStorage.load(pageState.mainObjectId);
    Operator logged = pageState.getLoggedOperator();

    boolean canRead = fileStorage.hasPermissionFor(logged, TeamworkPermissions.fileStorage_explorer_canRead);
    boolean canWrite=  fileStorage.hasPermissionFor(logged, TeamworkPermissions.fileStorage_explorer_canWrite);
    boolean canCreateDirectory = fileStorage.hasPermissionFor(logged, TeamworkPermissions.fileStorage_explorer_canCreate);
    String rootPath=null;

    Explorer explorer = new Explorer(FileStorage.class, fileStorage);
    explorer.zipAllowed = true;

    // is there is no global permissions check if there is some rights in session
    // in case your are coming from a task and you have global read you can move outside the folder
    if (!canRead || !canWrite || !canCreateDirectory){
      Explorer.SecurityCarrier esc= (Explorer.SecurityCarrier) pageState.sessionState.getAttribute(Explorer.SecurityCarrier.getKey(pageState.mainObjectId));
      if (esc!=null){
        canRead = canRead || esc.canRead;
        canWrite = canWrite || esc.canWrite;
        canCreateDirectory= canCreateDirectory || esc.canCreateDirectory;
        rootPath=esc.rootPath;
      }
    }

    if (canRead){
      explorer.canWrite =canWrite;
      explorer.canCreateDirectory = canCreateDirectory;

      RemoteFile rfs = RemoteFile.getInstance(fileStorage);
      explorer.rfs = rfs;
      String path = pageState.getEntry("PATH").stringValueNullIfEmpty();
      rootPath = (rootPath == null ? "" : rootPath);
      explorer.rootpath = rootPath;
      if (path == null)
        path = rootPath;
      else if (!path.toLowerCase().replace(File.separatorChar, '/').startsWith(rootPath.toLowerCase().replace(File.separatorChar, '/')))
        path = rootPath;

      explorer.path = path;
      rfs.setTarget(path);

      PageSeed self = pageState.thisPage(request);
      self.mainObjectId = fileStorage.getId();
      self.setPopup(pageState.isPopup());
      self.setCommand(Commands.FIND);
      self.addClientEntry("PATH", explorer.path);
      Form f = new Form(self);
      f.encType = Form.MULTIPART_FORM_DATA;
      pageState.setForm(f);
      f.start(pageContext);
      explorer.toHtml(pageContext);

      f.end(pageContext);
    }
  }
%>
