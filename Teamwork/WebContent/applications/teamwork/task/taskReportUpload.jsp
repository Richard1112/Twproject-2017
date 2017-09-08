<%@page import="org.jblooming.security.Permission"%>
<%@page import="org.jblooming.utilities.JSP"%>
<%@page import="java.util.List"%>
<%@page import="org.jblooming.oql.OqlQuery"%>
<%@page import="com.teamwork.expand.TaskReportType"%>
<%@page import="com.teamwork.expand.ReportExplorerController"%>
<%@page import="com.twproject.task.Task"%>
<%@page import="com.teamwork.expand.ReportExplorer"%>
<%@page import="org.jblooming.system.SystemConstants"%>
<%@page import="org.jblooming.waf.settings.ApplicationState"%>
<%@page import="org.jblooming.remoteFile.RemoteFileSystem"%>
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
    final ScreenArea body = new ScreenArea(new ReportExplorerController() , request);
    body.areaHtmlClass="lreq30";
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {
	String taskId= pageState.getEntry("TASK_ID").stringValueNullIfEmpty();
	Task task=Task.load(taskId);
    FileStorage fileStorage = new FileStorage();
    fileStorage.setContent(ApplicationState.getApplicationSetting(SystemConstants.FLD_REPOSITORY_URL));
    fileStorage.setName("Root");
    Operator logged = pageState.getLoggedOperator();
    boolean canRead = logged.hasPermissionFor(TeamworkPermissions.report_canCreate);
    String canPath=pageState.getEntry("PATH").stringValue();
    boolean canWrite=canPath.equalsIgnoreCase(task.getCode())?false:true;
    canWrite=canWrite&&(canPath.endsWith("/backup")?false:true);
    if(canWrite&&logged.hasPermissionFor(TeamworkPermissions.report_canWrite)){
    	canWrite=true;
    }else{
    	canWrite=false;
    }
    boolean canCreateDirectory = canPath.equalsIgnoreCase(task.getCode())?false:true;
    canCreateDirectory=canCreateDirectory&&(canPath.endsWith("/backup")?false:true);
    if(canCreateDirectory&&(logged.hasPermissionFor(TeamworkPermissions.fileStorage_explorer_canCreate))){
    	canCreateDirectory=true;
    }else{
    	canCreateDirectory=false;
    }
    
    
    String rootPath=null;
    if(pageState.getEntry("PATH").stringValue().equalsIgnoreCase(task.getCode())){
    	String hql = "from " + TaskReportType.class.getName() + " as tt ";
        OqlQuery oql = new OqlQuery(hql);
        List<TaskReportType> tts = oql.list();
        for(TaskReportType tt:tts){
        	if(JSP.ex(tt.getName())){
        		File f=new File(ApplicationState.getApplicationSetting(SystemConstants.FLD_REPOSITORY_URL)+File.separator+task.getCode()+File.separator+tt.getName());
        		if(!f.exists())f.mkdirs();
        	}
        }
    }
    ReportExplorer explorer = new ReportExplorer(FileStorage.class, fileStorage);
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

      RemoteFile rfs = new RemoteFileSystem(fileStorage);
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
      self.addClientEntry("TASK_ID", task.getId());
      self.addClientEntry("PATH", explorer.path);
      Form f = new Form(self);
      f.id="reportUploadFormId";
      f.encType = Form.MULTIPART_FORM_DATA;
      f.alertOnChange=false;
      pageState.setForm(f);
      f.start(pageContext);
      explorer.toHtml(pageContext);

      f.end(pageContext);
    }
  }
%>
