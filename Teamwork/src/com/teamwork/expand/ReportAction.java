package com.teamwork.expand;

import java.text.ParseException;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;

import org.jblooming.ApplicationException;
import org.jblooming.messaging.SomethingHappened;
import org.jblooming.ontology.Documentable;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.ontology.VersionHome;
import org.jblooming.ontology.businessLogic.DeleteHelper;
import org.jblooming.operator.Operator;
import org.jblooming.oql.OqlQuery;
import org.jblooming.oql.QueryHelper;
import org.jblooming.page.HibernatePage;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.security.Area;
import org.jblooming.security.Role;
import org.jblooming.security.SecurityException;
import org.jblooming.system.SystemConstants;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.ActionSupport;
import org.jblooming.waf.ActionUtilities;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.constants.OperatorConstants;
import org.jblooming.waf.constants.SecurityConstants;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.html.display.DataTable;
import org.jblooming.waf.html.display.Paginator;
import org.jblooming.waf.html.input.SmartCombo;
import org.jblooming.waf.html.input.Uploader;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.state.PersistentSearch;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.RestState;

import com.twproject.document.TeamworkDocument;
import com.twproject.document.businessLogic.DocumentAction;
import com.twproject.operator.TeamworkOperator;
import com.twproject.rank.Hit;
import com.twproject.resource.Company;
import com.twproject.resource.Person;
import com.twproject.resource.Resource;
import com.twproject.security.TeamworkPermissions;
import com.twproject.task.Assignment;
import com.twproject.task.Task;

public class ReportAction extends ActionSupport{

	public TeamworkOperator logged;
	public Task task=null;
	private TaskReport taskReport;
	public Documentable documentable;
	public ReportAction(RestState pageState) {
		super(pageState);
		// TODO Auto-generated constructor stub
		  this.logged = (TeamworkOperator) pageState.getLoggedOperator();
	}
	
   private void loadReportable() throws PersistenceException {
	    task = Task.load(restState.getEntry("TASK_ID").intValueNoErrorCodeNoExc() + "");
	    restState.setAttribute("REFERRAL_OBJECT",task);
   }

   

   public void editNoMake() throws PersistenceException {
     if (JSP.ex(restState.mainObjectId))
    	 taskReport=TaskReport.load(restState.mainObjectId);

     // se c'è già su document uso quello. robik 2015 04 03
     if (taskReport!=null) {
       if (taskReport.getTaskId()!= null) {
         restState.addClientEntry("TASK_ID", taskReport.getTaskId());
         restState.addClientEntry("DOCUMENT_PATH", taskReport.getDictionaryName());
         //restState.addClientEntry("", taskReport.getContent());
         
       } 
     }
     loadReportable();
   }
   
   
   public void cmdAddVersion() throws PersistenceException, SecurityException {
	    editNoMake();
	    taskReport.testPermission(logged, TeamworkPermissions.report_canCreate);
	    TaskReport oldVersion = this.taskReport;
	    restState.addClientEntry("REPORT_ROOT_ID", restState.mainObjectId);
	    TaskReport mainObject = new TaskReport();
	    mainObject.setIdAsNew();

	    mainObject.setVersion(String.valueOf(Integer.valueOf(oldVersion.getVersion())));
	    restState.addClientEntry("DOCUMENT_VERSION", mainObject.getVersion());

//	    mainObject.setType(oldVersion.getType());
//	    restState.addClientEntry("DOCUMENT_TYPE", mainObject.getType());

	    //clean file path
	    restState.removeEntry("sp_fi_br_DOCUMENT_UPLOAD_upl");

	    restState.setMainObject(mainObject);
	  }

   
   
   public void cmdAdd() throws SecurityException, PersistenceException{
	    editNoMake();
	    //testPermission(TeamworkPermissions.document_canCreate);
	    if (task!=null){
	      task.testPermission(logged, TeamworkPermissions.report_canCreate);
	    }  else {
	      throw new SecurityException(SecurityConstants.I18N_PERMISSION_LACKING,TeamworkPermissions.document_canCreate);
	    }

	    TaskReport mainObject = new TaskReport();
	    mainObject.setIdAsNew();
	    restState.addClientEntry("DOCUMENT_PATH", "/"+task.getCode()+"/");
	    restState.setMainObject(mainObject);
   }
   

   
   public void cmdAuthAndAdd(PageState pageState,HttpServletRequest request) throws SecurityException, PersistenceException, ApplicationException, ActionException{
	    editNoMake();
	    //testPermission(TeamworkPermissions.document_canCreate);
	    TaskReport reportToBeSaved;
	    boolean isNew = false;
	    pageState.addClientEntry(OperatorConstants.FLD_LOGIN_NAME, this.logged.getLoginName());
	   // pageState.addClientEntry(OperatorConstants.FLD_PWD, this.logged.getLoginName());
	    Boolean bo=OperatorAuth.authPassword(pageState, request);
	    if(!bo){
	    	pageState.removeEntry(OperatorConstants.FLD_LOGIN_NAME);
	    	pageState.removeEntry(OperatorConstants.FLD_PWD);
	    	this.restState.addMessageError("password is error,please input again!");
	        TaskReport mainObject = new TaskReport();
		    mainObject.setIdAsNew();
		   // restState.addClientEntry("DOCUMENT_PATH", "/"+task.getCode()+"/");
		    restState.setMainObject(mainObject);
		    restState.setError("0000000");
	    	return;
	    }
	    if (PersistenceHome.NEW_EMPTY_ID.equals(restState.mainObjectId)) {
	      if (task!=null){
	        task.testPermission(logged, TeamworkPermissions.report_canCreate);
	      }  else {
	        throw new SecurityException(SecurityConstants.I18N_PERMISSION_LACKING,TeamworkPermissions.document_canCreate);
	      }

	      reportToBeSaved = new TaskReport();
	      reportToBeSaved.setIdAsNew();
	      isNew = true;
	    } else {
	       taskReport.testPermission(logged,TeamworkPermissions.report_canWrite);
	       reportToBeSaved = this.taskReport;
	    }
	    
	    reportToBeSaved.setCreationDate(new Date());
	    String type=restState.getEntry("REPORT_TYPE_ID_txt").stringValueNullIfEmpty();
	    if(JSP.ex(type)&&(type.startsWith("WEEK")||type.equals("周报"))){
	    	Date[] s1=getWeekStartAndEnd(new Date());
	    	reportToBeSaved.setStartDate(s1[0]);
	    	reportToBeSaved.setEndDate(s1[1]);
	    	reportToBeSaved.setYear(DateUtils.dateToString(new Date(), 18));
	    	reportToBeSaved.setDictionaryName(restState.getEntry("DOCUMENT_PATH").stringValueNullIfEmpty());
	    }else if(JSP.ex(type)&&(type.startsWith("MONTH")||type.equals("月报"))){
	    	Date[] s1=getMonthStartAndEnd();
	    	reportToBeSaved.setStartDate(s1[0]);
	    	reportToBeSaved.setEndDate(s1[1]);
	    	reportToBeSaved.setDictionaryName(restState.getEntry("DOCUMENT_PATH").stringValueNullIfEmpty());
	    	reportToBeSaved.setYear(DateUtils.dateToString(new Date(), 18));
	    }else{
	    	reportToBeSaved.setYear(DateUtils.dateToString(new Date(), 18));
	    	reportToBeSaved.setStartDate(new Date());
	    	reportToBeSaved.setEndDate(new Date());
	    	reportToBeSaved.setDictionaryName(restState.getEntry("DOCUMENT_PATH").stringValueNullIfEmpty());
	    }
	    reportToBeSaved.setCreationDate(new Date());
	    restState.setMainObject(reportToBeSaved);
	  //  reportToBeSaved.setCode(restState.getEntry("DOCUMENT_CODE").stringValueNullIfEmpty());
	    try {
	      String reportTypeId = restState.getEntryAndSetRequired("REPORT_TYPE_ID").stringValue();
	      TaskReportType tt=(TaskReportType)PersistenceHome.findByPrimaryKey(TaskReportType.class, reportTypeId);
	      reportToBeSaved.setReportTypeId(reportTypeId);
	      reportToBeSaved.setTaskReportType(tt);
	    } catch (ActionException e) {
	    }
	    Company tt2=(Company)PersistenceHome.findByPrimaryKey(Company.class, restState.getEntry("REPORT_DEPART_ID").stringValue());
	    reportToBeSaved.setReportDepartId(restState.getEntry("REPORT_DEPART_ID").stringValueNullIfEmpty());
	    reportToBeSaved.setCompany(tt2);
	    String idArea = restState.getEntry("REPORT_AREA").stringValueNullIfEmpty();
	    if (idArea != null) {
	      Area area = (Area) PersistenceHome.findByPrimaryKey(Area.class, idArea);
	      reportToBeSaved.setArea(area);
	    }


	   reportToBeSaved.setContent(restState.getEntry("REPORT_CONTENT").stringValueNullIfEmpty());
	

	    if (restState.validEntries()) {
	      if (PersistenceHome.NEW_EMPTY_ID.equals(restState.mainObjectId)) {
	  	    reportToBeSaved.setCreator(logged.getDisplayName());
		    reportToBeSaved.setOwner(logged);
	      }else{
	  	    reportToBeSaved.setCreator(logged.getDisplayName());
		    reportToBeSaved.setOwner(logged);
	      }

	      PersistentFile persistentFile = reportToBeSaved.getFile();
		    if (persistentFile == null) {
		        persistentFile = new PersistentFile(0, null,PersistentFile.DEFAULT_STORAGE_TYPE);
		    }
	    persistentFile.fileDir=reportToBeSaved.getDictionaryName();
	    reportToBeSaved.setFile(Uploader.save(reportToBeSaved, persistentFile, "DOCUMENT_UPLOAD", restState));
	    reportToBeSaved.setOriginalFileName(persistentFile.getOriginalFileName());
	    reportToBeSaved.setVersion("1");
	      //task & resource managament
	      if (task!=null) {
	    	reportToBeSaved.setTask(task);
	    	reportToBeSaved.setTaskId(task.getId().toString());
	    	reportToBeSaved.store();
	        ReportAction.generateReportEvent(reportToBeSaved, logged);

	      } else {
	    	  reportToBeSaved.store();
	      }

	      Hit.getInstanceAndStore(reportToBeSaved, logged, .2);

	      // ok message feedback
	      if (isNew)
	        restState.addMessageOK(I18n.get("REPORT_CORRECTLY_CREATED"));
	      else
	        restState.addMessageOK(I18n.get("REPORT_CORRECTLY_SAVED"));

	    }
  }
   public void cmdEdit() throws PersistenceException, SecurityException {
	    editNoMake();
	   // document.testPermission(logged,TeamworkPermissions.document_canRead);

	    restState.setMainObject(taskReport);
	    make(taskReport);
	    Hit.getInstanceAndStore(taskReport, logged, .1);
	}
   public void cmdMkdir() throws PersistenceException, SecurityException {
	    editNoMake();
	   // document.testPermission(logged,TeamworkPermissions.document_canRead);
	   
	    if(JSP.ex(restState.getEntry("DIR_NAME"))){
	    	 String regPath=restState.getEntry("DIR_NAME").stringValueNullIfEmpty();
	    	 restState.addClientEntry("DOCUMENT_PATH", restState.getEntry("DOCUMENT_PATH").stringValueNullIfEmpty()+regPath+"/");
	    }
	    restState.removeEntry("DIR_NAME");
        if(taskReport!=null&&taskReport.getId()!=null){
        	restState.setMainObject(taskReport);
      	    make(taskReport);
      	    Hit.getInstanceAndStore(taskReport, logged, .1);
        }else{
        	TaskReport mainObject = new TaskReport();
     	    mainObject.setIdAsNew();
     	    restState.setMainObject(mainObject);
        }
	    
	}
   
   public void make(TaskReport taskReport) throws PersistenceException {
	    restState.addClientEntry("REPORT_TYPE_ID", taskReport.getReportTypeId());
	    restState.addClientEntry("REPORT_DEPART_ID", taskReport.getReportDepartId());
	    restState.addClientEntry("REPORT_DICTIONARY_NAME", taskReport.getDictionaryName());
	    restState.addClientEntry("REPORT_CONTENT", taskReport.getContent());
	    if(taskReport.getFile()!=null){
	    	 ClientEntry uplCe = new ClientEntry("DOCUMENT_UPLOAD", taskReport.getFile().serialize());
	 	    restState.addClientEntry(uplCe);
	    }
	   
	      
	  }
   
   
  private Date[] getWeekStartAndEnd(Date date){
	  Date[] darray=new Date[2];
	  Calendar cal = Calendar.getInstance();		
	  cal.setTime(date);		
	  int d = 0;		
	  if(cal.get(Calendar.DAY_OF_WEEK)==1){		
		  d = -6;		
	  }else{			
			  d = 2-cal.get(Calendar.DAY_OF_WEEK);		
	 }	
	  cal.add(Calendar.DAY_OF_WEEK, d);		
	  //所在周开始日期		
	  darray[0]=cal.getTime();
	//.cal.  System.out.println(new SimpleDateFormat("yyyy-MM-dd").format(cal.getTime()));		
	  cal.add(Calendar.DAY_OF_WEEK, 6);		//所在周结束日期		
	  darray[1]=cal.getTime();
	  return darray;
	// .cal. System.out.println(new SimpleDateFormat("yyyy-MM-dd").format(cal.getTime()));
  }
  
  
  public Date[] getMonthStartAndEnd(){
	  Date[] darray=new Date[2];
	   //获取当前月第一天：
      Calendar c = Calendar.getInstance();    
      c.add(Calendar.MONTH, 0);
      c.set(Calendar.DAY_OF_MONTH,1);//设置为1号,当前日期既为本月第一天 
      darray[0]=c.getTime();

      
      //获取当前月最后一天
      Calendar ca = Calendar.getInstance();    
      ca.set(Calendar.DAY_OF_MONTH, ca.getActualMaximum(Calendar.DAY_OF_MONTH));  
      darray[0]=ca.getTime();
      return darray;
  }
   
   public void cmdSave() throws PersistenceException, ActionException, ApplicationException, SecurityException {
	    editNoMake();
	    TaskReport reportToBeSaved;
	    boolean isNew = false;
	    if (PersistenceHome.NEW_EMPTY_ID.equals(restState.mainObjectId)) {
	      if (task!=null){
	        task.testPermission(logged, TeamworkPermissions.report_canCreate);
	      }  else {
	        throw new SecurityException(SecurityConstants.I18N_PERMISSION_LACKING,TeamworkPermissions.document_canCreate);
	      }

	      reportToBeSaved = new TaskReport();
	      reportToBeSaved.setIdAsNew();
	      isNew = true;
	    } else {
	     taskReport.testPermission(logged,TeamworkPermissions.report_canWrite);
	      reportToBeSaved = this.taskReport;
	    }
	    
	   

	    reportToBeSaved.setCreationDate(new Date());
	    String type=restState.getEntry("REPORT_TYPE_ID_txt").stringValueNullIfEmpty();
	    if(JSP.ex(type)&&(type.startsWith("WEEK")||type.equals("周报"))){
	    	Date[] s1=getWeekStartAndEnd(new Date());
	    	reportToBeSaved.setStartDate(s1[0]);
	    	reportToBeSaved.setEndDate(s1[1]);
	    	reportToBeSaved.setYear(DateUtils.dateToString(new Date(), 18));
	    	reportToBeSaved.setDictionaryName(restState.getEntry("DOCUMENT_PATH").stringValueNullIfEmpty());
	    }else if(JSP.ex(type)&&(type.startsWith("MONTH")||type.equals("月报"))){
	    	Date[] s1=getMonthStartAndEnd();
	    	reportToBeSaved.setStartDate(s1[0]);
	    	reportToBeSaved.setEndDate(s1[1]);
	    	reportToBeSaved.setDictionaryName(restState.getEntry("DOCUMENT_PATH").stringValueNullIfEmpty());
	    	reportToBeSaved.setYear(DateUtils.dateToString(new Date(), 18));
	    }else{
	    	reportToBeSaved.setYear(DateUtils.dateToString(new Date(), 18));
	    	reportToBeSaved.setStartDate(new Date());
	    	reportToBeSaved.setEndDate(new Date());
	    	reportToBeSaved.setDictionaryName(restState.getEntry("DOCUMENT_PATH").stringValueNullIfEmpty());
	    }
	    reportToBeSaved.setVersion("1");
	    reportToBeSaved.setCreationDate(new Date());
	    restState.setMainObject(reportToBeSaved);
	  //  reportToBeSaved.setCode(restState.getEntry("DOCUMENT_CODE").stringValueNullIfEmpty());
	    try {
	      String reportTypeId = restState.getEntryAndSetRequired("REPORT_TYPE_ID").stringValue();
	      TaskReportType tt=(TaskReportType)PersistenceHome.findByPrimaryKey(TaskReportType.class, reportTypeId);
	      reportToBeSaved.setReportTypeId(reportTypeId);
	      reportToBeSaved.setTaskReportType(tt);
	    } catch (ActionException e) {
	    }
	    Company tt2=(Company)PersistenceHome.findByPrimaryKey(Company.class, restState.getEntry("REPORT_DEPART_ID").stringValue());
	    reportToBeSaved.setReportDepartId(restState.getEntry("REPORT_DEPART_ID").stringValueNullIfEmpty());
	    reportToBeSaved.setCompany(tt2);
	    String idArea = restState.getEntry("REPORT_AREA").stringValueNullIfEmpty();
	    if (idArea != null) {
	      Area area = (Area) PersistenceHome.findByPrimaryKey(Area.class, idArea);
	      reportToBeSaved.setArea(area);
	    }


	   reportToBeSaved.setContent(restState.getEntry("REPORT_CONTENT").stringValueNullIfEmpty());
	

	    if (restState.validEntries()) {
	      if (PersistenceHome.NEW_EMPTY_ID.equals(restState.mainObjectId)) {
	  	    reportToBeSaved.setCreator(logged.getDisplayName());
		    reportToBeSaved.setOwner(logged);
	      }else{
	  	    reportToBeSaved.setCreator(logged.getDisplayName());
		    reportToBeSaved.setOwner(logged);
	      }

	      PersistentFile persistentFile = reportToBeSaved.getFile();
		    if (persistentFile == null) {
		        persistentFile = new PersistentFile(0, null,PersistentFile.DEFAULT_STORAGE_TYPE);
		    }
	    persistentFile.fileDir=reportToBeSaved.getDictionaryName();
	    reportToBeSaved.setFile(Uploader.save(reportToBeSaved, persistentFile, "DOCUMENT_UPLOAD", restState));
	    reportToBeSaved.setOriginalFileName(persistentFile.getOriginalFileName());
	    
	      //task & resource managament
	      if (task!=null) {
	    	reportToBeSaved.setTask(task);
	    	reportToBeSaved.setTaskId(task.getId().toString());
	    	reportToBeSaved.store();
	        ReportAction.generateReportEvent(reportToBeSaved, logged);

	      } else {
	    	  reportToBeSaved.store();
	      }

	      Hit.getInstanceAndStore(reportToBeSaved, logged, .2);

	      // ok message feedback
	      if (isNew)
	        restState.addMessageOK(I18n.get("REPORT_CORRECTLY_CREATED"));
	      else
	        restState.addMessageOK(I18n.get("REPORT_CORRECTLY_SAVED"));

	    }
	  }

   
   public static void generateReportEvent(TaskReport report, TeamworkOperator logged) throws StoreException {
	    //generate event
	    // add the message to the queue if there is somebody waiting for that notification.
	    // event is generated for task-document only
	    if (report.getTask() != null) {
	      SomethingHappened change = new SomethingHappened();
	      change.setIdAsNew();
	      change.setEventType(Task.Event.TASK_REPORT_ADDED + "");
	      change.getMessageParams().put("SUBJECT", JSP.limWr(report.getTask().getDisplayName(), 30));

	      change.setMessageTemplate(Task.Event.TASK_REPORT_ADDED + "_MESSAGE_TEMPLATE");

	      change.getMessageParams().put("task", report.getTask().getDisplayName());
	      change.getMessageParams().put("documentTitle", JSP.w(report.getName()));
	      change.setWhoCausedTheEvent(logged);

	      PageSeed ps = new PageSeed(ApplicationState.serverURL + "/applications/teamwork/task/taskReportList.jsp");
	      ps.setCommand("LIST_REPORT");
	      ps.addClientEntry("REPORT_ID",report.getId());
	      ps.addClientEntry("TASK_ID", report.getTask().getId());

	      ButtonLink edit = new ButtonLink(ps);
	      edit.label = report.getTask().getDisplayName();
	      change.setLink(edit.toPlainLink());
	      change.setIdentifiable(report.getTask());
	      change.store();
	    }

	  }
   
   
   public void cmdDelete() throws SecurityException, PersistenceException {
	    editNoMake();
	    taskReport.testPermission(logged, TeamworkPermissions.report_canDelete);
	    DeleteHelper.cmdDelete(taskReport, restState);
	 }

   
   public void cmdFind() throws PersistenceException, SecurityException {
	   editNoMake();
	    //search for default filter
	    if (restState.getCommand() == null) {
	      if (!PersistentSearch.feedFromDefaultSearch("REPORTFILTER", restState))
	        // when not set use last changed docs
	        restState.addClientEntry(Fields.FLD_FILTER_NAME, "PF_REPORTS_RECENTLY_CHANGED");
	    }
	    TeamworkOperator logged = (TeamworkOperator) restState.getLoggedOperator();

	    String hql = "select distinct report.id from " + TaskReport.class.getName() + " as report";

	    //this is used for pre-cooked queries
	    String additionalSort = null;

	    QueryHelper qhelp = new QueryHelper(hql);
	    boolean recoveredFromSavedFilter = false;
	    boolean isPresetFilter = false;
	    if (restState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty() != null)
	      if (restState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().startsWith("PF_"))
	        isPresetFilter = true;

	    if (!isPresetFilter) {
	      recoveredFromSavedFilter = PersistentSearch.feedFromSavedSearch(restState);
	    } else {
	      // uso di un filtro presettato
	      recoveredFromSavedFilter = true;
	      if (restState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty() != null) {
	        String cmd = restState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty();
	        restState.getClientEntries().getClientEntries().clear();

	        restState.addClientEntry(Fields.FLD_FILTER_NAME, cmd);
	      }
	    }

	    boolean somethingSearched = recoveredFromSavedFilter;

	    String filter = restState.getEntry("REPORT_TYPE_ID").stringValueNullIfEmpty();
	    if (filter != null) {
	      qhelp.addQBEORClauses(
	              filter,
	              qhelp.getOrElement("report.reportTypeId", "reportTypeId", QueryHelper.TYPE_CHAR)
	      );

	      somethingSearched = true;
	    }
	    
	    String filter2 = restState.getEntry("REPORT_YEAR_txt").stringValueNullIfEmpty();
	    if (filter2 != null) {
	      qhelp.addQBEORClauses(
	    		  filter2,
	              qhelp.getOrElement("report.year", "year", QueryHelper.TYPE_CHAR)
	      );

	      somethingSearched = true;
	    }
	    String filter3 = restState.getEntry("REPORT_DIC_NAME_txt").stringValueNullIfEmpty();
	    if (filter3 != null) {
	      qhelp.addQBEORClauses(
	    		  filter3,
	              qhelp.getOrElement("report.dictionaryName", "dictionaryName", QueryHelper.TYPE_CHAR)
	      );

	      somethingSearched = true;
	    }

	    filter = restState.getEntry("REPORT_DEPART_ID").stringValueNullIfEmpty();
	    if (filter != null) {
	      qhelp.addQBEORClauses(
	              filter,
	              qhelp.getOrElement("report.reportDepartId", "reportDepartId", QueryHelper.TYPE_CHAR)
	      );

	      somethingSearched = true;
	    }

	    //ActionUtilities.addQBEClause("TASK_CODE","task.code","tskCode",qhelp,QueryHelper.TYPE_CHAR,restState);

	    if (restState.getEntry("TASK_TYPE").intValueNoErrorCodeNoExc() > 0) {
	      ActionUtilities.addOQLClause("TASK_TYPE", "task.type.id", "typeId", qhelp, QueryHelper.TYPE_INT, restState);
	    }


//
//	    String lastModified = restState.getEntry("DOCUMENT_LAST_MODIFIED").stringValueNullIfEmpty();
//	    if (lastModified != null) {
//	      qhelp.addQBEClause("report.lastModified", "modified", lastModified, QueryHelper.TYPE_DATE);
//	      somethingSearched = true;
//	    }

//	    String value = restState.getEntry("DOCUMENT_AUTHOR").stringValueNullIfEmpty();
//	    if (value != null) {
//	      qhelp.addQBEClause("report.author", "author", value, QueryHelper.TYPE_CHAR);
//	      somethingSearched = true;
//	    }

//	    value = restState.getEntry("task").stringValueNullIfEmpty();
//	    if (value != null) {
//	      qhelp.addOQLClause("report.taskId=:taskId", "taskId", value);
//	      somethingSearched = true;
//	    }

	    String value = restState.getEntry("TASK_ID").stringValueNullIfEmpty();
	    if (value != null) {
	      qhelp.addOQLClause("report.taskId=:taskId", "taskId", value);
	      somethingSearched = true;
	    }
	    value = restState.getEntry("UPLOAD_START_DATE").stringValueNullIfEmpty();
	    if (value != null) {
	      qhelp.addOQLClause("to_char(report.creationDate,'YYYY-MM-dd')=:createDate", "createDate", value);
	      somethingSearched = true;
	    }

	    

	    
	    
	  

//	    if (!somethingSearched && Commands.FIND.equals(restState.getCommand())) {
//	      qhelp.addQBEClause("doc.name", "docname", "*", QueryHelper.TYPE_CHAR);
//	      somethingSearched = true;
//	    }

	    if (!logged.hasPermissionAsAdmin()) {
	/*
	________________________________________________________________________________________________________________________________________________________________________


	  begin security

	________________________________________________________________________________________________________________________________________________________________________

	*/

	      //take care that this alias is used also out of the method e.g. in search
	     qhelp.addJoinAlias(" left outer join task.assignments as assignment");


	      qhelp.addJoinAlias(" left outer join report.task  as task");
	   //   qhelp.addJoinAlias(" left outer join report.resource  as resource");

	      // TASK CLAUSES

	      //open unique clause and set owner; qhelp puts the "and" before; all following clauses are in or
	      qhelp.addOQLClause("( ( task.owner = :logged", "logged", logged);

	      //areas
	      Set<Area> areas = logged.getAreasForPermission(TeamworkPermissions.document_canRead);
	      if (areas.size() > 0) {
	        qhelp.addOrQueryClause("task.area in (:areas)");
	        qhelp.addParameter("areas", areas);
	     //   qhelp.addOrQueryClause("resource.area in (:areasR)");
	    //    qhelp.addParameter("areasR", areas);
	      }
	      //else
	      //  throw new SecurityException(SecurityConstants.I18N_PERMISSION_LACKING, TeamworkPermissions.document_canRead);

	      //assignments
	      Person myPerson = logged.getPerson();
	      if (myPerson != null) {

	        List<Resource> myAncs = myPerson.getAncestors();

	        OqlQuery oqlQuery = new OqlQuery(
	                " select distinct role from " + Assignment.class.getName() + " as ass join ass.role as role where role.permissionIds like :docRead and " +
	                        "ass.resource in (:myAncs)");

	        oqlQuery.getQuery().setParameterList("myAncs", myAncs);
	        oqlQuery.getQuery().setString("docRead", "%" + TeamworkPermissions.document_canRead.toString() + "%");

	        List<Role> roles = oqlQuery.list();

	        if (roles.size() > 0) {
	          qhelp.addOrQueryClause("assignment.role in (:assigRoles) and assignment.resource = :myself");
	          qhelp.addParameter("myself", myPerson);
	          qhelp.addParameter("assigRoles", roles);
	        }
	      }

	      //in order to keep all security conditions in a unique and clause
	      qhelp.addToHqlString(" ) or task is null ");

	      // RESOURCE CLAUSES
	      //open unique clause and set owner; qhelp puts the "and" before; all following clauses are in or
	    //  qhelp.addOQLClause("( resource.owner = :logged", "logged", logged);

	      //in order to keep all security conditions in a unique and clause
	  //    qhelp.addToHqlString(")");

	      //end security big clause
	      qhelp.addToHqlString(")");

	/*
	________________________________________________________________________________________________________________________________________________________________________


	  end security

	________________________________________________________________________________________________________________________________________________________________________

	*/
	    }


	    qhelp.wrapHql("select report from " + TaskReport.class.getName() + " as report where report.id in (", ")");

	    if (somethingSearched) {
	      DataTable.orderAction(qhelp, "REPORTLST", restState, JSP.ex(additionalSort) ? additionalSort : "creationDate desc");
	      restState.setPage(HibernatePage.getHibernatePageInstance(qhelp.toHql().getQuery(), Paginator.getWantedPageNumber(restState), Paginator.getWantedPageSize("REPORTLST", restState)));
	     // System.out.println(restState.getPage().getTotalNumberOfElements());
	    }else{
	       DataTable.orderAction(qhelp, "REPORTLST", restState, JSP.ex(additionalSort) ? additionalSort : "creationDate desc");
		   restState.setPage(HibernatePage.getHibernatePageInstance(qhelp.toHql().getQuery(), Paginator.getWantedPageNumber(restState), Paginator.getWantedPageSize("REPORTLST", restState)));
		  
	    }

	  }

 
   
}
