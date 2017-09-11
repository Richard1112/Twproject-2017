<%@page import="com.twproject.task.TaskAuditSubject"%>
<%@ page
	import="java.util.ArrayList,com.twproject.task.TaskAudit,
	com.twproject.task.TaskAuditSubject,
	com.twproject.task.TaskAuditReview,com.twproject.task.TaskAuditStatus,
	com.twproject.task.TaskAuditLog,com.twproject.task.TaskAuditType,com.twproject.document.TeamworkDocument, 
	com.twproject.document.businessLogic.DocumentAction, com.twproject.operator.TeamworkOperator, 
	com.twproject.resource.Resource, com.twproject.resource.ResourceBricks, 
	com.twproject.security.RoleTeamwork, com.twproject.security.TeamworkPermissions, 
	com.twproject.task.Assignment, com.twproject.task.Task, com.twproject.task.businessLogic.AssignmentAction, 
	com.twproject.task.businessLogic.TaskAction, com.twproject.task.financial.Cost, 
	com.twproject.task.financial.CostClassification, net.sf.json.JSONArray, net.sf.json.JSONObject, 
	org.jblooming.agenda.Period, org.jblooming.designer.DesignerField, org.jblooming.ontology.PersistentFile, 
	org.jblooming.ontology.SerializedMap, org.jblooming.persistence.PersistenceHome, org.jblooming.utilities.JSP, 
	org.jblooming.utilities.NumberUtilities, org.jblooming.waf.ActionUtilities, org.jblooming.waf.JSONHelper, 
	org.jblooming.waf.html.input.Uploader, org.jblooming.waf.view.ClientEntry, org.jblooming.waf.view.PageState, 
	org.jblooming.waf.view.RestState, java.util.Date, com.twproject.task.TaskDataHistory, java.util.List, 
	org.jblooming.waf.settings.I18n, java.util.HashSet, java.util.Set, com.twproject.task.TaskBricks, 
	org.jblooming.oql.QueryHelper, org.jblooming.agenda.CompanyCalendar, org.jblooming.messaging.MessagingSystem,
	 org.jblooming.waf.constants.Fields, org.jblooming.tracer.Tracer, org.jblooming.waf.constants.OperatorConstants, 
	 net.wimpi.pim.util.Base64, java.io.InputStream, java.io.ByteArrayInputStream, org.jblooming.utilities.StringUtilities, 
	 java.io.File, org.jblooming.waf.settings.ApplicationState, java.io.FileOutputStream, 
	 java.awt.Insets,
java.io.File,
java.io.IOException,
java.net.MalformedURLException,
java.net.URL,
java.security.InvalidParameterException,
org.zefer.pd4ml.PD4Constants,
org.zefer.pd4ml.PD4ML,
org.jblooming.utilities.DateUtilities,
org.jblooming.system.SystemConstants,
	 org.jblooming.utilities.file.FileUtilities"%>
<%
	PageState pageState = PageState.getCurrentPageState(request);
	TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

	JSONHelper jsonHelper = new JSONHelper();
	JSONObject json = jsonHelper.json;

	try {
		Resource loggedRes = logged.getPerson();

		TaskAction taskAction = new TaskAction(pageState);
		// ---------------------------------------- task change audit submit  ----------------------------------------
		if ("NEWADC".equals(pageState.command)) {
			pageState.initializeEntries("cell");
			Task task = Task.load(pageState.getEntry("taskId").intValueNoErrorCodeNoExc() + "");
			if (task != null) {

				JSONObject ass = JSONObject.fromObject(pageState.getEntry("ass").stringValueNullIfEmpty());

				TaskAudit taskAudit = new TaskAudit();
				String auditId = ass.getString("auditId");
				
				if (auditId != null && !"".equals(auditId) && !"null".equals(auditId)) {
					taskAudit = TaskAudit.load(auditId);
				}

				taskAudit.setTitle(TaskAuditSubject.load(ass.getString("title")));
				taskAudit.setContent(ass.getString("content"));
				taskAudit.setReportor(logged.getPerson());
				taskAudit.setReviewer(Resource.load(ass.getString("reviewer")));
				taskAudit.setType(TaskAuditType.load(ass.getString("auditType")));
				taskAudit.setAuditStatus(TaskAuditStatus.loadByCode(1));
				taskAudit.setTask(task);
				taskAudit.setIsClosed(1);
				taskAudit.setAuditLevel(0);

				taskAudit.store();

				//save reviewers
				String reviewers = ass.getString("reviewers");
				JSONArray ids = JSONArray.fromObject(reviewers);
				for (Object o : ids) {
					JSONObject id = (JSONObject) o;
					
					TaskAuditReview review = TaskAuditReview.loadByReviewer(id.getString("id"), taskAudit.getId().toString());
					if (review==null){
						review = new TaskAuditReview();
					}
					review.setAuditStatus(TaskAuditStatus.loadByCode(1));
					review.setMainAudit(taskAudit);
					review.setReviewer(Resource.load(id.getString("id")));
					review.store();
				}
				//save log
				TaskAuditLog log = new TaskAuditLog();
				log.setMainAudit(taskAudit);
				log.setSubmituser(logged.getPerson());
				log.setSubmitdate(new Date());
				//log.setAudituser(logged.getPerson());
				log.setAuditdate(new Date());
				log.setAuditStatus(TaskAuditStatus.loadByCode(1));
				log.setContent(I18n.get("RE_AUDIT").replace("${level}", "1"));

				log.store();
				//send message

				/* RestState rs= new RestState(logged);
				rs.setMainObjectId(PersistenceHome.NEW_EMPTY_ID);
				rs.addClientEntry("TASK_ID", task.getId());
				rs.addClientEntry("DOCUMENT_NAME", subject);
				rs.addClientEntry("DOCUMENT_AUTHORED", new Date());
				rs.addClientEntry("DOCUMENT_AUTHOR", logged.getDisplayName());
				rs.addClientEntry("DOCUMENT_AREA", task.getArea());
				rs.addClientEntry("DOCUMENT_TYPE", TeamworkDocument.IS_CONTENT);
				rs.addClientEntry("SUMMA", body+ "<br><small>"+I18n.get("MESSAGE_SENT_TO")+": "+sendTo+"</small>");
				new DocumentAction(rs).cmdSave();
				
				org.jblooming.messaging.Message twMessage = new org.jblooming.messaging.Message();
				
				twMessage.setToOperator(Resource.load(ass.getString("reportor")).getMyself());
				  if (logged != null)
				  	twMessage.setFromOperator(logged);
				  twMessage.setSubject(I18n.get("SUBMIT_AUDIT"));
				  twMessage.setDefaultExpires();
				  twMessage.setMedia("DIGEST");
				  twMessage.setMessageBody("you have a message");
				  org.jblooming.waf.view.PageSeed ps = new org.jblooming.waf.view.PageSeed(ApplicationState.serverURL + "/applications/teamwork/task/taskAudit.jsp");
				  ps.setCommand(org.jblooming.waf.constants.Commands.EDIT);
				  ps.setMainObjectId(taskAudit.getId());
				  org.jblooming.waf.html.button.ButtonLink editLink = org.jblooming.waf.html.button.ButtonLink.getTextualInstance(I18n.get("SUBMIT_AUDIT"), ps);
				
				  twMessage.setLink(editLink.toPlainLink());
				  twMessage.store();
				
				  String warning = I18n.get("ICAL_NEW_PROPOSAL_NOTIFIED"); */
				// try {
				//if (message!=null)
				//org.jblooming.messaging.MailHelper.mailToUrl("alex.wang@tech-coffee.com","wjsdt1234@126.com","test",warning);
				//org.jblooming.messaging.MailHelper.replyToMessage(message, warning);
				//} catch (javax.mail.MessagingException e) {
				//	  org.jblooming.tracer.Tracer.platformLogger.error(e);
				//}
			}
			json.element("command", pageState.command);
			// --------------------------- resubmit --------------------------------
		} else if ("READC".equals(pageState.command)) {
			pageState.initializeEntries("cell");
			Task task = Task.load(pageState.getEntry("taskId").intValueNoErrorCodeNoExc() + "");
			if (task != null) {

				JSONObject ass = JSONObject.fromObject(pageState.getEntry("ass").stringValueNullIfEmpty());

				TaskAudit taskAudit = new TaskAudit();
				String auditId = ass.getString("auditId");
				if (auditId != null && !"".equals(auditId)) {
					taskAudit = TaskAudit.load(auditId);
				}
				int level = taskAudit.getAuditLevel();
				level++;
				//taskAudit.setReportor(logged.getPerson());
				taskAudit.setAuditStatus(TaskAuditStatus.loadByCode(1));
				taskAudit.setCreationDate(new Date());
				taskAudit.setAuditLevel(level);
				taskAudit.store();

				//save reviewers
				String reviewers = ass.getString("reviewers");
				JSONArray ids = JSONArray.fromObject(reviewers);
				for (Object o : ids) {
					JSONObject id = (JSONObject) o;
					TaskAuditReview review = TaskAuditReview.loadByReviewer(id.getString("id"), auditId);
					if (review==null){
						review = new TaskAuditReview();
					}
					review.setAuditStatus(TaskAuditStatus.loadByCode(1));
					review.setMainAudit(taskAudit);
					review.setReviewer(Resource.load(id.getString("id")));
					review.setAuditLevel(level);
					review.store();
				}

				//save log
				TaskAuditLog log = new TaskAuditLog();
				log.setMainAudit(taskAudit);
				log.setContent(I18n.get("RE_AUDIT").replace("${level}", level + 1 + ""));
				log.setSubmituser(logged.getPerson());
				log.setSubmitdate(new Date());
				//log.setAudituser(logged.getPerson());
				log.setAuditdate(new Date());
				log.setAuditStatus(TaskAuditStatus.loadByCode(1));
				log.setAuditLevel(level);

				log.store();
			}
			json.element("command", pageState.command);
			// --------------------------- pass audit --------------------------------
		} else if ("RSADC".equals(pageState.command)) {
			pageState.initializeEntries("cell");
			Task task = Task.load(pageState.getEntry("taskId").intValueNoErrorCodeNoExc() + "");
			if (task != null) {

				JSONObject ass = JSONObject.fromObject(pageState.getEntry("ass").stringValueNullIfEmpty());

				TaskAudit taskAudit = new TaskAudit();
				String auditId = ass.getString("auditId");
				if (auditId != null && !"".equals(auditId)) {
					taskAudit = TaskAudit.load(auditId);
				}
				int level = taskAudit.getAuditLevel();
				level++;
				//taskAudit.setReportor(logged.getPerson());
				taskAudit.setCreationDate(new Date());
				taskAudit.setAuditLevel(level);
				taskAudit.store();

				//save reviewers
				String reviewers = ass.getString("reviewers");
				JSONArray ids = JSONArray.fromObject(reviewers);
				for (Object o : ids) {
					JSONObject id = (JSONObject) o;
					TaskAuditReview review = TaskAuditReview.loadByReviewer(id.getString("id"), auditId);
					if (review==null){
						review = new TaskAuditReview();
					}
					review.setAuditStatus(TaskAuditStatus.loadByCode(1));
					review.setMainAudit(taskAudit);
					review.setReviewer(Resource.load(id.getString("id")));
					review.setAuditLevel(level);
					review.store();
				}

				//save log
				TaskAuditLog log = new TaskAuditLog();
				log.setMainAudit(taskAudit);
				log.setContent(I18n.get("RE_AUDIT").replace("${level}", level + 1 + ""));
				log.setSubmituser(logged.getPerson());
				log.setSubmitdate(new Date());
				//log.setAudituser(logged.getPerson());
				log.setAuditdate(new Date());
				log.setAuditStatus(TaskAuditStatus.loadByCode(1));
				log.setAuditLevel(level);

				log.store();
			}
			json.element("command", pageState.command);
			// --------------------------- pass audit --------------------------------
		} else if ("PASS".equals(pageState.command)) {
			pageState.initializeEntries("cell");
			Task task = Task.load(pageState.getEntry("taskId").intValueNoErrorCodeNoExc() + "");
			//if (task != null) {
			JSONObject ass = JSONObject.fromObject(pageState.getEntry("ass").stringValueNullIfEmpty());

			String mainId = ass.getString("auditId");
			TaskAudit taskAudit = TaskAudit.load(mainId);
			if (taskAudit != null) {
				if ("1".equals(taskAudit.getType().getId().toString())) {
					taskAudit.setAuditStatus(TaskAuditStatus.loadByCode(3));
					taskAudit.store();

					TaskAuditReview tv = TaskAuditReview.loadByReviewer(logged.getPerson().getId().toString(),
							taskAudit.getId().toString());
					tv.setAuditStatus(TaskAuditStatus.loadByCode(3));
					tv.store();
				} else {
					List<TaskAuditReview> lv = taskAudit.getReviewers();
					if (lv != null) {
						int count = 0;
						for (TaskAuditReview v : lv) {
							if (v.getAuditStatus().getIntValue() == 3)
								count++;
						}
						if (count == lv.size() - 1) {
							taskAudit.setAuditStatus(TaskAuditStatus.loadByCode(3));
							taskAudit.store();
						}
						TaskAuditReview tv = taskAudit.getCurrentReviewer(taskAudit.getReviewers(),
								logged.getPerson().getId().toString(), taskAudit.getIntId());
						tv.setAuditStatus(TaskAuditStatus.loadByCode(3));
						tv.store();
					}
				}

				//save log
				TaskAuditLog log = new TaskAuditLog();
				log.setMainAudit(taskAudit);
				log.setContent(ass.getString("sujection"));
				log.setSubmituser(taskAudit.getReportor());
				log.setSubmitdate(new Date());
				log.setAudituser(logged.getPerson());
				log.setAuditdate(new Date());
				log.setAuditStatus(TaskAuditStatus.loadByCode(3));

				log.store();

				//send message
			}
			//}
			json.element("command", pageState.command);
		} else if ("REJECT".equals(pageState.command)) {
			pageState.initializeEntries("cell");
			Task task = Task.load(pageState.getEntry("taskId").intValueNoErrorCodeNoExc() + "");
			//if (task != null) {
			JSONObject ass = JSONObject.fromObject(pageState.getEntry("ass").stringValueNullIfEmpty());

			String mainId = ass.getString("auditId");
			TaskAudit taskAudit = TaskAudit.load(mainId);
			if (taskAudit != null) {
				//taskAudit.setAuditStatus(TaskAuditStatus.loadByCode(1));
				//taskAudit.store();

				if ("1".equals(taskAudit.getType().getId().toString())) {
					taskAudit.setAuditStatus(TaskAuditStatus.loadByCode(2));
					taskAudit.store();

					TaskAuditReview tv = TaskAuditReview.loadByReviewer(logged.getPerson().getId().toString(),
							taskAudit.getId().toString());
					tv.setAuditStatus(TaskAuditStatus.loadByCode(2));
					tv.store();
				} else {
					List<TaskAuditReview> lv = taskAudit.getReviewers();
					if (lv != null) {
						int count = 0;
						for (TaskAuditReview v : lv) {
							if (v.getAuditStatus().getIntValue() == 2)
								count++;
						}
						if (count == lv.size()-1) {
							taskAudit.setAuditStatus(TaskAuditStatus.loadByCode(2));
							taskAudit.store();
						}
						TaskAuditReview tv = taskAudit.getCurrentReviewer(taskAudit.getReviewers(),
								logged.getPerson().getId().toString(), taskAudit.getIntId());
						tv.setAuditStatus(TaskAuditStatus.loadByCode(2));
						tv.store();
					}
				}

				//save log
				TaskAuditLog log = new TaskAuditLog();
				log.setMainAudit(taskAudit);
				log.setContent(ass.getString("sujection"));
				log.setSubmituser(taskAudit.getReportor());
				log.setSubmitdate(new Date());
				log.setAudituser(logged.getPerson());
				log.setAuditdate(new Date());
				log.setAuditStatus(TaskAuditStatus.loadByCode(2));

				log.store();

				//send message
			}
			//}
			json.element("command", pageState.command);
			// --------------------------- callback audit --------------------------------
		} else if ("CALLBACK".equals(pageState.command)) {
			pageState.initializeEntries("cell");

			String auditId = pageState.getEntry("auditId").stringValueNullIfEmpty();
			TaskAudit taskAudit = new TaskAudit();
			if (auditId != null && !"".equals(auditId)) {
				taskAudit = TaskAudit.load(auditId);
			}
			taskAudit.setAuditStatus(TaskAuditStatus.loadByCode(0));
			taskAudit.store();

			//delete reviewers
			List<TaskAuditReview> rList = taskAudit.getReviewers();
			if (rList != null) {
				for (TaskAuditReview review : rList) {
					review.remove();
				}
			}
			//send message

			json.element("command", pageState.command);
		} else if ("CLOSE".equals(pageState.command)) {
			pageState.initializeEntries("cell");
			String auditId = pageState.getEntry("auditId").stringValueNullIfEmpty();
			TaskAudit taskAudit = new TaskAudit();
			if (auditId != null && !"".equals(auditId)) {
				taskAudit = TaskAudit.load(auditId);
			}
			taskAudit.setIsClosed(2);
			taskAudit.store();

			//send message
			json.element("command", pageState.command);
		} else if ("RMIMG".equals(pageState.command)) {
			
			TaskAuditSubject subject = TaskAuditSubject.load(pageState.getEntry("SUB_ID").intValueNoErrorCodeNoExc() + "");
			if (subject != null) {
				PersistentFile oldPhoto = subject.getPicture();
				if (oldPhoto != null)
					oldPhoto.delete();
				subject.setPicture(null);
				subject.store();
			}
			json.element("imageUrl", subject.bricks.getAvatarImageUrl());
			// ------------------------------------------------ SAVE PROFILE IMAGE -------------------------------------------------------------
		} else if ("SVIMG".equals(pageState.command)) {
			String imgData = pageState.getEntry("imgData").stringValueNullIfEmpty();
			if (JSP.ex(imgData)) {

				String fileName = "";
				String imgType = "";

				if (imgData.toLowerCase().indexOf("base64,") > -1) {
					imgType = "." + imgData.substring(imgData.indexOf("/") + 1, imgData.indexOf(";"));
					imgData = imgData.substring(imgData.indexOf("base64,") + "base64,".length());
				}

				byte[] decode = Base64.decode(imgData.getBytes());
				InputStream in = new ByteArrayInputStream(decode);

				TaskAuditSubject subject = TaskAuditSubject.load(pageState.getEntry("SUB_ID").intValueNoErrorCodeNoExc() + "");
				if (subject!=null){
					PersistentFile oldPhoto = subject.getPicture();
					if (oldPhoto != null)
						oldPhoto.delete();
				}
				

				//no ext case
				if (!JSP.ex(imgType))
					imgType = ".jpeg";
				String imageName = StringUtilities.paddTo(subject.getIntId(), "000000");

				imageName = imageName + "_" + StringUtilities.generatePassword(5) + imgType;

				File f = new File(ApplicationState.webAppFileSystemRootPath + File.separator + "avatars");
				if (!f.exists())
					f.mkdirs();

				FileOutputStream fos = new FileOutputStream(ApplicationState.webAppFileSystemRootPath
						+ File.separator + "avatars" + File.separator + imageName);
				FileUtilities.copy(in, fos, true);

				PersistentFile persistentFile = new PersistentFile(subject.getIntId(), imageName,
						PersistentFile.TYPE_WEBAPP_FILESTORAGE);
				persistentFile.setFileLocation("avatars/" + imageName);

				if (subject!=null){
					subject.setPicture(persistentFile);
					subject.store();
				}
				json.element("imageUrl", subject.bricks.getAvatarImageUrl());
			}
		} else if ("GETPIC".equals(pageState.command)) {
			String subjectId = pageState.getEntry("subjectId").stringValueNullIfEmpty();
			if (JSP.ex(subjectId)) {
				TaskAuditSubject subject = TaskAuditSubject.load(subjectId);
				json.element("imageUrl", subject.bricks.getAvatarImageUrl());
			}
		}
	} catch (Throwable t) {
		jsonHelper.error(t);
	}
	json.element("ok", true);
	jsonHelper.close(pageContext);
%>
