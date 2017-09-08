<%@ page import="com.twproject.document.TeamworkDocument, com.twproject.document.businessLogic.DocumentAction, com.twproject.operator.TeamworkOperator, com.twproject.resource.Resource, com.twproject.resource.ResourceBricks, com.twproject.security.RoleTeamwork, com.twproject.security.TeamworkPermissions, com.twproject.task.Assignment, com.twproject.task.Task, com.twproject.task.businessLogic.AssignmentAction, com.twproject.task.businessLogic.TaskAction, com.twproject.task.financial.Cost, com.twproject.task.financial.CostClassification, net.sf.json.JSONArray, net.sf.json.JSONObject, org.jblooming.agenda.Period, org.jblooming.designer.DesignerField, org.jblooming.ontology.PersistentFile, org.jblooming.ontology.SerializedMap, org.jblooming.persistence.PersistenceHome, org.jblooming.utilities.JSP, org.jblooming.utilities.NumberUtilities, org.jblooming.waf.ActionUtilities, org.jblooming.waf.JSONHelper, org.jblooming.waf.html.input.Uploader, org.jblooming.waf.view.ClientEntry, org.jblooming.waf.view.PageState, org.jblooming.waf.view.RestState, java.util.Date, com.twproject.task.TaskDataHistory, java.util.List, org.jblooming.waf.settings.I18n, java.util.HashSet, java.util.Set, com.twproject.task.TaskBricks, org.jblooming.oql.QueryHelper, org.jblooming.agenda.CompanyCalendar, org.jblooming.messaging.MessagingSystem, org.jblooming.waf.constants.Fields, org.jblooming.tracer.Tracer, org.jblooming.waf.constants.OperatorConstants, net.wimpi.pim.util.Base64, java.io.InputStream, java.io.ByteArrayInputStream, org.jblooming.utilities.StringUtilities, java.io.File, org.jblooming.waf.settings.ApplicationState, java.io.FileOutputStream, org.jblooming.utilities.file.FileUtilities" %>
<%


  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  JSONHelper jsonHelper = new JSONHelper();
  JSONObject json = jsonHelper.json;

  try {
    Resource loggedRes = logged.getPerson();

    // --------------------------- CREATE NEW TASK and/or CREATE NEW ASSIGNMENTS --------------------------------
    TaskAction taskAction = new TaskAction(pageState);
    if ("NEWTASK".equals(pageState.command) || "NEWASSIGS".equals(pageState.command)) {

      Task task = null;

      //se è un nuovo task si crea altrimenti si carica
      if ("NEWTASK".equals(pageState.command)) {
        //si chiama la task action tanto le CE sono le stesse, i controlli sicurezza li fa lui
        taskAction.cmdSave();
        task = (Task) pageState.getMainObject();
      } else {
        task = Task.load(pageState.getEntry("taskId").intValueNoErrorCodeNoExc() + "");
      }

      //se tutto è andato bene si creano le assegnazioni
      if (task != null && pageState.validEntries()) {
        JSONArray assigs = JSONArray.fromObject(pageState.getEntry("assigs").stringValueNullIfEmpty());

        for (Object o : assigs) {
          JSONObject ass = (JSONObject) o;

          RoleTeamwork roleTeamwork = RoleTeamwork.load(ass.getString("roleId"));
          if (roleTeamwork == null)
            continue;

          Resource resource = Resource.load(ass.getString("resId"));
          if (resource == null)
            continue;


          RestState restState = new RestState(logged);
          restState.addClientEntry("TASK_ID", task.getId());
          AssignmentAction assAct = new AssignmentAction(restState);
          assAct.cmdAdd();

          restState.addClientEntry("ASSIGNEE", ass.getString("resId"));
          restState.addClientEntry("ASSIG_ROLE", ass.getString("roleId"));
          restState.addClientEntry("ESTIMATE_WORKLOG", ass.getString("estWl"));

          //add or make costs
          if (resource!=null) {
            //dobbiamo vedere se il logged ha i permessi di vedere i costi reali della risorsa
            if (resource.hasPermissionFor(logged,TeamworkPermissions.resource_cost_canRead) && resource.getHourlyCost() > 0)
              restState.addClientEntry("ASSIG_COST", resource.getHourlyCost() );
            else
              restState.addClientEntry("ASSIG_COST", JSP.currency(ResourceBricks.getDefaultHourlyCost()));
          }

          //add role subs default client entries
          SerializedMap<String, String> subm = roleTeamwork.getDefaultSubscriptions();
          if (subm != null) {
            for (String k : subm.keySet()) {
              restState.addClientEntry(k, subm.get(k));
            }

            TeamworkOperator myself = resource.getMyself();
            if (myself!=null && (!logged.equals(myself) || Fields.TRUE.equals(myself.getOption(OperatorConstants.NOTIFY_MY_EVENTS_TO_MYSELF)) ) ) // non notifichiamo noi stessi
              for (String media: myself.getPreferredMediaOrDefault(MessagingSystem.Media.STICKY))
                restState.addClientEntry("ASSIGNEE_NOTIFY_"+ media, Fields.TRUE);
          }

          assAct.cmdSave();
        }

        json.element("taskId", task.getId());
      }


      // ---------------------------------------- CHANGE TASK STATUS  ----------------------------------------
    } else if ("CHTSKSTS".equals(pageState.command)) {
      Set<Task> tasks = taskAction.cmdChangeStatus();
      json.element("ok", tasks.size()>0);


      // ---------------------------------------- GET TASK HISTORY DATA --------------------------------
    } else if ("GETHIST".equals(pageState.command)) {
      pageState.initializeEntries("cell");
      Task task = Task.load(pageState.mainObjectId);
      if (task != null) {
        if (task.hasPermissionFor(logged, TeamworkPermissions.task_cost_canRead)) {
          List<TaskDataHistory> dataHistory = task.getDataHistory();
          JSONArray jsa=new JSONArray();
          for (TaskDataHistory tdh:dataHistory){
            jsa.add(tdh.jsonify());
          }
          json.element("dataHistory",jsa);
        }
      }

      // --------------------------- UPDATE ASSIGNMENT ROW --------------------------------
    } else if ("UPDATEASSIGROW".equals(pageState.command)) {
      String assId = pageState.getEntry("assId").stringValueNullIfEmpty();
      String fld = pageState.getEntry("fld").stringValueNullIfEmpty();
      if (JSP.ex(assId) && JSP.ex(fld)) {
        Assignment assig = (Assignment) PersistenceHome.findByPrimaryKey(Assignment.class, assId);

        if (assig != null) {
          if (fld.startsWith("ASSPRIO_")) {
            pageState.initializeEntries("row");
            AssignmentAction.updateAssignmentPriority(assig,  pageState.getEntry("val").intValueNoErrorCodeNoExc(), Period.getDayPeriodInstance(new Date()).getStartDate().getTime());
          } else if (fld.startsWith("ACTIVE")) {
            if (loggedRes.equals(assig.getResource()) || assig.hasPermissionFor(logged,TeamworkPermissions.assignment_canCRW)) {
              ActionUtilities.setBoolean(pageState.getEntry("val"), assig, "enabled");
              assig.store();
            }
          } else if (fld.startsWith("ESTIMATE_WORKLOG")) {
            if (assig.hasPermissionFor(logged,TeamworkPermissions.assignment_canCRW) ) {
              ActionUtilities.setDurationInMillis(pageState.getEntry("val"), true, assig, "estimatedWorklog");
              assig.store();
            }

          } else if (fld.startsWith("HOURLY_COST")) {
            if (assig.hasPermissionFor(logged,TeamworkPermissions.assignment_canCRW)) {
              ClientEntry hCost = pageState.getEntryOrDefault("val");
              ActionUtilities.setCurrency(hCost, assig, "hourlyCost");
              assig.store();
            }

          }
        }
      }

      // --------------------------- GET RESOURCE COST --------------------------------
    } else if ("GETRESCOST".equals(pageState.command)) {
      String resId = pageState.getEntry("resId").stringValueNullIfEmpty();
      if (resId != null) {
        Resource assignee = (Resource) PersistenceHome.findByPrimaryKey(Resource.class, resId);
        json.element("resCost", assignee.getHourlyCost() > 0 ? JSP.currency(assignee.getHourlyCost()) : JSP.currency(ResourceBricks.getDefaultHourlyCost()));
      }

      // --------------------------- SPLIT ASSIGNMENT --------------------------------
    } else if ("SPLITASS".equals(pageState.command)) {
      Assignment fromAss = Assignment.load(pageState.getEntry("fromAss").intValueNoErrorCodeNoExc() + "");
      Resource res = Resource.load(pageState.getEntry("toRes").intValueNoErrorCodeNoExc() + "");
      long hours = pageState.getEntry("hours").timeValueInMillis();
      if (hours > 0 && fromAss != null && res != null) {
        Task task = fromAss.getTask();
        if (task.hasPermissionFor(logged, TeamworkPermissions.assignment_canCRW)) {

          //new assig or existing one
          Assignment targetAss = new AssignmentAction(pageState).getOrCreateAssignment(task, res, fromAss.getRole());

          //set hours
          targetAss.setEstimatedWorklog(targetAss.getEstimatedWorklog() + hours);

          //decrement old one
          long wl = fromAss.getEstimatedWorklog() - hours;
          wl = wl > fromAss.getWorklogDone() ? wl : fromAss.getWorklogDone();
          fromAss.setEstimatedWorklog(wl);
        }
      }

      // --------------------------- ENABLE / DISABLE ASSIGNMENT--------------------------------
    } else if ("ENABDISABASS".equals(pageState.command)) {
      Assignment assig = Assignment.load(pageState.getEntry("assId").stringValue());
      if (assig != null) {
        Task task = assig.getTask();
        if (assig.getResource().equals(logged.getPerson()) ||
          assig.hasPermissionFor(logged, TeamworkPermissions.resource_manage)) {
          assig.setEnabled(pageState.getEntry("enable").checkFieldValue());

          assig.store();
        }
      }

      // ---------------------------------------- DOCUMENT DROP MANAGEMENT ----------------------------------------
    } else if ("DROPDOC".equals(pageState.command)) {

      Task task = Task.load(pageState.mainObjectId);
      if (task != null && task.hasPermissionFor(logged, TeamworkPermissions.document_canCreate)) {

        PersistentFile pf = Uploader.save(task, "file", pageState);


        //controllare se esiste già caso mai fare una versione
        TeamworkDocument previousVersion = null;
        boolean isLocked = false;
        for (Object o : task.getDocuments()) {
          TeamworkDocument td = (TeamworkDocument) o;
          //get last version -> root
          if (td.getParent() == null) {
            if (td.getFile() != null && pf.getOriginalFileName().equals(td.getFile().getOriginalFileName())) {
              if (td.getLockedBy() != null) {
                isLocked = true;
              }
              previousVersion = td;
              break;

            }
          }
        }

        if (!isLocked) {
          TeamworkDocument document = new TeamworkDocument();
          document.setIdAsNew();
          document.setName(pf.getOriginalFileName());
          document.setAuthor(logged.getDisplayName());
          document.setType(TeamworkDocument.IS_UPLOAD);
          document.setVersion(previousVersion == null ? "01" : previousVersion.nextVersion());
          document.setTask(task);
          document.setFile(pf);
          document.setContent("");
          document.store();


          if (previousVersion != null) {
            previousVersion.setParentAndStore(document);
            //set references
            previousVersion.addNewVersionToReferralAndStore(document);
          }

          DocumentAction.generateDocumentEvent(document, logged);

          json.element("docMime", document.getFile().getMimeImageName());
          json.element("docHRef", document.bricks.getContentLink(pageState).getHtml());
          json.element("docId", document.getId());

        } else {
          json.element("ok", false);
          throw new Exception("File: " + pf.getOriginalFileName() + " locked by: " + previousVersion.getLockedBy().getDisplayName());
        }

      } else {
        json.element("ok", false);
        throw new Exception("No permission to create document.");
      }

    // ---------------------------------------- MOVE TASK  ----------------------------------------
    } else if ("TSK_MOVE".equals(pageState.command)) {
      boolean ret= taskAction.cmdMove();
      json.element("ok", ret);
      if (ret) {
        json.element("task",taskAction.task.jsonify(false,pageState));
      } else {
        throw new Exception(I18n.get("CHANGE_SCHEDULE_MISMATCH"));
      }

    // ---------------------------------------- COPY/CLONE TASK  ----------------------------------------
    } else if ("TSK_CLONE".equals(pageState.command)) {
      boolean ret= taskAction.cmdClone();
      json.element("ok", ret);
      if (ret) {
        json.element("task",taskAction.task.jsonify(false,pageState));
      } else {
        pageState.addMessageError(I18n.get("CHANGE_SCHEDULE_MISMATCH"));
      }

    // ---------------------------------------- ADD FROM TEMPLATE  ----------------------------------------
    } else if ("TSK_ADD_TPL".equals(pageState.command)) {
      boolean ret= taskAction.cmdAddFromTemplate();
      json.element("ok", ret);
      if (ret) {
        json.element("task",taskAction.task.jsonify(false,pageState));
      } else {
        throw new Exception(I18n.get("CHANGE_SCHEDULE_MISMATCH"));
      }

    // ---------------------------------------- RECODE TREE  ----------------------------------------
    } else if ("RECODE".equals(pageState.command)) {
      boolean ret= taskAction.cmdRecode();
      json.element("ok", ret);

      // ---------------------------------------- GET TASKS OPEN IN A PERIOD  ----------------------------------------
    } else if ("GTSKOPPER".equals(pageState.command)) {

      long start=pageState.getEntry("startMillis").longValueNoErrorNoCatchedExc();
      long end=pageState.getEntry("endMillis").longValueNoErrorNoCatchedExc();


      QueryHelper qh= new QueryHelper("select task from "+ Task.class.getName()+" as task");
      //qh.addOQLClause("not (task.schedule.start >= :d2 or task.schedule.end <= :d1) ");
      qh.addOQLClause("((task.schedule.start between :d1 and :d2) or (task.schedule.end between :d1 and :d2))");

      qh.setParameter("d1", new Date(start));
      qh.setParameter("d2", new Date(end));
      qh.addOQLClause("assignment.resource=:logRes","logRes",loggedRes);

      TaskBricks.addSecurityReadClauses(qh, pageState);

      List<Task> tks = qh.toHql().list();
      Set<Task> tasks = new HashSet<Task>(tks);

      JSONArray jts= new JSONArray();
      for (Task t:tasks){
        jts.add(t.jsonify(false,pageState));
      }

      if (JSP.ex(jts))
        json.element("tasks",jts);

      // ------------------------------------------------ SAVE PROFILE IMAGE -------------------------------------------------------------
    } else if ("SVIMG".equals(pageState.command)) {
      Task task = Task.load(pageState.getEntry("TASK_ID").intValueNoErrorCodeNoExc() + "");
      String imgData = pageState.getEntry("imgData").stringValueNullIfEmpty();
      if (JSP.ex(imgData) && task != null && task.hasPermissionFor(logged, TeamworkPermissions.task_canWrite)) {

        String fileName = "";
        String imgType = "";

        if (imgData.toLowerCase().indexOf("base64,") > -1) {
          imgType = "." + imgData.substring(imgData.indexOf("/") + 1, imgData.indexOf(";"));
          imgData = imgData.substring(imgData.indexOf("base64,") + "base64,".length());
        }

        byte[] decode = Base64.decode(imgData.getBytes());
        InputStream in = new ByteArrayInputStream(decode);

        PersistentFile oldPhoto = task.bricks.getImage();
        if (oldPhoto!=null)
          oldPhoto.delete();

        //no ext case
        if (!JSP.ex(imgType))
          imgType = ".jpeg";
        String imageName = "T"+StringUtilities.paddTo(task.getIntId(), "000000");

        imageName = imageName+"_" +StringUtilities.generatePassword(5) +imgType;

        File f=new File(ApplicationState.webAppFileSystemRootPath+File.separator+"avatars");
        if (!f.exists())
          f.mkdirs();

        FileOutputStream fos= new FileOutputStream(ApplicationState.webAppFileSystemRootPath+File.separator+"avatars"+File.separator+imageName);
        FileUtilities.copy(in, fos, true);

        PersistentFile persistentFile = new PersistentFile(task.getIntId(),imageName,PersistentFile.TYPE_WEBAPP_FILESTORAGE);
        persistentFile.setFileLocation("avatars/"+imageName);

        task.bricks.setImage(persistentFile);
        task.store();
        json.element("imageUrl",task.bricks.getImageUrl());

      }
    } else if ("RMIMG".equals(pageState.command)) {
      Task task = Task.load(pageState.getEntry("TASK_ID").intValueNoErrorCodeNoExc() + "");
      if (task != null && task.hasPermissionFor(logged, TeamworkPermissions.task_canWrite)) {
        PersistentFile image = task.bricks.getImage();
        if (image!=null)
          image.delete();
        task.bricks.setImage(null);
        task.store();
      }

    }


  } catch (Throwable t) {
    jsonHelper.error(t);
  }

  jsonHelper.close(pageContext);

%>
