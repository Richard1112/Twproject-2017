<%@ page import="com.twproject.agenda.Event, com.twproject.operator.TeamworkOperator, com.twproject.resource.Person, com.twproject.resource.Resource, com.twproject.security.TeamworkPermissions, com.twproject.task.*, com.twproject.task.businessLogic.IssueAction, net.sf.json.JSONArray, net.sf.json.JSONObject, org.jblooming.agenda.Period, org.jblooming.messaging.MessagingSystem, org.jblooming.messaging.SomethingHappened, org.jblooming.ontology.Pair, org.jblooming.ontology.PersistentFile, org.jblooming.ontology.businessLogic.DeleteHelper, org.jblooming.oql.OqlQuery, org.jblooming.persistence.PersistenceHome, org.jblooming.persistence.exceptions.PersistenceException, org.jblooming.persistence.hibernate.PersistenceContext, org.jblooming.tracer.Tracer, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.utilities.StringUtilities, org.jblooming.waf.JSONHelper, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.exceptions.ActionException, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.input.Uploader, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.*, java.util.*, org.jblooming.waf.html.input.SmartCombo" %>
<%
  JSONHelper jsonHelper = new JSONHelper();
  JSONObject json = jsonHelper.json;
  PageState pageState = PageState.getCurrentPageState(request);
  try {
    TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
    Person loggedPerson = logged.getPerson();

    if ("TASKCHANGED".equals(pageState.command)) { // rinfrescxa solo lo smartCombo in sessione
      String issueId = pageState.getEntry("ISSUEID").intValueNoErrorCodeNoExc() + "";
      String taskId = pageState.getEntry("TASKID").intValueNoErrorCodeNoExc() + "";
      int fieldSize = pageState.getEntry("SIZE").intValueNoErrorCodeNoExc();

      Task task = null;
      if (JSP.ex(taskId))
        task = (Task) PersistenceHome.findByPrimaryKey(Task.class, taskId);

      Issue issue = Issue.load(issueId);

      SmartCombo assignee;

      Resource resAssig = logged.getPerson();
      if (issue != null) {
        // make CE
        resAssig = issue.getAssignedTo();
      }

      if (resAssig != null)
        pageState.addClientEntry("ASSIGNEE", resAssig.getId());

      if (task == null && issue != null)
        task = issue.getTask();

      if (task != null) {
        //se hai il canManageAssignment puoi vedere anche altre risorse, altrimenti solo quelle che sono già assegnate
        if (task.hasPermissionFor(logged, TeamworkPermissions.assignment_canCRW)) {
          assignee = task.bricks.getAssignableResourceCombo("ASSIGNEE", false, pageState);
        } else {
          String hql = "select resource.id, resource.name from " + Resource.class.getName() + " as resource ";
          String whereForId = "where resource.id = :" + SmartCombo.FILTER_PARAM_NAME;
          // si mette una clausola che non trova niente, tanto quelle già assegnate si mettono comunque
          String whereForFiltering = "where resource.id='' and resource.id = :" + SmartCombo.FILTER_PARAM_NAME;
          assignee= new SmartCombo("ASSIGNEE",hql,whereForFiltering, whereForId);
          assignee.addAllowed=true;
          assignee.classic=true;
        }

        String hql = "select distinct assig.resource.id, assig.resource.name from " + Assignment.class.getName() + " as assig where assig.task = :task order by assig.resource.name";
        OqlQuery oql = new OqlQuery(hql);
        oql.getQuery().setEntity("task", task);
        List<Object[]> resOfAssigsOnTask = oql.list();

        assignee.fieldSize = fieldSize;
        assignee.separator = "";
        assignee.label = "";
        assignee.readOnly = !task.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.issue_canWrite);

        assignee.additionalLines = resOfAssigsOnTask;
        assignee.linkToEntity = null;

        json.element("task",task.jsonify());

      } else {
        assignee =TaskBricks.getAssignableResourceCombo(null,"ASSIGNEE", false, pageState);
        assignee.fieldSize = fieldSize;
        assignee.separator = "";
        assignee.label = "";
        assignee.linkToEntity=null;
        assignee.readOnly=issue==null?false:!issue.hasPermissionFor(logged,TeamworkPermissions.issue_canWrite);
      }

      pageState.sessionState.getAttributes().put(assignee.fieldName, assignee);

      // ---------------------------------------------------------------------- SAVE ISSUE SINGOLA (chiamato solo da organizer e da ChangeStatus)
    } else if (Commands.SAVE.equals(pageState.command)) {

      RestState restState=pageState; //qui si lavora con il pageStatevero
      IssueAction action = new IssueAction(restState);

      //ce coming from multi editor are incomplete
      // perform a make and override arrived CE
      ClientEntries newCes = new ClientEntries();
      newCes.addEntries(restState.getClientEntriesSet());

      // first I have to perform a fake-make or an add
      if ((restState.mainObjectId + "").startsWith("new")) {
        action.cmdAdd(false);
        restState.mainObjectId = PersistenceHome.NEW_EMPTY_ID;
      } else {
        action.cmdEdit();
      }

      IssueStatus oldStatus = ((Issue) restState.getMainObject()).getStatus();

      // override ce from edit with saved ones
      restState.addClientEntries(newCes);

      //in some cases i want to get/store values as default
      if (restState.getEntry("setValuesAsDefault").checkFieldValue()) {
        restState.getEntryOrDefault("ISSUE_STATUS",IssueStatus.getStatusOpen().getId()+"");
        restState.getEntryOrDefault("ISSUE_GRAVITY",Issue.GRAVITY_MEDIUM);
        restState.getEntryOrDefault("ISSUE_TASK");
      }

      // check if have to notify assignee   == and subscribe me to close event
      if (!JSP.w(restState.getEntry("ASSIGNEE").stringValueNullIfEmpty()).equals(JSP.w(loggedPerson.getId()))) {
        restState.addClientEntry("ASSIGNEE_NOTIFY", Fields.TRUE);
        for (MessagingSystem.Media media : MessagingSystem.activeMedia)
          restState.addClientEntry("ISSUE_SUBSCRIBE_CLOSE_" + media, Fields.TRUE);
      }


      // and now perform a real save
      if ( action.cmdSave()) {
        Issue issue = (Issue) restState.getMainObject();

        IssueStatus newStatus = ((Issue) restState.getMainObject()).getStatus();

        // check if worklog input can be displayed
        if (oldStatus != null && oldStatus != newStatus && newStatus.isAskForWorklog()) {
          Assignment ass = issue.getAssignmentOnTask(loggedPerson);
          if (ass != null) {
            json.element("assId", ass.getId() + "");
          }
        }
        //check if requires notes
        if (issue.getLastIssueHistory() != null && issue.getStatus().isAskForComment()) {
          //return back last history
          json.element("histId", issue.getLastIssueHistory().getId() + "");
        }

        //insert a json object used in organizer.jsp
        json.element("issue", issue.jsonify());


        //ci potrebbe essere anche un ordinamento;
        if (JSP.ex(restState.getEntry("SORT_FLAVOUR")) && JSP.ex(restState.getEntry("issues"))){
          action.cmdSortIssues();
        }

      }

      //---------------------------------------------------------------------- CHANGE STATUS
    } else if ("CHISSSTS".equals(pageState.command)) {

      RestState restState=pageState; //qui si lavora con il pageState vero
      IssueAction action = new IssueAction(restState);

      //ce coming from multi editor are incomplete
      // perform a make and override arrived CE
      ClientEntries newCes = new ClientEntries();
      newCes.addEntries(restState.getClientEntriesSet());

      // first I have to perform a fake-make or an add
      if ((restState.mainObjectId + "").startsWith("new")) {
        action.cmdAdd(false);
        restState.mainObjectId = PersistenceHome.NEW_EMPTY_ID;
      } else {
        action.cmdEdit();
      }

      IssueStatus oldStatus = ((Issue) restState.getMainObject()).getStatus();

      // override ce from edit with saved ones
      restState.addClientEntries(newCes);



      // and now perform a real save
      if ( action.cmdChangeStatus()) {
        Issue issue = (Issue) restState.getMainObject();

        IssueStatus newStatus = ((Issue) restState.getMainObject()).getStatus();

        // check if worklog input can be displayed
        if (oldStatus != null && oldStatus != newStatus && newStatus.isAskForWorklog()) {
          Assignment ass = issue.getAssignmentOnTask(loggedPerson);
          if (ass != null) {
            json.element("assId", ass.getId() + "");
          }
        }
        //check if requires notes
        if (issue.getLastIssueHistory() != null && issue.getStatus().isAskForComment()) {
          //return back last history
          json.element("histId", issue.getLastIssueHistory().getId() + "");
        }

        //insert a json object used in organizer.jsp
        json.element("issue", issue.jsonify());

      }

      //---------------------------------------------------------------------- SAVE MULTIPLE ISSUES
    } else if ("SVISS".equals(pageState.command)) {

      JSONArray issues = JSONArray.fromObject(pageState.getEntry("issues").stringValueNullIfEmpty());
      JSONObject toBeRefreshed= new JSONObject();

      for (Object o : issues) {
        JSONObject jIss = (JSONObject) o;


        RestState restState = new RestState(logged);  // qui si lavora con un restState ad hoc con i dati di una sola riga alla volta
        IssueAction action = new IssueAction(restState);

        String issueId=jIss.getString("OBJID");
        restState.mainObjectId=issueId;

        // first I have to perform a fake-make or an add
        if ((restState.mainObjectId + "").startsWith("new")) {
          action.cmdAdd(false);
          restState.mainObjectId = PersistenceHome.NEW_EMPTY_ID;
        } else {
          action.cmdEdit();
        }

        // override ce from edit with saved ones
        for (Object u:jIss.keySet()){
          String key=u+"";
          if (!"OBJID".equals(key))
            restState.addClientEntry(key, jIss.getString(key));
        }

        //in some cases i want to get/store values as default
        if (restState.getEntry("setValuesAsDefault").checkFieldValue()) {
          restState.getEntryOrDefault("ISSUE_STATUS");
          restState.getEntryOrDefault("ISSUE_GRAVITY");
          restState.getEntryOrDefault("ISSUE_TASK");
        }

        // check if have to notify assignee   == and subscribe me to close event
        if (!JSP.w(restState.getEntry("ASSIGNEE").stringValueNullIfEmpty()).equals(JSP.w(loggedPerson.getId()))) {
          restState.addClientEntry("ASSIGNEE_NOTIFY", Fields.TRUE);
          for (MessagingSystem.Media media : MessagingSystem.activeMedia)
            restState.addClientEntry("ISSUE_SUBSCRIBE_CLOSE_" + media, Fields.TRUE);
        }

        // and now perform a real save
        if (action.cmdSave()) { //save ok
          toBeRefreshed.element(issueId+"",restState.getMainObject().getId()+"");
          //PersistenceContext.getDefaultPersistenceContext().commitAndClose();  //ARRGHHH non si deve chiudere la sessione
          PersistenceContext.getDefaultPersistenceContext().checkPoint();
        } else {
          //PersistenceContext.getDefaultPersistenceContext().rollbackAndClose();  //ARRGHHH non si deve chiudere la sessione
          PersistenceContext.getDefaultPersistenceContext().rollbackPoint();

          if (!json.has("rowErrors"))
            json.element("rowErrors",new JSONObject());

          JSONObject rowErrors = json.getJSONObject("rowErrors");
          //gli errori si vrappano
          JSONArray ceErrors= new JSONArray();
          for (ClientEntry ce : restState.getClientEntriesSet()) {
            if (ce.errorCode!=null && !"__ERROR".equals(ce.name)){
              JSONObject ceE = new JSONObject();
              ceE.element("name",ce.name);
              ceE.element("label",I18n.get(ce.name));
              ceE.element("error",I18n.get(ce.errorCode));
              ceErrors.add(ceE);
            }
          }
          rowErrors.element(issueId+"",ceErrors);
        }

      }

      json.element("oldIdNewId",toBeRefreshed); //contiene solo le issues salvate



      //---------------------------------------------------------------------- GET HISTORY LIST
    } else if ("GETHISTNOTES".equals(pageState.command)) {
      Issue isssue = Issue.load(pageState.mainObjectId + "");
      if (isssue != null && isssue.hasPermissionFor(logged, TeamworkPermissions.issue_canRead)) {
        JSONArray hs = new JSONArray();
        for (IssueHistory h : isssue.getIssueHistories()) {
          hs.add(h.jsonify());
        }
        json.element("histories", hs);
      }


      //---------------------------------------------------------------------- DELETE HISTORY NOTES
    } else if ("DLHISTNOTES".equals(pageState.command)) {
      IssueHistory history = IssueHistory.load(pageState.getEntry("histId").intValueNoErrorCodeNoExc());
      if (history != null) {

        ////da dentro TW puoi cancellare tutto, ma se è una history si svuota il commento
        if (history.getIssue().hasPermissionFor(logged, TeamworkPermissions.issue_canWrite)) {
          if (history.getTask() != null || history.getAssignee() != null || history.getOldStatus() != null || history.getStatus() != null) {
            history.setComment(null); // clear comment only
          } else {
            //history.remove();
            DeleteHelper.cmdDelete(history,pageState);
          }
        }
      }
      //---------------------------------------------------------------------- SAVE HISTORY NOTES
    } else if ("SVHISTNOTES".equals(pageState.command)) {
      IssueHistory history = IssueHistory.load(pageState.getEntry("histId").intValueNoErrorCodeNoExc());
      String notes = pageState.getEntry("notes").stringValueNullIfEmpty();
      if (history != null) {
        if (history.getIssue().hasPermissionFor(logged, TeamworkPermissions.issue_canWrite)) {
          history.setComment(notes);
          history.store();
        }
      } else {  // you are adding a comment

        Issue issue = Issue.load(pageState.getEntry("issueId").intValueNoErrorCodeNoExc() + "");
        if (JSP.ex(notes) && issue != null && issue.hasPermissionFor(logged, TeamworkPermissions.issue_canWrite)) {
          history = issue.addComment(notes);
          history.store();
          //notify all previous commenters
          Set<TeamworkOperator>ops=new HashSet();
          for (IssueHistory comment: issue.getComments()){
            if (comment.getOwner()!=null)
              ops.add((TeamworkOperator) comment.getOwner());
          }
          ops.remove(logged);


          //si notificano tutti quelli coinvolti via log
          for (TeamworkOperator op:ops){
            issue.generateCommentMessage(logged,op,history, MessagingSystem.Media.LOG);
          }

          //se la issue era stata generata da un esterno gli si notifica la nota
          if (JSP.ex(issue.getExtRequesterEmail())){
            issue.generateCommentMessageForExternalResource(history);
          }

          //si genera un evento su task se si può
          issue.riseCommentAddedEvent(logged, notes);



        }
      }
      if (history != null)
        json.element("history", history.jsonify());


      // ---------------------------------------------------------------------- ISSUES DELETE
      // return nothing if everithing is ok, otherwise return an html formatted error
    } else if (Commands.DELETE.equals(pageState.command)) {

      Issue delenda = Issue.load(pageState.mainObjectId + "");
      if (delenda != null && delenda.hasPermissionFor(logged,TeamworkPermissions.issue_canDelete)) {
        try {
          new DeleteHelper().cmdDelete(delenda,pageState);

        } catch (PersistenceException pe) {
          throw new ActionException(I18n.get("DPRBOBJ", delenda.getDisplayName() + " (id:" + delenda.getId() + ")"));
        }
      }


      // ---------------------------------------------------------------------- ISSUES CHANGE ORDER
    } else if ("SORT_ISSUES".equals(pageState.command)) {
      IssueAction action = new IssueAction(pageState);
      action.cmdSortIssues();


      // ---------------------------------------------------------------------- ISSUES CHANGE PLAN
    } else if ("ISSCHPL".equals(pageState.command)) {
      pageState.initializeEntries("row");
      IssueAction action = new IssueAction(pageState);

      // first strip ce from issue id suffix and save them;
      pageState.mainObjectId = "" + pageState.getEntry("issueId").intValueNoErrorCodeNoExc();
      int newAssignee = pageState.getEntry("newAssignee").intValueNoErrorCodeNoExc();
      long newMillis = pageState.getEntry("newMillis").longValueNoErrorNoCatchedExc();

      // first I have to perform a fake-make
      action.cmdEdit();

      pageState.addClientEntry("ASSIGNEE", newAssignee);
      pageState.addClientEntry("ISSUE_DATE_CLOSE_BY", newMillis <= 0 ? ((Date) null) : new Date(newMillis));

      // and now perform a real save
      action.cmdSave();

      // ------------------------------------------------------------------------- LOAD ESTIMATION FOR RESOURCES
    } else if ("LDESTM".equals(pageState.command)) {
      String resId1 = pageState.getEntry("resId").stringValueNullIfEmpty();
      if (JSP.ex(resId1)) {
        Set<String> resIds = StringUtilities.splitToSet(resId1, ",");

        long focusMillis = pageState.getEntry("millis").longValueNoErrorNoCatchedExc();
        if (focusMillis == 0)
          focusMillis = System.currentTimeMillis();


        Period period = Period.getWeekPeriodInstance(new Date(focusMillis), logged.getLocale());

        // calculate workdone
        String hql = "select issue.assignedTo.id, year(issue.shouldCloseBy)*10000+month(issue.shouldCloseBy)*100+day(issue.shouldCloseBy), sum(issue.estimatedDuration) from " + Issue.class.getName() + " as issue " +
          "where issue.assignedTo.id in (:ress) and issue.shouldCloseBy between :st and :en " +
          "and issue.status.behavesAsOpen=true " +
          "group by issue.assignedTo.id,year(issue.shouldCloseBy)*10000+month(issue.shouldCloseBy)*100+day(issue.shouldCloseBy)";
        OqlQuery oql = new OqlQuery(hql);
        oql.getQuery().setParameterList("ress", resIds);
        oql.getQuery().setTimestamp("st", period.getStartDate());
        oql.getQuery().setTimestamp("en", period.getEndDate());
        List<Object[]> oas = oql.getQuery().list();

        String resId = null;

        JSONArray estimForRes = new JSONArray();
        JSONObject estims = new JSONObject();

        for (Object[] ao : oas) {

          if (resId == null) {
            resId = ao[0] + "";
          } else if (!resId.equals(ao[0])) { //key break
            estims.element(resId + "", estimForRes);
            estimForRes = new JSONArray();
            resId = ao[0] + "";
          }

          JSONObject e = new JSONObject();
          e.element("millis", DateUtilities.intToDate((Integer) ao[1]).getTime());
          e.element("estim", (Long) ao[2]);
          estimForRes.add(e);
        }
        if (resId != null) {
          estims.element(resId + "", estimForRes);
          estimForRes = new JSONArray();
        }

        json.element("estimations", estims);

        //get unavailability
        JSONArray unavail = new JSONArray();
        for (String rId : resIds) {
          Resource r = Resource.load(rId);
          //first read unavail from agenda
          TreeMap<Integer, Long> unavailPerDay = new TreeMap();
          for (Period up : Event.getUnavailabilityPeriodsInPeriodFor(r, period)) {
            int key = DateUtilities.dateToInt(up.getStartDate());
            Long unavailToday = unavailPerDay.get(key);
            if (unavailToday == null)
              unavailToday = 0l;
            unavailPerDay.put(key, unavailToday + up.getDurationInMillis());
          }

          for (Integer dayInt : unavailPerDay.keySet()) {
            JSONObject e = new JSONObject();
            e.element("resId", rId);
            e.element("millis", DateUtilities.intToDate(dayInt).getTime());
            e.element("unavail", unavailPerDay.get(dayInt));
            unavail.add(e);
          }
        }
        json.element("unavailabilities", unavail);
      }

    } else if ("REFRESHFILTER".equals(pageState.command)) {

      String resId = pageState.getEntry("resId").stringValueNullIfEmpty();
      boolean add = pageState.getEntry("add").checkFieldValue();
      String savedPlannerFilter = logged.getOption("SAVED_PLANNER_SEARCH");

      Set<String> setres = new HashSet<String>();
      if (JSP.ex(resId)) {
        if (JSP.ex(savedPlannerFilter)) {
          String[] res = savedPlannerFilter.split(",");
          setres = org.jblooming.utilities.CollectionUtilities.toSet(res);
        }
        if (setres.contains(resId))
          setres.remove(resId);
        savedPlannerFilter = "";
        for (String r : setres) {
          savedPlannerFilter = (JSP.ex(savedPlannerFilter) ? savedPlannerFilter + "," + r : r);
        }
        if (add)
          savedPlannerFilter = savedPlannerFilter + "," + resId;

        Resource r = Resource.load(resId);
        if (r != null) {
          json.element("resource", r.jsonify(false));
        }

        logged.putOption("SAVED_PLANNER_SEARCH", savedPlannerFilter);
        logged.store();
      }
      // ------------------------------------------------------------------------- LOAD ISSUE PLANNER
    } else if ("ISS_PLANNER".equals(pageState.command)) {
      IssueAction action = new IssueAction(pageState);
      jsonHelper.json = action.cmdISS_PLANNER();

    } else if ("GETISSUE".equals(pageState.command)) {
      Issue issue = Issue.load(pageState.getEntry("id").intValueNoErrorCodeNoExc() + "");
      if (issue != null && issue.hasPermissionFor(logged, TeamworkPermissions.issue_canRead)) {
        json.element("issue", issue.jsonify());
      }


      // ------------------------------------------------------------------------- TO DO  DONE / NOT DONE
    } else if ("TDDONE".equals(pageState.command)) {

      Issue todo = Issue.load(pageState.mainObjectId + "");
      if (todo.hasPermissionFor(logged, TeamworkPermissions.issue_canWrite)) {
        if (pageState.getEntry("done").checkFieldValue())
          todo.setStatusClosed();
        else
          todo.setStatusOpen();

        todo.store();
        json.element("todo", todo.jsonify());
      }


      // ------------------------------------------------------------------------- ADD TO DO
    } else if ("ADDTODO".equals(pageState.command)) {
      String desc = pageState.getEntry("descr").stringValueNullIfEmpty();
      if (JSP.ex(desc)) {
        Issue todo = new Issue();
        todo.setIdAsNew();
        todo.setArea(logged);
        todo.setOwner(logged);
        todo.setAssignedBy(loggedPerson);
        todo.setAssignedTo(loggedPerson);
        todo.setDescription(desc);
        todo.setStatusOpen();
        todo.setGravity(Issue.GRAVITY_MEDIUM);
        todo.store();

        json.element("todo", todo.jsonify());
      }

      // ------------------------------------------------------------------------- TOGGLECOMPACT
    } else if ("TOGGLECOMPACT".equals(pageState.command)) {
      pageState.getEntryOrDefault("COMPACT_VIEW", Fields.FALSE);


      // ------------------------------------------------------------------------- DROP DOCUMENT
    } else if ("DROPDOC".equals(pageState.command)) {
      Issue issue = Issue.load(pageState.getEntry("issueId").intValueNoErrorCodeNoExc() + "");
      if (issue != null && issue.hasPermissionFor(logged, TeamworkPermissions.issue_canWrite)) {

        PersistentFile pf = Uploader.save(issue, "file", pageState);
        issue.addFile(pf);
        issue.store();
        json.element("file",pf.jsonify());
        Tracer.platformLogger.info("Uploaded file" + pf.getUID() + " on issue " + issue.getId());

        //nel caso di upload su issue non ancora salvata -> issue==null
      } else if (issue==null) {
        PersistentFile pf = Uploader.save(null, "file", pageState);
        json.element("file",pf.jsonify());
        Tracer.platformLogger.info("Uploaded file" + pf.getUID() + " on not saved issue ");
      }

      // ------------------------------------------------------------------------- DELETE SCREENSHOT
    } else if ("DELSCRSHT".equals(pageState.command)) {
      Issue issue = Issue.load(pageState.getEntry("issueId").intValueNoErrorCodeNoExc() + "");
      if (issue != null && issue.hasPermissionFor(logged, TeamworkPermissions.issue_canWrite)) {
        String fileUID= pageState.getEntry("fileUID").stringValueNullIfEmpty();
        if (JSP.ex(fileUID)) {
          PersistentFile pf = PersistentFile.deserialize(fileUID);
          if (pf!=null && pf.checkChecksum(pageState.getEntry("ck").stringValueNullIfEmpty()+"")) { //si controlla anche il checksum
            pf.delete();
            issue.removeFile(pf);
            issue.store();
          }
        }
      }

    //------------------------------------------------ BULK ISSUE ACTIONS -----------------------------------------------------------------------------------------------
    } else if ("BULK_MOVE_TO_TASK".equals(pageState.command)) {
      new IssueAction(pageState).cmdBulkMoveToTask();

    } else if ("BULK_COPY_TO_TASK".equals(pageState.command)) {
      new IssueAction(pageState).cmdBulkCopyToTask();

    } else if ("BULK_MOVE_TO_RES".equals(pageState.command)) {
      new IssueAction(pageState).cmdBulkMoveToRes();

    } else if ("BULK_SET_NEW_DATE".equals(pageState.command)) {
      new IssueAction(pageState).cmdBulkSetNewDate();

    } else if ("BULK_SET_STATUS".equals(pageState.command)) {
      new IssueAction(pageState).cmdBulkSetStatus();

    } else if ("BULK_SET_GRAVITY".equals(pageState.command)) {
      new IssueAction(pageState).cmdBulkSetGravity();

    } else if ("BULK_SET_IMPACT".equals(pageState.command)) {
      new IssueAction(pageState).cmdBulkSetImpact();

    } else if ("BULK_ADD_COMMENT".equals(pageState.command)) {
      new IssueAction(pageState).cmdBulkAddComment();

    } else if ("BULK_ADD_TAGS".equals(pageState.command)) {
      new IssueAction(pageState).cmdBulkAddTags();

    } else if ("BULK_DEL_ISSUES".equals(pageState.command)) {
      new IssueAction(pageState).cmdBulkDelIssues(false);

    } else if ("BULK_CLOSE_ISSUES".equals(pageState.command)) {
      new IssueAction(pageState).cmdBulkCloseIssues();

    } else if ("BULK_MERGE_ISSUES".equals(pageState.command)) {
      new IssueAction(pageState).cmdBulkMergeIssues();

    } else if ("BULK_SET_NEW_DATE".equals(pageState.command)) {
      new IssueAction(pageState).cmdBulkSetNewDate();

    }

  } catch (Throwable t) {
    jsonHelper.error(t);
  }

  jsonHelper.close(pageContext);


%>