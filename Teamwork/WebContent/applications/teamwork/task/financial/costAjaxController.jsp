<%@ page import="com.twproject.document.TeamworkDocument, com.twproject.document.businessLogic.DocumentAction, com.twproject.operator.TeamworkOperator, com.twproject.resource.Resource, com.twproject.resource.ResourceBricks, com.twproject.security.RoleTeamwork, com.twproject.security.TeamworkPermissions, com.twproject.task.Assignment, com.twproject.task.Task, com.twproject.task.businessLogic.AssignmentAction, com.twproject.task.businessLogic.TaskAction, com.twproject.task.financial.Cost, com.twproject.task.financial.CostClassification, net.sf.json.JSONArray, net.sf.json.JSONObject, org.jblooming.agenda.Period, org.jblooming.designer.DesignerField, org.jblooming.ontology.PersistentFile, org.jblooming.ontology.SerializedMap, org.jblooming.persistence.PersistenceHome, org.jblooming.utilities.JSP, org.jblooming.utilities.NumberUtilities, org.jblooming.waf.ActionUtilities, org.jblooming.waf.JSONHelper, org.jblooming.waf.html.input.Uploader, org.jblooming.waf.view.ClientEntry, org.jblooming.waf.view.PageState, org.jblooming.waf.view.RestState, java.util.Date, com.twproject.task.TaskDataHistory, java.util.List, org.jblooming.waf.settings.I18n, java.util.HashSet, java.util.Set, com.twproject.task.TaskBricks, org.jblooming.oql.QueryHelper, org.jblooming.agenda.CompanyCalendar, org.jblooming.messaging.MessagingSystem, org.jblooming.waf.constants.Fields, org.jblooming.tracer.Tracer, com.twproject.worklog.WorklogStatus, com.twproject.task.businessLogic.ExpenseAction, com.twproject.task.businessLogic.AdditionalCostAction, org.jblooming.security.License, org.jblooming.ontology.businessLogic.DeleteHelper" %>
<%


  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  JSONHelper jsonHelper = new JSONHelper();
  JSONObject json = jsonHelper.json;

  try {
    Resource loggedRes = logged.getPerson();


    // ------------------------------------------------------  ASSIGNMENT COST MANAGEMENT ----------------------------------------------------------------

    // --------------------------- SAVE ASSIGNMENT COST --------------------------------
    if ("SVASSCOST".equals(pageState.command)) {

      JSONObject cost= new ExpenseAction(pageState).cmdSave();
      if (cost!=null)
        json.element("cost",cost);
      else
        json.element("ok",false);


      // --------------------------- DELETE ASSIGNMENT COST --------------------------------
    } else if ("DLASSCOST".equals(pageState.command)) {
      Assignment assig = Assignment.load(pageState.getEntry("assId").stringValue());
      if (assig != null) {
        Task task = assig.getTask();
        //if (logged.getPerson().equals(assig.getResource()) || task.hasPermissionFor(logged, TeamworkPermissions.assignment_manage)) {
        boolean canManageAssCost= assig.hasPermissionFor(logged, TeamworkPermissions.expense_manage);
        boolean isYourCost = logged.getPerson().equals(assig.getResource());
        Cost cost = Cost.load(pageState.getEntry("costId").intValueNoErrorCodeNoExc() + "");

        if ( (isYourCost && !cost.isLockedByDateOrStatus()) || canManageAssCost) {
          assig.getCosts().remove(cost);
          assig.store();
          //cost.remove();
          DeleteHelper.cmdDelete(cost,pageState);
        }
      }


      //  ---------------------------  ATTACHMENT COST DOCUMENT ---------------------------------
    } else if ("COSTATTACH".equals(pageState.command)) {
      Assignment assig= Assignment.load(pageState.getEntry("assId").intValueNoErrorCodeNoExc() + "");
      Task task=Task.load(pageState.getEntry("taskId").intValueNoErrorCodeNoExc() + "");

      boolean assigCondition=false;
      boolean isYours=false;
      boolean assigCanManage=false;
      if (assig!=null) {
        isYours = logged.getPerson().equals(assig.getResource());
        assigCanManage = assig.hasPermissionFor(logged, TeamworkPermissions.expense_manage);
        assigCondition = (isYours || assigCanManage);
      }

      boolean taskCondition = task != null && task.hasPermissionFor(logged, TeamworkPermissions.task_cost_canWrite);

      Cost cost = Cost.load(pageState.getEntry("costId").intValueNoErrorCodeNoExc() + "");

      if (assigCondition || taskCondition || cost==null){
        if (cost != null ) {
          if (taskCondition || isYours &&!cost.isLockedByDateOrStatus() || assigCanManage) {// posso caricare documenti non nel passato o se ho i permessi
            PersistentFile pf = Uploader.save(cost, "attachment", pageState);
            cost.setAttachment(pf);
            cost.store();
            json.element("attachment", pf.jsonify());
            Tracer.platformLogger.info("Uploaded file" + pf.getUID() + " on cost " + cost.getId());
          }

          //nel caso di upload su cost non ancora salvato -> cost==null
        } else if (cost == null) {
          PersistentFile pf = Uploader.save(null, "attachment", pageState);
          json.element("attachment", pf.jsonify());
          Tracer.platformLogger.info("Uploaded file" + pf.getUID() + " on not saved cost ");
        }
      }

      // ---------------------------------------- DELETE COST ATTACHMENT  ----------------------------------------
    } else if ("DELCOSTATTACH".equals(pageState.command)) {
      Assignment assig= Assignment.load(pageState.getEntry("assId").intValueNoErrorCodeNoExc() + "");
      Task task=Task.load(pageState.getEntry("taskId").intValueNoErrorCodeNoExc() + "");
      boolean assigCondition=false;
      boolean isYours=false;
      boolean assigCanManage=false;
      if (assig!=null) {
        isYours = logged.getPerson().equals(assig.getResource());
        assigCanManage = assig.hasPermissionFor(logged, TeamworkPermissions.expense_manage);
        assigCondition = (isYours || assigCanManage);
      }
      boolean taskCondition = task != null && task.hasPermissionFor(logged, TeamworkPermissions.task_cost_canWrite);
      if (assigCondition || taskCondition) {
        Cost cost = Cost.load(pageState.getEntry("costId").intValueNoErrorCodeNoExc() + "");
        if (cost != null ) {
          if ( taskCondition || isYours &&!cost.isLockedByDateOrStatus() || assigCanManage) {// posso eliminare non nel passato o se ho i permessi
            cost.getAttachment().delete();
            cost.setAttachment(null);
          }
        } else {
          PersistentFile.deserialize(pageState.getEntry("fileUID").stringValue()).delete();
        }
      }

      // --------------------------- GET ASSIGNMENT COSTS --------------------------------
    } else if ("GETCOSTS".equals(pageState.command)) {
      String assId = pageState.getEntry("assId").stringValueNullIfEmpty();
      if (assId != null) {
        Assignment assig = Assignment.load(assId);
        Task task = assig.getTask();
        if (logged.getPerson().equals(assig.getResource()) || task.hasPermissionFor(logged, TeamworkPermissions.task_cost_canRead)|| assig.hasPermissionFor(logged, TeamworkPermissions.expense_manage)) {
          json.element("assBudget", assig.getBudget());
          json.element("assReal", assig.getCostDone(false));
        }

      }

      // --------------------------- SAVE ASSIGNMENT BUDGET --------------------------------
    } else if ("SVBDG".equals(pageState.command)) {
      pageState.initializeEntries("row");
      Assignment assig = Assignment.load(pageState.getEntry("assId").stringValue());
      if (assig != null) {
        Task task = assig.getTask();
        if (task.hasPermissionFor(logged, TeamworkPermissions.task_cost_canWrite) ) {  // il budget è comunque un costo di progetto
          ActionUtilities.setCurrency(pageState.getEntryAndSetRequired("budget"), assig, "budget");
          assig.store();
        }
      }

      // --------------------------- COST CHANGE STATUS --------------------------------
    } else if ("COSTCHSTS".equals(pageState.command)) {
      if (License.assertLevel(30)) {

        Assignment assig = Assignment.load(pageState.getEntry("assId").stringValue());
        if (assig != null) {
          if (assig.hasPermissionFor(logged, TeamworkPermissions.expense_manage)) {
            Cost cost = Cost.load(pageState.getEntry("costId").intValueNoErrorCodeNoExc() + "");
            if (cost != null) {
              WorklogStatus sts = WorklogStatus.load(pageState.getEntry("csSts").intValueNoErrorCodeNoExc() + "");
              cost.setStatus(sts);
              cost.store();
            }
          }
        }
      }



      //----------------------------------------- PROJECT COSTS ------------------------------------------------------------------------

      // --------------------------- DELETE TASK ADDITIONAL COST --------------------------------
    } else if ("DLADDCOST".equals(pageState.command)) {
      Task task=Task.load(pageState.getEntry("taskId").intValueNoErrorCodeNoExc() + "");
      boolean taskCondition = task != null && task.hasPermissionFor(logged, TeamworkPermissions.task_cost_canWrite);
      if (taskCondition) {
        Cost cost = Cost.load(pageState.getEntry("costId").intValueNoErrorCodeNoExc() + "");
        task.getCosts().remove(cost);
        task.store();
        //cost.remove();
        DeleteHelper.cmdDelete(cost,pageState);
      }


      // --------------------------- SAVE TASK ADDITIONAL COST --------------------------------
    } else if ("SVADDCOST".equals(pageState.command)) {
      JSONObject cost= new AdditionalCostAction(pageState).cmdSave();
      if (cost!=null)
        json.element("cost",cost);
      else
        json.element("ok",false);


      // ---------------------------------------- SAVE COSTS BULK --------------------------------
    } else if ("SAVECOSTSBULK".equals(pageState.command)) {
      Task task = Task.load(pageState.mainObjectId);
      if (task != null) {

        //budget
        if (JSP.ex(pageState.getEntry("budget")) && task.hasPermissionFor(logged, TeamworkPermissions.task_cost_canWrite)) {
          task.setForecasted(pageState.getEntry("budget").currencyValueNoErrorNoCatchedExc());
          task.store();
        }

        //Custom fields
        DesignerField.saveCustomFields("BUDGET_CUSTOM_FIELD_","budgetCustomField" ,4, task, pageState);


        // worklog estimation costs
        if (JSP.ex(pageState.getEntry("worklogChanges"))) {
          JSONObject worklogChanges = JSONObject.fromObject(pageState.getEntry("worklogChanges").stringValue());
          for (Object o : worklogChanges.keySet()) {
            String assId = (String) o;
            Assignment assig = Assignment.load(assId);
            if (assig.hasPermissionFor(logged, TeamworkPermissions.assignment_canCRW)) {
              assig.setEstimatedWorklog(worklogChanges.getLong(assId));
              assig.store();
            }
          }
        }


        // hourly costs
        if (JSP.ex(pageState.getEntry("assCosts"))) {
          JSONObject assigCosts = JSONObject.fromObject(pageState.getEntry("assCosts").stringValue());
          for (Object o : assigCosts.keySet()) {
            String assId = (String) o;
            Assignment assig = Assignment.load(assId);
            if (assig.hasPermissionFor(logged, TeamworkPermissions.task_cost_canWrite)) {
              assig.setHourlyCost(new ClientEntry("FUFF", assigCosts.getString(assId)).currencyValueNoErrorNoCatchedExc());
              assig.store();
            }
          }
        }

        // ass budget
        if (JSP.ex(pageState.getEntry("budgetChanges")) && License.assertLevel(20)) {
          JSONObject budgetChanges = JSONObject.fromObject(pageState.getEntry("budgetChanges").stringValue());
          for (Object o : budgetChanges.keySet()) {
            String assId = (String) o;
            Assignment assig = Assignment.load(assId);
            if (assig.hasPermissionFor(logged, TeamworkPermissions.task_cost_canWrite)) {
              assig.setBudget(new ClientEntry("FUFF", budgetChanges.getString(assId)).currencyValueNoErrorNoCatchedExc());
              assig.store();
            }
          }
        }


        // additional Costs
        /*if (JSP.ex(pageState.getEntry("additionalCosts"))) {
          JSONArray newlyAddedCosts = new JSONArray();
          JSONArray addCosts = JSONArray.fromObject(pageState.getEntry("additionalCosts").stringValueNullIfEmpty());
          for (Object o : addCosts) {
            JSONObject addCost = (JSONObject) o;
            Cost cost = Cost.load(addCost.getString("id"));
            Task costTask = Task.load(addCost.getString("taskId"));
            if (costTask != null && costTask.hasPermissionFor(logged, TeamworkPermissions.task_cost_canWrite)) {
              boolean isNew = false;
              if (cost == null) {
                cost = new Cost();
                costTask.getCosts().add(cost);
                isNew = true;
              }
              cost.setClassification(CostClassification.load(JSP.ex(addCost.getString("classification")) ? Integer.parseInt(addCost.getString("classification")) : 0));
              cost.setEstimatedCost(JSP.ex(addCost.getString("estimatedCost")) ? NumberUtilities.parseCurrencyNoError(addCost.getString("estimatedCost")) : 0);
              cost.setRealCost(JSP.ex(addCost.getString("realCost")) ? NumberUtilities.parseCurrencyNoError(addCost.getString("realCost")) : 0);
              cost.setDescription(addCost.getString("description"));
              cost.store();

              if (isNew) {
                JSONObject cst = cost.jsonify();
                cst.element("taskId", costTask.getId());
                newlyAddedCosts.add(cst);
              }
              costTask.store();
            }
          }

          json.element("newlyAddedCosts", newlyAddedCosts);
        }*/
        json.element("budget", JSP.currency(task.getForecasted()));
      }
    }

  } catch (Throwable t) {
    jsonHelper.error(t);
  }

  jsonHelper.close(pageContext);

%>
