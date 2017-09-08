package com.twproject.document.businessLogic;

import com.twproject.document.TeamworkDocument;
import com.twproject.operator.TeamworkOperator;
import com.twproject.rank.Hit;
import com.twproject.resource.Person;
import com.twproject.resource.Resource;
import com.twproject.security.TeamworkPermissions;
import com.twproject.task.Assignment;
import com.twproject.task.Task;
import org.jblooming.ApplicationException;
import org.jblooming.messaging.SomethingHappened;
import org.jblooming.ontology.Documentable;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.ontology.VersionHome;
import org.jblooming.ontology.businessLogic.DeleteHelper;
import org.jblooming.oql.OqlQuery;
import org.jblooming.oql.QueryHelper;
import org.jblooming.page.HibernatePage;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.security.Area;
import org.jblooming.security.Role;
import org.jblooming.security.SecurityException;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.ActionSupport;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.Fields;
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
import org.jblooming.waf.view.RestState;

import java.text.ParseException;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

public class DocumentAction extends ActionSupport {

  public TeamworkOperator logged;


  public Documentable documentable;

  public TeamworkDocument document;

  public Task task=null;
  public Resource resource=null;


  public DocumentAction(RestState pageState){
    super(pageState);
    this.logged = (TeamworkOperator) pageState.getLoggedOperator();
  }


  public void editNoMake() throws PersistenceException {
    if (JSP.ex(restState.mainObjectId))
      document= TeamworkDocument.load(restState.mainObjectId);

    // se c'è già su document uso quello. robik 2015 04 03
    if (document!=null) {
      if (document.getTask() != null) {
        restState.addClientEntry("TASK_ID", document.getTask());
      } else if (document.getResource() != null) {
        restState.addClientEntry("RES_ID", document.getTask());
      }
    }
    loadDocumentable();

  }

  private void loadDocumentable() throws PersistenceException {
    task = Task.load(restState.getEntry("TASK_ID").intValueNoErrorCodeNoExc() + "");
    resource = Resource.load(restState.getEntry("RES_ID").intValueNoErrorCodeNoExc() + "");
    restState.setAttribute("REFERRAL_OBJECT",task!=null?task:resource);
  }


  public void cmdAdd() throws SecurityException, PersistenceException{
    editNoMake();
    //testPermission(TeamworkPermissions.document_canCreate);
    if (task!=null){
      task.testPermission(logged, TeamworkPermissions.document_canCreate);
    } else if (resource!=null){
      resource.testPermission(logged, TeamworkPermissions.document_canCreate);
    } else {
      throw new SecurityException(SecurityConstants.I18N_PERMISSION_LACKING,TeamworkPermissions.document_canCreate);
    }

    TeamworkDocument mainObject = new TeamworkDocument();
    mainObject.setIdAsNew();
    mainObject.setVersion(VersionHome.VERSION_ROOT);
    restState.addClientEntry("DOCUMENT_VERSION", mainObject.getVersion());
    if (restState.getEntry("DOCUMENT_TYPE").stringValueNullIfEmpty() == null)
      restState.addClientEntry("DOCUMENT_TYPE", "1");
    restState.setMainObject(mainObject);

  }

  public void cmdAddVersion() throws PersistenceException, SecurityException {
    editNoMake();
    document.testPermission(logged, TeamworkPermissions.document_canCreate);
    TeamworkDocument oldVersion = this.document;
    restState.addClientEntry("DOC_ROOT_ID", restState.mainObjectId);
    TeamworkDocument mainObject = new TeamworkDocument();
    mainObject.setIdAsNew();

    mainObject.setVersion(oldVersion.nextVersion());
    restState.addClientEntry("DOCUMENT_VERSION", mainObject.getVersion());

    mainObject.setType(oldVersion.getType());
    restState.addClientEntry("DOCUMENT_TYPE", mainObject.getType());

    //clean file path
    restState.removeEntry("sp_fi_br_DOCUMENT_UPLOAD_upl");

    restState.setMainObject(mainObject);
  }

  public void cmdEdit() throws PersistenceException, SecurityException {
    editNoMake();
    document.testPermission(logged,TeamworkPermissions.document_canRead);

    restState.setMainObject(document);
    make(document);
    Hit.getInstanceAndStore(document, logged, .1);
  }

  public void cmdSave() throws PersistenceException, ActionException, ApplicationException, SecurityException {
    editNoMake();

    TeamworkDocument docToBeSaved;
    boolean isNew = false;
    if (PersistenceHome.NEW_EMPTY_ID.equals(restState.mainObjectId)) {
      if (task!=null){
        task.testPermission(logged, TeamworkPermissions.document_canCreate);
      } else if (resource!=null){
        resource.testPermission(logged, TeamworkPermissions.document_canCreate);
      } else {
        throw new SecurityException(SecurityConstants.I18N_PERMISSION_LACKING,TeamworkPermissions.document_canCreate);
      }

      docToBeSaved = new TeamworkDocument();
      docToBeSaved.setIdAsNew();
      isNew = true;
    } else {
      document.testPermission(logged,TeamworkPermissions.document_canWrite);
      docToBeSaved = this.document;
    }
    restState.setMainObject(docToBeSaved);
    docToBeSaved.setCode(restState.getEntry("DOCUMENT_CODE").stringValueNullIfEmpty());
    try {
      String name = restState.getEntryAndSetRequired("DOCUMENT_NAME").stringValue();
      docToBeSaved.setName(name);
    } catch (ActionException e) {
    }
    try {
      docToBeSaved.setAuthored(restState.getEntry("DOCUMENT_AUTHORED").dateValue());
    } catch (ParseException e) {
    }
    docToBeSaved.setAuthor(restState.getEntry("DOCUMENT_AUTHOR"+ SmartCombo.TEXT_FIELD_POSTFIX).stringValue());
    String idArea = restState.getEntry("DOCUMENT_AREA").stringValueNullIfEmpty();
    if (idArea != null) {
      Area area = (Area) PersistenceHome.findByPrimaryKey(Area.class, idArea);
      docToBeSaved.setArea(area);
    }

    //set tags: si ripuliscono, si rendono univoci e si riseparano con ", " occhio allo spazio!
    docToBeSaved.setTags(StringUtilities.setToString(StringUtilities.splitToOrderSet(JSP.w(restState.getEntry("DOCUMENT_TAGS").stringValue()), ","),", ")  );

    docToBeSaved.setVersion(restState.getEntry("DOCUMENT_VERSION").stringValue());
    docToBeSaved.setVersionLabel(restState.getEntry("DOCUMENT_VERSION_LABEL").stringValue());
    int typeId = restState.getEntry("DOCUMENT_TYPE").intValueNoErrorCodeNoExc();
    if (typeId > 0)
      docToBeSaved.setType(typeId);
    String summa = restState.getEntry("SUMMA").stringValueNullIfEmpty();

    if (docToBeSaved.getType() == TeamworkDocument.IS_UPLOAD) {
      docToBeSaved.setContent(summa);
      //check the entry, as it is mandatory
      if (JSP.ex(restState.getEntry("DOCUMENT_UPLOAD").name))
        restState.getEntryAndSetRequired("DOCUMENT_UPLOAD").stringValue();

      PersistentFile persistentFile = docToBeSaved.getFile();
      if (persistentFile == null) {
        persistentFile = new PersistentFile(0, null,PersistentFile.DEFAULT_STORAGE_TYPE);
      }
      docToBeSaved.store(); // il documento deve già essere salvato dato che l'uploader potrebbe scrivere su BLOB e gli serve l'id del doc.
      docToBeSaved.setFile(Uploader.save(docToBeSaved, persistentFile, "DOCUMENT_UPLOAD", restState));

    } else if (docToBeSaved.getType() == TeamworkDocument.IS_FILE_STORAGE) {
      restState.initializeEntries("table");
      docToBeSaved.setSumma(summa);

      ClientEntry docUrlCE = restState.getEntry("DOCUMENT_URL_TO_CONTENT");
      String value = docUrlCE.stringValueNullIfEmpty();
      if (value != null && !value.toUpperCase().startsWith("RF"))
        docUrlCE.errorCode = "ERR_INVALID_FILE_STORAGE_URL";
      else
        docToBeSaved.setContent(value);

    } else if (docToBeSaved.getType() == TeamworkDocument.IS_URL) {
      docToBeSaved.setSumma(summa);

      ClientEntry docUrlCE = restState.getEntry("DOCUMENT_URL_TO_CONTENT");
      String value = docUrlCE.stringValueNullIfEmpty();
      if (value.toUpperCase().startsWith("RF"))
        docUrlCE.errorCode = "ERR_INVALID_URL";
      else
        docToBeSaved.setContent(value);

    } else if (docToBeSaved.getType() == TeamworkDocument.IS_CONTENT) {
      docToBeSaved.setContent(summa);
    }


    boolean isLocked = restState.getEntry("IS_LOCKED").checkFieldValue();
    if (isLocked && docToBeSaved.getLockedBy() == null)
      docToBeSaved.setLockedBy(Person.getLoggedPerson(restState));
    else
      docToBeSaved.setLockedBy(null);

    if (restState.validEntries()) {
      if (PersistenceHome.NEW_EMPTY_ID.equals(restState.mainObjectId)) {
        docToBeSaved.setOwner(restState.getLoggedOperator());
      }

      if (restState.getEntry("DOC_ROOT_ID").stringValueNullIfEmpty() != null) {
        TeamworkDocument oldRoot = (TeamworkDocument) PersistenceHome.findByPrimaryKey(TeamworkDocument.class, restState.getEntry("DOC_ROOT_ID").stringValue());
        oldRoot.setParentAndStore(docToBeSaved);
        //set references
        oldRoot.addNewVersionToReferralAndStore(docToBeSaved);
      }

      //task & resource managament
      if (task!=null) {
        docToBeSaved.setTask(task);
        docToBeSaved.store();
        DocumentAction.generateDocumentEvent(docToBeSaved, logged);

      } else if (resource!=null) {
        docToBeSaved.setResource(resource);
        docToBeSaved.store();

      } else {
        docToBeSaved.store();
      }

      Hit.getInstanceAndStore(docToBeSaved, logged, .2);

      // ok message feedback
      if (isNew)
        restState.addMessageOK(I18n.get("DOCUMENT_CORRECTLY_CREATED"));
      else
        restState.addMessageOK(I18n.get("DOCUMENT_CORRECTLY_SAVED"));

    }
  }


  public static void generateDocumentEvent(TeamworkDocument document, TeamworkOperator logged) throws StoreException {
    //generate event
    // add the message to the queue if there is somebody waiting for that notification.
    // event is generated for task-document only
    if (document.getTask() != null) {
      SomethingHappened change = new SomethingHappened();
      change.setIdAsNew();
      change.setEventType(Task.Event.TASK_DOCUMENT_ADDED + "");
      change.getMessageParams().put("SUBJECT", JSP.limWr(document.getTask().getDisplayName(), 30));

      change.setMessageTemplate(Task.Event.TASK_DOCUMENT_ADDED + "_MESSAGE_TEMPLATE");

      change.getMessageParams().put("task", document.getTask().getDisplayName());
      change.getMessageParams().put("documentTitle", JSP.w(document.getName()));
      change.setWhoCausedTheEvent(logged);

      PageSeed ps = new PageSeed(ApplicationState.serverURL + "/applications/teamwork/task/taskDocumentList.jsp");
      ps.setCommand("LIST_DOCS");
      ps.addClientEntry("DOC_ID",document.getId());
      ps.addClientEntry("TASK_ID", document.getTask().getId());

      ButtonLink edit = new ButtonLink(ps);
      edit.label = document.getTask().getDisplayName();
      change.setLink(edit.toPlainLink());
      change.setIdentifiable(document.getTask());
      change.store();
    }

  }


  public void cmdDelete() throws SecurityException, PersistenceException {
    editNoMake();
    document.testPermission(logged, TeamworkPermissions.document_canDelete);
    DeleteHelper.cmdDelete(document, restState);
  }


  public void cmdFind() throws PersistenceException, SecurityException {

    //search for default filter
    if (restState.getCommand() == null) {
      if (!PersistentSearch.feedFromDefaultSearch(TeamworkDocument.DOCUMENT, restState))
        // when not set use last changed docs
        restState.addClientEntry(Fields.FLD_FILTER_NAME, "PF_DOCUMENTS_RECENTLY_CHANGED");
    }
    TeamworkOperator logged = (TeamworkOperator) restState.getLoggedOperator();

    String hql = "select distinct doc.id from " + TeamworkDocument.class.getName() + " as doc";

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

        if ("PF_DOCUMENTS_RECENTLY_CHANGED".equals(cmd)) {
          restState.addClientEntry("DOCUMENT_LAST_MODIFIED", ">-2w");
          additionalSort = "document.lastModified desc";
        }
      }
    }

    boolean somethingSearched = recoveredFromSavedFilter;

    String filter = restState.getEntry("ID_CODE").stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addQBEORClauses(
              filter,
              qhelp.getOrElement("doc.id", "idx", QueryHelper.TYPE_CHAR),
              qhelp.getOrElement("doc.code", "code", QueryHelper.TYPE_CHAR)
      );

      somethingSearched = true;
    }

    filter = restState.getEntry("NAME_DESCRIPTION").stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addQBEORClauses(
              filter,
              qhelp.getOrElement("doc.name", "name", QueryHelper.TYPE_CHAR),
              qhelp.getOrElement("doc.summa", "summa", QueryHelper.TYPE_CHAR),
              qhelp.getOrElement("doc.content", "content", QueryHelper.TYPE_CLOB)
      );

      somethingSearched = true;
    }

    filter = restState.getEntry("DOCUMENT_TAGS").stringValueNullIfEmpty();
    if (filter != null) {
      qhelp.addQBEClause("doc.tags", "tags", filter, QueryHelper.TYPE_CHAR);
      somethingSearched = true;
    }

    int ivalue = restState.getEntry("TYPE").intValueNoErrorCodeNoExc();
    if (ivalue > 0) {
      qhelp.addOQLClause("doc.type= :typ", "typ", ivalue);
      somethingSearched = true;
    }

    String value = restState.getEntry("DOCUMENT_AUTHORED").stringValueNullIfEmpty();
    if (value != null) {
      qhelp.addQBEClause("doc.authored", "authored", value, QueryHelper.TYPE_DATE);
      somethingSearched = true;
    }

    String lastModified = restState.getEntry("DOCUMENT_LAST_MODIFIED").stringValueNullIfEmpty();
    if (lastModified != null) {
      qhelp.addQBEClause("doc.lastModified", "modified", lastModified, QueryHelper.TYPE_DATE);
      somethingSearched = true;
    }

    value = restState.getEntry("DOCUMENT_AUTHOR").stringValueNullIfEmpty();
    if (value != null) {
      qhelp.addQBEClause("doc.author", "author", value, QueryHelper.TYPE_CHAR);
      somethingSearched = true;
    }

    value = restState.getEntry("task").stringValueNullIfEmpty();
    if (value != null) {
      qhelp.addOQLClause("doc.task.id=:taskId", "taskId", value);
      somethingSearched = true;
    }

    value = restState.getEntry("TASK_ID").stringValueNullIfEmpty();
    if (value != null) {
      qhelp.addOQLClause("doc.task.id=:taskId", "taskId", value);
      somethingSearched = true;
    }

    value = restState.getEntry("resource").stringValueNullIfEmpty();
    if (value != null) {
      qhelp.addOQLClause("doc.resource.id=:resourceId", "resourceId", value);
      somethingSearched = true;
    }
    value = restState.getEntry("RES_ID").stringValueNullIfEmpty();
    if (value != null) {
      qhelp.addOQLClause("doc.resource.id=:resourceId", "resourceId", value);
      somethingSearched = true;
    }

    if (!somethingSearched && Commands.FIND.equals(restState.getCommand())) {
      qhelp.addQBEClause("doc.name", "docname", "*", QueryHelper.TYPE_CHAR);
      somethingSearched = true;
    }

    if (!logged.hasPermissionAsAdmin()) {
/*
________________________________________________________________________________________________________________________________________________________________________


  begin security

________________________________________________________________________________________________________________________________________________________________________

*/

      //take care that this alias is used also out of the method e.g. in search
      qhelp.addJoinAlias(" left outer join task.assignments as assignment");


      qhelp.addJoinAlias(" left outer join doc.task  as task");
      qhelp.addJoinAlias(" left outer join doc.resource  as resource");

      // TASK CLAUSES

      //open unique clause and set owner; qhelp puts the "and" before; all following clauses are in or
      qhelp.addOQLClause("( ( task.owner = :logged", "logged", logged);

      //areas
      Set<Area> areas = logged.getAreasForPermission(TeamworkPermissions.document_canRead);
      if (areas.size() > 0) {
        qhelp.addOrQueryClause("task.area in (:areas)");
        qhelp.addParameter("areas", areas);
        qhelp.addOrQueryClause("resource.area in (:areasR)");
        qhelp.addParameter("areasR", areas);
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
      qhelp.addOQLClause("( resource.owner = :logged", "logged", logged);

      //in order to keep all security conditions in a unique and clause
      qhelp.addToHqlString(")");

      //end security big clause
      qhelp.addToHqlString(")");

/*
________________________________________________________________________________________________________________________________________________________________________


  end security

________________________________________________________________________________________________________________________________________________________________________

*/
    }


    qhelp.wrapHql("select document from " + TeamworkDocument.class.getName() + " as document where document.id in (", ")");

    if (somethingSearched) {
      DataTable.orderAction(qhelp, "DOCLST", restState, JSP.ex(additionalSort) ? additionalSort : "name");
      restState.setPage(HibernatePage.getHibernatePageInstance(qhelp.toHql().getQuery(), Paginator.getWantedPageNumber(restState), Paginator.getWantedPageSize("DOCLST", restState)));
    }

  }


  public void make(TeamworkDocument document) throws PersistenceException {
    restState.addClientEntry("DOCUMENT_CODE", document.getCode());
    restState.addClientEntry("DOCUMENT_NAME", document.getName());
    restState.addClientEntry("DOCUMENT_AUTHORED", DateUtilities.dateToString(document.getAuthored()));
    restState.addClientEntry("DOCUMENT_AUTHOR"+ SmartCombo.TEXT_FIELD_POSTFIX, document.getAuthor());
    restState.addClientEntry("DOCUMENT_AREA", (document.getArea() != null ? document.getArea().getId() : ""));
    restState.addClientEntry("DOCUMENT_TAGS", document.getTags());
    restState.addClientEntry("DOCUMENT_VERSION", document.getVersion());
    restState.addClientEntry("DOCUMENT_VERSION_LABEL", document.getVersionLabel());
    restState.addClientEntry("DOCUMENT_TYPE", document.getType());
    if (document.getLockedBy() != null)
      restState.addClientEntry("IS_LOCKED", Fields.TRUE);

    if (document.getType() == TeamworkDocument.IS_UPLOAD && document.getFile() != null) {
      ClientEntry uplCe = new ClientEntry("DOCUMENT_UPLOAD", document.getFile().serialize());
      if (!document.existsFile())
        uplCe.errorCode = "DOCUMENT_DATA_INVALID";
      restState.addClientEntry(uplCe);
      restState.addClientEntry("SUMMA", document.getContent());

    } else if (document.getType() == TeamworkDocument.IS_URL) {
      restState.addClientEntry("DOCUMENT_URL_TO_CONTENT", document.getContent());
      restState.addClientEntry("SUMMA", document.getSumma());

    } else if (document.getType() == TeamworkDocument.IS_CONTENT) {
      restState.addClientEntry("SUMMA", document.getContent());

    } else if (document.getType() == TeamworkDocument.IS_FILE_STORAGE) {
      restState.addClientEntry("DOCUMENT_URL_TO_CONTENT", document.getContent());
      restState.addClientEntry("SUMMA", document.getSumma());
    }

  }


  public void cmdTakeOwnership() throws PersistenceException, SecurityException {
    cmdEdit();
    TeamworkDocument task = (TeamworkDocument) restState.getMainObject();
    TeamworkOperator teamworkOperator = (TeamworkOperator) restState.getLoggedOperator();

    task.setOwner(teamworkOperator);
    task.store();
    if (restState.getEntry("TAKE_OWNERSHIP_PROPAGATE").checkFieldValue()) {
      List<TeamworkDocument> descs = (List<TeamworkDocument>) task.getDescendants(TeamworkDocument.class);
      for (TeamworkDocument desc : descs) {
        desc.setOwner(teamworkOperator);
        desc.store();
      }
    }
  }


  public void cmdBulkMoveToTask() throws ActionException, PersistenceException, SecurityException {
    try {
      String taskId = restState.getEntryAndSetRequired("DOCUMENT_MOVE_TO_TASK").stringValue();
      task = (Task) PersistenceHome.findByPrimaryKey(Task.class, taskId);
      int docMoved=0;
      if (task!=null) {
        //devo poter creare un doc sul task di destinazione
        task.testPermission(logged, TeamworkPermissions.document_canCreate);
        Set<String> ids = StringUtilities.splitToSet(restState.getEntry("docIds").stringValueNullIfEmpty(), ",");
        for (String docId : ids) {
          TeamworkDocument doc = TeamworkDocument.load(docId);

          if (!task.equals(doc.getTask())) {
            doc.testPermission(logged, TeamworkPermissions.document_canCreate);
            doc.setTask(task);
            doc.setResource(null);

            doc.store();

            //genera evento di aggiunta doc su task
            generateDocumentEvent(doc, logged);
          }
          docMoved++;
        }
        if (docMoved>0)
          restState.addMessageOK(I18n.get("DOCUMENT_MOVED_TO_FEEDBACK_%%",docMoved+"",task.getDisplayName()));
      } else {
        restState.addMessageError(I18n.get("NO_TASKS_DEFINED"));
      }

    } catch (ActionException e) {
    }
  }


  public void cmdBulkMoveToResource() throws ActionException, PersistenceException, SecurityException {
    try {
      String resId = restState.getEntryAndSetRequired("DOCUMENT_MOVE_TO_RES").stringValue();
      resource = Resource.load(resId);
      int docMoved=0;
      if (resource!=null) {
        //devo poter creare un doc sulla risorsa di destinazione
        resource.testPermission(logged, TeamworkPermissions.document_canCreate);
        Set<String> ids = StringUtilities.splitToSet(restState.getEntry("docIds").stringValueNullIfEmpty(), ",");
        for (String docId : ids) {
          TeamworkDocument doc = TeamworkDocument.load(docId);

          if (!resource.equals(doc.getResource())) {
            doc.testPermission(logged, TeamworkPermissions.document_canCreate);
            doc.setResource(resource);
            doc.setTask(null);

            doc.store();
          }
          docMoved++;
        }
        if (docMoved>0)
          restState.addMessageOK(I18n.get("DOCUMENT_MOVED_TO_FEEDBACK_%%",docMoved+"",resource.getDisplayName()));
      } else {
        restState.addMessageError(I18n.get("NO_TASKS_DEFINED"));
      }
    } catch (ActionException e) {
    }
  }




  public void cmdBulkAddTags() throws ActionException, PersistenceException, SecurityException {
    Set<String> ids = StringUtilities.splitToSet(restState.getEntry("docIds").stringValueNullIfEmpty(), ",");
    String tags = restState.getEntry("DOCUMENT_TAGS").stringValueNullIfEmpty();
    boolean replace = restState.getEntry("REPLACE_EXISTING").checkFieldValue();
    LinkedHashSet<String> tagsToAdd = StringUtilities.splitToOrderSet(JSP.w(tags), ","); //fa anche il trim dei singoli tag
    for (String docId : ids) {
      TeamworkDocument doc = TeamworkDocument.load(docId);
      if (doc != null && doc.hasPermissionFor(logged, TeamworkPermissions.document_canWrite)) {
        LinkedHashSet<String> finalTags;
        if (replace) {
          finalTags = new LinkedHashSet();
        }else{
          finalTags = StringUtilities.splitToOrderSet(doc.getTags(), ",");
        }
        finalTags.addAll(tagsToAdd);
        doc.setTags(StringUtilities.setToString(finalTags,", ")); //attenzione lo spazio nel separatore ci DEVE essere
        doc.store();
      }
    }
  }



}