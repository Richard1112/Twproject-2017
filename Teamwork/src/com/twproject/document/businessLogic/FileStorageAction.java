package com.twproject.document.businessLogic;

import com.twproject.operator.TeamworkOperator;
import com.twproject.security.TeamworkPermissions;
import org.jblooming.ontology.businessLogic.DeleteHelper;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.remoteFile.Document;
import org.jblooming.remoteFile.FileStorage;
import org.jblooming.remoteFile.RemoteFileSvn;
import org.jblooming.security.Area;
import org.jblooming.security.SecurityException;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.file.fileStorage.FileStorageUtilities;
import org.jblooming.waf.ActionSupport;
import org.jblooming.waf.ActionUtilities;
import org.jblooming.waf.constants.FieldErrorConstants;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.view.PageState;

import java.io.File;

public class FileStorageAction extends ActionSupport {

  TeamworkOperator logged;

  public FileStorageAction(PageState pageState) {
    super(pageState);
    this.logged = (TeamworkOperator) pageState.getLoggedOperator();

  }


  public void cmdAdd() throws PersistenceException, SecurityException {

    logged.testPermission(TeamworkPermissions.fileStorage_canCreate);

    FileStorage mainObject = new FileStorage();
    mainObject.setIdAsNew();
    restState.setMainObject(mainObject);

    TeamworkOperator logged = (TeamworkOperator) restState.getLoggedOperator();

    Area defaultAreaForPermission = logged.getDefaultAreaForPermission(TeamworkPermissions.fileStorage_canCreate);

    mainObject.setArea(defaultAreaForPermission);

    mainObject.setOwner(logged);

    restState.addClientEntry("AREA", mainObject.getArea() != null ? mainObject.getArea().getId() : "");
    restState.addClientEntry("connectionType", "FS");

  }

  public void cmdEdit() throws PersistenceException, org.jblooming.security.SecurityException {

    FileStorage document = (FileStorage) PersistenceHome.findByPrimaryKey(FileStorage.class, restState.getMainObjectId());

    document.testPermission(logged, TeamworkPermissions.fileStorage_canRead);

    restState.setMainObject(document);
    make(document);

  }

  public void cmdSave() throws PersistenceException, ActionException, SecurityException {
    restState.initializeEntries("table");
    FileStorage fileStorage;
    boolean isNew = false;
    if (PersistenceHome.NEW_EMPTY_ID.equals(restState.mainObjectId)) {
      fileStorage = new FileStorage();
      fileStorage.setIdAsNew();
      isNew = true;
    } else
      fileStorage = (FileStorage) PersistenceHome.findByPrimaryKey(FileStorage.class, restState.getMainObjectId());

    ActionUtilities.setIdentifiable(restState.getEntryAndSetRequired("AREA"), fileStorage, "area");

    fileStorage.testPermission(logged, TeamworkPermissions.fileStorage_canWrite);


    restState.setMainObject(fileStorage);
    fileStorage.setCode(restState.getEntry("DOCUMENT_CODE").stringValueNullIfEmpty());
    try {
      String name = restState.getEntryAndSetRequired("DOCUMENT_NAME").stringValue();
      fileStorage.setName(name);
    } catch (ActionException e) {
    }


    fileStorage.setType(Document.IS_FILE_STORAGE);
    ActionUtilities.setString(restState.getEntry("connectionType"), fileStorage, "connectionType");


    if (Document.ConnectionType.FS.equals(fileStorage.getConnType())) {
      try {
        String urlToContent = restState.getEntryAndSetRequired("DOCUMENT_URL_TO_CONTENT").stringValue();
        //verify that it is allowed URL
        boolean allowed = FileStorageUtilities.validUrlToContent(urlToContent);

        if (allowed) {
          while (urlToContent.endsWith(File.separator))
            urlToContent = urlToContent.substring(0, urlToContent.length() - 1);
          fileStorage.setContent(urlToContent);

        } else
          restState.getEntry("DOCUMENT_URL_TO_CONTENT").errorCode = restState.getI18n("PATH_NOT_ALLOWED");
      } catch (ActionException e) {
        Tracer.platformLogger.error(e);
      }
    } else {
      String urlToContent = restState.getEntry("DOCUMENT_URL_TO_CONTENT").stringValueNullIfEmpty();
      fileStorage.setContent(JSP.w(urlToContent));
    }
    String ct = restState.getEntryAndSetRequired("connectionType").stringValueNullIfEmpty();
    if (ct != null) {
      fileStorage.setConnType(Document.ConnectionType.valueOf(ct));
    } else {
      restState.getEntry("connectionType").errorCode = FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY;
    }

    if (Document.ConnectionType.SVN.equals(fileStorage.getConnType()) || Document.ConnectionType.SVN_Http.equals(fileStorage.getConnType()) || Document.ConnectionType.SVN_Https.equals(fileStorage.getConnType())) {
      ActionUtilities.setString(restState.getEntryAndSetRequired("connectionHost"), fileStorage, "connectionHost");
    } else {
      fileStorage.setConnectionHost(restState.getEntry("connectionHost").stringValueNullIfEmpty());
    }
    fileStorage.setConnectionUser(restState.getEntry("connectionUser").stringValueNullIfEmpty());
    fileStorage.setConnectionPwd(restState.getEntry("connectionPwd").stringValueNullIfEmpty());
    fileStorage.setSumma(restState.getEntry("SUMMA").stringValueNullIfEmpty());
    if (Document.ConnectionType.SVN.equals(fileStorage.getConnType()) || Document.ConnectionType.SVN_Http.equals(fileStorage.getConnType()) || Document.ConnectionType.SVN_Https.equals(fileStorage.getConnType())) {
      RemoteFileSvn rfs = new RemoteFileSvn(fileStorage);
      boolean found = rfs.setTarget(fileStorage.getContent());
      if (!found) {
        restState.getEntry("DOCUMENT_URL_TO_CONTENT").errorCode = restState.getI18n("PATH_NOT_ALLOWED");
      }
      rfs.closeSession();
    }
    if (restState.validEntries()) {
      fileStorage.store();

      // ok message feedback
      if (isNew)
        restState.addMessageOK(restState.getI18n("FILESTORAGE_CORRECTLY_CREATED"));
      else
        restState.addMessageOK(restState.getI18n("FILESTORAGE_CORRECTLY_SAVED"));

    }
  }


  public void cmdDelete() throws org.jblooming.security.SecurityException, PersistenceException {

    FileStorage delenda = (FileStorage) PersistenceHome.findByPrimaryKey(FileStorage.class, restState.getMainObjectId());
    delenda.testPermission(logged, TeamworkPermissions.fileStorage_canCreate);
    DeleteHelper.cmdDelete(delenda, restState);
  }


  public void make(FileStorage document) throws PersistenceException {
    restState.addClientEntry("DOCUMENT_CODE", document.getCode());
    restState.addClientEntry("DOCUMENT_NAME", document.getName());
    restState.addClientEntry("DOCUMENT_AUTHORED", DateUtilities.dateToString(document.getAuthored()));
    restState.addClientEntry("DOCUMENT_AUTHOR", document.getAuthor());
    restState.addClientEntry("DOCUMENT_AREA", (document.getArea() != null ? document.getArea().getId() : ""));
    restState.addClientEntry("DOCUMENT_KEYWORDS", document.getKeywords());
    restState.addClientEntry("DOCUMENT_VERSION", document.getVersion());
    restState.addClientEntry("DOCUMENT_VERSION_LABEL", document.getVersionLabel());
    restState.addClientEntry("DOCUMENT_TYPE", document.getType());

    restState.addClientEntry("AREA", document.getArea().getId());

    if (document.getType() == Document.IS_UPLOAD && document.getFile() != null) {
      restState.addClientEntry("DOCUMENT_UPLOAD", document.getFile());
    } else if (document.getType() == Document.IS_URL) {
      restState.addClientEntry("DOCUMENT_URL_TO_CONTENT", document.getContent());
    } else if (document.getType() == Document.IS_CONTENT) {
      restState.addClientEntry("SUMMA", document.getContent());
    } else if (document.getType() == Document.IS_FILE_STORAGE) {
      restState.addClientEntry("DOCUMENT_URL_TO_CONTENT", document.getContent());
      restState.addClientEntry("connectionType", document.getConnType());
      restState.addClientEntry("connectionHost", document.getConnectionHost());
      restState.addClientEntry("connectionUser", document.getConnectionUser());
      restState.addClientEntry("connectionPwd", document.getConnectionPwd());
    }
    if (document.getType() != Document.IS_CONTENT)
      restState.addClientEntry("SUMMA", document.getSumma());

  }


}
