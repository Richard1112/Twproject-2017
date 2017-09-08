package org.jblooming.remoteFile.businessLogic;

import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.remoteFile.Document;
import org.jblooming.remoteFile.RemoteFile;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.constants.FieldErrorConstants;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.html.input.Uploader;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.tracer.Tracer;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.*;
import java.util.List;
import java.util.Set;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;


/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class ExplorerAction {

  public void cmdZip(HttpServletRequest request, HttpServletResponse response, PageState pageState) throws PersistenceException, IOException {

    Document document = (Document) PersistenceHome.findByPrimaryKey(Document.class, pageState.mainObjectId);
    response.setContentType("application/zip");

    RemoteFile rfs = RemoteFile.getInstance(document);
    String path = JSP.w(pageState.getEntry("PATH").stringValueNullIfEmpty());
    path+= (path.endsWith(rfs.getSeparator())?"":rfs.getSeparator());


    //get selected files
    Set<String> selFiles = pageState.getClientEntries().getEntriesStartingWithStripped("FILE_", Fields.TRUE).keySet();
    String zipName = document.getName();
    if (selFiles.size() == 1)
      zipName = selFiles.iterator().next();

    response.setHeader("Content-Disposition", "attachment; filename=\"" + zipName + ".zip\"");
    ZipOutputStream zipout = new ZipOutputStream(response.getOutputStream());
    zipout.setComment("File Storage Service");

    for (String fileName : selFiles) {

      rfs.setTarget(path + fileName);
      if (rfs.isDirectory()) {
        List<RemoteFile> lrf = rfs.expandFileList();
        for (RemoteFile foundRF : lrf) {
          if (!foundRF.isDirectory())
            zipRemoteFile(foundRF, path, zipout);
        }
      } else
        zipRemoteFile(rfs, path, zipout);
    }

    try {
      zipout.finish();
      //response.getWriter().flush();
    } catch (java.util.zip.ZipException e) {
      Tracer.platformLogger.error(e);
    }

  }

  public void cmdDelete(PageState pageState) throws PersistenceException, IOException {

    Document document = (Document) PersistenceHome.findByPrimaryKey(Document.class, pageState.mainObjectId);

    RemoteFile rfs = RemoteFile.getInstance(document);
    String path = JSP.w(pageState.getEntry("PATH").stringValueNullIfEmpty());
    path+= (path.endsWith(rfs.getSeparator())?"":rfs.getSeparator());

    //get selected files
    Map<String,ClientEntry> entryMap = pageState.getClientEntries().getEntriesStartingWithStripped("FILE_", Fields.TRUE);

    for (String fileName : entryMap.keySet()) {
      rfs.setTarget(path + fileName);
      if (!rfs.delete()){
        entryMap.get(fileName).errorCode="CANT_DELETE_FILE";         
      }
    }

  }


  private void zipRemoteFile(RemoteFile foundRF, String currentPath, ZipOutputStream zipout) throws IOException {
    String fileDir = foundRF.getRelativePath().substring(currentPath.length());
    ZipEntry zipEntry = new ZipEntry(fileDir);
    zipout.putNextEntry(zipEntry);
    InputStream remoteInputStream = foundRF.getRemoteInputStream();
    BufferedInputStream fr = new BufferedInputStream(remoteInputStream);
    int b;
    byte[] buf = new byte[1024];
    int len;
    while ((len = fr.read(buf)) > 0) {
      zipout.write(buf, 0, len);
    }
    fr.close();
    zipout.closeEntry();
    remoteInputStream.close();
  }

  public void mkdir(PageState pageState) throws FindByPrimaryKeyException {
    Document document = (Document) PersistenceHome.findByPrimaryKey(Document.class, pageState.mainObjectId);
    try {
      String dirName = pageState.getEntryAndSetRequired("DIR_NAME").stringValueNullIfEmpty();

      if (dirName != null) {
        RemoteFile rfs = RemoteFile.getInstance(document);
        String path = JSP.w(pageState.getEntry("PATH").stringValueNullIfEmpty());
        path+= (path.endsWith(rfs.getSeparator())?"":rfs.getSeparator());
        if (dirName.indexOf(rfs.getSeparator()) != -1) {
          pageState.getEntry("DIR_NAME").errorCode = pageState.getI18n("PATH_NOT_ALLOWED");
        } else {
          rfs.setTarget(path +  dirName);
          rfs.mkdir();
        }
      }
    } catch (Exception e) {
      pageState.getEntry("DIR_NAME").errorCode = e.getMessage();
    }
  }

  public RemoteFile upload(PageState pageState) throws PersistenceException, IOException {

    Document document = (Document) PersistenceHome.findByPrimaryKey(Document.class, pageState.mainObjectId);
    Uploader.UploadHelper uh = Uploader.getHelper("UPLOAD_HERE", pageState);

    RemoteFile rfs = RemoteFile.getInstance(document);
    String path = JSP.w(pageState.getEntry("PATH").stringValueNullIfEmpty());
    path += (path.endsWith(rfs.getSeparator()) ? "" : rfs.getSeparator());

    String fileName = pageState.getEntryAndSetRequired("UPLOAD_HERE").stringValueNullIfEmpty();
    if (fileName != null) {
        rfs.setTarget(path + uh.originalFileName);
        try {
          boolean isUpload = rfs.upload(uh.temporaryFile,false);
          if (!isUpload)
            pageState.getEntry("UPLOAD_HERE").errorCode = FieldErrorConstants.ERR_NAME_USED;
        } catch (Throwable e) {
          pageState.getEntry("UPLOAD_HERE").errorCode = FieldErrorConstants.ERR_NAME_USED;
          Tracer.platformLogger.error(e);
        }
    } else {
      pageState.getEntry("UPLOAD_HERE").errorCode = FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY;
    }
    return rfs;

  }

}
