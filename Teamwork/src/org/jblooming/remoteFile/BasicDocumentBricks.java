package org.jblooming.remoteFile;

import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.tracer.Tracer;
import org.jblooming.PlatformRuntimeException;

import javax.persistence.Lob;
import javax.servlet.http.HttpServletRequest;
import java.io.InputStream;
import java.io.IOException;
import java.io.Serializable;

public class BasicDocumentBricks {
  public static Document mainObject;

  public BasicDocumentBricks(Document basicDocument) {
    mainObject = basicDocument;
  }

  //
  private static String getStringFromRemoteInputStream(InputStream input) throws IOException {
    StringBuffer str = new StringBuffer();
    int c;
    //getClient();
    //InputStream input = remoteFile.getRemoteInputStream();
    while (((c = input.read()) != -1)) {
      str.append((char) c);
    }
    input.close();
    return str.toString();

  }

  public static PageSeed getPageSeedForExplorer(HttpServletRequest request, PageState pageState, RemoteFile remoteFile, Document document) {
    mainObject = document;
    PageSeed downOrExplore = null;

    if (remoteFile.isDirectory()) {
      PageSeed ps = pageState.thisPage(request);
      ps.addClientEntry(pageState.getEntry("ROOTPATH"));
      ps.mainObjectId = mainObject.getId();
      ps.setPopup(pageState.isPopup());
      ps.addClientEntry("PATH", remoteFile.getRelativePath());
      downOrExplore = ps;
    } else {
      if (Document.ConnectionType.SERVICE.equals(document.getConnType())) {
        try {
          String psst = getStringFromRemoteInputStream(remoteFile.getRemoteInputStream());
          if (psst != null && !psst.trim().equals("")) {
            downOrExplore = new PageSeed(psst);
          } else {
            Tracer.platformLogger.error(" Cannot get URL ");

          }
        } catch (IOException e) {
          Tracer.platformLogger.error(" Cannot get URL ", e);
        }
      } else {
        downOrExplore = getPageSeedForDownload(mainObject,remoteFile.getRelativePath());
      }
    }

    return downOrExplore;

  }

  public static PageSeed getPageSeedForContent(String docContent, boolean openMainDir) throws FindByPrimaryKeyException {

    PageSeed downOrExplore = null;
    if (docContent != null) {
      if (docContent.startsWith("RF")) {
        String string = docContent.substring(2);
        String[] valori = string.split(":");
        if (valori != null && valori.length > 0) {
          String id = valori[0];
          FileStorage docFS = (FileStorage) PersistenceHome.findByPrimaryKey(FileStorage.class, id);
          if (docFS != null) {
            String relativePath = valori[1];
            RemoteFile rf = null;
            try {
              rf = RemoteFile.getInstance(docFS);
              rf.setTarget(relativePath); //docFS.getContent() +
              // controllo per aprire la directory che contiene il file
              if (!rf.isDirectory() && openMainDir) {
                int lastIndex = relativePath.lastIndexOf("/");
                if (lastIndex != -1) {
                  relativePath = relativePath.substring(0, lastIndex + 1);
                  rf.setTarget(relativePath);
                }
              }
              if (!rf.exists())
                return null; // add - graziella 04/09/08
            } catch (Exception e) {
              throw new PlatformRuntimeException(e);
            }

            if (rf != null) {
              if (!rf.isDirectory()) {
                downOrExplore = BasicDocumentBricks.getPageSeedForDownload(docFS, relativePath);
              } else {
                downOrExplore = new PageSeed(ApplicationState.contextPath + "/applications/teamwork/document/explorer.jsp");
                downOrExplore.mainObjectId = docFS.getId();
                downOrExplore.setPopup(true);
                downOrExplore.addClientEntry("PATH", relativePath);
                downOrExplore.addClientEntry("ROOTPATH", relativePath);
              }
            }
          }
        }
      } else { // standard url string e.g.: twproject.com
        downOrExplore = new PageSeed(docContent);
      }
    }
    return downOrExplore;
  }


  public static PageSeed getPageSeedForDownload(Document fileStorage, String relativePath){
    PageSeed ps = new PageSeed(ApplicationState.contextPath + "/commons/layout/partDownload.jsp");
    ps.mainObjectId = fileStorage.getId();
    ps.setPopup(true);
    ps.addClientEntry("PATH", relativePath);
    ps.addClientEntry("CK", _computeCk(fileStorage.getId(),relativePath,System.currentTimeMillis()));
    return ps;
  }

  public static boolean checkChecksum(Serializable docId, String relativePath, String checksum){
    String ms=checksum.substring(0,checksum.indexOf("."));
    long t=Long.parseLong(ms,Character.MAX_RADIX);
    if (System.currentTimeMillis()-t< CompanyCalendar.MILLIS_IN_DAY) // a link is valid for 24 hours
      return checksum.equals(_computeCk(docId, relativePath, t));
    else
      return false;
  }

  private static String _computeCk(Serializable docId, String relativePath,long millis){
    String ms = Long.toString(millis, Character.MAX_RADIX).toLowerCase();
    return ms+"."+StringUtilities.md5Encode(docId+relativePath,millis+"s41t3d");
  }

}
