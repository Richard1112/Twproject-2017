package org.jblooming.waf.html.display;

import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.container.ButtonBar;
import org.jblooming.waf.view.PageState;
import org.jblooming.remoteFile.Document;
import org.jblooming.remoteFile.RemoteFile;

import java.io.IOException;
import java.io.Serializable;

/**
 * Explorer (c) 2005 - Open Lab - www.open-lab.com
 */
public class Explorer  extends JspHelper  {

  public boolean zipAllowed = true;
  public boolean canWrite = false;
  public boolean canCreateDirectory=false;

  public Class aDocumentClass;
  public Document document;
  public String path;
  public RemoteFile rfs;
    public boolean canSeeProperties = false;         // visualise property
    public boolean canSeeFileserverCode = false;     // visualise coming from
  
  /**
   * when rootpath is set, Explore cann not navigate out of this root
   */
  public String rootpath;


  public Explorer (Class aClass, Document doc) {
    this.aDocumentClass = aClass;
    this.document = doc;
    this.urlToInclude = "/commons/layout/partExplorer.jsp";
  }


  /**
   * this class is used in SessionState in attributes map with key
  */
  public static class SecurityCarrier {
    public String rootPath ="";
    public boolean canRead = true;
    public boolean canWrite = true;
    public boolean canCreateDirectory = true;
    public static String getKey(Serializable docId){
      return "FSDCID"+docId;
    }

  }
}
