package org.jblooming.remoteFile;

import org.jblooming.ontology.Node;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.waf.view.PageState;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import net.sf.json.JSONObject;

import java.util.Date;
import java.io.Serializable;

public class FileStorage extends Document {


  public FileStorage getParent() {
    return (FileStorage) parent;
  }

  public Node getParentNode() {
    return getParent();
  }

   public void setParent(FileStorage n) {
    parent = n;
  }

   public void setParentNode(Node node) {
    setParent((FileStorage) node);
  }


  public JSONObject jsonify() {
    JSONObject ret = super.jsonify();
    ret.element("id", getId());
    ret.element("code", getCode());
    ret.element("name", getName());
    ret.element("path", getContent());
    return ret;


  }


  public static FileStorage load(Serializable id) throws FindByPrimaryKeyException {
    return (FileStorage) PersistenceHome.findByPrimaryKey(FileStorage.class, id);
  }


}
