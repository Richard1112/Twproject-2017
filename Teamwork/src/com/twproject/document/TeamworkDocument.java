package com.twproject.document;

import java.io.Serializable;

import org.apache.lucene.analysis.core.StopAnalyzer;
import org.hibernate.search.annotations.Analyzer;
import org.hibernate.search.annotations.DocumentId;
import org.hibernate.search.annotations.Field;
import org.hibernate.search.annotations.FieldBridge;
import org.hibernate.search.annotations.Fields;
import org.hibernate.search.annotations.Indexed;
import org.hibernate.search.bridge.builtin.StringBridge;
import org.jblooming.classification.Taggable;
import org.jblooming.ontology.Identifiable;
import org.jblooming.ontology.Node;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.ontology.PlatformComparators;
import org.jblooming.operator.User;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.remoteFile.Document;
import org.jblooming.remoteFile.RemoteFile;
import org.jblooming.security.Permission;
import org.jblooming.security.PermissionCache;
import org.jblooming.security.SecurableWithArea;
import org.jblooming.utilities.HttpUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.view.PageState;

import com.opnlb.fulltext.Indexable;
import com.opnlb.fulltext.IndexingMachine;
import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Person;
import com.twproject.resource.Resource;
import com.twproject.task.Task;

import net.sf.json.JSONObject;

@Indexed(index = "fulltext")
public class TeamworkDocument extends Document implements Indexable, Taggable {

  public DocumentBricks bricks = new DocumentBricks(this);

  private Task task;
  private Resource resource;

  private Person lockedBy;

  private String tags;


  public TeamworkDocument() {
    super();
  }

  public TeamworkDocument(String dId) {
    super(dId);
  }


  @Override
@DocumentId
  @FieldBridge(impl = StringBridge.class)
  public Serializable getId() {
    return super.getId();
  }


  public void setParent(TeamworkDocument n) {
    parent = n;
  }

  @Override
public void setParentNode(Node node) {
    setParent((TeamworkDocument) node);
  }

  public Task getTask() {
    return task;
  }

  public void setTask(Task task) {
    this.task = task;
    if (task != null)
      setArea(task.getArea());
  }


  public Resource getResource() {
    return resource;
  }

  public void setResource(Resource resource) {
    this.resource = resource;
    if (resource != null)
      setArea(resource.getArea());

  }

  public void setLockedBy(Person lockedBy) {
    this.lockedBy = lockedBy;
  }

  public Person getLockedBy() {
    return lockedBy;
  }

  public Identifiable getReferral() {

    Identifiable result = null;

    if (!isNew()) {
      if (getTask() != null)
        result = getTask();
      else
        result = getResource();
    }
    return result;
  }

  public void addNewVersionToReferralAndStore(TeamworkDocument newReferred) throws StoreException {

    if (!isNew()) {
      Task task = getTask();
      if (task != null) {
        newReferred.setTask(task);
        newReferred.store();
        return;
      }

      Resource resource = getResource();
      if (resource != null) {
        newReferred.setResource(resource);
        newReferred.store();
      }
    }
  }

  public boolean isEnabled(TeamworkOperator logged) throws PersistenceException {
    boolean isLocked = getLockedBy() != null;
    return (!isLocked || (isLocked && (getLockedBy().equals(logged.getPerson()))));
  }

  public TeamworkDocument getParent() {
    return (TeamworkDocument) parent;
  }

  @Override
public Node getParentNode() {
    return getParent();
  }

  public int compareVersionTo(TeamworkDocument doc) {
    PlatformComparators.VersionComparator vc = new PlatformComparators.VersionComparator();
    return vc.compare(this.getVersion(), doc.getVersion());
  }

  @Override
public boolean hasPermissionFor(User u, Permission p) {
    boolean result = false;
    PermissionCache.PermissionCacheEnabled sec = getTask() != null ? getTask() : getResource();

    if (sec != null)
      result = sec.hasPermissionFor(u, p);
    else
      result = super.hasPermissionFor(u, p);

    return result;
  }

  @Override
public String getTags() {
    return tags;
  }

  @Override
public void setTags(String tags) {
    this.tags = tags;
  }


  @Override
public String getAbstractForIndexing() {

    return JSP.w(
      JSP.w(getAuthor()) + " " +
        JSP.w(getCode()) + " " +
        JSP.w(getTags()) + " " +
        JSP.w(getName()) + " " +
        JSP.w(getSumma()) + " " +
        (IS_UPLOAD == getType() || IS_CONTENT == getType() ? JSP.w(getContent()) : "")
    );
  }

  @Fields({
    @Field(name = "fullcontent", analyzer = @Analyzer(impl = StopAnalyzer.class)),
    @Field(name = "content")
  })
  private String getContentForIndexing() {

    if (getFile() != null && getReferral() != null && ((SecurableWithArea) getReferral()).getArea() != null) {
      IndexingMachine.addToBeIndexed(this, ((SecurableWithArea) getReferral()).getArea().getId(), getFile());
    }
    return getAbstractForIndexing();

  }

  public static TeamworkDocument load(Serializable id) throws FindByPrimaryKeyException {
    return (TeamworkDocument) PersistenceHome.findByPrimaryKey(TeamworkDocument.class, id);
  }

	public static TeamworkDocument load(String docName) throws FindByPrimaryKeyException {
		return (TeamworkDocument) PersistenceHome.findUniqueNullIfEmpty(TeamworkDocument.class, "name", docName);
	}

  @Override
public JSONObject jsonify() {
    return jsonify(null);
  }

  public JSONObject jsonify(PageState pageState) {
    JSONObject ret = super.jsonify();
    ret.element("loadComplete", false);
    ret.element("id", getId());
    ret.element("type", getType());
    ret.element("code", getCode());
    ret.element("name", getName());
    ret.element("img", HttpUtilities.getContentType(getName()).replace('/', '_') + ".png");

    ret.element("mime", HttpUtilities.getContentType(getName()));
    ret.element("lastModified", getLastModified().getTime());
    ret.element("lastModifier", getLastModifier());


    ret.element("loadComplete", true);
    ret.element("version", getVersion());
    ret.element("versionLbl", getVersionLabel());
    ret.element("author", getAuthor());
    ret.element("date", JSP.w(getAuthored()));

    if (getTask() != null) {
      ret.element("taskId", getTask().getId());
      ret.element("taskName", getTask().getName());
      ret.element("taskCode", getTask().getCode());
    }

    if (getResource() != null) {
      ret.element("resId", getResource().getId());
      ret.element("resName", getResource().getDisplayName());
    }

    //in case of file storage supply fs data
    if (getType() == IS_FILE_STORAGE) {
      ret.element("content", getSumma());
      RemoteFile rf = getRemoteFile();
      if (rf != null)
        ret.element("remoteFile", rf.jsonify());

    } else if (getType() == IS_UPLOAD) {
      ret.element("content", getContent());
      PersistentFile pf = getFile();
      if (pf != null)
        ret.element("persFile", pf.jsonify());

    } else if (getType() == IS_URL) {
      ret.element("content", getSumma());
      ret.element("url", getContent());

    } else if (getType() == IS_CONTENT) {
      ret.element("content", getContent());
    }

    if (pageState!=null)
      ret.element("contentLink", bricks.getContentLink(pageState).href);

    return ret;

  }
}