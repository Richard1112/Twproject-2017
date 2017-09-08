package com.opnlb.website.forum;

import com.opnlb.fulltext.Indexable;
import com.opnlb.website.forum.security.ForumPermissions;
import com.twproject.operator.TeamworkOperator;
import net.sf.json.JSONObject;
import org.apache.lucene.analysis.core.StopAnalyzer;
import org.hibernate.annotations.ForeignKey;
import org.hibernate.annotations.Type;
import org.hibernate.search.annotations.*;
import org.hibernate.search.annotations.Index;
import org.hibernate.search.bridge.builtin.IntegerBridge;
import org.jblooming.ontology.Node;
import org.jblooming.ontology.SecuredNodeSupport;
import org.jblooming.operator.Operator;
import org.jblooming.operator.User;
import org.jblooming.security.Permission;
import org.jblooming.security.PermissionCache;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.ReflectionUtilities;

import javax.persistence.*;
import java.io.Serializable;
import java.util.Date;
import java.util.Set;


/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */

@Entity
@Table(name = "_ws_forum")
@Inheritance(strategy=InheritanceType.SINGLE_TABLE)
@DiscriminatorColumn(
name="discriminator",
discriminatorType=DiscriminatorType.STRING
)
@DiscriminatorValue("FE")
public class ForumEntry extends SecuredNodeSupport implements Indexable, PermissionCache.PermissionCacheEnabled{

  @Transient
  public ForumBricks bricks = new ForumBricks(this);

  private Operator author;

  private String title;

  private String content = "ROOT";

  private boolean active = true;
  private int order;
  private Date postedOn;
  private Date lastPostOnBranch;
  private String lastPosterOnBranch;
  protected ForumEntry parent;

  private int hits;

  public static final String FORUM_ENTRY = "FRME";

  private boolean postForLogged;
  private boolean postToAll;
  private boolean privateForum;

  @Id
  @Type(type = "int")
  @GeneratedValue(strategy = GenerationType.AUTO)
  @DocumentId
  @FieldBridge(impl= IntegerBridge.class)
  public Serializable getId() {
      return super.getId();
  }

  public void setParent(ForumEntry n) {
    parent = n;
  }

  @ManyToOne(targetEntity = ForumEntry.class)
  @ForeignKey(name = "fk_forum_parent")
  @JoinColumn(name = "parent")
  public ForumEntry getParent() {
    return parent;
  }

  @Transient
  public Node getParentNode() {
    return getParent();
  }

  public void setParentNode(Node node) {
    setParent((ForumEntry) node);
  }

  @Field(name = "content")
  @Boost(2.5f)
  public String getTitle() {
    return title;
  }

  public void setTitle(String title) {
    this.title = title;
  }

  @Lob
  @Column(name = "contentx")
  @Type(type = "org.hibernate.type.TextType")
  public String getContent() {
    return content;
  }

  @Transient
  @Field(name = "content")
  @Boost(2)  
  public String getContentClean() {
    return JSP.cleanHTML(content);
  }

  public void setContent(String content) {
    this.content = content;
  }

  @ManyToOne(targetEntity = TeamworkOperator.class)
  @ForeignKey(name = "fk_forum_operator")
  public Operator getAuthor() {
    return author;
  }

  public void setAuthor(Operator author) {
    this.author = author;
  }

  public boolean isActive() {
    return active;
  }

  public void setActive(boolean active) {
    this.active = active;
  }

  @Column(name = "orderx")
  public int getOrder() {
    return order;
  }

  public void setOrder(int order) {
    this.order = order;
  }

  public Date getPostedOn() {
    return postedOn;
  }

  public void setPostedOn(Date postedOn) {
    this.postedOn = postedOn;
  }

  @Column(name = "lastpost")
  public Date getLastPostOnBranch() {
    return lastPostOnBranch;
  }

  public void setLastPostOnBranch(Date lastPostOnBranch) {
    this.lastPostOnBranch = lastPostOnBranch;
  }

  @Column(name = "hitsx")
  public int getHits() {
    return hits;
  }

  public void setHits(int hits) {
    this.hits = hits;
  }

  @Column(name = "writetologged")
  public boolean isPostForLogged() {
    return postForLogged;
  }

  public void setPostForLogged(boolean postForLogged) {
    this.postForLogged = postForLogged;
  }

  @Column(name = "writetoall")
  public boolean isPostToAll() {
    return postToAll;
  }

  public void setPostToAll(boolean postToAll) {
    this.postToAll = postToAll;
  }

  @Transient
  public String getName() {
    return getTitle();
  }

  @Column(name = "lastposter")
  public String getLastPosterOnBranch() {
    return lastPosterOnBranch;
  }

  public void setLastPosterOnBranch(String lastPosterOnBranch) {
    this.lastPosterOnBranch = lastPosterOnBranch;
  }

  public boolean isPrivateForum() {
    return privateForum;
  }

  @Transient
  @Field(name = "private", analyze = Analyze.NO ,index = Index.YES, store = Store.YES)
  public String isPrivateForumValue() {
    return privateForum+"";
  }

  public void setPrivateForum(boolean privateForum) {
    this.privateForum = privateForum;
  }

  @Transient
  public ForumEntry getRootPost() {
    if (getDepth() < 3)
      return this;
    else
      return getParent();
  }

  @OneToMany(cascade = CascadeType.REMOVE, targetEntity = ForumEntry.class)
  @JoinColumn(name = "parent")
  public Set getChildren() {
    return super.getChildren();
  }

  public boolean hasPermissionFor(User u, Permission p) {
    return PermissionCache.hasPermissionFor(u,this,p);
  }


  public boolean hasPermissionForUnCached(User u, Permission p) {
    boolean result = false;
    if (u != null)
      result = super.hasPermissionFor(u, p);

    if (!result) {
      if (ForumPermissions.forum_canRead.equals(p)) {
        result = !isPrivateForum();
      } else if (ForumPermissions.forum_canPost.equals(p)) {
        result = isPostToAll() || (u != null && isPostForLogged());
      } else if (ForumPermissions.forum_canEdit.equals(p)) {
        result = u != null && u.equals(getAuthor());
      }
    }
    return result;
  }

  public JSONObject jsonify(){
    JSONObject jso = ReflectionUtilities.jsonify(this,"title","content","active","order");

    if (author!=null) jso.element("author",author.jsonify());
    if (parent!=null) jso.element("parentId",parent.getId());
    if (postedOn!=null) jso.element("postedOn",getPostedOn().getTime());

    return jso;

  }


  @Transient
  public String getAbstractForIndexing() {
    return getContentForIndexing();
  }

  @Transient
  @Fields({
  @Field(name = "fullcontent", analyzer = @Analyzer(impl = StopAnalyzer.class)),
  @Field(name = "content")
          })
  private String getContentForIndexing() {
    return JSP.w(getTitle()) +" "+JSP.w(getContent()); 
  }
}
