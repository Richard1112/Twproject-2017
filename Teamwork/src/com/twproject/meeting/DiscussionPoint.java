package com.twproject.meeting;

import com.twproject.agenda.DiscussionPointStatus;
import com.twproject.document.TeamworkDocument;
import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Person;
import com.twproject.security.TeamworkPermissions;
import com.twproject.task.Task;
import net.sf.json.JSONArray;
import net.sf.json.JSONObject;
import org.hibernate.annotations.ForeignKey;
import org.hibernate.annotations.Index;
import org.hibernate.annotations.Type;
import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.ontology.SerializedList;
import org.jblooming.operator.Operator;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.view.PageState;

import javax.persistence.*;
import java.io.Serializable;
import java.util.Collection;

/**
 * @author Roberto Baldi rbaldi@open-lab.com
 *         Date: 3-ago-2007 : 14.21.37
 */
@Entity
@Table(name = "twk_disc_point")
public class DiscussionPoint extends IdentifiableSupport {

  // The lead of discussion
  private Person lead;
  // Title of discussion
  private String title;
  // Task that discussion is associated
  private Task task;
  private long timeScheduled;      // here is set the duration of the discussion point and ot the beginnig time.
  private DiscussionPointStatus status;
  private Operator owner;

  // Type of this discussionPoint
  private DiscussionPointType type;

  // List of documents
  // private Set<TeamworkDocument> documents= new HashSet<TeamworkDocument>();

  private Meeting meeting;
  private String minute;
  private SerializedList<Serializable> documents = new SerializedList();
  private int orderBy;

  @Id
  @Type(type = "int")
  @GeneratedValue(strategy = GenerationType.AUTO)
  public Serializable getId() {
    return super.getId();
  }


  @ManyToOne(targetEntity = Meeting.class)
  @ForeignKey(name = "fk_discp_meeting")
  @JoinColumn(name = "meeting")
  public Meeting getMeeting() {
    return meeting;
  }

  public void setMeeting(Meeting meeting) {
    this.meeting = meeting;
  }

  @ManyToOne(targetEntity = Person.class)
  @ForeignKey(name = "fk_discp_person")
  @Index(name = "idx_discp_person")
  @JoinColumn(name = "person")
  public Person getLead() {
    return lead;
  }


  public void setLead(Person lead) {
    this.lead = lead;
  }

  public String getTitle() {
    return title;
  }

  public void setTitle(String title) {
    this.title = title;
  }

  @ManyToOne(targetEntity = Task.class)
  @ForeignKey(name = "fk_discp_task")
  @Index(name = "idx_discp_task")
  @JoinColumn(name = "task")
  public Task getTask() {
    return task;
  }

  public void setTask(Task task) {
    this.task = task;
  }


  @ManyToOne(targetEntity = DiscussionPointType.class)
  @ForeignKey(name = "fk_discp_discPointType")
  @Index(name = "idx_discp_discPointType")
  @JoinColumn(name = "disc_point_type")
  public DiscussionPointType getType() {
    return type;
  }

  public void setType(DiscussionPointType type) {
    this.type = type;
  }


  public int getOrderBy() {
    return orderBy;
  }

  public void setOrderBy(int orderBy) {
    this.orderBy = orderBy;
  }

  public long getTimeScheduled() {
    return timeScheduled;
  }

  public void setTimeScheduled(long timeScheduled) {
    this.timeScheduled = timeScheduled;
  }

  @Lob
  @Type(type = "org.hibernate.type.TextType")
  public String getMinute() {
    return minute;
  }

  public void setMinute(String minute) {
    this.minute = minute;
  }


  @ManyToOne
  @ForeignKey(name = "fk_discp_discPointStatus")
  public DiscussionPointStatus getStatus() {
    return status;
  }

  public void setStatus(DiscussionPointStatus status) {
    this.status = status;
  }

  @Type(type = "org.jblooming.ontology.SerializedListType")
  @Column(name = "documentx")
  public SerializedList<Serializable> getDocuments() {
    return documents;
  }

  public void setDocuments(SerializedList documents) {
    this.documents = documents;
  }


  @ManyToOne(targetEntity = TeamworkOperator.class)
  @ForeignKey(name = "fk_discussionpoint_owner")
  public Operator getOwner() {
    return owner;
  }

  public void setOwner(Operator owner) {
    this.owner = owner;
  }

  public static DiscussionPoint load(Serializable s) throws FindByPrimaryKeyException {
    return (DiscussionPoint) PersistenceHome.findByPrimaryKey(DiscussionPoint.class, s);
  }


  public JSONObject jsonify()  {
    return jsonify(null);
  }

  public JSONObject jsonify(PageState pageState) {
    JSONObject ret = super.jsonify();
    ret.element("id", getId());
    ret.element("meeting", getMeeting().getId());
    ret.element("orderBy", getOrderBy());

    if (getLead() != null) {
      ret.element("lead", getLead().getId());
      ret.element("leadName", getLead().getDisplayName());
    }
//    ret.element("canManage", loggedOperator!=null && (loggedOperator.hasPermissionAsAdmin() || loggedOperator.equals(getAuthor().getMyself())));

    ret.element("title", getTitle());


    boolean canRead = true;
    if (getTask() != null) {
      ret.element("task", getTask().getId());
      //ret.element("taskName",getTask().getDisplayName());
      ret.element("taskName", getTask().getName());
      ret.element("taskCode", getTask().getCode());
    }
    ret.element("canRead", canRead);

    ret.element("timeScheduled", DateUtilities.getMillisInHoursMinutes(getTimeScheduled()));

    if (getStatus() != null) {
      ret.element("status", getStatus().getId());
      ret.element("statusDescription", getStatus().getDescription());
    }

    if (getType() != null) {
      ret.element("type", getType().getId());
      ret.element("typeDescription", getType().getDescription());
    }
    ret.element("minute", getMinute());

    if (getOwner() != null)
      ret.element("owner", getOwner().getId());


    if (pageState != null) {
      if (getTask() != null) {
        ret.element("canRead", task.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.task_canRead));
      }

      if (JSP.ex((Collection) getDocuments())) {
        JSONArray docs = new JSONArray();

        if (getTask() != null) {
          for (Serializable docId : getDocuments()) {
            try {
              TeamworkDocument doc = null;
              doc = TeamworkDocument.load(docId);
              if (doc != null && task.getDocuments().contains(doc)) {
                docs.add(doc.jsonify(pageState));
              }
            } catch (FindByPrimaryKeyException e) {
            }
          }
        }
        ret.element("documents", docs);
      }

    }

    return ret;
  }

}
