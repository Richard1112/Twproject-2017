package com.twproject.meeting;

import com.opnlb.fulltext.Indexable;
import com.twproject.agenda.Event;
import com.twproject.messaging.board.Board;
import com.twproject.operator.TeamworkOperator;
import com.twproject.utilities.TeamworkComparators;
import org.apache.lucene.analysis.core.StopAnalyzer;
import org.hibernate.annotations.ForeignKey;
import org.hibernate.annotations.Index;
import org.hibernate.annotations.Type;
import org.hibernate.search.annotations.*;
import org.hibernate.search.bridge.builtin.IntegerBridge;
import org.jblooming.ontology.SecuredLoggableHideableSupport;
import org.jblooming.operator.Operator;
import org.jblooming.operator.User;
import org.jblooming.security.Area;
import org.jblooming.security.Permission;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.JSP;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;

import javax.persistence.*;
import java.io.Serializable;
import java.util.*;


/**
 * @author Roberto Baldi rbaldi@open-lab.com
 *         Date: 3-ago-2007 : 14.21.37
 */
@Entity
@Table(name = "twk_meeting")
@Indexed(index = "fulltext")
public class Meeting extends SecuredLoggableHideableSupport implements Indexable {

  // Event of the Meeting
  private Event event;

  private Board board;

  // set of DiscussionPoint
  private Set<DiscussionPoint> discussionPoints = new HashSet<DiscussionPoint>();

  @Id
  @Type(type = "int")
  @GeneratedValue(strategy = GenerationType.AUTO)
  @DocumentId
  @FieldBridge(impl = IntegerBridge.class)
  public Serializable getId() {
    return super.getId();
  }

  @ManyToOne(targetEntity = TeamworkOperator.class)
  @ForeignKey(name = "fk_meeting_owner")
  @Index(name = "idx_meeting_owner")
  @JoinColumn(name = "ownerx")
  public Operator getOwner() {
    return super.getOwner();
  }

  @OneToMany(cascade = CascadeType.REMOVE, targetEntity = DiscussionPoint.class)
  @OrderBy("orderBy")
  @JoinColumn(name = "meeting")
  public Set<DiscussionPoint> getDiscussionPoints() {
    return discussionPoints;
  }

  public void setDiscussionPoints(Set<DiscussionPoint> discussionPoints) {
    this.discussionPoints = discussionPoints;
  }


  @OneToOne(targetEntity = Event.class, mappedBy = "meeting")
  public Event getEvent() {
    return event;
  }

  public void setEvent(Event event) {
    this.event = event;
  }


  @ManyToOne(targetEntity = Board.class)
  @ForeignKey(name = "fk_meeting_board")
  @Index(name = "idx_meeting_board")
  @JoinColumn(name = "board")
  public Board getBoard() {
    return board;
  }

  public void setBoard(Board board) {
    this.board = board;
  }

  @Transient
  public List<DiscussionPoint> getDiscussionPointsOrdered() {
    List<DiscussionPoint> discussionPointsOrd = new ArrayList(getDiscussionPoints());
    Collections.sort(discussionPointsOrd, new TeamworkComparators.DiscussionPointComparator());
    return discussionPointsOrd;
  }



  @Transient
  public String getAbstractForIndexing() {
    String afi = "";
    for (DiscussionPoint dp: getDiscussionPoints()) {
      afi = afi + JSP.w(dp.getTitle())+" M#"+getId()+"#\n"+
              (dp.getLead()!=null ? dp.getLead().getDisplayName()+"\n" : "")+
              JSP.w(dp.getMinute());
    }
    return afi;
  }

  @Transient
  @Fields({
          @Field(name = "fullcontent", analyzer = @Analyzer(impl = StopAnalyzer.class)),
          @Field(name = "content")
          })
  private String getContentForIndexing() {
    return getAbstractForIndexing();
  }

  /**
   * Careful: this is a fake implementation: it is meant to be used only for checking read permissions
   */
  public boolean hasPermissionFor(User u, Permission p) {
    return getEvent().hasPermissionFor(u,p);
   }


  @IndexedEmbedded
  @Transient
  public Area getArea() {
    Event ev = getEvent();
    if (ev==null) {
      Tracer.platformLogger.error("Che skifo un meeting senza event. Meeting id:" + getId());
      return null;
    }
    return ev.getArea();
  }



  public static Meeting load(int id) throws FindByPrimaryKeyException {
    return (Meeting) PersistenceHome.findByPrimaryKey(Meeting.class,id);
  }

}
