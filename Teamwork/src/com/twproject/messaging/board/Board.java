package com.twproject.messaging.board;

import com.opnlb.fulltext.Indexable;
import com.twproject.messaging.stickyNote.StickyNote;
import org.apache.lucene.analysis.core.StopAnalyzer;
import org.hibernate.search.annotations.*;
import org.hibernate.search.bridge.builtin.StringBridge;
import org.hibernate.annotations.Type;
import org.jblooming.logging.Sniffable;
import org.jblooming.ontology.SecuredSupportWithArea;
import org.jblooming.utilities.JSP;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.PersistenceHome;

import javax.persistence.Id;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;
import java.io.Serializable;

/**
 * @author Pietro Polsinelli : ppolsinelli@open-lab.com
 */
@Indexed(index = "fulltext")
public class Board extends SecuredSupportWithArea implements Sniffable, Indexable {

  private String name;
  private String description;

  private boolean active = true;

  private Date lastPostedOn;

  private Set<StickyNote> stickyNotes = new HashSet<StickyNote>();

  public BoardBricks bricks = new BoardBricks(this);


  public Board() {
  }


  @DocumentId
  @FieldBridge(impl= StringBridge.class)

   public Serializable getId() {
       return super.getId();
   }


  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }


  /**
   * is inverse
   */
  public Set<StickyNote> getStickyNotes() {
    return stickyNotes;
  }

  private void setStickyNotes(Set<StickyNote> stickyNotes) {
    this.stickyNotes = stickyNotes;
  }


  public boolean isActive() {
    return active;
  }

  public void setActive(boolean active) {
    this.active = active;
  }

  public Date getLastPostedOn() {
    return lastPostedOn;
  }

  public void setLastPostedOn(Date lastPostedOn) {
    this.lastPostedOn = lastPostedOn;
  }

  public String getAbstractForIndexing() {

    String ad = "";
    for (StickyNote note : getStickyNotes()) {
      ad = ad + JSP.w(note.getName() + "\n") + JSP.w(note.getMessage());
    }

    return JSP.w(getName() + " B#"+getId()+"#\n" + JSP.w(ad));
  }

  @Fields({
  @Field(name = "fullcontent", analyzer = @Analyzer(impl = StopAnalyzer.class)),
  @Field(name = "content")
          })
  private String getContentForIndexing() {
    return getAbstractForIndexing();
  }

 public static Board load(Serializable mainObjectId) throws FindByPrimaryKeyException {
    return (Board) PersistenceHome.findByPrimaryKey(Board.class, mainObjectId);
  }
}
