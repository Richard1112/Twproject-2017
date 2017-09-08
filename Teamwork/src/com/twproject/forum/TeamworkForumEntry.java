package com.twproject.forum;

import com.opnlb.fulltext.Indexable;
import com.opnlb.website.forum.ForumEntry;
import com.twproject.task.Task;
import org.hibernate.annotations.ForeignKey;
import org.hibernate.annotations.Type;
import org.hibernate.search.annotations.Indexed;
import org.hibernate.search.annotations.IndexedEmbedded;
import org.hibernate.search.annotations.DocumentId;
import org.hibernate.search.annotations.FieldBridge;
import org.hibernate.search.bridge.builtin.IntegerBridge;
import org.jblooming.security.Area;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;

import javax.persistence.*;
import java.io.Serializable;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Oct 29, 2007
 * Time: 5:01:04 PM
 */
@Entity
@DiscriminatorValue("TFE")
@Indexed(index = "fulltext")
public class TeamworkForumEntry extends ForumEntry implements Indexable {

  private Task task;

  @ManyToOne(targetEntity = Task.class)
  @ForeignKey(name = "fk_forum_task")
  @JoinColumn(name = "task")
  public Task getTask() {
    return task;
  }

  public void setTask(Task task) {
    this.task = task;
  }

  @IndexedEmbedded
  @Transient
  public Area getArea() {
    if (getTask()!=null)
      return getTask().getArea();
    else
      return null;
  }

  public static TeamworkForumEntry load(Serializable s) throws FindByPrimaryKeyException {
    return (TeamworkForumEntry) PersistenceHome.findByPrimaryKey(TeamworkForumEntry.class,s);
  }
}
