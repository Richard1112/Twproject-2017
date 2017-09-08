package com.twproject.worklog;

import com.twproject.task.Assignment;
import com.twproject.task.Issue;
import com.twproject.worklog.businessLogic.WorklogBricks;
import org.hibernate.search.annotations.Indexed;
import org.jblooming.operator.User;
import org.jblooming.persistence.exceptions.RemoveException;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.security.*;
import org.jblooming.waf.settings.I18n;

import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import javax.persistence.Transient;

import net.sf.json.JSONObject;

@Entity
@Indexed(index = "fulltext")
@DiscriminatorValue("W")
public class Worklog extends WorklogSupport {

  public WorklogBricks bricks = new WorklogBricks(this);

  public Worklog() {
  }

  @Transient
  public void store(PersistenceContext pc) throws StoreException {
    super.store(pc);

    //mark assign to be recalculated
    getAssig().markAsDirty();

    //mark issue to be recalculated
    if(getIssue()!=null)
      getIssue().markAsDirty();

  }

  @Transient
  public void remove(PersistenceContext pc) throws RemoveException {
    Assignment assig = getAssig();
    Issue iss=getIssue();
    super.remove(pc);

    //deve rinfrescare il wl sull'assegnazione
    assig.markAsDirty();

    //mark issue to be recalculated
    if(iss!=null){
      iss.markAsDirty();
    }
  }

  /**
   * NON controlla se il WL è tuo o meno, ma controlla solo il permesso sull'assegnazione
   * Da usare per le operazioni di "manageWl".
   * @see com.twproject.worklog.businessLogic.WorklogBricks
   * @param u
   * @param p
   * @return
   */
  @Transient
  public boolean hasPermissionFor(User u, Permission p) {
    boolean ret = false;
    if (assig != null) {
      ret = assig.hasPermissionFor(u, p);
    }
    return ret;
  }

  public static Worklog load(String id) throws FindByPrimaryKeyException {
    return (Worklog) PersistenceHome.findByPrimaryKey(Worklog.class, id);
  }

  public JSONObject jsonify() {

    JSONObject ret = super.jsonify();

    ret.element("id", getId());
    ret.element("duration", getDuration());
    ret.element("action", getAction());
    ret.element("insertedMillis", getInserted().getTime());
    Assignment assig= getAssig();
    ret.element("assId", assig.getId());
    ret.element("totalWl", assig.getWorklogDone());
    ret.element("taskId", assig.getTask().getId());
    //ret.element("taskName", assig.getTask().getDisplayName());
    ret.element("taskName", assig.getTask().getName());
    ret.element("taskCode", assig.getTask().getCode());
    ret.element("resId", assig.getResource().getId());
    ret.element("resName", assig.getResource().getDisplayName());
    ret.element("resAvatarUrl", assig.getResource().bricks.getAvatarImageUrl());
    ret.element("issueId",getIssue()==null?null:getIssue().getId());

    WorklogStatus worklogStatus = getStatus();
    ret.element("stsName", worklogStatus ==null?null:worklogStatus.getDescription());
    ret.element("stsColor", worklogStatus ==null?null: I18n.get("COLOR_WORKLOG_STATUS_" + worklogStatus.getIntValue()));

    ret.element("customField1", getCustomField1());
    ret.element("customField2", getCustomField2());
    ret.element("customField3", getCustomField3());
    ret.element("customField4", getCustomField4());


    return ret;
  }


}