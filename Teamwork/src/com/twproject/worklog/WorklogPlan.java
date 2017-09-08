package com.twproject.worklog;

import org.hibernate.search.annotations.Indexed;
import org.hibernate.search.annotations.DocumentId;
import org.hibernate.search.annotations.FieldBridge;
import org.hibernate.search.bridge.builtin.StringBridge;
import org.hibernate.annotations.Type;

import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.Id;
import java.io.Serializable;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Sep 18, 2008
 * Time: 11:55:47 AM
 */
@Entity
@Indexed(index = "fulltext")
@DiscriminatorValue("P")
public class WorklogPlan extends WorklogSupport {

  public WorklogPlan() {
  }


  /**
   * @param duration if a duration <=0 is set to 1 millis.
   *                 This beautifull hack is used mainly in plan in order to manage day with plan with "zero" hours. We can't use 0 because all the plan related methods return integer arrays:
   *                 no way do discern 0 set by user or 0 filled by the system. So DO NOT CHANGE!
   */
//  public void setDuration(long duration) {
//    if (duration <= 0)
//      duration = 1;
//    super.setDuration(duration);
//  }
}
