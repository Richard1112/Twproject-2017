package org.jblooming.flowork.samples;

import org.jbpm.graph.def.*;
import org.jbpm.graph.exe.*;
import org.jbpm.graph.node.*;
import org.jbpm.taskmgmt.def.*;
import org.jbpm.jpdl.exe.MilestoneInstance;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.persistence.hibernate.HibernateFactory;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.PlatformRuntimeException;

import java.util.*;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class MilestoneReachedActionHandler implements ActionHandler {

  private static final long serialVersionUID = 1L;

  private String milestoneName = null;
  //private String relativeTokenPath = null;

  public MilestoneReachedActionHandler() {
  }

  public MilestoneReachedActionHandler( String milestoneName) { //(, String relativeTokenPath
    this.milestoneName = milestoneName;
    //this.relativeTokenPath = relativeTokenPath;
  }

  public void execute(ExecutionContext ac) {

    MilestoneInstance mi = MilestoneInstance.getMilestoneInstance(milestoneName, ac.getToken());
    mi.setReached(true);
    mi.notifyListeners();


  }

  public String getMilestoneName() {
    return milestoneName;
  }
  public void setMilestoneName(String milestoneName) {
    this.milestoneName = milestoneName;
  }
 /* public String getRelativeTokenPath() {
    return relativeTokenPath;
  }
  public void setRelativeTokenPath(String relativeTokenPath) {
    this.relativeTokenPath = relativeTokenPath;
  }*/
}
