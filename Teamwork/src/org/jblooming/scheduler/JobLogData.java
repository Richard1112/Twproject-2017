package org.jblooming.scheduler;

import java.util.Date;
import java.io.Serializable;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class JobLogData {
  public Date date;
  public Serializable id;
  public String notes="";
  public boolean successfull;
}
