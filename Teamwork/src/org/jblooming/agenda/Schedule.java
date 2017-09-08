package org.jblooming.agenda;

import org.jblooming.ApplicationException;
import org.jblooming.ontology.Identifiable;

import java.util.Collection;
import java.util.Date;

import net.sf.json.JSONObject;


public interface Schedule extends Identifiable {

  Date getStartDate();
  Date getEndDate();

  int getStartTimeInMillis();
  long getDurationInMillis();

  int getFrequency();
  int getRepetitions();
  //String getExeptions();

  /*
   * Return true if the specified date is in ANY of schedule; false oterwhise
   *
   */
  boolean contains(Date date);

  boolean overlap(Period p);
  long getValidityStartTime();
  long getValidityEndTime();
  Date getValidityStartDate();
  Date getValidityEndDate();
  Period getPeriod();
  Date getNextFireDate();
  Date getNextDateAfter(Date afterTime);
  Date getPreviousDateBefore(Date beforeTime);
  long getNextFireTime();
  long getNextFireTimeAfter(long afterTime);
  long getPreviousFireTimeBefore(long beforeTime);
  
  JSONObject jsonify();

}


