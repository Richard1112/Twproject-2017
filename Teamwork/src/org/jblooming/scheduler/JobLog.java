package org.jblooming.scheduler;

import org.jblooming.ontology.Identifiable;
import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.persistence.PersistenceHome;

import java.util.Date;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class JobLog extends IdentifiableSupport {

  private Date date;
  private Job job;
  private boolean successfull;
  private String notes = "";

  public JobLog() {
  }

  public boolean isSuccessfull() {
    return successfull;
  }

  public boolean equals(Object o) {
    return this.getIntId()==(((Identifiable) o).getIntId());
  }

  public int hashCode() {
    int result = 0;
    if (PersistenceHome.NEW_EMPTY_ID.equals(id))
      result = System.identityHashCode(this);
    else
      result = (id + "").hashCode();

    return result;
  }

  public int compareTo(Object o) {
    if (this == o)
      return 0;
    if (o == null)
      return -1;
    else
      return this.getDate().compareTo(((JobLog) o).getDate());
  }

  public Date getDate() {
    return date;
  }

  public void setDate(Date date) {
    this.date = date;
  }

  public Job getJob() {
    return job;
  }

  public void setJob(Job job) {
    this.job = job;
  }

  public void setSuccessfull(boolean successfull) {
    this.successfull = successfull;
  }

  public String getNotes() {
    return notes;
  }

  public void setNotes(String notes) {
    this.notes = notes;
  }
}
