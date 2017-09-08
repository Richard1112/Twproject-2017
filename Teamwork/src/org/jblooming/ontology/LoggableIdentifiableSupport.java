package org.jblooming.ontology;

import org.hibernate.search.annotations.Field;
import org.hibernate.search.annotations.Store;
import org.jblooming.persistence.hibernate.PersistenceContext;

import javax.persistence.MappedSuperclass;
import java.io.Serializable;
import java.util.Date;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */
@MappedSuperclass
public abstract class LoggableIdentifiableSupport extends IdentifiableSupport implements LoggableIdentifiable {

  protected Date lastModified;
  protected String lastModifier;
  protected String creator;
  protected Date creationDate;

  public LoggableIdentifiableSupport() {
  }


  public LoggableIdentifiableSupport(Serializable id) {
    this.id = id;
  }

  @Field(store= Store.NO,name = "content") //aggiunto da robik il 24/1/2014 per forzare la re-indicizzazione quando si cambia l'oggetto
  private String lastModifiedMillis() {return getLastModified()==null?"":getLastModified().getTime()+"";
  }

  public Date getLastModified() {
    return lastModified;
  }

  public void setLastModified(Date lastModified) {
    this.lastModified = lastModified;
  }

  public String getLastModifier() {
    return lastModifier;
  }

  public void setLastModifier(String lastModifier) {
    this.lastModifier = lastModifier;
  }

  public String getCreator() {
    return creator;
  }

  public void setCreator(String creator) {
    this.creator = creator;
  }


  public Date getCreationDate() {
    return creationDate;
  }

  public void setCreationDate(Date creationDate) {
    this.creationDate = creationDate;
  }


}
