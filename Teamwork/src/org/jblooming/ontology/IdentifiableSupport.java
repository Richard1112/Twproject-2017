package org.jblooming.ontology;

import net.sf.json.JSONObject;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.oql.OqlQuery;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.RemoveException;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.hibernate.NonUniqueResultException;

import java.io.Serializable;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */

public abstract class IdentifiableSupport<K> implements Identifiable, Comparable {


  protected Serializable id;

  public IdentifiableSupport() {
  }

  public int getIntId() {
    if (getId() != null && getId() instanceof Integer)
      return (Integer) getId();
    else if (getId() != null && getId() instanceof String) {
      String stringId = (String) getId();
      if ("newEmptyId".equals(getId()))
        throw new PlatformRuntimeException("Using getIntId with wrong argument: id is 'newEmptyId'");
      else
        return Integer.parseInt(stringId);
    } else
      throw new PlatformRuntimeException("Using getIntId with wrong arguments: id is " + getId());
  }

  public Serializable getId() {
    return id;
  }

  public void setId(Serializable id) {
    this.id = id;
  }

  public void setIdAsNew() {
    this.id = PersistenceHome.NEW_EMPTY_ID;
  }


  public boolean isNew() {
    return id == null || PersistenceHome.NEW_EMPTY_ID.equals(id);
  }


  public boolean equals(Object o) {
    return this.compareTo(o) == 0;
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
    if (o == null || this.getId() == null || (o instanceof Identifiable && ((Identifiable) o).getId() == null) || (o instanceof Identifiable && PersistenceHome.NEW_EMPTY_ID.equals(((Identifiable) o).getId())))
      return -1;

    else {
      if (this.getId() != null && this.getId() instanceof Integer && o instanceof Identifiable && ((Identifiable) o).getId() instanceof Integer) {
        Integer integer = (Integer) (this.getId());
        Identifiable identifiable = ((Identifiable) o);
        Integer anotherInteger = (Integer) identifiable.getId();
        return integer.compareTo(anotherInteger);

      } else if (this.getId() != null && this.getId() instanceof Long && o instanceof Identifiable)
        return ((Long) (this.getId())).compareTo((Long) ((Identifiable) o).getId());
      else if (o instanceof Identifiable)
        return this.getId().toString().compareTo(((Identifiable) o).getId().toString());
      else
        return -1;
    }
  }

  public String getName() {
    return "" + id;
  }

  public String getDisplayName() {
    return getName();
  }

  public void store() throws StoreException {
    store(PersistenceContext.get(this));
  }

  public void store(PersistenceContext pc) throws StoreException {
    PersistenceHome.store(this, pc);
  }

  public void remove() throws RemoveException {
    remove(PersistenceContext.get(this));
  }             

  public void remove(PersistenceContext pc) throws RemoveException {
    PersistenceHome.remove(this, pc);
  }

  public void copy() throws StoreException {
    copy(PersistenceContext.get(this));
  }

  public void copy(PersistenceContext pc) {
    pc.session.evict(this);
    setIdAsNew();
  }

  public boolean isUnique(String attribute) {
    return isUnique(attribute, PersistenceContext.get(this));
  }

  public boolean isUnique(String attribute, PersistenceContext pc) {
    boolean ret;
    try {
      OqlQuery oqlQuery = new OqlQuery("select obj.id from " + ReflectionUtilities.deProxy(this.getClass().getName()) + " as obj where obj." + attribute + " = :aparam", pc);
      Object param = ReflectionUtilities.getFieldValue(attribute, this);
      if (param == null) {
        ret = true;
      } else {
        String id =  (String)oqlQuery.getQuery().setParameter("aparam", param).uniqueResult();
        if (id == null)
          ret = true;
        else {
          ret = id.equals(getId());
        }
      }
    } catch (NonUniqueResultException e) {
      ret = false;
    } catch (Throwable e) {
      throw new PlatformRuntimeException(e);
    }
    return ret;
  }





  //accoda l'oggetto per chiamare una callback prima del commit
  public void markAsDirty(){
    if (this instanceof HasDenormalizedFields)
      PersistenceContext.getDefaultPersistenceContext().enqueueForDefferredStore((HasDenormalizedFields) this);
  }



  public JSONObject jsonify(){

    //se ci sono oggetti da rinfrescare li rinfresca tutti prima
    PersistenceContext.getDefaultPersistenceContext().updateDeferred();

    JSONObject ret = new JSONObject();
    ret.element("id",getId());
    return ret;
  }

}