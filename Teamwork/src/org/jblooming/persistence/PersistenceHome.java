package org.jblooming.persistence;

import org.hibernate.Hibernate;
import org.hibernate.HibernateException;
import org.hibernate.LockMode;
import org.hibernate.proxy.HibernateProxy;
import org.jblooming.ontology.Identifiable;
import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.exceptions.*;
import org.jblooming.persistence.hibernate.HibernateFactory;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.ReflectionUtilities;

import java.io.Serializable;
import java.util.List;

/**
 * Handles the persistence of all identifiable objects
 */
public class PersistenceHome {

  public static final NewEmptyId NEW_EMPTY_ID = new NewEmptyId();
  public static boolean isUpgradingSchema=false; //è settato a true quando si sta eseguendo un upgrade dello schema. Fa si che non si modifichino i lastChangeDate etc.

  /**
   * @param clazz
   * @param id
   * @return
   * @throws FindByPrimaryKeyException if not found
   */
  public static Identifiable findByPrimaryKey(Class<? extends Identifiable> clazz, int id) throws FindByPrimaryKeyException {
    return HibernateFactory.findByPrimaryKey(clazz, id);
  }

  public static Identifiable findByPrimaryKey(Class<? extends Identifiable> clazz, Serializable id) throws FindByPrimaryKeyException {
    return HibernateFactory.findByPrimaryKey(clazz, id);
  }

  public static Identifiable findByPrimaryKeyNullIfError(Class<? extends Identifiable> clazz, Serializable id)  {
    Identifiable ret=null;
    try {
      ret = HibernateFactory.findByPrimaryKey(clazz, id);
    } catch (FindByPrimaryKeyException e) {
      e.printStackTrace();
    }
    return ret;
  }

  public static Identifiable findFirst(Class<? extends Identifiable> clazz, String field, Object value) throws PersistenceException {
    final OqlQuery oqlQuery = new OqlQuery("from " + clazz.getName() + " as obj where obj." + field + " = :aparam");
    oqlQuery.getQuery().setMaxResults(1);
    try {
      List list = oqlQuery.getQuery().setParameter("aparam", value).list();
      if (JSP.ex(list))
        return (Identifiable) list.get(0);
      else
        return null;
    } catch (HibernateException e) {
      throw new PersistenceException(oqlQuery.doDebug(new Object[]{value}),e);
    }
  }



  public static Identifiable findUnique(Class<? extends Identifiable> clazz, String field, Object value) throws PersistenceException {
    final OqlQuery oqlQuery = new OqlQuery("from " + clazz.getName() + " as obj where obj." + field + " = :aparam");
    try {
      return (Identifiable) oqlQuery.getQuery().setParameter("aparam", value).uniqueResult();
    } catch (HibernateException e) {
      throw new PersistenceException(oqlQuery.doDebug(new Object[]{value}),e);
    }
  }

  public static Identifiable findUnique(Class<? extends Identifiable> clazz, String field, Object value, PersistenceContext pc) throws PersistenceException {
    final OqlQuery oqlQuery = new OqlQuery("from " + clazz.getName() + " as obj where obj." + field + " = :aparam", pc);
    try {
      return (Identifiable) oqlQuery.getQuery().setParameter("aparam", value).uniqueResult();
    } catch (HibernateException e) {
      throw new PersistenceException(oqlQuery.doDebug(new Object[]{value}),e);
    }
  }

  public static Object findUniqueObject(Class clazz, String field, Object value, PersistenceContext pc) throws PersistenceException {
    final OqlQuery oqlQuery = new OqlQuery("from " + clazz.getName() + " as obj where obj." + field + " = :aparam", pc);
    try {
      return oqlQuery.getQuery().setParameter("aparam", value).uniqueResult();
    } catch (HibernateException e) {
      throw new PersistenceException(oqlQuery.doDebug(new Object[]{value}),e);
    }
  }

  

  public static Identifiable findUniqueNullIfEmpty(Class<? extends Identifiable> clazz, String field, Object value) {
    Identifiable res = null;
    try {
      res = findUnique(clazz, field, value);
    } catch (PersistenceException e) {
    }
    return res;
  }

  public static Identifiable findUniqueNullIfEmpty(Class<? extends Identifiable> clazz, String field, Object value, PersistenceContext pc) {
    Identifiable res = null;
    try {
      res = findUnique(clazz, field, value, pc);
    } catch (PersistenceException e) {
    }
    return res;
  }

  public static Identifiable findByPrimaryKey(Class<? extends Identifiable> clazz, Serializable id, PersistenceContext pc) throws FindByPrimaryKeyException {
    return HibernateFactory.findByPrimaryKey(clazz, id, pc);
  }

  /**
   * @param i
   * @throws StoreException if anything goes wrong, say the database is unreachable, the exception is wrapped in this
   * @deprecated use i.store()
   */
  public static void store(IdentifiableSupport i) throws StoreException {
    i.store();
  }

  public static void store(Identifiable i, PersistenceContext pc) throws StoreException {
    HibernateFactory.store(i, pc);
  }



  public static void refresh(IdentifiableSupport i) {
    PersistenceContext.get(i).session.refresh(i);
  }

  public static void reAssociateUnmodified(IdentifiableSupport i){
    PersistenceContext.get(i).session.lock(i, LockMode.NONE);
  }

  public static void remove(IdentifiableSupport i, PersistenceContext pc) throws RemoveException {
    HibernateFactory.remove(i, pc.session);
  }

  public static boolean isInitialized(Object o) {
    return HibernateFactory.isInitialized(o);
  }

  public static void initialize(Object o) throws HibernateException {
    HibernateFactory.initialize(o);
  }

  public static <T> T initializeAndUnproxy(T entity) {
    if (entity == null) {
      throw new
        NullPointerException("Entity passed for initialization is null");
    }

    Hibernate.initialize(entity);
    if (entity instanceof HibernateProxy) {
      entity = (T) ((HibernateProxy) entity).getHibernateLazyInitializer().getImplementation();
    }
    return entity;
  }

  public static String dePackage(String className) {
    return ReflectionUtilities.deProxy(className).substring(ReflectionUtilities.deProxy(className).lastIndexOf('.') + 1);
  }


  public static class NewEmptyId implements Serializable {

    static final String newEmptyId = "newEmptyId";

    public boolean equals(Object o) {
      return newEmptyId.equals(o + "");
    }

    public String toString() {
      return newEmptyId;
    }

  }

}
