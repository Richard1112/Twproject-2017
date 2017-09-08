package org.jblooming.persistence.hibernate;


import org.hibernate.*;
import org.hibernate.Session;
import org.hibernate.cfg.Configuration;
import org.jblooming.ApplicationException;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.ontology.Identifiable;
import org.jblooming.ontology.LoggableIdentifiable;
import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.operator.Operator;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.RemoveException;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.security.Securable;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.PlatformConfiguration;
import org.jblooming.waf.settings.PersistenceConfiguration;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.util.Date;
import java.util.List;
import java.util.Map;

public class HibernateFactory {

  private static boolean debugObjectLoadTime = false;


  /**
   * @deprecated
   */
  public static Session getSession() {
    return PersistenceContext.getDefaultPersistenceContext().session;
  }

   /**
   * @deprecated
   */
  public static void checkPoint() {
    PersistenceContext.getDefaultPersistenceContext().checkPoint();
  }

  /**
   * @deprecated use the constructor of PersistenceContext
   */
  public static PersistenceContext newFreeSession() {
    return new PersistenceContext();
  }

  /**
   * @deprecated use it on PersistenceContext
   */
  public static PersistenceContext getDefaultPersistenceContext() {
    return PersistenceContext.getDefaultPersistenceContext();
  }

  /**
   * @deprecated use it on PersistenceConfiguration selecting the appropiate config
   */
  public static SessionFactory getSessionFactory() {
    return PersistenceConfiguration.getDefaultPersistenceConfiguration().getSessionFactory();
  }

  public static void store(Identifiable i, PersistenceContext pc) throws StoreException {
    HibernateUtilities.incrementIdIfNew(i);
    try {
        save(pc, i);
    } catch (Exception ex) {
      throw new StoreException("HibernateFactory store exception saving " + (i != null ? i.getClass().getSimpleName() + " " + i.getName() + " " + i.getId() : ""), ex);
    }
  }

  private static void save(PersistenceContext pc, Identifiable i) throws HibernateException {
    Session s=pc.session;
    final Date absoluteNow = new Date();
    Operator op =  PersistenceContext.threadLocalPersistenceContextCarrier.get() != null ? PersistenceContext.threadLocalPersistenceContextCarrier.get().getOperator() : null;

    // object is new: INSERT
    if (!s.contains(i)) {
      if (i instanceof LoggableIdentifiable) {
        LoggableIdentifiable li = (LoggableIdentifiable) i;
        if (op != null)
          li.setCreator(op.getFullname());
        if (li.getCreationDate()==null)  // added by bicch on 7/6/2010
          li.setCreationDate(absoluteNow);
      }
      if (i instanceof Securable) {
        if (op != null)
          ((Securable) i).setOwner(op);
      }
    }
    // object is already saved: UPDATE
    if (i instanceof LoggableIdentifiable && !PersistenceHome.isUpgradingSchema) {
      LoggableIdentifiable li = (LoggableIdentifiable) i;
      if (op != null)
        li.setLastModifier(op.getFullname());
      li.setLastModified(absoluteNow);
    }

    //se l'oggetto estende UniqueDeferredStore si mette in coda per l'aggiornamento
    //if (i instanceof UniqueDeferredStore){
    //  ((UniqueDeferredStore) i).enqueueForDefferredStore();
    //}


    boolean b = ApplicationState.platformConfiguration != null;
    if (b)
      if (!PlatformConfiguration.StringConversionType.NONE.equals(pc.persistenceConfiguration.stringPrimitiveFieldsConversion) ||
              !PlatformConfiguration.StringConversionType.NONE.equals(pc.persistenceConfiguration.searchStringParametersConversion)) {

        Map<String, Field> dif = ReflectionUtilities.getDeclaredInheritedFields(i.getClass());
        for (Field f : dif.values()) {
          if (f.getType().equals(String.class)) {
            try {
              String value = (String) ReflectionUtilities.getFieldValue(f.getName(), i);
              if (value != null) {
                if (pc.persistenceConfiguration.stringPrimitiveFieldsConversion.equals(PlatformConfiguration.StringConversionType.LOWER))
                  value = value.toLowerCase();
                else
                  value = value.toUpperCase();
                ReflectionUtilities.setFieldValue(f.getName(), i, value);
              }
            } catch (Exception e) {
              throw new PlatformRuntimeException(e);
            }
          }
        }
      }

    s.save(i);
  }

  public static void remove(IdentifiableSupport i) throws RemoveException {
    remove(i, PersistenceContext.get(i).session);
  }

  public static void remove(IdentifiableSupport i, Session s) throws RemoveException {
    try {
      s.delete(i);
    } catch (Exception ex) {
      throw new RemoveException("HibernateFactory remove exception.", ex);
    }
  }

  public static Object load(Class cls, Serializable id) throws ApplicationException {
    try {
      return PersistenceContext.get(cls).session.load(cls, id);
    } catch (Exception ex) {
      throw new ApplicationException("HibernateFactory session close exception.", ex);
    }
  }

  public static Identifiable findByPrimaryKey(Class cls, Serializable id) throws FindByPrimaryKeyException {
    return findByPrimaryKey(cls, id, PersistenceContext.get(cls));
  }

  public static Identifiable findByPrimaryKey(Class cls, Serializable id, PersistenceContext pc) throws FindByPrimaryKeyException {

    if (PersistenceHome.NEW_EMPTY_ID.equals(id))
      return null;

    if (id == null)
      throw new FindByPrimaryKeyException("HibernateFactory::findByPrimaryKey: passed id null.");

    //test whether id is of the rigth class
    if (id instanceof String) {
      if (HibernateUtilities.isIdInteger(cls))
        id = new Integer((String) id);
    }

    long timeToLoadObj = 0;
    java.lang.Object o = null;

    if (debugObjectLoadTime) {
      timeToLoadObj = System.currentTimeMillis();
      Tracer.platformLogger.debug("HibernateScalarQueryCursorServer::findByPrimaryKey  before load" + timeToLoadObj);
    }

    o = pc.session.get(cls, id);

    if (debugObjectLoadTime) {
      Tracer.platformLogger.debug("HibernateScalarQueryCursorServer::findByPrimaryKey time to load: " + (System.currentTimeMillis() - timeToLoadObj) / 1000);
    }

    return (Identifiable) o;
  }

  public static void addArgs(Object[] args, Query query) throws HibernateException {
    java.lang.Object arg;
    for (int i = 0; i < args.length; ++i) {
      arg = args[i];
      if (arg != null) {
        if (Double.class.isAssignableFrom(arg.getClass()))
          query.setDouble(i, (Double) arg);
        else if (Integer.class.isAssignableFrom(arg.getClass()))
          query.setInteger(i, (Integer) arg);
        else if (Date.class.isAssignableFrom(arg.getClass()))
          query.setTimestamp(i, (Date) arg);
        else if (Boolean.class.isAssignableFrom(arg.getClass()))
          query.setBoolean(i, (Boolean) arg);
        else if (String.class.isAssignableFrom(arg.getClass()))
          query.setString(i, (String) arg);
        else
          query.setParameter(i, arg);
      }
    }
  }

  public static void addArgs(List args, Query query) throws HibernateException {
    if (args != null) {
      Object[] objs = args.toArray();
      addArgs(objs, query);
    }
  }

  public static void initialize(Object o) throws HibernateException {
    Hibernate.initialize(o);
  }

  public static boolean isInitialized(Object o) {
    return Hibernate.isInitialized(o);
  }

  /**
   *
   * @deprecated use PersistenceConfiguration.getDefaultPersistenceConfiguration().getHibernateConfiguration() or
   */
  public static Configuration getConfig() {
    return PersistenceConfiguration.getDefaultPersistenceConfiguration().getHibernateConfiguration();    
  }

}