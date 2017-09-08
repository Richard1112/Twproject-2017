package org.jblooming.uidgen;

import org.hibernate.HibernateException;
import org.hibernate.SessionFactory;
import org.hibernate.persister.entity.SingleTableEntityPersister;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.persistence.hibernate.HibernateFactory;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.waf.settings.PersistenceConfiguration;
import org.jblooming.oql.OqlQuery;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.Map;


public class CounterHome {

  private static String uniqueIdTable;

  public static String nextSer(Class theClass) throws StoreException {
    return getNextId(theClass.getName(), true, PersistenceConfiguration.getFirstPersistenceConfiguration().getSessionFactory()) + "";
  }

  public static String nextSer(String name) throws StoreException {
    return getNextId(name, true, PersistenceConfiguration.getFirstPersistenceConfiguration().getSessionFactory()) + "";
  }

  public static int next(Class theClass) throws StoreException {
    return getNextId(theClass.getName(), true, PersistenceConfiguration.getFirstPersistenceConfiguration().getSessionFactory());
  }

  public static int next(String counterName) throws StoreException {
    return getNextId(counterName, true, PersistenceConfiguration.getFirstPersistenceConfiguration().getSessionFactory());
  }

  public static int next(String counterName, SessionFactory sf) throws StoreException {
    return getNextId(counterName, true, sf);
  }

  public static int getUniqueIdInCurrentTransaction(String counterName) throws StoreException {
    return getNextId(counterName, false, PersistenceConfiguration.getFirstPersistenceConfiguration().getSessionFactory());
  }

  public static int getUniqueIdInCurrentTransaction(String counterName, SessionFactory sf) throws StoreException {
    return getNextId(counterName, false, sf);
  }

  public static int getLastUsedNumber(String counterName) {
    OqlQuery oqlQuery = new OqlQuery("select c.value from " + Counter.class.getName() + " as c where c.name = :name");
    oqlQuery.getQuery().setString("name",counterName);
    return (Integer)oqlQuery.uniqueResultNullIfEmpty();
  }

  private static int getNextId(String counterName, boolean newTransaction, SessionFactory sf) throws StoreException {

    if (uniqueIdTable == null) {

      Map acm = null;
      try {
        acm = sf.getAllClassMetadata();
      } catch (HibernateException e) {
        throw new StoreException(e);
      }
      SingleTableEntityPersister ep = (SingleTableEntityPersister) acm.get(Counter.class.getName());
      uniqueIdTable = ep.getTableName();
    }
    int nextValue = -1;
    PreparedStatement select = null;
    PreparedStatement insert = null;
    PreparedStatement update = null;
    Connection connection = null;
    ResultSet result = null;
    Throwable exception = null;
    PersistenceContext pc = null;
    int originalAutoCommitSetting = -1;

    try {
      if (newTransaction) {
        pc = new PersistenceContext();
        connection = pc.session.connection();
        originalAutoCommitSetting = connection.getAutoCommit() ? 1 : 0;
        connection.setAutoCommit(false);
      } else {
        connection =PersistenceContext.get(Counter.class).session.connection();
      }


      update = connection.prepareStatement("update " + uniqueIdTable + " set valuex=valuex+1 where id = ?");
      update.setString(1, counterName);
      int updated = update.executeUpdate();
      if (updated <= 0){
        try {
          insert = connection.prepareStatement("insert into " + uniqueIdTable + " (id,valuex) values(?,?)");
          insert.setString(1, counterName);
          insert.setInt(2, 1);
          insert.executeUpdate();
        } catch (Throwable t) {
          connection.rollback();
        }
      }
      
      select = connection.prepareStatement("select valuex from " + uniqueIdTable + " where id = ?");
      select.setString(1, counterName);
      result = select.executeQuery();
      result.next();
      nextValue = result.getInt(1);

      if (newTransaction)
        connection.commit();

    } catch (Throwable ex) {
      exception = ex;
    }

    try {
      result.close();
    } catch (Throwable t) {
    }
    try {
      select.close();
    } catch (Throwable t) {
    }
    try {
      insert.close();
    } catch (Throwable t) {
    }
    try {
      update.close();
    } catch (Throwable t) {
    }

    try {
      if (exception != null && newTransaction) connection.rollback();
    } catch (Throwable t) {
    }

    try {
      if (originalAutoCommitSetting >= 0 && newTransaction) {
        if (originalAutoCommitSetting == 1)
          connection.setAutoCommit(true);
        else
          connection.setAutoCommit(false);
      }
    } catch (Throwable t) {
    }

    if (newTransaction) {
      try {
        pc.commitAndClose();
      } catch (Throwable t) {
      }
    }

    if (exception != null)
      throw new StoreException(exception);
    else
      return nextValue;
  }


  public static void setCounterSeed(String counterName, int newSeed) throws StoreException {
    SessionFactory sf = PersistenceConfiguration.getFirstPersistenceConfiguration().getSessionFactory();

    if (counterName != null) {
      if (uniqueIdTable == null) {

        Map acm = null;
        try {
          acm = sf.getAllClassMetadata();
        } catch (HibernateException e) {
          throw new StoreException(e);
        }
        SingleTableEntityPersister ep = (SingleTableEntityPersister) acm.get(Counter.class.getName());
        uniqueIdTable = ep.getTableName();
      }
      PreparedStatement select = null;
      PreparedStatement insert = null;
      PreparedStatement update = null;
      Connection connection = null;
      ResultSet result = null;
      Throwable exception = null;
      PersistenceContext pc = null;
      int originalAutoCommitSetting = -1;

      try {
        pc = new PersistenceContext();
        connection = pc.session.connection();
        originalAutoCommitSetting = connection.getAutoCommit() ? 1 : 0;
        connection.setAutoCommit(false);

        try {
          insert = connection.prepareStatement("insert into " + uniqueIdTable + " (id,valuex) values(?,?)");
          insert.setString(1, counterName);
          insert.setInt(2, newSeed);
          insert.executeUpdate();
        } catch (Throwable t) {
          connection.rollback();
        }

        update = connection.prepareStatement("update " + uniqueIdTable + " set valuex=" + newSeed + " where id = ?");
        update.setString(1, counterName);
        int updated = update.executeUpdate();
        if (updated <= 0)
          throw new Throwable("It was not possibile to update valuex for counter = " + counterName);

        connection.commit();

      } catch (Throwable ex) {
        exception = ex;
      }

      try {
        result.close();
      } catch (Throwable t) {
      }
      try {
        select.close();
      } catch (Throwable t) {
      }
      try {
        insert.close();
      } catch (Throwable t) {
      }
      try {
        update.close();
      } catch (Throwable t) {
      }

      try {
        if (exception != null) connection.rollback();
      } catch (Throwable t) {
      }

      try {
        if (originalAutoCommitSetting >= 0) {
          if (originalAutoCommitSetting == 1)
            connection.setAutoCommit(true);
          else
            connection.setAutoCommit(false);
        }
      } catch (Throwable t) {
      }

      try {
        pc.commitAndClose();
      } catch (Throwable t) {
      }

      if (exception != null)
        throw new StoreException(exception);
    }
  }


  public static Counter findByPrimaryKey(String id) throws FindByPrimaryKeyException {
    //Counter c = (Counter) PersistenceHome.findByPrimaryKey(Counter.class, id);
    //return c;

    if (id == null)
      throw new FindByPrimaryKeyException("HibernateFactory::findByPrimaryKey: passed null id.");

    Counter o = null;
    try {
      o = (Counter) PersistenceContext.get(Counter.class).session.load(Counter.class, id);
    } catch (HibernateException e) {
      throw new FindByPrimaryKeyException(e);
    }
    return o;
  }

}