package org.jblooming.logging;

import org.hibernate.CallbackException;
import org.hibernate.Session;
import org.hibernate.HibernateException;
import org.jblooming.persistence.hibernate.HibernateFactory;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.operator.Operator;
import org.jblooming.utilities.JSP;
import org.jblooming.PlatformRuntimeException;

import javax.persistence.Entity;
import javax.persistence.Table;
import java.sql.Connection;
import java.io.Serializable;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class AuditLogHelper {

  public static void logEvent(
          String message,
          Sniffable entity,
          Operator logged,
          Connection connection)
          throws CallbackException {

    logEvent(message,
            entity,
            entity.getId(),
            logged,
            connection);

  }

  public static void logEvent(
          String message,
          Sniffable entity,
          Serializable entityId,
          Operator logged,
          Connection connection)
          throws CallbackException {

    AuditLogRecord record = new AuditLogRecord(message, entityId, entity.getClass().getName(), logged);
    // AuditLogRecord record = new AuditLogRecord(message, "pippo", AuditLog.class, new Operator());
    logEvent(record);
  }

  public static void logEvent(AuditLogRecord record) {
    if (JSP.ex(record.getData())) {
      Session session = null;
      try {
        session = HibernateFactory.getSessionFactory().openSession();
        session.beginTransaction();
        session.save(record);
        session.getTransaction().commit();
      } catch (Exception ex) {
        if (session!=null && session.getTransaction() != null)
          session.getTransaction().rollback();
        throw new CallbackException(ex);
      } finally {
        try {
          if (session!=null)
            session.close();
        } catch (Throwable e) {
          throw new PlatformRuntimeException(e);
        }
      }
    }
  }


}