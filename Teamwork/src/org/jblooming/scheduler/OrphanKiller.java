package org.jblooming.scheduler;

import org.jblooming.messaging.Listener;
import org.jblooming.messaging.Message;
import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.hibernate.HibernateUtilities;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.scheduler.ExecutableSupport;
import org.jblooming.scheduler.JobLogData;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.utilities.JSP;

import java.sql.ResultSet;
import java.sql.Statement;
import java.util.Date;
import java.util.List;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class OrphanKiller extends ExecutableSupport {

  public JobLogData run(JobLogData jobLogData) throws Exception {

    PersistenceContext pc = null;

    try {
      pc = PersistenceContext.getDefaultPersistenceContext();

      // remove expired messages
      String hql = "select mes from " + Message.class.getName() + " as mes where mes.expires<:today ";
      OqlQuery query = new OqlQuery(hql);
      query.getQuery().setTimestamp("today", new Date());
      List<Message> expMes = query.list();
      for (Message mes : expMes) {
        mes.remove();
      }

      // remove expired listener
      hql = "select lis from " + Listener.class.getName() + " as lis where lis.validityEnd<:today ";
      query = new OqlQuery(hql);
      query.getQuery().setTimestamp("today", new Date());
      List<Listener> expLis = query.list();
      for (Listener lis : expLis) {
        lis.remove();
      }

      // remove orphan listener
      hql = "select distinct listener.theClass from " + Listener.class.getName() + " as listener ";
      query = new OqlQuery(hql);
      List<String> kinds = query.list();

      String listenerTableName = HibernateUtilities.getTableName(Listener.class);

      //loop for each listener related object
      for (String theClass : kinds) {
        try {
          Class claz = Class.forName(ReflectionUtilities.deProxy(theClass));
          if (ReflectionUtilities.extendsOrImplements(claz, IdentifiableSupport.class)) {
            String tableName = HibernateUtilities.getTableName(claz);
            if (JSP.ex(tableName)) {

              String oldButAlwaysValidSQL = "SELECT listener.id " +
                      "FROM        " + listenerTableName + " listener LEFT OUTER JOIN " + tableName + " ref ON listener.identifiableId = ref.id " +
                      "WHERE     (listener.theClass = '" + theClass + "' AND ref.id IS NULL)";

              Statement s = pc.session.connection().createStatement();
              ResultSet r = s.executeQuery(oldButAlwaysValidSQL);
              StringBuffer ids = new StringBuffer();
              boolean isFirst = true;
              while (r.next()) {
                if (!isFirst)
                  ids.append(",");
                isFirst = false;
                ids.append("'" + r.getString("id") + "'");
              }
              r.close();

              if (!isFirst) {
                //todo: scale where ids are so many that reach sql string length
                String delSQL = "DELETE FROM " + listenerTableName + " WHERE ID IN (" + ids + ")";
                s.execute(delSQL);
              }
              s.close();
            }
          }

        } catch (ClassNotFoundException e) {
        }
      }

      pc.commitAndClose();


      jobLogData.notes = jobLogData.notes + "OrphanKiller executed on " + DateUtilities.dateAndHourToString(new Date());

    } catch (Throwable e) {
      Tracer.platformLogger.error("OrphanKiller error", e);
      if (pc != null) {
        pc.rollbackAndClose();
      }
      jobLogData.successfull = false;

    }

    return jobLogData;
  }

}
