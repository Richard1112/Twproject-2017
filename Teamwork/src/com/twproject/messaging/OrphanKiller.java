package com.twproject.messaging;

import com.twproject.task.Task;
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
import org.jblooming.agenda.CompanyCalendar;

import java.sql.ResultSet;
import java.sql.Statement;
import java.util.Date;
import java.util.List;

import com.twproject.rank.Hit;

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
      String hql = "delete from " + Message.class.getName() + " as mes where mes.expires<:today or mes.received<:someTimeAgo";
      OqlQuery query = new OqlQuery(hql);
      query.getQuery().setTimestamp("today", new Date());
      query.getQuery().setTimestamp("someTimeAgo", new Date(System.currentTimeMillis()-CompanyCalendar.MILLIS_IN_3_MONTH));
      query.getQuery().executeUpdate();

      // remove expired listener
      hql = "delete from " + Listener.class.getName() + " as lis where lis.validityEnd<:today ";
      query = new OqlQuery(hql);
      query.getQuery().setTimestamp("today", new Date());
      query.getQuery().executeUpdate();


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
                ids.append("" + r.getString("id"));
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

      //remove listener for closed/failed untouched tasks
      //String delHQL = "DELETE " + Listener.class.getName()+ " as lis WHERE lis.theClass=:tc and identifiableId IN ( select task.id from " + Task.class.getName() + " as task where task)";


      //remove old hits
      CompanyCalendar cc = new CompanyCalendar();
      cc.add(CompanyCalendar.DAY_OF_YEAR, -100);
      OqlQuery oql = new OqlQuery("delete from " + Hit.class.getName() + " as hit where hit.when < :when");
      oql.getQuery().setLong("when", cc.getTime().getTime());
      oql.getQuery().executeUpdate();

      //remove dead hits
      cc = new CompanyCalendar();
      cc.add(CompanyCalendar.DAY_OF_YEAR, -7);
      Hit.removeDeleted(cc.getTime());



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
