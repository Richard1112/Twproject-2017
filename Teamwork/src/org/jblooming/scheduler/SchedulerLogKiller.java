package org.jblooming.scheduler;

  import org.jblooming.scheduler.ExecutableSupport;
  import org.jblooming.scheduler.JobLogData;
  import org.jblooming.persistence.hibernate.PersistenceContext;
  import org.jblooming.persistence.hibernate.HibernateUtilities;
  import org.jblooming.utilities.DateUtilities;
  import org.jblooming.tracer.Tracer;
  import org.jblooming.agenda.CompanyCalendar;

  import java.sql.PreparedStatement;
  import java.util.Date;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class SchedulerLogKiller extends ExecutableSupport {
  // @Parameter("[how may days of log do you want to preserve]")
  public String daysToSave = "2";

  public JobLogData run(JobLogData jobLogData) throws Exception {
    PersistenceContext pc = null;

    try {

      CompanyCalendar cc = new CompanyCalendar();
      cc.set(CompanyCalendar.DAY_OF_YEAR, -Integer.parseInt(daysToSave));
      pc = PersistenceContext.getDefaultPersistenceContext();

      String jobLogTableName = HibernateUtilities.getTableName(JobLog.class);
      String delSQL = "DELETE FROM " + jobLogTableName + " WHERE " + jobLogTableName + ".DATEX < ?;";

      PreparedStatement s = pc.session.connection().prepareStatement(delSQL);
      s.setDate(1, new java.sql.Date(cc.getTimeInMillis()));
      s.execute();
      s.close();

      pc.commitAndClose();

      jobLogData.notes = jobLogData.notes + "SchedulerLogKiller executed on " + DateUtilities.dateAndHourToString(new Date());
    } catch (Throwable e) {
      Tracer.platformLogger.error("SchedulerLogKiller error", e);

      if (pc != null) {
        pc.rollbackAndClose();
      }
      jobLogData.successfull = false;
    }

    return jobLogData;
  }

}
